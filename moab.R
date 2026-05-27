# ============================================================
# M.O.A.B — Mother of All Boards
# Premium football intelligence platform powered by flashscore
# Ivory · Cobalt · Soft Gold theme
# ============================================================

library(shiny)
library(DT)
library(dplyr)
library(rvest)
library(httr)
library(jsonlite)
library(future)
library(parallelly)

# ════════════════════════════════════════════════════════════
# CONFIG
# ════════════════════════════════════════════════════════════

BASE_PATH       <- "C:/Users/Ogbuta/OneDrive/New Projects 3"
if (!dir.exists(BASE_PATH)) dir.create(BASE_PATH, recursive = TRUE)
CACHE_PATH      <- file.path(BASE_PATH, "match_cache.rds")
DIRECTORY_PATH  <- file.path(BASE_PATH, "league_directory.rds")
ENRICHED_PATH   <- file.path(BASE_PATH, "enriched_fixtures.rds")
CACHE_VERSION   <- 1L
BASE_URL        <- "https://www.flashscore.com"

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.data.frame(a) || is.list(a)) return(a)
  if (length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

# ════════════════════════════════════════════════════════════
# CACHE
# ════════════════════════════════════════════════════════════

load_cache <- function() {
  if (!file.exists(CACHE_PATH)) return(list())
  tryCatch(readRDS(CACHE_PATH), error = function(e) {
    file.copy(CACHE_PATH, paste0(CACHE_PATH, ".corrupt"), overwrite = TRUE)
    list()
  })
}

save_cache <- function(cache) {
  tryCatch({
    tmp <- paste0(CACHE_PATH, ".tmp")
    saveRDS(cache, tmp)
    file.rename(tmp, CACHE_PATH)
  }, error = function(e) invisible())
}

is_valid_cache_entry <- function(entry) {
  if (is.null(entry)) return(FALSE)
  ev <- entry$cache_version %||% 0L
  if (ev < CACHE_VERSION) return(FALSE)
  status <- entry$status %||% "UNKNOWN"
  if (status == "FINAL") {
    if (is.null(entry$ft_home) || is.null(entry$ft_away)) return(FALSE)
    if (is.na(entry$ft_home) || is.na(entry$ft_away)) return(FALSE)
    return(TRUE)
  }
  FALSE
}

cache_hit <- function(cache, match_id, force_refresh = FALSE) {
  if (isTRUE(force_refresh)) return(FALSE)
  is_valid_cache_entry(cache[[match_id]])
}

prepare_for_cache <- function(data) {
  if (is.null(data)) return(NULL)
  data$cache_version <- CACHE_VERSION
  data$cached_at     <- Sys.time()
  if (!is_valid_cache_entry(data)) attr(data, "skip_cache") <- TRUE
  data
}

# ════════════════════════════════════════════════════════════
# SELENIUM
# ════════════════════════════════════════════════════════════

# Worker = one chromedriver process on its own port, one Chrome window, with
# multiple tabs (handles) we round-robin through for true parallel page loads.

N_WORKERS  <- 4   # number of Chrome windows (used by sequential pool for fixtures/standings)
N_TABS     <- 2   # tabs per window
N_PARALLEL_WORKERS <- 6  # true parallel R processes for form/H2H enrichment
CHROME_PATH       <- "C:/Program Files/Google/Chrome/Application/chrome.exe"
CHROMEDRIVER_PATH <- "C:/Users/Ogbuta/Downloads/chromedriver-win64/chromedriver.exe"
WORKER_PROGRESS_DIR <- file.path(BASE_PATH, "worker_progress")
if (!dir.exists(WORKER_PROGRESS_DIR)) dir.create(WORKER_PROGRESS_DIR, recursive = TRUE)

# Pool of workers — populated by start_selenium_pool()
SEL_POOL <- list()

kill_all_chromedrivers <- function() {
  for (i in 1:3) {
    system("taskkill /F /IM chromedriver.exe", ignore.stdout = TRUE,
           ignore.stderr = TRUE, wait = TRUE); Sys.sleep(0.5)
  }
  Sys.sleep(1)
}

# Find N free ports for N workers
find_free_ports <- function(n) {
  ports <- c()
  set.seed(as.integer(Sys.time()))
  candidates <- c(8888, 8889, 7777, 7778, 6666, 5555, 9876, 9877, 9878, 9879)
  for (p in candidates) {
    if (length(ports) >= n) break
    ports <- c(ports, p)
  }
  while (length(ports) < n) {
    p <- sample(10000:60000, 1)
    if (!p %in% ports) ports <- c(ports, p)
  }
  ports
}

# Start ONE worker: chromedriver on a specific port + create N_TABS tabs.
# Returns list with port, session_id, handles (list of window handles).
start_worker <- function(port) {
  system2(CHROMEDRIVER_PATH, args = paste0("--port=", port), wait = FALSE)
  Sys.sleep(2)
  response <- tryCatch(POST(
    paste0("http://localhost:", port, "/session"),
    body = list(capabilities = list(alwaysMatch = list(
      browserName = "chrome",
      `goog:chromeOptions` = list(
        binary = CHROME_PATH,
        args = list("--no-sandbox", "--disable-dev-shm-usage",
                    "--disable-blink-features=AutomationControlled",
                    "--disable-extensions"),
        excludeSwitches = list("enable-automation"),
        useAutomationExtension = FALSE)))),
    encode = "json", timeout(30)), error = function(e) NULL)
  if (is.null(response) || status_code(response) != 200) stop("Worker start failed on port ", port)
  sd <- fromJSON(content(response, as = "text"))
  session_id <- sd$sessionId %||% sd$value$sessionId
  
  # Get the initial window handle
  r <- GET(paste0("http://localhost:", port, "/session/", session_id, "/window/handles"))
  handles <- fromJSON(content(r, as = "text"))$value
  
  # Open additional tabs to reach N_TABS total
  worker <- list(port = port, session_id = session_id, handles = handles,
                 current_tab = 1)
  while (length(worker$handles) < N_TABS) {
    body_json <- '{"type": "tab"}'
    POST(paste0("http://localhost:", port, "/session/", session_id, "/window/new"),
         body = body_json, encode = "raw",
         add_headers(`Content-Type` = "application/json"), timeout(15))
    Sys.sleep(0.5)
    r <- GET(paste0("http://localhost:", port, "/session/", session_id, "/window/handles"))
    worker$handles <- fromJSON(content(r, as = "text"))$value
  }
  worker
}

# Start the whole pool: N_WORKERS workers, each with N_TABS tabs
start_selenium_pool <- function() {
  kill_all_chromedrivers()
  ports <- find_free_ports(N_WORKERS)
  SEL_POOL <<- list()
  for (i in seq_along(ports)) {
    message("[ POOL ] Starting worker ", i, "/", N_WORKERS, " on port ", ports[i])
    w <- tryCatch(start_worker(ports[i]), error = function(e) {
      message("Worker ", i, " failed: ", e$message); NULL
    })
    if (!is.null(w)) SEL_POOL[[length(SEL_POOL) + 1]] <<- w
    Sys.sleep(1)
  }
  if (length(SEL_POOL) == 0) stop("All workers failed to start")
  # Warm up each worker
  for (w in SEL_POOL) {
    tab_navigate(w, w$handles[1], paste0(BASE_URL, "/"))
    Sys.sleep(3)
    tab_dismiss_cookie(w, w$handles[1])
  }
  message("[ POOL ] ", length(SEL_POOL), " workers ready (",
          length(SEL_POOL) * N_TABS, " concurrent tabs)")
}

stop_selenium_pool <- function() {
  for (w in SEL_POOL) {
    tryCatch(DELETE(paste0("http://localhost:", w$port, "/session/", w$session_id),
                    timeout(10)), error = function(e) invisible(NULL))
  }
  kill_all_chromedrivers()
  SEL_POOL <<- list()
}

# Tab-level operations (operate on a specific window handle within a worker)
switch_to_tab <- function(worker, handle) {
  body_json <- sprintf('{"handle": "%s"}', handle)
  POST(paste0("http://localhost:", worker$port, "/session/", worker$session_id, "/window"),
       body = body_json, encode = "raw",
       add_headers(`Content-Type` = "application/json"), timeout(10))
}

tab_navigate <- function(worker, handle, url) {
  switch_to_tab(worker, handle)
  POST(paste0("http://localhost:", worker$port, "/session/", worker$session_id, "/url"),
       body = list(url = url), encode = "json", timeout(30))
}

tab_get_source <- function(worker, handle) {
  switch_to_tab(worker, handle)
  res <- GET(paste0("http://localhost:", worker$port, "/session/", worker$session_id,
                    "/source"), timeout(30))
  html_raw <- fromJSON(content(res, as = "text"))$value
  tryCatch(read_html(html_raw), error = function(e) NULL)
}

tab_js <- function(worker, handle, script_text) {
  switch_to_tab(worker, handle)
  body_json <- sprintf('{"script": %s, "args": []}',
                       toJSON(script_text, auto_unbox = TRUE))
  tryCatch({
    r <- POST(paste0("http://localhost:", worker$port, "/session/", worker$session_id,
                     "/execute/sync"),
              body = body_json, encode = "raw",
              add_headers(`Content-Type` = "application/json"),
              timeout(15))
    fromJSON(content(r, as = "text"))$value
  }, error = function(e) NULL)
}

tab_dismiss_cookie <- function(worker, handle) {
  tab_js(worker, handle,
         "var b=document.querySelectorAll('button,a');var k=['Reject','Decline','Accept','Agree'];for(var x of b){for(var y of k){if(x.innerText&&x.innerText.trim().toLowerCase().includes(y.toLowerCase())){x.click();return;}}}")
  Sys.sleep(0.5)
}

tab_wait_for_page <- function(worker, handle, max_wait = 20) {
  for (i in seq_len(max_wait)) {
    Sys.sleep(1)
    title <- tab_js(worker, handle, "return document.title;")
    title <- if (is.null(title)) "" else as.character(title)[1]
    if (length(title) == 1 && !grepl("Just a moment|Checking", title, ignore.case = TRUE) &&
        nchar(title) > 0) return(TRUE)
  }
  FALSE
}

tab_get_html <- function(worker, handle, url) {
  Sys.sleep(runif(1, 0.5, 1.5))
  tab_navigate(worker, handle, url)
  Sys.sleep(runif(1, 2, 3))
  tab_wait_for_page(worker, handle, max_wait = 15)
  tab_dismiss_cookie(worker, handle)
  Sys.sleep(0.5)
  tab_get_source(worker, handle)
}

tab_click_show_more <- function(worker, handle) {
  for (attempt in 1:2) {
    n <- tab_js(worker, handle, paste0(
      "var btns = document.querySelectorAll('button.wclButtonLink--h2h');",
      "var clicked = 0;",
      "for (var i = 0; i < btns.length; i++) {",
      "  try { btns[i].scrollIntoView({block:'center'}); btns[i].click(); clicked++; } catch(e) {}",
      "}",
      "return clicked;"))
    Sys.sleep(2)
    if (is.null(n) || (is.numeric(n) && n == 0)) break
  }
}

# Backward-compat helpers that pick a tab from the pool round-robin
# (used by sequential code paths like fixtures/standings scrape)
pool_tab_idx <- 1L
get_next_tab <- function() {
  if (length(SEL_POOL) == 0) stop("Pool not started")
  total_tabs <- length(SEL_POOL) * N_TABS
  idx <- (pool_tab_idx - 1) %% total_tabs + 1
  pool_tab_idx <<- pool_tab_idx + 1L
  worker_idx <- ((idx - 1) %/% N_TABS) + 1
  tab_within  <- ((idx - 1) %%  N_TABS) + 1
  w <- SEL_POOL[[worker_idx]]
  list(worker = w, handle = w$handles[tab_within])
}

selenium_get_html <- function(url) {
  t <- get_next_tab()
  tab_get_html(t$worker, t$handle, url)
}

selenium_navigate <- function(url) {
  t <- get_next_tab()
  tab_navigate(t$worker, t$handle, url)
}

selenium_get_source <- function() {
  if (length(SEL_POOL) == 0) return(NULL)
  w <- SEL_POOL[[1]]; tab_get_source(w, w$handles[1])
}

dismiss_cookie_popup <- function() {
  if (length(SEL_POOL) == 0) return(invisible())
  w <- SEL_POOL[[1]]; tab_dismiss_cookie(w, w$handles[1])
}

wait_for_page <- function(max_wait = 20) {
  if (length(SEL_POOL) == 0) return(FALSE)
  w <- SEL_POOL[[1]]; tab_wait_for_page(w, w$handles[1], max_wait)
}

js_eval <- function(script_text) {
  if (length(SEL_POOL) == 0) return(NULL)
  w <- SEL_POOL[[1]]; tab_js(w, w$handles[1], script_text)
}

click_show_more_h2h <- function() {
  if (length(SEL_POOL) == 0) return(invisible())
  w <- SEL_POOL[[1]]; tab_click_show_more(w, w$handles[1])
}

# Compatibility wrappers for old API
start_selenium <- function() start_selenium_pool()
stop_selenium  <- function() stop_selenium_pool()

# ════════════════════════════════════════════════════════════
# PARSERS
# ════════════════════════════════════════════════════════════

build_url <- function(base, page) paste0(base, page, "/")

YEAR <- format(Sys.Date(), "%Y")
parse_date <- function(s) {
  m <- regmatches(s, regexpr("\\d{1,2}\\.\\d{1,2}\\.", s))
  if (length(m) == 0 || nchar(m) == 0) return(list(date = NA, time = NA))
  d <- tryCatch(as.Date(paste0(m, YEAR), "%d.%m.%Y"), error = function(e) NA)
  t <- regmatches(s, regexpr("\\d{1,2}:\\d{2}", s))
  list(date = d, time = if (length(t) > 0) t else NA)
}

scrape_fixtures <- function(league_base, league_name) {
  url <- build_url(league_base, "fixtures")
  page <- selenium_get_html(url)
  if (is.null(page)) return(data.frame())
  match_nodes <- page %>% html_nodes("div.event__match")
  if (length(match_nodes) == 0) return(data.frame())
  out <- data.frame()
  for (m in match_nodes) {
    time_node <- m %>% html_node("div.event__time")
    if (is.null(time_node)) next
    dt <- parse_date(html_text(time_node, trim = TRUE))
    home_img <- m %>% html_node("div.event__homeParticipant img")
    away_img <- m %>% html_node("div.event__awayParticipant img")
    home <- if (!is.null(home_img)) html_attr(home_img, "alt") else NA
    away <- if (!is.null(away_img)) html_attr(away_img, "alt") else NA
    if (is.na(home) || is.na(away)) next
    link_node <- m %>% html_node("a.eventRowLink")
    match_url <- if (!is.null(link_node)) html_attr(link_node, "href") else NA
    match_id <- NA
    div_id <- html_attr(m, "id") %||% ""
    if (grepl("g_1_", div_id)) match_id <- gsub("^g_1_", "", div_id)
    home_team_id <- NA; away_team_id <- NA
    if (!is.na(match_url)) {
      matches <- regmatches(match_url, gregexpr("-([A-Za-z0-9]{8})/", match_url))[[1]]
      if (length(matches) >= 2) {
        ids <- gsub("[-/]", "", matches)
        home_team_id <- ids[1]; away_team_id <- ids[2]
      }
    }
    out <- rbind(out, data.frame(
      league = league_name, home = home, away = away,
      home_team_id = home_team_id, away_team_id = away_team_id,
      fixture_date = dt$date, match_time = dt$time,
      match_id = match_id, match_url = match_url,
      stringsAsFactors = FALSE
    ))
  }
  out
}

scrape_standings <- function(league_base, league_name) {
  url <- build_url(league_base, "standings")
  page <- selenium_get_html(url)
  if (is.null(page)) return(data.frame())
  rows <- page %>% html_nodes("div.ui-table__row")
  if (length(rows) == 0) return(data.frame())
  out <- data.frame()
  for (r in rows) {
    rank_node <- r %>% html_node("div.tableCellRank")
    if (is.null(rank_node)) next
    rank <- suppressWarnings(as.integer(gsub("\\.", "", html_text(rank_node, trim = TRUE))))
    if (is.na(rank)) next
    promo_title <- html_attr(rank_node, "title") %||% ""
    team_node <- r %>% html_node("a.tableCellParticipant__name")
    team_name <- if (!is.null(team_node)) html_text(team_node, trim = TRUE) else NA
    team_url  <- if (!is.null(team_node)) html_attr(team_node, "href") else NA
    if (is.na(team_name)) next
    vals <- r %>% html_nodes("span.table__cell--value") %>% html_text(trim = TRUE)
    if (length(vals) < 7) next
    g <- vals[5]
    gfor <- NA; gag <- NA
    if (!is.na(g) && grepl(":", g)) {
      gp <- strsplit(g, ":")[[1]]
      gfor <- suppressWarnings(as.integer(gp[1])); gag <- suppressWarnings(as.integer(gp[2]))
    }
    form_nodes <- r %>% html_nodes("div.tableCellFormIcon div.wcl-badgeform_AKaAR")
    form_chars <- character()
    for (fn in form_nodes) {
      tt <- html_attr(fn, "data-testid") %||% ""
      form_chars <- c(form_chars,
                      if (grepl("win", tt)) "W"
                      else if (grepl("lose", tt)) "L"
                      else if (grepl("draw", tt)) "D"
                      else "?")
    }
    if (length(form_chars) > 0 && form_chars[1] == "?") form_chars <- form_chars[-1]
    team_id <- NA
    if (!is.na(team_url)) {
      mid <- regmatches(team_url, regexpr("/[A-Za-z0-9]{8}/?$", team_url))
      if (length(mid) > 0) team_id <- gsub("/", "", mid)
    }
    out <- rbind(out, data.frame(
      league = league_name, rank = rank, team = team_name, team_id = team_id,
      team_url = ifelse(!is.na(team_url), paste0(BASE_URL, team_url), NA),
      mp = suppressWarnings(as.integer(vals[1])),
      w  = suppressWarnings(as.integer(vals[2])),
      d  = suppressWarnings(as.integer(vals[3])),
      l  = suppressWarnings(as.integer(vals[4])),
      goals_for = gfor, goals_against = gag,
      gd = suppressWarnings(as.integer(vals[6])),
      pts = suppressWarnings(as.integer(vals[7])),
      form = paste(form_chars, collapse = ""),
      promo_title = promo_title, stringsAsFactors = FALSE
    ))
  }
  out
}

parse_h2h_row <- function(row_node) {
  date_node  <- row_node %>% html_node("span.h2h__date")
  event_node <- row_node %>% html_node("span.h2h__event")
  home_node  <- row_node %>% html_node("span.h2h__homeParticipant span.h2h__participantInner")
  away_node  <- row_node %>% html_node("span.h2h__awayParticipant span.h2h__participantInner")
  result_nodes <- row_node %>% html_nodes("span.h2h__result > span")
  match_url <- html_attr(row_node, "href")
  competition_full <- if (!is.null(event_node)) html_attr(event_node, "title") else NA
  competition_tag  <- if (!is.null(event_node)) html_text(event_node, trim = TRUE) else NA
  home_score <- NA; away_score <- NA
  if (length(result_nodes) >= 2) {
    home_score <- suppressWarnings(as.integer(html_text(result_nodes[1], trim = TRUE)))
    away_score <- suppressWarnings(as.integer(html_text(result_nodes[2], trim = TRUE)))
  }
  match_id <- NA
  if (!is.na(match_url)) {
    mid <- regmatches(match_url, regexpr("mid=([A-Za-z0-9]+)", match_url))
    if (length(mid) > 0) match_id <- gsub("mid=", "", mid)
  }
  list(match_id = match_id, match_url = match_url,
       date = if (!is.null(date_node)) html_text(date_node, trim = TRUE) else NA,
       home = if (!is.null(home_node)) html_text(home_node, trim = TRUE) else NA,
       away = if (!is.null(away_node)) html_text(away_node, trim = TRUE) else NA,
       home_score = home_score, away_score = away_score,
       competition_full = competition_full, competition_tag = competition_tag)
}

scrape_h2h_page <- function(fixture_match_url, league_name_for_filter) {
  h2h_url <- sub("(\\?mid=)", "h2h/overall/\\1", fixture_match_url)
  Sys.sleep(runif(1, 2, 4)); selenium_navigate(h2h_url); Sys.sleep(runif(1, 4, 6))
  wait_for_page(max_wait = 20); dismiss_cookie_popup(); Sys.sleep(2)
  click_show_more_h2h()
  page <- selenium_get_source()
  if (is.null(page)) return(list(home_form = list(), away_form = list(), h2h = list()))
  sections <- page %>% html_nodes("div.h2h__section")
  out <- list(home_form = list(), away_form = list(), h2h = list())
  for (i in seq_along(sections)) {
    sec <- sections[[i]]
    title_node <- sec %>% html_node("span.wcl-scores-overline-02_bpqU7")
    title_txt  <- if (!is.null(title_node)) html_text(title_node, trim = TRUE) else ""
    rows <- sec %>% html_nodes("a.h2h__row")
    parsed <- lapply(rows, parse_h2h_row)
    for (k in seq_along(parsed)) {
      cf <- parsed[[k]]$competition_full %||% ""
      parsed[[k]]$is_main_league <- grepl(league_name_for_filter, cf, ignore.case = TRUE)
    }
    if (i == 1)      out$home_form <- head(parsed, 10)
    else if (i == 2) out$away_form <- head(parsed, 10)
    else if (i == 3) out$h2h       <- head(parsed, 5)
  }
  out
}

WANTED_STATS <- c("Expected goals (xG)", "Ball possession", "Total shots",
                  "Shots on target", "Shots off target", "Blocked shots",
                  "Shots inside the box", "Shots outside the box",
                  "Big chances", "Corner kicks", "Touches in opposition box",
                  "Fouls", "Offsides", "Free kicks", "Throw ins",
                  "Yellow cards", "Red cards", "Goalkeeper saves")

slug <- function(x) {
  x <- tolower(x); x <- gsub("[^a-z0-9]+", "_", x); gsub("^_|_$", "", x)
}

scrape_match_summary <- function(match_url) {
  page <- tryCatch(selenium_get_html(match_url), error = function(e) NULL)
  if (is.null(page)) return(list(goal_times_home = list(), goal_times_away = list()))
  result <- list(goal_times_home = list(), goal_times_away = list())
  home_rows <- page %>% html_nodes("div.smv__participantRow.smv__homeParticipant")
  away_rows <- page %>% html_nodes("div.smv__participantRow.smv__awayParticipant")
  extract <- function(rows) {
    t <- character()
    for (n in rows) {
      goal_icon_div <- n %>% html_node("div.smv__incidentIcon")
      if (is.null(goal_icon_div) || length(goal_icon_div) == 0) next
      goal_svg <- goal_icon_div %>% html_node("svg[data-testid='wcl-icon-incidents-goal-soccer']")
      if (is.null(goal_svg) || length(goal_svg) == 0) next
      tm <- n %>% html_node("div.smv__timeBox") %>% html_text(trim = TRUE)
      if (!is.na(tm) && nchar(tm) > 0) t <- c(t, tm)
    }
    t
  }
  result$goal_times_home <- as.list(extract(home_rows))
  result$goal_times_away <- as.list(extract(away_rows))
  result
}

build_stats_url <- function(match_url) {
  if (grepl("summary/stats/overall", match_url)) return(match_url)
  sub("(\\?mid=)", "summary/stats/overall/\\1", match_url)
}

scrape_match_stats <- function(match_url) {
  stats_url <- build_stats_url(match_url)
  page <- tryCatch(selenium_get_html(stats_url), error = function(e) NULL)
  if (is.null(page)) return(NULL)
  result <- list(stats_home = list(), stats_away = list(),
                 ht_home = NA, ht_away = NA, ft_home = NA, ft_away = NA,
                 goal_times_home = list(), goal_times_away = list(),
                 status = "UNKNOWN")
  rows <- page %>% html_nodes("div.wcl-row_2oCpS")
  for (r in rows) {
    cat_node <- r %>% html_node("div.wcl-category_6sT1J span")
    if (is.null(cat_node)) next
    cat_name <- html_text(cat_node, trim = TRUE)
    if (!(cat_name %in% WANTED_STATS)) next
    vals <- r %>% html_nodes("div.wcl-value_XJG99 span.wcl-bold_NZXv6")
    if (length(vals) < 2) next
    key <- slug(cat_name)
    result$stats_home[[key]] <- html_text(vals[1], trim = TRUE)
    result$stats_away[[key]] <- html_text(vals[2], trim = TRUE)
  }
  header_score <- page %>% html_nodes("div.detailScore__wrapper, span.detailScore__matchResult")
  if (length(header_score) > 0) {
    txt <- html_text(header_score[1], trim = TRUE)
    nums <- regmatches(txt, gregexpr("\\d+", txt))[[1]]
    if (length(nums) >= 2) {
      result$ft_home <- as.integer(nums[1])
      result$ft_away <- as.integer(nums[2])
      result$status <- "FINAL"
    }
  }
  summary_data <- tryCatch(scrape_match_summary(match_url),
                           error = function(e) list(goal_times_home = list(),
                                                    goal_times_away = list()))
  result$goal_times_home <- summary_data$goal_times_home
  result$goal_times_away <- summary_data$goal_times_away
  parse_min <- function(t) {
    t <- gsub("[^0-9+]", "", t)
    if (grepl("\\+", t)) { p <- as.integer(strsplit(t, "\\+")[[1]]); p[1] + p[2] }
    else suppressWarnings(as.integer(t))
  }
  if (length(result$goal_times_home) > 0)
    result$ht_home <- sum(sapply(result$goal_times_home, function(t) parse_min(t) <= 45), na.rm = TRUE)
  else if (!is.na(result$ft_home)) result$ht_home <- 0
  if (length(result$goal_times_away) > 0)
    result$ht_away <- sum(sapply(result$goal_times_away, function(t) parse_min(t) <= 45), na.rm = TRUE)
  else if (!is.na(result$ft_away)) result$ht_away <- 0
  result
}

enrich_form_entry <- function(entry, concerned_team_name, cache, force_refresh = FALSE) {
  if (is.null(entry$match_id) || is.na(entry$match_id))
    return(list(entry = entry, cache_updated = FALSE, new_data = NULL))
  cache_key <- entry$match_id
  use_cache <- cache_hit(cache, cache_key, force_refresh)
  if (use_cache) cached <- cache[[cache_key]]
  else {
    cached <- scrape_match_stats(entry$match_url)
    if (is.null(cached)) return(list(entry = entry, cache_updated = FALSE, new_data = NULL))
    cached <- prepare_for_cache(cached)
  }
  is_concerned_home <- !is.na(entry$home) &&
    grepl(concerned_team_name, entry$home, ignore.case = TRUE)
  entry$concerned_was_home <- is_concerned_home
  entry$ht_for     <- if (is_concerned_home) cached$ht_home else cached$ht_away
  entry$ht_against <- if (is_concerned_home) cached$ht_away else cached$ht_home
  entry$ft_for     <- if (is_concerned_home) cached$ft_home else cached$ft_away
  entry$ft_against <- if (is_concerned_home) cached$ft_away else cached$ft_home
  entry$goal_times <- if (is_concerned_home) cached$goal_times_home else cached$goal_times_away
  entry$stats      <- if (is_concerned_home) cached$stats_home else cached$stats_away
  entry$status     <- cached$status
  list(entry = entry, cache_updated = !use_cache, new_data = cached)
}


# ════════════════════════════════════════════════════════════
# PARALLEL WORKER FUNCTION
# ════════════════════════════════════════════════════════════
# Each worker runs in an isolated R process via future::multisession.
# It starts its own chromedriver, scrapes its assigned chunk of match URLs,
# checkpoints to disk every N matches, and returns the result list.
#
# Workers NEVER write to the main cache file. Main process merges results.

# League worker — scrapes fixtures + standings for a chunk of leagues.
# Returns list(fixtures=df, standings=df)
run_league_worker <- function(worker_id, leagues_df, chrome_path, chromedriver_path,
                              base_url, progress_dir) {
  library(rvest); library(httr); library(jsonlite); library(dplyr)
  
  port <- 30000 + worker_id * 271 + sample(1:999, 1)
  session_id <- NULL
  
  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (is.data.frame(a) || is.list(a)) return(a)
    if (length(a) == 0) return(b)
    if (length(a) == 1 && is.na(a)) return(b)
    a
  }
  YEAR <- format(Sys.Date(), "%Y")
  parse_date <- function(s) {
    m <- regmatches(s, regexpr("\\d{1,2}\\.\\d{1,2}\\.", s))
    if (length(m) == 0 || nchar(m) == 0) return(list(date = NA, time = NA))
    d <- tryCatch(as.Date(paste0(m, YEAR), "%d.%m.%Y"), error = function(e) NA)
    t <- regmatches(s, regexpr("\\d{1,2}:\\d{2}", s))
    list(date = d, time = if (length(t) > 0) t else NA)
  }
  build_url <- function(base, page) paste0(base, page, "/")
  
  navigate <- function(url) {
    tryCatch(POST(paste0("http://localhost:", port, "/session/", session_id, "/url"),
                  body = list(url = url), encode = "json", timeout(30)),
             error = function(e) NULL)
  }
  get_source <- function() {
    res <- tryCatch(GET(paste0("http://localhost:", port, "/session/", session_id, "/source"),
                        timeout(30)), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    html_raw <- tryCatch(fromJSON(content(res, as = "text"))$value, error = function(e) NULL)
    if (is.null(html_raw)) return(NULL)
    tryCatch(read_html(html_raw), error = function(e) NULL)
  }
  js_eval <- function(script_text) {
    body_json <- sprintf('{"script": %s, "args": []}', toJSON(script_text, auto_unbox = TRUE))
    tryCatch({
      r <- POST(paste0("http://localhost:", port, "/session/", session_id, "/execute/sync"),
                body = body_json, encode = "raw",
                add_headers(`Content-Type` = "application/json"), timeout(15))
      fromJSON(content(r, as = "text"))$value
    }, error = function(e) NULL)
  }
  wait_for_page <- function(max_wait = 15) {
    for (i in seq_len(max_wait)) {
      Sys.sleep(1)
      title <- js_eval("return document.title;")
      title <- if (is.null(title)) "" else as.character(title)[1]
      if (length(title) == 1 && !grepl("Just a moment|Checking", title, ignore.case = TRUE) &&
          nchar(title) > 0) return(TRUE)
    }
    FALSE
  }
  dismiss_cookie <- function() {
    js_eval("var b=document.querySelectorAll('button,a');var k=['Reject','Decline','Accept','Agree'];for(var x of b){for(var y of k){if(x.innerText&&x.innerText.trim().toLowerCase().includes(y.toLowerCase())){x.click();return;}}}")
    Sys.sleep(0.5)
  }
  get_html <- function(url) {
    Sys.sleep(runif(1, 1, 2))
    navigate(url)
    Sys.sleep(runif(1, 2, 3))
    wait_for_page(15)
    dismiss_cookie()
    Sys.sleep(0.5)
    get_source()
  }
  
  scrape_fixtures <- function(league_base, league_name) {
    url <- build_url(league_base, "fixtures")
    page <- get_html(url)
    if (is.null(page)) return(data.frame())
    nodes <- page %>% html_nodes("div.event__match")
    if (length(nodes) == 0) return(data.frame())
    out <- data.frame()
    for (m in nodes) {
      tn <- m %>% html_node("div.event__time")
      if (is.null(tn)) next
      dt <- parse_date(html_text(tn, trim = TRUE))
      hi <- m %>% html_node("div.event__homeParticipant img")
      ai <- m %>% html_node("div.event__awayParticipant img")
      home <- if (!is.null(hi)) html_attr(hi, "alt") else NA
      away <- if (!is.null(ai)) html_attr(ai, "alt") else NA
      if (is.na(home) || is.na(away)) next
      lk <- m %>% html_node("a.eventRowLink")
      mu <- if (!is.null(lk)) html_attr(lk, "href") else NA
      mid <- NA
      did <- html_attr(m, "id") %||% ""
      if (grepl("g_1_", did)) mid <- gsub("^g_1_", "", did)
      hid <- NA; aid <- NA
      if (!is.na(mu)) {
        mtch <- regmatches(mu, gregexpr("-([A-Za-z0-9]{8})/", mu))[[1]]
        if (length(mtch) >= 2) { ids <- gsub("[-/]", "", mtch); hid <- ids[1]; aid <- ids[2] }
      }
      out <- rbind(out, data.frame(
        league = league_name, home = home, away = away,
        home_team_id = hid, away_team_id = aid,
        fixture_date = dt$date, match_time = dt$time,
        match_id = mid, match_url = mu, stringsAsFactors = FALSE))
    }
    out
  }
  
  scrape_standings <- function(league_base, league_name) {
    url <- build_url(league_base, "standings")
    page <- get_html(url)
    if (is.null(page)) return(data.frame())
    rows <- page %>% html_nodes("div.ui-table__row")
    if (length(rows) == 0) return(data.frame())
    out <- data.frame()
    for (r in rows) {
      rn <- r %>% html_node("div.tableCellRank")
      if (is.null(rn)) next
      rank <- suppressWarnings(as.integer(gsub("\\.", "", html_text(rn, trim = TRUE))))
      if (is.na(rank)) next
      pt <- html_attr(rn, "title") %||% ""
      tn <- r %>% html_node("a.tableCellParticipant__name")
      tname <- if (!is.null(tn)) html_text(tn, trim = TRUE) else NA
      turl  <- if (!is.null(tn)) html_attr(tn, "href") else NA
      if (is.na(tname)) next
      vals <- r %>% html_nodes("span.table__cell--value") %>% html_text(trim = TRUE)
      if (length(vals) < 7) next
      g <- vals[5]; gfor <- NA; gag <- NA
      if (!is.na(g) && grepl(":", g)) {
        gp <- strsplit(g, ":")[[1]]
        gfor <- suppressWarnings(as.integer(gp[1]))
        gag <- suppressWarnings(as.integer(gp[2]))
      }
      fn <- r %>% html_nodes("div.tableCellFormIcon div.wcl-badgeform_AKaAR")
      fc <- character()
      for (f in fn) {
        tt <- html_attr(f, "data-testid") %||% ""
        fc <- c(fc, if (grepl("win", tt)) "W"
                else if (grepl("lose", tt)) "L"
                else if (grepl("draw", tt)) "D" else "?")
      }
      if (length(fc) > 0 && fc[1] == "?") fc <- fc[-1]
      tid <- NA
      if (!is.na(turl)) {
        mid <- regmatches(turl, regexpr("/[A-Za-z0-9]{8}/?$", turl))
        if (length(mid) > 0) tid <- gsub("/", "", mid)
      }
      out <- rbind(out, data.frame(
        league = league_name, rank = rank, team = tname, team_id = tid,
        team_url = ifelse(!is.na(turl), paste0(base_url, turl), NA),
        mp = suppressWarnings(as.integer(vals[1])),
        w  = suppressWarnings(as.integer(vals[2])),
        d  = suppressWarnings(as.integer(vals[3])),
        l  = suppressWarnings(as.integer(vals[4])),
        goals_for = gfor, goals_against = gag,
        gd = suppressWarnings(as.integer(vals[6])),
        pts = suppressWarnings(as.integer(vals[7])),
        form = paste(fc, collapse = ""),
        promo_title = pt, stringsAsFactors = FALSE))
    }
    out
  }
  
  # Start chromedriver
  for (attempt in 1:3) {
    system2(chromedriver_path, args = paste0("--port=", port), wait = FALSE)
    Sys.sleep(3)
    resp <- tryCatch(POST(
      paste0("http://localhost:", port, "/session"),
      body = list(capabilities = list(alwaysMatch = list(
        browserName = "chrome",
        `goog:chromeOptions` = list(
          binary = chrome_path,
          args = list("--no-sandbox", "--disable-dev-shm-usage",
                      "--disable-blink-features=AutomationControlled",
                      "--disable-extensions"),
          excludeSwitches = list("enable-automation"),
          useAutomationExtension = FALSE)))),
      encode = "json", timeout(30)), error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200) {
      sd <- fromJSON(content(resp, as = "text"))
      session_id <- sd$sessionId %||% sd$value$sessionId
      break
    }
    port <- port + 1
  }
  if (is.null(session_id)) {
    return(list(fixtures = data.frame(), standings = data.frame()))
  }
  navigate(paste0(base_url, "/"))
  Sys.sleep(3); wait_for_page(30); dismiss_cookie(); Sys.sleep(2)
  
  progress_path <- file.path(progress_dir, paste0("worker_a_", worker_id, "_progress.txt"))
  all_fix <- data.frame(); all_std <- data.frame()
  total <- nrow(leagues_df)
  for (i in seq_len(total)) {
    lg <- leagues_df[i, ]
    fix <- tryCatch(scrape_fixtures(lg$league_url, lg$league_name),
                    error = function(e) data.frame())
    if (nrow(fix) > 0) all_fix <- rbind(all_fix, fix)
    std <- tryCatch(scrape_standings(lg$league_url, lg$league_name),
                    error = function(e) data.frame())
    if (nrow(std) > 0) all_std <- rbind(all_std, std)
    writeLines(paste0(i, "/", total), progress_path)
  }
  writeLines(paste0(total, "/", total, " DONE"), progress_path)
  
  tryCatch(DELETE(paste0("http://localhost:", port, "/session/", session_id),
                  timeout(10)), error = function(e) invisible())
  
  list(fixtures = all_fix, standings = all_std)
}

# H2H worker — scrapes H2H pages for a chunk of fixtures.
# Returns list of h2h data keyed by position in chunk.
run_h2h_worker <- function(worker_id, fixtures_df, chrome_path, chromedriver_path,
                           base_url, progress_dir) {
  library(rvest); library(httr); library(jsonlite); library(dplyr)
  
  port <- 40000 + worker_id * 419 + sample(1:999, 1)
  session_id <- NULL
  
  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (is.data.frame(a) || is.list(a)) return(a)
    if (length(a) == 0) return(b)
    if (length(a) == 1 && is.na(a)) return(b)
    a
  }
  navigate <- function(url) {
    tryCatch(POST(paste0("http://localhost:", port, "/session/", session_id, "/url"),
                  body = list(url = url), encode = "json", timeout(30)),
             error = function(e) NULL)
  }
  get_source <- function() {
    res <- tryCatch(GET(paste0("http://localhost:", port, "/session/", session_id, "/source"),
                        timeout(30)), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    html_raw <- tryCatch(fromJSON(content(res, as = "text"))$value, error = function(e) NULL)
    if (is.null(html_raw)) return(NULL)
    tryCatch(read_html(html_raw), error = function(e) NULL)
  }
  js_eval <- function(script_text) {
    body_json <- sprintf('{"script": %s, "args": []}', toJSON(script_text, auto_unbox = TRUE))
    tryCatch({
      r <- POST(paste0("http://localhost:", port, "/session/", session_id, "/execute/sync"),
                body = body_json, encode = "raw",
                add_headers(`Content-Type` = "application/json"), timeout(15))
      fromJSON(content(r, as = "text"))$value
    }, error = function(e) NULL)
  }
  wait_for_page <- function(max_wait = 15) {
    for (i in seq_len(max_wait)) {
      Sys.sleep(1)
      title <- js_eval("return document.title;")
      title <- if (is.null(title)) "" else as.character(title)[1]
      if (length(title) == 1 && !grepl("Just a moment|Checking", title, ignore.case = TRUE) &&
          nchar(title) > 0) return(TRUE)
    }
    FALSE
  }
  dismiss_cookie <- function() {
    js_eval("var b=document.querySelectorAll('button,a');var k=['Reject','Decline','Accept','Agree'];for(var x of b){for(var y of k){if(x.innerText&&x.innerText.trim().toLowerCase().includes(y.toLowerCase())){x.click();return;}}}")
    Sys.sleep(0.5)
  }
  click_show_more <- function() {
    for (attempt in 1:2) {
      n <- js_eval(paste0(
        "var btns = document.querySelectorAll('button.wclButtonLink--h2h');",
        "var clicked = 0;",
        "for (var i = 0; i < btns.length; i++) {",
        "  try { btns[i].scrollIntoView({block:'center'}); btns[i].click(); clicked++; } catch(e) {}",
        "}",
        "return clicked;"))
      Sys.sleep(2)
      if (is.null(n) || (is.numeric(n) && n == 0)) break
    }
  }
  
  parse_h2h_row <- function(row_node) {
    date_node  <- row_node %>% html_node("span.h2h__date")
    event_node <- row_node %>% html_node("span.h2h__event")
    home_node  <- row_node %>% html_node("span.h2h__homeParticipant span.h2h__participantInner")
    away_node  <- row_node %>% html_node("span.h2h__awayParticipant span.h2h__participantInner")
    result_nodes <- row_node %>% html_nodes("span.h2h__result > span")
    mu <- html_attr(row_node, "href")
    cf <- if (!is.null(event_node)) html_attr(event_node, "title") else NA
    ct <- if (!is.null(event_node)) html_text(event_node, trim = TRUE) else NA
    hs <- NA; as_ <- NA
    if (length(result_nodes) >= 2) {
      hs  <- suppressWarnings(as.integer(html_text(result_nodes[1], trim = TRUE)))
      as_ <- suppressWarnings(as.integer(html_text(result_nodes[2], trim = TRUE)))
    }
    mid <- NA
    if (!is.na(mu)) {
      m <- regmatches(mu, regexpr("mid=([A-Za-z0-9]+)", mu))
      if (length(m) > 0) mid <- gsub("mid=", "", m)
    }
    list(match_id = mid, match_url = mu,
         date = if (!is.null(date_node)) html_text(date_node, trim = TRUE) else NA,
         home = if (!is.null(home_node)) html_text(home_node, trim = TRUE) else NA,
         away = if (!is.null(away_node)) html_text(away_node, trim = TRUE) else NA,
         home_score = hs, away_score = as_,
         competition_full = cf, competition_tag = ct)
  }
  
  scrape_h2h <- function(fixture_match_url, league_name_for_filter) {
    h2h_url <- sub("(\\?mid=)", "h2h/overall/\\1", fixture_match_url)
    Sys.sleep(runif(1, 1, 2))
    navigate(h2h_url)
    Sys.sleep(runif(1, 2, 3))
    wait_for_page(15)
    dismiss_cookie()
    Sys.sleep(1)
    click_show_more()
    page <- get_source()
    if (is.null(page)) return(list(home_form = list(), away_form = list(), h2h = list()))
    sections <- page %>% html_nodes("div.h2h__section")
    out <- list(home_form = list(), away_form = list(), h2h = list())
    for (i in seq_along(sections)) {
      sec <- sections[[i]]
      rows <- sec %>% html_nodes("a.h2h__row")
      parsed <- lapply(rows, parse_h2h_row)
      for (k in seq_along(parsed)) {
        cf <- parsed[[k]]$competition_full %||% ""
        parsed[[k]]$is_main_league <- grepl(league_name_for_filter, cf, ignore.case = TRUE)
      }
      if (i == 1)      out$home_form <- head(parsed, 10)
      else if (i == 2) out$away_form <- head(parsed, 10)
      else if (i == 3) out$h2h       <- head(parsed, 5)
    }
    out
  }
  
  # Start chromedriver
  for (attempt in 1:3) {
    system2(chromedriver_path, args = paste0("--port=", port), wait = FALSE)
    Sys.sleep(3)
    resp <- tryCatch(POST(
      paste0("http://localhost:", port, "/session"),
      body = list(capabilities = list(alwaysMatch = list(
        browserName = "chrome",
        `goog:chromeOptions` = list(
          binary = chrome_path,
          args = list("--no-sandbox", "--disable-dev-shm-usage",
                      "--disable-blink-features=AutomationControlled",
                      "--disable-extensions"),
          excludeSwitches = list("enable-automation"),
          useAutomationExtension = FALSE)))),
      encode = "json", timeout(30)), error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200) {
      sd <- fromJSON(content(resp, as = "text"))
      session_id <- sd$sessionId %||% sd$value$sessionId
      break
    }
    port <- port + 1
  }
  if (is.null(session_id)) {
    return(replicate(nrow(fixtures_df), NULL, simplify = FALSE))
  }
  navigate(paste0(base_url, "/"))
  Sys.sleep(3); wait_for_page(30); dismiss_cookie(); Sys.sleep(2)
  
  progress_path <- file.path(progress_dir, paste0("worker_h_", worker_id, "_progress.txt"))
  results <- list()
  total <- nrow(fixtures_df)
  for (i in seq_len(total)) {
    fx <- fixtures_df[i, ]
    results[[i]] <- tryCatch(scrape_h2h(fx$match_url, fx$league),
                             error = function(e) NULL)
    writeLines(paste0(i, "/", total), progress_path)
  }
  writeLines(paste0(total, "/", total, " DONE"), progress_path)
  
  tryCatch(DELETE(paste0("http://localhost:", port, "/session/", session_id),
                  timeout(10)), error = function(e) invisible())
  results
}

run_parallel_worker <- function(worker_id, work_chunk, chrome_path, chromedriver_path,
                                base_url, progress_dir, cache_version,
                                wanted_stats) {
  # Re-import libs in worker process
  library(rvest); library(httr); library(jsonlite); library(dplyr)
  
  port <- 20000 + worker_id * 137 + sample(1:999, 1)
  session_id <- NULL
  
  # Local helpers (self-contained)
  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (is.data.frame(a) || is.list(a)) return(a)
    if (length(a) == 0) return(b)
    if (length(a) == 1 && is.na(a)) return(b)
    a
  }
  slug <- function(x) { x <- tolower(x); x <- gsub("[^a-z0-9]+", "_", x); gsub("^_|_$", "", x) }
  
  navigate <- function(url) {
    tryCatch(POST(paste0("http://localhost:", port, "/session/", session_id, "/url"),
                  body = list(url = url), encode = "json", timeout(30)),
             error = function(e) NULL)
  }
  get_source <- function() {
    res <- tryCatch(GET(paste0("http://localhost:", port, "/session/", session_id, "/source"),
                        timeout(30)), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    html_raw <- tryCatch(fromJSON(content(res, as = "text"))$value, error = function(e) NULL)
    if (is.null(html_raw)) return(NULL)
    tryCatch(read_html(html_raw), error = function(e) NULL)
  }
  js_eval <- function(script_text) {
    body_json <- sprintf('{"script": %s, "args": []}', toJSON(script_text, auto_unbox = TRUE))
    tryCatch({
      r <- POST(paste0("http://localhost:", port, "/session/", session_id, "/execute/sync"),
                body = body_json, encode = "raw",
                add_headers(`Content-Type` = "application/json"), timeout(15))
      fromJSON(content(r, as = "text"))$value
    }, error = function(e) NULL)
  }
  wait_for_page <- function(max_wait = 20) {
    for (i in seq_len(max_wait)) {
      Sys.sleep(1)
      title <- js_eval("return document.title;")
      title <- if (is.null(title)) "" else as.character(title)[1]
      if (length(title) == 1 && !grepl("Just a moment|Checking", title, ignore.case = TRUE) &&
          nchar(title) > 0) return(TRUE)
    }
    FALSE
  }
  dismiss_cookie <- function() {
    js_eval("var b=document.querySelectorAll('button,a');var k=['Reject','Decline','Accept','Agree'];for(var x of b){for(var y of k){if(x.innerText&&x.innerText.trim().toLowerCase().includes(y.toLowerCase())){x.click();return;}}}")
    Sys.sleep(0.5)
  }
  get_html <- function(url) {
    Sys.sleep(runif(1, 1, 2))
    navigate(url)
    Sys.sleep(runif(1, 2, 3))
    wait_for_page(max_wait = 15)
    dismiss_cookie()
    Sys.sleep(0.5)
    get_source()
  }
  
  # Scrape summary page (goal times)
  scrape_summary <- function(match_url) {
    page <- tryCatch(get_html(match_url), error = function(e) NULL)
    if (is.null(page)) return(list(goal_times_home = list(), goal_times_away = list()))
    extract <- function(rows) {
      t <- character()
      for (n in rows) {
        gid <- n %>% html_node("div.smv__incidentIcon")
        if (is.null(gid) || length(gid) == 0) next
        gsvg <- gid %>% html_node("svg[data-testid='wcl-icon-incidents-goal-soccer']")
        if (is.null(gsvg) || length(gsvg) == 0) next
        tm <- n %>% html_node("div.smv__timeBox") %>% html_text(trim = TRUE)
        if (!is.na(tm) && nchar(tm) > 0) t <- c(t, tm)
      }
      t
    }
    home_rows <- page %>% html_nodes("div.smv__participantRow.smv__homeParticipant")
    away_rows <- page %>% html_nodes("div.smv__participantRow.smv__awayParticipant")
    list(goal_times_home = as.list(extract(home_rows)),
         goal_times_away = as.list(extract(away_rows)))
  }
  
  # Scrape stats page
  scrape_stats <- function(match_url) {
    stats_url <- if (grepl("summary/stats/overall", match_url)) match_url
    else sub("(\\?mid=)", "summary/stats/overall/\\1", match_url)
    page <- tryCatch(get_html(stats_url), error = function(e) NULL)
    if (is.null(page)) return(NULL)
    result <- list(stats_home = list(), stats_away = list(),
                   ht_home = NA, ht_away = NA, ft_home = NA, ft_away = NA,
                   goal_times_home = list(), goal_times_away = list(),
                   status = "UNKNOWN")
    rows <- page %>% html_nodes("div.wcl-row_2oCpS")
    for (r in rows) {
      cn <- r %>% html_node("div.wcl-category_6sT1J span")
      if (is.null(cn)) next
      cat_name <- html_text(cn, trim = TRUE)
      if (!(cat_name %in% wanted_stats)) next
      vals <- r %>% html_nodes("div.wcl-value_XJG99 span.wcl-bold_NZXv6")
      if (length(vals) < 2) next
      key <- slug(cat_name)
      result$stats_home[[key]] <- html_text(vals[1], trim = TRUE)
      result$stats_away[[key]] <- html_text(vals[2], trim = TRUE)
    }
    hdr <- page %>% html_nodes("div.detailScore__wrapper, span.detailScore__matchResult")
    if (length(hdr) > 0) {
      txt <- html_text(hdr[1], trim = TRUE)
      nums <- regmatches(txt, gregexpr("\\d+", txt))[[1]]
      if (length(nums) >= 2) {
        result$ft_home <- as.integer(nums[1])
        result$ft_away <- as.integer(nums[2])
        result$status <- "FINAL"
      }
    }
    s <- tryCatch(scrape_summary(match_url), error = function(e)
      list(goal_times_home = list(), goal_times_away = list()))
    result$goal_times_home <- s$goal_times_home
    result$goal_times_away <- s$goal_times_away
    parse_min <- function(t) {
      t <- gsub("[^0-9+]", "", t)
      if (grepl("\\+", t)) { p <- as.integer(strsplit(t, "\\+")[[1]]); p[1] + p[2] }
      else suppressWarnings(as.integer(t))
    }
    if (length(result$goal_times_home) > 0)
      result$ht_home <- sum(sapply(result$goal_times_home,
                                   function(t) parse_min(t) <= 45), na.rm = TRUE)
    else if (!is.na(result$ft_home)) result$ht_home <- 0
    if (length(result$goal_times_away) > 0)
      result$ht_away <- sum(sapply(result$goal_times_away,
                                   function(t) parse_min(t) <= 45), na.rm = TRUE)
    else if (!is.na(result$ft_away)) result$ht_away <- 0
    result
  }
  
  # Start chromedriver
  for (attempt in 1:3) {
    system2(chromedriver_path, args = paste0("--port=", port), wait = FALSE)
    Sys.sleep(3)
    resp <- tryCatch(POST(
      paste0("http://localhost:", port, "/session"),
      body = list(capabilities = list(alwaysMatch = list(
        browserName = "chrome",
        `goog:chromeOptions` = list(
          binary = chrome_path,
          args = list("--no-sandbox", "--disable-dev-shm-usage",
                      "--disable-blink-features=AutomationControlled",
                      "--disable-extensions"),
          excludeSwitches = list("enable-automation"),
          useAutomationExtension = FALSE)))),
      encode = "json", timeout(30)), error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200) {
      sd <- fromJSON(content(resp, as = "text"))
      session_id <- sd$sessionId %||% sd$value$sessionId
      break
    }
    port <- port + 1
  }
  if (is.null(session_id)) {
    return(list(worker_id = worker_id, success = FALSE, error = "Failed to start chromedriver",
                results = list()))
  }
  
  # Warm up
  navigate(paste0(base_url, "/"))
  Sys.sleep(4)
  wait_for_page(30)
  dismiss_cookie()
  Sys.sleep(2)
  
  # Process chunk, checkpoint every 25
  results <- list()
  checkpoint_path <- file.path(progress_dir, paste0("worker_", worker_id, ".rds"))
  progress_path   <- file.path(progress_dir, paste0("worker_", worker_id, "_progress.txt"))
  
  # Resume from existing checkpoint if present
  if (file.exists(checkpoint_path)) {
    results <- tryCatch(readRDS(checkpoint_path), error = function(e) list())
  }
  
  total <- length(work_chunk)
  for (i in seq_along(work_chunk)) {
    w <- work_chunk[[i]]
    # Skip if already done in this worker's checkpoint
    if (!is.null(results[[w$match_id]])) {
      writeLines(paste0(i, "/", total, " (cached in checkpoint)"), progress_path)
      next
    }
    data <- tryCatch(scrape_stats(w$match_url), error = function(e) NULL)
    if (!is.null(data)) {
      data$cache_version <- cache_version
      data$cached_at <- Sys.time()
      results[[w$match_id]] <- data
    }
    writeLines(paste0(i, "/", total), progress_path)
    if (i %% 25 == 0) saveRDS(results, checkpoint_path)
  }
  saveRDS(results, checkpoint_path)
  writeLines(paste0(total, "/", total, " DONE"), progress_path)
  
  # Cleanup
  tryCatch(DELETE(paste0("http://localhost:", port, "/session/", session_id),
                  timeout(10)), error = function(e) invisible())
  
  list(worker_id = worker_id, success = TRUE, error = NULL, results = results,
       checkpoint_path = checkpoint_path)
}

# ════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,500;9..144,700;9..144,900&family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap"),
    tags$style(HTML("
      *, *::before, *::after { margin:0; padding:0; box-sizing:border-box; }
      :root {
        --ivory:#FAF7F2;
        --ivory-2:#F5EEDF;
        --cobalt:#1E3A8A;
        --cobalt-deep:#0F1F4D;
        --gold:#C9A14A;
        --gold-deep:#8C6A1E;
        --text:#0F1F4D;
        --text-2:#5c5448;
        --text-3:#8e8678;
        --border:#e8dfc9;
        --white:#ffffff;
        --success:#0F6E56;
        --success-bg:#E1F5EE;
        --danger:#A32D2D;
        --danger-bg:#FCEBEB;
        --warning:#854F0B;
        --warning-bg:#FAEEDA;
        --fh:'Fraunces',Georgia,serif;
        --fb:'Inter',system-ui,sans-serif;
        --fm:'JetBrains Mono',monospace;
      }
      body { background:var(--ivory); color:var(--text); font-family:var(--fb); }

      /* HEADER */
      .moab-hd { background:var(--cobalt); color:var(--ivory);
                  padding:24px 36px; display:flex; align-items:center;
                  justify-content:space-between; position:relative; overflow:hidden;
                  border-bottom:3px solid var(--gold); }
      .moab-hd::before { content:''; position:absolute; right:-40px; top:-40px;
                          width:200px; height:200px; opacity:0.12;
                          background:radial-gradient(circle, var(--gold) 0%, transparent 70%); }
      .moab-brand { display:flex; align-items:baseline; gap:18px; position:relative; z-index:1; }
      .moab-logo { font-family:var(--fh); font-size:38px; font-weight:900;
                    letter-spacing:-0.5px; line-height:1; color:var(--ivory); }
      .moab-logo .dot { color:var(--gold); }
      .moab-sub { font-family:var(--fb); font-size:10px; font-weight:600;
                    letter-spacing:4px; text-transform:uppercase;
                    color:rgba(250,247,242,0.75); }
      .moab-meta { font-family:var(--fm); font-size:11px; color:rgba(250,247,242,0.8);
                    position:relative; z-index:1; text-align:right; }
      .moab-meta-l { color:var(--gold); font-weight:500; letter-spacing:1px;
                      text-transform:uppercase; font-size:9px; margin-bottom:4px; }

      /* PAGE */
      .pg { max-width:1500px; margin:0 auto; padding:32px 36px; }

      /* TABS */
      .nav-tabs { border:none !important;
                  border-bottom:1px solid var(--border) !important; margin-bottom:32px !important; }
      .nav-tabs > li > a { font-family:var(--fb) !important; font-size:11px !important;
                            font-weight:600 !important; letter-spacing:2.5px !important;
                            text-transform:uppercase !important; color:var(--text-3) !important;
                            border:none !important; padding:14px 28px !important;
                            border-bottom:2px solid transparent !important;
                            background:transparent !important; }
      .nav-tabs > li.active > a { color:var(--cobalt) !important;
                                   border-bottom-color:var(--gold) !important; }

      /* CARDS */
      .card-elite { background:var(--white); border:1px solid var(--border);
                      border-radius:6px; padding:28px 30px; margin-bottom:18px;
                      position:relative; box-shadow:0 1px 3px rgba(15,31,77,0.04); }
      .card-elite::before { content:''; position:absolute; top:0; left:0; width:48px; height:2px;
                              background:var(--gold); }
      .ct { font-family:var(--fb); font-size:10px; font-weight:700; letter-spacing:3px;
            text-transform:uppercase; color:var(--text-3); margin-bottom:16px; }
      .card-eyebrow { font-family:var(--fb); font-size:9px; letter-spacing:3px;
                       text-transform:uppercase; color:var(--gold-deep); font-weight:700; }

      /* BUTTONS */
      .btn-primary-moab { font-family:var(--fb) !important; font-size:11px !important;
                          font-weight:600 !important; letter-spacing:2px !important;
                          text-transform:uppercase !important;
                          background:var(--cobalt) !important; color:var(--ivory) !important;
                          border:none !important; border-radius:3px !important;
                          padding:12px 28px !important; cursor:pointer !important;
                          box-shadow:0 2px 8px rgba(30,58,138,0.18) !important;
                          transition:all 0.2s !important; }
      .btn-primary-moab:hover { background:var(--cobalt-deep) !important;
                                 box-shadow:0 4px 16px rgba(30,58,138,0.3) !important; }
      .btn-outline-moab { font-family:var(--fb) !important; font-size:10px !important;
                          font-weight:600 !important; letter-spacing:2px !important;
                          text-transform:uppercase !important; background:transparent !important;
                          color:var(--cobalt) !important;
                          border:1px solid var(--cobalt) !important;
                          border-radius:3px !important; padding:9px 22px !important;
                          cursor:pointer !important; }
      .btn-outline-moab:hover { background:var(--cobalt) !important; color:var(--ivory) !important; }
      .btn-gold { font-family:var(--fb) !important; font-size:10px !important;
                  font-weight:600 !important; letter-spacing:2px !important;
                  text-transform:uppercase !important;
                  background:var(--gold) !important; color:var(--cobalt-deep) !important;
                  border:none !important; border-radius:3px !important;
                  padding:9px 22px !important; cursor:pointer !important; }
      .btn-gold:hover { background:var(--gold-deep) !important; color:var(--ivory) !important; }

      /* KPI */
      .kpi-grid { display:grid; grid-template-columns:repeat(4,1fr); gap:14px; margin-bottom:24px; }
      .kpi-card { background:var(--white); border:1px solid var(--border); border-radius:6px;
                  padding:22px 24px; position:relative; }
      .kpi-card::before { content:''; position:absolute; top:0; left:0; width:32px; height:2px;
                            background:var(--cobalt); }
      .kpi-num { font-family:var(--fh); font-size:40px; font-weight:900; line-height:1;
                  color:var(--cobalt); letter-spacing:-1px; }
      .kpi-lab { font-family:var(--fm); font-size:9px; font-weight:500; letter-spacing:2.5px;
                  text-transform:uppercase; color:var(--text-3); margin-top:10px; }

      /* TABLES */
      table.dataTable thead th { background:var(--ivory-2) !important; font-size:10px !important;
                                  font-weight:700 !important; letter-spacing:2px !important;
                                  text-transform:uppercase !important; color:var(--text-2) !important;
                                  font-family:var(--fb) !important; padding:14px 16px !important;
                                  border-bottom:1px solid var(--border) !important; }
      table.dataTable tbody td { font-size:13px !important; padding:11px 16px !important;
                                  font-family:var(--fb) !important;
                                  border-bottom:1px solid var(--border) !important; }
      table.dataTable tbody tr:hover td { background:var(--ivory-2) !important; cursor:pointer; }

      /* INPUTS */
      .form-control, .selectize-input { font-family:var(--fb) !important; font-size:13px !important;
                                          border:1px solid var(--border) !important;
                                          border-radius:3px !important;
                                          padding:8px 12px !important; }
      .shiny-input-container label { font-family:var(--fb) !important; font-size:10px !important;
                                       font-weight:600 !important; letter-spacing:2px !important;
                                       text-transform:uppercase !important;
                                       color:var(--text-2) !important; margin-bottom:6px !important; }

      /* LOG */
      .log-pane { background:var(--cobalt-deep); border-radius:4px; padding:16px 18px;
                   font-family:var(--fm); font-size:11px; color:var(--gold);
                   height:280px; overflow-y:auto; white-space:pre-wrap; line-height:1.8; }

      /* BADGES */
      .pill { display:inline-block; font-family:var(--fm); font-size:10px; font-weight:500;
              letter-spacing:1px; padding:4px 12px; border-radius:12px; border:1px solid; }
      .pill-gold { background:#FDF7E7; color:var(--gold-deep); border-color:#E5D4A0; }
      .pill-cobalt { background:#E8EEF8; color:var(--cobalt-deep); border-color:#BCC8E0; }
      .pill-success { background:var(--success-bg); color:var(--success); border-color:#9FE1CB; }
      .pill-danger { background:var(--danger-bg); color:var(--danger); border-color:#F7C1C1; }
      .pill-warning { background:var(--warning-bg); color:var(--warning); border-color:#FAC775; }

      /* PROGRESS */
      .progress-track { width:100%; height:8px; background:var(--ivory-2);
                         border-radius:4px; overflow:hidden; margin:12px 0; }
      .progress-fill { height:100%; background:linear-gradient(90deg, var(--cobalt) 0%, var(--gold) 100%);
                        transition:width 0.3s; }

      /* SECTION LABEL */
      .section-lab { font-family:var(--fh); font-size:24px; font-weight:700;
                      color:var(--cobalt-deep); letter-spacing:-0.5px; margin-bottom:6px; }
      .section-sub { font-family:var(--fb); font-size:13px; color:var(--text-3);
                      margin-bottom:24px; }

      /* SHINY NOTIFICATIONS */
      .shiny-notification { font-family:var(--fb) !important; border-radius:4px !important;
                             border:1px solid var(--border) !important; }
    "))
  ),
  div(class = "moab-hd",
      div(class = "moab-brand",
          div(class = "moab-logo", "M.O", tags$span(class = "dot", "."), "A.B"),
          div(class = "moab-sub", "Mother of all Boards")),
      div(class = "moab-meta",
          div(class = "moab-meta-l", "Football intelligence · v2"),
          textOutput("hdr_status", inline = TRUE))),
  div(class = "pg",
      tabsetPanel(id = "main_tabs",
                  # ─────────────── PIPELINE ───────────────
                  tabPanel("Pipeline",
                           div(style = "padding-top:8px;",
                               div(class = "section-lab", "Scrape pipeline"),
                               div(class = "section-sub",
                                   "Fetch fixtures, standings, form and head-to-head across all verified leagues."),
                               uiOutput("pipeline_kpis"),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Controls"),
                                   div(style = "display:flex; gap:14px; align-items:center; margin-top:14px;",
                                       actionButton("fetch_btn", "Fetch data", class = "btn-primary-moab"),
                                       actionButton("reload_btn", "Reload from disk", class = "btn-outline-moab"),
                                       div(style = "flex:1;"),
                                       checkboxInput("force_refresh", "Force refresh cache", value = FALSE))
                               ),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Activity log"),
                                   div(class = "log-pane", textOutput("log_text"))
                               )
                           )
                  ),
                  # ─────────────── DATA ───────────────
                  tabPanel("Data",
                           div(style = "padding-top:8px;",
                               div(class = "section-lab", "Enriched fixtures"),
                               div(class = "section-sub",
                                   "Browse fixtures with form, head-to-head, and per-match statistics."),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Fixtures in window"),
                                   div(style = "margin-top:14px;", DTOutput("fixtures_table"))
                               ),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Standings preview"),
                                   div(style = "margin-top:14px;", DTOutput("standings_table"))
                               )
                           )
                  ),
                  # ─────────────── PREDICTIONS ───────────────
                  tabPanel("Predictions",
                           div(style = "padding-top:8px;",
                               div(class = "section-lab", "Market predictions"),
                               div(class = "section-sub",
                                   "Coming next — multi-market analysis layer."),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Status"),
                                   div(style = "margin-top:14px; font-family:var(--fb); font-size:13px; color:var(--text-2);",
                                       "Predictions layer will be built after scrape pipeline is validated end-to-end. ",
                                       "Markets in scope: HT Draw, FH O0.5, Total Goals, BTTS, Cards, Corners, Shots on Target."))
                           )
                  ),
                  # ─────────────── SETTINGS ───────────────
                  tabPanel("Settings",
                           div(style = "padding-top:8px;",
                               div(class = "section-lab", "Settings"),
                               div(class = "section-sub", "Paths, cache, and run options."),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Storage paths"),
                                   tags$table(style = "width:100%; margin-top:12px; font-family:var(--fm); font-size:12px;",
                                              tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3); width:200px;", "Base path"),
                                                      tags$td(style = "padding:8px 0;", BASE_PATH)),
                                              tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3);", "Cache"),
                                                      tags$td(style = "padding:8px 0;", CACHE_PATH)),
                                              tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3);", "Directory"),
                                                      tags$td(style = "padding:8px 0;", DIRECTORY_PATH)),
                                              tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3);", "Enriched"),
                                                      tags$td(style = "padding:8px 0;", ENRICHED_PATH)))
                               ),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Cache statistics"),
                                   div(style = "margin-top:14px;", uiOutput("cache_stats"))
                               ),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Maintenance"),
                                   div(style = "margin-top:14px; display:flex; gap:12px;",
                                       actionButton("clear_cache_btn", "Clear cache", class = "btn-outline-moab"),
                                       actionButton("clear_enriched_btn", "Clear enriched fixtures", class = "btn-outline-moab"))
                               )
                           )
                  )
      )
  )
)

# ════════════════════════════════════════════════════════════
# SERVER
# ════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  rv <- reactiveValues(
    log = "Ready. Click Fetch data to begin.\n",
    directory = NULL,
    fixtures  = NULL,
    standings = NULL,
    enriched  = NULL,
    cache     = NULL,
    last_run  = NULL
  )
  
  log_msg <- function(msg) {
    ts <- format(Sys.time(), "%H:%M:%S")
    rv$log <- paste0(rv$log, "[", ts, "] ", msg, "\n")
  }
  
  # Load existing files ONCE on startup using isolate so it doesn't re-fire
  isolate({
    if (file.exists(DIRECTORY_PATH))
      rv$directory <- tryCatch(readRDS(DIRECTORY_PATH), error = function(e) NULL)
    if (file.exists(ENRICHED_PATH))
      rv$enriched <- tryCatch(readRDS(ENRICHED_PATH), error = function(e) NULL)
    rv$cache <- load_cache()
    dir_n <- if (is.null(rv$directory)) 0 else nrow(rv$directory)
    enr_n <- if (is.null(rv$enriched)) 0 else length(rv$enriched)
    log_msg(paste0("Loaded directory (", dir_n, " entries), ",
                   "cache (", length(rv$cache), " matches), ",
                   "enriched (", enr_n, " fixtures)."))
  })
  
  output$log_text   <- renderText({ rv$log })
  output$hdr_status <- renderText({
    if (!is.null(rv$enriched)) paste0(length(rv$enriched), " fixtures · ",
                                      length(rv$cache), " cached matches")
    else paste0(length(rv$cache), " cached matches")
  })
  
  output$pipeline_kpis <- renderUI({
    verified <- if (!is.null(rv$directory))
      sum(rv$directory$verified == TRUE, na.rm = TRUE) else 0
    div(class = "kpi-grid",
        div(class = "kpi-card", div(class = "kpi-num", verified),
            div(class = "kpi-lab", "Verified leagues")),
        div(class = "kpi-card", div(class = "kpi-num", length(rv$cache)),
            div(class = "kpi-lab", "Cached matches")),
        div(class = "kpi-card",
            div(class = "kpi-num", length(rv$enriched %||% list())),
            div(class = "kpi-lab", "Enriched fixtures")),
        div(class = "kpi-card",
            div(class = "kpi-num", style = "font-size:16px; padding-top:14px;",
                if (is.null(rv$last_run)) "Never"
                else format(rv$last_run, "%d %b %H:%M")),
            div(class = "kpi-lab", "Last run")))
  })
  
  output$cache_stats <- renderUI({
    n <- length(rv$cache)
    sz <- if (file.exists(CACHE_PATH))
      paste0(round(file.info(CACHE_PATH)$size / 1024, 1), " KB")
    else "0 KB"
    tags$table(style = "width:100%; font-family:var(--fm); font-size:12px;",
               tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3); width:200px;", "Entries"),
                       tags$td(style = "padding:8px 0;", n)),
               tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3);", "File size"),
                       tags$td(style = "padding:8px 0;", sz)),
               tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3);", "Version"),
                       tags$td(style = "padding:8px 0;", CACHE_VERSION)))
  })
  
  observeEvent(input$reload_btn, {
    if (file.exists(ENRICHED_PATH))
      rv$enriched <- tryCatch(readRDS(ENRICHED_PATH), error = function(e) NULL)
    rv$cache <- load_cache()
    log_msg(paste0("Reloaded from disk: ", length(rv$enriched %||% list()),
                   " fixtures, ", length(rv$cache), " cache entries."))
    showNotification("Reloaded from disk", type = "message", duration = 2)
  })
  
  observeEvent(input$clear_cache_btn, {
    showModal(modalDialog(title = "Clear cache?",
                          "This will delete all cached match data. Subsequent scrapes will re-fetch everything.",
                          footer = tagList(modalButton("Cancel"),
                                           actionButton("clear_cache_confirm", "Clear", class = "btn-primary-moab")),
                          easyClose = TRUE))
  })
  observeEvent(input$clear_cache_confirm, {
    rv$cache <- list()
    if (file.exists(CACHE_PATH)) file.remove(CACHE_PATH)
    log_msg("Cache cleared.")
    removeModal()
    showNotification("Cache cleared", type = "warning", duration = 3)
  })
  
  observeEvent(input$clear_enriched_btn, {
    rv$enriched <- NULL
    if (file.exists(ENRICHED_PATH)) file.remove(ENRICHED_PATH)
    log_msg("Enriched fixtures cleared.")
    showNotification("Enriched cleared", type = "warning", duration = 3)
  })
  
  # ── FETCH ──
  observeEvent(input$fetch_btn, {
    log_msg("Fetch button clicked.")
    showNotification("Pipeline starting...", type = "message", duration = 3)
    if (is.null(rv$directory)) {
      log_msg("ERROR: No league_directory.rds found at " %||% DIRECTORY_PATH)
      log_msg(paste0("ERROR: No league_directory.rds at ", DIRECTORY_PATH))
      showNotification("Directory missing - run directory + verify scripts first",
                       type = "error", duration = 6)
      return()
    }
    log_msg(paste0("Directory loaded: ", nrow(rv$directory), " entries."))
    verified <- rv$directory %>% filter(verified == TRUE, !is.na(verified))
    if (nrow(verified) == 0) {
      log_msg("ERROR: No verified leagues found in directory.")
      showNotification("No verified leagues", type = "error", duration = 4)
      return()
    }
    log_msg(paste0("Verified leagues: ", nrow(verified)))
    today <- Sys.Date(); horizon <- today + 7
    log_msg(paste0("Pipeline started. Targeting ", nrow(verified), " leagues."))
    
    tryCatch({
      # Phase A: parallel fixtures + standings scrape using future workers
      log_msg(paste0("Spawning ", N_PARALLEL_WORKERS,
                     " parallel workers for fixtures + standings..."))
      n_workers_a <- min(N_PARALLEL_WORKERS, nrow(verified))
      league_chunks <- split(seq_len(nrow(verified)),
                             cut(seq_len(nrow(verified)), n_workers_a, labels = FALSE))
      
      chrome_p <- CHROME_PATH; cd_p <- CHROMEDRIVER_PATH
      base_u <- BASE_URL; prog_dir <- WORKER_PROGRESS_DIR
      
      plan(multisession, workers = n_workers_a)
      
      for (i in seq_len(n_workers_a)) {
        pf <- file.path(WORKER_PROGRESS_DIR, paste0("worker_a_", i, "_progress.txt"))
        if (file.exists(pf)) file.remove(pf)
      }
      
      futures_a <- list()
      for (i in seq_len(n_workers_a)) {
        local_leagues <- verified[league_chunks[[i]], ]
        startup_delay <- (i - 1) * runif(1, 5, 10)
        futures_a[[i]] <- future({
          Sys.sleep(startup_delay)
          run_league_worker(i, local_leagues, chrome_p, cd_p, base_u, prog_dir)
        }, seed = TRUE,
        globals = list(i = i, local_leagues = local_leagues,
                       startup_delay = startup_delay,
                       chrome_p = chrome_p, cd_p = cd_p, base_u = base_u,
                       prog_dir = prog_dir,
                       run_league_worker = run_league_worker))
      }
      
      withProgress(message = "Fixtures + standings (parallel)", value = 0, {
        done <- rep(FALSE, n_workers_a)
        while (!all(done)) {
          Sys.sleep(5)
          total_progress <- 0
          for (i in seq_len(n_workers_a)) {
            pf <- file.path(WORKER_PROGRESS_DIR, paste0("worker_a_", i, "_progress.txt"))
            if (file.exists(pf)) {
              txt <- tryCatch(readLines(pf, warn = FALSE)[1], error = function(e) "")
              if (grepl("DONE", txt)) done[i] <- TRUE
              m <- regmatches(txt, regexpr("(\\d+)/(\\d+)", txt))
              if (length(m) > 0) {
                parts <- as.integer(strsplit(m, "/")[[1]])
                if (length(parts) == 2 && parts[2] > 0)
                  total_progress <- total_progress + (parts[1] / parts[2])
              }
            }
            if (resolved(futures_a[[i]])) done[i] <- TRUE
          }
          avg <- total_progress / n_workers_a
          setProgress(value = avg,
                      detail = paste0(round(avg * 100), "% across ", n_workers_a, " workers"))
          if (all(done)) break
        }
      })
      
      log_msg("Workers finished. Merging fixtures and standings...")
      all_fixtures <- data.frame(); all_standings <- data.frame()
      for (i in seq_len(n_workers_a)) {
        res <- tryCatch(value(futures_a[[i]]), error = function(e) {
          log_msg(paste0("Worker ", i, " error: ", e$message))
          list(fixtures = data.frame(), standings = data.frame())
        })
        if (nrow(res$fixtures) > 0) all_fixtures <- rbind(all_fixtures, res$fixtures)
        if (nrow(res$standings) > 0) all_standings <- rbind(all_standings, res$standings)
      }
      plan(sequential)
      log_msg(paste0("Collected fixtures: ", nrow(all_fixtures),
                     " | standings: ", nrow(all_standings)))
      
      rv$fixtures <- all_fixtures
      rv$standings <- all_standings
      log_msg(paste0("Collected ", nrow(all_fixtures), " fixtures and ",
                     nrow(all_standings), " standings rows."))
      
      in_window <- all_fixtures %>%
        filter(!is.na(fixture_date), fixture_date >= today, fixture_date <= horizon) %>%
        arrange(fixture_date, match_time)
      log_msg(paste0(nrow(in_window), " fixtures in 7-day window."))
      
      enriched <- list(); cache <- rv$cache
      # Build a flat work queue of (fixture_idx, h2h_data) by first scraping
      # all h2h pages in parallel across tabs, then enriching each form game in parallel.
      if (nrow(in_window) > 0) {
        total_tabs <- length(SEL_POOL) * N_TABS
        log_msg(paste0("Parallel enrichment across ", total_tabs, " tabs (",
                       length(SEL_POOL), " windows x ", N_TABS, " tabs)."))
        
        # Phase H2H: scrape H2H pages in parallel
        log_msg("Phase H2H: scraping H2H pages in parallel...")
        n_workers_h <- min(N_PARALLEL_WORKERS, nrow(in_window))
        fix_chunks <- split(seq_len(nrow(in_window)),
                            cut(seq_len(nrow(in_window)), n_workers_h, labels = FALSE))
        
        chrome_p <- CHROME_PATH; cd_p <- CHROMEDRIVER_PATH
        base_u <- BASE_URL; prog_dir <- WORKER_PROGRESS_DIR
        
        for (i in seq_len(n_workers_h)) {
          pf <- file.path(WORKER_PROGRESS_DIR, paste0("worker_h_", i, "_progress.txt"))
          if (file.exists(pf)) file.remove(pf)
        }
        
        plan(multisession, workers = n_workers_h)
        futures_h <- list()
        for (i in seq_len(n_workers_h)) {
          local_fix <- in_window[fix_chunks[[i]], ]
          startup_delay <- (i - 1) * runif(1, 5, 10)
          futures_h[[i]] <- future({
            Sys.sleep(startup_delay)
            run_h2h_worker(i, local_fix, chrome_p, cd_p, base_u, prog_dir)
          }, seed = TRUE,
          globals = list(i = i, local_fix = local_fix,
                         startup_delay = startup_delay,
                         chrome_p = chrome_p, cd_p = cd_p, base_u = base_u,
                         prog_dir = prog_dir,
                         run_h2h_worker = run_h2h_worker))
        }
        
        withProgress(message = "H2H pages (parallel)", value = 0, {
          done <- rep(FALSE, n_workers_h)
          while (!all(done)) {
            Sys.sleep(5)
            total_prog <- 0
            for (i in seq_len(n_workers_h)) {
              pf <- file.path(WORKER_PROGRESS_DIR, paste0("worker_h_", i, "_progress.txt"))
              if (file.exists(pf)) {
                txt <- tryCatch(readLines(pf, warn = FALSE)[1], error = function(e) "")
                if (grepl("DONE", txt)) done[i] <- TRUE
                m <- regmatches(txt, regexpr("(\\d+)/(\\d+)", txt))
                if (length(m) > 0) {
                  parts <- as.integer(strsplit(m, "/")[[1]])
                  if (length(parts) == 2 && parts[2] > 0)
                    total_prog <- total_prog + (parts[1] / parts[2])
                }
              }
              if (resolved(futures_h[[i]])) done[i] <- TRUE
            }
            avg <- total_prog / n_workers_h
            setProgress(value = avg,
                        detail = paste0(round(avg * 100), "% across ", n_workers_h, " workers"))
            if (all(done)) break
          }
        })
        
        # Collect H2H results: each worker returns list of (fixture_idx -> h2h data)
        h2h_results <- vector("list", nrow(in_window))
        for (i in seq_len(n_workers_h)) {
          res <- tryCatch(value(futures_h[[i]]), error = function(e) {
            log_msg(paste0("H2H worker ", i, " error: ", e$message))
            list()
          })
          chunk_indices <- fix_chunks[[i]]
          for (j in seq_along(chunk_indices)) {
            h2h_results[[chunk_indices[j]]] <- res[[j]]
          }
        }
        plan(sequential)
        log_msg(paste0("H2H scraping complete: ",
                       sum(sapply(h2h_results, function(x) !is.null(x))),
                       "/", nrow(in_window), " successful."))
        
        # Phase B: collect ALL unique form/h2h match URLs to scrape
        log_msg("Phase B: building work queue of form/h2h match details...")
        work_queue <- list()  # each: list(match_id, match_url, fixture_idx, side, entry_idx)
        for (k in seq_len(nrow(in_window))) {
          h <- h2h_results[[k]]
          if (is.null(h)) next
          fx <- in_window[k, ]
          add_to_queue <- function(form_list, side, fx_idx) {
            for (j in seq_along(form_list)) {
              entry <- form_list[[j]]
              if (is.null(entry$match_id) || is.na(entry$match_id)) next
              if (cache_hit(cache, entry$match_id, input$force_refresh)) next
              work_queue[[length(work_queue) + 1]] <<- list(
                match_id = entry$match_id, match_url = entry$match_url,
                fixture_idx = fx_idx, side = side, entry_idx = j)
            }
          }
          add_to_queue(h$home_form, "home", k)
          add_to_queue(h$away_form, "away", k)
        }
        log_msg(paste0("Need to scrape ", length(work_queue), " match detail pages (cache hits skipped)."))
        
        # Phase C: parallel scrape via future::multisession workers
        if (length(work_queue) > 0) {
          
          # Split work_queue into N chunks
          n_workers <- min(N_PARALLEL_WORKERS, length(work_queue))
          chunks <- split(work_queue,
                          cut(seq_along(work_queue), n_workers, labels = FALSE))
          log_msg(paste0("Spawning ", n_workers, " parallel workers (", 
                         round(length(work_queue) / n_workers),
                         " matches each)..."))
          
          # Set up future plan
          plan(multisession, workers = n_workers)
          
          # Capture vars for workers
          chrome_p <- CHROME_PATH; cd_p <- CHROMEDRIVER_PATH
          base_u <- BASE_URL; prog_dir <- WORKER_PROGRESS_DIR
          cv <- CACHE_VERSION; ws <- WANTED_STATS
          
          # Submit all worker jobs
          futures <- list()
          for (i in seq_len(n_workers)) {
            local_chunk <- chunks[[i]]
            futures[[i]] <- future({
              run_parallel_worker(i, local_chunk, chrome_p, cd_p,
                                  base_u, prog_dir, cv, ws)
            }, seed = TRUE,
            globals = list(i = i, local_chunk = local_chunk,
                           chrome_p = chrome_p, cd_p = cd_p, base_u = base_u,
                           prog_dir = prog_dir, cv = cv, ws = ws,
                           run_parallel_worker = run_parallel_worker))
          }
          
          # Poll progress files while workers run
          withProgress(message = "Parallel workers running", value = 0, {
            done <- rep(FALSE, n_workers)
            while (!all(done)) {
              Sys.sleep(5)
              total_progress <- 0
              for (i in seq_len(n_workers)) {
                pf <- file.path(WORKER_PROGRESS_DIR, paste0("worker_", i, "_progress.txt"))
                if (file.exists(pf)) {
                  txt <- tryCatch(readLines(pf, warn = FALSE)[1], error = function(e) "")
                  if (grepl("DONE", txt)) done[i] <- TRUE
                  m <- regmatches(txt, regexpr("(\\d+)/(\\d+)", txt))
                  if (length(m) > 0) {
                    parts <- as.integer(strsplit(m, "/")[[1]])
                    if (length(parts) == 2 && parts[2] > 0) {
                      total_progress <- total_progress + (parts[1] / parts[2])
                    }
                  }
                }
                if (resolved(futures[[i]])) done[i] <- TRUE
              }
              avg <- total_progress / n_workers
              setProgress(value = avg,
                          detail = paste0(round(avg * 100), "% across ", n_workers, " workers"))
              if (all(done)) break
            }
          })
          
          # Collect all worker results
          log_msg("All workers finished. Merging results...")
          all_results <- list()
          for (i in seq_len(n_workers)) {
            res <- tryCatch(value(futures[[i]]), error = function(e) {
              log_msg(paste0("Worker ", i, " error: ", e$message))
              list(success = FALSE, results = list())
            })
            if (!is.null(res$success) && res$success) {
              all_results <- c(all_results, res$results)
              log_msg(paste0("Worker ", i, ": ", length(res$results), " matches scraped"))
            }
          }
          
          # Merge into main cache
          for (mid in names(all_results)) {
            d <- all_results[[mid]]
            if (is_valid_cache_entry(d)) cache[[mid]] <- d
          }
          save_cache(cache)
          log_msg(paste0("Merged ", length(all_results), " matches into main cache."))
          
          # Cleanup worker progress files but KEEP checkpoint RDS for safety
          for (i in seq_len(n_workers)) {
            pf <- file.path(WORKER_PROGRESS_DIR, paste0("worker_", i, "_progress.txt"))
            if (file.exists(pf)) file.remove(pf)
          }
          
          # Reset future plan
          plan(sequential)
          
          # Restart sequential pool for Phase D (stitching uses cache only, no scraping)
          # Actually Phase D doesn't scrape - it just reads cache. No pool needed.
        }
        
        # Phase D: stitch enriched fixtures using cache (which now has everything)
        log_msg("Phase D: stitching enriched fixtures...")
        for (k in seq_len(nrow(in_window))) {
          h <- h2h_results[[k]]
          if (is.null(h)) next
          fx <- in_window[k, ]
          enrich_side <- function(form_list, team_name) {
            out <- list()
            for (e in form_list) {
              r <- enrich_form_entry(e, team_name, cache, FALSE)
              out[[length(out) + 1]] <- r$entry
            }
            out
          }
          enriched[[k]] <- list(
            fixture   = fx,
            home_form = enrich_side(h$home_form, fx$home),
            away_form = enrich_side(h$away_form, fx$away),
            h2h       = h$h2h
          )
        }
        saveRDS(enriched, ENRICHED_PATH)
      }
      # Pool already shut down inside Phase C; just save final state
      save_cache(cache); saveRDS(enriched, ENRICHED_PATH)
      rv$cache <- cache; rv$enriched <- enriched; rv$last_run <- Sys.time()
      log_msg(paste0("Pipeline complete. ", length(enriched), " fixtures enriched."))
      showNotification("Pipeline complete", type = "message", duration = 4)
    }, error = function(e) {
      tryCatch(stop_selenium(), error = function(e2) invisible())
      log_msg(paste0("ERROR: ", e$message))
      showNotification(paste0("Error: ", e$message), type = "error", duration = 6)
    })
  })
  
  # ── DATA TABLES ──
  output$fixtures_table <- renderDT({
    req(rv$enriched)
    if (length(rv$enriched) == 0) return(datatable(data.frame(Message = "No data")))
    rows <- lapply(rv$enriched, function(e) {
      if (is.null(e)) return(NULL)
      fx <- e$fixture
      data.frame(
        Date = as.character(fx$fixture_date), Time = fx$match_time,
        League = fx$league, Home = fx$home, Away = fx$away,
        HomeForm = paste0(length(e$home_form), " games"),
        AwayForm = paste0(length(e$away_form), " games"),
        H2H = paste0(length(e$h2h), " games"),
        stringsAsFactors = FALSE)
    })
    d <- do.call(rbind, Filter(Negate(is.null), rows))
    if (is.null(d) || nrow(d) == 0) return(datatable(data.frame(Message = "No data")))
    datatable(d, rownames = FALSE, selection = "none",
              options = list(pageLength = 25, dom = "ftp", scrollX = TRUE))
  }, server = FALSE)
  
  output$standings_table <- renderDT({
    req(rv$standings)
    if (is.null(rv$standings) || nrow(rv$standings) == 0)
      return(datatable(data.frame(Message = "Standings refresh on next fetch")))
    datatable(rv$standings %>%
                select(league, rank, team, mp, w, d, l, goals_for, goals_against, gd, pts, form),
              rownames = FALSE, selection = "none", colnames = c("League","#","Team","MP","W","D","L","GF","GA","GD","Pts","Form"),
              options = list(pageLength = 25, dom = "ftp", scrollX = TRUE))
  }, server = FALSE)
}

shinyApp(ui = ui, server = server)