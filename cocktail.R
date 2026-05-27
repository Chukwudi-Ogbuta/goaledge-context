library(shiny)
library(DT)
library(dplyr)
library(openxlsx)
library(jsonlite)
library(rvest)
library(httr)
library(stringdist)

BASE_PATH      <- "C:/Users/Ogbuta/OneDrive/New Projects"
PIPELINE_PATH  <- file.path(BASE_PATH, "WorldFootball.R")
HISTORY_PATH   <- file.path(BASE_PATH, "cocktail_history.json")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ════════════════════════════════════════════════════════════
# DEFAULT SETTINGS
# ════════════════════════════════════════════════════════════

DEFAULT_THRESHOLDS <- list(
  fh_over05  = 0.945,
  over15     = 0.92,
  btts       = 0.73,
  ht_draw    = 0.78,
  under45    = 0.97
)

DEFAULT_PRIORITY <- c("HT Draw", "BTTS", "Under 4.5 FT", "Over 1.5 FT", "FH Over 0.5")

# ════════════════════════════════════════════════════════════
# HELPERS
# ════════════════════════════════════════════════════════════

read_raw_excel <- function(path, sheet = 1) {
  tryCatch({
    d <- read.xlsx(path, sheet = sheet)
    names(d) <- trimws(names(d))
    names(d) <- gsub("\\.", "_", names(d))
    names(d) <- sapply(names(d), function(nm) {
      if (grepl("^(H[1-5]|A[1-5]|H2H[1-5])$", toupper(nm))) toupper(nm)
      else tolower(nm)
    })
    d
  }, error = function(e) NULL)
}

load_history <- function() {
  if (!file.exists(HISTORY_PATH)) return(list())
  tryCatch(fromJSON(HISTORY_PATH, simplifyDataFrame = FALSE), error = function(e) list())
}

save_history <- function(h) {
  tryCatch(write(toJSON(h, auto_unbox = TRUE, pretty = TRUE), HISTORY_PATH),
           error = function(e) invisible())
}

parse_excel_date <- function(x) {
  if (is.na(x) || x == "" || x == "NA") return(as.Date(NA))
  n <- suppressWarnings(as.numeric(x))
  if (!is.na(n) && n > 1000) return(as.Date(n, origin = "1899-12-30"))
  tryCatch(as.Date(x), error = function(e) as.Date(NA))
}

# Smart team similarity (matches production behaviour)
smart_sim <- function(a, b) {
  a <- tolower(trimws(a)); b <- tolower(trimws(b))
  cf <- function(x) gsub("\\s*(fc|afc|sc|ac|bk|if|fk|sk|il|ok|cf|cd|ud|sd|vfl|vfb|fsv|tsv|sv)\\s*$", "", x, ignore.case = TRUE)
  a <- trimws(cf(a)); b <- trimws(cf(b))
  jw <- 1 - stringdist(a, b, method = "jw")
  ta <- unlist(strsplit(a, "\\s+")); tb <- unlist(strsplit(b, "\\s+"))
  tok <- if (max(length(ta), length(tb)) > 0) length(intersect(ta, tb)) / max(length(ta), length(tb)) else 0
  sh <- if (nchar(a) <= nchar(b)) a else b
  lo <- if (nchar(a) <= nchar(b)) b else a
  sub <- if (nchar(sh) >= 3 && grepl(sh, lo, fixed = TRUE)) 0.85 else 0
  max(jw, tok, sub)
}

eval_market <- function(home_ft, away_ft, home_ht, away_ht, market) {
  if (is.na(home_ft) || is.na(away_ft)) return("PENDING")
  ft <- home_ft + away_ft
  if (market == "FH Over 0.5") {
    if (is.na(home_ht) || is.na(away_ht)) return("PENDING")
    return(if ((home_ht + away_ht) >= 1) "WIN" else "LOSS")
  }
  if (market == "Over 1.5 FT")  return(if (ft >= 2) "WIN" else "LOSS")
  if (market == "Under 4.5 FT") return(if (ft <= 4) "WIN" else "LOSS")
  if (market == "BTTS")         return(if (home_ft >= 1 && away_ft >= 1) "WIN" else "LOSS")
  if (market == "HT Draw") {
    if (is.na(home_ht) || is.na(away_ht)) return("PENDING")
    return(if (home_ht == away_ht) "WIN" else "LOSS")
  }
  "PENDING"
}



# ════════════════════════════════════════════════════════════
# JSON safety helpers
# ════════════════════════════════════════════════════════════

# Force any JSON-loaded structure back to a proper data.frame
to_df <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  if (is.list(x) && length(x) > 0 && is.list(x[[1]])) {
    tryCatch({
      # Get the union of all column names across rows
      all_cols <- unique(unlist(lapply(x, names)))
      # Build each row as a named list, filling missing cols with NA
      rows <- lapply(x, function(row) {
        vals <- setNames(vector("list", length(all_cols)), all_cols)
        for (col in all_cols) {
          v <- row[[col]]
          vals[[col]] <- if (is.null(v) || length(v) == 0) NA else v[[1]]
        }
        as.data.frame(vals, stringsAsFactors = FALSE)
      })
      do.call(rbind, rows)
    }, error = function(e) { message("to_df error: ", e$message); NULL })
  } else {
    tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
  }
}

# ════════════════════════════════════════════════════════════
# SCRAPE FIXTURE RESULT (embedded from live_checker)
# ════════════════════════════════════════════════════════════

scrape_fixture_result <- function(home, away, fixture_date, home_href, competition_id=NULL) {
  
  if (is.na(home_href) || is.null(home_href) || nchar(trimws(home_href)) == 0)
    return(list(status="ERROR", result="No home_href available"))
  
  url  <- paste0("https://www.worldfootball.net",
                 gsub("/$","",trimws(home_href)), "/all-matches/")
  page <- tryCatch(selenium_get_html(url), error=function(e) NULL)
  if (is.null(page)) return(list(status="ERROR", result="Page load failed"))
  
  all_rows <- page %>% html_nodes("div.module-gameplan table tr")
  if (length(all_rows) == 0) return(list(status="PENDING", result=NA_character_))
  
  if (!is.null(competition_id) && !is.na(competition_id) && nchar(trimws(as.character(competition_id))) > 0) {
    comp_id_str <- as.character(competition_id)
    rows <- all_rows[sapply(all_rows, function(r) {
      row_comp <- html_attr(r, "data-competition_id") %||% ""
      row_comp == comp_id_str
    })]
    if (length(rows) == 0) rows <- all_rows
  } else {
    rows <- all_rows
  }
  
  played_rows <- rows[sapply(rows, function(r) {
    cls <- html_attr(r, "class") %||% ""
    (grepl("finished", cls) || grepl("live", cls)) && grepl("match", cls)
  })]
  upcoming_rows <- rows[sapply(rows, function(r) {
    cls <- html_attr(r, "class") %||% ""
    grepl("upcoming", cls) && grepl("match", cls)
  })]
  
  target_date <- tryCatch(as.Date(fixture_date), error=function(e) NA)
  for (up_row in upcoming_rows) {
    up_date_str <- tryCatch(up_row %>% html_node("td.match-date") %>% html_text(trim=TRUE), error=function(e) NA)
    up_date     <- tryCatch(as.Date(up_date_str, "%d.%m.%Y"), error=function(e) NA)
    up_opp      <- tryCatch(up_row %>% html_node("td.team-shortname_extended a") %>%
                              html_text(trim=TRUE), error=function(e) NA)
    up_sim <- if (!is.na(up_opp)) smart_sim(up_opp, away) else 0
    if (!is.na(up_date) && !is.na(target_date) &&
        up_date == target_date && up_sim >= 0.6) {
      return(list(status="PENDING", result=NA_character_))
    }
  }
  
  if (length(played_rows) == 0) return(list(status="PENDING", result=NA_character_))
  
  date_mismatch_row <- NULL
  
  for (row in rev(played_rows)) {
    cls <- html_attr(row, "class") %||% ""
    row_date_str <- row %>% html_node("td.match-date") %>% html_text(trim=TRUE)
    row_date     <- tryCatch(as.Date(row_date_str, "%d.%m.%Y"), error=function(e) NA)
    opp_node  <- row %>% html_node("td.team-shortname_extended a")
    opponent  <- if (!is.null(opp_node)) html_text(opp_node, trim=TRUE) else NA_character_
    
    if (is.na(row_date) || is.na(opponent)) next
    
    target_date <- tryCatch(as.Date(fixture_date), error=function(e) NA)
    date_ok  <- !is.na(target_date) && !is.na(row_date) && row_date == target_date
    opp_sim  <- smart_sim(opponent, away)
    opp_ok   <- opp_sim >= 0.6
    
    if (date_ok) {
      # confirmed match by date
    } else if (opp_ok) {
      if (is.null(date_mismatch_row)) date_mismatch_row <- list(row=row, row_date=row_date)
      next
    } else {
      next
    }
    
    is_live  <- grepl("live",     cls) && !grepl("finished", cls)
    is_final <- grepl("finished", cls)
    
    ft_raw <- tryCatch(row %>% html_node("span.match-result-0 a") %>% html_text(trim=TRUE),
                       error=function(e) NA_character_)
    ht_raw <- tryCatch(row %>% html_node("span.match-result-1 a") %>% html_text(trim=TRUE),
                       error=function(e) NA_character_)
    
    parse_sc <- function(s) {
      if (is.na(s) || !grepl(":", s)) return(list(h=NA_integer_, a=NA_integer_))
      p <- strsplit(trimws(s), ":")[[1]]
      list(h=suppressWarnings(as.integer(p[1])), a=suppressWarnings(as.integer(p[2])))
    }
    
    if (is_live) {
      cur  <- parse_sc(ft_raw)
      ht_s <- parse_sc(ht_raw)
      return(list(
        status   = "LIVE",
        result   = ft_raw %||% "0:0",
        ht_score = if (!is.na(ht_raw)) ht_raw else NA_character_,
        home_ft  = cur$h, away_ft = cur$a,
        home_ht  = ht_s$h, away_ht = ht_s$a
      ))
    }
    
    if (is_final) {
      ft  <- parse_sc(ft_raw)
      ht  <- parse_sc(ht_raw)
      return(list(
        status   = "FINAL",
        result   = ft_raw %||% NA_character_,
        ht_score = ht_raw %||% NA_character_,
        home_ft  = ft$h, away_ft = ft$a,
        home_ht  = ht$h, away_ht = ht$a
      ))
    }
  }
  
  if (!is.null(date_mismatch_row)) {
    return(list(status="DATE_MISMATCH",
                result=paste0("WF date: ", date_mismatch_row$row_date, " vs expected: ", fixture_date)))
  }
  
  list(status="PENDING", result=NA_character_)
}

# ════════════════════════════════════════════════════════════
# COCKTAIL ANALYSIS PIPELINE
# ════════════════════════════════════════════════════════════

run_cocktail_analysis <- function(data_frame, thresholds) {
  # Source the production analysis logic but with cocktail thresholds
  # Inline minimal versions of the needed functions
  source_text <- "
  library(dplyr); library(purrr); library(tidyr)

  parse_score <- function(s) {
    tryCatch({
      s <- trimws(as.character(s))
      ft_part <- trimws(gsub('\\\\(.*\\\\)', '', s))
      ft <- as.integer(strsplit(ft_part, ':')[[1]])
      ht_part <- trimws(gsub('.*\\\\((.*)\\\\).*', '\\\\1', s))
      ht <- as.integer(strsplit(ht_part, ':')[[1]])
      list(ft_scored=ft[1], ft_conceded=ft[2], ht_scored=ht[1], ht_conceded=ht[2],
           ft_total=ft[1]+ft[2], ht_total=ht[1]+ht[2])
    }, error = function(e) list(ft_scored=0, ft_conceded=0, ht_scored=0, ht_conceded=0, ft_total=0, ht_total=0))
  }
  "
  eval(parse(text = source_text), envir = globalenv())
  
  message("Preprocessing fixtures...")
  result <- data_frame
  
  for (i in seq_len(nrow(result))) {
    for (j in 1:5) {
      hs <- parse_score(result[[paste0("H", j)]][i])
      result[[paste0("H", j, "_fts")]][i] <- hs$ft_scored
      result[[paste0("H", j, "_ftc")]][i] <- hs$ft_conceded
      result[[paste0("H", j, "_hts")]][i] <- hs$ht_scored
      result[[paste0("H", j, "_htc")]][i] <- hs$ht_conceded
      result[[paste0("H", j, "_ftt")]][i] <- hs$ft_total
      result[[paste0("H", j, "_htt")]][i] <- hs$ht_total
      
      as <- parse_score(result[[paste0("A", j)]][i])
      result[[paste0("A", j, "_fts")]][i] <- as$ft_scored
      result[[paste0("A", j, "_ftc")]][i] <- as$ft_conceded
      result[[paste0("A", j, "_hts")]][i] <- as$ht_scored
      result[[paste0("A", j, "_htc")]][i] <- as$ht_conceded
      result[[paste0("A", j, "_ftt")]][i] <- as$ft_total
      result[[paste0("A", j, "_htt")]][i] <- as$ht_total
      
      hs2 <- parse_score(result[[paste0("H2H", j)]][i])
      result[[paste0("H2H", j, "_ftt")]][i] <- hs2$ft_total
      result[[paste0("H2H", j, "_htt")]][i] <- hs2$ht_total
    }
  }
  
  result <- result %>%
    mutate(
      home_l5_scored_ft   = (H1_fts + H2_fts + H3_fts + H4_fts + H5_fts) / 5,
      home_l5_conceded_ft = (H1_ftc + H2_ftc + H3_ftc + H4_ftc + H5_ftc) / 5,
      home_l5_scored_ht   = (H1_hts + H2_hts + H3_hts + H4_hts + H5_hts) / 5,
      home_l5_conceded_ht = (H1_htc + H2_htc + H3_htc + H4_htc + H5_htc) / 5,
      away_l5_scored_ft   = (A1_fts + A2_fts + A3_fts + A4_fts + A5_fts) / 5,
      away_l5_conceded_ft = (A1_ftc + A2_ftc + A3_ftc + A4_ftc + A5_ftc) / 5,
      away_l5_scored_ht   = (A1_hts + A2_hts + A3_hts + A4_hts + A5_hts) / 5,
      away_l5_conceded_ht = (A1_htc + A2_htc + A3_htc + A4_htc + A5_htc) / 5,
      h2h_avg_ft = (H2H1_ftt + H2H2_ftt + H2H3_ftt + H2H4_ftt + H2H5_ftt) / 5,
      h2h_avg_ht = (H2H1_htt + H2H2_htt + H2H3_htt + H2H4_htt + H2H5_htt) / 5,
      home_season_scored_ft   = round(home_goal_scored   / home_match_played, 3),
      home_season_conceded_ft = round(home_goal_conceded / home_match_played, 3),
      away_season_scored_ft   = round(away_goal_scored   / away_match_played, 3),
      away_season_conceded_ft = round(away_goal_conceded / away_match_played, 3),
      home_attack_ft  = 0.6 * home_l5_scored_ft   + 0.4 * home_season_scored_ft,
      home_defense_ft = 0.6 * home_l5_conceded_ft + 0.4 * home_season_conceded_ft,
      away_attack_ft  = 0.6 * away_l5_scored_ft   + 0.4 * away_season_scored_ft,
      away_defense_ft = 0.6 * away_l5_conceded_ft + 0.4 * away_season_conceded_ft,
      home_attack_ht  = home_l5_scored_ht,
      home_defense_ht = home_l5_conceded_ht,
      away_attack_ht  = away_l5_scored_ht,
      away_defense_ht = away_l5_conceded_ht
    )
  
  league_avgs <- result %>%
    group_by(league) %>%
    summarise(
      league_avg_attack_ft = mean(c(home_season_scored_ft, away_season_scored_ft), na.rm = TRUE),
      league_avg_attack_ht = mean(c(home_l5_scored_ht, away_l5_scored_ht), na.rm = TRUE),
      .groups = "drop"
    )
  result <- result %>% left_join(league_avgs, by = "league")
  
  message("Computing probabilities for ", nrow(result), " fixtures...")
  result$p_over15    <- NA_real_
  result$p_under45   <- NA_real_
  result$p_btts      <- NA_real_
  result$p_fh_over05 <- NA_real_
  result$p_ht_draw   <- NA_real_
  
  for (i in seq_len(nrow(result))) {
    league_avg_ft <- result$league_avg_attack_ft[i]
    league_avg_ht <- result$league_avg_attack_ht[i]
    if (is.na(league_avg_ft) || league_avg_ft == 0) league_avg_ft <- 1.3
    if (is.na(league_avg_ht) || league_avg_ht == 0) league_avg_ht <- 0.5
    
    home_att <- result$home_attack_ft[i]  / league_avg_ft
    home_def <- result$home_defense_ft[i] / league_avg_ft
    away_att <- result$away_attack_ft[i]  / league_avg_ft
    away_def <- result$away_defense_ft[i] / league_avg_ft
    lambda_home_ft <- max(0.1, home_att * away_def * league_avg_ft, na.rm = TRUE)
    lambda_away_ft <- max(0.1, away_att * home_def * league_avg_ft, na.rm = TRUE)
    
    h2h_avg     <- suppressWarnings(as.numeric(result$h2h_avg_ft[i]))
    poisson_avg <- suppressWarnings(as.numeric(lambda_home_ft + lambda_away_ft))
    if (!is.na(h2h_avg) && !is.na(poisson_avg) && h2h_avg > 0 && poisson_avg > 0) {
      h2h_ratio <- max(0.7, min(1.3, h2h_avg / poisson_avg))
      lambda_home_ft <- lambda_home_ft * (0.8 + 0.2 * h2h_ratio)
      lambda_away_ft <- lambda_away_ft * (0.8 + 0.2 * h2h_ratio)
    }
    
    max_goals <- 8
    prob_matrix <- outer(dpois(0:max_goals, lambda_home_ft), dpois(0:max_goals, lambda_away_ft))
    p_over15 <- 1 - (prob_matrix[1,1] + prob_matrix[1,2] + prob_matrix[2,1])
    
    # Under 4.5
    p_u45 <- 0
    for (h in 0:max_goals) for (a in 0:max_goals) {
      if ((h + a) <= 4) p_u45 <- p_u45 + prob_matrix[h+1, a+1]
    }
    
    p_btts <- (1 - dpois(0, lambda_home_ft)) * (1 - dpois(0, lambda_away_ft))
    
    result$p_over15[i]  <- round(p_over15, 4)
    result$p_under45[i] <- round(p_u45, 4)
    result$p_btts[i]    <- round(p_btts, 4)
    
    # HT
    home_att_ht <- result$home_attack_ht[i] / league_avg_ht
    home_def_ht <- result$home_defense_ht[i] / league_avg_ht
    away_att_ht <- result$away_attack_ht[i] / league_avg_ht
    away_def_ht <- result$away_defense_ht[i] / league_avg_ht
    lambda_home_ht <- max(0.05, home_att_ht * away_def_ht * league_avg_ht, na.rm = TRUE)
    lambda_away_ht <- max(0.05, away_att_ht * home_def_ht * league_avg_ht, na.rm = TRUE)
    
    h2h_ht <- suppressWarnings(as.numeric(result$h2h_avg_ht[i]))
    poisson_avg_ht <- lambda_home_ht + lambda_away_ht
    if (!is.na(h2h_ht) && !is.na(poisson_avg_ht) && h2h_ht > 0 && poisson_avg_ht > 0) {
      h2h_ratio_ht <- max(0.7, min(1.3, h2h_ht / poisson_avg_ht))
      lambda_home_ht <- lambda_home_ht * (0.8 + 0.2 * h2h_ratio_ht)
      lambda_away_ht <- lambda_away_ht * (0.8 + 0.2 * h2h_ratio_ht)
    }
    
    result$p_fh_over05[i] <- round(1 - dpois(0, lambda_home_ht) * dpois(0, lambda_away_ht), 4)
    pm_ht <- outer(dpois(0:5, lambda_home_ht), dpois(0:5, lambda_away_ht))
    result$p_ht_draw[i] <- round(sum(diag(pm_ht)), 4)
  }
  
  # Apply filters per market
  fh_picks <- result %>%
    filter(p_fh_over05 >= thresholds$fh_over05) %>%
    filter(H1 != "0:0 (0:0)", A1 != "0:0 (0:0)") %>%
    rowwise() %>%
    filter(
      sum(c(H1_hts, H2_hts, H3_hts, H4_hts, H5_hts) > 0, na.rm = TRUE) >= 3 |
        sum(c(A1_hts, A2_hts, A3_hts, A4_hts, A5_hts) > 0, na.rm = TRUE) >= 3
    ) %>% ungroup() %>%
    select(league, home, away, fixture_date, home_href, away_href, competition_id,
           home_position, away_position, p_fh_over05) %>%
    mutate(market = "FH Over 0.5",
           confidence = paste0(round(p_fh_over05 * 100, 1), "%"))
  
  o15_picks <- result %>%
    filter(p_over15 >= thresholds$over15) %>%
    filter(H1 != "0:0 (0:0)", A1 != "0:0 (0:0)") %>%
    rowwise() %>%
    filter(
      sum(c(H1_ftt, H2_ftt, H3_ftt, H4_ftt, H5_ftt) >= 2, na.rm = TRUE) >= 3 |
        sum(c(A1_ftt, A2_ftt, A3_ftt, A4_ftt, A5_ftt) >= 2, na.rm = TRUE) >= 3
    ) %>% ungroup() %>%
    select(league, home, away, fixture_date, home_href, away_href, competition_id,
           home_position, away_position, p_over15) %>%
    mutate(market = "Over 1.5 FT",
           confidence = paste0(round(p_over15 * 100, 1), "%"))
  
  btts_picks <- result %>%
    filter(p_btts >= thresholds$btts) %>%
    filter(H1 != "0:0 (0:0)", A1 != "0:0 (0:0)") %>%
    rowwise() %>%
    filter(
      sum(c(H1_fts, H2_fts, H3_fts, H4_fts, H5_fts) > 0 &
            c(H1_ftc, H2_ftc, H3_ftc, H4_ftc, H5_ftc) > 0, na.rm = TRUE) >= 3 |
        sum(c(A1_fts, A2_fts, A3_fts, A4_fts, A5_fts) > 0 &
              c(A1_ftc, A2_ftc, A3_ftc, A4_ftc, A5_ftc) > 0, na.rm = TRUE) >= 3
    ) %>% ungroup() %>%
    select(league, home, away, fixture_date, home_href, away_href, competition_id,
           home_position, away_position, p_btts) %>%
    mutate(market = "BTTS",
           confidence = paste0(round(p_btts * 100, 1), "%"))
  
  ht_draw_picks <- result %>%
    filter(p_ht_draw >= thresholds$ht_draw) %>%
    filter(H1 != "0:0 (0:0)", A1 != "0:0 (0:0)") %>%
    rowwise() %>%
    filter(
      sum(c(H1_htt, H2_htt, H3_htt, H4_htt, H5_htt) <= 1, na.rm = TRUE) >= 3 &
        sum(c(A1_htt, A2_htt, A3_htt, A4_htt, A5_htt) <= 1, na.rm = TRUE) >= 3
    ) %>%
    filter(
      sum(c(H2H1_htt, H2H2_htt, H2H3_htt, H2H4_htt, H2H5_htt) <= 1, na.rm = TRUE) >= 3
    ) %>% ungroup() %>%
    select(league, home, away, fixture_date, home_href, away_href, competition_id,
           home_position, away_position, p_ht_draw) %>%
    mutate(market = "HT Draw",
           confidence = paste0(round(p_ht_draw * 100, 1), "%"))
  
  u45_picks <- result %>%
    filter(p_under45 >= thresholds$under45) %>%
    filter(H1 != "0:0 (0:0)", A1 != "0:0 (0:0)") %>%
    rowwise() %>%
    filter(
      sum(c(H1_ftt, H2_ftt, H3_ftt, H4_ftt, H5_ftt) <= 4, na.rm = TRUE) >= 4 &
        sum(c(A1_ftt, A2_ftt, A3_ftt, A4_ftt, A5_ftt) <= 4, na.rm = TRUE) >= 4
    ) %>%
    filter(
      sum(c(H2H1_ftt, H2H2_ftt, H2H3_ftt, H2H4_ftt, H2H5_ftt) <= 4, na.rm = TRUE) >= 4
    ) %>% ungroup() %>%
    select(league, home, away, fixture_date, home_href, away_href, competition_id,
           home_position, away_position, p_under45) %>%
    mutate(market = "Under 4.5 FT",
           confidence = paste0(round(p_under45 * 100, 1), "%"))
  
  list(
    "FH Over 0.5"  = fh_picks,
    "Over 1.5 FT"  = o15_picks,
    "BTTS"         = btts_picks,
    "HT Draw"      = ht_draw_picks,
    "Under 4.5 FT" = u45_picks
  )
}

build_cocktail <- function(picks_list, priority) {
  all_picks <- data.frame()
  for (mkt in priority) {
    d <- picks_list[[mkt]]
    if (is.null(d) || nrow(d) == 0) next
    # Build a standard frame with all required columns from each market pick
    p <- data.frame(
      LEAGUE         = as.character(d$league %||% ""),
      HOME           = as.character(d$home),
      AWAY           = as.character(d$away),
      FIXTURE_DATE   = as.character(as.Date(sapply(d$fixture_date, parse_excel_date), origin = "1970-01-01")),
      HOME_HREF      = as.character(d$home_href %||% NA),
      AWAY_HREF      = as.character(d$away_href %||% NA),
      COMPETITION_ID = as.character(d$competition_id %||% NA),
      MARKET         = as.character(d$market),
      CONFIDENCE     = as.character(d$confidence),
      stringsAsFactors = FALSE
    )
    all_picks <- rbind(all_picks, p)
  }
  if (nrow(all_picks) == 0) return(NULL)
  
  all_picks$KEY <- paste0(tolower(trimws(all_picks$HOME)), "|", tolower(trimws(all_picks$AWAY)))
  cocktail <- all_picks[!duplicated(all_picks$KEY), ]
  cocktail$KEY <- NULL
  cocktail
}

# ════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700;900&family=Barlow+Condensed:wght@400;600;700;900&family=JetBrains+Mono:wght@400;500&display=swap"),
    tags$style(HTML("
      *, *::before, *::after { margin: 0; padding: 0; box-sizing: border-box; }
      :root {
        --primary: #D4AF37; --primary-dark: #B8860B; --accent: #DA291C;
        --bg: #faf8f3; --bg-card: #ffffff; --bg-soft: #f5f1e8;
        --border: #ebe6d8; --border-soft: #f0ebd9;
        --text: #1a1d23; --text-2: #5a5346; --text-3: #8a8472;
        --green: #15803d; --green-bg: #ecfdf5;
        --red-bg: #fef2f2; --amber-bg: #fffbeb;
        --fh: 'Playfair Display', serif;
        --fb: 'Barlow Condensed', sans-serif;
        --fm: 'JetBrains Mono', monospace;
      }
      body { background: var(--bg); color: var(--text); font-family: var(--fb); }

      .sh { background: linear-gradient(135deg, #1a1614 0%, #2a221c 100%);
            padding: 18px 32px; display: flex; align-items: center; justify-content: space-between;
            position: relative; overflow: hidden;
            border-bottom: 2px solid var(--primary); }
      .sh::after { content: ''; position: absolute; right: -50px; top: -50px; width: 200px; height: 200px;
                   background: radial-gradient(circle, rgba(212,175,55,0.2) 0%, transparent 70%); }
      .sb { display: flex; align-items: baseline; gap: 16px; }
      .lg { font-family: var(--fh); font-size: 32px; font-weight: 900; color: white; letter-spacing: 1px; line-height: 1; }
      .lg span { color: var(--primary); font-style: italic; }
      .tg { font-family: var(--fm); font-size: 10px; letter-spacing: 3px; text-transform: uppercase;
            color: rgba(255,255,255,0.65); font-weight: 500; }

      .pg { max-width: 1500px; margin: 0 auto; padding: 28px 32px; }

      .intro { background: linear-gradient(135deg, #fff9e6 0%, #fff5d6 100%);
               border: 1px solid var(--primary); border-radius: 4px; padding: 18px 24px;
               margin-bottom: 24px; position: relative; }
      .intro::before { content: ''; position: absolute; left: 0; top: 0; bottom: 0; width: 3px;
                       background: var(--primary); border-radius: 2px 0 0 2px; }
      .intro-title { font-family: var(--fh); font-size: 18px; font-weight: 700; color: var(--primary-dark);
                      margin-bottom: 6px; }
      .intro-text { font-family: var(--fb); font-size: 14px; color: var(--text-2); line-height: 1.6; }

      .nav-tabs { border: none !important; border-bottom: 1px solid var(--border) !important; margin-bottom: 28px !important; }
      .nav-tabs > li > a { font-family: var(--fb) !important; font-size: 12px !important; font-weight: 700 !important;
                            letter-spacing: 2.5px !important; text-transform: uppercase !important; color: var(--text-3) !important;
                            border: none !important; padding: 14px 28px !important;
                            border-bottom: 2px solid transparent !important; background: transparent !important; }
      .nav-tabs > li > a:hover { color: var(--text) !important; }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
        color: var(--primary-dark) !important; border-bottom-color: var(--primary) !important; background: transparent !important; }

      .cd { background: var(--bg-card); border: 1px solid var(--border); border-radius: 4px;
            padding: 24px 26px; margin-bottom: 18px; position: relative; }
      .cd::before { content: ''; position: absolute; top: 0; left: 0; width: 28px; height: 2px; background: var(--primary); }
      .ct { font-family: var(--fb); font-size: 10px; font-weight: 700; letter-spacing: 3px;
            text-transform: uppercase; color: var(--text-3); margin-bottom: 14px; }

      .bp { font-family: var(--fb) !important; font-size: 12px !important; font-weight: 700 !important;
            letter-spacing: 2.5px !important; text-transform: uppercase !important;
            background: linear-gradient(135deg, var(--primary) 0%, var(--primary-dark) 100%) !important;
            color: white !important; border: none !important; border-radius: 3px !important;
            padding: 11px 26px !important; cursor: pointer !important; transition: all 0.2s !important;
            box-shadow: 0 2px 8px rgba(184,134,11,0.3) !important; }
      .bp:hover { transform: translateY(-1px); box-shadow: 0 4px 12px rgba(184,134,11,0.4) !important; }
      .ba { font-family: var(--fb) !important; font-size: 12px !important; font-weight: 700 !important;
            letter-spacing: 2.5px !important; text-transform: uppercase !important; background: var(--accent) !important;
            color: white !important; border: none !important; border-radius: 3px !important;
            padding: 11px 26px !important; cursor: pointer !important; }
      .ba:hover { background: #B91C1C !important; }
      .bo { font-family: var(--fb) !important; font-size: 11px !important; font-weight: 600 !important;
            letter-spacing: 2px !important; text-transform: uppercase !important; background: transparent !important;
            color: var(--primary-dark) !important; border: 1.5px solid var(--primary) !important;
            border-radius: 3px !important; padding: 8px 20px !important; cursor: pointer !important; }
      .bo:hover { background: rgba(212,175,55,0.08) !important; }

      .ks { display: grid; grid-template-columns: repeat(4, 1fr); gap: 14px; margin-bottom: 24px; }
      .kc { background: var(--bg-card); border: 1px solid var(--border); border-radius: 4px;
            padding: 18px; position: relative; text-align: center; }
      .kc::before { content: ''; position: absolute; top: 0; left: 0; width: 28px; height: 2px; background: var(--primary); }
      .kn { font-family: var(--fh); font-size: 36px; font-weight: 900; line-height: 1; color: var(--primary-dark); }
      .kl { font-family: var(--fm); font-size: 9px; font-weight: 500; letter-spacing: 2px;
            text-transform: uppercase; color: var(--text-3); margin-top: 8px; }

      .pl { display: inline-block; font-family: var(--fm); font-size: 10px; font-weight: 600;
            letter-spacing: 1px; padding: 4px 12px; border-radius: 12px; border: 1px solid; }
      .pl-fn { background: var(--green-bg); color: var(--green); border-color: rgba(21,128,61,0.2); }
      .pl-pn { background: var(--bg-soft); color: var(--text-3); border-color: var(--border); }
      .pl-lv { background: var(--amber-bg); color: #B45309; border-color: rgba(217,119,6,0.25); }
      .pl-er { background: var(--red-bg); color: var(--accent); border-color: rgba(218,41,28,0.25); }

      table.dataTable thead th { background: var(--bg-soft) !important; font-size: 10px !important;
                                  font-weight: 700 !important; letter-spacing: 2px !important;
                                  text-transform: uppercase !important; color: var(--text-3) !important;
                                  font-family: var(--fb) !important; padding: 12px 14px !important; }
      table.dataTable tbody td { font-size: 13px !important; padding: 10px 14px !important; font-family: var(--fb) !important; }

      .form-control, .selectize-input { font-family: var(--fb) !important; font-size: 13px !important;
                                          border: 1px solid var(--border) !important; border-radius: 3px !important; }
      .form-control:focus { border-color: var(--primary) !important; box-shadow: 0 0 0 3px rgba(212,175,55,0.15) !important; }

      .shiny-input-container label { font-family: var(--fb) !important; font-size: 11px !important;
                                       font-weight: 600 !important; letter-spacing: 1px !important;
                                       text-transform: uppercase !important; color: var(--text-3) !important; }

      .lo { background: #1a1614; border: 1px solid #2d2820; border-radius: 4px;
            padding: 14px 16px; font-family: var(--fm); font-size: 11px; color: #d4af37;
            height: 160px; overflow-y: auto; white-space: pre-wrap; line-height: 1.8; }

      ::-webkit-scrollbar { width: 6px; height: 6px; }
      ::-webkit-scrollbar-thumb { background: var(--border); border-radius: 3px; }
    "))
  ),
  
  div(class = "sh",
      div(class = "sb",
          div(class = "lg", "Goal", tags$span("Edge"), " Cocktail"),
          div(class = "tg", "Master Accumulator")
      ),
      div(class = "tg", textOutput("hdr_status", inline = TRUE))
  ),
  
  div(class = "pg",
      div(class = "intro",
          div(class = "intro-title", "Your Master Accumulator"),
          div(class = "intro-text",
              "This app builds a single accumulator slip combining the highest-confidence picks across 5 cocktail markets: ",
              tags$strong("FH Over 0.5, Over 1.5 FT, BTTS, HT Draw, Under 4.5 FT."),
              " Duplicate fixtures are deduped by market priority (highest-value odds first). Commit a slip to track it via the Live Checker, then review long-term performance in History.")
      ),
      tabsetPanel(id = "tabs", type = "tabs",
                  
                  tabPanel("Generate", value = "generate",
                           div(style = "padding-top: 8px;",
                               div(class = "cd",
                                   div(class = "ct", "Step 1 — Upload Raw Data"),
                                   fileInput("raw_excel", NULL, accept = ".xlsx", width = "100%"),
                                   uiOutput("raw_status")
                               ),
                               div(class = "cd",
                                   div(class = "ct", "Step 2 — Build Cocktail"),
                                   actionButton("build_btn", "Generate Cocktail", class = "bp")
                               ),
                               uiOutput("cocktail_summary"),
                               uiOutput("cocktail_table_card"),
                               uiOutput("commit_card")
                           )
                  ),
                  
                  tabPanel("Live Checker", value = "live",
                           div(style = "padding-top: 8px;", uiOutput("live_content"))
                  ),
                  
                  tabPanel("History", value = "history",
                           div(style = "padding-top: 8px;", uiOutput("history_content"))
                  ),
                  
                  tabPanel("Settings", value = "settings",
                           div(style = "padding-top: 8px;",
                               div(class = "cd",
                                   div(class = "ct", "Threshold Settings"),
                                   fluidRow(
                                     column(4, numericInput("th_fh",     "FH Over 0.5", 0.945, min = 0.5, max = 1, step = 0.005)),
                                     column(4, numericInput("th_o15",    "Over 1.5 FT", 0.92,  min = 0.5, max = 1, step = 0.01)),
                                     column(4, numericInput("th_btts",   "BTTS",         0.73, min = 0.5, max = 1, step = 0.01))
                                   ),
                                   fluidRow(
                                     column(4, numericInput("th_htd",    "HT Draw",      0.78, min = 0.3, max = 1, step = 0.01)),
                                     column(4, numericInput("th_u45",    "Under 4.5 FT", 0.97, min = 0.5, max = 1, step = 0.005))
                                   )
                               ),
                               div(class = "cd",
                                   div(class = "ct", "Market Priority (Highest Odds First)"),
                                   div(style = "font-family: var(--fm); font-size: 11px; color: var(--text-3); margin-bottom: 12px;",
                                       "1. HT Draw  →  2. BTTS  →  3. Under 4.5 FT  →  4. Over 1.5 FT  →  5. FH Over 0.5"),
                                   div(style = "font-family: var(--fb); font-size: 12px; color: var(--text-2);",
                                       "Dedup priority: when same fixture appears in multiple markets, the higher-priority market wins.")
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
    raw_df       = NULL,
    picks        = NULL,
    cocktail     = NULL,
    log          = "Ready. Upload raw scraped data to begin.\n",
    history      = load_history(),
    active_slip  = NULL,
    live_results = NULL
  )
  
  log_msg <- function(msg) {
    ts <- format(Sys.time(), "%H:%M:%S")
    rv$log <- paste0(rv$log, "[", ts, "] ", msg, "\n")
  }
  
  output$hdr_status <- renderText({
    if (!is.null(rv$cocktail)) paste0(nrow(rv$cocktail), " legs in current slip")
    else if (length(rv$history) > 0) paste0(length(rv$history), " saved slips")
    else "No data loaded"
  })
  
  current_thresholds <- reactive({
    list(
      fh_over05 = input$th_fh   %||% 0.945,
      over15    = input$th_o15  %||% 0.92,
      btts      = input$th_btts %||% 0.73,
      ht_draw   = input$th_htd  %||% 0.78,
      under45   = input$th_u45  %||% 0.97
    )
  })
  
  observeEvent(input$raw_excel, {
    req(input$raw_excel)
    d <- read_raw_excel(input$raw_excel$datapath)
    if (is.null(d)) { log_msg("Failed to read Excel."); return() }
    rv$raw_df <- d
    log_msg(paste0("Raw data loaded: ", nrow(d), " fixtures"))
  })
  
  output$raw_status <- renderUI({
    req(rv$raw_df)
    div(style = "margin-top: 8px;",
        span(class = "pl pl-fn", paste0(nrow(rv$raw_df), " fixtures ready")))
  })
  
  observeEvent(input$build_btn, {
    req(rv$raw_df)
    log_msg("Running analysis...")
    withProgress(message = "Analysing fixtures...", value = 0.5, {
      tryCatch({
        picks <- run_cocktail_analysis(rv$raw_df, current_thresholds())
        rv$picks <- picks
        rv$cocktail <- build_cocktail(picks, DEFAULT_PRIORITY)
        log_msg(paste0("Cocktail built: ", nrow(rv$cocktail), " unique legs"))
        showNotification(paste0("Cocktail ready: ", nrow(rv$cocktail), " legs"),
                         type = "message", duration = 4)
      }, error = function(e) {
        log_msg(paste0("Error: ", e$message))
        showNotification(paste0("Error: ", e$message), type = "error", duration = 6)
      })
    })
  })
  
  output$cocktail_summary <- renderUI({
    req(rv$cocktail)
    c <- rv$cocktail
    counts <- table(c$MARKET)
    legs   <- nrow(c)
    
    tagList(
      div(class = "ks",
          div(class = "kc",
              div(class = "kn", legs),
              div(class = "kl", "Total Legs")),
          div(class = "kc",
              div(class = "kn", counts["HT Draw"] %||% 0),
              div(class = "kl", "HT Draw")),
          div(class = "kc",
              div(class = "kn", counts["BTTS"] %||% 0),
              div(class = "kl", "BTTS")),
          div(class = "kc",
              div(class = "kn", counts["Under 4.5 FT"] %||% 0),
              div(class = "kl", "Under 4.5"))
      )
    )
  })
  
  output$cocktail_table_card <- renderUI({
    req(rv$cocktail)
    div(class = "cd",
        div(class = "ct", "Generated Cocktail"),
        DTOutput("cocktail_table")
    )
  })
  
  output$cocktail_table <- renderDT({
    req(rv$cocktail)
    c <- rv$cocktail
    d <- data.frame(
      League     = c$LEAGUE,
      Date       = format(as.Date(sapply(c$FIXTURE_DATE, parse_excel_date), origin = "1970-01-01"), "%d %b"),
      Home       = c$HOME,
      Away       = c$AWAY,
      Market     = c$MARKET,
      Confidence = c$CONFIDENCE,
      stringsAsFactors = FALSE
    )
    datatable(d, rownames = FALSE, selection = "none",
              options = list(pageLength = 30, dom = "ftp"))
  }, server = FALSE)
  
  output$commit_card <- renderUI({
    req(rv$cocktail)
    div(class = "cd",
        div(class = "ct", "Commit Slip"),
        div(style = "display: flex; gap: 14px; align-items: end;",
            div(style = "flex: 1; max-width: 320px;",
                textInput("commit_label", "Slip Label",
                          value = paste0("Round_", format(Sys.Date(), "%Y%m%d")),
                          width = "100%")),
            actionButton("commit_btn", "Commit Slip", class = "bp"),
            actionButton("export_btn", "Export Excel", class = "bo")
        )
    )
  })
  
  observeEvent(input$commit_btn, {
    req(rv$cocktail)
    label <- trimws(input$commit_label %||% "")
    if (nchar(label) == 0) {
      showNotification("Label required", type = "warning"); return()
    }
    id <- format(Sys.time(), "%Y%m%d%H%M%S")
    rv$history[[id]] <- list(
      id        = id,
      label     = label,
      committed = format(Sys.time(), "%d %b %Y · %H:%M"),
      cocktail  = rv$cocktail,
      results   = NULL  # filled by live checker later
    )
    save_history(rv$history)
    rv$active_slip <- id
    showNotification(paste0("Slip committed: ", label), type = "message", duration = 4)
    log_msg(paste0("Slip committed: ", label, " (", nrow(rv$cocktail), " legs)"))
  })
  
  observeEvent(input$export_btn, {
    req(rv$cocktail)
    tryCatch({
      fname <- paste0("cocktail_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      path  <- file.path(BASE_PATH, fname)
      wb    <- createWorkbook()
      addWorksheet(wb, "cocktail", tabColour = "#D4AF37", gridLines = FALSE)
      hs <- createStyle(fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
                        fgFill = "#B8860B", halign = "CENTER", textDecoration = "BOLD")
      writeData(wb, "cocktail", rv$cocktail, startRow = 1, startCol = 1)
      addStyle(wb, "cocktail", hs, rows = 1, cols = 1:ncol(rv$cocktail), gridExpand = TRUE)
      setColWidths(wb, "cocktail", cols = 1:ncol(rv$cocktail), widths = "auto")
      freezePane(wb, "cocktail", firstRow = TRUE)
      saveWorkbook(wb, path, overwrite = TRUE)
      showNotification(paste0("Exported: ", fname), type = "message", duration = 4)
    }, error = function(e) showNotification(paste0("Export error: ", e$message), type = "error"))
  })
  
  # ── LIVE CHECKER ──
  output$live_content <- renderUI({
    if (is.null(rv$active_slip)) {
      return(div(class = "cd", style = "text-align: center; padding: 60px 20px;",
                 div(style = "font-family: var(--fh); font-size: 22px; color: var(--text-3); margin-bottom: 8px;",
                     "No active slip"),
                 div(style = "font-family: var(--fm); font-size: 11px; color: var(--text-3); letter-spacing: 1px;",
                     "Generate and commit a slip to track results")))
    }
    slip <- rv$history[[rv$active_slip]]
    div(
      div(class = "cd",
          div(class = "ct", "Active Slip"),
          div(style = "font-family: var(--fh); font-size: 20px; font-weight: 700;", slip$label),
          div(style = "font-family: var(--fm); font-size: 10px; color: var(--text-3); margin-top: 4px;",
              slip$committed),
          div(style = "margin-top: 18px;",
              actionButton("fetch_live", "Fetch Results", class = "ba")
          )
      ),
      uiOutput("live_summary"),
      uiOutput("live_table_card")
    )
  })
  
  observeEvent(input$fetch_live, {
    req(rv$active_slip)
    slip <- rv$history[[rv$active_slip]]
    log_msg("Fetching live results...")
    withProgress(message = "Fetching from worldfootball...", value = 0, {
      tryCatch({
        source(PIPELINE_PATH, local = FALSE)
        start_selenium()
        Sys.sleep(10)
        cocktail <- to_df(slip$cocktail)
        if (is.null(cocktail)) { log_msg("ERROR: cocktail conversion failed"); stop_selenium(); return() }
        
        log_msg(paste0("DEBUG cols: ", paste(names(cocktail), collapse=", ")))
        log_msg(paste0("DEBUG fixture_date[1]: ", as.character(cocktail$FIXTURE_DATE[1] %||% "NULL")))
        log_msg(paste0("DEBUG home_href[1]: ", as.character(cocktail$HOME_HREF[1] %||% "NULL")))
        
        # Force fixture_date to proper Date format
        cocktail$FIXTURE_DATE <- as.character(as.Date(sapply(cocktail$FIXTURE_DATE, parse_excel_date), origin = "1970-01-01"))
        log_msg(paste0("DEBUG fixture_date[1] after parse: ", cocktail$FIXTURE_DATE[1]))
        
        cocktail$HOME_FT <- NA_integer_
        cocktail$AWAY_FT <- NA_integer_
        cocktail$HOME_HT <- NA_integer_
        cocktail$AWAY_HT <- NA_integer_
        cocktail$STATUS  <- "PENDING"
        cocktail$OUTCOME <- "PENDING"
        
        for (i in seq_len(nrow(cocktail))) {
          incProgress(1 / nrow(cocktail), detail = paste0(cocktail$HOME[i], " vs ", cocktail$AWAY[i]))
          res <- scrape_fixture_result(
            home = cocktail$HOME[i],
            away = cocktail$AWAY[i],
            fixture_date = cocktail$FIXTURE_DATE[i],
            home_href = cocktail$HOME_HREF[i],
            competition_id = cocktail$COMPETITION_ID[i]
          )
          cocktail$STATUS[i]  <- res$status %||% "PENDING"
          cocktail$HOME_FT[i] <- suppressWarnings(as.integer(res$home_ft %||% NA))
          cocktail$AWAY_FT[i] <- suppressWarnings(as.integer(res$away_ft %||% NA))
          cocktail$HOME_HT[i] <- suppressWarnings(as.integer(res$home_ht %||% NA))
          cocktail$AWAY_HT[i] <- suppressWarnings(as.integer(res$away_ht %||% NA))
          log_msg(paste0("  ", cocktail$HOME[i], " vs ", cocktail$AWAY[i], " -> ", cocktail$STATUS[i],
                         if (!is.na(res$result %||% NA)) paste0(" (", res$result, ")") else ""))
          
          if (cocktail$STATUS[i] %in% c("FINAL", "LIVE")) {
            cocktail$OUTCOME[i] <- eval_market(cocktail$HOME_FT[i], cocktail$AWAY_FT[i],
                                               cocktail$HOME_HT[i], cocktail$AWAY_HT[i],
                                               cocktail$MARKET[i])
          }
        }
        stop_selenium()
        
        # Save results back to history
        rv$history[[rv$active_slip]]$results <- cocktail
        save_history(rv$history)
        rv$live_results <- cocktail
        log_msg("Live fetch complete")
        showNotification("Live results updated", type = "message", duration = 3)
      }, error = function(e) {
        tryCatch(stop_selenium(), error = function(e2) invisible())
        log_msg(paste0("Fetch error: ", e$message))
        showNotification(paste0("Fetch error: ", e$message), type = "error", duration = 6)
      })
    })
  })
  
  output$live_summary <- renderUI({
    req(rv$active_slip)
    slip <- rv$history[[rv$active_slip]]
    res <- to_df(slip$results)
    if (is.null(res)) return(NULL)
    
    w <- sum(res$OUTCOME == "WIN", na.rm = TRUE)
    l <- sum(res$OUTCOME == "LOSS", na.rm = TRUE)
    p <- sum(res$OUTCOME == "PENDING", na.rm = TRUE)
    hr <- if ((w + l) > 0) paste0(round(w / (w + l) * 100, 1), "%") else "—"
    
    div(class = "ks",
        div(class = "kc", div(class = "kn", w), div(class = "kl", "Wins")),
        div(class = "kc", div(class = "kn", l), div(class = "kl", "Losses")),
        div(class = "kc", div(class = "kn", p), div(class = "kl", "Pending")),
        div(class = "kc", div(class = "kn", hr), div(class = "kl", "Hit Rate"))
    )
  })
  
  output$live_table_card <- renderUI({
    req(rv$active_slip)
    slip <- rv$history[[rv$active_slip]]
    if (is.null(slip$results)) return(NULL)
    div(class = "cd",
        div(class = "ct", "Live Status"),
        DTOutput("live_table")
    )
  })
  
  output$live_table <- renderDT({
    req(rv$active_slip)
    slip <- rv$history[[rv$active_slip]]
    r <- to_df(slip$results)
    req(r)
    ft <- ifelse(is.na(r$HOME_FT) | is.na(r$AWAY_FT), "—", paste0(r$HOME_FT, ":", r$AWAY_FT))
    ht <- ifelse(is.na(r$HOME_HT) | is.na(r$AWAY_HT), "—", paste0(r$HOME_HT, ":", r$AWAY_HT))
    out <- sapply(r$OUTCOME, function(o) {
      cls <- switch(o, WIN = "pl-fn", LOSS = "pl-er", PENDING = "pl-pn", "pl-pn")
      paste0('<span class="pl ', cls, '">', o, '</span>')
    })
    d <- data.frame(
      Market   = r$MARKET,
      Home     = r$HOME,
      Away     = r$AWAY,
      HT       = ht,
      FT       = ft,
      Outcome  = out,
      stringsAsFactors = FALSE
    )
    datatable(d, rownames = FALSE, selection = "none", escape = FALSE,
              options = list(pageLength = 30, dom = "ftp"))
  }, server = FALSE)
  
  # ── HISTORY ──
  output$history_content <- renderUI({
    h <- rv$history
    if (length(h) == 0) {
      return(div(class = "cd", style = "text-align: center; padding: 60px 20px;",
                 div(style = "font-family: var(--fh); font-size: 22px; color: var(--text-3); margin-bottom: 8px;",
                     "No committed slips yet"),
                 div(style = "font-family: var(--fm); font-size: 11px; color: var(--text-3); letter-spacing: 1px;",
                     "Generate and commit slips to build performance history")))
    }
    
    # Build summary
    ord <- names(h)[order(sapply(h, function(x) x$id), decreasing = TRUE)]
    
    rows <- lapply(ord, function(id) {
      e <- h[[id]]
      coc_df <- to_df(e$cocktail)
      res_df <- to_df(e$results)
      legs <- if (!is.null(coc_df)) nrow(coc_df) else 0
      if (!is.null(res_df) && "OUTCOME" %in% names(res_df)) {
        w <- sum(res_df$OUTCOME == "WIN", na.rm = TRUE)
        l <- sum(res_df$OUTCOME == "LOSS", na.rm = TRUE)
        p <- sum(res_df$OUTCOME == "PENDING", na.rm = TRUE)
        hr <- if ((w + l) > 0) paste0(round(w / (w + l) * 100, 1), "%") else "—"
        status_cls <- if (l == 0 && p == 0) "pl-fn" else if (l > 0) "pl-er" else "pl-pn"
        status_txt <- if (l == 0 && p == 0) "FULL WIN" else if (l > 0) paste0(l, " LOSS") else "PENDING"
      } else {
        w <- l <- p <- 0; hr <- "—"
        status_cls <- "pl-pn"; status_txt <- "NOT TRACKED"
      }
      tags$tr(
        tags$td(style = "padding: 12px 14px; font-family: var(--fh); font-weight: 700; font-size: 14px;", e$label),
        tags$td(style = "padding: 12px 14px; font-family: var(--fm); font-size: 10px; color: var(--text-3);", e$committed),
        tags$td(style = "padding: 12px 14px; text-align: center;", legs),
        tags$td(style = "padding: 12px 14px; text-align: center; color: var(--green); font-weight: 700;", w),
        tags$td(style = "padding: 12px 14px; text-align: center; color: var(--accent); font-weight: 700;", l),
        tags$td(style = "padding: 12px 14px; text-align: center;", p),
        tags$td(style = "padding: 12px 14px; text-align: center; font-family: var(--fh); font-weight: 800;", hr),
        tags$td(style = "padding: 12px 14px;", HTML(paste0('<span class="pl ', status_cls, '">', status_txt, '</span>'))),
        tags$td(style = "padding: 12px 14px; white-space: nowrap;",
                actionButton(paste0("activate_", id), "Activate", class = "bo",
                             style = "padding: 5px 12px !important; font-size: 10px !important; margin-right: 4px;"),
                actionButton(paste0("refetch_", id), "Re-fetch", class = "bo",
                             style = "padding: 5px 12px !important; font-size: 10px !important; margin-right: 4px;"),
                actionButton(paste0("rename_", id), "Rename", class = "bo",
                             style = "padding: 5px 12px !important; font-size: 10px !important; margin-right: 4px;"),
                actionButton(paste0("delete_", id), "Delete", class = "bo",
                             style = "padding: 5px 12px !important; font-size: 10px !important; color: var(--accent) !important; border-color: rgba(218,41,28,0.3) !important;"))
      )
    })
    
    # Compute per-market cumulative across all tracked slips
    all_outcomes <- list()
    for (id in names(h)) {
      r_df <- to_df(h[[id]]$results)
      if (!is.null(r_df) && "MARKET" %in% names(r_df) && "OUTCOME" %in% names(r_df)) {
        all_outcomes[[id]] <- r_df[, c("MARKET", "OUTCOME"), drop = FALSE]
      }
    }
    
    market_summary_ui <- NULL
    if (length(all_outcomes) > 0) {
      mo <- do.call(rbind, all_outcomes)
      mkt_stats <- mo %>%
        group_by(MARKET) %>%
        summarise(
          Total = n(),
          Wins = sum(OUTCOME == "WIN"),
          Losses = sum(OUTCOME == "LOSS"),
          Pending = sum(OUTCOME == "PENDING"),
          HitRate = ifelse(Wins + Losses > 0,
                           paste0(round(Wins / (Wins + Losses) * 100, 1), "%"), "—"),
          .groups = "drop"
        )
      
      mkt_rows <- lapply(seq_len(nrow(mkt_stats)), function(i) {
        tags$tr(
          tags$td(style = "padding: 10px 14px; font-family: var(--fb); font-weight: 700;", mkt_stats$MARKET[i]),
          tags$td(style = "padding: 10px 14px; text-align: center;", mkt_stats$Total[i]),
          tags$td(style = "padding: 10px 14px; text-align: center; color: var(--green); font-weight: 700;", mkt_stats$Wins[i]),
          tags$td(style = "padding: 10px 14px; text-align: center; color: var(--accent); font-weight: 700;", mkt_stats$Losses[i]),
          tags$td(style = "padding: 10px 14px; text-align: center;", mkt_stats$Pending[i]),
          tags$td(style = "padding: 10px 14px; text-align: center; font-family: var(--fh); font-weight: 800;", mkt_stats$HitRate[i])
        )
      })
      
      market_summary_ui <- div(class = "cd",
                               div(class = "ct", "Per-Market Cumulative Performance"),
                               tags$table(style = "width: 100%; border-collapse: separate;",
                                          tags$thead(tags$tr(style = "border-bottom: 2px solid var(--border);",
                                                             tags$th(style = "text-align: left; padding: 10px 14px; font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: var(--text-3);", "Market"),
                                                             tags$th(style = "text-align: center; padding: 10px 14px; font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: var(--text-3);", "Total"),
                                                             tags$th(style = "text-align: center; padding: 10px 14px; font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: var(--text-3);", "Wins"),
                                                             tags$th(style = "text-align: center; padding: 10px 14px; font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: var(--text-3);", "Losses"),
                                                             tags$th(style = "text-align: center; padding: 10px 14px; font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: var(--text-3);", "Pending"),
                                                             tags$th(style = "text-align: center; padding: 10px 14px; font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: var(--text-3);", "Hit Rate")
                                          )),
                                          tags$tbody(do.call(tagList, mkt_rows))
                               )
      )
    }
    
    tagList(
      div(class = "cd",
          div(class = "ct", "Slip History"),
          tags$table(style = "width: 100%; border-collapse: separate;",
                     tags$thead(tags$tr(style = "border-bottom: 2px solid var(--border);",
                                        tags$th(style = "text-align: left; padding: 10px 14px; font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: var(--text-3);", "Label"),
                                        tags$th(style = "text-align: left; padding: 10px 14px; font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: var(--text-3);", "Committed"),
                                        tags$th(style = "text-align: center; padding: 10px 14px; font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: var(--text-3);", "Legs"),
                                        tags$th(style = "text-align: center; padding: 10px 14px; font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: var(--text-3);", "W"),
                                        tags$th(style = "text-align: center; padding: 10px 14px; font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: var(--text-3);", "L"),
                                        tags$th(style = "text-align: center; padding: 10px 14px; font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: var(--text-3);", "P"),
                                        tags$th(style = "text-align: center; padding: 10px 14px; font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: var(--text-3);", "Rate"),
                                        tags$th(style = "text-align: left; padding: 10px 14px; font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: var(--text-3);", "Status"),
                                        tags$th(style = "text-align: left; padding: 10px 14px; font-size: 10px; font-weight: 700; letter-spacing: 2px; text-transform: uppercase; color: var(--text-3);", "")
                     )),
                     tags$tbody(do.call(tagList, rows))
          )
      ),
      market_summary_ui
    )
  })
  
  # Shared fetch logic — runs scrape for a given slip id and saves results
  do_fetch_for_slip <- function(slip_id) {
    slip <- rv$history[[slip_id]]
    if (is.null(slip)) return()
    log_msg(paste0("Fetching for: ", slip$label))
    withProgress(message = paste0("Fetching: ", slip$label), value = 0, {
      tryCatch({
        source(PIPELINE_PATH, local = FALSE)
        start_selenium()
        Sys.sleep(10)
        cocktail <- to_df(slip$cocktail)
        if (is.null(cocktail)) { log_msg("ERROR: cocktail conversion failed"); stop_selenium(); return() }
        
        cocktail$FIXTURE_DATE <- as.character(as.Date(sapply(cocktail$FIXTURE_DATE, parse_excel_date), origin = "1970-01-01"))
        cocktail$HOME_FT <- NA_integer_
        cocktail$AWAY_FT <- NA_integer_
        cocktail$HOME_HT <- NA_integer_
        cocktail$AWAY_HT <- NA_integer_
        cocktail$STATUS  <- "PENDING"
        cocktail$OUTCOME <- "PENDING"
        
        for (i in seq_len(nrow(cocktail))) {
          incProgress(1 / nrow(cocktail), detail = paste0(cocktail$HOME[i], " vs ", cocktail$AWAY[i]))
          res <- scrape_fixture_result(
            home = cocktail$HOME[i],
            away = cocktail$AWAY[i],
            fixture_date = cocktail$FIXTURE_DATE[i],
            home_href = cocktail$HOME_HREF[i],
            competition_id = cocktail$COMPETITION_ID[i]
          )
          cocktail$STATUS[i]  <- res$status %||% "PENDING"
          cocktail$HOME_FT[i] <- suppressWarnings(as.integer(res$home_ft %||% NA))
          cocktail$AWAY_FT[i] <- suppressWarnings(as.integer(res$away_ft %||% NA))
          cocktail$HOME_HT[i] <- suppressWarnings(as.integer(res$home_ht %||% NA))
          cocktail$AWAY_HT[i] <- suppressWarnings(as.integer(res$away_ht %||% NA))
          if (cocktail$STATUS[i] %in% c("FINAL", "LIVE")) {
            cocktail$OUTCOME[i] <- eval_market(cocktail$HOME_FT[i], cocktail$AWAY_FT[i],
                                               cocktail$HOME_HT[i], cocktail$AWAY_HT[i],
                                               cocktail$MARKET[i])
          }
        }
        stop_selenium()
        rv$history[[slip_id]]$results <- cocktail
        save_history(rv$history)
        log_msg(paste0("Re-fetch complete for: ", slip$label))
        showNotification(paste0("Updated: ", slip$label), type = "message", duration = 3)
      }, error = function(e) {
        tryCatch(stop_selenium(), error = function(e2) invisible())
        log_msg(paste0("Fetch error: ", e$message))
        showNotification(paste0("Error: ", e$message), type = "error", duration = 5)
      })
    })
  }
  
  observe({
    h <- rv$history
    lapply(names(h), function(id) {
      observeEvent(input[[paste0("activate_", id)]], {
        rv$active_slip <- id
        updateTabsetPanel(session, "tabs", selected = "live")
        showNotification(paste0("Activated: ", h[[id]]$label), type = "message", duration = 3)
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("refetch_", id)]], {
        do_fetch_for_slip(id)
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("rename_", id)]], {
        showModal(modalDialog(
          title = "Rename Slip",
          textInput(paste0("new_label_", id), "New Label", value = h[[id]]$label, width = "100%"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(paste0("rename_confirm_", id), "Save", class = "bp")
          ),
          easyClose = TRUE
        ))
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("rename_confirm_", id)]], {
        new_label <- trimws(input[[paste0("new_label_", id)]] %||% "")
        if (nchar(new_label) > 0) {
          rv$history[[id]]$label <- new_label
          save_history(rv$history)
          showNotification(paste0("Renamed to: ", new_label), type = "message", duration = 3)
        }
        removeModal()
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("delete_", id)]], {
        showModal(modalDialog(
          title = "Delete Slip?",
          HTML(paste0(
            "<p style=\"font-family: var(--fb); font-size: 14px;\">",
            "This will permanently delete <strong>", h[[id]]$label, "</strong>.<br><br>",
            "This cannot be undone. Continue?</p>"
          )),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(paste0("delete_confirm_", id), "Yes, Delete", class = "ba")
          ),
          easyClose = TRUE
        ))
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("delete_confirm_", id)]], {
        label <- rv$history[[id]]$label
        rv$history[[id]] <- NULL
        save_history(rv$history)
        if (!is.null(rv$active_slip) && rv$active_slip == id) rv$active_slip <- NULL
        removeModal()
        showNotification(paste0("Deleted: ", label), type = "message", duration = 3)
      }, ignoreInit = TRUE)
    })
  })
}

shinyApp(ui = ui, server = server)