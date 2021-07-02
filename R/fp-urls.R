#New Code to query FantasyPros Data


fp_auth <- function() { 
  user_agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
  x_api_key="zjxN52G3lP4fORpHRftGI2mTU8cTwxVNvkjByM3j"
  authorization = "Basic WC1BcGktS2V5OnpqeE41MkczbFA0Zk9ScEhSZnRHSTJtVFU4Y1R3eFZOdmtqQnlNM2o="

}

fp_build_url <- function(base = "https://api.fantasypros.com/v2/json/nfl/", year, type, scoring, pos, week){

  fp_url <- paste0(base,year,"/consensus-rankings?type=",type,
               "&scoring=",scoring,"&position=",pos,"&week=",week,"&experts=available")

  fp_url
}
####OLD CODDE#####
fp_get_data <- function(url, skip_parse_cols = c("Player", "Pos", "Team")) {
  fp_html <- xml2::read_html(url)
  fpdf <- rvest::html_table(fp_html)[[1]]

  num_cols <- setdiff(names(fpdf), skip_parse_cols)
  fpdf[num_cols][] <- lapply(fpdf[num_cols], function(x) gsub("bye|-$", NA, x))
  fpdf[num_cols][] <- lapply(fpdf[num_cols], readr::parse_number)

  clean_names <- janitor::make_clean_names(names(fpdf))
  clean_names <- gsub(pattern = "x", replacement = "w", clean_names)
  names(fpdf) <- clean_names

  tibble::as_tibble(fpdf)
}

fp_get_ranking_data <- function(url, pos) {

  fp_html <- xml2::read_html(url)
  fpdf <- rvest::html_table(fp_html, fill = TRUE)[[1]]
  fpdf <- fpdf[ , !is.na(names(fpdf))]

  # depending on which position you query this column is named differntly
  # so rename it up front
  player_info_col <- which(grepl("team", names(fpdf), ignore.case = TRUE))
  names(fpdf)[player_info_col] <- "player_info"

  junk_cols <- names(fpdf) %in% c(NA, "WSID", "Notes", "WSIS")
  fpdf <- fpdf[ , !junk_cols]

  junk_rows <- grepl("\r|\n|function\\(\\)|}\\);|&nbsp", fpdf$player_info)
  #fpdf <- fpdf[!junk_rows, ]

  fpdf <- dplyr::mutate(fpdf,
            tier = dplyr::case_when(
              grepl("Tier", Rank) ~ readr::parse_number(Rank)
             )
            )
  fpdf <- tidyr::fill(fpdf, tier)

  num_cols <- !grepl("player_info|pos|tier|notes", names(fpdf), ignore.case = TRUE)
  num_cols <- names(fpdf)[num_cols]
  fpdf[num_cols][] <- lapply(fpdf[num_cols], readr::parse_number)

  fpdf$player_info <- readr::parse_character(fpdf$player_info)
  fpdf <- fpdf[!is.na(fpdf$player_info), ]

  fpdf$player <- gsub("(?<=[a-z]|I|V|Jr\\.)[A-Z]\\..*|\\(.*\\).* ",
                      replacement = "",
                      fpdf$player_info, perl = TRUE)
  fpdf$team <- sapply(stringr::str_split(fpdf$player_info, " "),
                      function(x) x[length(x)]
  )

  if (!"Pos" %in% names(fpdf)) fpdf$Pos <- paste0(pos, fpdf$Rank)

  fpdf$pos <- gsub("[0-9]", "", fpdf$Pos)
  fpdf$pos_rank <- readr::parse_number(fpdf$Pos)
  fpdf$player_info <- NULL
  fpdf$Pos <- NULL

  fpdf <- dplyr::select(fpdf,
                        Rank,
                        tier:pos_rank,
                        dplyr::everything()
  )

  fpdf <- janitor::clean_names(fpdf)
  tibble::as_tibble(fpdf)
}


fp_get_stats_data <- function(url, pos) {

  fp_html <- xml2::read_html(url)
  fpdf <- rvest::html_table(fp_html, fill = TRUE)[[1]]

  if (pos %in% c("qb", "rb", "wr", "te")) {
    first_row_names <- fpdf[1 , ]
    second_row_names <- fpdf[2 , ]

    new_row_names <- paste(first_row_names, second_row_names, sep = "_")
    new_row_names <- gsub("^_|MISC_", "", new_row_names, ignore.case = TRUE)

    fpdf <- fpdf[3:nrow(fpdf), ]
    names(fpdf) <- new_row_names
  }

  fpdf$player <- gsub("\\s\\(.*", "", fpdf$Player, perl = TRUE)
  fpdf$team <- gsub(".*\\s|\\(|\\)", "", fpdf$Player)
  fpdf$pos <- toupper(pos)
  fpdf$Player <- NULL

  # this gets columns that are already numbers
  num_cols <- which(grepl("player|team|pos", names(fpdf), ignore.case = TRUE))
  already_numeric <- which(sapply(1:ncol(fpdf), function(i) class(fpdf[, i])) != "character")

  num_cols <- sort(unique(c(num_cols, already_numeric)))
  num_cols <- setdiff(1:ncol(fpdf), num_cols)
  num_cols <- names(fpdf)[num_cols]
  fpdf[num_cols][] <- lapply(fpdf[num_cols], readr::parse_number)

  fpdf <- dplyr::select(fpdf,
                        player,
                        pos,
                        team,
                        dplyr::everything()
  )

  fpdf <- janitor::clean_names(fpdf)
  tibble::as_tibble(fpdf)
}
