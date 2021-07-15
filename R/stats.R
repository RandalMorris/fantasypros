#' NFL Positional Statistics
#'
#' @param pos \code{Charcater}. Specific position you want to return. Default will return all
#'            all offensive postions. Available options include
#'            \itemize{
#'             \item \code{"QB"}
#'             \item \code{"RB"}
#'             \item \code{"WR"}
#'             \item \code{"TE"}
#'             \item \code{"K"}
#'             \item \code{"DST"}
#'            }
#' @param season \code{Numeric}. The NFL season. If missing it will return the current year's season.
#'               Supported season only go back to \code{2013}
#'
#' @return a tibble
#' @export
#'
#' @note \itemize{
#'       \item If you query a single week all players that were on a bye that week are not returned
#'        }
#'
#' @examples
#'
#' Stats_QB <- fp_get_stats_pos("qb")
#' Stats_RB <- fp_get_stats_pos("rb")
#' Stats_WR <- fp_get_stats_pos("wr")
#' Stats_TE <- fp_get_stats_pos("te")
#' Stats_K <- fp_get_stats_pos("k")
#' Stats_DST <- fp_get_stats_pos("dst")
#'
fp_get_stats_data <- function(pos=c("qb", "rb", "wr", "te", "k"), 
                              season = c(2013,2014,2015,2016,2017,2018,2019,2020)) {
  
  base_url="https://www.fantasypros.com/nfl/stats/"

  url=paste0(base_url,pos,".php?&year=",season)
  cat(paste0("Scraping ", toupper(pos), " Season Stats from"), url, sep = "\n  ")
  
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
  fpdf$Season <- season
    
  # this gets columns that are already numbers
  num_cols <- which(grepl("player|team|pos", names(fpdf), ignore.case = TRUE))
  already_numeric <- which(sapply(1:ncol(fpdf), function(i) class(fpdf[, i])) != "character")
  
  num_cols <- sort(unique(c(num_cols, already_numeric)))
  num_cols <- setdiff(1:ncol(fpdf), num_cols)
  num_cols <- names(fpdf)[num_cols]
  fpdf[num_cols][] <- lapply(fpdf[num_cols], readr::parse_number)
  
  if (pos != "dst"){ fpdf$fantasypro_id <- tolower(gsub(" ", "-", fpdf$player))
  } else { fpdf$fantasypro_id <- tolower(paste0(gsub("([A-Za-z]+).*", "\\1", fpdf$player),"-defense")) } 
  
  
  
  fpdf <- dplyr::select(fpdf, Season, player, fantasypro_id, pos, team, dplyr::everything()) %>% janitor::clean_names() 

  fpdf$player <- fpdf$player %>% gsub("II","", .) %>% gsub("III","", .) %>% gsub("IIII","", .) %>% gsub("JR","", .)
  fpdf <- as_tibble(fpdf)
}


fp_get_stats_pos <- function(pos) {
  #retrieve stats for year by selected pos
  FP_2013 <- fp_get_stats_data(pos, 2013)
  FP_2014 <- fp_get_stats_data(pos, 2014)
  FP_2015 <- fp_get_stats_data(pos, 2015)
  FP_2016 <- fp_get_stats_data(pos, 2016)
  FP_2017 <- fp_get_stats_data(pos, 2017)
  FP_2018 <- fp_get_stats_data(pos, 2018)
  FP_2019 <- fp_get_stats_data(pos, 2019)
  FP_2020 <- fp_get_stats_data(pos, 2020)
  
  #combine data to 1 set
  out <- rbind(FP_2013, FP_2014, FP_2015, FP_2016, FP_2017, FP_2018, FP_2019, FP_2020) %>% filter(team != "FA")

}
