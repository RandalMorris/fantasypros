#' Fantasy Pros Expert Consensus Draft Rankings
#'
#' @param pos \code{Charcater}. Specific position you want to return. Default will return overall.
#'            Available options include
#'            \itemize{
#'             \item \code{"overall"}
#'             \item \code{"QB"}
#'             \item \code{"RB"}
#'             \item \code{"WR"}
#'             \item \code{"TE"}
#'             \item \code{"K"}
#'             \item \code{"DST"}
#'             \item \code{"IDP"}
#'             \item \code{"DL"}
#'             \item \code{"LB"}
#'             \item \code{"DB"}
#'             \item \code{"Team QB"}
#'             \item \code{"Team RB"}
#'             \item \code{"Team WR"}
#'             \item \code{"Team TE"}
#'             \item \code{"Team K"}
#'             \item \code{"Team OL"}
#'             \item \code{"Head Coach"}
#'            }
#' @param scoring \code{Charcater}. Fantasy scoring format. Default is \code{"half"}
#'
#' @return a tibble
#' @export
#'
#' @examples
#'
#' # overall ECR for half point ppr
#' fp_draft_rankings()
#'
#' # TE ECR using standard scoring
#' fp_draft_rankings(pos = "TE", scoring = "std")
#'
#' # Individual Defensive Player ECR
#' fp_draft_rankings(pos = "IDP")
fp_draft_rankings <- function(pos = c("ALL","QB","RB","WR","TE"), scoring = c("HALF", "PPR", "STD"),
                              year=2021, 
                              type="draft",
                              week=0) {
  pos <- match.arg(pos)
  scoring <- match.arg(scoring)
  if (exists("pos") == F) pos <- "ALL" else pos <- toupper(pos)
  if (exists("scoring") == F) scoring <- "ALL" else scoring <- toupper(scoring)
  if (exists("type") == F) type <- "draft" else type <- tolower(type)
  
  #url = paste0("https://api.fantasypros.com/v2/json/nfl/",year,"/consensus-rankings?type=",type,
  #             "&scoring=",scoring,"&position=",pos,"&week=",week,"&experts=available")
  #user_agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
  #x_api_key="zjxN52G3lP4fORpHRftGI2mTU8cTwxVNvkjByM3j"
  #authorization = "Basic WC1BcGktS2V5OnpqeE41MkczbFA0Zk9ScEhSZnRHSTJtVFU4Y1R3eFZOdmtqQnlNM2o="
  
  fp_auth = fp_auth()
  fp_url <- fp_build_url(year = year, type = type, scoring = scoring, pos = pos, week = week)
    
  res = httr::GET(fp_url,httr::add_headers("user-agent" = fp_auth$user_agent,
                                  "x-api-key" =  fp_auth$x_api_key,
                                  "Authorization" =  fp_auth$authorization))
  
  raw_data <- httr::content(res, "parsed", "application/json") 
  raw_data <- raw_data[["players"]]
  data <- data.frame(t(sapply(raw_data,c)))
  data <- janitor::clean_names(data) %>% as_tibble()
  return(data)
}
