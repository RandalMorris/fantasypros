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
#'"Team QB","Team RB","Team WR","Team TE","Team K","Team OL","COACH"), 
#' # Individual Defensive Player ECR
#' fp_draft_rankings(pos = "IDP")
fp_draft_rankings <- function(pos = c("ALL","QB","RB","WR","TE","K","DST","IDP","DL","LB","DB"),
                                     scoring = c("HALF", "PPR", "STD"),
                              year=2021, 
                              type=c("both","draft","adp"),
                              week=0) {
  pos <- match.arg(pos)
  type <- match.arg(type)
  scoring <- match.arg(scoring)
  if (exists("pos") == F) pos <- "ALL" else pos <- toupper(pos)
  if (exists("scoring") == F) scoring <- "ALL" else scoring <- toupper(scoring)
  if (exists("type") == F) type <- "draft" else type <- tolower(type)
  
  fp_ranking_data <- fp_get_ranking_data(pos=pos, scoring=scoring, year=year, type=type, week=week)
  
  
 }
