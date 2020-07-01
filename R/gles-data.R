#' German Longitudinal Election Study
#'
#' A sample of 1,000 respondents in the Rolling Cross Sectional
#' study in the German Longitudinal Election Study in 2017.
#'
#' @docType data
#'
#' @usage data(gles)
#'
#' @format An data frame with 1,000 observations and 6 variables:
#' \describe{
#'   \item{vote}{Voting decision for party}
#'   \item{egoposition_immigration}{Ego-position toward immigration (0 = very open to 10 = very restrictive )}
#'   \item{ostwest}{Dummy for respondents from Eastern Germany (= 1)}
#'   \item{political_interest}{Measurement for political interst (0 = low, 4 = high)}
#'   \item{income}{Self-reported income satisfaction (0 = low, 4 = high)}
#'   \item{gender}{Self-reported gender (binary coding with 1 = female)}
#' }
#'
#'
#' @keywords datasets
#'
#' @references Roßteutscher, Sigrid et al. 2019.
#' “Rolling Cross-Section-Wahlkampfstudie Mit Nachwahl-Panelwelle (GLES 2017).”
#' ZA6803 Datenfile Version 4.0.1.
#' (\href{https://www.doi.org/10.4232/1.13213}{Gesis Datenarchiv}).
#'
#' @source \href{https://www.doi.org/10.4232/1.13213}{Gesis Datenarchiv}
#'
#' @examples
#' data(gles)
#' table(gles$vote)
"gles"
