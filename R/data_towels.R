#' Data: Reuse of Towels in Hotels
#'
#' Set of studies that investigated how often people reuse towels in hotels (Scheibehenne, Jamil, & Wagenmakers, 2016).
#' @details
#'  Two groups of hotel guests received different messages that encouraged them to reuse their towels. One message simply informed the guests about the benefits of environmental protection (the control condition), and the other message indicated that the majority of guests actually reused their towels in the past (the descriptive-social-norm condition). The results suggested that the latter message facilitated towel reuse.
#'
#' @format A data frame 2 variables:
#' \describe{
#'   \item{\code{study}}{original study (see Scheibehenne et. al, 2016)}
#'   \item{\code{logOR}}{log-odds ratio of towel reuse (descriptive-social-norm vs. control)}
#'   \item{\code{SE}}{standard error of log-odds ratio per study}
#' }
#' @references Scheibehenne, B., Jamil, T., & Wagenmakers, E.-J. (2016). Bayesian Evidence Synthesis Can Reconcile Seemingly Inconsistent Results: The Case of Hotel Towel Reuse. Psychological Science, 27(7), 1043–1046. \url{https://doi.org/10.1177/0956797616644081}
#' @examples
#' data(towels)
#' head(towels)
"towels"
