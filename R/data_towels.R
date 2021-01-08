#' Data Set: Reuse of Towels in Hotels
#'
#' Set of studies that investigated whether people reuse towels in hotels more often if they are provided with a descriptive norm (Scheibehenne, Jamil, & Wagenmakers, 2016).
#' @details
#'  Two groups of hotel guests received different messages that encouraged them to reuse their towels. One message simply informed the guests about the benefits of environmental protection (the control condition), and the other message indicated that the majority of guests actually reused their towels in the past (the descriptive-social-norm condition). The results suggested that the latter message facilitated towel reuse.
#'
#' @format A data frame with three variables:
#' \describe{
#'   \item{\code{study}}{Authors of original study (see Scheibehenne et. al, 2016)}
#'   \item{\code{logOR}}{Measure of effect size: log-odds ratio of towel reuse (descriptive-social-norm vs. control)}
#'   \item{\code{SE}}{Measure of precision: standard error of log-odds ratio per study}
#' }
#' @references
#' Scheibehenne, B., Jamil, T., & Wagenmakers, E.-J. (2016). Bayesian Evidence Synthesis Can Reconcile Seemingly Inconsistent Results: The Case of Hotel Towel Reuse. Psychological Science, 27, 1043–1046. \doi{10.1177/0956797616644081}
#' @examples
#' data(towels)
#' head(towels)
"towels"
