#' Data Set: Power Pose Effect
#'
#' Includes six pre-registered replication studies testing whether participants feel more powerful if they adopt expansive as opposed to constrictive body postures. In the data set \code{power_pose_unfamiliar}, only those participants are included who were unfamiliar with the power pose effect.
#' @details
#' See Carney, Cuddy, and Yap (2010) for more details.
#'
#' @format A data frame with three variables:
#' \describe{
#'   \item{\code{study}}{Authors of original study}
#'   \item{\code{n_high_power}}{number of participants in high-power condition}
#'   \item{\code{n_low_power}}{number of participants in low-power condition}
#'   \item{\code{mean_high_power}}{mean rating in high-power condition on a 5-point Likert scale}
#'   \item{\code{mean_low_power}}{mean rating in low-power condition on a 5-point Likert scale}
#'   \item{\code{sd_high_power}}{standard deviation of ratings in high-power condition}
#'   \item{\code{sd_low_power}}{standard deviation of ratings in low-power condition}
#'   \item{\code{t_value}}{t-value for two-sample t-test}
#'   \item{\code{df}}{degrees of freedom for two-sample t-test}
#'   \item{\code{two_sided_p_value}}{two-sided p-value of two-sample t-test}
#'   \item{\code{one_sided_p_value}}{one-sided p-value of two-sample t-test}
#'   \item{\code{effectSize}}{Cohen's d, the standardized effect size (high vs. low power)}
#'   \item{\code{SE}}{Standard error of Cohen's d}
#' }
#' @references
#' Carney, D. R., Cuddy, A. J. C., & Yap, A. J. (2010). Power posing: Brief nonverbal displays affect neuroendocrine levels and risk tolerance. Psychological Science, 21, 1363–1368.
#'
#' Gronau, van Erp, Heck, Cesario, Jonas, & Wagenmakers (in preparation). A Bayesian model-averaged meta-analysis of the power pose effect with informed and default priors: The case of felt power.
#'
#' @format Data frame with 6 rows and 13 variables
#' @examples
#' data(power_pose)
#' head(power_pose)
#'
#' # Simple fixed-effects meta-analysis
#' mfix <- meta_fixed(power_pose$effectSize, power_pose$SE,
#'                    power_pose$study)
#' mfix
#' plot_posterior(mfix)
"power_pose"


#' @rdname power_pose
"power_pose_unfamiliar"
