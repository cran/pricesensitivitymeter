#---------------------
# Meta: PSM Class Definition, Summary Function and Internal Helpers
#---------------------

#-------
# Definition of PSM Class

setOldClass("survey.design2")
psm.class <- setClass("psm", slots = c(
  data_input = "data.frame",
  validated = "logical",
  invalid_cases = "numeric",
  total_sample = "numeric",
  data_vanwestendorp = "data.frame",
  pricerange_lower = "numeric",
  pricerange_upper = "numeric",
  idp = "numeric",
  opp = "numeric",
  weighted = "logical",
  survey_design = "survey.design2",
  NMS = "logical",
  data_nms = "data.frame",
  pi_scale = "data.frame",
  price_optimal_reach = "numeric",
  price_optimal_revenue = "numeric"
))

#-------
# Summary Function for PSM Class

summary.psm <- function(object, ...) {
  cat("Van Westendorp Price Sensitivity Meter Analysis\n\n")

  cat("Accepted Price Range:", round(object$pricerange_lower, digits = 2), "-", round(object$pricerange_upper, digits = 2), "\n")
  cat("Indifference Price Point:", object$idp, "\n")
  cat("Optimal Price Point:", object$opp, "\n\n")

  if (object$nms == TRUE) {
    cat("Newton Miller Smith Extension\n")
    cat("Price with Optimal Reach:", object$price_optimal_reach, "\n")
    cat("Price with Optimal Revenue:", object$price_optimal_revenue, "\n\n")
  }

  cat("---\n")
  cat(ifelse(object$validated == TRUE, object$total_sample - object$invalid_cases, object$total_sample), " cases with individual price preferences were analyzed (", ifelse(object$weighted == TRUE, "weighted", "unweighted"), " data).\n", sep = "")

  if (object$invalid_cases > 0) {
    cat("Total data set consists of ", object$total_sample, " cases. Analysis was ",
      ifelse(object$validated == TRUE, "", "not "), "limited to cases with transitive price preferences.\n",
      sep = ""
    )

    if (object$validated == TRUE) {
      cat("(Removed: n = ", object$invalid_cases, " / ", round(object$invalid_cases / object$total_sample * 100, digits = 0), "% of data)", sep = "")
    } else {
      cat("Consider re-running the analysis with option 'validate = TRUE' to exclude all cases with invalid price preferences (n = ", object$invalid_cases, ")", sep = "")
    }
  } # end of "invalid cases" section
}
