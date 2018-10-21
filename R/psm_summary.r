# Definition of psm class

psm.class <- setClass("psm", slots = c(data_input = "data.frame",
                                       validated = "logical",
                                       invalid_cases = "numeric",
                                       total_sample = "numeric",
                                       data_vanwestendorp = "data.frame",
                                       pricerange_lower = "numeric",
                                       pricerange_upper = "numeric",
                                       idp = "numeric",
                                       opp = "numeric",
                                       NMS = "logical",
                                       data_nms = "data.frame",
                                       pi_scale = "data.frame",
                                       price_optimal_trial = "numeric",
                                       price_optimal_revenue = "numeric"))


# summary function for psm class

summary.psm <- function(object, ...) {
  cat("Van Westendorp Price Sensitivity Meter Analysis\n\n")

  cat("Accepted Price Range:", round(object$pricerange_lower, digits = 2), "-", round(object$pricerange_upper, digits = 2),"\n")
  cat("Indifference Price Point:", object$idp,"\n")
  cat("Optimal Price Point:", object$opp, "\n\n")

  if(object$NMS == TRUE) {
    cat("Newton Miller Smith Extension\n")
    cat("Price with Optimal Trial Rate:", object$price_optimal_trial, "\n")
    cat("Price with Optimal Revenue:", object$price_optimal_revenue, "\n\n")
  }

  cat("---\n")
  cat(ifelse(object$validated == TRUE, object$total_sample - object$invalid_cases, object$total_sample), "cases with individual price preferences were analyzed\n")

  if(object$invalid_cases > 0) {
    cat("Total data set consists of ", object$total_sample, " cases. Analysis was ",
        ifelse(object$validated == TRUE, "", "not "), "limited to cases with transitive price preferences.\n", sep = "")

    if(object$validated == TRUE) {
      cat("(Removed: n = ", object$invalid_cases, " / ", round(object$invalid_cases / object$total_sample * 100, digits = 0), "% of data)", sep = "")
    } else {
      cat("Consider re-running the analysis with option 'validate = TRUE' to exclude all cases with invalid price preferences (n = ", object$invalid_cases, ")", sep = "")
    }
  } # end of "invalid cases" section
}