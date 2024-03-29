context("Data Output Checks")

#----
# Setting up test dataframes: with and without NMS
#----

data_psm_test <- data.frame(
  tch = round(rnorm(n = 20, mean = 5, sd = 0.5), digits = 2),
  ch = round(rnorm(n = 20, mean = 6, sd = 1), digits = 2),
  ex = round(rnorm(n = 20, mean = 13, sd = 0.75), digits = 2),
  tex = round(rnorm(n = 20, mean = 17, sd = 1), digits = 2),
  pi_cheap = sample(x = c(1:5), size = 20, replace = TRUE, prob = c(0.0, 0.1, 0.2, 0.3, 0.5)),
  pi_expensive = sample(x = c(1:5), size = 20, replace = TRUE, prob = c(0.0, 0.1, 0.2, 0.3, 0.5))
  )

psm_result1 <- psm_analysis(toocheap = "tch",
                            cheap = "ch",
                            expensive = "ex",
                            tooexpensive = "tex",
                            data = data_psm_test)

psm_result2 <- psm_analysis(toocheap = "tch",
                            cheap = "ch",
                            expensive = "ex",
                            tooexpensive = "tex",
                            pi_cheap = "pi_cheap",
                            pi_expensive = "pi_expensive",
                            data = data_psm_test)

psm_result3 <- psm_analysis(toocheap = "tch",
                            cheap = "ch",
                            expensive = "ex",
                            tooexpensive = "tex",
                            pi_cheap = "pi_cheap",
                            pi_expensive = "pi_expensive",
                            data = data_psm_test,
                            interpolate = TRUE)

#----
# Overall Features of the Output Object
#----

test_that("Data Output: Length of Output Object", {
  expect_length(psm_result1, 12)
  expect_length(psm_result2, 16)
})

test_that("Data Output: Class of Output Object", {
  expect_equal(class(psm_result1), "psm")
  expect_equal(class(psm_result2), "psm")
})

#----
# Internal Structure of the Output Object
#----

test_that("Data Output: Matrices have rows and columns", {
  expect_gt(nrow(psm_result1$data_input), 0)
  expect_gt(nrow(psm_result1$data_vanwestendorp), 0)
  expect_gt(nrow(psm_result2$data_input), 0)
  expect_gt(nrow(psm_result2$data_vanwestendorp), 0)
  expect_gt(nrow(psm_result2$data_nms), 0)

  expect_equal(ncol(psm_result1$data_input), 4)
  expect_equal(ncol(psm_result1$data_vanwestendorp), 7)

  expect_equal(ncol(psm_result2$data_input), 8)
  expect_equal(ncol(psm_result2$data_vanwestendorp), 7)
  expect_equal(ncol(psm_result2$data_nms), 3)
})

test_that("Data Output: Numeric Data in Matrices", {
  expect_true(unique(sapply(psm_result1$data_input, is.numeric)))
  expect_true(unique(sapply(psm_result1$data_vanwestendorp, is.numeric)))

  expect_true(unique(sapply(psm_result2$data_input, is.numeric)))
  expect_true(unique(sapply(psm_result2$data_vanwestendorp, is.numeric)))
  expect_true(unique(sapply(psm_result2$data_nms, is.numeric)))
})


test_that("Data Output: Rest of Output Object Structure", {
  # Standard PSM
  expect_true(is.logical(psm_result1$validated))
  expect_length(psm_result1$validated, 1)

  expect_true(is.logical(psm_result1$weighted))
  expect_length(psm_result1$weighted, 1)
  expect_false(psm_result1$weighted)

  expect_true(is.numeric(psm_result1$invalid_cases))
  expect_false(is.nan(psm_result1$invalid_cases))
  expect_length(psm_result1$invalid_cases, 1)

  expect_true(is.numeric(psm_result1$total_sample))
  expect_false(is.nan(psm_result1$total_sample))
  expect_length(psm_result1$total_sample, 1)

  expect_true(is.numeric(psm_result1$pricerange_lower))
  expect_false(is.nan(psm_result1$pricerange_lower))
  expect_length(psm_result1$pricerange_lower, 1)

  expect_true(is.numeric(psm_result1$pricerange_upper))
  expect_false(is.nan(psm_result1$pricerange_upper))
  expect_length(psm_result1$pricerange_upper, 1)

  expect_true(is.numeric(psm_result1$idp))
  expect_false(is.nan(psm_result1$idp))
  expect_length(psm_result1$idp, 1)

  expect_true(is.numeric(psm_result1$opp))
  expect_false(is.nan(psm_result1$opp))
  expect_length(psm_result1$opp, 1)

  expect_true(is.logical(psm_result1$nms))
  expect_false(is.nan(psm_result1$nms))
  expect_length(psm_result1$nms, 1)

  # PSM with NMS
  expect_true(is.logical(psm_result2$validated))
  expect_length(psm_result2$validated, 1)

  expect_true(is.numeric(psm_result2$invalid_cases))
  expect_false(is.nan(psm_result2$invalid_cases))
  expect_length(psm_result2$invalid_cases, 1)

  expect_true(is.numeric(psm_result2$total_sample))
  expect_false(is.nan(psm_result2$total_sample))
  expect_length(psm_result2$total_sample, 1)

  expect_true(is.numeric(psm_result2$pricerange_lower))
  expect_false(is.nan(psm_result2$pricerange_lower))
  expect_length(psm_result2$pricerange_lower, 1)

  expect_true(is.numeric(psm_result2$pricerange_upper))
  expect_false(is.nan(psm_result2$pricerange_upper))
  expect_length(psm_result2$pricerange_upper, 1)

  expect_true(is.numeric(psm_result2$idp))
  expect_false(is.nan(psm_result2$idp))
  expect_length(psm_result2$idp, 1)

  expect_true(is.numeric(psm_result2$opp))
  expect_false(is.nan(psm_result2$opp))
  expect_length(psm_result2$opp, 1)

  expect_true(is.logical(psm_result2$nms))
  expect_false(is.nan(psm_result2$nms))
  expect_length(psm_result2$nms, 1)

  expect_true(is.numeric(psm_result2$pi_scale$pi_calibrated))
  expect_false(unique(is.nan(psm_result2$pi_scale$pi_calibrated)))

  expect_true(is.numeric(psm_result2$price_optimal_revenue))
  expect_false(is.nan(psm_result2$price_optimal_revenue))
  expect_length(psm_result2$price_optimal_revenue, 1)

  expect_true(is.numeric(psm_result2$price_optimal_revenue))
  expect_false(is.nan(psm_result2$price_optimal_revenue))
  expect_length(psm_result2$price_optimal_revenue, 1)
})

#----
# Expecting Specific Values
#----

test_that("Data Output: NMS correctly (not) included in output", {
  expect_false(psm_result1$nms)
  expect_true(psm_result2$nms)
})

test_that("Data Output: All prices included in the empirical cumulative density function data", {
  expect_true(unique(psm_result1$data_input$toocheap %in% psm_result1$data_vanwestendorp$price))
  expect_true(unique(psm_result1$data_input$cheap %in% psm_result1$data_vanwestendorp$price))
  expect_true(unique(psm_result1$data_input$expensive %in% psm_result1$data_vanwestendorp$price))
  expect_true(unique(psm_result1$data_input$tooexpensive %in% psm_result1$data_vanwestendorp$price))
})

test_that("Data Output: All prices included in the NMS data", {
  expect_equal(min(psm_result2$data_vanwestendorp$price), min(psm_result2$data_nms$price))
  expect_equal(max(psm_result2$data_vanwestendorp$price), max(psm_result2$data_nms$price))
})

#----
# Expecting Specific Values
#----

test_that("Data Output: When interpolating, have the full numeric price matrix without any missing values", {
  expect_equal(nrow(psm_result3$data_vanwestendorp), length(seq.int(from = min(psm_result3$data_input$toocheap, psm_result3$data_input$cheap, psm_result3$data_input$expensive, psm_result3$data_input$tooexpensive), to = max(psm_result3$data_input$toocheap, psm_result3$data_input$cheap, psm_result3$data_input$expensive, psm_result3$data_input$tooexpensive), by = 0.01)))

  expect_false(anyNA(psm_result3$data_vanwestendorp))

  expect_equal(ncol(psm_result3$data_vanwestendorp), sum(sapply(psm_result3$data_vanwestendorp, is.numeric)))
})

# Constant Curvature:
# "too cheap" and "cheap" should decrease continuously
# "expensive" and "too expensive" should increase continuously

# Creating a lagged variable of the empirical cumulative densities

psm_result3$data_vanwestendorp$ecdf_toocheap_lagged <- c(NA, psm_result3$data_vanwestendorp$ecdf_toocheap[1:(length(psm_result3$data_vanwestendorp$ecdf_toocheap) - 1)])
psm_result3$data_vanwestendorp$ecdf_cheap_lagged <- c(NA, psm_result3$data_vanwestendorp$ecdf_cheap[1:(length(psm_result3$data_vanwestendorp$ecdf_cheap) - 1)])
psm_result3$data_vanwestendorp$ecdf_expensive_lagged <- c(NA, psm_result3$data_vanwestendorp$ecdf_expensive[1:(length(psm_result3$data_vanwestendorp$ecdf_expensive) - 1)])
psm_result3$data_vanwestendorp$ecdf_tooexpensive_lagged <- c(NA, psm_result3$data_vanwestendorp$ecdf_tooexpensive[1:(length(psm_result3$data_vanwestendorp$ecdf_tooexpensive) - 1)])

# Calculate the difference between the empirical cumulative density and the lagged variant
psm_result3$data_vanwestendorp$ecdf_toocheap_diff <- psm_result3$data_vanwestendorp$ecdf_toocheap - psm_result3$data_vanwestendorp$ecdf_toocheap_lagged
psm_result3$data_vanwestendorp$ecdf_cheap_diff <- psm_result3$data_vanwestendorp$ecdf_cheap - psm_result3$data_vanwestendorp$ecdf_cheap_lagged
psm_result3$data_vanwestendorp$ecdf_expensive_diff <- psm_result3$data_vanwestendorp$ecdf_expensive - psm_result3$data_vanwestendorp$ecdf_expensive_lagged
psm_result3$data_vanwestendorp$ecdf_tooexpensive_diff <- psm_result3$data_vanwestendorp$ecdf_tooexpensive - psm_result3$data_vanwestendorp$ecdf_tooexpensive_lagged



test_that("Data Output: When interpolating, have the full numeric price matrix without any missing values", {
 # "too cheap"
  expect_lte(min(psm_result3$data_vanwestendorp$ecdf_toocheap_diff[!is.na(psm_result3$data_vanwestendorp$ecdf_toocheap_diff)]), 0)
  expect_lte(max(psm_result3$data_vanwestendorp$ecdf_toocheap_diff[!is.na(psm_result3$data_vanwestendorp$ecdf_toocheap_diff)]), 0)

  # "cheap"
  expect_lte(min(psm_result3$data_vanwestendorp$ecdf_cheap_diff[!is.na(psm_result3$data_vanwestendorp$ecdf_cheap_diff)]), 0)
  expect_lte(max(psm_result3$data_vanwestendorp$ecdf_cheap_diff[!is.na(psm_result3$data_vanwestendorp$ecdf_cheap_diff)]), 0)

  # "expensive"
  expect_gte(min(psm_result3$data_vanwestendorp$ecdf_expensive_diff[!is.na(psm_result3$data_vanwestendorp$ecdf_expensive_diff)]), 0)
  expect_gte(max(psm_result3$data_vanwestendorp$ecdf_expensive_diff[!is.na(psm_result3$data_vanwestendorp$ecdf_expensive_diff)]), 0)

  # "too expensive"
  expect_gte(min(psm_result3$data_vanwestendorp$ecdf_tooexpensive_diff[!is.na(psm_result3$data_vanwestendorp$ecdf_tooexpensive_diff)]), 0)
  expect_gte(max(psm_result3$data_vanwestendorp$ecdf_tooexpensive_diff[!is.na(psm_result3$data_vanwestendorp$ecdf_tooexpensive_diff)]), 0)
})


#----
# Plausible Values
#----

test_that("Data Output - Plausibility: Number of total cases must be greater than the number of invalid cases", {
  expect_gt(psm_result1$total_sample, psm_result1$invalid_cases)
  expect_gt(psm_result2$total_sample, psm_result2$invalid_cases)
})

test_that("Data Output - Plausibility: Price estimations must be within range of provided prices", {
  expect_gte(psm_result1$pricerange_lower, min(psm_result1$data_vanwestendorp$price))
  expect_lte(psm_result1$pricerange_lower, max(psm_result1$data_vanwestendorp$price))

  expect_gte(psm_result1$pricerange_upper, min(psm_result1$data_vanwestendorp$price))
  expect_lte(psm_result1$pricerange_upper, max(psm_result1$data_vanwestendorp$price))

  expect_gte(psm_result1$idp, min(psm_result1$data_vanwestendorp$price))
  expect_lte(psm_result1$idp, max(psm_result1$data_vanwestendorp$price))

  expect_gte(psm_result1$opp, min(psm_result1$data_vanwestendorp$price))
  expect_lte(psm_result1$opp, max(psm_result1$data_vanwestendorp$price))

  expect_gte(psm_result2$price_optimal_revenue, min(psm_result2$data_vanwestendorp$price))
  expect_lte(psm_result2$price_optimal_revenue, max(psm_result2$data_vanwestendorp$price))

  expect_gte(psm_result2$price_optimal_revenue, min(psm_result2$data_vanwestendorp$price))
  expect_lte(psm_result2$price_optimal_revenue, max(psm_result2$data_vanwestendorp$price))
})

test_that("Data Output - Plausibility: Hierarchy between lower and upper limit of price range", {
  expect_gte(psm_result1$pricerange_upper, psm_result1$pricerange_lower)
})

test_that("Data Output - Plausibility: Hierarchy between IDP, OPP and limits of price range", {
  expect_gte(psm_result1$idp, psm_result1$pricerange_lower)
  expect_lte(psm_result1$idp, psm_result1$pricerange_upper)

  expect_gte(psm_result1$opp, psm_result1$pricerange_lower)
  expect_lte(psm_result1$opp, psm_result1$pricerange_upper)
})

test_that("Data Output - Plausibility: Consistent price esimations across models", {
  expect_equal(psm_result1$pricerange_lower, psm_result2$pricerange_lower)
  expect_equal(psm_result1$pricerange_upper, psm_result2$pricerange_upper)
  expect_equal(psm_result1$idp, psm_result2$idp)
  expect_equal(psm_result1$opp, psm_result2$opp)
})


#----
# Structure of Output Object
#----

test_that("Ensuring that cases with invalid cases are removed", {
  expect_equal(nrow(psm_result1$data_input), psm_result1$total_sample - psm_result1$invalid_cases)
})

# clean up workspace after test
rm(data_psm_test, psm_result1, psm_result2, psm_result3)
