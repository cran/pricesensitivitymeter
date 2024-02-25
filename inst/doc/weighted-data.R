## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE----------------------------------------------------------------
set.seed(1976)
library(survey)

# Creating dataset with price acceptance and biased gender variable
input_data <- data.frame(tch = round(rnorm(n = 250, mean = 8, sd = 0.5), digits = 2),
                         ch = round(rnorm(n = 250, mean = 12, sd = 0.5), digits = 2),
                         ex = round(rnorm(n = 250, mean = 13, sd = 0.5), digits = 2),
                         tex = round(rnorm(n = 250, mean = 15, sd = 0.5), digits = 2),
                         gender = sample(x = c("male", "female"),
                                         size = 250,
                                         replace = TRUE,
                                         prob = c(2/3, 1/3)))

# for women: increasing the price acceptance by +50%
input_data$tch[input_data$gender == "female"] <- input_data$tch[input_data$gender == "female"] * 1.5
input_data$ch[input_data$gender == "female"] <- input_data$ch[input_data$gender == "female"] * 1.5
input_data$ex[input_data$gender == "female"] <- input_data$ex[input_data$gender == "female"] * 1.5
input_data$tex[input_data$gender == "female"] <- input_data$tex[input_data$gender == "female"] * 1.5

# for survey design object: occurrence of each gender in the target population
# it's only one figure in this example because we assume that gender should be perfectly balanced. 
# if this is not balanced in the population, we would need a vector with the number of occurrences in the population.
# the sum of the strata size across all strata gives the total population size
# (here: two strata with 5k each = 10k total population) 
input_data$gender_pop <- 5000

# creating the survey design object for post-stratification based on gender
# we assume that the selection of respondents within each gender is biased and...
# only the gender balance in the sample is problematic
input_design <- survey::svydesign(ids = ~ 1, # no clusters
                          probs = NULL, # hence no cluster sampling probabilities,
                          strata = input_data$gender, # stratified by gender
                          fpc = input_data$gender_pop, # strata size in the population
                          data = input_data) # data object used as input

## ----echo=TRUE----------------------------------------------------------------
library(pricesensitivitymeter)

## ----label='c1', echo=TRUE, cache=TRUE----------------------------------------
output_weighted_psm <- psm_analysis_weighted(toocheap = "tch",
                                             cheap = "ch",
                                             expensive = "ex",
                                             tooexpensive = "tex",
                                             design = input_design)

summary(output_weighted_psm)

## ----echo=TRUE----------------------------------------------------------------
set.seed(20)
library(survey)

## ----label='c5', echo=TRUE, cache=TRUE----------------------------------------
# Creating dataset with price acceptance and unbiased gender variable
input_data_2 <- data.frame(tch = round(rnorm(n = 250, mean = 4, sd = 0.5), digits = 2),
                         ch = round(rnorm(n = 250, mean = 8, sd = 0.5), digits = 2),
                         ex = round(rnorm(n = 250, mean = 12, sd = 0.5), digits = 2),
                         tex = round(rnorm(n = 250, mean = 16, sd = 0.5), digits = 2),
                         gender = sample(x = c("male", "female"),
                                         size = 250,
                                         replace = TRUE,
                                         prob = c(0.5, 0.5)))

# for women: increasing the price acceptance by +50%
input_data_2$tch[input_data_2$gender == "female"] <- input_data_2$tch[input_data_2$gender == "female"] * 1.5
input_data_2$ch[input_data_2$gender == "female"] <- input_data_2$ch[input_data_2$gender == "female"] * 1.5
input_data_2$ex[input_data_2$gender == "female"] <- input_data_2$ex[input_data_2$gender == "female"] * 1.5
input_data_2$tex[input_data_2$gender == "female"] <- input_data_2$tex[input_data_2$gender == "female"] * 1.5

# now let's create a sample design object (using the survey package)
# ... assuming that gender is balanced equally in the population of 10000

# for survey design object: occurrence of each gender in the target population
# would usually be information from sampling frame, differs here only for demonstration purposes
# here: scaling up based on actual sample information (hypothetical population of 250 * 4 = 10k)
input_data_2$gender_pop <- NA
input_data_2$gender_pop[input_data_2$gender == "female"] <- sum(input_data_2$gender == "female") * 40
input_data_2$gender_pop[input_data_2$gender == "male"] <- sum(input_data_2$gender == "male") * 40

# creating the survey design object for post-stratification based on gender
input_design_2 <- survey::svydesign(ids = ~ 1, # no clusters
                                  probs = NULL, # hence no cluster sampling probabilities,
                                  strata = input_data_2$gender, # stratified by gender
                                  fpc = input_data_2$gender_pop, # strata size in the population
                                  data = input_data_2) # data object used as input

# quick check: there is only one weight for all our strata
# if we would have different weights per gender, we would see two unique values here
unique(weights(input_design_2, type = "analysis"))

## ----label='c2', echo=TRUE, cache=TRUE----------------------------------------
# running both weighted and unweighted analysis on the same data 
check_weighted_1 <- psm_analysis_weighted(toocheap = "tch",
                                          cheap = "ch",
                                          expensive = "ex",
                                          tooexpensive = "tex",
                                          design = input_design_2)

check_unweighted_1 <- psm_analysis(toocheap = "tch",
                                   cheap = "ch",
                                   expensive = "ex",
                                   tooexpensive = "tex",
                                   data = input_data_2)

# results should be identical
summary(check_weighted_1)
summary(check_unweighted_1)

## -----------------------------------------------------------------------------
input_data_3 <- input_data_2

manipulated_men <- sample(which(input_data_2$gender == "male"), 10)
input_data_3$ch[manipulated_men] <- input_data_3$tex[manipulated_men]

## ----label='c3', cache=TRUE---------------------------------------------------
# creating the survey design object for post-stratification based on gender
input_design_3 <- survey::svydesign(ids = ~ 1, # no clusters
                                  probs = NULL, # hence no cluster sampling probabilities,
                                  strata = input_data_3$gender, # stratified by gender
                                  fpc = input_data_3$gender_pop, # strata size in the population
                                  data = input_data_3) # data object used as input

check_weighted_2 <- psm_analysis_weighted(toocheap = "tch",
                                          cheap = "ch",
                                          expensive = "ex",
                                          tooexpensive = "tex",
                                          design = input_design_3)

check_unweighted_2 <- psm_analysis(toocheap = "tch",
                                   cheap = "ch",
                                   expensive = "ex",
                                   tooexpensive = "tex",
                                   data = input_data_3)

# results should be different now
summary(check_weighted_2)
summary(check_unweighted_2)


## ----label='c4', cache=TRUE---------------------------------------------------
# setting up data with NAs in "too cheap" variable
input_data_2 <- input_data
input_data_2$tch <- NA

# create new sample design
input_design_2 <- survey::svydesign(ids = ~ 1, # no clusters
                                    probs = NULL, # hence no cluster samling probabilities,
                                    strata = input_data_2$gender, # stratified by gender
                                    fpc = input_data_2$gender_pop, # strata size in the population
                                    data = input_data_2) # data object used as input

test_2 <- psm_analysis_weighted(toocheap = "tch",
                                cheap = "ch",
                                expensive = "ex",
                                tooexpensive = "tex",
                                design = input_design_2)
summary(test_2)


## -----------------------------------------------------------------------------
# setting up dataset with purchase intent information
input_data_3 <- input_data

input_data_3$pi_ch <- sample(x = c(1:5), size = nrow(input_data_3),
                             replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3))

input_data_3$pi_ex <- sample(x = c(1:5), size = nrow(input_data_3),
                             replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.1, 0.1))


# re-creating the survey design object
input_design_3 <- survey::svydesign(ids = ~ 1,
                                    probs = NULL,
                                    strata = input_data_3$gender,
                                    fpc = input_data_3$gender_pop,
                                    data = input_data_3) 

# running the weighted Price Sensitivity Meter analysis
test_3 <- psm_analysis_weighted(toocheap = "tch",
                                cheap = "ch",
                                expensive = "ex",
                                tooexpensive = "tex",
                                design = input_design_3,
                                pi_cheap = "pi_ch",
                                pi_expensive = "pi_ex",
                                pi_scale = 5:1,
                                pi_calibrated = c(0.7, 0.5, 0.3, 0.1, 0))

summary(test_3)


