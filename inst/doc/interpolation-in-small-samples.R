## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
set.seed(1976)

# create random variables
toocheap <- sort(round(rnorm(n = 50, mean = 8, sd = 1.5), digits = 2))
tooexpensive <- sort(round(rnorm(n = 50, mean = 20, sd = 1), digits = 2))
cheap <- sort(round(rnorm(n = 50, mean = 15, sd = 3), digits = 2))
expensive <- sort(round(rnorm(n = 50, mean = 16, sd = 3), digits = 2))

# setting up alternative "cheap"/"expensive" variables with a price step
pricestep_cheap <- which(cheap > 14.25 & cheap < 17.5)
cheap_alt <- cheap
cheap_alt[pricestep_cheap] <- round(mean(cheap_alt[pricestep_cheap]), digits = 2)

pricestep_expensive <- which(expensive > 14 & expensive < 16)
expensive_alt <- expensive
expensive_alt[pricestep_expensive] <- round(mean(expensive_alt[pricestep_expensive]), digits = 2)

## ---- fig.width = 7, fig.height = 4.5------------------------------------
library(pricesensitivitymeter)

# Running the analysis with price steps and without interpolation
psm_steps <- psm_analysis(toocheap = toocheap,
                          cheap = cheap_alt,
                          expensive = expensive_alt,
                          tooexpensive = tooexpensive,
                          validate = TRUE,
                          interpolate = FALSE)

library(ggplot2)

# Plot with price steps
psmplot_steps <-  ggplot(data = psm_steps$data_vanwestendorp, aes(x = price)) +
  geom_line(aes(y = ecdf_not_cheap, # line: not cheap
                colour = "not cheap"),
            size = 1) +
  geom_line(aes(y = ecdf_not_expensive, # line: not expensive
                colour = "not expensive"),
            size = 1) +
  geom_rug() +
  annotate(geom = "point", # Indifference Price Point (intersection of "not cheap" and "not expensive")
           x = psm_steps$idp,
           y = psm_steps$data_vanwestendorp$ecdf_not_expensive[psm_steps$data_vanwestendorp$price == psm_steps$idp],
           size = 5,
           shape = 18,
           colour = "#D55E00")

psmplot_steps +
  labs(x = "Price",
       y = "Share of Respondents (0-1)",
       title = "Price Sensitivity Meter Plot (without interpolation)")  +
  scale_colour_manual(name = "Legend",
                      values = c("not cheap" = "#009E73",
                                 "not expensive" = "#D55E00")) +
  annotate(geom = "text", # Label of Indifference Price Point
           x = psm_steps$idp + 1,
           y = psm_steps$data_vanwestendorp$ecdf_not_expensive[psm_steps$data_vanwestendorp$price == psm_steps$idp] - 0.05,
           label = paste("IDP: ", psm_steps$idp)) +
  coord_cartesian(xlim = c(10, 20)) +
  theme_minimal()

## ---- fig.width=7, fig.height=4.5----------------------------------------
# Running the analysis with interpolation
psm_interpolated <- psm_analysis(toocheap = toocheap,
                                 cheap = cheap_alt,
                                 expensive = expensive_alt,
                                 tooexpensive = tooexpensive,
                                 validate = TRUE,
                                 interpolate = TRUE)

# Plot with interpolated price steps

psmplot_interpolated <-  ggplot(data = psm_interpolated$data_vanwestendorp, aes(x = price)) +
  geom_line(aes(y = ecdf_not_cheap, # line: not cheap
                colour = "not cheap"),
            size = 1) +
  geom_line(aes(y = ecdf_not_expensive, # line: not expensive
                colour = "not expensive"),
            size = 1) +
  geom_rug() +
  annotate(geom = "point", # Indifference Price Point (intersection of "cheap" and "expensive")
           x = psm_interpolated$idp,
           y = psm_interpolated$data_vanwestendorp$ecdf_not_expensive[psm_interpolated$data_vanwestendorp$price == psm_interpolated$idp],
           size = 5,
           shape = 18,
           colour = "#D55E00")

psmplot_interpolated +
  labs(x = "Price",
       y = "Share of Respondents (0-1)",
       title = "Price Sensitivity Meter Plot (with interpolation)")  +
  scale_colour_manual(name = "Legend",
                      values = c("not cheap" = "#009E73",
                                 "not expensive" = "#D55E00")) +
  annotate(geom = "text", # Label of Indifference Price Point
           x = psm_interpolated$idp + 1,
           y = psm_interpolated$data_vanwestendorp$ecdf_not_expensive[psm_interpolated$data_vanwestendorp$price == psm_interpolated$idp],
           label = paste("IDP: ", psm_interpolated$idp)) +
  coord_cartesian(xlim = c(10, 20)) +
  theme_minimal()

## ---- fig.width=7, fig.height=4.5, echo = FALSE--------------------------
psmplot_comparison <-  ggplot(data = psm_interpolated$data_vanwestendorp, aes(x = price)) +
  geom_line(aes(y = ecdf_not_cheap, # line: not cheap, interpolated
                colour = "not cheap"),
            size = 4, alpha = 0.25) +
  geom_line(aes(y = ecdf_not_expensive, # line: not expensive, interpolated
                colour = "not expensive"),
            size = 4, alpha = 0.25) +
  geom_line(data = psm_steps$data_vanwestendorp, # line: not cheap, not interpolated
            aes(y = ecdf_not_cheap, 
                colour = "not cheap"),
            size = 1) +
  geom_line(data = psm_steps$data_vanwestendorp, # line: not expensive, not interpolated
            aes(y = ecdf_not_expensive, 
                colour = "not expensive"),
            size = 1)

psmplot_comparison +
  labs(x = "Price",
       y = "Share of Respondents (0-1)",
       title = "Price Sensitivity Meter Plot: with vs. without interpolation")  +
  scale_colour_manual(name = "Legend",
                      values = c("not cheap" = "#009E73",
                                 "not expensive" = "#D55E00")) +
  coord_cartesian(xlim = c(10, 20)) +
  theme_minimal()

