## Functions necessary to define the Hall energy in/energy out function.
## For source, see documentation/

if (!require(tidyverse)) install.packages('tidyverse')
library("tidyverse")
# library("plotly")
# setwd("~/calorie model 2.0")


# function parameters from external spreadsheet.

hall <- read_rds(here("data", "hall parameters.rds"))

# Gather a list of paramaters for males
males <- hall %>% pull(`Value (males)`) %>% as.list()
names(males) <- pull(hall, `Model Parameter`)


# Gather list of parameters for females
females <- hall %>% pull(`Value (females)`) %>% as.list()
names(females) <- pull(hall, `Model Parameter`)


# Combine the male and female lists into one list
params <- list("males" = males, "females" = females)

# Add a parmater for sex, as a numeric.
params$males$sex <- 1
params$females$sex <- 2



# functions


## effective energy density of fat-free mass
rho_hat <- function(FFM) {

  # These parameters do not depend on sex. Therefore this function does not take a sex argument.
  # However, we need to throw an error if this parameter is different for males and females.
  stopifnot(
    params[[1]]$rho_hat_ffm_m == params[[2]]$rho_hat_ffm_m,
    params[[1]]$rho_hat_ffm_c == params[[2]]$rho_hat_ffm_c
  )

  m <- params[[1]]$rho_hat_ffm_m
  c <- params[[1]]$rho_hat_ffm_c

  return(m * FFM + c)
}

## Energy partition - ratio of FM:FFM that changes in a state of energy inbalance.
part <- function(FM, FFM) {


  # these parameters does not depend on sex. Therefore this function does not take a sex argument.
  # However, we need to throw an error if this parameter is different for males and females.

  stopifnot(
    params[[1]]$rho_fm == params[[2]]$rho_fm
  )

  rho_hat <- rho_hat(FFM)

  C <- C(rho_hat, params[[1]]$rho_fm)


  return(C / (C + FM))
}

## energy partition coefficient
C <- function(rho_hat, rho) {
  return(10.4 * rho_hat / rho)
}

## growth parameter
g <- function(sex, t) {
  p <- params[[sex]]

  value <- p$a * exp(-(t - p$t_a) / (p$tau_a)) +
    p$b * exp(-(t - p$t_b)^2 / (2 * p$tau_b^2)) +
    p$d * exp(-(t - p$t_d)^2 / (2 * p$tau_d^2))


  return(value)
}

## Activity rates
delta <- function(sex, t) {
  p <- params[[sex]]

  p$delta_min + ((p$delta_max - p$delta_min) * p$p^p$h) / (t^p$h + p$p^p$h)
}

## Reference energy intake
eb <- function(sex, t) {
  p <- params[[sex]]

  p$eb_1 * exp(-(t - p$eb_2) / p$eb_3) +
    p$eb_4 * exp(-(t - p$eb_5)^2 / (2 * p$eb_6^2)) +
    p$eb_7 * exp(-(t - p$eb_8)^2 / (2 * p$eb_9^2))
}

# Clean up variable that's no longer needed.
rm(hall)
