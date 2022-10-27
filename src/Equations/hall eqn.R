if (!require(tidyverse)) install.packages('tidyverse')
library("tidyverse")
if (!require(deSolve)) install.packages('deSolve')
library("deSolve") # Numerical approximation of differential equations.
if (!require(sitar)) install.packages('sitar')
library("sitar") # Child growth references.
if (!require(here)) install.packages('here')
library("here")

# Functions necessary to generating this function.
source(here("src/Equations/background functions.R"))

# Processing males, then processing females

# Turning the reference intake table into a reference intake function, using approxfun
male_I_ref <- read_rds(here("data", "FFM reference/male intake reference.rds")) %>%
  select(Age, I_ref)

# Reference intake is "join the dots" between known points. Outside the known range,
# the intake reference is constant.

males$I_ref <- approxfun(x = pull(male_I_ref, Age), y = pull(male_I_ref, I_ref), rule = 2)

female_I_ref <- read_rds(here("data", "FFM reference", "female intake reference.rds")) %>%
  select(Age, I_ref)

females$I_ref <- approxfun(x = pull(female_I_ref, Age), y = pull(female_I_ref, I_ref), rule = 2)

params <- list("males" = males, "females" = females)



## energy burn
e <- function(sex, t, I, FFM, FM) {
  p <- params[[sex]]

  delta <- delta(sex, t)
  delta_I <- I - p$I_ref(t)
  rho_hat <- rho_hat(FFM)
  g <- g(sex, t)
  part <- part(FM, FFM)

  ffm_ratio <- p$eta_ffm / rho_hat
  fm_ratio <- p$eta_fm / p$rho_fm

  # This function is long. To avoid typos, the numerator and denominator were seperately defined.
  numerator <- p$k +
    (p$gamma_ffm + delta) * FFM +
    (p$gamma_fm + delta) * FM +
    p$beta * delta_I +
    (ffm_ratio * part + fm_ratio * (1 - part)) * I +
    g * (ffm_ratio - fm_ratio)



  denominator <- 1 +
    ffm_ratio * part +
    fm_ratio * (1 - part)

  return(numerator / denominator)
}

d_weight <- function(t, state, p) {
  ## Defines change in weight, for a given age, state (current weight), and external parameters p, including intake

  FM <- state[1]
  FFM <- state[2]


  with(as.list(p), {
    i <- I_actual(t)

    g <- g(sex, t)

    E <- e(sex, t, i, FFM, FM)

    part <- part(FM, FFM)

    rho_hat <- rho_hat(FFM)

    dFFM <- (part * (i - E) + g) / rho_hat
    dFM <- ((1 - part) * (i - E) - g) / rho_fm

    return(list(c(dFM * 365, dFFM * 365)))
  })
}


# Given reference male and female fat, fat-free masses at given ages.
reference_male <- read_rds(here("data", "FFM reference", "male intake reference.rds"))
FM <- approxfun(reference_male$Age, reference_male$`Fat (kg)`)
FFM <- approxfun(reference_male$Age, reference_male$`FFBM (kg)`)
reference_male <- list(FM = FM, FFM = FFM)


reference_female <- read_rds(here("data", "FFM reference", "female intake reference.rds"))
FM <- approxfun(reference_female$Age, reference_female$`Fat (kg)`)
FFM <- approxfun(reference_female$Age, reference_female$`FFBM (kg)`)
reference_female <- list(FM = FM, FFM = FFM)

reference <- list(reference_male, reference_female)

params$males$sex <- 1
params$females$sex <- 2

# Wrapper around desolve, to generate a BMI forecast for a given individual.
simulate <- function(I_actual, sex, min_age = 5, max_age = 18, tick = 1 / 12) {
  y <- c(FM = reference[[sex]]$FM(5), FFM = reference[[sex]]$FFM(5))

  # Given the model, age must start at 5, we want the stated minimum age, and maximum age,
  times <- c(5, min_age, max_age, seq(5, min_age, by = tick), seq(min_age, max_age, by = tick)) %>%
    unique() %>%
    sort()

  simulation <- ode(y, times, d_weight, c(params[[sex]], list(I_actual = I_actual))) %>% # call to desolve
    as.data.frame() %>% # Convert matrix to dataframe
    as_tibble()  # Easier to handle as tibble
    
  if(any(simulation$FFM<0) | (nrow(simulation) != length(times))){ # Underfed someone to the point of starvation, return something 
    return(tibble(age=seq(min_age,max_age,by=tick), bmi=0))
  }
  #else
  simulation %>%
    rename(age = time) %>%
    filter(age >= min_age) %>% # We only need ages at least the min age
    mutate(weight = FM + FFM) %>% # Weight is total weight
    mutate(z = LMS2z(pmin(age, 22), weight, sex, "wt", "uk90")) %>% # calculate z-score of weight
    mutate(bmi = LMS2z(pmin(age, 22), z, sex, "bmi", "uk90", FALSE)) %>% # Convert z-score of weight to BMI
    select(-z) # Drop z-score.
}

# reference_table <- read_rds(here("data", "lookup tables", "all intakes.rds")) %>%
#   mutate(percentile = round(percentile, 3))
#
# percentile_max <- max(reference_table$percentile)
# percentile_min <- min(reference_table$percentile)

# starting_parameters <- function(sex, age, bmi) {
#   # a = min(age, 22.5) #assuming that people stop growing at 23.
#
#   # h <- LMS2z(a, 0, sex, "ht", "uk90", FALSE)/100 #LMS returns height in cm, we need meters
#   a <- pmin(age, 23)
#   s <- sex
#
#   p <- LMS2z(
#     a,
#     bmi,
#     sex,
#     "bmi",
#     "uk90"
#   ) %>%
#     pnorm() %>%
#     round(3) %>%
#     min(percentile_max) %>%
#     max(percentile_min)
#
#
#   specific_intake <- reference_table %>%
#     filter(sex == s &
#       percentile == p) %>%
#     select(age, `kcal per day`)
#
#   assertthat::assert_that(nrow(specific_intake) > 0, msg = paste0(
#     "failed on inputs",
#     " sex =", sex,
#     " age =", age,
#     " bmi=", bmi
#   ))
#   return(approxfun(specific_intake$age, specific_intake$`kcal per day`, rule = 2:2))
# }

rm(males, females, female_I_ref, male_I_ref)
