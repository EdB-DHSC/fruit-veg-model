library("tidyverse")
library("furrr")
library("heemod")

library("lubridate")
library("here")

plan(multisession(workers = 4)) # Requirement for the model to use parallel-processing on DHSC laptop.7

economic_with_parameters <- function(keep_liver, keep_liver_disease, keep_pancreas){
  parameters <- read_csv("heemod data/parameters.csv")
  parameters$value[1] <- keep_liver
  parameters$value[2] <- keep_liver_disease
  parameters$value[3] <- keep_pancreas
  
  write_csv(parameters, "heemod data/parameters.csv")
  
  run_model_tabular("heemod data/")
}


bmi_simulation <- function(age_range, treatment_kcal, filename = "treat.csv") {
  # Generates the treatment BMI trajectories.
  #
  # Args:
  #   age_range: vector of ages to run the model on.
  #   treatment_kcal: was supposed to be the effect of the policy in kcal/child/day, but did not work.
  #   filename: name for the file saving the BMI trajectories for the heemod analysis.
  source(here("src", "hall eqn.R")) # Loads the Hall formulas.
  
  hse <- read_csv("data/heemod_master_files/counterfactual.csv") %>%
    filter(initial_age %in% age_range)
  
  ages_below_5 <- c()
  
  if(any(age_range<5)){ # Case when we need additional 5-year olds
    
    ages_below_5 <- age_range[age_range<5]
    
  }
  
  
  
  ## Write the correct demographic file
  
  read_csv("data/heemod_master_files/demographics.csv") %>%
    filter(initial_age %in% age_range) %>%
    write_csv("heemod data/demographics.csv")
  
  hse %>% # Writing just the counterfactual that we want.
    filter(age>=initial_age, age==round(age,0)) %>%
    select(age, initial_age, sex, cut_bmi, bmi) %>%
    write_csv("heemod data/additional params/counterfactual.csv")
  
  treatment_population <- hse %>% filter(initial_age >=5) %>%
    split(list(hse$sex, hse$cut_bmi, hse$initial_age)) # Split our population into a list of tibbles
  
  treatment_group <-
    future_map_dfr(treatment_population, function(individual) {
      # iterating over individual groups within the population, generating BMI tables.
      # 'individual' is a tibble.
      with(individual, { # Allowing us to use the column names of individual as local varaibles.
        
        if (nrow(individual) == 0) {
          return(NULL) # The above split sometimes returns empty groups, in which case, siltently return NULL.
        }
        
        ref_intake <- approxfun(age, I, rule = 2)
        sex <- first(individual$sex)
        
        init_age <- first(initial_age) 
        
        intake <- function(age, ..) { # kcal of policy is hardcoded because we ran out of time.
          ref_intake(age) - treatment_kcal# Change to treatment scenario
        }
        
        intake %>%
          simulate(sex, init_age, max_age = 101, tick = 1/12) %>%
          # mutate(I = intake(age)) %>%
          mutate(initial_age = init_age) %>%
          mutate(sex = first(sex)) %>%
          mutate(cut_bmi = first(cut_bmi))
      })
    }, .progress = T) %>%
    filter(age == round(age,0)) %>%
    select(age, bmi, initial_age, sex, cut_bmi)
  
  #Handling the kids who are born into the model.
  
  five_year_olds <- treatment_group %>%
    filter(initial_age == 5)
  
  additional_five_year_olds <- map_dfr(ages_below_5 , function(age_year_zero) {
    five_year_olds %>%
      mutate(initial_age = age_year_zero) 
  })
  
  if(nrow(additional_five_year_olds) > 0){
    bind_rows(treatment_group, additional_five_year_olds) %>%
      write_csv("heemod data/additional params/treat.csv")
  }
  else{
    write_csv(treatment_group, "heemod data/additional params/treat.csv")
  }
  return(NULL)
}


simulate_and_save <- function(kcal, keep_liver, keep_liver_disease, keep_pancreas){
  
  bmi_simulation(5:15, kcal)
  
  model <- economic_with_parameters(keep_liver, keep_liver_disease, keep_pancreas)
  
  filename <- paste0("models/kids/",as.integer(Sys.time()))
  
  list(model=model, keep_liver=keep_liver,
       keep_liver_disease=keep_liver_disease,
       keep_pancreas=keep_pancreas,
       kcal=kcal) %>%
    write_rds(filename)
  
  message("\n")
  pb$tick()$print()
  message("\n")
}

shuffle <- function(vec){
  sample(vec, length(vec))
}

kcal_vec <- c(2)

logicals <- c(F,T)

parms <- cross_df(list(kcal=kcal_vec, keep_liver=logicals, keep_liver_disease=logicals, keep_pancreas=logicals))

pb <- progress_estimated(nrow(parms))

pwalk(parms, simulate_and_save)
