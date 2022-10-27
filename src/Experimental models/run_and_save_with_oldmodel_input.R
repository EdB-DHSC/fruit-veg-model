library("tidyverse")
library("furrr")
library("heemod")

plan(multisession(workers = 4)) # Requirement for the model to use parallel-processing on DHSC laptop.7


bmi_simulation <- function(age_range, input_kcal, filename = "treat.csv") {
  # Generates the treatment BMI trajectories.
  #
  # Args:
  #   age_range: vector of ages to run the model on.
  #   treatment_kcal: was supposed to be the effect of the policy in kcal/child/day, but did not work.
  #   filename: name for the file saving the BMI trajectories for the heemod analysis.
  source("src/Equations/hall eqn.R") # Loads the Hall formulas.
  
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
        
        age_category <- case_when(
          init_age < 11 ~ 1,
          init_age < 19 ~ 2,
          init_age < 65 ~ 3,
          TRUE          ~ 4
        )
        
        treatment_kcal <- input_kcal %>% 
          filter(sex==Sex, age_cat==age_category) %>%
          pull(kcal)
        
        intake <- function(age, ..) { 
          ref_intake(age) - if_else(age > init_age,treatment_kcal, 0)
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


simulate_and_save <- function(input_file, folder_name){
  
  system_time <- str_sub(Sys.time(),1,-4)
  
  name <- input_file %>%
    str_remove("inputs/") %>%
    str_replace_all("/", "_") %>% #So all models come out in the same folder.
    str_remove(".csv") 
  
  kcal <- read_csv(input_file)
  
  

  
  bmi_simulation(-21:100, kcal)
  
  model <- run_model_tabular("heemod data/")
  
  model$nhs_multiplier <- "no"
  
  population <- sum(model$demographics$weights)
  
  model$effect <- diff(model$demographics$combined_model$run_model$.effect) *
    population / 1000
  
  model$summary <- model$demographics$combined_model$run_model %>%
    select_at(vars(ends_with("_disc"))) %>%
    summarise_all(~(diff(.x) * population / 1000))
    
  model$kcal <- kcal
  
  model$name <- name
  
  output_dir <- here::here("models", folder_name)
  
  filename <- str_glue("{output_dir}/{lubridate::today()} {name}.rds")

  if(!dir.exists(output_dir)) dir.create(output_dir)
  
  write_rds(model, filename)
  
  source('src/Model analysis/model analysis.R', local=T)
  
  pb$tick()$print()

}

input_files <- list.files("inputs", full.names = T, recursive = T)


pb <- progress_estimated(length(input_files))

walk(input_files, ~simulate_and_save(.x, "Promos/"))