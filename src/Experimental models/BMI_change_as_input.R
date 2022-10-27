# CALORIE MODEL VERSION 3 MODIFIED FOR DIRECT BMI CHANGE
# TO USE:
# - SET PARAMETERS IN SECTION 1 (LINES 17,19,21)
# - SET PARAMETERS IN SECTION 2 (LINES 48,54,59)

if (!require(tidyverse)) install.packages('tidyverse')
library("tidyverse")
if (!require(heemod)) install.packages('heemod')
library("heemod")

#######################################
#######################################
# section 1 - Set up input parameters #
#######################################
#######################################

model_name <- "bmi-test-deleteme"

nhs_multiplier <- "yes" #Are NHS savings valued as QALYs, then back into £?

eval_period <- 100 # over what period are benifits evaluated

population_age <- c(15,100) # Lower and upper ages to include.

# CHAAANGE PARAMETERS IN MODEL INPUT FILES
#--------------------------------------------------------------

options <- read_csv("heemod data/options.csv")

options$value[3] <- eval_period # CHANGE EVALUATION PERIOD IN INPUT FILE

write_csv(options, "heemod data/options.csv")

parameters <- read_csv("heemod data/parameters.csv")

parameters$value[1] <- ifelse(nhs_multiplier == 'yes',4,1) # CHANGE NHS MULTIPLIER IN INPUT FILE

write_csv(parameters, "heemod data/parameters.csv")

system_time <- str_sub(Sys.time(),1,-4)

## Store average heights

height <- read_rds("data/HSE_OFFICIAL_SENSITIVE/everyone.rds") %>%
  filter(age>=15) %>%
  group_by(sex) %>%
  summarise(height=mean(height)/100) %>%
  mutate(sex=recode(sex, "1"="male", "2"="female")) %>%
  pivot_wider(names_from = sex,values_from = height) 

######################################
######################################
# section 2 - Set up the heemod data #
######################################
######################################

read_csv("data/heemod_master_files/counterfactual.csv") %>%
  select(age, initial_age, sex, cut_bmi, bmi) %>%
  filter(between(initial_age, population_age[1], population_age[2]),age==round(age,0)) %>% 
  #filter(between(cut_bmi,1,4)) %>% #Additional filter on bmi category.
  write_csv("heemod data/additional params/counterfactual.csv")

read_csv("heemod data/additional params/counterfactual.csv") %>%
  filter(between(initial_age, population_age[1], population_age[2]),age==round(age,0)) %>% 
  mutate(markov_cycle = age-initial_age) %>%
  mutate(height = if_else(sex==1, height$male, height$female)) %>% 
  mutate(bmi = if_else(markov_cycle==1, bmi-1.3/height^2, bmi)) %>% #change this line according to treat bmi
  select(-markov_cycle) %>%
  select(-height) %>% 
  write_csv("heemod data/additional params/treat.csv")

read_csv("data/heemod_master_files/demographics.csv") %>%  
  filter(between(initial_age, population_age[1], population_age[2])) %>% 
  write_csv("heemod data/demographics.csv")

######################################
######################################
### section 3 - run calorie model ####
######################################
######################################

model <- run_model_tabular("heemod data/") # RUN MODEL

# ADD SETUP PARAMETERS TO THE MODEL FILE
#-------------------------------------------------------------------
model$kcal <- tibble(Sex="ignore",age_cat="ignore",kcal="ignore") 

model$name <- model_name

model$nhs_multiplier <- nhs_multiplier

#############################################  
#############################################
## SECTION 4 - SAVE MODEL AND WRITE REPORT ##
#############################################
#############################################


if(!dir.exists('model')) dir.create('model')

write_rds(model,paste0('model/',model$name,' - ',Sys.Date(),'.rds')) # SAVE MODEL

source('src/Model analysis/model analysis.R') # RUN MODEL ANALYSIS FILED
