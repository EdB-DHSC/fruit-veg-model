#### CODE TO ANALYSE THE OUTPUT OF THE CALORIE MODEL
########################################################################
########################################################################

if (!require(tidyverse)) install.packages('tidyverse')
library("tidyverse")
if (!require(here)) install.packages('here')
library("here")
if (!require(heemod)) install.packages('heemod')
library("heemod")
if (!require(svDialogs)) install.packages('svDialogs')
library(svDialogs)
if (!require(rmarkdown)) install.packages('rmarkdown')
library(rmarkdown)
if (!require(readxl)) install.packages('readxl')
library(readxl)
if (!require(knitr)) install.packages('knitr')
library(knitr)
if (!require(kableExtra)) install.packages('kableExtra')
library(kableExtra)

#####################################
#####################################
#### SECTION 1 - MODEL DETAILS ######
#####################################
#####################################

treat <- read_csv('heemod data/additional params/treat.csv')

counterfactual <- read_csv('heemod data/additional params/counterfactual.csv')

# Storing the total population (including those who are born into the model).
population_size <- sum(model$demographics$weights) 

population_size_formatted <- format(round(population_size/1000000,2),big.mark = ',') # ROUND AND FORMAT THE MODEL POPULATION

###################################
###################################
#### SECTION 2 - STATE SUMMARY ####
###################################
###################################

# Plotting the proportions in each state in each scenario, mostly as a sanity check on the model.
state_evolution <- plot(model$demographics$combined_model, type = 'counts', panel = 'by_strategy') + 
  labs(x = "Model Time (years)",
       y = 'Percentage of Model Population',
       title = 'State Evolution in the Fruit and Vegtable Model') +
  scale_y_continuous(labels = function(x) paste(x/10,'%')) +
  theme_bw()

# THE FINAL POPULATION OF THE STATES
state_counts <- get_counts(model$demographics$combined_model)

#FILTER STATE_COUNTS TO GIVE A TABLE SHOWING THE NUMBER OF PREMATURE AND NON-PREMATURE DEATHS
overall_death_state_counts <- state_counts %>%
  filter(state_names == 'Premature_Death' |
         state_names == 'Death') %>%
  mutate(state_names = str_replace(state_names,'^Death$','Non-premature Death'),
         state_names = str_replace(state_names,'^Premature_Death$','Premature Death')) %>% 
  spread(.strategy_names,count) %>%
  arrange(state_names) %>%
  mutate(diff = (new - standard)/1000) %>%  # THE DIFF BETWEEN THE STATES IN THE COUNTERFACTUAL AND TREATMENT MODELS
  group_by(state_names) %>%
  summarise(change = last(diff),
            tot = last(standard)/1000) %>%
  ungroup() %>%
  mutate(p_change = paste(round((change/tot)*100,2),'%'), # FORMATE OUTPUT
         change = format(round(change * population_size,
                               -3),
                         big.mark = ','),
         tot = format(round(tot * population_size,
                            -3),
                      big.mark = ',')
  ) %>%
  rename('Approximate change in state population due to the policy' = change,
         'Approximate percentage change in state population due to the policy' = p_change,
         'Approximate state population with no policy applied' = tot,
         State = state_names)


# FILTER STATE_COUNTS TO GIVE A TABLE SHOWING HOW MANY PEOPLE AVOID GETTING A DISEASE AND HOW MANY QALYS THIS IS WORTH
#=======================================================================

# MORBIDIDY RELATED QALY COUNT - FORMATED
#-------------------------------------------------------
overall_disease_state_counts <- state_counts %>%
  filter(state_names == 'Stomach' |
           state_names == 'CHD' |
           state_names == 'Colorectal' |
           state_names == 'COPD' |
           state_names == 'Diabetes' |
           state_names == 'Lung' |
           state_names == 'Stroke') %>%
  mutate(state_names = str_replace(state_names,'^Stomach$','Stomach Cancer'),
         state_names = str_replace(state_names,'^CHD$','Coranry Heart Disease'),
         state_names = str_replace(state_names,'^Colorectal$','Colorectal Cancer'),
         state_names = str_replace(state_names,'^COPD$','COPD'),
         state_names = str_replace(state_names,'^Diabetes$','Type II Diabetse'),
         state_names = str_replace(state_names,'^Lung$','Lung Cancer'),
         state_names = str_replace(state_names,'^Stroke$','Stroke')) %>% 
  spread(.strategy_names,count) %>%
  arrange(state_names) %>%
  mutate(diff = (new - standard)/1000) %>%  # THE DIFF BETWEEN THE STATES IN THE COUNTERFACTUAL AND TREATMENT MODELS
  group_by(state_names) %>%
  summarise(change = sum(diff) * population_size) %>%
  ungroup() %>%
  mutate(QALY_det = c(0.052, # COLORECTAL
                      0.077, # COPD
                      0.077, # CHD
                      0.052, # LUNG
                      0.052, # STOMACH
                      0.043, # STROKE
                      0.072)) %>% # DIABETIES 
  mutate(nhs_cost = c(1685, # COLORECTAL
                      703, # COPD
                      625, # CHD
                      1838, # LUNG
                      3501, # STOMACH
                      779, # STROKE
                      1800)) %>% # DIABETIES 
  mutate(QALY_change = -change * QALY_det) %>% 
  mutate(nhs_change = change * nhs_cost) %>%
  mutate(change = format(round(change,
                               -2),
                         big.mark = ','),
         QALY_change = format(round(QALY_change,
                                    -2),
                              big.mark = ','),
         nhs_change = paste('$£$',format(round(nhs_change,
                                        -2),
                                 big.mark = ','))) %>%
  select(-nhs_cost,-QALY_det) %>%
  rename('Approximate change in disease population due to the policy (years)' = change,
         'Undiscounted disease morbidity QALY change' = QALY_change,
         'Undiscounted disease treatment cost change' = nhs_change,
         Disease = state_names)

# MORBIDITY RELATED QALY COUNT - UNFORMATTED
#-------------------------------------------------------------
temp <- state_counts %>%
  filter(state_names == 'Stomach' |
           state_names == 'CHD' |
           state_names == 'Colorectal' |
           state_names == 'COPD' |
           state_names == 'Diabetes' |
           state_names == 'Lung' |
           state_names == 'Stroke') %>%
  mutate(state_names = str_replace(state_names,'^Stomach$','Stomach Cancer'),
         state_names = str_replace(state_names,'^CHD$','Coranry Heart Disease'),
         state_names = str_replace(state_names,'^Colorectal$','Colorectal Cancer'),
         state_names = str_replace(state_names,'^COPD$','COPD'),
         state_names = str_replace(state_names,'^Diabetes$','Type II Diabetse'),
         state_names = str_replace(state_names,'^Lung$','Lung Cancer'),
         state_names = str_replace(state_names,'^Stroke$','Stroke')) %>% 
  spread(.strategy_names,count) %>%
  arrange(state_names) %>%
  mutate(diff = (new - standard)/1000) %>%  # THE DIFF BETWEEN THE STATES IN THE COUNTERFACTUAL AND TREATMENT MODELS
  group_by(state_names) %>%
  summarise(change = sum(diff) * population_size) %>%
  ungroup() %>%
  mutate(QALY_det = c(0.052, # COLORECTAL
                      0.077, # COPD
                      0.077, # CHD
                      0.052, # LUNG
                      0.052, # STOMACH
                      0.043, # STROKE
                      0.072)) %>% # DIABETIES 
  mutate(QALY_change = -change * QALY_det) 

morbidity_QALY <- sum(temp$QALY_change) %>% # TOTAL MORBIDIY RELATED QALY COUNT
  round(-3) %>% 
  format(big.mark = ',')

# BINARY FLAG TO INDICATE AN INCREASE OR DECREASE IN PREMATURE DEATHS            
more <- overall_death_state_counts %>% filter(State == 'Premature Death') %>%
  select('Approximate change in state population due to the policy') %>% 
  str_detect('-')

state_diff <- spread(state_counts,.strategy_names,count) %>% 
  mutate(value = (standard - new) * population_size) %>% 
  ggplot(aes(markov_cycle,value)) +
  geom_line(aes(col = state_names)) 

#####################################################
#####################################################
### SECTION 3 - EXTRACT ECONOMIC RESULTS OF MODEL ###
#####################################################
#####################################################

# FUNCTION TO EXTRACT THE TOTAL EFFECTS OF THE MODEL
# INPUT - HEEMOD MODEL
# OUTPUT - CUMULATIVE BENIFIT OF POLICY IN DISCOUNTED ?
#=====================================================================
benefit_of_policy <- function(model) {

  vec <- heemod:::get_effect(model$demographics$combined_model) # Internal heemod function that finds the total ? in both scenarioes.
  
  benefit_per_person <- vec[2] - vec[1] # Differencing the scenarioes to get the effect of the policy, per person.
  
  return(benefit_per_person * sum(model$demographics$weights)) # Scaling up to population.
}

## Printing the benefit in M.
#----------------------------------------
benifit_num <- format(round(benefit_of_policy(model) / 1e9,0),big.mark = ',') #1e9 USED TO SCALE AS BENIFITS GIVEN IN MILLIONS AND POPULATION BASE UNIT IS 1e3

benifit_breakdown <- get_values(model$demographics$combined_model) %>%
  as_tibble() %>%
  filter(str_detect(value_names, "disc")) %>%
  mutate(value_names = str_remove(value_names, "_disc$")) %>%
  group_by(markov_cycle, value_names) %>%
  summarise(value = last(value) - first(value)) %>%
  group_by(value_names) %>%
  mutate(value = (cumsum(value) * population_size) / 1e9) %>%
  ungroup() %>%
  mutate(value_names = forcats::fct_reorder(value_names, value),
         value_names = case_when(value_names == 'earnings' ~ 'Economic Output',
                                 value_names == 'nhs' ~ 'NHS Costs',
                                 value_names == 'qaly' ~ 'QALYs')) %>% 
  filter(markov_cycle == max(markov_cycle)) %>% 
  mutate(value = format(round(value,0),big.mark = ',')) %>% 
  select(-markov_cycle) %>% 
  rename('Component' = value_names,
         'NPV (m)' = value)

## CALCULATE QALY COUNT
#------------------------------------------------------------------
qaly_undis <- get_values(model$demographics$combined_model) %>%
  as_tibble() %>%
  filter(value_names=='qaly') %>%
  select(-value_names) %>%
  group_by(markov_cycle) %>%
  summarise(value = last(value) - first(value)) %>%
  mutate(value = (cumsum(value) * population_size) / 1e3) %>%
  ungroup() %>%
  filter(markov_cycle == max(markov_cycle)) 

qaly_count_undis <- format(round(as.numeric(qaly_undis[1,2]),-3),big.mark = ',') # EXTRACT QALY COUNT

qaly <- get_values(model$demographics$combined_model) %>%
  as_tibble() %>%
  filter(value_names=='qaly_disc') %>%
  select(-value_names) %>%
  group_by(markov_cycle) %>%
  summarise(value = last(value) - first(value)) %>%
  mutate(value = (cumsum(value) * population_size) / 1e3,
         value = value / 60000) %>%
  ungroup() %>%
  filter(markov_cycle == max(markov_cycle)) 

qaly_count <- format(round(as.numeric(qaly[1,2]),-3),big.mark = ',') # EXTRACT QALY COUNT

# Graphing cumulative modelled benefits by component.
#==============================================================================
benifit1 <- get_values(model$demographics$combined_model) %>%
  as_tibble() %>%
  filter(str_detect(value_names, "disc")) %>%
  mutate(value_names = str_remove(value_names, "_disc$")) %>%
  group_by(markov_cycle, value_names) %>%
  summarise(value = last(value) - first(value)) %>%
  group_by(value_names) %>%
  mutate(value = (cumsum(value) * population_size) / 1e9) %>%
  ungroup() %>%
  mutate(value_names = forcats::fct_reorder(value_names, value),
         value_names = case_when(value_names == 'earnings' ~ 'Economic Output',
                                 value_names == 'nhs' ~ 'NHS Costs',
                                 value_names == 'qaly' ~ 'QALYs')) %>%
  ggplot(aes(x = markov_cycle, y = value, fill = value_names)) + 
  geom_area() + 
  theme_bw() + 
  labs(title = 'Cumulative Policy Benefits: Total NPV',
       x = "Model Time (years)",
       y = 'Cumulative Policy Benifits (m)',
       fill = 'Economic Effects') +
  scale_y_continuous(labels = function(x) format(x,big.mark = ',')) 

benifitf2 <- get_values(model$demographics$combined_model) %>%
  as_tibble() %>%
  filter(str_detect(value_names, "disc")) %>%
  mutate(value_names = str_remove(value_names, "_disc$")) %>%
  group_by(markov_cycle, value_names) %>%
  summarise(value = last(value) - first(value)) %>%
  group_by(value_names) %>%
  mutate(value = (cumsum(value) * population_size) / 1e9) %>%
  ungroup() %>%
  mutate(value_names = forcats::fct_reorder(value_names, value),
         value_names = case_when(value_names == 'earnings' ~ 'Economic Output',
                                 value_names == 'nhs' ~ 'NHS Costs',
                                 value_names == 'qaly' ~ 'QALYs')) %>%
  ggplot(aes(x = markov_cycle, y = value, fill = value_names)) + 
  geom_area() + 
  facet_wrap(~value_names, scales = 'free') +
  theme_bw() + 
  labs(title = 'Cumulative Policy Benefits: Individual Componants',
       x = "Model Time (years)",
       y = 'Cumulative Policy Benifits (m)',
       fill = 'Economic Effects') +
  theme(legend.position = 'none') +
  scale_y_continuous(labels = function(x) format(x,big.mark = ',')) 


benifit_table <- get_values(model$demographics$combined_model) %>%
  as_tibble() %>%
  filter(str_detect(value_names, "disc")) %>%
  mutate(value_names = str_remove(value_names, "_disc$")) %>%
  group_by(markov_cycle, value_names) %>%
  summarise(value = last(value) - first(value)) %>%
  group_by(value_names) %>%
  mutate(value = format(round((cumsum(value) * population_size) / 1e9,1), big.mark = ',')) %>%
  ungroup() %>% 
  spread(value_names,value) %>% 
  mutate(markov_cycle = round(markov_cycle,0)) %>% 
  rename('Model Year' = markov_cycle,
         'Economic Output' = earnings,
         'NHS Costs' = nhs,
         'QALYs' = qaly)

#################################################
#################################################
######## SECTION 4 - BMI TRAJECTARY #############
#################################################
#################################################

treat <- treat %>% 
  rename(treat=fv_consumption)

min_age <- if_else(min(treat$initial_age)<5,5,min(treat$initial_age))

counterfactual <- counterfactual %>% 
  rename(counterfactual=fv_consumption)

fv_trajectories <- left_join(treat,
                              counterfactual,
                              by = c("age", "initial_age", "sex", "fv_cat")) %>% 
  select(age,sex,initial_age,fv_cat,treat,counterfactual) %>% 
  mutate(fv_cat = case_when(fv_cat == 1 ~ 'low consumption',
                             fv_cat == 2 ~ 'average consumption',
                             fv_cat == 3 ~ 'high consumption')) %>% 
  mutate(fv_cat = factor(fv_cat,levels = c('low consumption','average consumption','high consumption'))) %>% 
  pivot_longer(cols = treat:counterfactual) %>% 
  filter(initial_age==min_age,
         sex==1) %>% 
  ggplot(aes(age,value,col=name)) +
  geom_line() +
  facet_wrap(~fv_cat,scales = 'free') +
  labs(title = 'F&V Consumption',
       subtitle = 'Males',
       y = 'BMI')

#################################################################
#################################################################
########  SECTION 4 - WRITE REPORT ##############################
#################################################################
#################################################################

if(!dir.exists('report')) dir.create('report') # CREATE REPORT FOLDER

  render( 
    input = "src/model analysis/model analysis.Rmd",
    output_file = paste0(model$name,' - ',Sys.Date(),'.html'),  
    output_dir = 'report'
    )
