library(tidyverse)
library(readr)
library(sitar)

#####################
#####################
### SEC 1 - READ-IN ###
#####################
#####################

HSE <- read_csv("data/HSE_OFFICIAL_SENSITIVE/everyone_fv.csv") %>% 
  rename(age = Age,
         sex = Sex)

pop <- readRDS("data/ONS/enpppopendata2018.rds")

#demographics_old <- read_csv("heemod data/demographics-old.csv")

#####################
#####################
### SEC 2 - POP #####
#####################
#####################

demographics_new <- HSE %>% 
                  mutate(fv_cat = case_when(porfv <= 2 ~ 1,
                                            porfv > 2 & porfv < 5 ~ 2,
                                            porfv >= 5 ~3)) %>% 
  group_by(age,sex,fv_cat) %>% 
  summarise(pop_proportion = sum(wt_int)) %>% 
  ungroup() %>% 
  group_by(age,sex) %>% 
  mutate(tot_wt = sum(pop_proportion)) %>% 
  ungroup() %>% 
  mutate(pop_proportion2 = pop_proportion/tot_wt) %>% 
  left_join(pop %>% filter(year == 2019), # JOIN ONS POP PROJECTIONS
            by = c('age','sex')) %>% 
  mutate(pop = pop_proportion2 * population) %>% 
  select(age,sex,fv_cat,pop,pop_proportion2) %>% 
  filter(age < 91,
         age > 4)

### ADD PEOPLE WHO WILL BE 'BORN INTO' THE MODEL AS 5 YEAR OLDS
#################################################################

five_year_olds <- demographics_new %>% # Splitting off today's 5-yr olds to generate the future cohorts.
  filter(age == 5)

age_year_zero <- seq(4,-5) # SET AGE RANGE FOR PEOPLE TO BE BORN IN

additional_five_year_olds <- five_year_olds %>%
    select(-pop) %>% # Unnecessary columns
    left_join(pop %>% filter(year == (2019 + (5 - age_year_zero[1]))), # Joining with the appropriate year of population projection
              by = c("age", "sex")
    ) %>%
    mutate(age = age_year_zero[1]) %>% # Correcting inital_age
    mutate(pop = pop_proportion2 * population) # Rescaling back to population in the appropriate year.

for (x in 2:length(age_year_zero)) {
  additional_five_year_olds2 <- five_year_olds %>%
    select(-pop) %>% # Unnecessary columns
    left_join(pop %>% filter(year == (2019 + (5 - age_year_zero[x]))), # Joining with the appropriate year of population projection
              by = c("age", "sex")
    ) %>%
    mutate(age = age_year_zero[x]) %>% # Correcting inital_age
    mutate(pop = pop_proportion2 * population) # Rescaling back to population in the appropriate year.
  
  additional_five_year_olds <- bind_rows(additional_five_year_olds,
                                         additional_five_year_olds2)
                                      
}

# ADD ADDITIONAL 5 YEAR OLDS TO DEMOGRAPHIC FILE
#---------------------------------------------------------

demographics_new <- demographics_new %>% 
  select(age,sex,fv_cat,pop) %>% 
  bind_rows(additional_five_year_olds %>% 
              select(age,sex,fv_cat,pop))

#####################
#####################
### SEC 3 - TEST ###
#####################
#####################

demographics_new %>%
  rename(initial_age = age,
         '.weight' = pop) %>%
write_csv('demographics.csv')

#####################
#####################
### SEC 4 - WRITE OUT 
#####################
#####################

