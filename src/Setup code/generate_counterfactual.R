library(tidyverse)
library(readr)
library(furrr)

#####################
#####################
### SEC 1 - READ-IN ###
#####################
#####################

HSE <- read_csv("data/HSE_OFFICIAL_SENSITIVE/everyone_fv.csv") %>% 
  rename(age = Age,
         sex = Sex)

#####################
#####################
### SEC 2 - POP #####
#####################
#####################

counterfactual <- HSE %>% 
                  mutate(fv_cat = case_when(porfv <= 2 ~ 1,
                                            porfv > 2 & porfv < 5 ~ 2,
                                            porfv >= 5 ~3)) %>% 
  group_by(age,sex,fv_cat) %>% 
  summarise(fv_consumption = mean(porfv)) %>% 
  ungroup() %>% 
  filter(age >= 5,
         age <= 90)

# add fv_consumption for all ages
#==============================

counterfactual <- future_map_dfr(rep(5:90,1), function(a) {
  counterfactual %>%
    mutate(initial_age = a)
}
)

## add under 5s
#==============================

ages_below_5 <- rep(-100:5,1)

five_year_olds <- counterfactual %>%
  filter(initial_age == 5) # EXTRACT THE TREATMENT GROUP WITH INITIAL AGE OF 5

# MAP THE TREATMENT TRAGECTORY OF PEOPLE WITH INITIAL AGE 5 ONTO ALL MODEL COHORTS WITH AN INITIAL AGE LESS THAN 5
additional_five_year_olds <- map_dfr(ages_below_5 , function(age_year_zero) {
  five_year_olds %>%
    mutate(initial_age = age_year_zero) 
})

counterfactual <- bind_rows(counterfactual, additional_five_year_olds)


#####################
#####################
### SEC 3 - TEST ###
#####################
#####################

counterfactual %>% filter(initial_age == 50) %>% 
  ggplot(aes(age,fv_consumption,col = as.factor(fv_cat))) +
  geom_line() +
  facet_wrap(~sex)

#####################
#####################
### SEC 4 - WRITE OUT 
#####################
#####################

write_csv(counterfactual %>% 
            unique(),
          'counterfactual.csv')
