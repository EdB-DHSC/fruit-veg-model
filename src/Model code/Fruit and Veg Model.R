if (!require(tidyverse)) install.packages('tidyverse')
library("tidyverse")
if (!require(furrr)) install.packages('furrr')
library("furrr")
if (!require(heemod)) install.packages('heemod')
library("heemod")
if (!require(svDialogs)) install.packages('svDialogs')
library('svDialogs')

###################################################
###################################################
## SECTION 1 - FRONT END TO SET MODEL PARAMETERS ##
###################################################
###################################################

system_time <- str_sub(Sys.time(),1,-4)

# FUNCTION TO VALIDATE NUMERIC INPUTS
# ARGS:
# name = the name of the variable under consideration
# range_min = the min standard value
# range_max = the max standard value
# input_message = the standard question to ask the user
# num = the value entered by the user
# unit = units of the value entered by the user
# variable = the name of the variable output from the function
numeric_readin_check <- function(name,range_min,range_max,input_message,num,unit,variable){
  while(is.na(num)){ # IF NON NUMERIC ASK USER TO RE-ENTER VALUE, THIS REPEATES UNTILL A VALID NUMBER IS ENTERED
    dlgMessage(paste('Non-numeric', 
                     name,
                     '!! Please re-enter as number in next box'))$res
    num <- dlgInput(input_message,Sys.info()[""])$res %>% # POP UP BOX FOR THE NUMERIC VALUE
      as.numeric()
  }
  
  if(num < range_min | num > range_max){ # IF THE NUMBER IS OUTSIDE THE EXPECTED RANGE, ASK FOR CONFERMATION
    confirm <- dlgMessage(paste('The entered',
                                name,
                                'of',
                                num,
                                unit,
                                'is outside the standard range of',
                                range_min,
                                '-',
                                range_max,
                                unit,
                                '\n',
                                '\nPress "OK" to confirm or "Cancel" to re-enter'),
                          'okcancel')$res
    if(confirm == 'cancel'){
      num <- dlgInput(input_message,Sys.info()[""])$res %>% # IF VALUE IS WRONG, POP UP BOX TO RE-ENTER
        as.numeric()
      while(is.na(num)){ # IF NON NUMERIC ASK USER TO RE-ENTER VALUE, THIS REPEATES UNTILL A VALID NUMBER IS ENTERED
        dlgMessage(paste('Non-numeric',
                         name,
                         '!! Please re-enter in next box'))$res
        num <- dlgInput(input_message,Sys.info()[""])$res %>% # POP UP BOX FOR THE NUMERIC VALUE
          as.numeric()
      }
    }
  }
  assign(variable,num,.GlobalEnv)
}

welcome <- dlgMessage(paste('          Welcome to the Fruit & Vegetable Model',
                            '\n',
                            '\nA series of pop up boxes will allow you to set up your model simulation before running the model.'),"yesno")$res # ARE ADULTS AFFECTED BY THE POLICY
if(welcome == 'no'){
  dlgMessage("Fruit & Vegetable Model started in error so quitting.")$res # REMIND USER TO MAKE FOLDER COPY 
  quit(save = 'no') # CLOSE R IF USER HAS NOT COPIED THE WORKING FOLDER
}

folder <- dlgMessage("Before setting up your model please confirm that you have copied and renamed the working folder", "yesno")$res # ARE ADULTS AFFECTED BY THE POLICY

if(folder == 'no'){
  dlgMessage("Fruit & Vegetable Model started in error so quitting, copy and rename the working folder and try again")$res # REMIND USER TO MAKE FOLDER COPY 
  quit(save = 'no') # CLOSE R IF USER HAS NOT COPIED THE WORKING FOLDER
}

### ?IS NEXT QUESTION NEEDED - LEFT IN FOR FUTURE DEVELOPMENT

adults <- dlgMessage("Are adults included in the model cohort?", "yesno")$res # ARE ADULTS AFFECTED BY THE POLICY

policy_lifetime <- dlgInput("the lifetime of the policy being modelled/ length of time the policy is active and effects new model cohorts (in years):",
                            Sys.info()[""])$res %>% # POP UP BOX FOR THE LIFETIME OF THE POLICY MODELED
  as.numeric()

# CHECK POLICY LIFETIME
numeric_readin_check(name = 'Policy Lifetime',
                     range_min = 5,
                     range_max = 25,
                     input_message = "the lifetime of the policy being modelled/ length of time the policy is active and effects new model cohorts (in years):",
                     num = policy_lifetime,
                     unit = 'years',
                     variable = 'policy_lifetime')

age_min_initial <- 6 - policy_lifetime # CALCULATE MINIMUM AGE IN THE MODEL (6 IS USED, NOT 5, TO ADJUST FOR THE PRESENCE OF ZERO YEAR OLDS IN THE MODEL COHORT)

age_max <- ifelse(adults == 'yes',100,18) # CALCULATE MAXIMUM AGE IN THE MODEL

age_range <- c(age_min_initial:age_max) # CALCULATE AGE RANGE IN THE POLICY

uni_treatment_fnv <- dlgMessage('is there a uniform (all age groups, male and female) daily fruit and vegetable consumption increase for all people included in the policy', 'yesno')$res # POP UP BOX FOR THE fruit and vegetable increase OF THE POLICY MODELED

# INITIALISE fruit and vegetable increase DATA FRAME
fnv <- data.frame(Sex = c(1,1,1,1,2,2,2,2), # MALES AND FEMALES
                  age_cat = c(1,2,3,4,1,2,3,4), # 4 AGE CATEGORIES
                  fnv = rep(0,8)) # FV INCREASES ARE SET TO 0 INITIALY

if (uni_treatment_fnv == 'yes'){ # IF ALL INDIVIDUALS ARE SUBJECT TO THE SAME F&V INCREASE
  treatment_fnv <- dlgInput("uniform (all age groups, male and female) daily fruit and vegetable consumption increase for all people included in the policy (portions - 80g):",
                             Sys.info()[" "])$res %>% # POP UP BOX FOR THE F&V INCREASE OF THE POLICY MODELED
    as.numeric()
  # CHECK UNIFORM CAL REDUCTION
  numeric_readin_check(name = 'uniform (all age groups, male and female) daily fruit and vegetable increase',
                       range_min = 0,
                       range_max = 5,
                       input_message = "uniform (all age groups, male and female) fruit and vegetable consumption increase for all people included in the policy (portions - 80g):",
                       num = treatment_fnv,
                       unit = 'portions',
                       variable = 'treatment_fnv')
  
  if(adults == 'no'){ # IF UNIFORM F&V INCREASE AND ADULTS NOT INCLUDED
    fnv$fnv[1] <- treatment_fnv # MAKE ALL CHILD F&V INCREASES THE SAME
    fnv$fnv[2] <- treatment_fnv
    fnv$fnv[5] <- treatment_fnv
    fnv$fnv[6] <- treatment_fnv
  }
  if(adults == 'yes'){ # IF UNIFORM F&V REDUCTION AND ADULTS INCLUDED
    fnv$fnv <- treatment_fnv # MAKE ALL F&V INCREASES THE SAME
  }
  
} else {
  
  treatment_fnv <- dlgInput("fruit and vegetable increase for 5 - 10 year old, males (portions - 80g):",
                             Sys.info()[" "])$res %>% # POP UP BOX FOR THE F&V INCREASE REDUCTION OF THE POLICY MODELED
    as.numeric()
  
  numeric_readin_check(name = 'fruit and vegetable increase for 5 - 10 year old males',
                       range_min = 0,
                       range_max = 5,
                       input_message = "fruit and vegetable increase for 5 - 10 year old, males (portions - 80g):",
                       num = treatment_fnv,
                       unit = 'portions',
                       variable = 'treatment_fnv')
  
  fnv$fnv[1] <- treatment_fnv
  
  treatment_fnv <- dlgInput("fruit and vegetable increase for 5 - 10 year old, females (portions - 80g):",
                             Sys.info()[" "])$res %>% # POP UP BOX FOR THE F&V INCREASE OF THE POLICY MODELED
    as.numeric()
  
  numeric_readin_check(name = 'fruit and vegetable increase for 5 - 10 year old females',
                       range_min = 0,
                       range_max = 5,
                       input_message = "fruit and vegetable increase for 5 - 10 year old, females (portions - 80g):",
                       num = treatment_fnv,
                       unit = 'portions',
                       variable = 'treatment_fnv')
  
  fnv$fnv[5] <- treatment_fnv
  
  treatment_fnv <- dlgInput("fruit and vegetable increase for 11 - 18 year old, males (portions - 80g):",
                             Sys.info()[" "])$res %>% # POP UP BOX FOR THE F&V INCREASE OF THE POLICY MODELED
    as.numeric()
  
  numeric_readin_check(name = 'fruit and vegetable increase for 11 - 18 year old males',
                       range_min = 0,
                       range_max = 5,
                       input_message = "fruit and vegetable increase for 11 - 18 year old, males (portions - 80g):",
                       num = treatment_fnv,
                       unit = 'portions',
                       variable = 'treatment_fnv')
  
  fnv$fnv[2] <- treatment_fnv
  
  treatment_fnv <- dlgInput("fruit and vegetable increase for 11 - 18 year old, females (portions - 80g):",
                             Sys.info()[" "])$res %>% # POP UP BOX FOR THE F&V INCREASE OF THE POLICY MODELED
    as.numeric()
  
  numeric_readin_check(name = 'fruit and vegetable increase for 11 - 18 year old females',
                       range_min = 0,
                       range_max = 5,
                       input_message = "fruit and vegetable increase for 11 - 18 year old, females (portions - 80g):",
                       num = treatment_fnv,
                       unit = 'portions',
                       variable = 'treatment_fnv')
  
  fnv$fnv[6] <- treatment_fnv
  
  if (adults == 'yes'){
    treatment_fnv <- dlgInput("fruit and vegetable increase for 19 - 64 year old, males (portions - 80g):",
                               Sys.info()[" "])$res %>% # POP UP BOX FOR THE F&V INCREASE OF THE POLICY MODELED
      as.numeric()
    
    numeric_readin_check(name = 'daily fruit and vegetable increase for 19 - 64 year old males',
                         range_min = 0,
                         range_max = 5,
                         input_message = "fruit and vegetable increase for 19 - 64 year old, males (portions - 80g):",
                         num = treatment_fnv,
                         unit = 'portions',
                         variable = 'treatment_fnv')
    
    fnv$fnv[3] <- treatment_fnv
    
    treatment_fnv <- dlgInput("fruit and vegetable increase for 19 - 64 year old, females (portions - 80g):",
                               Sys.info()[" "])$res %>% # POP UP BOX FOR THE F&V INCREASE OF THE POLICY MODELED
      as.numeric()
    
    numeric_readin_check(name = 'daily fruit and vegetable increase for 19 - 64 year old females',
                         range_min = 0,
                         range_max = 5,
                         input_message = "fruit and vegetable increase for 19 - 64 year old, females (portions - 80g):",
                         num = treatment_fnv,
                         unit = 'portions',
                         variable = 'treatment_fnv')
    
    fnv$fnv[7] <- treatment_fnv
    
    treatment_fnv <- dlgInput("fruit and vegetable increase for 65+ year old, males (portions - 80g):",
                               Sys.info()[" "])$res %>% # POP UP BOX FOR THE F&V INCREASE OF THE POLICY MODELED
      as.numeric()
    
    numeric_readin_check(name = 'daily fruit and vegetable increase for 65+ year old males',
                         range_min = 0,
                         range_max = 5,
                         input_message = "fruit and vegetable increase for 65+ year old, males (portions - 80g):",
                         num = treatment_fnv,
                         unit = 'portions',
                         variable = 'treatment_fnv')
    
    fnv$fnv[4] <- treatment_fnv
    
    treatment_fnv <- dlgInput("fruit and vegetable increase for 65+ year old, females (portions - 80g):",
                               Sys.info()[" "])$res %>% # POP UP BOX FOR THE F&V INCREASE OF THE POLICY MODELED
      as.numeric()
    
    numeric_readin_check(name = 'daily fruit and vegetable increase for 65+ year old females',
                         range_min = 0,
                         range_max = 5,
                         input_message = "fruit and vegetable increase for 65+ year old, females (portions - 80g):",
                         num = treatment_fnv,
                         unit = 'portions',
                         variable = 'treatment_fnv')
    
    fnv$fnv[8] <- treatment_fnv
  }
}

eval_period <- dlgInput("over what period should the policy be evaluated (years):",
                        Sys.info()[" "])$res %>% # POP UP BOX FOR THE EVALUATION PERIOD OF THE POLICY MODELED
  as.numeric()

numeric_readin_check(name = 'policy evaluation period',
                     range_min = 10,
                     range_max = 100,
                     input_message = "over what period should the policy be evaluated (years):",
                     num = eval_period,
                     unit = 'years',
                     variable = 'eval_period')

nhs_multiplier <- dlg_message('should the 4.7 time multiplier for NHS costs be used?','yesno')$res # POP UP BOX FOR THE USE OF MULTIPLIERPOLICY MODELED

model_name <- dlgInput("Enter model name: 
                       \n(the model and report will be saved as this with a time-stamp added)",
                       Sys.info()[" "])$res # POP UP BOX FOR THE NAME OF THE MODEL RUN

if(model_name == 'NA'){
  dlgMessage('no model name given so a default name (unnamed_model) will be used')
  model_name <- 'unnamed_model'
}

# CONFIRM INPPUTS
#########################################################################

confirm <- 'no' # SET CONFIRM TO 'NO' AS INITIAL VALUE

while(confirm == 'no'){ # ENTER CONDITIONAL CONFFIRM LOOP THAT ASKS FOR CONFIRMATION UNTILL IT IS GIVEN
  
  if(nhs_multiplier == 'yes'){  # PRINT CONFIRM SCREEN SHOWING INPUTS AS CURRENTLY EXIST
    if(adults == 'yes'){
      confirm <- dlg_message(paste('You have selected a policy lifetime of:',policy_lifetime,'years',
                                   '\n',
                                   '\nYou have selected a policy evaluation period of:',eval_period,'years',
                                   '\n',
                                   '\nAdults are included in the fruit & veg consumption policy',
                                   '\n',
                                   '\nNHS costs will be multiplied by 4.7',
                                   '\n',
                                   '\nThe per day fruit and vegetable increase:',
                                   '\n            Males      Females',
                                   '\n5 - 10      ',fnv$fnv[1],'        ',fnv$fnv[5],
                                   '\n11 - 18    ',fnv$fnv[2],'        ',fnv$fnv[6],
                                   '\n19 - 64    ',fnv$fnv[3],'        ',fnv$fnv[7], 
                                   '\n65+         ',fnv$fnv[4],'        ',fnv$fnv[8],
                                   '\n',
                                   '\nYour model will be saved as:',model_name,'-',system_time,'.rds',
                                   '\n',
                                   '\nYour model summary report will be saved as:',model_name,'-',system_time,'.html',
                                   '\n',
                                   '\nPlease confirm this is correct'),'yesno')$res # POP UP BOX FOR CONFERMATION OF MODEL PARAMETERS
    } else {
      confirm <- dlg_message(paste('You have selected a policy lifetime of:',policy_lifetime,'years',
                                   '\n',
                                   '\nYou have selected a policy evaluation period of:',eval_period,'years',
                                   '\n',
                                   '\nAdults are not included in the fruit and vegetable increase policy',
                                   '\n',
                                   '\nNHS costs will be multiplied by 4.7',
                                   '\n',
                                   '\nThe per day fruit and vegetable increase:',
                                   '\n            Males      Females',
                                   '\n5 - 10      ',fnv$fnv[1],'        ',fnv$fnv[5],
                                   '\n11 - 18    ',fnv$fnv[2],'        ',fnv$fnv[6],
                                   '\n19 - 64    ',fnv$fnv[3],'        ',fnv$fnv[7], 
                                   '\n65+         ',fnv$fnv[4],'        ',fnv$fnv[8],
                                   '\n',
                                   '\nYour model will be saved as:',model_name,'-',system_time,'.rds',
                                   '\n',
                                   '\nYour model summary report will be saved as:',model_name,'-',system_time,'.html',
                                   '\n',
                                   '\nPlease confirm this is correct'),'yesno')$res # POP UP BOX FOR CONFERMATION OF MODEL PARAMETERS
    }
  } else {
    if(adults == 'yes'){
      confirm <- dlg_message(paste('You have selected a policy lifetime of:',policy_lifetime,'years',
                                   '\n',
                                   '\nYou have selected a policy evaluation period of:',eval_period,'years',
                                   '\n',
                                   '\nAdults are included in the fruit and vegetable increase policy',
                                   '\n',
                                   '\nNHS costs will not be multiplied by 4.7',
                                   '\n',
                                   '\nThe per day fruit and vegetable increase:',
                                   '\n            Males      Females',
                                   '\n5 - 10      ',fnv$fnv[1],'        ',fnv$fnv[5],
                                   '\n11 - 18    ',fnv$fnv[2],'        ',fnv$fnv[6],
                                   '\n19 - 64    ',fnv$fnv[3],'        ',fnv$fnv[7], 
                                   '\n65+         ',fnv$fnv[4],'        ',fnv$fnv[8],
                                   '\n',
                                   '\nYour model will be saved as:',model_name,'-',system_time,'.rds',
                                   '\n',
                                   '\nYour model summary report will be saved as:',model_name,'-',system_time,'.html',
                                   '\n',
                                   '\nPlease confirm this is correct'),'yesno')$res # POP UP BOX FOR CONFERMATION OF MODEL PARAMETERS
    } else {
      confirm <- dlg_message(paste('You have selected a policy lifetime of:',policy_lifetime,'years',
                                   '\n',
                                   '\nYou have selected a policy evaluation period of:',eval_period,'years',
                                   '\n',
                                   '\nAdults are not included in the fruit and vegetable increase policy',
                                   '\n',
                                   '\nNHS costs not will be multiplied by 4.7',
                                   '\n',
                                   '\nThe per day fruit and vegetable increase:',
                                   '\nAge Category    Males      Females',
                                   '\n5 - 10      ',fnv$fnv[1],'        ',fnv$fnv[5],
                                   '\n11 - 18    ',fnv$fnv[2],'        ',fnv$fnv[6],
                                   '\n19 - 64    ',fnv$fnv[3],'        ',fnv$fnv[7], 
                                   '\n65+         ',fnv$fnv[4],'        ',fnv$fnv[8],
                                   '\n',
                                   '\nYour model will be saved as:',model_name,'-',system_time,'.rds',
                                   '\n',
                                   '\nYour model summary report will be saved as:',model_name,'-',system_time,'.html',
                                   '\n',
                                   '\nPlease confirm this is correct'),'yesno')$res # POP UP BOX FOR CONFERMATION OF MODEL PARAMETERS
    }  
  }
  
  if(confirm == 'no'){ # ENTER PARAMETER RE-ENTRY LOOP IF CONFERMATION IS NOT GIVEN
    
    # CHECK IF ADULTS ARE INCLUDED IN THE POLICY OR NOT
    #-------------------------------------------------------------------------
    if(adults == 'yes'){
      adults_change <- dlg_message('adults will be included in the fruit and vegetable increase policy \nwould you like to change this?','yesno')$res 
      if(adults_change == 'yes'){
        dlgMessage('adults will not be included in the fruit and vegetable increase policy')$res
        adults <- 'no'
      } else {
        dlgMessage('adults will be included in the fruit and vegetable increase policy')$res
        adults <- 'yes'      
      }
    } else {
      adults_change <- dlg_message('adults will not be included in the fruit and vegetable increase policy \nwould you like to change this?','yesno')$res 
      if(adults_change == 'no'){
        dlgMessage('adults will not be included in the fruit and vegetable increase policy')$res
        adults <- 'no'
      } else {
        dlgMessage('adults will be included in the fruit and vegetable increase policy')$res
        adults <- 'yes'      
      }
    }
    
    # CHECK IF THE POLICY LIFETIME GIVEN WAS CORRECT
    #--------------------------------------------------------------------
    policy_lifetime_correct <- dlg_message(paste('the policy lifetime is',
                                                 policy_lifetime,
                                                 '\nwould you like to change this?'),'yesno')$res
    
    if(policy_lifetime_correct == 'yes'){ #IF POLICY LIFETIME IS INCORECT - RE-ENTER
      policy_lifetime <- dlgInput("the lifetime of the policy being modelled/ length of time the fruit and vegetable increase is sustained (in years):",
                                  Sys.info()[""])$res %>% # POP UP BOX FOR THE LIFETIME OF THE POLICY MODELED
        as.numeric()
      
      # CHECK POLICY LIFETIME
      numeric_readin_check(name = 'Policy Lifetime',
                           range_min = 5,
                           range_max = 25,
                           input_message = "the lifetime of the policy being modelled/ length of time the fruit and vegetable increase is sustained (in years):",
                           num = policy_lifetime,
                           unit = 'years',
                           variable = 'policy_lifetime')
      
      age_min_initial <- 6 - policy_lifetime # CALCULATE MINIMUM AGE IN THE MODEL
      
      age_max <- ifelse(adults == 'yes',100,18) # CALCULATE MAXIMUM AGE IN THE MODEL
      
      age_range <- c(age_min_initial:age_max) # CALCULATE AGE RANGE IN THE POLICY
    }
    
    # CHECK IF THE EVALUATION PERIOD GIVEN WAS CORRECT
    #---------------------------------------------------------------------------
    eval_period_correct <- dlg_message(paste('the evaluation period is',
                                             eval_period,'years',
                                             '\nwould you like to change this?'),'yesno')$res
    
    if(eval_period_correct == 'yes'){
      eval_period <- dlgInput("over what period should the policy be evaluated (years):",
                              Sys.info()[" "])$res %>% # POP UP BOX FOR THE EVALUATION PERIOD OF THE POLICY MODELED
        as.numeric()
      
      numeric_readin_check(name = 'policy evaluation period',
                           range_min = 10,
                           range_max = 100,
                           input_message = "over what period should the policy be evaluated (years):",
                           num = eval_period,
                           unit = 'years',
                           variable = 'eval_period')
    }
    
    # CHECK IF NHS MULTIPLIER IS USED OR NOT
    #-------------------------------------------------------------------------
    if(nhs_multiplier == 'yes'){
      nhs_multiplier_change <- dlg_message('the NHS cost 4.7 times multiplier will be applied \nwould you like to change this?','yesno')$res 
      if(nhs_multiplier_change == 'yes'){
        dlgMessage('the NHS cost 4.7 times multiplier will not be applied')$res
        nhs_multiplier <- 'no'
      } else {
        dlgMessage('the NHS cost 4.7 times multiplier will be applied')$res
        nhs_multiplier <- 'yes'      
      }
    } else {
      nhs_multiplier_change <- dlg_message('the NHS cost 4.7 times multiplier will not be applied \nwould you like to change this?','yesno')$res 
      if(nhs_multiplier_change == 'no'){
        dlgMessage('the NHS cost 4.7 times multiplier will not be applied')$res
        nhs_multiplier <- 'no'
      } else {
        dlgMessage('the NHS cost 4.7 times multiplier will be applied')$res
        nhs_multiplier <- 'yes'      
      }
    }
    
    # CHECK IF THE TREATMENT fnv FOR 4-10 YEAR OLD MALES GIVEN WAS CORRECT
    #---------------------------------------------------------------------------------------
    treatment_fnv_correct <- dlg_message(paste('the daily fruit and vegetable increase for 4 - 10 year old males is',
                                                fnv$fnv[1],'portions',
                                                '\nwould you like to change this?'),'yesno')$res
    
    if(treatment_fnv_correct == 'yes'){
      treatment_fnv <- dlgInput("daily fruit and vegetable increase for 4 - 10 year old, males (portions):",
                                 Sys.info()[" "])$res %>% # POP UP BOX FOR THE fruit and vegetable increase OF THE POLICY MODELED
                                 as.numeric()
      
      numeric_readin_check(name = 'daily fruit and vegetable increase for 4 - 10 year old males',
                           range_min = 0,
                           range_max = 5,
                           input_message = "fruit and vegetable increase for 4 - 10 year old, males (portions):",
                           num = treatment_fnv,
                           unit = 'portions',
                           variable = 'treatment_fnv')
      
      fnv$fnv[1] <- treatment_fnv
    }
    
    # CHECK IF THE TREATMENT fnv FOR 4-10 YEAR OLD FEMALES GIVEN WAS CORRECT
    #-------------------------------------------------------------------------------------------
    treatment_fnv_correct <- dlg_message(paste('the daily fruit and vegetable increase for 4 - 10 year old females is',
                                                fnv$fnv[5],'portions',
                                                '\nwould you like to change this?'),'yesno')$res
    
    if(treatment_fnv_correct == 'yes'){
      treatment_fnv <- dlgInput("fruit and vegetable increase for 4 - 10 year old, females (portions):",
                                 Sys.info()[" "])$res %>% # POP UP BOX FOR THE fruit and vegetable increase OF THE POLICY MODELED
        as.numeric()
      
      numeric_readin_check(name = 'daily fruit and vegetable increase for 4 - 10 year old females',
                           range_min = 0,
                           range_max = 5,
                           input_message = "fruit and vegetable increase for 4 - 10 year old, females (portions):",
                           num = treatment_fnv,
                           unit = 'portions',
                           variable = 'treatment_fnv')
      
      fnv$fnv[5] <- treatment_fnv
    }
    
    # CHECK IF THE TREATMENT fnv FOR 11-18 YEAR OLD MALES GIVEN WAS CORRECT
    #-------------------------------------------------------------------------------------------------
    treatment_fnv_correct <- dlg_message(paste('the daily fruit and vegetable increase for 11 - 18 year old males is',
                                                fnv$fnv[2],'portions',
                                                '\nwould you like to change this?'),'yesno')$res
    
    if(treatment_fnv_correct == 'yes'){
      
      treatment_fnv <- dlgInput("fruit and vegetable increase for 11 - 18 year old, males (portions):",
                                 Sys.info()[" "])$res %>% # POP UP BOX FOR THE fruit and vegetable increase OF THE POLICY MODELED
        as.numeric()
      
      numeric_readin_check(name = 'daily fruit and vegetable increase for 11 - 18 year old males',
                           range_min = 0,
                           range_max = 5,
                           input_message = "fruit and vegetable increase for 11 - 18 year old, males (portions):",
                           num = treatment_fnv,
                           unit = 'portions',
                           variable = 'treatment_fnv')
      
      fnv$fnv[2] <- treatment_fnv
    }
    
    # CHECK IF THE TREATMENT fnv FOR 11-18 YEAR OLD FEMALES GIVEN WAS CORRECT
    #-----------------------------------------------------------------------------------
    treatment_fnv_correct <- dlg_message(paste('the daily fruit and vegetable increase for 11 - 18 year old females is',
                                                fnv$fnv[6],'portions',
                                                '\nwould you like to change this?'),'yesno')$res
    
    if(treatment_fnv_correct == 'yes'){
      
      treatment_fnv <- dlgInput("fruit and vegetable increase for 11 - 18 year old, females (portions):",
                                 Sys.info()[" "])$res %>% # POP UP BOX FOR THE fruit and vegetable increase OF THE POLICY MODELED
        as.numeric()
      
      numeric_readin_check(name = 'daily fruit and vegetable increase for 11 - 18 year old females',
                           range_min = 0,
                           range_max = 5,
                           input_message = "fruit and vegetable increase for 11 - 18 year old, females (portions):",
                           num = treatment_fnv,
                           unit = 'portions',
                           variable = 'treatment_fnv')
      
      fnv$fnv[6] <- treatment_fnv
    }
    
    if(adults == 'yes'){
      # CHECK IF THE TREATMENT fnv FOR 19-64 YEAR OLD MALES GIVEN WAS CORRECT
      #-------------------------------------------------------------------------------------
      treatment_fnv_correct <- dlg_message(paste('the daily fruit and vegetable increase for 19-64 year old males is',
                                                  fnv$fnv[3],'portions',
                                                  '\nwould you like to change this?'),'yesno')$res
      
      if(treatment_fnv_correct == 'yes'){
        
        treatment_fnv <- dlgInput("fruit and vegetable increase for 19 - 64 year old, males (portions):",
                                   Sys.info()[" "])$res %>% # POP UP BOX FOR THE fruit and vegetable increase OF THE POLICY MODELED
          as.numeric()
        
        numeric_readin_check(name = 'daily fruit and vegetable increase for 19 - 64 year old males',
                             range_min = 0,
                             range_max = 5,
                             input_message = "fruit and vegetable increase for 19 - 64 year old, males (portions):",
                             num = treatment_fnv,
                             unit = 'portions',
                             variable = 'treatment_fnv')
        
        fnv$fnv[3] <- treatment_fnv
      }
      
      # CHECK IF THE TREATMENT fnv FOR 19-64 YEAR OLD FEMALES GIVEN WAS CORRECT
      #-------------------------------------------------------------------------------------
      treatment_fnv_correct <- dlg_message(paste('the daily fruit and vegetable increase for 19 - 64 year old females is',
                                                  fnv$fnv[7],'portions',
                                                  '\nwould you like to change this?'),'yesno')$res
      
      if(treatment_fnv_correct == 'yes'){
        
        treatment_fnv <- dlgInput("fruit and vegetable increase for 19 - 64 year old, females (portions):",
                                   Sys.info()[" "])$res %>% # POP UP BOX FOR THE fruit and vegetable increase OF THE POLICY MODELED
          as.numeric()
        
        numeric_readin_check(name = 'daily fruit and vegetable increase for 19 - 64 year old females',
                             range_min = 0,
                             range_max = 5,
                             input_message = "fruit and vegetable increase for 19 - 64 year old, females (portions):",
                             num = treatment_fnv,
                             unit = 'portions',
                             variable = 'treatment_fnv')
        
        fnv$fnv[7] <- treatment_fnv
      }
      
      # CHECK IF THE TREATMENT fnv FOR 65+ YEAR OLD MALES GIVEN WAS CORRECT
      #--------------------------------------------------------------------------------------
      treatment_fnv_correct <- dlg_message(paste('the daily fruit and vegetable increase for 65+ year old males is',
                                                  fnv$fnv[4],'portions',
                                                  '\nwould you like to change this?'),'yesno')$res
      
      if(treatment_fnv_correct == 'yes'){
        
        treatment_fnv <- dlgInput("fruit and vegetable increase for 65+ year old, males (portions):",
                                   Sys.info()[" "])$res %>% # POP UP BOX FOR THE fruit and vegetable increase OF THE POLICY MODELED
          as.numeric()
        
        numeric_readin_check(name = 'daily fruit and vegetable increase for 65+ year old males',
                             range_min = 0,
                             range_max = 5,
                             input_message = "fruit and vegetable increase for 65+ year old, males (portions):",
                             num = treatment_fnv,
                             unit = 'portions',
                             variable = 'treatment_fnv')
        
        fnv$fnv[4] <- treatment_fnv
      }
      
      # CHECK IF THE TREATMENT fnv FOR 65+ YEAR OLD FEMALES GIVEN WAS CORRECT
      #-------------------------------------------------------------------------------------
      treatment_fnv_correct <- dlg_message(paste('the daily fruit and vegetable increase for 65+ year old females is',
                                                  fnv$fnv[8],'portions',
                                                  '\nwould you like to change this?'),'yesno')$res
      
      if(treatment_fnv_correct == 'yes'){
        
        treatment_fnv <- dlgInput("fruit and vegetable increase for 65+ year old, females (portions):",
                                   Sys.info()[" "])$res %>% # POP UP BOX FOR THE fruit and vegetable increase OF THE POLICY MODELED
          as.numeric()
        
        numeric_readin_check(name = 'daily fruit and vegetable increase for 65+ year old females',
                             range_min = 0,
                             range_max = 5,
                             input_message = "fruit and vegetable increase for 65+ year old, females (portions):",
                             num = treatment_fnv,
                             unit = 'portions',
                             variable = 'treatment_fnv')
        
        fnv$fnv[8] <- treatment_fnv
      }
    } else {
      fnv$fnv[3] <- 0
      fnv$fnv[4] <- 0
      fnv$fnv[7] <- 0
      fnv$fnv[8] <- 0
    }
    
    # CHECK IF THE MODEL NAME GIVEN WAS CORRECT
    #-------------------------------------------------------------------------------
    model_name_correct <- dlg_message(paste('The model name is',
                                            model_name,
                                            '\nwould you like to change this?'),'yesno')$res 
    
    if(model_name_correct == 'yes'){
      model_name <- dlgInput("Enter model name: 
                             \n(the model and report will be saved as this with a time-stamp added)",
                             Sys.info()[" "])$res # POP UP BOX FOR THE NAME OF THE MODEL RUN
      
    }
  } 
}  
# 
# # IS FV TARGETING USED - SECTion 1.3
# ##################################################
# 
# fv_targetting <- dlg_message('Is FV targeting used?','yesno')$res 
# 
# # SET UP FV INCREASE TABEL IF FV TARGETTING IS USED
# #----------------------------------------------------
# if(fv_targetting == 'yes'){
#   fv_increases <- c()
#   for(sex in c('males','females')){
#     for(age in c('5 - 10','11 - 18','19 - 64','65+')){
#       for(fv in c('low FV consumption','average FV consumption','high FV consumption')){
#         
#         fv_increase <- dlgInput(paste('fruit and vegetable increase for',fv,age,'year old',sex,'(portions):'),
#                                 Sys.info()[" "])$res %>% # POP UP BOX FOR THE fruit and vegetable increase OF THE POLICY MODELED
#           as.numeric()
#         
#         numeric_readin_check(name = paste('fruit and vegetable increase for',fv,age,'year old',sex),
#                              range_min = 0,
#                              range_max = 5,
#                              input_message = paste('fruit and vegetable increase for',fv,age,'year old',sex,'(portions):'),
#                              num = fv_increase,
#                              unit = 'portions',
#                              variable = 'fv_increase')
#       
#       fv_increase <- c(fv_increases,fv_increase)
#       }
#     }
#   }
#   
#   # INITIALISE fruit and vegetable increase DATA FRAME
#   fnv <- data.frame(Sex = c(rep(1,12),rep(2,12)), # MALES AND FEMALES
#                     age_cat = c(rep(1,3),rep(2,3),rep(3,3),rep(4,3),rep(1,3),rep(2,3),rep(3,3),rep(4,3)), # 4 AGE CATEGORIES
#                     fv_cat = rep(c(1,2,3),8),
#                     fnv = fv_increase)
#   
#   confirm2 <- 'no'
#   
#   while(confirm2 == 'no'){
#     confirm2 <- dlg_message(paste('\nThe per day fruit and vegetable increase:',
#                                   '\nAge Category    FV Category     Males      Females',
#                                   '\n5 - 10               low FV consumption    ',fnv$fnv[1],'        ',fnv$fnv[21],
#                                   '\n5 - 10               average FV consumption',fnv$fnv[2],'        ',fnv$fnv[22],
#                                   '\n5 - 10               high FV consumption   ',fnv$fnv[3],'        ',fnv$fnv[23],
#                                   '\n11 - 18              low FV consumption    ',fnv$fnv[7],'        ',fnv$fnv[27],
#                                   '\n11 - 18              average FV consumption',fnv$fnv[8],'        ',fnv$fnv[28],
#                                   '\n11 - 18              high FV consumption   ',fnv$fnv[9],'        ',fnv$fnv[29],
#                                   '\n19 - 64              low FV consumption    ',fnv$fnv[11],'        ',fnv$fnv[31], 
#                                   '\n19 - 64              average FV consumption',fnv$fnv[12],'        ',fnv$fnv[32], 
#                                   '\n19 - 64              high FV consumption   ',fnv$fnv[13],'        ',fnv$fnv[33], 
#                                   '\n65+                  low FV consumption    ',fnv$fnv[16],'        ',fnv$fnv[36],
#                                   '\n65+                  average FV consumption',fnv$fnv[17],'        ',fnv$fnv[37],
#                                   '\n65+                  high FV consumption   ',fnv$fnv[18],'        ',fnv$fnv[38],
#                                   '\n',
#                                   '\nPlease confirm this is correct'),'yesno')$res 
#     
#     if(confirm2 == 'no'){
#       n <- 0 #LOOP COUNTER
#       fv_increases <- c() #BLANK LIST TO FILL UP
#       for(sex in c('males','females')){
#         for(age in c('5 - 10','11 - 18','19 - 64','65+')){
#           for(fv in c('low FV consumption','average FV consumption','high FV consumption')){
#             n <- n + 1 #LOOP COUNTER
#             
#             fv_increase <- fnv$fnv[n]
#             
#             # CHECK IF THE TREATMENT fnv FOR 65+ YEAR OLD FEMALES GIVEN WAS CORRECT
#             #-------------------------------------------------------------------------------------
#             treatment_fnv_correct <- dlg_message(paste('fruit and vegetable increase for',fv,age,'year old',sex,'is', 
#                                                         fnv$fnv[n],'portions',
#                                                         '\nwould you like to change this?'),'yesno')$res 
#             
#             if(treatment_fnv_correct == 'yes'){
#               
#               fv_increase <- dlgInput(paste('fruit and vegetable increase for',fv,age,'year old',sex,'(portions):'),
#                                             Sys.info()[" "])$res %>% # POP UP BOX FOR THE fruit and vegetable increase OF THE POLICY MODELED
#                 as.numeric()
#               
#               numeric_readin_check(name = paste('fruit and vegetable increase for',fv,age,'year old',sex),
#                                    range_min = 0,
#                                    range_max = 5,
#                                    input_message = paste('fruit and vegetable increase for',fv,age,'year old',sex,'(portions):'),
#                                    num = fv_increase,
#                                    unit = 'portions',
#                                    variable = 'fv_increase')
#               
#             }
#             fv_increase <- c(fv_increases,fv_increase)
#             
#           }
#         }
#       }
#       # INITIALISE fruit and vegetable increase DATA FRAME
#       fnv <- data.frame(Sex = c(rep(1,12),rep(2,12)), # MALES AND FEMALES
#                         age_cat = c(rep(1,3),rep(2,3),rep(3,3),rep(4,3),rep(1,3),rep(2,3),rep(3,3),rep(4,3)), # 4 AGE CATEGORIES
#                         fv_cat = rep(c(1,2,3),8),
#                         fnv = fv_increase)
#     }
#   }
# } else {
#   fnv <- data.frame(Sex = c(rep(1,12),rep(2,12)), # MALES AND FEMALES
#                     age_cat = c(rep(1,3),rep(2,3),rep(3,3),rep(4,3),rep(1,3),rep(2,3),rep(3,3),rep(4,3)), # 4 AGE CATEGORIES
#                     fv_cat = rep(c(1,2,3),8),
#                     fnv = c(rep(fnv$fnv[1],3),rep(fnv$fnv[2],3),rep(fnv$fnv[3],3),rep(fnv$fnv[4],3),rep(fnv$fnv[5],3),rep(fnv$fnv[6],3),rep(fnv$fnv[7],3),rep(fnv$fnv[8],3))) # FV REDUCTIONS
# }

run <- dlg_message(paste('          Run Model',
                         '\n',
                         '\n(this will take a few minutes)'),'yesno')$res

if(run == 'no'){
  dlgMessage("Fruit and Vegtable model aborted, so quitting.")$res # USER ABORTED THE RUN
  quit(save = 'no') # CLOSER R IF USER HAS ABORTED
}

options <- read_csv("heemod data/options.csv")

options$value[3] <- eval_period # CHANGE EVALUATION PERIOD IN INPUT FILE

write_csv(options, "heemod data/options.csv")

parameters <- read_csv("heemod data/parameters.csv")

parameters$value[1] <- ifelse(nhs_multiplier == 'yes',4.7,1) # CHANGE NHS MULTIPLIER IN INPUT FILE

write_csv(parameters, "heemod data/parameters.csv")

#############################################
#############################################
#### SECTION 2 - SET UP TREATMENT GROUP #####
#############################################
#############################################

plan(multisession(workers = 4)) # Requirement for the model to use parallel-processing on DHSC laptop.7

start_readin <- as.numeric(Sys.time())

hse <- read_csv("data/heemod_master_files/counterfactual.csv") %>%
  filter(initial_age %in% age_range) # LOAD COUNTERFACTUALS AND FILTER TO AGE RANGE OF INTEREST

read_csv("data/heemod_master_files/demographics.csv") %>%
  filter(initial_age %in% age_range) %>% # Write the correct demographic file
  write_csv("heemod data/demographics.csv")

hse %>% # Writing just the counterfactual that we want.
  filter(age >= initial_age) %>%
  select(age, initial_age, sex, fv_cat, fv_consumption) %>%
  write_csv("heemod data/additional params/counterfactual.csv")

end_readin <- as.numeric(Sys.time())

read_csv("heemod data/additional params/counterfactual.csv") %>%
  mutate(fv_consumption = if_else(initial_age >= 5 & initial_age <= 10, # ALTER F&V CONSUMPTION ACCORDING TO AGE AND SEX
                                  if_else(sex == 1, 
                                          fv_consumption + fnv$fnv[1],
                                          fv_consumption + fnv$fnv[5]),
                                  fv_consumption)) %>% 
  mutate(fv_consumption = if_else(initial_age >= 11 & initial_age <= 18,
                                  if_else(sex == 1, 
                                         fv_consumption + fnv$fnv[2],
                                         fv_consumption + fnv$fnv[6]),
                                  fv_consumption)) %>% 
  mutate(fv_consumption = if_else(initial_age >= 19 & initial_age <= 64,
                                 if_else(sex == 1, 
                                         fv_consumption + fnv$fnv[3],
                                         fv_consumption + fnv$fnv[7]),
                                 fv_consumption)) %>% 
  mutate(fv_consumption = if_else(initial_age >= 65,
                                 if_else(sex == 1, 
                                         fv_consumption + fnv$fnv[4],
                                         fv_consumption + fnv$fnv[8]),
                                 fv_consumption)) %>% 
  write_csv("heemod data/additional params/treat.csv")

######################################
######################################
### section 3 - run calorie model ####
######################################
######################################

start_model <- as.numeric(Sys.time())

model <- run_model_tabular("heemod data/") # RUN MODEL FROM THE PARAMETERS AND DATA THAT IS IN THE HEEMOD DATA FILES

end_model <- as.numeric(Sys.time())

# ADD TIMING AND SETUP PARAMETERS TO THE MODEL FILE
#-------------------------------------------------------------------
model$timings <- data.frame('Modelling stage' = c('HSE readin',
                                                  'Markov model run'),
                            'Time elapsed' = c(paste(-round((start_readin - end_readin)/60,2),'mins'),
                                               paste(-round((start_model - end_model)/60,2),'mins')))

model$fnv <- fnv 

model$name <- model_name

model$nhs_multiplier <- nhs_multiplier

model$policy_lifetime <- policy_lifetime

#model$fnv_targeting <- fnv_targetting

#############################################  
#############################################
## SECTION 4 - SAVE MODEL AND WRITE REPORT ##
#############################################
#############################################


if(!dir.exists('model')) dir.create('model')

write_rds(model,paste0('model/',model$name,' - ',Sys.Date(),'.rds')) # SAVE MODEL

source('src/Model analysis/model analysis.R') # RUN MODEL ANALYSIS FILED

if(!dir.exists('model')){
  dlgMessage('Ops, something went wrong....')
} else {
  dlgMessage(paste0('          Model simulation complete!!',
                    '\n',
                    '\nNow archive the current working directary in the shared drive to maintain an audit trail of your model run.', # REMIND USER TO ARCHIVE 
                    '\n',
                    '\nA summary report on the model, called ',
                    model$name,' - ',system_time,'.html, can be found at ',
                    getwd(),'/report',
                    '\n',
                    '\nThe full model, called ',
                    model$name,' - ',system_time,'.rds, can be found at ',
                    getwd(),'/model',
                    '\n',
                    '\nThank you for using the DHSC Fruit & Vegetebale Model'), 
             'yesno')$res 
}
