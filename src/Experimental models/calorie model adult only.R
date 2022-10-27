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

welcome <- dlgMessage(paste('          Welcome to the Calorie Model',
                            '\n',
                            '\nA series of pop up boxes will allow you to set up your model simulation before running the model.'),"yesno")$res # ARE ADULTS AFFECTED BY THE POLICY
if(welcome == 'no'){
  dlgMessage("Calorie model started in error so quitting.")$res # REMIND USER TO MAKE FOLDER COPY 
  quit(save = 'no') # CLOSER R IF USER HAS NOT COPIED THE WORKING FOLDER
}

folder <- dlgMessage("Before setting up your model please confirm that you have copied and renamed the working folder as described in step 1 of section 4.1 in the model documentation?", "yesno")$res # ARE ADULTS AFFECTED BY THE POLICY

if(folder == 'no'){
  dlgMessage("Calorie model started in error so quitting, copy and rename the working folder and try again")$res # REMIND USER TO MAKE FOLDER COPY 
  quit(save = 'no') # CLOSER R IF USER HAS NOT COPIED THE WORKING FOLDER
}

#adults <- dlgMessage("Are adults included in the model cohort?", "yesno")$res # ARE ADULTS AFFECTED BY THE POLICY
adults <- 'yes' # set as an adult only policy

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

age_min_initial <- 18 - policy_lifetime # CALCULATE MINIMUM INITIAL AGE IN THE MODEL (changed to 18 from 5 as an adult only policy)

age_max <- ifelse(adults == 'yes',100,18) # CALCULATE MAXIMUM AGE IN THE MODEL

age_range <- c(age_min_initial:age_max) # CALCULATE AGE RANGE IN THE POLICY

uni_treatment_kcal <- dlgMessage('is there a uniform (all age groups, male and female) daily calorie reduction for all people included in the policy', 'yesno')$res # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED

# INITIALISE CALORIE REDUCTION DATA FRAME
kcal <- data.frame(Sex = c(1,1,1,1,2,2,2,2), # MALES AND FEMALES
                   age_cat = c(1,2,3,4,1,2,3,4), # 4 AGE CATEGORIES
                   kcal = rep(0,8)) # CAL EDUCTIONS ARE SET TO 0 INITIALY

if (uni_treatment_kcal == 'yes'){ # IF ALL INDIVIDUALS ARE SUBJECT TO THE SAME CAL REDUCTION
  treatment_kcal <- dlgInput("uniform (all age groups, male and female) daily calorie reduction for all people included in the policy (kcal):",
                             Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
    as.numeric()
  # CHECK UNIFORM CAL REDUCTION
  numeric_readin_check(name = 'uniform (all age groups, male and female) daily calorie reduction',
                       range_min = 0,
                       range_max = 25,
                       input_message = "uniform (all age groups, male and female) daily calorie reduction for all people included in the policy (kcal):",
                       num = treatment_kcal,
                       unit = 'kcals',
                       variable = 'treatment_kcal')
  
  if(adults == 'no'){ # IF UNIFORM CAL REDUCTION AND ADULTS NOT INCLUDED
    kcal$kcal[1] <- treatment_kcal# MAKE ALL CHILD CAL REDUCTIONS THE SAME
    kcal$kcal[2] <- treatment_kcal
    kcal$kcal[5] <- treatment_kcal
    kcal$kcal[6] <- treatment_kcal
  }
  if(adults == 'yes'){ # IF UNIFORM CAL REDUCTION AND ADULTS INCLUDED
    kcal$kcal <- treatment_kcal # MAKE ALL CAL REDUCTION THE SAME
  }
  
} else {
  
  treatment_kcal <- dlgInput("calorie reduction for 5 - 10 year old, males (kcal):",
                             Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
    as.numeric()
  
  numeric_readin_check(name = 'daily calorie reduction for 5 - 10 year old males',
                       range_min = 0,
                       range_max = 25,
                       input_message = "calorie reduction for 5 - 10 year old, males (kcal):",
                       num = treatment_kcal,
                       unit = 'kcals',
                       variable = 'treatment_kcal')
  
  kcal$kcal[1] <- treatment_kcal
  
  treatment_kcal <- dlgInput("calorie reduction for 5 - 10 year old, females (kcal):",
                             Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
    as.numeric()
  
  numeric_readin_check(name = 'daily calorie reduction for 5 - 10 year old females',
                       range_min = 0,
                       range_max = 25,
                       input_message = "calorie reduction for 5 - 10 year old, females (kcal):",
                       num = treatment_kcal,
                       unit = 'kcals',
                       variable = 'treatment_kcal')
  
  kcal$kcal[5] <- treatment_kcal
  
  treatment_kcal <- dlgInput("calorie reduction for 11 - 18 year old, males (kcal):",
                             Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
    as.numeric()
  
  numeric_readin_check(name = 'daily calorie reduction for 11 - 18 year old males',
                       range_min = 0,
                       range_max = 25,
                       input_message = "calorie reduction for 11 - 18 year old, males (kcal):",
                       num = treatment_kcal,
                       unit = 'kcals',
                       variable = 'treatment_kcal')
  
  kcal$kcal[2] <- treatment_kcal
  
  treatment_kcal <- dlgInput("calorie reduction for 11 - 18 year old, females (kcal):",
                             Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
    as.numeric()
  
  numeric_readin_check(name = 'daily calorie reduction for 11 - 18 year old females',
                       range_min = 0,
                       range_max = 25,
                       input_message = "calorie reduction for 11 - 18 year old, females (kcal):",
                       num = treatment_kcal,
                       unit = 'kcals',
                       variable = 'treatment_kcal')
  
  kcal$kcal[6] <- treatment_kcal
  
  if (adults == 'yes'){
    treatment_kcal <- dlgInput("calorie reduction for 19 - 64 year old, males (kcal):",
                               Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
      as.numeric()
    
    numeric_readin_check(name = 'daily calorie reduction for 19 - 64 year old males',
                         range_min = 0,
                         range_max = 25,
                         input_message = "calorie reduction for 19 - 64 year old, males (kcal):",
                         num = treatment_kcal,
                         unit = 'kcals',
                         variable = 'treatment_kcal')
    
    kcal$kcal[3] <- treatment_kcal
    
    treatment_kcal <- dlgInput("calorie reduction for 19 - 64 year old, females (kcal):",
                               Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
      as.numeric()
    
    numeric_readin_check(name = 'daily calorie reduction for 19 - 64 year old females',
                         range_min = 0,
                         range_max = 25,
                         input_message = "calorie reduction for 19 - 64 year old, females (kcal):",
                         num = treatment_kcal,
                         unit = 'kcals',
                         variable = 'treatment_kcal')
    
    kcal$kcal[7] <- treatment_kcal
    
    treatment_kcal <- dlgInput("calorie reduction for 65+ year old, males (kcal):",
                               Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
      as.numeric()
    
    numeric_readin_check(name = 'daily calorie reduction for 65+ year old males',
                         range_min = 0,
                         range_max = 25,
                         input_message = "calorie reduction for 65+ year old, males (kcal):",
                         num = treatment_kcal,
                         unit = 'kcals',
                         variable = 'treatment_kcal')
    
    kcal$kcal[4] <- treatment_kcal
    
    treatment_kcal <- dlgInput("calorie reduction for 65+ year old, females (kcal):",
                               Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
      as.numeric()
    
    numeric_readin_check(name = 'daily calorie reduction for 65+ year old females',
                         range_min = 0,
                         range_max = 25,
                         input_message = "calorie reduction for 65+ year old, females (kcal):",
                         num = treatment_kcal,
                         unit = 'kcals',
                         variable = 'treatment_kcal')
    
    kcal$kcal[8] <- treatment_kcal
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

nhs_multiplier <- dlg_message('should the 4 time multiplier for NHS costs be used?','yesno')$res # POP UP BOX FOR THE USE OF MULTIPLIERPOLICY MODELED

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
                                   '\nAdults are included in the calorie reduction policy',
                                   '\n',
                                   '\nNHS costs will be multiplied by 4',
                                   '\n',
                                   '\nThe per day calorie reduction:',
                                   '\n            Males      Females',
                                   '\n5 - 10      ',kcal$kcal[1],'        ',kcal$kcal[5],
                                   '\n11 - 18    ',kcal$kcal[2],'        ',kcal$kcal[6],
                                   '\n19 - 64    ',kcal$kcal[3],'        ',kcal$kcal[7], 
                                   '\n65+         ',kcal$kcal[4],'        ',kcal$kcal[8],
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
                                   '\nAdults are not included in the calorie reduction policy',
                                   '\n',
                                   '\nNHS costs will be multiplied by 4',
                                   '\n',
                                   '\nThe per day calorie reduction:',
                                   '\n            Males      Females',
                                   '\n5 - 10      ',kcal$kcal[1],'        ',kcal$kcal[5],
                                   '\n11 - 18    ',kcal$kcal[2],'        ',kcal$kcal[6],
                                   '\n19 - 64    ',kcal$kcal[3],'        ',kcal$kcal[7], 
                                   '\n65+         ',kcal$kcal[4],'        ',kcal$kcal[8],
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
                                   '\nAdults are included in the calorie reduction policy',
                                   '\n',
                                   '\nNHS costs will not be multiplied by 4',
                                   '\n',
                                   '\nThe per day calorie reduction:',
                                   '\n            Males      Females',
                                   '\n5 - 10      ',kcal$kcal[1],'        ',kcal$kcal[5],
                                   '\n11 - 18    ',kcal$kcal[2],'        ',kcal$kcal[6],
                                   '\n19 - 64    ',kcal$kcal[3],'        ',kcal$kcal[7], 
                                   '\n65+         ',kcal$kcal[4],'        ',kcal$kcal[8],
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
                                   '\nAdults are not included in the calorie reduction policy',
                                   '\n',
                                   '\nNHS costs not will be multiplied by 4',
                                   '\n',
                                   '\nThe per day calorie reduction:',
                                   '\nAge Category    Males      Females',
                                   '\n5 - 10      ',kcal$kcal[1],'        ',kcal$kcal[5],
                                   '\n11 - 18    ',kcal$kcal[2],'        ',kcal$kcal[6],
                                   '\n19 - 64    ',kcal$kcal[3],'        ',kcal$kcal[7], 
                                   '\n65+         ',kcal$kcal[4],'        ',kcal$kcal[8],
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
      adults_change <- dlg_message('adults will be included in the calorie reduction policy \nwould you like to change this?','yesno')$res 
      if(adults_change == 'yes'){
        dlgMessage('adults will not be included in the calorie reduction policy')$res
        adults <- 'no'
      } else {
        dlgMessage('adults will be included in the calorie reduction policy')$res
        adults <- 'yes'      
      }
    } else {
      adults_change <- dlg_message('adults will not be included in the calorie reduction policy \nwould you like to change this?','yesno')$res 
      if(adults_change == 'no'){
        dlgMessage('adults will not be included in the calorie reduction policy')$res
        adults <- 'no'
      } else {
        dlgMessage('adults will be included in the calorie reduction policy')$res
        adults <- 'yes'      
      }
    }

    # CHECK IF THE POLICY LIFETIME GIVEN WAS CORRECT
    #--------------------------------------------------------------------
    policy_lifetime_correct <- dlg_message(paste('the policy lifetime is',
                                                 policy_lifetime,
                                                 '\nwould you like to change this?'),'yesno')$res
    
    if(policy_lifetime_correct == 'yes'){ #IF POLICY LIFETIME IS INCORECT - RE-ENTER
      policy_lifetime <- dlgInput("the lifetime of the policy being modelled/ length of time the calorie reduction is sustained (in years):",
                                  Sys.info()[""])$res %>% # POP UP BOX FOR THE LIFETIME OF THE POLICY MODELED
        as.numeric()
      
      # CHECK POLICY LIFETIME
      numeric_readin_check(name = 'Policy Lifetime',
                           range_min = 5,
                           range_max = 25,
                           input_message = "the lifetime of the policy being modelled/ length of time the calorie reduction is sustained (in years):",
                           num = policy_lifetime,
                           unit = 'years',
                           variable = 'policy_lifetime')
      
      age_min_initial <- 18 - policy_lifetime # CALCULATE MINIMUM INITIAL AGE IN THE MODEL
      
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
      nhs_multiplier_change <- dlg_message('the NHS cost 4 times multiplier will be applied \nwould you like to change this?','yesno')$res 
      if(nhs_multiplier_change == 'yes'){
        dlgMessage('the NHS cost 4 times multiplier will not be applied')$res
        nhs_multiplier <- 'no'
      } else {
        dlgMessage('the NHS cost 4 times multiplier will be applied')$res
        nhs_multiplier <- 'yes'      
      }
    } else {
      nhs_multiplier_change <- dlg_message('the NHS cost 4 times multiplier will not be applied \nwould you like to change this?','yesno')$res 
      if(nhs_multiplier_change == 'no'){
        dlgMessage('the NHS cost 4 times multiplier will not be applied')$res
        nhs_multiplier <- 'no'
      } else {
        dlgMessage('the NHS cost 4 times multiplier will be applied')$res
        nhs_multiplier <- 'yes'      
      }
    }
    
    # CHECK IF THE TREATMENT KCAL FOR 4-10 YEAR OLD MALES GIVEN WAS CORRECT
    #---------------------------------------------------------------------------------------
    treatment_kcal_correct <- dlg_message(paste('the daily calorie reduction for 4 - 10 year old males is',
                                                kcal$kcal[1],'kcal',
                                                '\nwould you like to change this?'),'yesno')$res
    
    if(treatment_kcal_correct == 'yes'){
      treatment_kcal <- dlgInput("calorie reduction for 4 - 10 year old, males (kcal):",
                                 Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
        as.numeric()
      
      numeric_readin_check(name = 'daily calorie reduction for 4 - 10 year old males',
                           range_min = 0,
                           range_max = 25,
                           input_message = "calorie reduction for 4 - 10 year old, males (kcal):",
                           num = treatment_kcal,
                           unit = 'kcals',
                           variable = 'treatment_kcal')
      
      kcal$kcal[1] <- treatment_kcal
    }
    
    # CHECK IF THE TREATMENT KCAL FOR 4-10 YEAR OLD FEMALES GIVEN WAS CORRECT
    #-------------------------------------------------------------------------------------------
    treatment_kcal_correct <- dlg_message(paste('the daily calorie reduction for 4 - 10 year old females is',
                                                kcal$kcal[5],'kcal',
                                                '\nwould you like to change this?'),'yesno')$res
    
    if(treatment_kcal_correct == 'yes'){
      treatment_kcal <- dlgInput("calorie reduction for 4 - 10 year old, females (kcal):",
                                 Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
        as.numeric()
      
      numeric_readin_check(name = 'daily calorie reduction for 4 - 10 year old females',
                           range_min = 0,
                           range_max = 25,
                           input_message = "calorie reduction for 4 - 10 year old, females (kcal):",
                           num = treatment_kcal,
                           unit = 'kcals',
                           variable = 'treatment_kcal')
      
      kcal$kcal[5] <- treatment_kcal
    }
    
    # CHECK IF THE TREATMENT KCAL FOR 11-18 YEAR OLD MALES GIVEN WAS CORRECT
    #-------------------------------------------------------------------------------------------------
    treatment_kcal_correct <- dlg_message(paste('the daily calorie reduction for 11 - 18 year old males is',
                                                kcal$kcal[2],'kcal',
                                                '\nwould you like to change this?'),'yesno')$res
    
    if(treatment_kcal_correct == 'yes'){
      
      treatment_kcal <- dlgInput("calorie reduction for 11 - 18 year old, males (kcal):",
                                 Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
        as.numeric()
      
      numeric_readin_check(name = 'daily calorie reduction for 11 - 18 year old males',
                           range_min = 0,
                           range_max = 25,
                           input_message = "calorie reduction for 11 - 18 year old, males (kcal):",
                           num = treatment_kcal,
                           unit = 'kcals',
                           variable = 'treatment_kcal')
      
      kcal$kcal[2] <- treatment_kcal
    }
    
    # CHECK IF THE TREATMENT KCAL FOR 11-18 YEAR OLD FEMALES GIVEN WAS CORRECT
    #-----------------------------------------------------------------------------------
    treatment_kcal_correct <- dlg_message(paste('the daily calorie reduction for 11 - 18 year old females is',
                                                kcal$kcal[6],'kcal',
                                                '\nwould you like to change this?'),'yesno')$res
    
    if(treatment_kcal_correct == 'yes'){
      
      treatment_kcal <- dlgInput("calorie reduction for 11 - 18 year old, females (kcal):",
                                 Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
        as.numeric()
      
      numeric_readin_check(name = 'daily calorie reduction for 11 - 18 year old females',
                           range_min = 0,
                           range_max = 25,
                           input_message = "calorie reduction for 11 - 18 year old, females (kcal):",
                           num = treatment_kcal,
                           unit = 'kcals',
                           variable = 'treatment_kcal')
      
      kcal$kcal[6] <- treatment_kcal
    }
    
    if(adults == 'yes'){
    # CHECK IF THE TREATMENT KCAL FOR 19-64 YEAR OLD MALES GIVEN WAS CORRECT
    #-------------------------------------------------------------------------------------
    treatment_kcal_correct <- dlg_message(paste('the daily calorie reduction for 19-64 year old males is',
                                                kcal$kcal[3],'kcal',
                                                '\nwould you like to change this?'),'yesno')$res
    
    if(treatment_kcal_correct == 'yes'){
      
      treatment_kcal <- dlgInput("calorie reduction for 19 - 64 year old, males (kcal):",
                                 Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
        as.numeric()
      
      numeric_readin_check(name = 'daily calorie reduction for 19 - 64 year old males',
                           range_min = 0,
                           range_max = 25,
                           input_message = "calorie reduction for 19 - 64 year old, males (kcal):",
                           num = treatment_kcal,
                           unit = 'kcals',
                           variable = 'treatment_kcal')
      
      kcal$kcal[3] <- treatment_kcal
    }
    
    # CHECK IF THE TREATMENT KCAL FOR 19-64 YEAR OLD FEMALES GIVEN WAS CORRECT
    #-------------------------------------------------------------------------------------
    treatment_kcal_correct <- dlg_message(paste('the daily calorie reduction for 19 - 64 year old females is',
                                                kcal$kcal[7],'kcal',
                                                '\nwould you like to change this?'),'yesno')$res
    
    if(treatment_kcal_correct == 'yes'){
      
      treatment_kcal <- dlgInput("calorie reduction for 19 - 64 year old, females (kcal):",
                                 Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
        as.numeric()
      
      numeric_readin_check(name = 'daily calorie reduction for 19 - 64 year old females',
                           range_min = 0,
                           range_max = 25,
                           input_message = "calorie reduction for 19 - 64 year old, females (kcal):",
                           num = treatment_kcal,
                           unit = 'kcals',
                           variable = 'treatment_kcal')
      
      kcal$kcal[7] <- treatment_kcal
    }
    
    # CHECK IF THE TREATMENT KCAL FOR 65+ YEAR OLD MALES GIVEN WAS CORRECT
    #--------------------------------------------------------------------------------------
    treatment_kcal_correct <- dlg_message(paste('the daily calorie reduction for 65+ year old males is',
                                                kcal$kcal[4],'kcal',
                                                '\nwould you like to change this?'),'yesno')$res
    
    if(treatment_kcal_correct == 'yes'){
      
      treatment_kcal <- dlgInput("calorie reduction for 65+ year old, males (kcal):",
                                 Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
        as.numeric()
      
      numeric_readin_check(name = 'daily calorie reduction for 65+ year old males',
                           range_min = 0,
                           range_max = 25,
                           input_message = "calorie reduction for 65+ year old, males (kcal):",
                           num = treatment_kcal,
                           unit = 'kcals',
                           variable = 'treatment_kcal')
      
      kcal$kcal[4] <- treatment_kcal
    }
    
    # CHECK IF THE TREATMENT KCAL FOR 65+ YEAR OLD FEMALES GIVEN WAS CORRECT
    #-------------------------------------------------------------------------------------
    treatment_kcal_correct <- dlg_message(paste('the daily calorie reduction for 65+ year old females is',
                                                kcal$kcal[8],'kcal',
                                                '\nwould you like to change this?'),'yesno')$res
    
    if(treatment_kcal_correct == 'yes'){
      
      treatment_kcal <- dlgInput("calorie reduction for 65+ year old, females (kcal):",
                                 Sys.info()[" "])$res %>% # POP UP BOX FOR THE CALORIE REDUCTION OF THE POLICY MODELED
        as.numeric()
      
      numeric_readin_check(name = 'daily calorie reduction for 65+ year old females',
                           range_min = 0,
                           range_max = 25,
                           input_message = "calorie reduction for 65+ year old, females (kcal):",
                           num = treatment_kcal,
                           unit = 'kcals',
                           variable = 'treatment_kcal')
      
      kcal$kcal[8] <- treatment_kcal
    }
    } else {
      kcal$kcal[3] <- 0
      kcal$kcal[4] <- 0
      kcal$kcal[7] <- 0
      kcal$kcal[8] <- 0
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

run <- dlg_message(paste('          Run Model',
                         '\n',
                         '\n(this will take a few minutes)'),'yesno')$res 

if(run == 'no'){
  dlgMessage("Calorie model aborted, so quitting.")$res # USER ABORTED THE RUN
  quit(save = 'no') # CLOSER R IF USER HAS ABORTED
}

options <- read_csv("heemod data/options.csv")

options$value[3] <- eval_period # CHANGE EVALUATION PERIOD IN INPUT FILE

write_csv(options, "heemod data/options.csv")

parameters <- read_csv("heemod data/parameters.csv")

parameters$value[1] <- ifelse(nhs_multiplier == 'yes',4,1) # CHANGE NHS MULTIPLIER IN INPUT FILE

write_csv(parameters, "heemod data/parameters.csv")


#############################################
#############################################
#### SECTION 2 - SET UP TREATMENT GROUP #####
#############################################
#############################################

plan(multisession(workers = 4)) # Requirement for the model to use parallel-processing on DHSC laptop.7


bmi_simulation <- function(age_range, input_kcal, filename = "treat.csv") {
  # Generates the treatment BMI trajectories.
  #
  # Args:
  #   age_range: vector of ages to run the model on.
  #   input_kcal: data.table of calorie reductions split into age/sex groups
  #   filename: name for the file saving the BMI trajectories for the heemod analysis.
 
   source("src/Equations/hall eqn.R") # Loads the Hall formulas.
  
  hse <- read_csv("data/heemod_master_files/counterfactual.csv") %>%
    filter(initial_age %in% 18:max(age_range)) # LOAD COUNTERFACTUALS AND FILTER TO AGE RANGE OF INTEREST
  
  ages_below_18 <- c() # EMPTY VECTOR TO LIST UNDER 18 WHO ARE NEEDED
  
  if(any(age_range<18)){ # Case when we need additional 18-year olds
    
    ages_below_18 <- age_range[age_range<18] # LIST UNDER 18 NEEDED
    
  }
  
  read_csv("data/heemod_master_files/demographics.csv") %>%
    filter(initial_age %in% age_range) %>% # Write the correct demographic file
    write_csv("heemod data/demographics.csv")
  
  # Writing just the counterfactual that we want.
  #-----------------------------------------------------
counterfactual <- hse %>% 
    filter(age>=initial_age, age==round(age,0)) 

eighteen_year_olds <- counterfactual %>%
  filter(initial_age == 18) # EXTRACT THE COUNTERFACTUAL COHORT WITH INITIAL AGE OF 18

# MAP THE TREATMENT TRAGECTORY OF PEOPLE WITH INITIAL AGE 18 ONTO ALL MODEL COHORTS WITH AN INITIAL AGE LESS THAN 18
additional_eighteen_year_olds <- map_dfr(ages_below_18 , function(age_year_zero) {
  eighteen_year_olds %>%
    mutate(initial_age = age_year_zero) 
}) 

bind_rows(counterfactual, additional_eighteen_year_olds) %>%
    select(age, initial_age, sex, cut_bmi, bmi) %>%
    write_csv("heemod data/additional params/counterfactual.csv")
  
  # CALCULATE THE BMI OF THE TREATMENT GROUP
  ############################################################################
    treatment_population <- hse %>% filter(initial_age >=18) %>%
      split(list(hse$sex, hse$cut_bmi, hse$initial_age)) # Split our population into a list of tibbles, 1 for each age/sex/bmi_cat group
    
  treatment_group <-
    future_map_dfr(treatment_population, function(individual) {
      # iterating over individual groups within the population, generating BMI tables for treatment group
      # 'individual' is a tibble of 1 age/sex/bmi_cat
      with(individual, { # Allowing us to use the column names of individual as local varaibles.
        
        if (nrow(individual) == 0) {
          return(NULL) # The above split sometimes returns empty groups, in which case, siltently return NULL.
        }
        
        ref_intake <- approxfun(age, I, rule = 2) # FUNCTION TO GIVE THE REFERENCE INTAKE FOR EACH AGE/SEX/BMI_CAT GROUP
        
        sex <- first(individual$sex) # WHAT IS THE SEX OF THE GROUP
        
        init_age <- first(initial_age) # WHAT IS THE INITIALL AGE OF THE GROUP
        
        age_category <- case_when( # WHAT AGE CATEGORY IS THE COHORTS INITIAL AGE IN
          init_age < 11 ~ 1,
          init_age < 19 ~ 2,
          init_age < 65 ~ 3,
          TRUE          ~ 4
        )
        
        treatment_kcal <- input_kcal %>% # PULL KCAL REDUCTION FOR THAT COHORT
          filter(sex==Sex, age_cat==age_category) %>%
          pull(kcal)
        
        intake <- function(age, ..) { # FUNCTION TO REDUCE KCAL INTAKE
          ref_intake(age) - if_else(age > init_age,treatment_kcal, 0)
        }
        
        intake %>%
          simulate(sex, init_age, max_age = 101, tick = 1/12) %>% # APPLY HALL EQUATIONS TO COHORT TO CALCULATE BMI
          # mutate(I = intake(age)) %>%
          mutate(initial_age = init_age) %>%
          mutate(sex = first(sex)) %>%
          mutate(cut_bmi = first(cut_bmi))
      })
    }, .progress = T) %>%
    filter(age == round(age,0)) %>%
    select(age, bmi, initial_age, sex, cut_bmi)
  
  # Handling the kids who are born into the model
  ################################################################################
  
  eighteen_year_olds <- treatment_group %>%
    filter(initial_age == 18) # EXTRACT THE TREATMENT COHORT WITH INITIAL AGE OF 18
  
  # MAP THE TREATMENT TRAGECTORY OF PEOPLE WITH INITIAL AGE 18 ONTO ALL MODEL COHORTS WITH AN INITIAL AGE LESS THAN 18
   additional_eighteen_year_olds <- map_dfr(ages_below_18 , function(age_year_zero) {
     eighteen_year_olds %>%
       mutate(initial_age = age_year_zero) 
   }) 
   
   if(nrow(additional_eighteen_year_olds) > 0){
     bind_rows(treatment_group, additional_eighteen_year_olds) %>%
       write_csv("heemod data/additional params/treat.csv")
   }
   else{
    write_csv(treatment_group, "heemod data/additional params/treat.csv")
  }
  return(NULL)
}
  
start_bmi_simulation <- as.numeric(Sys.time()) 

  bmi_simulation(age_range,kcal)
  
  end_bmi_simulation <- as.numeric(Sys.time())
  
  
######################################
######################################
### section 3 - run calorie model ####
######################################
######################################
  
  start_model <- as.numeric(Sys.time())
  
  model <- run_model_tabular("heemod data/") # RUN MODEL

  end_model <- as.numeric(Sys.time())
  
  # ADD TIMING AND SETUP PARAMETERS TO THE MODEL FILE
  #-------------------------------------------------------------------
  model$timings <- data.frame('Modelling stage' = c('HSE readin',
                                                    'Treatment cohort setup',
                                                    'Markov model run'),
                              'Time elapsed' = c('test',
                                                 paste(-round((start_bmi_simulation - end_bmi_simulation)/60,2),'mins'),
                                                 paste(-round((start_model - end_model)/60,2),'mins')))
  model$kcal <- kcal 
  
  model$name <- model_name
  
  model$nhs_multiplier <- nhs_multiplier

  model$policy_lifetime <- policy_lifetime
  
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
                      '\nNow delete the `data` sub folder and archive the current working directary in the COP area of the shared drive to maintain an audit trail of your model run.', # REMIND USER TO ARCHIVE 
                      '\n',
                      '\nA summary report on the model, called ',
                      model$name,' - ',system_time,'.html, can be found at ',
                      getwd(),'/report',
                     '\n',
                     '\nThe full model, called ',
                      model$name,' - ',system_time,'.rds, can be found at ',
                      getwd(),'/model',
                     '\n',
                     '\nThank you for using the DHSC calorie model'), 
                     'yesno')$res 
  }
  