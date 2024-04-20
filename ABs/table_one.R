###################################################################################################################
#                                           *
#                                   ABs/cancer PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                 JANUARY 2019 - citylabs
#                                           *
#
# The following lines should be run on a 2nd stage after the code on file 'myPERSON2019_1.R to produce tableOne:
# 
####################################################################################################################

library(dplyr)

# Define new variables before running the tableOne function:
preparation_before_tableOne <- function(myPERSON){
  
  # create the factor to_myage for the variable "age at diagnosis" that will be included in the baseline table
  myPERSON$to_myage <- 0
  myPERSON$to_myage <- ifelse(myPERSON$myagediag >= 0 & myPERSON$myagediag <= 18, "[0-18]", myPERSON$to_myage)
  myPERSON$to_myage <- ifelse(myPERSON$myagediag > 18 & myPERSON$myagediag <= 30, "[19-30]", myPERSON$to_myage)
  myPERSON$to_myage <- ifelse(myPERSON$myagediag > 30 & myPERSON$myagediag <= 45, "[31-45]", myPERSON$to_myage)
  myPERSON$to_myage <- ifelse(myPERSON$myagediag > 45 & myPERSON$myagediag <= 55, "[46-55]", myPERSON$to_myage)
  myPERSON$to_myage <- ifelse(myPERSON$myagediag > 55 & myPERSON$myagediag <= 65, "[56-65]", myPERSON$to_myage)
  myPERSON$to_myage <- ifelse(myPERSON$myagediag > 65 & myPERSON$myagediag <= 75, "[66-75]", myPERSON$to_myage)
  myPERSON$to_myage <- ifelse(myPERSON$myagediag > 75, "[ > 75]", myPERSON$to_myage)
  myPERSON$to_myage <- as.factor(myPERSON$to_myage)
  
  # summary(myPERSON$to_myage)
  
  # split the years of follow-up according to the dureefol variable (expressed in days)
  myPERSON$to_dfu <- 0
  myPERSON$to_dfu <- ifelse(myPERSON$dureefol >= 0 & myPERSON$dureefol < 1096, "[0-3]", myPERSON$to_dfu)
  myPERSON$to_dfu <- ifelse(myPERSON$dureefol >= 1096 & myPERSON$dureefol <= 3650, "[3-10]", myPERSON$to_dfu)
  myPERSON$to_dfu <- ifelse(myPERSON$dureefol > 3650 & myPERSON$dureefol <= 365*15, "[10-15]", myPERSON$to_dfu)
  myPERSON$to_dfu <- ifelse(myPERSON$dureefol > 365*15 & myPERSON$dureefol <= 365*20, "[15-20]", myPERSON$to_dfu)
  myPERSON$to_dfu <- ifelse(myPERSON$dureefol > 365*20, "[ > 20]", myPERSON$to_dfu)
  myPERSON$to_dfu <- as.factor(myPERSON$to_dfu)
  
  # converts smoking to factor
  myPERSON$to_smoking <- NA
  myPERSON$to_smoking <- ifelse(myPERSON$smoking == 0, "non-smoker", myPERSON$to_smoking)
  myPERSON$to_smoking <- ifelse(myPERSON$smoking == 1, "ex-smoker", myPERSON$to_smoking)
  myPERSON$to_smoking <- ifelse(myPERSON$smoking == 2, "smoker", myPERSON$to_smoking)
  myPERSON$to_smoking <- ifelse(is.na(myPERSON$smoking), "not recorded", myPERSON$to_smoking)
  myPERSON$to_smoking <- as.factor(myPERSON$to_smoking)
  
  # create a factor indicating these comorbidities
  myPERSON$CVD <- ifelse(myPERSON$risk_charlson15 == 1, "CVD", 0)
  myPERSON$COPD <- ifelse((myPERSON$risk_charlson16 == 1 | myPERSON$risk_qcancer38 == 1), "COPD", 0)
  myPERSON$Diabetes <- ifelse((myPERSON$risk_charlson20 == 1 | myPERSON$risk_qcancer40 == 1 | myPERSON$risk_qcancer41 == 1), "Diabetes", 0)
  myPERSON$Moderate_severe_renal <- ifelse(myPERSON$risk_charlson27 == 1, "Moderate/severe renal", 0)
  myPERSON$MI <- ifelse((myPERSON$risk_charlson28 == 1 | myPERSON$risk_charlson17 == 1), "Heart failure", 0)
  myPERSON$Asthma <- ifelse(myPERSON$risk_qcancer33 == 1, "Asthma", 0)
  
  # for the bmi
  myPERSON$to_bmi <- NA
  myPERSON$to_bmi <- ifelse(myPERSON$bmi >= 0 & myPERSON$bmi <= 18.5, "underweight (<18.5)", myPERSON$to_bmi)
  myPERSON$to_bmi <- ifelse(myPERSON$bmi > 18.5 & myPERSON$bmi <= 25, "normal (18.5-25)", myPERSON$to_bmi)
  myPERSON$to_bmi <- ifelse(myPERSON$bmi > 25 & myPERSON$bmi <= 30, "overweight (25-30)", myPERSON$to_bmi)
  myPERSON$to_bmi <- ifelse(myPERSON$bmi > 30, "obese ( > 30)", myPERSON$to_bmi)
  myPERSON$to_bmi <- ifelse(is.na(myPERSON$bmi), "not recorded", myPERSON$to_bmi)
  myPERSON$to_bmi <- as.factor(myPERSON$to_bmi)
  
  # for race
  race_names <- c("BANGLADESH","CHINESE","INDIAN","OTH_ASIAN","PAKISTANI", "BLACK", "MIXED")
  myPERSON$to_race_gprd <- "Not recorded"
  myPERSON$to_race_gprd <- ifelse(myPERSON$race_gprd %in% race_names, "Non-white", myPERSON$to_race_gprd)
  myPERSON$to_race_gprd <- ifelse(myPERSON$race_gprd == "WHITE", "White", myPERSON$to_race_gprd)
  myPERSON$to_race_gprd <- ifelse(myPERSON$race_gprd %in% c("UNKNOWN","OTHER"), "Other/Unknown", myPERSON$to_race_gprd)
  myPERSON$to_race_gprd <- as.factor(myPERSON$to_race_gprd)
  levels(myPERSON$to_race_gprd)

  # for antibiotics
  myPERSON$Antibiotics <- "Other"
  myPERSON$Antibiotics <- ifelse(myPERSON$drugsubstance == "Amoxicillin", "Amoxicillin", myPERSON$Antibiotics)
  myPERSON$Antibiotics <- ifelse(myPERSON$drugsubstance == "Flucloxacillin", "Flucloxacillin", myPERSON$Antibiotics)
  myPERSON$Antibiotics <- ifelse(myPERSON$drugsubstance == "Erythromycin", "Erythromycin", myPERSON$Antibiotics)
  myPERSON$Antibiotics <- ifelse(myPERSON$drugsubstance == "Cefalexin", "Cefalexin", myPERSON$Antibiotics)
  myPERSON$Antibiotics <- ifelse(myPERSON$drugsubstance == "Phenoxymethylpenicillin", "Phenoxymethylpenicillin", myPERSON$Antibiotics)
  myPERSON$Antibiotics <- as.factor(myPERSON$Antibiotics)

  # rename some of your columns in order to have them in an appropriate format in your tableOne output:
  colnames(myPERSON)[which(names(myPERSON) == "gender")] <- "Gender"
  colnames(myPERSON)[which(names(myPERSON) == "to_myage")] <- "Age"
  colnames(myPERSON)[which(names(myPERSON) == "to_dfu")] <- "Followup"
  colnames(myPERSON)[which(names(myPERSON) == "to_smoking")] <- "Smoking_status"
  colnames(myPERSON)[which(names(myPERSON) == "to_bmi")] <- "BMI"
  colnames(myPERSON)[which(names(myPERSON) == "to_race_gprd")] <- "Race"
  
  myPERSON$Gender <- as.factor(myPERSON$Gender)
  
  return(myPERSON)
}
myPERSON2019_to <- preparation_before_tableOne(rescoh) # define your datasource 

# rescoh <- preparation_before_tableOne(rescoh)

# Produce TableOne
# Use the tableone package to create the baseline tables for every cancer:
library(tableone)

to_output <- function(can_name){
  
  # myvars = a vector that keeps only the variables I need to create the baseline table
  myvars <- c("myagediag", "Gender", "Diabetes", "COPD", "Asthma", "Moderate_severe_renal", "CVD", "Heart failure")
  
  # convert the given string (can_name) into an R object
  temp_var1 <- eval(paste0("mytableOne_", can_name))
  
  # produce TableOne
  temp_var2 <- CreateTableOne(vars = myvars, strata = can_name, data = myPERSON2019_to)
  
  # print it on your screen to be able to save it on the next step
  temp_var2
  
  # save the output in the current working directory, within the folder 'tableone2019'
  capture.output(temp_var2, file = paste0("/mnt/iusers01/ja01/mbmhted6/shared/tableone2019/", temp_var1, ".csv"))
}

# take cancer names:
# Tailor the cancers_names vector appropriatelly:
cancers_names <- c("leukemia", "lymphoma", "myeloma", "melanoma", "kidney", "ovary",
                   "urinary", "colorectal", "uterus", "breast", "prostate")

# Call the to_output function multiple times, by selecting the cancers of your interest.
for(i in 1:length(cancers_names)){
  to_output(cancers_names[i])
}
