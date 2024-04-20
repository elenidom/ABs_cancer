#######################################################################################
#                                           *
#                                   ABs/cancer PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                 JANUARY 2019 - citylabs
#                                           *
#
# The following lines should be run 1st before any other analysis. There are
# initial checks and commands on myPERSON2019 dataset preparation:
#
#######################################################################################

library(dplyr)

# read the initial file that has been stored as "pers_elini.csv"
myPERSON2019 <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_elini.csv")

breast <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_breast.csv")
colorectal <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_colorectal.csv")
kidney <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_kidney.csv")
leukemia <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_leukemia.csv")
lung <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_lung.csv")
lymphoma <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_lymphoma.csv")
melanoma <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_melanoma.csv")
myeloma <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_myeloma.csv")
oesophagus <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_oesophagus.csv")
ovary <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_ovary.csv")
pancreas <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_pancreas.csv")
prostate <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_prostate.csv")
stomach <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_stomach.csv")
urinary <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_urinary.csv")
uterus <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/pers_uterus.csv")

# link myPERSON2019 dataset with the rest of cancer datasets
prepare_initial_myPERSON2019 <- function(){
  mycancers <- full_join(myPERSON2019, breast, by="patid2") %>%
    full_join(., colorectal, by="patid2") %>%
    full_join(., kidney, by="patid2") %>%
    full_join(., leukemia, by="patid2") %>%
    full_join(., lung, by="patid2") %>%
    full_join(., lymphoma, by="patid2") %>%
    full_join(., melanoma, by="patid2") %>%
    full_join(., myeloma, by="patid2") %>%
    full_join(., oesophagus, by="patid2") %>%
    full_join(., ovary, by="patid2") %>%
    full_join(., pancreas, by="patid2") %>%
    full_join(., prostate, by="patid2") %>%
    full_join(., stomach, by="patid2") %>%
    full_join(., urinary, by="patid2") %>%
    full_join(., uterus, by="patid2")
  
  # keep your 1st clean dataset as follows:
  myPERSON2019 <- mycancers[, !(names(mycancers) %in% temp_drop19)]
  
  return(myPERSON2019)
}
# call the above function:
myPERSON2019 <- prepare_initial_myPERSON2019()

# check if cancers from all the extra csv files exist in myPERSON2019 file:
checks_initial <- function(){
  check_cancers_in_myperson <- function(dataset1, cancer_type){
    cancer_type1 <- eval(parse(text = cancer_type))
    sum1 <- sum(dataset1$patid2 %in% cancer_type1$patid2)
    printthis <- paste(cancer_type, sum1, sep=": ")
  }
  
  all_cancer_types <- c("breast","colorectal","kidney","leukemia","lung","lymphoma","melanoma",
                        "myeloma","oesophagus","ovary","pancreas","prostate","stomach",
                        "urinary","uterus")
  
  for(i in 1:length(all_cancer_types)){
    print(check_cancers_in_myperson(myPERSON2019, all_cancer_types[i]))
  }
  
  finalsum <- 0
  for(i in 1:length(all_cancer_types)){
    finalsum <- sum(myPERSON2019[all_cancer_types])
  }
  paste("FINAL SUM =", finalsum)
}
# function's call:
checks_initial()

# link the AB and myPERSON2019 datasets and then define exposure groups:
link_AB_with_myPERSON2019 <- function(){
  
  lastabexp <- myAB %>% group_by(patid2) %>% summarise(max(datepres))
  
  #change column names
  names(lastabexp) <- c("patid2", "lastday")
  
  summary(lastabexp)# min = -3704, max = 21173
  sum(lastabexp$lastday < 0) # 1
  
  # link the variable "lastday" into myPERSON2019
  myPERSON2019$lastabexp <- lastabexp$lastday
  
  # subtract the index date from the above column to calculate the days from the last exposure before the ID
  myPERSON2019$abdifflastid <- myPERSON2019$dtindex - myPERSON2019$lastabexp
  
  # convert days into months
  myPERSON2019$m_exp <- round(myPERSON2019$abdifflastid/30, 3)
  # summary(myPERSON2019$m_exp)
  # boxplot(myPERSON2019$m_exp)
  # sum(myPERSON2019$m_exp < 0) # 0
  
  # define exposure
  myPERSON2019$exposure_stage <- 0
  myPERSON2019$exposure_stage <- ifelse(myPERSON2019$m_exp <= 3 & myPERSON2019$m_exp >= 0, 2, myPERSON2019$exposure_stage)
  myPERSON2019$exposure_stage <- ifelse(myPERSON2019$m_exp <= 24 & myPERSON2019$m_exp > 3, 1, myPERSON2019$exposure_stage)
  myPERSON2019$exposure_stage <- ifelse(myPERSON2019$m_exp > 24, 0, myPERSON2019$exposure_stage)
  myPERSON2019$exposure_stage <- as.factor(myPERSON2019$exposure_stage)
  
  return(myPERSON2019)
}

# call the function to update the myPERSON2019 dataset with cancer patients that have been assigned to
# each of the 3 ABs exposure groups (variable name: "exposure_stage")
myPERSON2019 <- link_AB_with_myPERSON2019()



# The following function first replaces every NA value in the columes with cancers with 0, and then
# calculates the number of patients have different number of cancers:
recuring_cancers <- function(){
 
  # keep a dataframe with the names of cancers of interest:
  caname <- c("breast", "colorectal", "kidney", "leukemia", "lung", "lymphoma", 
              "melanoma", "myeloma", "oesophagus", "ovary", "pancreas", "prostate", 
              "stomach", "urinary", "uterus")
  
  # identify which column numbers in myPERSON2019 dataset have the above names:
  which(colnames(myPERSON2019) %in% caname)
  
  # remove NAs with zero only for the columns that include cancers
  myPERSON2019[,112:126][is.na(myPERSON2019[,112:126])] = 0
  
  # sum the number of cancers per row
  myPERSON2019$cancer_per_patient <- apply(myPERSON2019[-c(1:111,127:130)], MARGIN = 1, sum) 
  
  # return the whole dataset on function calling
  return(myPERSON2019)
}
# call the function
myPERSON2019 <- recuring_cancers()

# remove patiets with cancers other than cancers of interest (-80,100; 203,261 remain)
myPERSON2019 <- myPERSON2019[myPERSON2019$cancer_per_patient != 0, ]

# remove patients without follow-up or with death date < index date
diff_death_dindex <- function(){
  # convert dtindex.x into a numeric one to be able to make calculations:
  myPERSON2019$dtindex.x <- as.numeric(myPERSON2019$dtindex.x)
  
  sum(myPERSON2019$dtindex.x > myPERSON2019$deathdate, na.rm=TRUE)
  
  # substitute NAs in deathdate with zero
  myPERSON2019[,"deathdate"][is.na(myPERSON2019[,"deathdate"])] <- 0
  
  # create a flag with the following values:
  # [1] -- if death date is greater than index date
  # [2] -- if death date is smaller than index date
  # [0] -- if death date is zero (zero instead of NAs)
  myPERSON2019$death_index_flag <- 0
  myPERSON2019$death_index_flag <- ifelse(myPERSON2019$deathdate - myPERSON2019$dtindex.x > 0, 
                                          1, myPERSON2019$death_index_flag)
  myPERSON2019$death_index_flag <- ifelse(myPERSON2019$deathdate - myPERSON2019$dtindex.x < 0, 
                                          2, myPERSON2019$death_index_flag)
  myPERSON2019$death_index_flag <- ifelse(myPERSON2019$deathdate - myPERSON2019$dtindex.x < 0 &
                                            myPERSON2019$deathdate == 0, 
                                          0, myPERSON2019$death_index_flag)
  myPERSON2019$death_index_flag <- as.factor(myPERSON2019$death_index_flag)
  table(myPERSON2019$death_index_flag)
  
  # remove the rows where the flag is equal to zero:
  myPERSON2019 <- myPERSON2019[myPERSON2019$death_index_flag != 2, ]
  
  return(myPERSON2019)
}
myPERSON2019 <- diff_death_dindex()

# remove patients with BMI < 8
remove_insuf_bmi <- function(){
  # check how many patients have BMI < 8
  print(sum(myPERSON2019$bmi <8, na.rm=TRUE))
  
  # filter the data
  myPERSON2019 <- myPERSON2019 %>%
    filter(bmi>8 | is.na(bmi))
  
  # check the distribution of the new BMI
  hist(myPERSON2019$bmi)
  
  # return the updated dataset
  return(myPERSON2019)
}
myPERSON2019 <- remove_insuf_bmi()

# remove patients by applying the following criteria:
# prostate & women || uterus & men || ovary & men
remove_patients_with_wrong_gender <- function(){
  # filter the data
  df1 <- myPERSON2019 %>%
    filter(prostate == 1 & gender == 2)
  
  df2 <- myPERSON2019 %>%
    filter(uterus == 1 & gender == 1)
  
  df3 <- myPERSON2019 %>%
    filter(ovary == 1 & gender == 1)

  # add the above datasets that contains all the uneeded observations
  df <- union(df1, df2)
  df <- union(df, df3)

  # remove them from your initial dataset
  dfinal <- setdiff(myPERSON2019, df)

  # return the updated dataset that has been reduced according to the above criteria
  return(dfinal)
}
myPERSON2019 <- remove_patients_with_wrong_gender()


# calculating missingness for myPERSON2019 dataset
# summary(myPERSON2019)
# the summary output shows that I have missing values for the variables: 
# bmi, smoking, smokingoex, tod and numcigs_perday
#
# Here, I am calculating the persentage of missingness:
check_missingness <- function(){
  
  # create a data frame with the variables you want to calculate missingness
  miss_per_var <- c("bmi", "smoking", "smokingnoex", "tod", "numcigs_perday")
  
  # create a subset that keeps only the variables with NAs
  subdataset <- myPERSON2019[miss_per_var]
  
  # check if there are NAs values per variable and count them
  # sapply = applies the embedded function on the given subdataset
  isNA <- sapply(subdataset, function(x) sum(is.na(x)))
  
  # save the number of rows of your dataset
  rows <- nrow(subdataset)
  
  # calculate missingness and round on the 2nd decimal
  missper <- round(isNA/rows * 100, 2)  
  
  # save the results on a variable
  results <- paste("Missingness for ", miss_per_var, " = ", missper, "%")
  
  # print the results on separated lines in your screen
  cat(results, sep = "\n")
}
check_missingness()


# Create all the new variables you need. Every new variable starts with the word "my".
# The function requires the dataset as an argument:
add_new_variables <- function(myPERSON){
  
  # if BMI = NA, replace with zero, otherwise 1
  myPERSON$bmi_missing <- 0
  myPERSON$bmi_missing <- ifelse(is.na(myPERSON$bmi), 0, 1)
  
  # if smoking = NA, replace with zero, otherwise 1
  myPERSON$smoking_missing <- 0
  myPERSON$smoking_missing <- ifelse(is.na(myPERSON$smoking), 0, 1)
  
  # converts date of index from days format into date
  myPERSON$myindexdate <- as.Date(myPERSON$dtindex, origin="1960-01-01")
  
  # the following lines make the same for the rest of the variables that need conversion
  # from days into dates
  myPERSON$myfdate_abrx <- as.Date(myPERSON$fdate_abrx, origin="1960-01-01")
  myPERSON$myfdate_cancer <- as.Date(myPERSON$fdate_cancer, origin="1960-01-01")
  myPERSON$myfdate_cancer_year <- as.numeric(format(myPERSON$myfdate_cancer, format="%Y"))
  myPERSON$mydatevalid <- as.Date(myPERSON$dtvalid, origin="1960-01-01")
  myPERSON$mydatecens <- as.Date(myPERSON$dtcens, origin="1960-01-01")
  myPERSON$mylcd <- as.Date(myPERSON$lcd, origin="1960-01-01")
  myPERSON$myuts <- as.Date(myPERSON$uts, origin="1960-01-01")
  myPERSON$mycrd <- as.Date(myPERSON$crd, origin="1960-01-01")
  myPERSON$mydeathdate <- as.Date(myPERSON$deathdate, origin="1960-01-01")
  myPERSON$mydtcens <- as.Date(myPERSON$dtcens, origin="1960-01-01")
  
  # keeps the year from the death date for further calculations
  myPERSON$mydeathYear <- as.numeric(format(myPERSON$mydeathdate, format = "%Y"))
  
  # define the age: a) for a patient who had died and b) for someone who is alive
  myPERSON$myage <- ifelse(myPERSON$deathdate == 0, 2017-myPERSON$yob, myPERSON$mydeathYear - myPERSON$yob)
  
  myPERSON$myagediag <- myPERSON$myfdate_cancer_year - myPERSON$yob
  
  return(myPERSON)
}
# assign the function's result on myPERSON2019 dataset:
myPERSON2019 <- add_new_variables(myPERSON2019)


# Define survival as the days a patient was alive after cancer index date.
# This variable will be included in your Cox models to formulate your time-to-event variable:
# TIME = survdays; EVENT = death.
# Define also a binary indicator for death/not death:
define_survival <- function(){
  #create a variable that indicates death(1) or no death (0)
  myPERSON2019$death <- ifelse(myPERSON2019$deathdate == 0, 0, 1)
  
  # 21183 => "2017-12-30" which is the end of follow-up
  # myPERSON2019$survdays <- ifelse(myPERSON2019$death == 1, myPERSON2019$deathdate - myPERSON2019$dtindex, 21183 - myPERSON2019$dtindex)
  
  myPERSON2019$survdays <- ifelse(myPERSON2019$death == 1, myPERSON2019$deathdate - myPERSON2019$dtindex, myPERSON2019$dtcens - myPERSON2019$dtindex)
  
  # because the ID is higher than the death date... probably wrongly because this is the
  # only case that survdays could be a negative number. Otherwise the deathdate must have taken negative values, 
  # and this is not true. So, I will exclude these patients if there are.
  sum(myPERSON2019$survdays > 0) 
  
  myPERSON2019 <- myPERSON2019[myPERSON2019$survdays > 0,]
  
  return(myPERSON2019)
}
myPERSON2019 <- define_survival()
write.csv(myPERSON2019, file = "./myPERSON2019.csv")
