#######################################################################################
#                                           *
#                                   ABs/cancer PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                 JANUARY 2019 - citylabs
#                                           *
#
#   ATTENTION!!!
# ----------------
# The following line restricts your cohort between 2000-2017.
#
#######################################################################################


rescoh <- myPERSON2019[myPERSON2019$dtindex >= 14610,]

# check how many patients had at lest 3 years of follow-up before index date (fubid):
rescoh$fubid <- rescoh$dtindex - rescoh$crd

# check it
summary(rescoh$fubid)
# ------------------------------------------------
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1096    4456    7225    8669   11510   36330 
# ------------------------------------------------
# So we are ok, as the min difference which is 1096/365 = 3 years: the smallest acceptable follow-up 

# add a variable to represent years instead of days for your KM plots:
rescoh$survyears <- round((rescoh$survdays/365), 2)
# hist(rescoh$survyears)
# summary(rescoh$survyears)

# censoring indicator:
rescoh$censor <- ifelse(rescoh$dtcens != rescoh$deathdate, 1, 0)

# exclude children (age <= 12)
rescoh <- rescoh[rescoh$myagediag > 12, ]

#rescoh <- dplyr::inner_join(rescoh, m1, by = "patid2")

rescoh$dupliflag <- duplicated(rescoh$patid2)

# remove patids2 with more than 2 prescr:
rescoh <- rescoh[rescoh$dupliflag == FALSE, ]

rescoh$exposure_stage <- as.factor(rescoh$exposure_stage)
