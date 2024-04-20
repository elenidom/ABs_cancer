#######################################################################################
# DISSERTATION PROJECT
# ELENI DOMZARIDOU
# UNIVERSITY OF MANCHESTER
# SUMMER 2018 - citylabs
# ABrx file
#######################################################################################

myAB <- read.csv("/mnt/iusers01/ja01/mbmhted6/shared/old_pers_cancer/abrx_elini.csv")

summary(myAB)# it seems that there are no NAs but there are gaps with "":
ls(myAB)

vars_ab <- c("prodcode", "datepres", "drugsubstance", "description", "form_grouped", 
  "bnfchapter", "bnfcode", "productname", "strength", "formulation", 
  "route", "patid2")

sumAB <- 0
for(i in 1:ncol(myAB)){
  sumAB[i] <- sum(myAB[i] == "")
}

# find which column has empty values
cbind(vars_ab, sumAB)
#      vars_ab         sumAB    
# [1,] "prodcode"      "0"      
# [2,] "datepres"      "0"      
# [3,] "drugsubstance" "0"      
# [4,] "description"   "578,665" 
# [5,] "form_grouped"  "578,665" 
# [6,] "bnfchapter"    "0"      
# [7,] "bnfcode"       "2,430,920"
# [8,] "productname"   "0"      
# [9,] "strength"      "321,061" 
# [10,] "formulation"  "320,440" 
# [11,] "route"        "320,696" 
# [12,] "patid2"       "0"      

# returns the class of each column
sapply(myAB[1:12], class)
#prodcode      datepres   drugsubstance   description  form_grouped    bnfchapter       bnfcode   productname      strength 
#"integer"     "integer"      "factor"      "factor"      "factor"      "factor"      "factor"      "factor"      "factor" 
#formulation    route        patid2 
#"factor"      "factor"     "integer"

sapply(myAB[c(3,5,11)], class)
sapply(myAB[c(3,5,11)], levels)

# keeps the frequencies of every AB substance
ABtableFreq <- as.data.frame(table(myAB$drugsubstance))

# ...ordered from the most popular to less popular AB
ABtableFreq <- ABtableFreq[order(-ABtableFreq$Freq),]

# get the names of the 20 most popular ABs
popABnames <- as.data.frame(ABtableFreq[1:20,1])
names(popABnames) <- "abname"



# ------------------------------------------------------------------------------------------------------------
# The idea here is to identify in which ABs was the patient exposed to, in order to fill in the second table in
# the manuscript and to use then the tableone to calculate the frequencies based on different cancer type.

# find the last exposure (days)
library(dplyr)
library(tidyr)

aggregate_lastAB_name <- aggregate(datepres ~ patid2, FUN = max, data = myAB)

a <- dput(names(myAB))

which(colnames(myAB) %in% a)

selectedAB <- myAB[c(2, 3:6, 8:12)]

# merge the last exposure with the 
m <- right_join(aggregate_lastAB_name, selectedAB, by = c("patid2"))

m1 <- m %>% 
  group_by(datepres.x == datepres.y)

m1 <- subset(m1, m1[12]==TRUE)

m1 <- unique(m1)

#m1 <- m1[c(1,2,4,5)]

