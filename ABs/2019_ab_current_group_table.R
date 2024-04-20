#######################################################################################
#                                           *
#                                   ABs/cancer PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                 FEBUARY 2019 - citylabs
#                                           *
#
# Produce AB table for the surrent group:
#
#######################################################################################

library(dplyr)
library(magrittr)

currentsubset_leukemia <- rescoh2[rescoh2$exposure_stage == 2 & rescoh2$leukemia == 1, ]
currentsubset_lymphoma <- rescoh2[rescoh2$exposure_stage == 2 & rescoh2$lymphoma == 1, ]
currentsubset_myeloma <- rescoh2[rescoh2$exposure_stage == 2 & rescoh2$myeloma == 1, ]
currentsubset_melanoma <- rescoh2[rescoh2$exposure_stage == 2 & rescoh2$melanoma == 1, ]
currentsubset_kidney <- rescoh2[rescoh2$exposure_stage == 2 & rescoh2$kidney == 1, ]
currentsubset_ovary <- rescoh2[rescoh2$exposure_stage == 2 & rescoh2$ovary == 1, ]
currentsubset_colorectal <- rescoh2[rescoh2$exposure_stage == 2 & rescoh2$colorectal == 1, ]
currentsubset_urinary <- rescoh2[rescoh2$exposure_stage == 2 & rescoh2$urinary == 1, ]
currentsubset_prostate <- rescoh2[rescoh2$exposure_stage == 2 & rescoh2$prostate == 1, ]

antibiotics_leu_dt <- currentsubset_leukemia %>% 
  group_by(drugsubstance) %>% count() %>% 
  arrange(desc(n)) %>% 
  as.data.frame() %T>% 
  write.csv("/mnt/iusers01/ja01/mbmhted6/shared/AB_table2019/antibiotics_leu_dt.csv")

antibiotics_lym_dt <- currentsubset_lymphoma %>% 
  group_by(drugsubstance) %>% count() %>% 
  arrange(desc(n)) %>% 
  as.data.frame() %T>%
  write.csv("/mnt/iusers01/ja01/mbmhted6/shared/AB_table2019/antibiotics_lym_dt.csv")
  
antibiotics_mye_dt <- currentsubset_myeloma %>% 
  group_by(drugsubstance) %>% count() %>% 
  arrange(desc(n)) %>% 
  as.data.frame() %T>%
  write.csv("/mnt/iusers01/ja01/mbmhted6/shared/AB_table2019/antibiotics_mye_dt.csv")

antibiotics_mel_dt <- currentsubset_melanoma %>% 
  group_by(drugsubstance) %>% count() %>% 
  arrange(desc(n)) %>% 
  as.data.frame() %T>%
  write.csv("/mnt/iusers01/ja01/mbmhted6/shared/AB_table2019/antibiotics_mel_dt.csv")


