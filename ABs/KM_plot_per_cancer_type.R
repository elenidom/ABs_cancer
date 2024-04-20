#######################################################################################
#
# ELENI DOMZARIDOU
# UNIVERSITY OF MANCHESTER
# JAN 2019 - citylabs
# Built again KM plots on exposure, including risk tables
#
#######################################################################################

library(survival)
library(ggplot2)
#install.packages("survminer")
library(survminer) # for using ggsurnplot function

# give values to the following variables - they are the arguments of the function "KM_exp_plots":
# cancer_name = give the exact name to generate surv objects (give as a character using " ", e.g: "breast")
# cancer_title = the title you want to be displayed on the top of your KM plot
# call the function 
cancer_name <- "stomach"
cancer_title <- "Stomach Cancer"

# convert all the given character variables into R objects:
obj_exp <- eval(parse(text = paste0("obj_", cancer_name, "_y")))
  
mydataset_exp <- eval(parse(text = paste0("my_", cancer_name)))
  
pdf.expfile = paste0("/mnt/iusers01/ja01/mbmhted6/shared/2019_CPRD_Analysis/KM_exp_only_pdf/", cancer_name, "_cprd.pdf")
  
KM_exp <- survfit(obj_exp ~ exposure_stage, data=mydataset_exp)

KM_exp_plots(obj_exp, mydataset_exp, pdf.expfile, KM_exp, cancer_title) # call the function

# built the function
KM_exp_plots <- function(obj_exp, mydataset_exp, pdf.expfile, KM_exp, cancer_title){
  
  # create KM plot including a risk table down on:
  gg <- ggsurvplot(KM_exp,
             title = cancer_title, 
             data = mydataset_exp,
             xlab = "time to death (years)",
             xlim = c(0,10),
             break.time.by = 2,             # breaks time in x axis by 2 years
             pval = TRUE,
             #pval.method = TRUE,
             #pval.method.coord = c(0,1),
             pval.size = 3,
             pval.coord = c(0,0.03),
             conf.int = TRUE,
             conf.inf.alpha = 0.1,
             palette = "Dark2",
             ggtheme = theme_classic(),
             #surv.median.line = "h",       # drawing a horizontal line at the median survival
             risk.table = TRUE, 
             risk.table.title = "Patients at risk",
             risk.table.y.text.col = TRUE,  # colour risk table text annotations in y axis
             risk.table.y.text = FALSE,     # show bars instead of names in text annotations
             tables.theme = theme_cleantable(),
             #cumevents = TRUE,
             tables.y.text = FALSE,
             censor.shape = "|",            # change the type of censored events
             censor.size = 1,
             fontsize = 3,                  # font size of risk table
             legend.title = "Antibiotics group",
             legend.labs = c("past", "previous", "recent")             
          )
  
  # save the plot and table in a pdf file
  # set the 'onefile=FALSE' argument to exclude the 1st blank page that ggsave produces
  ggsave(file = pdf.expfile, print(gg), onefile = FALSE, width=7.5, height=4.5) 
  
  # if you want to save only the plot without the table:
  # ggsave(file = pdf.expfile, gg$plot)
}
