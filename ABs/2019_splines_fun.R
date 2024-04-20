
#######################################################################################
#                                           *
#                                   ABs/cancer PROJECT
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                 JANUARY 2019 - citylabs
#                                           *
#
# The following lines must be used in case you have identified a significant effect
# on your models' output that you need to explore further on whether it happens due
# to ABs (number of prescriptions) or not.
#
#######################################################################################

draw_spline <- function(model_sp, cancerName){
  
  ptemp <- termplot(model_sp, se=TRUE, plot=FALSE)
  
  countab_yb1_term <- ptemp$countab_yb1

  ytemp <- countab_yb1_term$y + outer(countab_yb1_term$se, c(0,-1.96, 1.96),'*')
  
  spline_diag <- matplot(countab_yb1_term$x, exp(ytemp), 
                          log='y',type='l', lty=c(1,2,2), col=3,
                          xlab = 'number of prescriptions',
                          ylab = 'HR',
                          main = cancerName)
  #capture.output(spline_diag, file = paste0("./2019_splines/", cancerName, ".jpg"))
  
  return(spline_diag)
}

#call : draw_spline(model, "cancer_name")



