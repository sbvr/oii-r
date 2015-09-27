#' A cross-tabulation with measures of association
#'
#' This function prints a simple cross-tabulation
#' and, optionally, various measures of association
#' @param r the row variable, a numeric vector
#' @param c the column variable, a numeric vector
#' @param row Show row percentages? Defaults to FALSE
#' @param col Show column percentages? Defaults to FALSE
#' @param stat Print measures of association? Defaults to FALSE
#' @param chicell Print Pearson chi-square for each cell? Defaults to FALSE
#' @param chires cell chi-square residual, pearson
#' @param chistd cell standardized chi-square residual, pearson
#' @param chiexp expected cell chi-square, pearson
#' @param warnings a logical value indicating whether warnings should be shown (defaults to FALSE, no warnings).
#' @export
#' @examples
#' oiixtab(test$rep78,test$foreign)
oiixtab <-function(r, c, row=FALSE, col=FALSE, stat=FALSE, chicell=FALSE, chires=FALSE, 
   chistd=FALSE, chiexp=FALSE, warnings=FALSE) {

   #basic table with row percentages
   gmodels::CrossTable(r, c, missing.include=FALSE, prop.c=col, prop.r=row, digits=2,
      prop.chisq=chicell, prop.t=FALSE, resid=chires, sresid=chistd, expected=chiexp, 
      format=c("SPSS"))

   if(stat==TRUE) {
      tab <- xtabs(~r+c)

      #Pearson chi squared
      chip = chisq.test(tab)
      
      cat("Pearson chi-square:         ", format(round(chip$statistic, digits=3), nsmall=3), 
         " df:", chip$parameter, 
         " p-value:", format(round(chip$p.value, digits=3), nsmall=3),
         "\n" )

      #Likelihood Ratio Chi Squared
      chilr=Deducer::likelihood.test(r,c)
      cat("Likelihood ratio chi-square:", format(round(chilr$statistic, digits=3), nsmall=3), 
         " df:", chilr$parameter, 
         " p-value:", format(round(chilr$p.value, digits=3), nsmall=3),
         "\n\n")

      #chi-sq stats and ordinal stats
      print(oii_association_measures(tab))


      #Goodman Kruskal Lambda
      cat(paste0("\n","Goodman-Kruskal Lambda:\n"))
      ll = rapport::lambda.test(tab)
      cat("     Row dependent:", format(round(ll$row, digits=3), nsmall=3), "\n")
      cat("  Column dependent:", format(round(ll$col, digits=3), nsmall=3), "\n")
      cat("\n")

   }
}

