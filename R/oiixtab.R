#' A cross-tabulation with measures of association
#'
#' This function prints a simple cross-tabulation
#' and, optionally, various measures of association
#' @param r the row variable, a numeric vector
#' @param c the column variable, a numeric vector
#' @param row Show row percentages? Defaults to FALSE
#' @param col Show column percentages? Defaults to FALSE
#' @param stats Print measures of association? Defaults to FALSE
#' @param chires cell chi-square residual, pearson
#' @param chistd cell standardized chi-square residual, pearson
#' @param chiexp expected cell chi-square, pearson
#' @param ... Additional parameters to be passed to \code{\link{gmodels::CrossTable}}
#' @param warnings a logical value indicating whether warnings should be shown (defaults to FALSE, no warnings).
#' @export
#' @seealso
#' \code{\link{oii:association.measures}}, \code{\link{gmodels::CrossTable}}, \code{\link{Deducer::likelihood.test}}, \code{\link{rapport::lambda.test}}
#' @examples
#' #Create var1 as 200 A's, B's, and C's
#' var1<-sample(LETTERS[1:3],size=200,replace=TRUE)
#' #Create var2 as 200 numbers in the range 1 to 4
#' var2<-sample(1:4,size=200,replace=TRUE)
#'
#' #Print a simple cross tab of var1 and var2
#' oiixtab(var1,var2)
#'
#' #Print the row and column percents
#' oiixtab(var1,var2,row=TRUE,col=TRUE)
#' 
#' #Print measures of association statistics
#' oiixtab(var1,var2,stats=TRUE)
#'
#' #If the variables are part of a data.frame
#' my.data.frame<-data.frame(x=var1,y=var2)
#' #We can use the $ to get the variables
#' oiixtab(my.data.frame$x,my.data.frame$y)
#' #or use the with(...) command to save some typing
#' with(my.data.frame,oiixtab(x,y))
#' 
oii.xtab <-function(r, c, row=FALSE, col=FALSE, stats=FALSE, chires=FALSE, 
	chistd=FALSE, chiexp=FALSE, warnings=FALSE, ...) {

	#basic table with row percentages
	gmodels::CrossTable(r, c, missing.include=FALSE, prop.c=col, prop.r=row, digits=2,
		prop.t=FALSE, resid=chires, sresid=chistd, expected=chiexp, 
		format=c("SPSS"), ...)

	if(stats) {
		tab <- xtabs(~r+c)

      #Pearson chi squared
		if (!warnings) {
			chip <- suppressWarnings(chisq.test(tab))
		} else {
			chip <- chisq.test(tab)
		}
      
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
		print(association.measures(tab))


		#Goodman Kruskal Lambda
		cat(paste0("\n","Goodman-Kruskal Lambda:\n"))
		ll = rapport::lambda.test(tab)
		cat("     Row dependent:", format(round(ll$row, digits=3), nsmall=3), "\n")
		cat("  Column dependent:", format(round(ll$col, digits=3), nsmall=3), "\n")
		cat("\n")

	}
}

