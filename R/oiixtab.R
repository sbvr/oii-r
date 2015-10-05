#' A cross-tabulation with measures of association
#'
#' This function prints a 2-way table with optional cell statistics 
#' and measures of association
#' @param r the row variable
#' @param c the column variable
#' @param row Show row percentages? Defaults to FALSE
#' @param col Show column percentages? Defaults to FALSE
#' @param stats Print measures of association? Defaults to FALSE
#' @param expcell Print expected cell count under the null hypothesis? Defaults to FALSE
#' @param rescell Print residual cell count under the null hypothesis?  Defaults to FALSE
#' @param pctcell Print cell percentages? Defaults to FALSE
#' @param chicell Print cell contribution to pearson chi-square? Defaults to FALSE
#' @param chistd Print cell standardized residuals to pearson chi-square? Defaults to FALSE
#' @param ... Additional parameters to be passed to \code{\link[gmodels]{CrossTable}}
#' @param warnings a logical value indicating whether warnings should be shown (defaults to FALSE, no warnings).
#' @export
#' @seealso
#' \code{\link{association.measures}}, \code{\link[gmodels]{CrossTable}}, \code{\link[Deducer]{likelihood.test}}, \code{\link[rapport]{lambda.test}}
#' @examples
#' #Create var1 as 200 A's, B's, and C's
#' var1<-sample(LETTERS[1:3],size=200,replace=TRUE)
#' #Create var2 as 200 numbers in the range 1 to 4
#' var2<-sample(1:4,size=200,replace=TRUE)
#'
#' #Print a simple 2-way table of var1 and var2
#' oii.xtab(var1,var2)
#'
#' #Print the row and column percents
#' oii.xtab(var1,var2,row=TRUE,col=TRUE)
#' 
#' #Print measures of association statistics
#' oii.xtab(var1,var2,stats=TRUE)
#'
#' #If the variables are part of a data.frame
#' my.data.frame<-data.frame(x=var1,y=var2)
#' #We can use the $ to get the variables
#' oii.xtab(my.data.frame$x,my.data.frame$y)
#' #or use the with(...) command to save some typing
#' with(my.data.frame,oii.xtab(x,y))
#' 
oii.xtab <-function(r, c, row=FALSE, col=FALSE, pctcell=FALSE, stats=FALSE, rescell=FALSE, 
	chistd=FALSE, expcell=FALSE, chicell=FALSE, warnings=FALSE, ...) {

	#basic table with row percentages
	gmodels::CrossTable(r, c, missing.include=FALSE, prop.c=col, prop.r=row, digits=2,
		prop.t=pctcell, resid=rescell, sresid=chistd, expected=expcell, prop.chisq=chicell,
		chisq=FALSE, format=c("SPSS"), ...)

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
		options(DeducerNoGUI=TRUE) #Do not try to add menus
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

