#' A cross-tabulation with measures of association
#'
#' This function prints a 2-way table with optional cell statistics 
#' and measures of association
#' @param r the row variable. If \code{r} is a \code{\link{table}}, \code{\link{data.frame}}, or \code{\link{matrix}}, then \code{c} and \code{s} are ignored.
#' @param c the column variable.
#' @param s the split variable. The \code{r} and \code{c} will be separately tabulated for each unique value of \code{s}.
#' @param row Show row percentages? Defaults to FALSE.
#' @param col Show column percentages? Defaults to FALSE.
#' @param stats Print measures of association? Defaults to FALSE. See \code{\link{association.measures}}.
#' @param expcell Print expected cell count under the null hypothesis? Defaults to FALSE.
#' @param rescell Print residual cell count under the null hypothesis?  Defaults to FALSE.
#' @param pctcell Print cell percentages? Defaults to FALSE.
#' @param chicell Print cell contribution to pearson chi-square? Defaults to FALSE.
#' @param chistd Print cell standardized residuals to pearson chi-square? Defaults to FALSE.
#' @param ... Additional parameters to be passed to \code{\link[gmodels]{CrossTable}}.
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
#' #Three-way tables are also possible
#' #Create var3 as 200 x's, y's, and z's
#' var3<-var1<-sample(letters[24:26],size=200,replace=TRUE)
#' oii.xtab(var1,var2,var3)
#'
#' #We can also pass in a data.frame directly as the first argument
#' my.data.frame<-data.frame(x=var1,y=var2,z=var3)
#' oii.xtab(my.data.frame,stats=TRUE)
#' #The variables in the data.frame are used in order; so, sometimes it is useful to re-order them. For example,
#' oii.xtab(my.data.frame[,c("var3","var1","var2")],stats=TRUE)
#' #Of course, it is also possible to pass in the variables one at a time or use with(...) as shown above.
#' 
oii.xtab <-function(r, c=NULL, s=NULL, row=FALSE, col=FALSE, pctcell=FALSE, stats=FALSE, rescell=FALSE, 
	chistd=FALSE, expcell=FALSE, chicell=FALSE, warnings=FALSE, varnames=NULL, ...) {

	if (is.null(varnames)) {
		varnames<-c(deparse(substitute(r)),deparse(substitute(c)),deparse(substitute(s)))
	}
	#3-way tables
	if (!is.null(s) & !is.null(c)) {
		#Three variables passed separately
		r<-data.frame(r=r,c=c,s=s)
	}
	if (is.data.frame(r) & length(r)>2) {
		#Three variables in data.frame
		#split on third var
		dfs<-split(r,r[,3])
		levs<-names(dfs)
		for (l in levs) {
			cat(paste(rep("-",options("width")),collapse=""),"\n")
			cat(varnames[3],":",l,"\n")
			df<-dfs[[l]]
			oii.xtab(df[,1:2],row=row,col=col,pctcell=pctcell,stats=stats,rescell=rescell,chistd=chistd,expcell=expcell,chicell=chicell,warnings=warnings,varnames=varnames,...)
		}
		return(invisible(NULL))
	}
	
	
	if (is.null(varnames[2])) {
		cat("\nCross-tabulation of", varnames[1], "\n")
	} else {
		cat("\nCross-tabulation of", varnames[1], "(rows) and", varnames[2],"(cols)\n")
	}
	
	tab<-make.table(r,c,exclude=TRUE)

	#basic table with row percentages
	gmodels::CrossTable(tab, missing.include=FALSE, prop.c=col, prop.r=row, digits=2,
		prop.t=pctcell, resid=rescell, sresid=chistd, expected=expcell, prop.chisq=chicell,
		chisq=stats, format=c("SPSS"), ...)

	if(stats) {
		#tab <- xtabs(~r+c)

      #Pearson chi squared
		#if (!warnings) {
		#	chip <- suppressWarnings(chisq.test(tab))
		#} else {
		#	chip <- chisq.test(tab)
		#}
      
		#cat("Pearson chi-square:         ", format(round(chip$statistic, digits=3), nsmall=3), 
		#	" df:", chip$parameter, 
		#	" p-value:", format(round(chip$p.value, digits=3), nsmall=3),
		#	"\n" )

		#Likelihood Ratio Chi Squared
		options(DeducerNoGUI=TRUE) #Do not try to add menus
		chilr=Deducer::likelihood.test(tab)
		cat("Likelihood ratio chi-square:", format(round(chilr$statistic, digits=3), nsmall=3), 
			" df:", chilr$parameter, 
			" p-value:", format(round(chilr$p.value, digits=3), nsmall=3),
			"\n\n")

		#Ordinal measures of association
		print(association.measures(tab))


		#Goodman Kruskal Lambda
		cat(paste0("\n","Goodman-Kruskal Lambda:\n"))
		ll = rapport::lambda.test(tab)
		cat("     Row dependent:", format(round(ll$row, digits=3), nsmall=3), "\n")
		cat("  Column dependent:", format(round(ll$col, digits=3), nsmall=3), "\n")
		cat("\n")

	}
}

