#' Print summary statistics for a numeric variable
#'
#' This function is designed to be like the built-in \code{\link{summary}}
#' function but include a few additional values.
#' If the input is not numeric, the built-in summary command is executed.
#' @param x a numeric vector for which summary statistics should be generated.
#' @param na.rm a logical value indicating whether ‘NA’ values should be
#' stripped before the computation proceeds (defaults to TRUE).
#' @param warnings a logical value indicating whether warnings should be shown (defaults to FALSE, no warnings).
#' @return
#' If \code{x} is not numeric, the built-in summary command is executed. 
#' If \code{x} is numeric (that is, \code{is.numeric(x)} returns TRUE), then a list with the following elements is returned:
#'   \item{min}{The minimum/smallest value in \code{x}}
#'   \item{median}{The median value of \code{x} after missing values are removed}
#'   \item{mean}{The mean value of \code{x} after missing values are removed}
#'   \item{max}{The maximum/largest value in \code{x}}
#'   \item{sd}{The standard deviation for values in \code{x}}
#'   \item{cases}{The number of non-missing values in \code{x} (Valid N)}
#'   \item{na}{The number of missing values in \code{x} (Missing N)}
#' @export
#' @seealso
#' \code{\link{summary}}, \code{\link{min}}, \code{\link{median}}, \code{\link{mean}}, \code{\link{max}}, \code{\link{sd}},
#' \code{\link{is.na}}, \code{\link{is.numeric}}
#' @examples
#'
#' #Generate data from a normal distribution with mean 0 and sd 1
#' #store the result in a variable called tmp
#' tmp<-rnorm(500,mean=0,sd=1)
#'
#' #Print the summary statistics about tmp
#' oiisummary(tmp)

oiisummary<-function(x,na.rm=TRUE,warnings=FALSE) {
	if (!is.numeric(x)) {
		return(summary(x))
	} else if (warnings) {
		return(oiisummary_stats(x,na.rm=na.rm))
	} else {
		return(suppressWarnings(oiisummary_stats(x,na.rm=na.rm)))
	}
}

oiisummary_stats<-function(x,na.rm=TRUE) {
	#quantiles<-quantile(x,probs=c(.25,.75),na.rm=TRUE)
	vals<-list(
		mean=mean(x,na.rm=na.rm),
		median=median(x,na.rm=na.rm),
		cases=sum(!is.na(x)),
		min=min(x,na.rm=na.rm),
		max=max(x,na.rm=na.rm),
		na=sum(is.na(x)),
		#q1=quantiles["25%"],
		#q3=quantiles["75%"],
		sd=sd(x,na.rm=na.rm)
	)
	class(vals) <- "oiisummary"
	return(vals)
}

print.oiisummary<-function(vals) {
	cat("Min.:          ",vals$min,"\n")
	#cat("1st Qu.:       ",vals$q1,"\n")
	cat("Median:        ",vals$median,"\n")
	cat("Mean:          ",vals$mean,"\n")
	#cat("3rd Qu.:       ",vals$q3,"\n")
	cat("Max.:          ",vals$max,"\n")
	cat("SD:            ",vals$sd,"\n")
	cat("Valid N:       ",vals$cases,"\n")  
	cat("Missing (NA's):",vals$na,"\n")

}

