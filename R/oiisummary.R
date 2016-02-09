#' Print summary statistics for a numeric variable
#'
#' This function is designed to be like the built-in \code{\link{summary}}
#' function but include a few additional values.
#' If the input is not numeric, the built-in summary command is executed.
#' @param x a numeric vector for which summary statistics should be generated.
#' @param extended a logical value indicating whether additional statistics should be printed (see Value section). Defaults to FALSE
#' stripped before the computation proceeds (defaults to TRUE).
#' @param warnings a logical value indicating whether warnings should be shown (defaults to FALSE, no warnings).
#' @return
#' If \code{x} is not numeric, the built-in summary command is executed. 
#' If \code{x} is numeric (that is, \code{is.numeric(x)} returns TRUE), then a list with the following elements is returned:
#'   \item{cases}{The number of non-missing values in \code{x} (Valid N)}
#'   \item{na}{The number of missing values in \code{x} (Missing N).}
#'   \item{mean}{The mean value of \code{x} after missing values are removed. See \code{\link{mean}}}
#'   \item{sd}{The standard deviation for values in \code{x}. See \code{\link{sd}}}
#'   \item{min}{The minimum/smallest value in \code{x}. See \code{\link{min}}}
#'   \item{max}{The maximum/largest value in \code{x}. See \code{\link{max}}}
#' {}
#' This function also calculates the following statistics, but these are not printed by default unless extended is set to TRUE
#'   \item{var}{The variance of \code{x} after missing values are removed. See \code{\link{var}}}
#'   \item{median}{The median value of \code{x} after missing values are removed. See \code{\link{median}}}
#'   \item{p25}{The 25th percentile of \code{x} after missing values are removed}
#'   \item{p75}{The 75th percentile of \code{x} after missing values are removed}
#'   \item{skewness}{The skewness coefficient for \code{x} after missing values are removed. See \code{\link[rapport]{skewness}}}
#'   \item{kurtosis}{The kurtosis coefficient for \code{x} after missing values are removed. See \code{\link[rapport]{kurtosis}}}
#' @export
#' @seealso
#' \code{\link{summary}}, \code{\link{min}}, \code{\link{median}}, \code{\link{mean}}, \code{\link{max}}, \code{\link{sd}},
#' \code{\link{is.na}}, \code{\link{is.numeric}}, \code{\link[rapport]{skewness}}, \code{\link[rapport]{kurtosis}}, \code{\link{var}}
#' @examples
#'
#' #Generate data from a normal distribution with mean 0 and sd 1
#' #store the result in a variable called tmp
#' tmp<-rnorm(500,mean=0,sd=1)
#'
#' #Print the summary statistics about tmp
#' oii.summary(tmp)
#' #Print even more summary statistics about tmp
#' oii.summary(tmp,extended=TRUE)

oii.summary<-function(x,extended=FALSE,warnings=FALSE) {
	if (!is.numeric(x)) {
		return(summary(x))
	} else if (warnings) {
		return(oii.summary.stats(x,extended=extended))
	} else {
		return(suppressWarnings(oii.summary.stats(x,extended=extended)))
	}
}

oii.summary.stats<-function(x,extended=FALSE) {
	quantiles<-quantile(x,probs=c(.25,.75),na.rm=TRUE)
	vals<-list(
		varname=deparse(substitute(x,env=parent.frame())),
		mean=mean(x,na.rm=TRUE),
		median=median(x,na.rm=TRUE),
		cases=sum(!is.na(x)),
		min=min(x,na.rm=TRUE),
		max=max(x,na.rm=TRUE),
		na=sum(is.na(x)),
		p25=quantiles["25%"],
		p75=quantiles["75%"],
		sd=sd(x,na.rm=TRUE),
		skewness=tryCatch(rapportools::skewness(x, na.rm=TRUE), error=function(e) {rapport::skewness(x, na.rm=TRUE)}),
		kurtosis=tryCatch(rapportools::kurtosis(x, na.rm=TRUE), error=function(e) {rapport::kurtosis(x, na.rm=TRUE)}),
		var=var(x,na.rm=TRUE)
	)
	if (extended) {
		class(vals) <- c("oiisummaryextended","oiisummary")
	} else {
		class(vals) <- "oiisummary"
	}
	return(vals)
}

#' @export
print.oiisummary<-function(x,...,digits=3) {
	cat("\nDescriptive statistics for ",x$varname,":\n")
	cat("Valid N:        ",x$cases,"\n")  
	cat("Missing (NA's): ",x$na,"\n")	
	cat("Mean:           ",format(round(x$mean,digits),nsmall=digits),"\n")
	cat("SD:             ",format(round(x$sd,digits),nsmall=digits),"\n")	
	cat("Min.:           ",format(round(x$min,digits),nsmall=digits),"\n")
	cat("Max.:           ",format(round(x$max,digits),nsmall=digits),"\n\n")
}

#' @export
print.oiisummaryextended<-function(x,...,digits=3) {
	cat("\nDescriptive statistics for ",x$varname,":\n")
	cat("Valid N:        ",x$cases,"\n")  
	cat("Missing (NA's): ",x$na,"\n")	
	cat("Mean:           ",format(round(x$mean,digits),nsmall=digits),"\n")
	cat("SD:             ",format(round(x$sd,digits),nsmall=digits),"\n")	
	cat("Variance:       ",format(round(x$var,digits),nsmall=digits),"\n\n")
	
	cat("Min.:           ",format(round(x$min,digits),nsmall=digits),"\n")
	cat("25th percentile:",format(round(x$p25,digits),nsmall=digits),"\n")
	cat("Median:         ",format(round(x$median,digits),nsmall=digits),"\n")
	cat("75th percentile:",format(round(x$p75,digits),nsmall=digits),"\n")
	cat("Max.:           ",format(round(x$max,digits),nsmall=digits),"\n\n")
	
	cat("Skewness:       ",format(round(x$skewness,digits),nsmall=digits),"\n")
	cat("Kurtosis:       ",format(round(x$kurtosis,digits),nsmall=digits),"\n\n")
}

