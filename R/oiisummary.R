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
#'   \item{na}{The number of missing values in \code{x} (Missing N)}
#'   \item{mean}{The mean value of \code{x} after missing values are removed}
#'   \item{sd}{The standard deviation for values in \code{x}}
#'   \item{min}{The minimum/smallest value in \code{x}}
#'   \item{max}{The maximum/largest value in \code{x}}
#' {}
#' This function also calculates the following statistics, but these are not printed by default unless extended is set to TRUE
#'   \item{median}{The median value of \code{x} after missing values are removed}
#'   \item{p25}{The 25th percentile of \code{x} after missing values are removed}
#'   \item{p75}{The 75th percentile of \code{x} after missing values are removed}
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
#' oii.summary(tmp)

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
		mean=mean(x,na.rm=TRUE),
		median=median(x,na.rm=TRUE),
		cases=sum(!is.na(x)),
		min=min(x,na.rm=TRUE),
		max=max(x,na.rm=TRUE),
		na=sum(is.na(x)),
		p25=quantiles["25%"],
		p75=quantiles["75%"],
		sd=sd(x,na.rm=TRUE)
	)
	if (extended) {
		class(vals) <- c("oiisummaryextended","oiisummary")
	} else {
		class(vals) <- "oiisummary"
	}
	return(vals)
}

print.oiisummary<-function(vals,digits=3) {
	cat("Valid N:        ",vals$cases,"\n")  
	cat("Missing (NA's): ",vals$na,"\n")	
	cat("Mean:           ",format(round(vals$mean,digits),nsmall=digits),"\n")
	cat("SD:             ",format(round(vals$sd,digits),nsmall=digits),"\n")	
	cat("Min.:           ",format(round(vals$min,digits),nsmall=digits),"\n")
	cat("Max.:           ",format(round(vals$max,digits),nsmall=digits),"\n")
}

print.oiisummaryextended<-function(vals,digits=3) {
	cat("Valid N:        ",vals$cases,"\n")  
	cat("Missing (NA's): ",vals$na,"\n")	
	cat("Mean:           ",format(round(vals$mean,digits),nsmall=digits),"\n")
	cat("SD:             ",format(round(vals$sd,digits),nsmall=digits),"\n")	
	cat("Min.:           ",format(round(vals$min,digits),nsmall=digits),"\n")
	cat("25th percentile:",format(round(vals$p25,digits),nsmall=digits),"\n")
	cat("Median:         ",format(round(vals$median,digits),nsmall=digits),"\n")
	cat("75th percentile:",format(round(vals$p75,digits),nsmall=digits),"\n")
	cat("Max.:           ",format(round(vals$max,digits),nsmall=digits),"\n")
}

