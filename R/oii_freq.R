#' Frequency tables
#'
#' This function prints a simple frequency table
#' with totals and percentages
#' @param x input variable, (usually of class \code{\link{factor}})
#' @return
#' A \code{\link{data.frame}} with one row per each unique value of \code{x}.
#' These values of \code{x} are assigned to the \code{row.names} of the data.frame.
#' The data.frame also has rows for:
#' \item{Valid Total}{The total number of non-missing cases (i.e., \code{sum(!is.na(x))})}
#' \item{Missing}{The total number of missing/NA cases (i.e., \code{sum(is.na(x))})}
#' \item{Total}{The total number of cases (i.e., \code{length(x)})}
#' 
#' The data.frame has the following columns:
#' \item{freq}{The number of cases with this value}
#' \item{percent}{The percentage of all cases that this value represents}
#' \item{valid_percent}{The percentage of all valid (i.e., not missing) cases that this value represents}
#' \item{cum_percent}{The cumulative percentage of valid cases}
#' @export
#' @seealso
#' \code{\link{data.frame}}, \code{\link{row.names}}
#' \code{\link{is.na}},  \code{\link{length}}, \code{\link{summary}}, \code{\link{table}}
#' @examples
#' #Create var as 200 A's, B's, and C's
#' var<-sample(LETTERS[1:3],size=200,replace=TRUE)
#' 
#' #Generate a frequency table for the counts of A's, B's, and C's
#' oii.freq(var)
#' 
oii.freq<-function(x) {
	
	freq<-table(x)
	names<-c(names(freq),"Valid Total","Missing","Total")
	freq<-as.numeric(freq)
	n<-sum(!is.na(x))
	isna<-sum(is.na(x))
	percent<-round((freq/length(x))*100,2)
	valid_percent<-round((freq/n)*100,2)
	cum_percent<-cumsum(valid_percent)
	
	freq<-c(freq,n,isna,n+isna)
	percent<-c(percent,sum(percent),round(isna/length(x),2),100)
	valid_percent<-c(valid_percent,sum(valid_percent),NA,NA)
	cum_percent<-c(cum_percent,NA,NA,NA)
	
	data.frame(freq=freq,percent=percent,valid_percent=valid_percent,cum_percent=cum_percent,row.names=names)


}
