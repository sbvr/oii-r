###########################################################
# Association measures
#
# Chi-square and ordinal measures of association
#
# Somers' d, Tau-b and Tau-c 
# Phi, Cramer's V, and the Contingency Coefficient 
#
# by Scott Hale and Grant Blank
# Oxford Internet Institute
# University of Oxford
#
# Goodman-Kruskal gamma code
# by Simon Jackman
# department of political science
# stanford university
# http://jackman.stanford.edu/classes/151B/06/class0517.r
# http://en.wikipedia.org/wiki/Goodman_and_Kruskal's_gamma
#
##########################################################

#Internal use --- if x is not a table/matrix, then make x,y table
make.table<-function(r,c=NULL,exclude=FALSE) {
	tab<-r
	if (is.data.frame(r)) {
		if (length(r)>2) {
			warning("Input is a data.frame with more than two variables. Only the first two variables will be used.")
		}
		if (exclude) {
			tab<-table(factor(r[,1]),factor(r[,2]))
		} else {
			tab<-table(r[,1],r[,2])
		}
	} else if(!is.table(tab) & !is.matrix(tab)){
		if (exclude) {
			tab<-table(factor(r),factor(c))
		} else {
			tab<-table(r,c)
		}
	}
	tab
}

#' The number of concordant pairs in a table or matrix
#'
#' @param x a table or matrix if \code{y} is NULL, or a numeric vector for the row variable
#' @param y the column variable, a numeric vector used only when \code{x} is not a table or matrix.
#' @return
#' The number of concordant pairs
#' @export
#' @seealso
#' \code{\link{association.measures}}, \code{\link{discordant.pairs}}, \code{\link{tied.pairs}}
concordant.pairs <- function(x,y=NULL){
	tab<-make.table(x,y)
	## get sum(matrix values > r AND > c) 
	## for each matrix[r, c] 
	mat.lr <- function(r,c){ 
		lr <- tab[(r.x > r) & (c.x > c)] 
		sum(lr) 
	} 

	## get row and column index for each 
	## matrix element 
	r.x <- row(tab) 
	c.x <- col(tab) 

	## return the sum of each matrix[r, c] * sums 
	## using mapply to sequence thru each matrix[r, c] 
	as.numeric(sum(tab * mapply(mat.lr, r = r.x, c = c.x)))
}

#' The number of discordant pairs in a table or matrix
#'
#' @param x a table or matrix if \code{y} is NULL, or a numeric vector for the row variable
#' @param y the column variable, a numeric vector used only when \code{x} is not a table or matrix.
#' @return
#' The number of discordant pairs
#' @export
#' @seealso
#' \code{\link{association.measures}}, \code{\link{concordant.pairs}}, \code{\link{tied.pairs}}
discordant.pairs <- function(x,y=NULL){
	tab<-make.table(x,y)
	## get sum(matrix values > r AND < c) 
	## for each matrix[r, c] 
	mat.ll <- function(r,c){ 
		ll <- x[(r.x > r) & (c.x < c)] 
		sum(ll) 
	} 

	## get row and column index for each 
	## matrix element 
	r.x <- row(tab) 
	c.x <- col(tab) 

	## return the sum of each matrix[r, c] * sums 
	## using mapply to sequence thru each matrix[r, c] 
	as.numeric(sum(tab * mapply(mat.ll, r = r.x, c = c.x)))
} 

#' The number of tied pairs, a measure of association
#'
#' @param x a table or matrix if \code{y} is NULL, or a numeric vector for the first variable
#' @param y the second variable, a numeric vector used only when \code{x} is not a table or matrix.
#' @return
#' A list with the following values:
#'   \item{first}{The number of pairs tied on the first variable, but not both variables}
#'   \item{second}{The number of pairs tied on the second variable, but not both variables}
#'   \item{both}{The number of pairs tied on both the first and second variables}

#' @export
#' @seealso
#' \code{\link{association.measures}}, \code{\link{concordant.pairs}}, \code{\link{discordant.pairs}}
tied.pairs <- function(x,y=NULL) {
	
	# tied_both: choose 2 of every element in the matrix and sum the results
	tied.both<-function(tab){sum(choose(tab,2))}
	
	tied.first <- function(tab){ 
		mult<-function(r,c) {
			#print(paste(r,c))
			#print((r.x > r) & (c.x == c))
			#print(x[(r.x > r) & (c.x == c)] ) 
			lr <- x[(r.x == r) & (c.x > c)]
			tab[r,c]*sum(lr)
		}

		r.x <- row(tab) 
		c.x <- col(tab) 

		tmp<-(mapply(mult, r = r.x, c = c.x))
		#print("----------------")
		#print(tmp)
		sum(as.numeric(tmp)) #Make sure we have doubles to avoid integer overflow
	}
	
	tab<-make.table(x,y)
	list(
		both=tied.both(tab),
		first=tied.first(tab),
		#for tied_second, transpose the matrix and run the same code as tied_first
		second=tied.first(t(tab))
	)
}


#' Measures of association
#'
#' This function calculates basic measures of association
#' @param x a table or matrix if \code{y} is NULL, or a numeric vector for the row variable
#' @param y the column variable, a numeric vector used only when \code{x} is not a table or matrix.
#' @param warnings a logical value indicating whether warnings should be shown (defaults to FALSE, no warnings).
#' @return
#' A list with the following elements is returned:
#'   \item{phi}{Phi, a chi-square-based measures of association.}
#'   \item{contingency_coefficient}{Contingency coefficient, a chi-square-based measures of association.}
#'   \item{cramersv}{Cramer's V, a chi-square-based measures of association.}
#'   \item{pairs_total}{Total number of pairs}
#'   \item{pairs_concordant}{Number of concordant pairs}
#'   \item{pairs_discordant}{Number of discordant pairs}
#'   \item{pairs_tied_first}{The number of pairs tied on the first variable (but not both variables)}
#'   \item{pairs_tied_second}{The number of pairs tied on the second variable (but not both variables)}
#'   \item{pairs_tied_both}{The number of pairs tied on both the first and second variables}
#'   \item{minimum_dim}{Minimum dimension of \code{x} and \code{y}}
#'   \item{n}{Number of cases}
#'   \item{gamma}{Goodman-Kruskal Gamma}
#'   \item{somersd}{Somers' d (assuming the column variable is the dependent variable)}
#'   \item{taub}{Kendall's tau-b}
#'   \item{tauc}{Stuart's tau-c}
#' @export
#' @seealso
#' \code{\link{oii.xtab}}, \code{\link[Deducer]{likelihood.test}}, \code{\link[rapport]{lambda.test}},
#' \code{\link{concordant.pairs}}, \code{\link{discordant.pairs}}, \code{\link{tied.pairs}}
#' 
#' @examples
#' #Create var1 as 200 A's, B's, and C's
#' var1<-sample(LETTERS[1:3],size=200,replace=TRUE)
#' #Create var2 as 200 numbers in the range 1 to 4
#' var2<-sample(1:4,size=200,replace=TRUE)
#'
#' #Print a simple cross tab of var1 and var2
#' association.measures(var1,var2)
association.measures <- function(x,y=NULL,warnings=FALSE){
	
	tab<-make.table(x,y)
  
	c <- concordant.pairs(tab) 
	d <- discordant.pairs(tab)
	ties<-tied.pairs(tab)
	b <- ties$both
	f <- ties$first
	s <- ties$second
	totp <- c + d + b + f + s
	n <- sum(tab)
	m <- min(dim(tab))
	wr <- sum( rowSums(tab)^2 )
	wc <- sum( colSums(tab)^2 )
	if (!warnings) {
		xsq <- suppressWarnings(chisq.test(tab))
	} else {
		xsq <- chisq.test(tab)
	}

	gamma <- (c - d) / (c + d)
	somersd <- (c - d) / (c + d + s)
	taub <- (c - d) / sqrt((c + d + s) * (c + d + f))
	tauc <- ( 2 * m * (c - d) ) / ( n^2 * (m - 1) )

	# cat("chi-sq",xsq$statistic,"\n")
	phi = sqrt(xsq$statistic / n)
	v   = sqrt(xsq$statistic / (n * (m - 1)))
	contc = sqrt(xsq$statistic / (xsq$statistic + n))

	arg <- (c + d) / (n * (1 - (gamma^2)))
	stdError <- 1/sqrt(arg)
	z <- gamma/stdError

	vals<-list(
		phi=phi,
		contingency_coefficient=contc,
		cramersv=v,
		pairs_total=totp,
		pairs_concordant=c,
		pairs_discordant=d,
		pairs_tied_first=f,
		pairs_tied_second=s,
		pairs_tied_both=b,
		minimum_dim=m,
		n=n,
		gamma=gamma,
		somersd=somersd,
		taub=taub,
		tauc=tauc
	)
	class(vals)<-"oii.association.measures"
	vals
}

#' @export
print.oii.association.measures<-function(x,...) {
cat("Chi-square-based measures of association:\n")
	cat(paste("   Phi:                     ", format(round(x$phi, digits=3), nsmall=3),"\n"))
	cat(paste("   Contingency coefficient: ", format(round(x$contingency_coefficient, digits=3), nsmall=3),"\n"))
	cat(paste("   Cramer's V:              ", format(round(x$cramersv, digits=3), nsmall=3),"\n\n"))


	cat("Ordinal measures of association:\n")
	cat(paste("   Total number of pairs:  ",x$pairs_total,"\n"))
	cat(paste("   Concordant pairs:       ",x$pairs_concordant,"  (",round(100*x$pairs_concordant/x$pairs_total,2),"%)\n"))
	cat(paste("   Discordant pairs:       ",x$pairs_discordant,"  (",round(100*x$pairs_discordant/x$pairs_total,2),"%)\n"))
	cat(paste("   Tied on first variable: ",x$pairs_tied_first,"  (",round(100*x$pairs_tied_first/x$pairs_total,2),"%)\n"))
	cat(paste("   Tied on second variable:",x$pairs_tied_second,"  (",round(100*x$pairs_tied_second/x$pairs_total,2),"%)\n"))
	cat(paste("   Tied on both variables: ",x$pairs_tied_both,"  (",round(100*x$pairs_tied_both/x$pairs_total,2),"%)\n\n"))
#	cat(paste("   Minimum Dimension:",x$minimum_dim,"\n"))
#	cat(paste("   Total N:",x$n,"\n"))
	cat(paste("   Goodman-Kruskal Gamma:", format(round(x$gamma, digits=3), nsmall=3),"\n"))
	cat(paste("   Somers' d (col dep.): ", format(round(x$somersd, digits=3), nsmall=3),"\n"))
	cat(paste("   Kendall's tau-b:      ", format(round(x$taub, digits=3), nsmall=3),"\n"))
	cat(paste("   Stuart's tau-c:       ",  format(round(x$tauc, digits=3), nsmall=3),"\n"))
}



