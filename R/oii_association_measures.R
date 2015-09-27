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
makeTable<-function(x,y=NULL) {
	tab<-x
	if(!is.table(tab) & !is.matrix(tab)){
		tab<-table(x,y)
	}
	tab
}

concordant_pairs <- function(x,y=NULL){
	tab<-makeTable(x,y)
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
	sum(tab * mapply(mat.lr, r = r.x, c = c.x)) 
}

discordant_pairs <- function(x,y=NULL){
	tab<-makeTable(x,y)
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
	sum(tab * mapply(mat.ll, r = r.x, c = c.x)) 
} 

# tied_both: choose 2 of every element in the matrix and sum the results
tied_pairs <- function(x,y=NULL) {
	tied_both<-function(tab){sum(choose(tab,2))}
	
	tied_first <- function(tab){ 
		mult<-function(r,c) {
			#print(paste(r,c))
			#print((r.x > r) & (c.x == c))
			#print(x[(r.x > r) & (c.x == c)] ) 
			lr <- x[(r.x == r) & (c.x > c)]
			tab[r,c]*sum(lr)
		}

		r.x <- row(tab) 
		c.x <- col(tab) 
		#cat("r.x: ",r.x,"\n")
		#cat("c.x: ",c.x,"\n")

		## return the sum of each matrix[r, c] * sums 
		## using mapply to sequence thru each matrix[r, c] 

		tmp<-(mapply(mult, r = r.x, c = c.x))
		#print("----------------")
		#print(tmp)
		sum(tmp)
	}
	
	tab<-makeTable(x,y)
	list(
		both=tied_both(tab),
		first=tied_first(tab),
		#for tied_second, transpose the matrix and run the same code as tied_first
		second=tied_first(t(tab))
	)
}



oii_association_measures <- function(x,y=NULL,warnings=FALSE){
	
	tab<-makeTable(x,y)
  
	c <- concordant_pairs(tab) 
	d <- discordant_pairs(tab)
	ties<-tied_pairs(tab)
	b <- ties$both
	f <- ties$first
	s <- ties$second
	totp <- c + d + b + f + s
	n <- sum(tab)
	m <- min(dim(tab))
	wr <- sum( rowSums(tab)^2 )
	wc <- sum( colSums(tab)^2 )
	if (!warnings) {
		xsq <- suppressWarnings(chisq.test(x))
	} else {
		xsq <- chisq.test(x)
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

	cat("Chi-square-based measures of association:\n")
	cat(paste("   Phi:                     ", format(round(phi, digits=3), nsmall=3),"\n"))
	cat(paste("   Contingency coefficient: ", format(round(contc, digits=3), nsmall=3),"\n"))
	cat(paste("   Cramer's V:              ", format(round(v, digits=3), nsmall=3),"\n\n"))


	cat("Ordinal measures of association:\n")
	cat(paste("   Total number of pairs:  ",totp,"\n"))
	cat(paste("   Concordant pairs:       ",c,"\n"))
	cat(paste("   Discordant pairs:       ",d,"\n"))
	cat(paste("   Tied on first Variable: ",f,"\n"))
	cat(paste("   Tied on second variable:",s,"\n"))
	cat(paste("   Tied on both variables: ",b,"\n\n"))
	cat(paste("   Minimum Dimension:",m,"\n"))
	cat(paste("   Total N:",n,"\n"))
	cat(paste("   Goodman-Kruskal Gamma:", format(round(gamma, digits=3), nsmall=3),"\n"))
	cat(paste("   Somers' d:            ", format(round(somersd, digits=3), nsmall=3),"\n"))
	cat(paste("   Kendall's tau-b:      ", format(round(taub, digits=3), nsmall=3),"\n"))
	cat(paste("   Stuart's tau-c:       ",  format(round(tauc, digits=3), nsmall=3),"\n"))

	invisible(NULL)
}



