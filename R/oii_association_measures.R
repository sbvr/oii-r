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

oii_association_measures <- function(x,y=NULL,warnings=FALSE){
	concordant <- function(x){ 
		## get sum(matrix values > r AND > c) 
		## for each matrix[r, c] 
		mat.lr <- function(r,c){ 
			lr <- x[(r.x > r) & (c.x > c)] 
			sum(lr) 
		} 

		## get row and column index for each 
		## matrix element 
		r.x <- row(x) 
		c.x <- col(x) 

		## return the sum of each matrix[r, c] * sums 
		## using mapply to sequence thru each matrix[r, c] 
		sum(x * mapply(mat.lr, r = r.x, c = c.x)) 
	} 

	discordant <- function(x){ 
		## get sum(matrix values > r AND < c) 
		## for each matrix[r, c] 
		mat.ll <- function(r,c){ 
			ll <- x[(r.x > r) & (c.x < c)] 
			sum(ll) 
		} 

		## get row and column index for each 
		## matrix element 
		r.x <- row(x) 
		c.x <- col(x) 

		## return the sum of each matrix[r, c] * sums 
		## using mapply to sequence thru each matrix[r, c] 
		sum(x * mapply(mat.ll, r = r.x, c = c.x)) 
	} 

	# tied_both: choose 2 of every element in the matrix and sum the results
	tied_both <- function(x) {sum(choose(x,2))}

	tied_first <- function(x){ 
		mult<-function(r,c) {
			#print(paste(r,c))
			#print((r.x > r) & (c.x == c))
			#print(x[(r.x > r) & (c.x == c)] ) 
			lr <- x[(r.x == r) & (c.x > c)]
			x[r,c]*sum(lr)
		}

		r.x <- row(x) 
		c.x <- col(x) 
		#cat("r.x: ",r.x,"\n")
		#cat("c.x: ",c.x,"\n")

		## return the sum of each matrix[r, c] * sums 
		## using mapply to sequence thru each matrix[r, c] 

		tmp<-(mapply(mult, r = r.x, c = c.x))
		#print("----------------")
		#print(tmp)
		sum(tmp)
	}       

	tied_second<-function(x) {tied_first(t(x))} #transpose the matrix and run the same code as for first
  
	if(!is.table(x) & !is.matrix(x)){
		x<-table(x,y)
	}
	c <- concordant(x) 
	d <- discordant(x)
	b <- tied_both(x)
	f <- tied_first(x)
	s <- tied_second(x)
	totp <- c + d + b + f + s
	n <- sum(x)
	m <- min(dim(x))
	wr <- sum( rowSums(x)^2 )
	wc <- sum( colSums(x)^2 )
	if (!warnings) {
		xsq <- suppressWarnings(chisq.test(x))
	} else {
		xsq <- chisq.test(x)
	}

	gamma <- (c - d) / (c + d)
	somersd <- (c - d) / (c + d + s)
	taub <- (c - d) / sqrt((c + d + s) * (c + d + f))
	# taubn <- (c - d) / (sqrt( (n^2 - wr) * (n^2 - wc) ) )
	# taubn2 <- (c - d) / (sqrt( (totp - f) * (totp - s)) )
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
	#below is original output
	#cat(paste("   Gamma:",
	#          signif(gamma,.Options$digits),
	#          "Standard error:",
	#          signif(stdError,.Options$digits),
	#          "\n\n"))

	#cat(paste("   H0: gamma = 0 vs HA: two-sided\n"))
	#cat(paste("   z:",
	#          signif(z, .Options$digits),
	#         "p-value:",
	#          signif(2*(1-pnorm(abs(z))), .Options$digits),
	#          "\n\n"))
	#if(c<51 | d<51){
	#  cat("Warning: p-values are based on a normal approximation to the\n")
	#  cat("sampling distribution of the z test statistic, which is commonly\n")
	#  cat("considered to be good only if C and D are both > 50.\n")
	#}

	invisible(NULL)
}



