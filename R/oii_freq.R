oii.freq<-function(x) {
	
	freq<-table(x)
	names<-c(names(freq),"Valid Total","Missing","Total")
	freq<-as.numeric(freq)
	n<-sum(!is.na(x))
	isna<-sum(is.na(x))
	percent<-round((freq/length(x))*100,2)
	valid_percent<-round((freq/n)*100,2)
	cum=cumsum(valid_percent)
	
	freq<-c(freq,n,isna,n+isna)
	percent<-c(percent,sum(percent),round(isna/length(x),2),100)
	valid_percent<-c(valid_percent,sum(valid_percent),NA,NA)
	cum<-c(cum,NA,NA,NA)
	
	data.frame(freq=freq,percent=percent,valid_percent=valid_percent,row.names=names,cum=cum)


}
