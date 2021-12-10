x = read.table("../Data/input_6.txt", colClasses = "numeric", sep = ",")

x = as.numeric(x[1,])
tab = data.frame(table(x))
tab$x = as.numeric(tab$x)
tab = rbind(data.frame(x = 0, Freq = 0), tab,data.frame(x = c(6:8), Freq = rep(0,3)))

iter = function(tab,i) {
	freq_0 = tab$Freq[1]
	## 1 to 8 become 0 to 7
	tab$Freq[1:8] = tab$Freq[2:9]
	## 0s become 8s
	tab$Freq[9] = freq_0
	## 0s get added to 6s
	tab$Freq[7] = tab$Freq[7] + freq_0 
	return(tab)
	#c(x[x>0]-1, x[x==0]+6, x[x==0]+8)
}

#
res = sum(Reduce(iter,1:256,tab)$Freq)
format(res, scientific=F)

