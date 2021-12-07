x = read.table("../Data/input_6.txt", colClasses = "numeric", sep = ",")

x = as.numeric(x[1,])

iter = function(x,i) {
	res = c(x[x>0]-1, x[x==0]+6, x[x==0]+8)
	return(res)
}

length(Reduce(iter,1:80,x))