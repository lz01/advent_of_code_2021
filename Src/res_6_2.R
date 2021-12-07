x = read.table("../Data/input_6.txt", colClasses = "numeric", sep = ",")

## Doesn't compute (too ram intensive)

x = as.numeric(x[1,])

iter = function(x,i) {
	c(x[x>0]-1, x[x==0]+6, x[x==0]+8)
}

r = Reduce(function(x,b) call("iter", x), 1:256, init = quote(x))
length(eval(r))
#length(Reduce(iter,1:256,x))