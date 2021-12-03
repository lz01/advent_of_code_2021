x = read.table("../Data/input_3.txt", h = F, colClasses = "character")
x_array = t(sapply(strsplit(x[,1],split = ""),function(x) c(as.numeric(x))))

x_gamma = apply(x_array,2,function(x) as.numeric(sum(x)>length(x)/2))
x_epsilon = 1 - x_gamma

res_gamma = 2^c(0:(length(x_gamma)-1)) %*% rev(x_gamma)
res_epsilon = 2^c(0:(length(x_epsilon)-1)) %*% rev(x_epsilon)

res_gamma * res_epsilon