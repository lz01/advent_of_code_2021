x = read.table("../Data/input_7.txt", colClasses = "numeric", sep = ",")

x = as.numeric(x[1,])

fuel = function(a) {dist = abs(x-a); sum(dist*(dist+1)/2)}

oo = optimize(fuel,c(min(x),max(x)))
aa = round(oo$minimum)

fuel(aa)