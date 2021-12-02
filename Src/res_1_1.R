x = read.table("../Data/input_1.txt",h=F)
x = x[,1]
sum(diff(x)>0)