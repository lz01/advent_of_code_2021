library(zoo)

x = read.table("../Data/input_1.txt", h = F)
x = x[,1]
v = rollsum(x, k = 3, fill = NA, align = "left")
sum(diff(v)>0)