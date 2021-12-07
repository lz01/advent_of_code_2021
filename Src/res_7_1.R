x = read.table("../Data/input_7.txt", colClasses = "numeric", sep = ",")

x = as.numeric(x[1,])

sum(abs(x-median(x)))
