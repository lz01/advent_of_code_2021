
x = read.table("../Data/input_2.txt", h = F)
x_horiz = sum(x[x[,1]=="forward",2])
x_depth = sum(x[x[,1]=="down",2]) - sum(x[x[,1]=="up",2])
x_horiz * x_depth