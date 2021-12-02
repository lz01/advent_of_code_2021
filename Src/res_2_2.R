
x = read.table("../Data/input_2.txt", h = F)
x$aim_sign = 0
x$aim_sign[x[,1] == "up"] = -1 
x$aim_sign[x[,1] == "down"] = 1 

x$aim = cumsum(x[,2] * x$aim_sign)
final_horiz = sum(x[,2] * (x[,1] == "forward"))
final_depth = sum(x[,2] * (x[,1] == "forward") * x$aim)

final_horiz * final_depth