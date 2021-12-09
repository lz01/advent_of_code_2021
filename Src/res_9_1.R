x = read.table("../Data/input_9.txt", colClasses = "character", sep = "\n")

mat = matrix(sapply(x,function(x) as.numeric(unlist(strsplit(x, "")))),nrow = nrow(x),byrow = T)

m = nrow(mat)
n = ncol(mat)

top_inf_bottom = (mat[1:(m-1),]-mat[2:m,])<0
lower_than_bottom = rbind(top_inf_bottom,TRUE)
lower_than_top = rbind(TRUE,!top_inf_bottom)

left_inf_right = (mat[,1:(n-1)]-mat[,2:n])<0
lower_than_right = cbind(left_inf_right,TRUE)
lower_than_left = cbind(TRUE,!left_inf_right)

four_cond = (lower_than_bottom + lower_than_top + lower_than_right + lower_than_left) == 4

sum(mat[four_cond] + 1)

