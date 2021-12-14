library(stringr)

x = read.table("../Data/input_13.txt", sep = "\n")

data = x[str_detect(x[,1],"^\\d+,\\d+$"),1]
instruc = str_match(x[str_detect(x[,1],"="),1],"([xy])=(\\d+)")
instruc = data.frame(dir = instruc[,2], pos = as.numeric(instruc[,3])+1)


df = t(sapply(data, function(x) as.numeric(unlist(strsplit(x,","))))+1)

n = max(df[,2])
m = max(df[,1])

mat = matrix(0, nrow = n, ncol = m)
mat[df[,c(2,1)]] = 1

pos = instruc$pos[1]
dir = instruc$dir[1]

if(dir == "x") {
	## Left side of the mat
	left_mat = mat[,1:(pos-1)]
	## Added mat
	added_mat = mat[,m:(pos+1)] 
	mat = left_mat 
	mat[,(ncol(mat)-m+pos+1):ncol(mat)] = mat[,(ncol(mat)-m+pos+1):ncol(mat)] + added_mat
} else
## Fold horizontally
 if(dir == "y") {
	## Top side of the mat
	top_mat = mat[1:(pos-1),]
	## Added mat
	added_mat = mat[n:(pos+1),] 
	mat = top_mat 
	mat[(nrow(mat)-n+pos+1):nrow(mat),] = mat[(nrow(mat)-n+pos+1):nrow(mat),] + added_mat
	mat[mat>1] = 1
}

sum(mat>=1)