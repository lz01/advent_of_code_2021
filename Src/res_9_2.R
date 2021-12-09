x = read.table("../Data/input_9.txt", colClasses = "character", sep = "\n")

mat = matrix(sapply(x,function(x) as.numeric(unlist(strsplit(x, "")))),nrow = nrow(x),byrow = T)

m = nrow(mat)
n = ncol(mat)

neighbourhood_higher = function(mat,myrow,mycol) {
	myval = mat[myrow,mycol]
	df_neigh = data.frame(row = c(myrow-1,rep(myrow,2),myrow+1), col = c(mycol, mycol-1, mycol +1, mycol))
	## Removing positions outside the matrix
	df_neigh = df_neigh[df_neigh$row %in% c(1:nrow(mat)) & df_neigh$col %in% c(1:ncol(mat)),]
	values = mat[as.matrix(df_neigh)]	
	df_neigh = df_neigh[values>myval & values<9,,drop = FALSE]
	return(df_neigh)
}

top_inf_bottom = (mat[1:(m-1),]-mat[2:m,])<0
lower_than_bottom = rbind(top_inf_bottom,TRUE)
lower_than_top = rbind(TRUE,!top_inf_bottom)

left_inf_right = (mat[,1:(n-1)]-mat[,2:n])<0
lower_than_right = cbind(left_inf_right,TRUE)
lower_than_left = cbind(TRUE,!left_inf_right)

four_cond = (lower_than_bottom + lower_than_top + lower_than_right + lower_than_left) == 4

ids = which(four_cond)
low_pos = data.frame(row = ids %% nrow(mat),
				col = ceiling(ids/nrow(mat)))
low_pos$row[low_pos$row == 0] = nrow(mat)

sizes = NA
for(j in seq_len(nrow(low_pos))) {
	low_row = low_pos$row[j]
	low_col = low_pos$col[j]
	df = neighbourhood_higher(mat, low_row, low_col)
	i = 1 
	while(i<=nrow(df)){
		df = rbind(df,neighbourhood_higher(mat,df[i,1],df[i,2])) 
		df = df[!duplicated(df),] 
		i = i+1
	}
	sizes = c(sizes,nrow(df)+1)
}

sizes = sizes[-1]

Reduce('*',sort(sizes,decr=T)[1:3])

