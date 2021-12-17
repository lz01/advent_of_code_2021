library(igraph)
library(Matrix)

x = read.table("../Data/input_15.txt", colClasses = "character", sep = "\n")

tile_mat = matrix(sapply(x,function(x) as.numeric(unlist(strsplit(x, "")))), nrow = nrow(x), byrow = T)

incr_mat = function(mat,i) {
	mat = mat+1
	mat[mat == 10] = 1
	return(mat)
}

ri = function(mat,j) {
	Reduce(incr_mat,1:j,mat)
}

mat = cbind(tile_mat,ri(tile_mat,1),ri(tile_mat,2),ri(tile_mat,3),ri(tile_mat,4))
mat = rbind(mat,cbind(ri(tile_mat,1),ri(tile_mat,2),ri(tile_mat,3),ri(tile_mat,4),ri(tile_mat,5)))
mat = rbind(mat,cbind(ri(tile_mat,2),ri(tile_mat,3),ri(tile_mat,4),ri(tile_mat,5),ri(tile_mat,6)))
mat = rbind(mat,cbind(ri(tile_mat,3),ri(tile_mat,4),ri(tile_mat,5),ri(tile_mat,6),ri(tile_mat,7)))
mat = rbind(mat,cbind(ri(tile_mat,4),ri(tile_mat,5),ri(tile_mat,6),ri(tile_mat,7),ri(tile_mat,8)))


## Return id positions around pos with row-col
ids_around = function(mat,row,col) {
	around = as.matrix(cbind(c(row-1,rep(row,2),row+1),c(col,col-1,col+1,col)))
	around = around[around[,1] %in% c(1:nrow(mat)),]
	around = around[around[,2] %in% c(1:ncol(mat)),]
	## Positions with the smallest value
	ids = nrow(mat)*(around[,2]-1)+around[,1]
	return(ids)
}

n = nrow(mat)
m = ncol(mat)

## Convert matrix to adjacency matrix
## col by col matrix
vec = as.vector(mat)
adj_mat = sparseMatrix(i = {}, j = {}, dims = c(n*m, n*m))
colnames(adj_mat) = as.character(c(1:(n*m)))
rownames(adj_mat) = as.character(c(1:(n*m)))

# Each node n in v is connected to v-1, v+1,  
for(row in seq_len(n)) {
	for(col in seq_len(m)) {
		pos = n*(col-1)+row
		around = ids_around(mat,row,col)
		adj_mat[cbind(pos,around)] = vec[pos]
	}

}


g = graph_from_adjacency_matrix(adj_mat, weighted= TRUE, mode = "directed")


s = shortest_paths(g, from = "1", to = as.character(n*m))

sum(vec[s$vpath[[1]]][-1]) 



