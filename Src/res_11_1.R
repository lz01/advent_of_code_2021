x = read.table("../Data/input_11.txt", colClasses = "character", sep = "\n")

mat = matrix(sapply(x,function(x) as.numeric(unlist(strsplit(x, "")))), nrow = nrow(x), byrow = T)

n = nrow(mat)
m = ncol(mat)

incr_around = function(mat,row,col) {
	around = as.matrix(expand.grid(row+c(-1,0,1),col+c(-1,0,1)))
	## Remove the actual pos
	around = around[-5,]
	## Remove outside pos
	around = around[around[,1] %in% c(1:nrow(mat)),]
	around = around[around[,2] %in% c(1:ncol(mat)),]
	mat[around] = mat[around] + 1
	return(mat)
}


n_steps = 100

flash_count = 0

for (i in 1:n_steps) {
	## Increment by 1
	mat = mat + 1
	## Flashing/increment loop
	while(sum(mat > 9, na.rm = TRUE)>0) {
		flashing_ids = which(mat>9)
		## Flashed turns to NA
		mat[mat > 9] = NA
		for(id in flashing_ids) {
			mat = incr_around(mat,row = ifelse(id %% n == 0,n,id %% n), col = ceiling(id/n))
		}
	}
	## Count flashes
	flash_count = flash_count + sum(is.na(mat))
	## Reset flashed to 0
	mat[is.na(mat)] = 0
}

flash_count

