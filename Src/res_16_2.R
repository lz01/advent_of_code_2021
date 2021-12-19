library(stringr)

x = read.table("../Data/input_16.txt")


hex_to_bin = c("0" = "0000","1" = "0001","2" = "0010","3" = "0011","4" = "0100","5" = "0101","6" = "0110","7" = "0111","8" = "1000","9" = "1001","A" = "1010","B" = "1011","C" = "1100","D" = "1101","E" = "1110","F" = "1111")

bin_to_dec = function(x)
{
	res = 2^c(0:(length(x)-1)) %*% rev(x)
	return(res)
}


x_bin = str_replace_all(x,hex_to_bin)
x_bin = as.numeric(unlist(strsplit(x_bin,"")))

types = c()
lengths_subpackets = c()
nbs_subpackets = c()
values = c()
lengths_packets = c()

## Unpack
id = 1
ids = 1
counter = 1

mat = matrix(0, length(x_bin), length(x_bin))

#
while(id<=(length(x_bin)-10)) {
	## Current packet
	type = bin_to_dec(x_bin[(id+3):(id+5)])
	types = c(types, type)
	cat("\n\nid",id,"\n")
	cat("counter",counter,"\n")
	cat("type",type,"\n")

	## Literal value
	if(type == 4) {
		## Ids of groups bits (1/0)
		group_bits = x_bin[(id+6):length(x_bin)][c(TRUE,rep(FALSE,4))]
		last_group_bit = which(group_bits == 0)[1]
		groups = x_bin[(id+6):(id+6-1+last_group_bit*5)][c(FALSE,rep(TRUE,4))]
		value = bin_to_dec(groups)
		cat("value",value,"\n")
		values = c(values, value)
		length_packet = 6+last_group_bit*5
		mat[id:(id+length_packet-1),counter] = 1
		cat("Filling from",id,"to",id+length_packet-1,"in column",counter,"\n")
	} 

	## Value operator
	else {
		lengthtype = bin_to_dec(x_bin[(id+6)])
		if(lengthtype == 0) {
			length_subpackets = bin_to_dec(x_bin[(id+7):(id+21)])
			cat("Total length of subpackets is",length_subpackets,"\n")
			lengths_subpackets = c(lengths_subpackets,length_subpackets)
			length_packet = 22
			values = c(values, NA)
			mat[id:(id+21+length_subpackets),counter] = 1
			cat("Filling from",id,"to",id+21+length_subpackets,"in column",counter,"\n")

		} else
		if(lengthtype == 1) {
			nb_subpackets = bin_to_dec(x_bin[(id+7):(id+17)])
			cat("Total number of subpackets is",nb_subpackets,"\n")
			ids = c(ids, seq(from = id+18, by = 11, length.out = nbpackets+1))
			nbs_subpackets = c(nbs_subpackets, nb_subpackets)
			length_packet = 18
			values = c(values, NA)
			mat[id:(id+18),counter] = nb_subpackets
			cat("Filling from",id,"to",id+18,"in column",counter,"with nb_subpackets\n")
		}	
	}

	cat("Length packet",length_packet,"\n")
	lengths_packets = c(lengths_packets,length_packet)

	# Next id
	id = id+length_packet
	ids = c(ids,id)
	counter = counter + 1

	
}

## Remove unneeded columns
mat = mat[,colSums(mat)>0]

## Fill in values in mat for packets that have a number of subpackets
val = apply(mat,2,max)

## Need to fill in the columns from the end
for(col in rev(which(val>1))) {
	ind = val[col]
	cat("Multi-packet column",col,"with",ind,"children\n")
	cat("First child",col+1,"\n")
	mat[,col] = as.numeric(apply(mat[,col:(col+1)],1,max)>0)
	while(ind>1) {
		next_child_row = tail(which(mat[,col]==1),1)+1
		cat("next child row",next_child_row,"\n")
		next_child = col+which(mat[next_child_row,col:ncol(mat)] == 1)[1]-1
		cat("next child",next_child,"\n")
		mat[,col] = as.numeric(apply(mat[,c(col,next_child)],1,max)>0)
		ind = ind-1
	}
}

#mat = mat[rowSums(mat)>=2,]


## Reducing
ops = list('+','*',min,max,c,'>','<','==')

descendants = function(mat,col) {
	res = col+which(colSums(mat[mat[,col]>0,(col+1):ncol(mat),drop = FALSE])>0)
	return(res)
}

children = function(mat,col) {
	res = descendants(mat,col)
	is_child = TRUE
	for(i in res){
		is_child = c(is_child,parent(mat,i) == col)
	}
	is_child = is_child[-1]
	res = res[is_child]
	return(res)
}

parent = function(mat,col) {
	res = tail(which(colSums(mat[mat[,col]>0,1:(col-1),drop = FALSE])>0),1)
	return(res)
}


cols = rev(1:ncol(mat))
curr_mat = mat
curr_col = cols[1]
incr_search = 1

while(length(cols)>1){
	cat("col",curr_col,"\n")
	parent_col = parent(curr_mat,curr_col)
	cat("parent",parent_col,"\n")
	parent_type = types[parent_col]
	cat("parent type", parent_type,"\n")
	siblings = children(curr_mat,parent_col)
	cat("siblings",siblings,"\n")
	if(any(is.na(values[siblings]))) {
		cat(curr_col,"has NA siblings\n")
		incr_search = incr_search+1
	}
	else {
		comp_value = as.numeric(Reduce(ops[[parent_type+1]],values[siblings]))
		values[parent_col] = comp_value
		cat("comp value",comp_value,"\n")
		cols = setdiff(cols,siblings)
	}
	if(incr_search>length(cols)){incr_search = 1}
			curr_col = cols[incr_search]


}


format(values[1], scientific = FALSE)
