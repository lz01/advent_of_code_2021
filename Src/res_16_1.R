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
versions = c()

id = 1

while(id<=(length(x_bin)-10)) {

	version = bin_to_dec(x_bin[id:(id+2)])
	versions = c(versions,version)
	type = bin_to_dec(x_bin[(id+3):(id+5)])

	## Literal value
	if(type == 4) {
		## Ids of groups bits (1/0)
		group_bits = x_bin[(id+6):length(x_bin)][c(TRUE,rep(FALSE,4))]
		last_group_bit = which(group_bits == 0)[1]
		id = id+6+last_group_bit*5
	} 

	## Value operator
	else {
		lengthtype = bin_to_dec(x_bin[(id+6)])
		if(lengthtype == 0) {
			id = id+22
		} else
		if(lengthtype == 1) {
			id = id+18
		}	
	}

}

sum(versions, na.rm = T)

