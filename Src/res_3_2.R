x = read.table("../Data/input_3.txt", h = F, colClasses = "character")
x_array = t(sapply(strsplit(x[,1],split = ""),function(x) c(as.numeric(x))))

most_least_freq_value = function(arr, j, most = TRUE) {
	res = apply(arr,2,function(x) as.numeric(sum(x)>=length(x)/2))[j]
	if(!most) res = 1 - res
	return(res)
}

bin_to_dec = function(x)
{
	res = 2^c(0:(length(x)-1)) %*% rev(x)
	return(res)
}

find_rating = function(array, most_freq)
{
	curr_col = 1

	while(nrow(array) > 1 & curr_col <= ncol(array)) {
		val = most_least_freq_value(arr = array, j = curr_col, most = most_freq)
		array = array[array[,curr_col] == val,,drop = FALSE]
		curr_col = curr_col +1
	}

	return(array)
}

res = bin_to_dec(find_rating(x_array,most_freq = TRUE)) * bin_to_dec(find_rating(x_array,most_freq = FALSE))

res