library(dplyr)
library(tidyr)
library(purrr)
library(stringr)



x = read.table("../Data/input_8.txt", sep = "\n", col.names = "X")

intersect_strings = function(x,y) {
	res = unlist(map2((strsplit(x,"")), strsplit(y,""), function(a,b) paste0(sort(intersect(a,b)), collapse = "")))
	return(res)
}

setdiff_strings = function(x,y) {
	res = unlist(map2((strsplit(x,"")), strsplit(y,""), function(a,b) paste0(sort(setdiff(a,b)), collapse = "")))
	return(res)
}

union_strings = function(x,y) {
	res = unlist(map2((strsplit(x,"")), strsplit(y,""), function(a,b) paste0(sort(union(a,b)), collapse = "")))
	return(res)
}

df = x %>%
	separate(X, into = c("input","output"), sep = "\\|") %>%
	mutate(input = str_squish(input), output = str_squish(output)) %>%
	separate(input, into = paste0("input_",c(1:10)), sep = " ", remove = FALSE) %>%
	separate(output, into = paste0("output_",c(1:4)), sep = " ", remove = FALSE)


res = df %>% 
	mutate(one = str_extract(input, "\\b[a-z]{2}\\b"),
		   four = str_extract(input, "\\b[a-z]{4}\\b"),
		   seven = str_extract(input, "\\b[a-z]{3}\\b"),
		   eight = str_extract(input, "\\b[a-z]{7}\\b")) %>%
	mutate(A = setdiff_strings(seven, one),
		   BD = setdiff_strings(four, one),
		   ABCDF = union_strings(four, seven))

res$nine = unlist(map2(str_extract_all(res$input,"\\b[a-z]{6}\\b"), res$ABCDF, 
	function(a,b) a[nchar(intersect_strings(a,b)) == 5]))

res = res %>%
	mutate(G = setdiff_strings(nine,ABCDF))

res$six = unlist(map2(str_extract_all(res$input,"\\b[a-z]{6}\\b"), res$one, 
	function(a,b) a[nchar(intersect_strings(a,b)) == 1]))

res$zero = unlist(map2(str_extract_all(res$input,"\\b[a-z]{6}\\b"), map2(res$nine, res$six, c), 
	function(a,b) setdiff(a,b)))

res$three = unlist(map2(str_extract_all(res$input,"\\b[a-z]{5}\\b"), res$one, 
	function(a,b) a[nchar(intersect_strings(a,b)) == 2]))

res$five = unlist(map2(str_extract_all(res$input,"\\b[a-z]{5}\\b"), res$BD, 
	function(a,b) a[nchar(intersect_strings(a,b)) == 2]))

res$two = unlist(map2(str_extract_all(res$input,"\\b[a-z]{5}\\b"), map2(res$three, res$five, c), 
	function(a,b) setdiff(a,b)))

res = res %>% 
	mutate(across(.cols = output_1:two, ~ lapply(strsplit(.x,""), function(y) paste0(sort(y), collapse = "")))) %>%
	rowwise() %>%
	mutate(code = list(list(zero,one,two,three,four,five,six,seven,eight,nine)))

res$output_1_dec =  map2(res$output_1, res$code, function(a,b) which(a == b)-1)
res$output_2_dec =  map2(res$output_2, res$code, function(a,b) which(a == b)-1)
res$output_3_dec =  map2(res$output_3, res$code, function(a,b) which(a == b)-1)
res$output_4_dec =  map2(res$output_4, res$code, function(a,b) which(a == b)-1)


sum(as.numeric(paste0(res$output_1_dec,res$output_2_dec,res$output_3_dec,res$output_4_dec)))	




