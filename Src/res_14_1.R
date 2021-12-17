library(stringr)
library(purrr)

x = read.table("../Data/input_14.txt", sep = "\n")

insert_chars = function(string,start,end,chars) {
	require(stringr)
	res = paste0(str_sub(string,1,start),chars,str_sub(string,end,nchar(string)))
	return(res)
}

template = x[1,1]

rules = t(sapply(x[-1,1], function(x) str_squish(unlist(strsplit(x,"->")))))
rules = data.frame(rules)
names(rules) = c("pattern","chars")
## We need to convert the pattern to have a lookahead so that the second letter is not consumed
## and can therefore be used for another match
rules$pattern = paste0(insert_chars(rules$pattern,1,2,"(?="),")")
polymer = template

for(s in 1:10) {


	l = str_locate_all(polymer,rules[,1])
	use_rules = rules[lengths(l)>0,]
	l = l[lengths(l)>0]
	## As we use lookahead, we need to add 1 to end
	use_rules = data.frame(do.call(rbind,map2(use_rules$chars,l,cbind)))
	names(use_rules) = c("chars","start","end")
	use_rules$start = as.numeric(use_rules$start)
		use_rules$end = as.numeric(use_rules$end)+1
	use_rules = use_rules[order(use_rules$start,decreasing = TRUE),]

	for(i in seq_len(nrow(use_rules))) {
		polymer = insert_chars(polymer,use_rules$start[i],use_rules$end[i],use_rules$chars[i])
	}
}

tab = sort(table(unlist(strsplit(polymer,split = ""))))

tail(tab,1)-head(tab,1)

