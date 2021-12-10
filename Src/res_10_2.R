x = read.table("../Data/input_10.txt", sep = "\n")

l = lapply(x[,1],function(y) unlist(strsplit(y,"")))

open_chars = c("(","[","{","<")
close_chars = c(")","]","}",">")

calc_score = function(last_open) {
	first_close = rev(last_open)
	score = 0
	for(char in first_close) {
		score = 5*score + which(open_chars == char)
	}
	return(score)
}


scores = c()

for(v in l) {
	last_open = c()
	incomplete = TRUE
	for(char in v) {
		if(char %in% open_chars) {
			last_open  = c(last_open, char)
		}
		else if(char %in% close_chars) {
			if(tail(last_open,1) == open_chars[close_chars == char]) {
				last_open = head(last_open, -1)
			}
			else {
				incomplete = FALSE
				break
			}
		}

	}
	if(incomplete) {
		scores = c(scores,calc_score(last_open))
	}

}

median(scores)
