x = read.table("../Data/input_10.txt", sep = "\n")

l = lapply(x[,1],function(y) unlist(strsplit(y,"")))


open_chars = c("(","[","{","<")
close_chars = c(")","]","}",">")

error_score = c(")" = 3, "]" = 57, "}" = 1197, ">" = 25137)

score = 0

for(v in l) {
	last_open = c()
	for(char in v) {
		if(char %in% open_chars) {
			last_open  = c(last_open, char)
		}
		else if(char %in% close_chars) {
			if(tail(last_open,1) == open_chars[close_chars == char]) {
				last_open = head(last_open, -1)
			}
			else {
				score = score + error_score[char]  
				break
			}
		}

	}
}

score