library(readr)
library(stringr)
library(beepr)


## For the loop to go faster, comment out the system call

txt = read_file("../Data/input_4.txt")

x = str_split(txt,"\n{2}")

draw = as.numeric(unlist(str_split(x[[1]][1],",")))

boards = lapply(x[[1]][-1],function(x) t(matrix(as.numeric(unlist(strsplit(str_squish(x),"\\s+"))),ncol = 5)))
names(boards) = c(1:100)

num_in_board = function(mat, num) {
	res = num %in% mat 
	return(res)
}

turn_drawn_to_NA =  function(mat, num) {
	mat[mat %in% num] = NA
	return(mat)
}

mat_has_bingo = function(mat) {
	res = (any(colSums(is.na(mat)) == ncol(mat)) | any(rowSums(is.na(mat)) == nrow(mat)))
	return(res)
}


i = 1

while(length(boards)>0 & i<=length(draw)) {
	drawn_num = draw[i]
	cat("Round",i,": \t", drawn_num, "\n")
	system(paste0("say ",drawn_num))
	boards = lapply(boards, turn_drawn_to_NA, drawn_num)
	bingo_watch = unlist(lapply(boards, mat_has_bingo))
	if(any(bingo_watch)) {
		win_board_id = which(bingo_watch)
		win_board_name = names(boards)[win_board_id]
		cat("Winning board is: ", win_board_name, "\n")
		if(length(boards) == 1) break
		boards = boards[-win_board_id]
		cat("Only", length(boards),"left..\n")
	}
	i = i+1

}

last_win_board = boards[[1]]
res = drawn_num * sum(last_win_board[!is.na(last_win_board)])

res