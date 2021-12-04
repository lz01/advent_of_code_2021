library(readr)
library(stringr)
library(beepr)

## For the loop to go faster, comment out the system call

txt = read_file("../Data/input_4.txt")

x = str_split(txt,"\n{2}")

draw = as.numeric(unlist(str_split(x[[1]][1],",")))

boards = lapply(x[[1]][-1],function(x) t(matrix(as.numeric(unlist(strsplit(str_squish(x),"\\s+"))),ncol = 5)))

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

for(i in seq_len(length(draw))) {
	drawn_num = draw[i]
	cat("Round",i,": \t", drawn_num, "\n")
	system(paste0("say ",drawn_num))
	boards = lapply(boards, turn_drawn_to_NA, drawn_num)
	bingo_watch = unlist(lapply(boards, mat_has_bingo))
	if(any(bingo_watch)) {
		cat("Bingo!\n\n")
		beep(3)
		system("say Bingo!!!")
		cat("Winning board is: ",which(bingo_watch),"\n")
		win_board = boards[[which(bingo_watch)]]
		res = drawn_num * sum(win_board[!is.na(win_board)])
		break
	}

}

res
