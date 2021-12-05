library(tidyr)
library(dplyr)
library(purrr)

x = read.table("../Data/input_5.txt", sep ="\n", col.names = "segment")

df = x %>% 
	separate(segment, sep = ",|->", into = c("x1","y1","x2","y2")) %>%
	## Go from 0 index to 1 index
	mutate(across(.cols = everything(), ~ as.numeric(.x)+1)) %>%
	## Only keeping rows where x1 = x2 OR y1 = y2
	filter((x1 == x2) | (y1 == y2))
	
xmax = max(c(df$x1,df$x2))
ymax = max(c(df$y1,df$y2))

mat = matrix(0, nrow = xmax, ncol = ymax)

tmp  = pmap(df,function(x1,x2,y1,y2) mat[cbind(x1:x2,y1:y2)] <<- mat[cbind(x1:x2,y1:y2)] + 1)

sum(mat>1)