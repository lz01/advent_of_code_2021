library(dplyr)
library(tidyr)
library(stringr)


x = read.table("../Data/input_8.txt", sep = "\n", col.names = "X")

df = x %>%
	separate(X, into = c("input","output"), sep = "\\|") %>%
	mutate(input = str_squish(input), output = str_squish(output)) %>%
	separate(input, into = paste0("input_",c(1:10)), sep = " ") %>%
	separate(output, into = paste0("output_",c(1:4)), sep = " ")


res = df %>% 
	select(starts_with("output")) %>% 
	mutate(across(.cols = everything(), ~ nchar(.x) %in% c(2,3,4,7))) %>%
	rowSums() %>%
	sum()