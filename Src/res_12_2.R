library(stringr)

x = read.table("../Data/input_12.txt", sep = "\n")

df = t(sapply(x[,1], function(x) unlist(strsplit(x,"-"))))

clique = function(df, node) {
	res = unique(c(df[df[,1] == node,2],df[df[,2] == node,1]))
	return(res)
}


last_node = function(paths) {
	res = unlist(lapply(strsplit(paths,","),tail,1))
	return(res)
}


paths = c("start")

while(any(last_node(paths) != "end")) {
	last_nodes = setdiff(unique(last_node(paths)),"end")

	for(curr_last_node in last_nodes) {
		my_clique_to_go = setdiff(clique(df,curr_last_node),"start")
		## Append clique to corresponding paths
		combin = expand.grid(paths[last_node(paths) == curr_last_node],my_clique_to_go)
		paths = c(paths,unlist(map2(combin[,1],combin[,2], ~ paste(.x,.y,sep=","))))
		## Remove paths that ended with curr_last_node
		paths = paths[last_node(paths) != curr_last_node]
		## Remove paths that visit a same small cave twice (including start)
		paths = paths[!str_detect(paths,"(?<=^|,)([a-z]+)(?=,.*(?<=,)\\1(?=,|$)).*(?<=,)([a-z]+)(?=,.*(?<=,)\\2(?=,|$))")]
	}
}

length(paths)



