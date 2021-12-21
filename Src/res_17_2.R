


xmin = 235
xmax = 259
ymin = -118
ymax = -62


## Solutions with y_move_0 > 0
## x_move_ = 22
## y_move between 1 and 57 and 61 and 117 



## Exloring wholeish space


sols_x = c()
sols_y = c()

for(x_move_0 in c(0:259)) {

	for(y_move_0 in c(-118:117)) {

		x_move = x_move_0
		y_move = y_move_0
		x = 0
		y = 0

		while(x<=xmax & y>=ymin){
			x = x + x_move
			y = y + y_move
			x_move = x_move - sign(x_move)
			y_move = y_move -1
			if(between(x,xmin,xmax) & between(y,ymin,ymax)) {
				sols_x = c(sols_x,x_move_0)
				sols_y = c(sols_y,y_move_0)
				cat("Solution: x:",x_move_0,"\ty:",y_move_0,"\n")
				break
			}
		}

	}
}


length(sols_x)