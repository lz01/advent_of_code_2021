

xmin = 235
xmax = 259
ymin = -118
ymax = -62

#####
## Analytical solution

## N(N+1)/2  between 235 and 259 ->  x_move_0 = 22
## y_move_0+1 = -ymin    -> y_move_0 = 117


x_move_0 = 22
y_move_0 = 117

res = 117*118/2

res

##Answer is 6903

###Plotting

x = 0
y = 0

vx = x
vy = y

x_move = x_move_0
y_move = y_move_0


while(x<=xmax & y>=ymin){
	x = x + x_move
	y = y + y_move
	x_move = x_move - sign(x_move)
	y_move = y_move -1
	vx = c(vx,x)
	vy = c(vy,y)
	cat("Position: x:",x,"\ty:",y,"\n")
}


plot(NA,xlim = c(0,xmax), ylim =c(ymin,7000), type = "n")
polygon(x=c(xmin,xmax,xmax,xmin),y=c(ymin,ymin,ymax,ymax),col=2)
points(vx,vy, pch = 20)

