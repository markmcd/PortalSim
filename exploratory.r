# defaults
# default energy by resonator level
res.defaultenergy <- c(1000, 1500, 2000, 2500, 3000, 4000, 5000, 6000)
# resonator colours, by level (stolen from IPAS)
res.colors = c("#fece5a", "#ffa630", "#ff7315", "#e40000", "#fd2992", "#eb26cd", "#c124e0", "#9627f4")

# input data

# ALL L1, 35m distance
# level, 1-8
res.level <- rep(1, 8)
# distance from portal center, in m, 1-40
res.distance <- rep(35, 8)

# LAZY LOADING, 1xL8 PLAYER, 35M DIST
res.level <- c(8,7,6,6,5,5,4,4)
res.distance <- rep(35, 8)

# HIGHEST RES PLACED ADJACENT TO LOWEST NEIGHBORS, 1xL8, 35m
res.level <- c(8,4,6,5,7,5,6,4)
res.distance <- rep(35, 8)

# remaining energy (initialise to 100%)
res.energy <- res.defaultenergy[res.level]

# functions
res.getx = function (distance, index) {
  # angle is in radians, index 0 = EAST, rotating CCW
  angle = (index - 1) * pi / 4
  cos(angle) * distance
}
res.gety = function (distance, index) {
  # angle is in radians, index 0 = EAST, rotating CCW
  angle = (index - 1) * pi / 4
  sin(angle) * distance
}


# chart the resonators
par(pty="s") # keep the aspect ratio square
plot(diag(sapply(res.distance, res.getx, 1:8)), diag(sapply(res.distance, res.gety, 1:8)), 
     frame.plot=F, axes=F, ylim=c(-40, 40), xlim=c(-40,40), xlab="", ylab="",
     col=res.colors[res.level], pch=19, cex=4)
#axis(1, pos=0, col="#aaaaaa", col.axis="#aaaaaa")
#axis(2, pos=0, las=2, col="#aaaaaa", col.axis="#aaaaaa")
