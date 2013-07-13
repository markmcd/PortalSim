# defaults
# default energy by resonator level
res.defaultenergy <- c(1000, 1500, 2000, 2500, 3000, 4000, 5000, 6000)
# resonator colours, by level (stolen from IPAS)
res.colors = c("#fece5a", "#ffa630", "#ff7315", "#e40000", "#fd2992", "#eb26cd", "#c124e0", "#9627f4")

# input data

# ALL L1, 35m distance
# level, 1-8
#res.level <- rep(1, 8)
# distance from portal center, in m, 1-40
#res.distance <- rep(35, 8)

# LAZY LOADING, 1xL8 PLAYER, 35M DIST
res.level <- c(8,7,6,6,5,5,4,4)
res.distance <- rep(35, 8)

# HIGHEST RES PLACED ADJACENT TO LOWEST NEIGHBORS, 1xL8, 35m
#res.level <- c(8,4,6,5,7,5,6,4)
#res.distance <- rep(35, 8)

# STREET PORTAL STRATEGY, traffic from the west
#res.level <- c(5,4,4,5,6,7,8,6)
#res.distance <- c(35, 35, 35, 35, 35, 5, 1, 5)

# remaining energy (initialise to 100%)
res.energy <- res.defaultenergy[res.level]

# BURSTERS
bursters.levels = c(7,6,5)
bursters.x = c(-40, -20,  0)
bursters.y = c(-5,   -5, -5)
bursters.col = res.colors[bursters.levels]

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


burster.max_range = c(42, 48, 58, 72, 90, 112, 138, 168)
burster.gr_steps = c(8, 9, 11, 14, 18, 22, 27, 33)
burster.base_damage = c(136, 224, 408, 630, 855, 1072, 1350, 1755)
# (x,y): co-ordinate for which to calculate damage
# bl: level of burster being fired
# (bx,by): co-ordinate of burster being fired
graphracer <- function (x,y,bl,bx,by) {
  dist = sqrt((bx-x)^2 + (by-y)^2)
  if (dist > burster.max_range[bl])
    0
  else {
    step = burster.gr_steps[bl]
    circle = floor(dist / step)
    dmg = burster.base_damage[bl] / (2 ^ circle)
    dmg = max(c(0, dmg))
    round(dmg)
  }
}

damage_func = graphracer

# plot the background; burster damage

# matrix & picture of burster damage
# 800x800 matrix for -40.0 to +40.0 w/0.1 increment steps
dmg_matrix <- matrix(0, 800, 800)
for (i in 1:length(bursters.levels)) {
  for (x in -400:400) {
    for (y in -400:400) {
      dx = x / 10
      dy = y / 10
      dmg_matrix[x+400,y+400] = dmg_matrix[x+400,y+400] + 
        damage_func(dx,dy, bursters.levels[i], bursters.x[i],bursters.y[i])
    }
  }
}
image(dmg_matrix, frame.plot=F, axes=F)
par(new=T) # persist previous image & draw over it

# calculate damage on the actual resonators
total_dmg = 0
res.symbols = rep(21, 8)
res.energypct = rep(1, 8)
for (i in 1:8) {
  x = res.getx(res.distance[i], i)
  y = res.gety(res.distance[i], i)
  # pull damage from the matrix, capping out at the resonator's energy level
  dmg = min(dmg_matrix[round(x*10)+400,round(y*10)+400], res.energy[i])
  # update resonator energy level
  new_energy = res.energy[i] - dmg
  res.energypct[i] = paste(round((new_energy / res.energy[i]) * 100), "%", sep="")
  res.energy[i] = new_energy
  if (res.energy[i] == 0)
    res.symbols[i] = 4
  
  total_dmg = total_dmg + dmg
}

# plot the resonators
par(pty="s") # keep the aspect ratio square
#plot(-40:40,-40:40,type="n",xaxs="i",yaxs="i") # draw an empty plot so we can get gridlines behind data points
#axis(1, pos=0, col="#aaaaaa", col.axis="#aaaaaa")
#axis(2, pos=0, las=2, col="#aaaaaa", col.axis="#aaaaaa")
#par(new=T)
plot(diag(sapply(res.distance, res.getx, 1:8)), diag(sapply(res.distance, res.gety, 1:8)), 
     ylim=c(-40, 40), xlim=c(-40,40), xlab="", ylab="", xaxs="i", yaxs="i",
     bg=res.colors[res.level], pch=res.symbols, cex=4, col="#ffffff")

# plot res %
text(diag(sapply(res.distance, res.getx, 1:8)), diag(sapply(res.distance, res.gety, 1:8)),
     res.energypct, cex=0.5)

# plot the bursters
points(bursters.x, bursters.y, pch=23, cex=2, col=bursters.col, bg=0)