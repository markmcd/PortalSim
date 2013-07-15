# display settings
# default energy by resonator level
res.defaultenergy <- c(1000, 1500, 2000, 2500, 3000, 4000, 5000, 6000)
# resonator colours, by level (stolen from IPAS)
res.colors = c("#fece5a", "#ffa630", "#ff7315", "#e40000", "#fd2992", "#eb26cd", "#c124e0", "#9627f4")
# radius of the map to show (max portal radius is 40m but you can expand for prettier charts that render slower)
map.radius = 50
# how fine the detail is on the heatmap (1 = 1m, 0.1 = 10cm)
map.resolution = 0.1
# show the heatmap background?  this is the slowest part of the process
map.showheatmap = T

# input data

# ALL L1, 35m distance
# level, 1-8, use NA for missing res
#res.level <- rep(1, 8)
# distance from portal center, in meters.  Ingress lets you place up to 40m away, usually about 35 at best though.
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
# 7,6,5 travelling west-to-east
#bursters.levels = c(7,6,5)
#bursters.x = c(-40, -20,  0)
#bursters.y = c(-5,   -5, -5)

# 8s in the center (the last param in rep is the number of bursters, e.g. 3)
#bursters.levels = rep(8, 3)
#bursters.x = rep(0, 3)
#bursters.y = rep(0, 3)

# burster placement that matches the lazy loading res arrangement (i.e. an 8 burster at 35m E, 7 at 35m NE, etc)
#bursters.levels = c(8,7,6,6,5,5,4,4)
#bursters.x = c(35, 25, 0,  -25, -35, -25, 0,   25)
#bursters.y = c(0,  25, 35, 25,  0,   -25, -35, -25)

# 3x8s over the eastern side (looks kinda nice, good for demoing ;)
bursters.levels = c(8,8,8)
bursters.x = c(15, 20, 25)
bursters.y = c(15, 20, 15)


# 4x8s over the eastern side
# this seems to be a decent strategy against lazy loading
#bursters.levels = c(8,8,8,8)
#bursters.x = c(15, 20, 25, 20)
#bursters.y = c(15, 20, 15, 10)

# no bursters, just show the resonators
#bursters.levels = c()
#bursters.x = c()
#bursters.y = c()


bursters.col = res.colors[bursters.levels]

# polar -> cartesian conversion functions
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
par(pty="s") # keep the aspect ratio square

# matrix & picture of burster damage
matrix_size = map.radius * 1/map.resolution
# 800x800 matrix for -40.0 to +40.0 w/0.1 increment steps
dmg_matrix <- matrix(0, 2 * matrix_size, 2 * matrix_size)
res.symbols = rep(21, 8)
res.energypct = rep("100%", 8)
total_dmg = 0
if (length(bursters.levels > 0)) {
  for (i in 1:length(bursters.levels)) {
    if (map.showheatmap) {
      for (x in -matrix_size:matrix_size) {
        for (y in -matrix_size:matrix_size) {
          dx = x * map.resolution
          dy = y * map.resolution
          dmg_matrix[x+matrix_size,y+matrix_size] = dmg_matrix[x+matrix_size,y+matrix_size] + 
            damage_func(dx,dy, bursters.levels[i], bursters.x[i],bursters.y[i])
        }
      }
      image(dmg_matrix, frame.plot=F, axes=F)
      par(new=T) # persist previous image & d
    }
    else {
      # just calculate damage at each of the resonators
      for (r in 1:8) {
        x = round(res.getx(res.distance[r], r) / map.resolution)
        y = round(res.gety(res.distance[r], r) / map.resolution)
        dx = x * map.resolution
        dy = y * map.resolution
        dmg_matrix[x+matrix_size,y+matrix_size] = dmg_matrix[x+matrix_size,y+matrix_size] + 
            damage_func(dx,dy, bursters.levels[i], bursters.x[i],bursters.y[i])
      }
    }
  }
  
  # calculate damage on the resonators
  for (i in 1:8) {
    if (!is.na(res.level[i])) {
      x = res.getx(res.distance[i], i)
      y = res.gety(res.distance[i], i)
      dmg = dmg_matrix[round(x/map.resolution)+matrix_size,round(y/map.resolution)+matrix_size]
      # capping damage at the resonator's energy level
      dmg = min(dmg, res.energy[i])
      # update resonator energy level
      new_energy = res.energy[i] - dmg
      res.energypct[i] = paste(round((new_energy / res.energy[i]) * 100), "%", sep="")
      res.energy[i] = new_energy
      if (res.energy[i] == 0)
        res.symbols[i] = 4
      
      total_dmg = total_dmg + dmg
    }
  }
}

# plot the resonators
#plot(-40:40,-40:40,type="n",xaxs="i",yaxs="i") # draw an empty plot so we can get gridlines behind data points
#axis(1, pos=0, col="#aaaaaa", col.axis="#aaaaaa")
#axis(2, pos=0, las=2, col="#aaaaaa", col.axis="#aaaaaa")
#par(new=T)
plot(diag(sapply(res.distance, res.getx, 1:8)), diag(sapply(res.distance, res.gety, 1:8)), 
     ylim=c(-map.radius, map.radius), xlim=c(-map.radius, map.radius), xlab="", ylab="", xaxs="i", yaxs="i",
     bg=res.colors[res.level], pch=res.symbols, cex=4, col="#ffffff")

# plot the bursters
points(bursters.x, bursters.y, pch=23, cex=2, col=bursters.col, bg=0)

# plot res %
if (length(bursters.levels) > 0) {
  text(diag(sapply(res.distance, res.getx, 1:8)), diag(sapply(res.distance, res.gety, 1:8)),
       res.energypct, cex=0.5)
  title(main=paste("Damage:", total_dmg, "/", (total_dmg+sum(res.energy))))
}

