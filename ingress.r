### Sample code

ingress.demo.code <- function() {
  # a burster
  b5 = burster(5)
  print(b5)
  
  # an L5 resonator at full energy
  r5 = resonator(5)
  print(r5)
  
  # an L6 resonator at 75%
  r6 = resonator(6, 3000)
  print(r6)
  
  # a L8 portal portal(resonators, distances)
  p8 = portal(
    list(resonator(8), resonator(8), resonator(8), resonator(8), resonator(8), resonator(8), resonator(8), resonator(8)), 
    rep(35, 8))
  print(p8)
  
  # the plot() command works too!
  # though you might prefer a square aspect ratio:
  par(pty="s")
  plot(p8)
  
  # an arrangement of portals is called a schema. add a portal at it's x,y position
  portalmap = portal.schema(p8, 0, 0)
  print(portalmap)
  
  # add another portal to a schema like so
  p7 = portal(list(resonator(7), resonator(7), resonator(7), resonator(7), resonator(7), resonator(7), resonator(7), resonator(7)), rep(35, 8))
  portalmap = portal.schema(p7, 90, 0, portalmap)
  print(portalmap)
  
}

### RESONATORS

resonator.default_energy = c(1000, 1500, 2000, 2500, 3000, 4000, 5000, 6000)
resonator.default_colour = c("#fece5a", "#ffa630", "#ff7315", "#e40000", "#fd2992", "#eb26cd", "#c124e0", "#9627f4")

resonator <- function (level, energy = NA) {
  r = list(level = level, energy = energy)
  
  if (is.na(r$energy))
    r$energy = resonator.default_energy[level]
  
  class(r) = "resonator"
  r
}

print.resonator <- function(r, ...) {
  maxenergy = resonator.default_energy[r$level]
  pct = round(100 * r$energy / maxenergy)
  cat(paste0("L",r$level," Resonator at ",r$energy,"/",maxenergy," (",pct,"%)", "\n"))
}

# polar -> cartesian conversion functions
resonator.getx = function (distance, index, px = 0) {
  # angle is in radians, index 0 = EAST, rotating CCW
  angle = (index - 1) * pi / 4
  cos(angle) * distance + px
}
resonator.gety = function (distance, index, py = 0) {
  # angle is in radians, index 0 = EAST, rotating CCW
  angle = (index - 1) * pi / 4
  sin(angle) * distance + py
}


### BURSTERS

burster <- function (level) {
  b = list(level = level)
  
  class(b) = "burster"
  b
}

print.burster <- function (b, ...) {
  cat(paste0("L",b$level," Burster"), "\n")
}


### PORTALS

# resonators are defined CCW starting at EAST
portal.slots = c("E", "NE", "N", "NW", "W", "SW", "S", "SE")

# having resonators implemented as lists is a bit of a PITA: TODO investigate alternatives
portal <- function(resonators = list(), distances = c()) {
  if (class(resonators) != "list")
    resonators = as.list(resonators)
  
  # turn any provided resonator list into length==8
  if (length(resonators) > 8)
    res = resonators[1:8]
  else
    res = c(resonators, rep(NA, 8-length(resonators)))
  
  # check that we have the right objects
  # all NAs is OK, as long as any present are the right class (only check 1)
  if (!(all(is.na(res)) || class(res[!is.na(res)][[1]]) == "resonator"))
    stop("resonators argument must be of class 'resonator' and provided in a list()")
  
  if (length(distances) > 8)
    dist = distances[1:8]
  else
    dist = c(distances, rep(NA, 8-length(distances)))
  
  p = list(resonators = res, distances = dist)
  
  class(p) = "portal"
  p
}

portal.level <- function(portal) {
  max(1, floor(sum(sapply(portal$resonators, function(x){if(is.na(x[1])) 0 else x$level})) / 8))
}

print.portal <- function(p, ...) {
  # level = sapply(p$resonators, '[[', 'level') does not work with NAs :/
  res = data.frame(level = sapply(p$resonators, function(x){if(is.na(x[1])) NA else x$level}),
                   energy = sapply(p$resonators, function(x){if(is.na(x[1])) NA else x$energy}),
                   distance = p$distances)
  row.names(res) = portal.slots
  cat(paste0("Level ",portal.level(p)," portal\n\nResonators:\n"))
  print(res)
  cat("\n")
  cat(paste0("Total energy: ",sum(sapply(p$resonators, function(x){if(is.na(x[1])) 0 else x$energy})),"\n"))
  cat("\n")
}

plot.portal <- function(p, map.radius=50, ...) {
  ps = portal.schema(p, 0, 0)
  plot(ps, map.radius = map.radius, ...)
}

### SCHEMATA

## BURSTER SCHEMA

# this is an additive function.  add more bursters by passing the schema to this constructor.
burster.schema <- function (burster, x, y, schema = NA) {
  bpos = list(burster = burster, x = x, y = y)
  
  if (is.na(schema))
    newschema = list(bpos)
  else
    newschema = append(schema, list(bpos))
  
  class(newschema) = "burster.schema"
  newschema
}

print.burster.schema <- function(bs, ...) {
    i = 1
    for (bp in bs) {
      cat(paste0("[",i,"] L",bp$burster$level," burster at (",bp$x,", ",bp$y,")\n"))
      i = i + 1
    }
}

## PORTAL SCHEMA

portal.schema <- function (portal, x, y, schema = NA) {
  ppos = list(portal = portal, x = x, y = y)
  
  if (is.na(schema))
    newschema = list(ppos)
  else
    newschema = append(schema, list(ppos))
  
  class(newschema) = "portal.schema"
  newschema
}

print.portal.schema <- function(ps, ...) {
  i = 1
  for (p in ps) {
    cat(paste0("[",i,"] L",portal.level(p$portal)," portal at (",p$x,", ",p$y,")\n"))
    i = i + 1
  }
}

plot.portal.schema <- function(ps, map.radius=50, ...) {
  x = y = levels = c()
  minx = miny = maxx = maxy = 0
  for (portal in ps) {
    # define the boundaries of the schema
    minx = min(portal$x, minx)
    miny = min(portal$y, miny)
    maxx = max(portal$x, maxx)
    maxy = max(portal$y, maxy)
    
    # collect the resonator points
    p = portal$portal
    x = c(x, diag(sapply(p$distance, resonator.getx, 1:8, portal$x)))
    y = c(y, diag(sapply(p$distance, resonator.gety, 1:8, portal$y)))
    levels = c(levels, sapply(p$resonators, function(x){if(is.na(x[1])) NA else x$level}))
  }  
  
  # preserve passed args, but provide defaults
  plotargs = list(xlab="", ylab="", xaxs="i", yaxs="i", 
                  bg=resonator.default_colour[levels], pch=21, cex=4, col="#ffffff")
  userargs = list(...)
  if(length(userargs) > 0){
    for(i in 1:length(userargs))
      plotargs[[names(userargs)[i]]] <- userargs[[i]]
  }
  
  do.call(plot.default, c(list(x=x, y=y, ylim=c(miny - map.radius, maxy + map.radius), xlim=c(minx-map.radius, maxx+map.radius)), plotargs))
}

### ATTACK MODEL

attack <- function(portals, bursters) {
  
}