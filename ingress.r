### RESONATORS

resonator.default_energy = c(1000, 1500, 2000, 2500, 3000, 4000, 5000, 6000)

resonator <- function (level, energy = NA) {
  r = list(level = level, energy = energy)
  
  if (is.na(r$energy))
    r$energy = resonator.default_energy[level]
  
  class(r) = "resonator"
  r
}

print.resonator <- function(r, ...) {
  i = 1
  for (x in list(r, ...)) {
    maxenergy = resonator.default_energy[x$level]
    pct = round(100 * x$energy / maxenergy)
    cat(paste0("[",i, "] L",x$level," Resonator at ",x$energy,"/",maxenergy," (",pct,"%)", "\n"))
    i = i + 1
  }
}

### BURSTERS

burster <- function (level) {
  b = list(level = level)
  
  class(b) = "burster"
  b
}

print.burster <- function (b, ...) {
  i = 1
  for (x in list(b, ...)) {
    cat(paste0("[",i, "] L",x$level," Burster"), "\n")
    i = i + 1
  }
}


### PORTALS

# resonators are defined CCW starting at EAST
portal.slots = c("E", "NE", "N", "NW", "W", "SW", "S", "SE")

# having resonators implementes as lists is a bit of a PITA: TODO
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
  floor(sum(sapply(p$resonators, function(x){if(is.na(x[1])) 0 else x$level})) / 8)
}

print.portal <- function(p, ...) {
  i = 1
  for (x in list(p, ...)) {
    # level = sapply(p$resonators, '[[', 'level') does not work with NAs :/
    res = data.frame(level = sapply(p$resonators, function(x){if(is.na(x[1])) NA else x$level}),
                     energy = sapply(p$resonators, function(x){if(is.na(x[1])) NA else x$energy}),
                     distance = p$distances)
    row.names(res) = portal.slots
    cat(paste0("[",i,"]\n"))
    cat(paste0("Level ",portal.level(x)," portal\n\nResonators:\n"))
    print(res)
    cat("\n")
    cat(paste0("Total energy: ",sum(sapply(p$resonators, function(x){if(is.na(x[1])) 0 else x$energy})),"\n"))
    cat("\n")
    i = i + 1
  }
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
  for (x in list(bs, ...)) {
    if (length(list(bs, ...)) > 1)
      cat(paste0("[",i,"]\n"))
    
    j = 1
    for (bp in bs) {
      cat(paste0("[",j,"] L",bp$burster$level," burster at (",bp$x,", ",bp$y,")\n"))
      j = j + 1
    }
    
    if (length(list(bs, ...)) > 1)
      cat("\n")
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
  for (x in list(ps, ...)) {
    if (length(list(ps, ...)) > 1)
      cat(paste0("[",i,"]\n"))
    
    j = 1
    for (p in ps) {
      cat(paste0("[",j,"] L",portal.level(p)," portal at (",p$x,", ",p$y,")\n"))
      j = j + 1
    }
    
    if (length(list(ps, ...)) > 1)
      cat("\n")
    i = i + 1
  }
}