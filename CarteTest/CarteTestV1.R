library(sf)

filename <- system.file("shape/nc.shp", package="sf")
nc <- st_read(filename)

plot(nc[3])

help(nc)
