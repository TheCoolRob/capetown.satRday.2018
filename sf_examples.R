
library(ggplot2)  #install.packages('devtools') #so that you can run the next line to get the right version of ggplot
                  #devtools::install_github("tidyverse/ggplot2") #Note: this dev version has the geom_sf()
library(purrr)    #install.packages("purrr")
library(sf)       #install.packages("sf")
library(dplyr)    #install.packages("dplyr")
library(lwgeom)   #install.packages('lwgeom') # needed for the st_area calc on proj.
library(leaflet)  #install.packages("leaflet")

# The sf package provides a new standard in how to wrangle geometries in R. 
# There are several interfaces into and out of the package, but most interestingly - you can use the package with the %>% syntax for pipe lining operations.
# This means you can handle geometries in a almost query-like fashion which was not possible with the sp pacakge. 
# sf represents an attempt to provide a handful of simple structures that ultimately support the bulk of typical geometry. 
# This talk will demonstrate some fun boiler-plate code that will showcase the operations and flexibility of sf.
# A light introduction to geometric principles will also be provided.

xy <- data.frame(x = round(runif(10), 2), y = round(runif(10), 2))
xy$geom<-  st_geometry(st_as_sf(xy,coords = c("x", "y")))

ggplot() + geom_sf(data = xy, aes(geometry = geom), size = 5, col = '#283848')
#Nice, we can plot some random points.

ls<- xy$geom %>% st_cast("MULTIPOINT") %>% st_cast("POINT") #cooercing the points into one collection
ls<- ls[1:4] %>% st_union() %>% st_cast("LINESTRING") %>% as.data.frame() #grabbing just the first 4 points to plot as a line

#have a look at ls, it's just a line string now.
ggplot() + geom_sf(data = xy, aes(geometry = geom), size = 5, col = 'grey') + geom_sf(data = ls, col = '#283848', lwd = 1.5)

ls<- xy$geom %>% st_union() %>% st_convex_hull() %>% as.data.frame()
#an example of making a single conve hull
ggplot() + geom_sf(data = xy, aes(geometry = geom), size = 5) + geom_sf(data = ls, lwd = 1.5, fill= '#283848', alpha =0.5)


sf_df<- xy %>% summarise(geometry = st_union(geom)) 

#lets look at sp:: we don't "love" this package.
library(sp)
xc = round(runif(10), 2)
yc = round(runif(10), 2)
xy = cbind(xc, yc)
xy.sp = SpatialPoints(xy)
str(xy.sp)                #this is what an sp object looks like.

xy.sp %>% select(coords)  #it does not play nice with dplyr - this easy to read line fails.
plot(xy.sp)               #but works with base R pretty well


xy.sp$stuff<- data.frame(morestuff = runif(10))
xy.sp %>% names()         #note we don't actually have a named column for the coordinates

xy.sp$col <- 1:10
s<- data.frame(col = sample(1:10, 5), rand = rnorm(5))

s %>% left_join(xy.sp)      #we can't "just" use dplyr straight

s %>% left_join(xy.sp@data) #we can cheat, but look, we lost the darn coordinates! this is why sf is much better than sp.


#OKAY, lets try doing some simple stuff with sf. 

xy = data.frame(x = round(runif(10),2), 
                y = round(runif(10),2))

xy$geom<-  st_geometry(st_as_sf(xy,coords = c("x", "y")))

sf_df<- xy %>% summarise(geometry = st_union(geom)) 
  
ggplot(sf_df) + geom_sf()       #nice, this just plots

plot(sf_df$geometry, pch = 16)  #if you want to plot in base R
plot(st_convex_hull(sf_df$geometry), add = T) #this also looks nice and simple


n= 20
xy = data.frame(x = runif(n), y = runif(n))
xy$id <- 1:n
xy$geom<-  st_geometry(st_as_sf(xy,coords = c("x", "y")))
xy$cluster<-  (xy %>% select(x, y) %>% kmeans(3))$cluster %>% as.character() #make some random sensible looking cluster.

xy %>% st_as_sf() %>%  arrange(cluster) %>% as.data.frame() #nice, the geom column arranges with the rest of the df as we'd expect
xy %>% st_as_sf() %>%  as.data.frame() %>% group_by(cluster) %>% summarise(mx = mean(x), my = mean(y))
                                                            #dplyr still does summarise nicely.

ggplot(data = xy) + geom_sf(aes(geometry = geom)) + geom_text(aes(label = cluster, x = x, y = y, hjust = -0.5, vjust = -0.5))

xg<- xy %>% group_by(cluster) %>% summarise(geometry = st_union(geom)) #wonderful, a collapsed geometry that hasn't lost any information

ggplot() + geom_sf(data = xg %>% mutate(geometry = st_convex_hull(geometry)), 
                   aes(fill = cluster), alpha = 0.5) + 
  geom_sf(data = xg)

#what about edges? or lines
#we want a line from each centroid to all points in the cluster.
centroids<- xy %>% group_by(cluster) %>% summarise(mx = mean(x), my = mean(y))

tmp<- xy %>% 
  left_join(centroids) %>% 
  mutate(geom2 = st_geometry(st_as_sf(cbind(mx, my) %>% as.data.frame(), coords = c("mx", "my")))) %>%
  group_by(id) %>% 
  mutate(geometry = st_cast(st_union(geom, geom2), to = 'LINESTRING') ) %>% 
  select(-c(mx,my,geom,geom2))
  #it's a bit icky, but it's not that complex. We're creating a line using the centroid and each point (to technically an edge)


ggplot() +
  geom_sf(data = xg)+
  geom_sf(data = tmp, aes(col = cluster))

#but wait, there's more.
#now we can voronoi all this stuff.

b1<- scale_x_continuous(limits = c(0,1))
b2<- scale_y_continuous(limits = c(0,1))


v<- st_union(xy$geom) %>% st_voronoi() %>% st_cast() %>%  data.frame() %>%  st_as_sf()
plot(v,xlim = c(0,1), ylim = c(0,1))

#this is kinda an important step. It works out which voronoi cell actually 
xy$voronoi<- v$geometry[xy$geom %>% st_within(v) %>% as.numeric()]
xy

ggplot() + geom_sf(data = xy, aes(geometry = voronoi, fill = cluster), col = 'black') +
  geom_sf(data = tmp, col = "light gray", alpha = 0.5)+
  geom_sf(data = centroids %>% st_as_sf(coords = c("mx", "my")), size =3, col = '#283848') + 
  geom_sf(data = xy, aes(geometry = geom), alpha = 0.5) + 
  b1 + b2

#okay, so now how about some serious fun with shape files.
#http://thematicmapping.org/downloads/world_borders.php

the_world<- st_read('TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp')
the_world %>% head

#pdf(height = 25, width = 50, 'test_world.pdf') #I pdf'd a high res version of this for the presso
ggplot(the_world) + geom_sf(aes(fill = AREA))
#dev.off()


world2 <- sf::st_transform(the_world, "+proj=laea +y_0=0 +lon_0=25 +lat_0=-28 +ellps=WGS84 +no_defs")
                                      # because JHB is awsome - we'll reorientate the map"
#pdf(height = 25, width = 50, 'test_world.pdf')
ggplot() + geom_sf(data = world2,aes(fill = AREA))
#dev.off()

ggplot(the_world) + geom_sf(aes(fill = as.character(REGION)))
ggplot(world2) + geom_sf(aes(fill = as.character(REGION)))    #yup, we agree, sf is sexy

library(leaflet)  #install.packages("leaflet")
names(the_world)
hlopt<- highlightOptions(weight = 5,
                         color = "#666",
                         dashArray = "",
                         fillOpacity = 0.7,
                         bringToFront = TRUE)

m <- leaflet() %>% addTiles() %>% addPolygons(data = the_world,
                                              label = ~NAME,
                                              highlight = hlopt)
m         #cool, nice plot, can walk them through parts of this.


#See: https://github.com/r-spatial/sf/issues/275
library(tilegramsR)  #install.packages("tilegramsR")
evs <- tilegramsR::Pitch_US_Population_2016_v1

plot(evs) # Each polygon is an electoral vote
sf_evs <- st_as_sf(evs)
st_precision(sf_evs) = 10000
plot(sf_evs$state)
#this is pretty too, but there's more prettyness coming..

sf_evs_states <- dplyr::group_by(sf_evs, state) %>%
  dplyr::summarise(geometry = st_union(geometry)) %>%
  dplyr::ungroup()

plot(sf_evs_states['state'])

#that's just very very pretty.
