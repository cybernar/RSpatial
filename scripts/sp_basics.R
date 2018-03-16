
## Some GPS coordinates
library(sp)
lon <- c(3.86379, 3.86291, 3.86243, 3.86220, 3.86314)
lat <- c(43.63838, 43.63878, 43.63863, 43.63821, 43.63810)
name <- c("AA", "BB", "CC", "DD", "EE")
color <- c("green", "green", "green", "blue", "blue")
df <- data.frame(name, lon, lat, color)

## The SpatialPoints class

matcoords <- as.matrix(df[,c("lon","lat")])
spts <- SpatialPoints(matcoords, proj4string = CRS("+proj=longlat +datum=WGS84"))
slotNames(spts)

## Distances between the points

matdist_meters <- spDists(spts, y=spts, longlat=T) * 1000
matdist_meters

lsegments_meters <- spDists(spts, longlat=T, segments=T) * 1000
lsegments_meters

## Building a SpatialPointDataFrame from a data.frame with coordinates
spts_df <- df
# turn a data.frame into a SpatialPointsDataFrame by providing X Y columns
coordinates(spts_df) <- c("lon","lat") 
# define the CRS (optional)
proj4string(spts_df) <- CRS("+init=EPSG:4326")
slotNames(spts_df)

## Saving the points under KML and Shapefile format
library(maptools)
# omit the extension to writer some_points.shp ...
writePointsShape(spts_df,"some_points")
# let us create 3 green markers and 2 blue markers
url_color_markers <- paste0("http://maps.google.com/mapfiles/ms/micons/",spts_df$color,".png")
kmlPoints(spts_df,kmlfile="points_TE.kml",name=spts_df$name, icon=url_color_markers)

# build 2 Lines object with ID slot = L1 and L2
matcoords1 <- as.matrix(df[,c("lon","lat")])
matcoords2 <- cbind(runif(5, -0.001, 0.001) + 3.8676, runif(5, -0.001, 0.001) + 43.6423)
line_1 <- Line(matcoords1)
line_2 <- Line(matcoords2)
lines_1 <- Lines(list(line_1), "L1")
lines_2 <- Lines(list(line_2), "L2")
splines <- SpatialLines(list(lines_1, lines_2))
str(splines)


# build a data.frame object with 2 columns and ID as the rows names.
NAME=c("LINE1", "RANDOM2")
LENGTH_M = SpatialLinesLengths(splines, longlat=T) * 1000
df_demo <- data.frame(NAME, LENGTH_M)
row.names(df_demo) <- c("L1","L2")
splines_df <- SpatialLinesDataFrame(splines, df_demo)
## save the SpatialLinesDataFrame as a shapefile
writeLinesShape(splines_df, fn="some_lines")

