library(RSAGA)#need to have saga installed
library(tidyverse)
library(rgdal)
library(rgeos)
library(raster)

# This file creates most of the input files.
# Download GMTED2010 DEM and use this as the basis for aspect, slope.
# Compute slope and aspect at local scale and regional scale. In this case, the regional scale is ~4km which should account for more generic weather patterns and is the max scale at which WRF simulations suggest are needed to reproduce weather patterns in complex terrain. see rasmussen et al 2011.
# Dissaggregate the 4km slope and aspect back to native dem resolution
# Derive x,y components for all slope and aspect
# Also computed distance to coast and distance to continental divide. These are more complicated and required "burniing" the coastline and divide line into the dem of the entire western US using gdal_rasterize. Then I used r.grow.distance in GRASS to compute the distance to those features. in the case of the distance to divide, pixels west of the divide are negative and pixels east of the divide are positive. see "create mask east/west divide" section below.


# setup workding dir ----
setwd('/Users/dosc3612/CUDrive/IDWY')
modeldir='rasters2model'
dir.create(modeldir,rec=TRUE)

# create dem for domain ----
gmted_fns <- dir(path='.',glob2rx('*gmted*.tif$'))
gmted_fns #check to make sure you are using the right files
gmted1 <- raster(gmted_fns[1])
gmted2 <- raster(gmted_fns[2])
#the pixels in gmted dataset don't exactly line up with our limits so merge and resample
dem=gdalUtils::gdalwarp(srcfile=gmted_fns,dstfile='IDWY_dem.tif',t_srs='+proj=longlat +datum=WGS84',te = c(-117.25,42,-106.15,49),tr=c(0.00416666667,0.0041666667),r='bilinear',dstnodata= '-99', output_Raster=TRUE,overwrite=TRUE)
file.copy('IDWY_dem.tif',file.path(modeldir,'IDWY_dem.tif'),overwrite=TRUE)

# create polygon of domain extent
dem_poly <- as(extent(dem),'SpatialPolygons')
proj4string(dem_poly) <- projection(dem)
dem_poly$ID=1
writeOGR(dem_poly,'IDWY.gpkg',layer='idwy',driver='GPKG',overwrite_layer=TRUE)

# lat/long variables ----
coords=coordinates(dem)
lat <- setValues(dem,coords[,2])
writeRaster(lat,'IDWY_lat.tif',NAflag=-99,overwrite=TRUE)
file.copy('IDWY_lat.tif',file.path(modeldir,'IDWY_lat.tif'),overwrite=TRUE)
lon <- setValues(dem,coords[,1])
writeRaster(lon,'IDWY_lon.tif',NAflag=-99,overwrite=TRUE)
file.copy('IDWY_lon.tif',file.path(modeldir,'IDWY_lon.tif'))


# local slope and slope derivatives ----
slope=gdalUtils::gdaldem('slope','IDWY_dem.tif','IDWY_slope.tif','GTiff',compute_edges = TRUE, s=111120,output_Raster=TRUE)
zness=sin(slope*pi/180)
writeRaster(zness,'IDWY_zness.tif',NAflag=-99,overwrite=T)
file.copy('IDWY_zness.tif',file.path(modeldir,'IDWY_zness.tif'))

# local aspect and aspect derivatives ----
aspect=gdalUtils::gdaldem('aspect','IDWY_dem.tif','IDWY_aspect.tif','GTiff',compute_edges = TRUE, s=111120,output_Raster=TRUE)

northness=cos(aspect*pi/180)
writeRaster(northness,'IDWY_northness.tif',NAflag=-99,overwrite=T)
file.copy('IDWY_northness.tif',file.path(modeldir,'IDWY_northness.tif'))

eastness=sin(aspect*pi/180)
writeRaster(eastness,'IDWY_eastness.tif',NAflag=-99,overwrite=T)
file.copy('IDWY_eastness.tif',file.path(modeldir,'IDWY_eastness.tif'))

# regional slope and derviatives
dem4km=aggregate(dem,8,na.rm=F,filename='IDWY_dem4km.tif',NAflag=-99,overwrite=T)

slope=gdalUtils::gdaldem('slope','IDWY_dem4km.tif','IDWY_slope4km.tif','GTiff',compute_edges = TRUE, s=111120,output_Raster=TRUE)
zness=sin(slope*pi/180)
writeRaster(zness,'IDWY_zness4km.tif',NAflag=-99,overwrite=T)
disaggregate(zness,8,filename='IDWY_regionalzness.tif',NAflag=-99,overwrite=T)
file.copy('IDWY_regionalzness.tif',file.path(modeldir,'IDWY_regionalzness.tif'))

# regional aspect and derivatives ----

aspect=gdalUtils::gdaldem('aspect','IDWY_dem4km.tif','IDWY_aspect4km.tif','GTiff',compute_edges = TRUE, s=111120,output_Raster=TRUE)
northness=cos(aspect*pi/180)
writeRaster(northness,'IDWY_northness4km.tif',NAflag=-99,overwrite=T)
disaggregate(northness,8,filename='IDWY_regionalnorthness.tif',NAflag=-99,overwrite=T)
file.copy('IDWY_regionalnorthness.tif',file.path(modeldir,'IDWY_regionalnorthness.tif'))

eastness=sin(aspect*pi/180)
writeRaster(eastness,'IDWY_eastness4km.tif',NAflag=-99,overwrite=T)
disaggregate(eastness,8,filename='IDWY_regionaleastness.tif',NAflag=-99,overwrite=T)
file.copy('IDWY_regionaleastness.tif',file.path(modeldir,'IDWY_regionaleastness.tif'))

# create mask for east/west of continental divide ----

# read polygon of modeling extent. save as shapefile for saga
ogrInfo('IDWY.gpkg')
box=readOGR('IDWY.gpkg',layer='idwy')
writeOGR(box,dsn='IDWY_polygon.shp',layer='IDWY_polygon',drive='ESRI Shapefile',overwrite_layer=TRUE)

# read reprojected Continental Divide file.
ogrInfo('ContDiv2m_wgs84.gpkg')
contdiv=readOGR('ContDiv2m_wgs84.gpkg',layer='ContDiv2m_wgs84')
# merge multilines as a single line for intersection to work
contdiv2=gLineMerge(contdiv)
contdiv2$ID=1
contdiv2
plot(contdiv2)
# save as shapefile for saga
writeOGR(contdiv2,dsn='ContDiv2m.shp',layer='ContDiv',drive='ESRI Shapefile')

plot(contdiv2)
plot(box,add=T)

# use saga-gis through r to find intersection of line and polygon and split polygon into multiple parts
RSAGA::rsaga.geoprocessor("shapes_polygons",
													"Polygon-Line Intersection",
													list(POLYGONS="IDWY_polygon.shp",
															 LINES="ContDiv2m.shp",
															 INTERSECT="IDWY-ContDivPolygons.shp"))

# read the split polygon file and assign different IDs for each polygon. -1 west of divide. 0 for the closed basin.
pli=readOGR('IDWY-ContDivPolygons.shp','IDWY-ContDivPolygons')
pli$FID=c(-1,1,0)
plot(pli,col=c('red','blue','green'))

# rasterize the split polygon with the FID values
rpoly=raster::rasterize(pli,dem,field='FID')
writeRaster(rpoly,'IDWY_ContDivPolygons_rasterized.tif',NAflag=-99,overwrite=T)

# mask distance to divide for west and east of barrier ----
dist2divide=raster('dist_to_contdiv.tif')#created in grassgis
dtd=crop(dist2divide,rpoly)
dist_to_contdiv_withdirection=dtd*rpoly
plot(dist_to_contdiv_withdirection)

writeRaster(dist_to_contdiv_withdirection,'IDWY_dist-to-contdiv_withdirection.tif',NAflag=-99,overwrite=TRUE)
file.copy('IDWY_dist-to-contdiv_withdirection.tif',file.path(modeldir,'IDWY_dist2contdiv.tif'))

# crop distance to coast to domain----
dist2coast=raster('dist_to_coast.tif')#created in grassgis
dtc=crop(dist2coast,dem)
writeRaster(dtc,'IDWY_dist-to-coast.tif',NAflag=-99,overwrite=TRUE)
file.copy('IDWY_dist-to-coast.tif',file.path(modeldir,'IDWY_dist2coast.tif'))
