# Converts output from reconstruction from .dat to .nc such that raster pkg in R will read the time dim correctly
# Assumes the first day in the record is March 1

require(raster)
require(ncdf4)
library(tidyverse)

# USER INPUT ----
## file management
# point recon_grads to a folder that contains the yearly folders in which swe.dat exists. folder should be absolute or relative to .Rproj file.
# output files will be local to the .Rproj file
recon_grads='recon_grads' #'/Volumes/hydroProjects/SWE/Sierras/Spatial_SWE/SWE_SNODIS'

## Sierra Nevada reconstruction
domain <- 'snm'
numcols <- 1140
numrows <- 1500
numdays <- 184
grid_spacing <- 15/3600
#The coordinates in a grads .ctl file are cell center. R raster uses cell extremes
lowerleft_lon <- -122.372916666666667 - grid_spacing/2 
lowerleft_lat <- 35.127083333333333 - grid_spacing/2

# ## UCRB reconstruction
# domain <- 'ucrb'
# numcols <- 1950
# numrows <- 2580
# numdays <- 184
# lowerleft_lon <- -112.25 #these are already cell extremes
# lowerleft_lat <- 33.0
# grid_spacing <- 15/3600

# FUNCTION ----
readgrads=function(t,fid=fid){
  numel=numcols*numrows
  dtype=4
  tindex=t*numel-numel+1
  # print(tindex/numel+1)
  #print(dtype*tindex-dtype)
  seek(fid, where = dtype*tindex-dtype,origin = "start") ## seek is 0 index based!
  num=readBin(fid,size=dtype,n=numel,what='numeric')
  # print(length(num))
  maty=matrix(num,byrow=T,ncol=numcols)
  mat=maty[nrow(maty):1,]
  return(mat)
}

# GO TIME ----
## (if there are other folders than the year folders in recon_grads this will fail)
tifdir <- 'recon_tif'
ncdir <- 'recon_nc'
dir.create(tifdir)
dir.create(ncdir)

fns <- dir(recon_grads,full.names=T)
dns <- fns[file.info(fns)$isdir]

yrdir <- dns[1]
for(yrdir in dns){
  yr <- basename(yrdir)
  print(yr)
  fid=file(file.path(yrdir,'swe.dat'),'rb')
  days=1:numdays
  rlist <- map(days,function(dy) {
    raster(
      readgrads(dy,fid),
      crs='+proj=longlat +datum=WGS84',
      xmn=lowerleft_lon,
      xmx=lowerleft_lon+numcols*grid_spacing,
      ymn=lowerleft_lat,
      ymx=lowerleft_lat+numrows*grid_spacing)
  }
  )
  close(fid)
  s=stack(rlist)
  dates <- seq(as.Date(paste0(yr,'-03-01')),by=1,length.out=numdays)
  snames <- strftime(dates,'%Y%m%d')
  names(s) <- snames
  
  ## write geotifs
  writeRaster(x=s,filename=file.path(tifdir,paste0('recon_',domain,'.tif')), NAflag=-99, bylayer=T, suffix='names')

  ## write netcdf. writeRaster will do most of the heavy lifting
  fn_w <- paste0('recondata_',domain,'_',yr,'.nc')
  writeRaster(x=s,filename=file.path(ncdir,fn_w),NAflag=-99,varname='swe',varunit='meters',longname='snow water equivalent (m)',xname='longitude',yname='latitude',zname='time',zunit='yyyymmdd', overwrite=T)
  #but it doesn't write the time dimension properly for some reason
  ncid=nc_open(file.path(ncdir,fn_w),write=T)
  ncvar_put(ncid,varid='time',vals=as.numeric(snames))
  nc_close(ncid)
}



