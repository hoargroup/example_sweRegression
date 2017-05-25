# library(raster)
# library(tidyverse)
library(stationsweRegression)
# library(rgdal)
# library(glmnetUtils)
# library(modelr)
# library(cowplot)
# library(viridis)

# input setup
PATH_MODSCAGDOWNLOAD='modscagdownloads'
PATH_PHV=paste0(RUNNAME,'/data/phv')

# output directories
PATH_SNOTEL=paste0(RUNNAME,'/data/snoteldownloads')
PATH_FSCA=paste0(RUNNAME,'/data/fsca')
PATH_OUTPUT=paste0(RUNNAME,'/output-w_obscuredstations')
PATH_MAPS=paste0(PATH_OUTPUT,'/swe_fsca_sidexside')

# constants
DATEFILE='ucrb_dates.csv'
RUNNAME='ucrb'
EXTENT_NORTH = 43.75
EXTENT_EAST = -106.125
EXTENT_SOUTH = 33
EXTENT_WEST = -112.25
RESO = 15/3600

# ------ set up folders
dir.create(path=PATH_SNOTEL,rec=TRUE)
dir.create(path=PATH_FSCA,rec=TRUE)
dir.create(path=PATH_OUTPUT,rec=TRUE)

# import watermask
watermask <- raster(paste0(RUNNAME,'/data/gis/',toupper(RUNNAME),'_watermask.tif'))

# ------- get station metadata
all_available_stations <- get_inv('snotel')
# filter stations by bounding box using latitude and longitude.
station_locations <- all_available_stations %>%
	# filter(Latitude>=33, Latitude<=43.75, Longitude<= -104.125, Longitude>= -112.25)
	filter(Latitude>=EXTENT_SOUTH, Latitude<=EXTENT_NORTH, Longitude<= EXTENT_EAST, Longitude>= EXTENT_WEST)

# # create a spatial object of the locations
snotellocs=as.data.frame(station_locations)
coordinates(snotellocs)= ~Longitude+Latitude
coordnames(snotellocs)=c('x','y')
proj4string(snotellocs)='+proj=longlat +datum=WGS84'
# snotellocs.usgs=spTransform(snotellocs,'+init=epsg:5070')

# get phv variables for entire domain
phvfilenames=dir(PATH_PHV,pattern='.tif$',full.names=TRUE)
phvstack=stack(phvfilenames)
names(phvstack) <- sapply(strsplit(names(phvstack),'_'),'[',2)

phvsnotel=raster::extract(phvstack,snotellocs,sp=T)
phvsnotel=phvsnotel %>%
	as.data.frame() %>%
	tbl_df %>%
	dplyr::select(Station_ID,Site_ID,site_name,dem:zness) %>%
	mutate_if(is.factor,as.character)

ucophv <- as.data.frame(phvstack)

# import list of dates to simulate
whichdates <- import_dates(DATEFILE)
# View(whichdates)


# ### --- sample date. which date are we simulating? best cloud free image from https://worldview.earthdata.nasa.gov
# simdate=as.Date('2001-04-26')
# yr=strftime(simdate,'%Y')
# doy=strftime(simdate,'%j')
# mth=strftime(simdate,'%m')
# dy=strftime(simdate,'%d')

irow=1
for(irow in 1:nrow(whichdates)){
	simdate=whichdates$dte[irow]
	yr=strftime(simdate,'%Y')
	doy=strftime(simdate,'%j')
	mth=strftime(simdate,'%m')
	dy=strftime(simdate,'%d')
	datestr=paste0(yr,mth,dy)

	print(simdate)

	mapfn=paste0(PATH_MAPS,'/maps_',yr,mth,dy,'.png')
	fe.logical=file.exists(mapfn)
	if(fe.logical) {
		print(paste0('map exists in ', PATH_MAPS,'. skipping.'))
		next
	}

# filter stations by dates
stations_available <- station_locations %>%
	filter(start_date<simdate) %>%
	filter(end_date>simdate)

# download station swe data for the year of simulation date and merge with station locations
station_data=get_stationswe_data(yr,stations_available,'snotel')

# --- get modscag NRT image or use archived modscag images
simfsca <- get_modscag_data(doy,yr,'historic',PATH_FSCA,RESO,EXTENT_WEST,EXTENT_EAST,EXTENT_SOUTH,EXTENT_NORTH)
simfscafilename=paste0(PATH_FSCA,'/modscag_fsca_',yr,doy,'.tif')
simfsca=raster(simfscafilename)
# plot(simfsca,zlim=c(0,100))

## ----- subset snotel data for simulation date
snoteltoday <-
	station_data %>%
	dplyr::select(-snwd,-swe) %>%
	dplyr::filter_(~dte == simdate) %>%
	dplyr::filter(!is.na(snotel)) %>% #i think files get downloaded with no data so they stay in the joined files
	mutate(
		yr=strftime(dte,'%Y'),
		doy=strftime(dte,'%j'),
		yrdoy=strftime(dte,'%Y%j'),
		dy=strftime(dte,'%d'))

## save current snotel as spatial vector file
snoteltoday.sp=data.frame(snoteltoday)
sp::coordinates(snoteltoday.sp)=~Longitude+Latitude
proj4string(snoteltoday.sp)='+proj=longlat +datum=WGS84'
snotelfilename=paste0(PATH_OUTPUT,'/snotel-',strftime(simdate,'%d%b%Y'),'.gpkg')
if(!file.exists(snotelfilename)){
  writeOGR(snoteltoday.sp,dsn=snotelfilename,layer='snotel_swe',driver='GPKG',overwrite_layer=TRUE)
}


# ## ---- merge snotelrecon data with snotel swe data and phv data
newnames=c(names(snoteltoday.sp),'fsca')
snotel_snow <-
	raster::extract(simfsca,snoteltoday.sp,sp=T) %>%
	as.data.frame() %>%
	tbl_df %>%
	dplyr::select(-Longitude,-Latitude) %>%
	setNames(newnames) %>%
	mutate_if(is.factor,as.character)

swedata=inner_join(snotel_snow,phvsnotel,by=c('Station_ID','Site_ID'))

## ------ combine fsca with phv data
predictdF <- bind_cols(ucophv,as.data.frame(simfsca) %>% setNames('fsca')) %>% tbl_df

# ucophvsc <- scale(ucophv)
# avg=attr(ucophvsc,'scaled:center')
# std=attr(ucophvsc,'scaled:scale')
# varind=which(names(phvsnotel) %in% names(ucophvsc))
# for(i in varind){
#   ucoind=which(names(avg) %in% names(phvsnotel)[i])
#   phvsnotel[,i]=(phvsnotel[,i]-avg[ucoind])/std[ucoind]
# }

## ---- merge today's snotel data and phv data and fit model
doidata=inner_join(snoteltoday,phvsnotel,by=c('Station_ID','Site_ID')) %>%
	inner_join(snotel_snow) %>%
	mutate(fsca=ifelse(fsca>100 & snotel>0,100,fsca)) %>%
	mutate(fsca=ifelse(fsca==0 & snotel>0,15,fsca)) %>%
	filter(fsca<=100) %>%
	mutate(swe=snotel)#*fsca/100)

mdl=gnet_phvfsca(doidata)

coef_dF <-
	as.data.frame(as.matrix((coef(mdl)))) %>%
	mutate(predictor=rownames(.)) %>%
	setNames(c('coefficient','predictor'))

write_tsv(format(coef_dF,sci=FALSE),
						path=paste0(PATH_OUTPUT,'/phvfsca_coefs_',datestr,'.txt')
						)

cl=NULL
allmdls <-
	doidata %>%
	group_by(dte) %>%
	do({
		#crossv_mc uses random sampling (with replacement?) to crossvalidate. loocv is also possible with crossv_kfold(...,k=nrow(.)) but it will take much longer. Also, r2 needs to be calculated outside this loop because currenlty the skill metric can't caluclate r2 with just 1 test point
		datsplit=crossv_mc(.,n=50,test=0.1)	%>%
			mutate(
				phvfsca_obj_glmmdl=map(train,gnet_phvfsca,cl),
				phvfsca_r2_glmmdl=map2_dbl(phvfsca_obj_glmmdl,test,myr2),
				phvfsca_pctmae_glmmdl=map2_dbl(phvfsca_obj_glmmdl,test,mypctmae)
			)
	})

stat_r2 <-
	allmdls %>%
	unnest(phvfsca_r2_glmmdl,.drop=T) %>%
	summarise(
		avg_r2=mean(phvfsca_r2_glmmdl,na.rm=T),
		sd_r2=sd(phvfsca_r2_glmmdl,na.rm=T),
		uci_r2=avg_r2+1.96*sd_r2/sqrt(n()),
		lci_r2=avg_r2-1.96*sd_r2/sqrt(n())
	)

write_tsv(format(stat_r2,sci=FALSE),
					path=paste0(PATH_OUTPUT,'/phvfsca_r2_',datestr,'.txt'))

stat_pctmae <-
	allmdls %>%
	unnest(phvfsca_pctmae_glmmdl,.drop=T) %>%
	summarise(
		avg_pctmae=mean(phvfsca_pctmae_glmmdl,na.rm=T),
		sd_pctmae=sd(phvfsca_pctmae_glmmdl,na.rm=T),
		uci_pctmae=avg_pctmae+1.96*sd_pctmae/sqrt(n()),
		lci_pctmae=avg_pctmae-1.96*sd_pctmae/sqrt(n())
	)

write_tsv(format(stat_pctmae,sci=FALSE),
					path=paste0(PATH_OUTPUT,'/phvfsca_pctmae_',datestr,'.txt'))

yhat=predict(mdl,predictdF)
simyhat=simfsca
values(simyhat) <- yhat
simyhat <- mask(simyhat,watermask,maskvalue=1,updatevalue=NA)
simyhat <- mask(simyhat,simfsca)
simyhat <- mask(simyhat,simfsca,maskvalue=250,updatevalue=250)
simyhat <- mask(simyhat,simfsca,maskvalue=0,updatevalue=0)
# spplot(simyhat)

outfile=paste0('phvfsca_',yr,mth,dy,'.tif')
writeRaster(simyhat,file.path(PATH_OUTPUT,outfile),NAflag=-99,overwrite=T)

}

r2_fns <- dir(PATH_OUTPUT,glob2rx('^phvfsca_r2*.txt$'),full.names = TRUE)
r2_fns <- r2_fns[!grepl('combined',r2_fns)]
r2_df <- suppressMessages(
	map_df(.x=r2_fns,.f=read_tsv)
)
write_tsv(r2_df,
					path=file.path(PATH_OUTPUT,'phvfsca_r2_combined.txt'))

# r2_with <- r2_df %>% mutate(stainclusion='with')
# r2_without <- r2_df %>% mutate(stainclusion='without')
#
# bind_rows(r2_with,r2_without) %>%
# 	group_by(stainclusion) %>%
# 	summarise(
# 		mean(avg_r2)
# 	)

pctmae_fns <- dir(PATH_OUTPUT,glob2rx('^phvfsca_pctmae*.txt$'),full.names = TRUE)
pctmae_fns <- pctmae_fns[!grepl('combined',pctmae_fns)]
pctmae_df <- suppressMessages(
	map_df(.x=pctmae_fns,.f=read_tsv)
)
write_tsv(pctmae_df,
					path=file.path(PATH_OUTPUT,'phvfsca_pctmae_combined.txt'))

# mae_without <- pctmae_df %>% mutate(stainclusion='without')
# mae_with <- pctmae_df %>%  mutate(stainclusion='with')
#
# bind_rows(mae_with,mae_without) %>%
# 	group_by(stainclusion) %>%
# 	summarise(
# 		mean(avg_pctmae)
# 	)
