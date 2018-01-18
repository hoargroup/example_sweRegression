library(stationsweRegression)
library(rgdal)
library(rgeos)
library(raster)
library(tidyverse)
library(glmnetUtils)
library(modelr)
library(cowplot)
library(viridis)
library(rasterVis)

# constants ----
DATEFILE = 'ucrb_2018dates.csv'
RUNNAME = 'ucrb'
SNOW_VAR = 'rcn'#rcn or fsca
PILLOW_NETWORK = 'snotel' #cdec (for CA) or snotel (for other locations)
PHV_VARS = ~lon+lat+dem+eastness+northness+dist2coast+dist2contdiv+regionaleastness+regionalnorthness+regionalzness+zness #don't forget the +'s and ~
EXTENT_NORTH = 43.75
EXTENT_EAST = -104.125
EXTENT_SOUTH = 33
EXTENT_WEST = -112.25
RESO = 15/3600

# input setup ----
PATH_MODSCAGDOWNLOAD='modscagdownloads/NRT'#always need this. path should point 1 level above /yr/doy/*.tif
PATH_RCNDOWNLOAD='rcn_ucrb'#don't need this if predicting with fsca instead of rcn
PATH_PHV=paste0(RUNNAME,'/data/phv')

# output directories ----
PATH_SNOTEL=paste0(RUNNAME,'/data/snoteldownloads')
PATH_FSCA=paste0(RUNNAME,'/data/fsca')
PATH_OUTPUT=paste0(RUNNAME,'/output/w_obscuredstations')
PATH_MAPS=paste0(PATH_OUTPUT,'/swe_fsca_sidexside')

# set up folders ----
dir.create(path=PATH_SNOTEL,rec=TRUE)
dir.create(path=PATH_FSCA,rec=TRUE)
dir.create(path=PATH_MAPS,rec=TRUE)

# import watermask ----
watermask <- raster(paste0(RUNNAME,'/data/gis/',toupper(RUNNAME),'_watermask.tif'))

# get station metadata ----
all_available_stations <- get_inv(PILLOW_NETWORK)
# filter stations by bounding box using latitude and longitude.
station_locations <- all_available_stations %>%
	filter(Latitude>=EXTENT_SOUTH, Latitude<=EXTENT_NORTH, Longitude<= EXTENT_EAST, Longitude>= EXTENT_WEST)

# create a spatial object of the locations ----
snotellocs=as.data.frame(station_locations)
coordinates(snotellocs)= ~Longitude+Latitude
coordnames(snotellocs)=c('x','y')
proj4string(snotellocs)='+proj=longlat +datum=WGS84'
# snotellocs.usgs=spTransform(snotellocs,'+init=epsg:5070')

# get phv variables ----
# get phv variables for entire domain from /data/phv folder and convert to a dataframe
# see use_package and Make_PHV_Inputs vignette
phvfilenames=dir(PATH_PHV,pattern='.tif$',full.names=TRUE)
phvstack=stack(phvfilenames)
names(phvstack) <- sapply(strsplit(names(phvstack),'_'),'[',2)
ucophv <- as.data.frame(phvstack)

#  extract the phv variable values for the station locations ----
phvsnotel=raster::extract(phvstack,snotellocs,sp=T)
phvsnotel=phvsnotel %>%
	as.data.frame() %>%
	tbl_df %>%
	mutate_if(is.factor,as.character) %>% 
	dplyr::select(Site_ID,site_name,dem:zness)

# import list of dates to simulate ----
whichdates <- import_dates(DATEFILE)
# View(whichdates)

# run estimate ----
# iterate through each date in the datefile to estimate the distribution of SWE
irow=1
for(irow in nrow(whichdates):1){#simulate in reverse will download less data
	# setup the date variables
	simdate=whichdates$dte[irow]
	yr=strftime(simdate,'%Y')
	doy=strftime(simdate,'%j')
	mth=strftime(simdate,'%m')
	dy=strftime(simdate,'%d')
	datestr=paste0(yr,mth,dy)
	
	print(simdate)
	
	# this checks for a map output that has swe and fsca side by side. remove this file if you want to make a new swe estimate
	mapfn=paste0(PATH_MAPS,'/maps_',yr,mth,dy,'.png')
	fe.logical=file.exists(mapfn)
	if(fe.logical) {
		print(paste0('map exists in ', PATH_MAPS,'. skipping.'))
		next
	}
	
	## filter stations by dates ----
	stations_available <- station_locations %>%
		filter(start_date<simdate) %>%
		filter(end_date>simdate)
	
	## download station swe data for the year of simulation date and merge with station locations ----
	# uses gethistoric.sh from https://www.wcc.nrcs.usda.gov/web_service/NWCC_Web_Report_Scripting.txt to download snotel data. this shell script comes with the package
	station_data=get_stationswe_data(stations_available,PILLOW_NETWORK)
	
	## get historical modscag image ----
	# get historical modscag image or use archived modscag images. see 'use_package' vignette
	simfsca <- get_modscag_data(doy,yr,'NRT',PATH_FSCA,RESO,EXTENT_WEST,EXTENT_EAST,EXTENT_SOUTH,EXTENT_NORTH)
	# Make sure fsca was properly retrieved!
	# plot(simfsca,zlim=c(0,100))
	
	## subset snotel data for simulation date ----
	snoteltoday <-
		station_data %>%
		filter(!is.na(Longitude),!is.na(Latitude)) %>% 
		dplyr::select(-snwd,-swe) %>%
		dplyr::filter_(~dte == simdate) %>%
		dplyr::filter(!is.na(snotel)) %>% #i think files get downloaded with no data so they stay in the joined files. need to remove these for the statistical model
		mutate(
			yr=strftime(dte,'%Y'),
			doy=strftime(dte,'%j'),
			yrdoy=strftime(dte,'%Y%j'),
			dy=strftime(dte,'%d'))
	
	## save current snotel as spatial vector file ----
	snoteltoday.sp=data.frame(snoteltoday)
	sp::coordinates(snoteltoday.sp)=~Longitude+Latitude
	proj4string(snoteltoday.sp)='+proj=longlat +datum=WGS84'
	snotelfilename=paste0(PATH_OUTPUT,'/snotel-',strftime(simdate,'%d%b%Y'),'.gpkg')
	if(!file.exists(snotelfilename)){
		writeOGR(snoteltoday.sp,dsn=snotelfilename,layer='snotel_swe',driver='GPKG',overwrite_layer=TRUE)
	}
	
	## setup the modeling data ----
	modelingdFs <- setup_modeldata(snoteltoday.sp,phvsnotel,simfsca,SNOW_VAR,PHV_VARS,PATH_RCNDOWNLOAD)
	doidata=modelingdFs[[1]]
	predictdF=modelingdFs[[2]]
	myformula=modelingdFs[[3]]
	
	## fit glmnet model ----
	mdl <- gnet_phvfsca(doidata,myformula)
	
	## predict on swe for domain and mask with fsca and watermask ----
	yhat=predict(mdl,predictdF,na.action=na.pass)
	simyhat=simfsca
	values(simyhat) <- yhat
	simyhat <- mask(simyhat,watermask,maskvalue=1,updatevalue=NA)
	simyhat <- mask(simyhat,simfsca)
	simyhat <- mask(simyhat,simfsca,maskvalue=235,updatevalue=235)
	simyhat <- mask(simyhat,simfsca,maskvalue=250,updatevalue=250)
	simyhat <- mask(simyhat,simfsca,maskvalue=0,updatevalue=0)
	
	## save prediction to file ----
	outfile=paste0('phv',SNOW_VAR,'_',yr,mth,dy,'.tif')
	writeRaster(simyhat,file.path(PATH_OUTPUT,outfile),NAflag=-99,overwrite=T)
	
	## create map of fsca and swe and save as image ----
	gf <-
		rasterVis::gplot(simfsca)+
		geom_raster(aes(x,y,fill=value))+
		coord_equal(expand=F)+
		scale_fill_distiller('fSCA',palette='YlGnBu',limits=c(0,100))+
		theme_cowplot(font_size=14)+
		theme(axis.line.x=element_line(color=NA),
					axis.line.y=element_line(color=NA))
	
	
	maxswe=max(simyhat[simyhat<200])
	gs <-
		rasterVis::gplot(simyhat)+
		geom_raster(aes(x,y,fill=value))+
		coord_equal(expand=F)+
		scale_fill_distiller('SWE [m]',palette='PuBu',limits=c(0,maxswe),direction = -1)+
		theme_cowplot(font_size=14)+
		theme(axis.line.x=element_line(color=NA),
					axis.line.y=element_line(color=NA))
	
	pg <- cowplot::plot_grid(gf,gs,nrow=2,align='hv')
	save_plot(plot=pg,filename=paste0(PATH_MAPS,'/phv',SNOW_VAR,'_maps_',yr,mth,dy,'.png'),base_height = 6)
	
	## save coefficients from model and write to file ----
	coef_dF <-
		as.data.frame(as.matrix((coef(mdl)))) %>%
		mutate(predictor=rownames(.)) %>%
		setNames(c('coefficient','predictor'))
	
	write_tsv(format(coef_dF,sci=FALSE),
						path=paste0(PATH_OUTPUT,'/phv',SNOW_VAR,'_coefs_',datestr,'.txt')
	)
	
	## cross validation statistics  ----
	cl=NULL
	allmdls <-
		doidata %>%
		group_by(dte) %>%
		do({
			#crossv_mc uses random sampling (with replacement?) to crossvalidate. loocv is also possible with crossv_kfold(...,k=nrow(.)) but it will take much longer. Also, r2 needs to be calculated outside this loop because currenlty the skill metric can't caluclate r2 with just 1 test point
			datsplit <- 
				crossv_mc(.,n=30,test=0.1)	%>%
				mutate(
					phvfsca_obj_glmmdl=map(train,gnet_phvfsca,myformula,cl),
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
	
	write_tsv(stationsweRegression::format_numeric(stat_r2,sci=FALSE),
						path=paste0(PATH_OUTPUT,'/phv',SNOW_VAR,'_r2_',datestr,'.txt'))
	
	stat_pctmae <-
		allmdls %>%
		unnest(phvfsca_pctmae_glmmdl,.drop=T) %>%
		summarise(
			avg_pctmae=mean(phvfsca_pctmae_glmmdl,na.rm=T),
			sd_pctmae=sd(phvfsca_pctmae_glmmdl,na.rm=T),
			uci_pctmae=avg_pctmae+1.96*sd_pctmae/sqrt(n()),
			lci_pctmae=avg_pctmae-1.96*sd_pctmae/sqrt(n())
		)
	
	write_tsv(format_numeric(stat_pctmae,sci=FALSE),
						path=paste0(PATH_OUTPUT,'/phv',SNOW_VAR,'_pctmae_',datestr,'.txt'))
	
}


# combine crossvalidation statistics from each date into a single file ----
search_str=paste0('^phv',SNOW_VAR,'_r2*.txt$')
r2_fns <- dir(PATH_OUTPUT,glob2rx(search_str),full.names = TRUE)
r2_fns <- r2_fns[!grepl('combined',r2_fns)]
r2_df <- suppressMessages(
	map_df(.x=r2_fns,.f=read_tsv)
)
write_tsv(format_numeric(r2_df,sci=FALSE),
					path=file.path(PATH_OUTPUT,paste0('phv',SNOW_VAR,'_r2_combined.txt')))

search_str=paste0('^phv',SNOW_VAR,'_pctmae*.txt$')
pctmae_fns <- dir(PATH_OUTPUT,glob2rx(search_str),full.names = TRUE)
pctmae_fns <- pctmae_fns[!grepl('combined',pctmae_fns)]
pctmae_df <- suppressMessages(
	map_df(.x=pctmae_fns,.f=read_tsv)
)
write_tsv(pctmae_df,
					path=file.path(PATH_OUTPUT,paste0('phv',SNOW_VAR,'_pctmae_combined.txt')))

