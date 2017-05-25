library(raster)
library(rasterVis)
library(tidyverse)
library(rgdal)
library(cowplot)
library(viridis)
library(stationsweRegression)


# constants
DATEFILE='Wyoming_NewDates.csv'
RUNNAME='idwy'

PATH_SNOTEL=paste0(RUNNAME,'/data/snoteldownloads')
PATH_MODSCAGDOWNLOAD='./modscagdownloads'
PATH_FSCA=paste0(RUNNAME,'/data/fsca')
PATH_PHV=paste0(RUNNAME,'/data/phv')
PATH_OUTPUT=paste0(RUNNAME,'/output/w_obscuredstations')
PATH_MAPS=paste0(PATH_OUTPUT,'/swe_fsca_sidexside')

dir.create(PATH_MAPS,rec=TRUE)

whichdates <- import_dates(DATEFILE)
# whichdates %>% View
# whichdates <- whichdates %>% filter(yr==2008,doy==118)

irow=1
for(irow in 1:nrow(whichdates)){
	dte=whichdates$dte[irow]
	mth=strftime(dte,'%m')
	dy=strftime(dte,'%d')
	yr=whichdates$yr[irow]
	doy=whichdates$doy[irow]

	print(dte)

	mapfn=paste0(PATH_MAPS,'/maps_',yr,mth,dy,'.png')
	if(file.exists(mapfn)) next

	modscag_fn <- paste0(PATH_FSCA,'/modscag_fsca_',yr,doy,'.tif')
	fsca=raster(modscag_fn)

	phvfsca_fn <- paste0(PATH_OUTPUT,'/phvfsca_',yr,mth,dy,'.tif')
	phvfsca=raster(phvfsca_fn)


	gf <-
		gplot(fsca)+
		geom_raster(aes(x,y,fill=value))+
		coord_equal(expand=F)+
		scale_fill_distiller('fSCA',palette='YlGnBu',limits=c(0,100))+
		theme_cowplot(font_size=14)+
		theme(axis.line.x=element_line(color=NA),
					axis.line.y=element_line(color=NA))


	maxswe=max(phvfsca[phvfsca<250])
	gs <-
		gplot(phvfsca)+
		geom_raster(aes(x,y,fill=value))+
		coord_equal(expand=F)+
		scale_fill_distiller('SWE [m]',palette='PuBu',limits=c(0,maxswe),direction = -1)+
		theme_cowplot(font_size=14)+
		theme(axis.line.x=element_line(color=NA),
					axis.line.y=element_line(color=NA))

	pg <- plot_grid(gf,gs,nrow=2,align='hv')
	save_plot(plot=pg,filename=paste0(PATH_MAPS,'/maps_',yr,mth,dy,'.png'))
}
