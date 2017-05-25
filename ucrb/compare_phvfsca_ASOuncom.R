# 
# ## something else -----
# 
# uncom=raster('UB20150430_SUPERswe_50p0m_agg_.tif')
# res(uncom) <- c(50,50)
# setMinMax(uncom)
# uncom[uncom==0] <- NA
# uncom500=aggregate(uncom,10,fun=mean,na.rm=T)
# plot(uncom500,colNA='black')
# 
# uncom_poly=as(extent(uncom500),'SpatialPolygons')
# proj4string(uncom_poly) <- proj4string(uncom500)
# uncom_box=spTransform(uncom_poly,proj4string(simyhat))
# 
# simyhat2=simyhat
# simyhat2[simyhat2>200] <- NA
# simyhat2[simyhat2==0] <- NA
# sim_uncom=simyhat2#crop(simyhat2,uncom_box)
# plot(sim_uncom,colNA='black')
# sim_utm=projectRaster(sim_uncom,uncom500,method = 'bilinear')
# plot(sim_utm)
# 
# sim_masked <- mask(sim_utm,uncom500)
# plot(stack(uncom500,sim_masked))
# 
# 
# 
# 
# rdf <-
# 	as.data.frame(uncom500,xy=T) %>%
# 	tbl_df %>%
# 	bind_cols(as.data.frame(sim_masked)) %>%
# 	setNames(c('x','y','obs','phvfsca')) %>%
# 	mutate(diffphvfsca=phvfsca-obs)
# 
# mdlcor <-
# 	rdf %>%
# 	# dplyr::select(-contains('diff')) %>%
# 	summarise(
# 		corphvfsca=cor(obs,phvfsca,use='complete.obs')
# 		# correconrt=cor(aso,reconrt,use='complete.obs')
# 	)
# knitr::kable(mdlcor)
# 
# mdlbias <-
# 	rdf %>%
# 	summarise(
# 		avgaso=mean(obs,na.rm=T),
# 		avgphvfsca=mean(phvfsca,na.rm=T),
# 		# avgphvrcn=mean(phvrcn,na.rm=T),
# 		# avgreconrt=mean(reconrt,na.rm=T),
# 		biasphvfsca=mean(diffphvfsca,na.rm=T),
# 		# biasphvrcn=mean(diffphvrcn,na.rm=T),
# 		# biasreconrt=mean(diffreconrt,na.rm=T),
# 		pctbiasphvfsca=biasphvfsca/mean(obs,na.rm=T)*100
# 		# pctbiasphvrcn=biasphvrcn/mean(aso,na.rm=T)*100
# 		# pctbiasreconrt=biasreconrt/mean(aso,na.rm=T)*100
# 	)
# knitr::kable(mdlbias)
# 
# mdlmae <-
# 	rdf %>%
# 	summarise(
# 		avgaso=mean(obs,na.rm=T),
# 		avgphvfsca=mean(phvfsca,na.rm=T),
# 		# avgphvrcn=mean(phvrcn,na.rm=T),
# 		# avgreconrt=mean(reconrt,na.rm=T),
# 		maephvfsca=mean(abs(diffphvfsca),na.rm=T),
# 		# maephvrcn=mean(abs(diffphvrcn),na.rm=T),
# 		# maereconrt=mean(abs(diffreconrt),na.rm=T),
# 		pctmaephvfsca=maephvfsca/mean(obs,na.rm=T)*100
# 		# pctmaephvrcn=maephvrcn/mean(aso,na.rm=T)*100
# 		# pctmaereconrt=maereconrt/mean(aso,na.rm=T)*100
# 	)
# knitr::kable(mdlmae)
# 
# 
# ## compare to ASO in Uncompaghre 2015-04-30 ----
# 
# simfsca2=simfsca
# simfsca2[simfsca2>100] <- NA
# simfsca_uncom=crop(simfsca2,uncom_box)
# plot(simfsca_uncom,colNA='black')
# simfsca_utm=projectRaster(simfsca_uncom,uncom500,method = 'bilinear')
# simfsca_utm <- mask(simfsca_utm,uncom500)
# 
# fscadF <- as.data.frame(simfsca_utm,xy=T) %>%
# 	tbl_df %>%
# 	setNames(c('x','y','fsca'))
# 
# ggf <-
# 	ggplot(fscadF,aes(x,y,fill=fsca))+
# 	geom_raster()+
# 	coord_fixed()+
# 	labs(x='Easting [m]',
# 			 y='Northing [m]')+
# 	scale_fill_viridis(option='A', guide='colorbar',direction=1)+
# 	guides(fill=guide_colorbar('fSCA [%]'))+
# 	theme(axis.text.x=element_text(angle=45,hjust=1),
# 				legend.position = 'bottom',
# 				legend.text=element_text(angle=45))
# 
# ggr1 <-
# 	rdf %>%
# 	dplyr::select(-contains('diff')) %>%
# 	gather(source,swe,obs:yhat) %>%
# 	ggplot(aes(x,y,fill=swe))+
# 	geom_raster()+
# 	coord_fixed()+
# 	labs(x='Easting [m]',
# 			 y='Northing [m]')+
# 	facet_wrap(~source,ncol=2)+
# 	scale_fill_viridis(option='D', guide='colorbar',direction=1)+
# 	guides(fill=guide_colorbar('SWE [m]'))+
# 	theme(axis.text.x=element_text(angle=45,hjust=1))
# # guides(fill=guide_legend(reverse=TRUE))
# 
# ggr2 <-
# 	rdf %>%
# 	dplyr::select(x,y,contains('diff')) %>%
# 	gather(source,swediff,diffphvfsca) %>%
# 	ggplot(aes(x,y,fill=swediff))+
# 	geom_raster()+
# 	coord_fixed()+
# 	labs(x='Easting [m]',
# 			 y='Northing [m]')+
# 	scale_fill_gradient2()+
# 	# facet_wrap(~source)+
# 	guides(fill=guide_colorbar('SWE\nDifference [m]'))+
# 	theme(
# 		axis.text.x=element_text(angle=45,hjust=1),
# 		legend.position = 'bottom',
# 		legend.text=element_text(angle=45))
# 
# swepg=ggr1#plot_grid(ggr1,NULL,
# 								# ncol=2,
# 								# rel_widths = c(4,1))
# diffpg=plot_grid(ggr2,ggf,
# 								 ncol=2,
# 								 rel_widths = c(1,1))
# cwg <- plot_grid(swepg,diffpg,align='hv',
# 								 # rel_widths=c(4,1),
# 								 nrow=2)
# show(cwg)
# save_plot(plot=cwg,
# 					filename='aso-phvfsca_comparison_20150430.jpg',
# 					ncol=1,
# 					nrow=2,
# 					base_height=6,
# 					base_width=12)
# 
# 
