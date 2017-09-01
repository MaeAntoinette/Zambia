#Raster Time Series
require(rts)
require(raster)
require(chron)
require(rgdal)

#List Files in NDVI folder 
#This is your NDVI zipped folder. 
NDVI.fold <- "F:/Zambia/MOD_NDVI_16Day/2011_2012/"
#Identify rasters
NDVI.list <- list.files(NDVI.fold,patt="500m_16_days_NDVI.tif",full.names = TRUE)
#Brick NDVI data.
NDVI.stack <- raster::stack(NDVI.list)

#Bring in PET (already adjusted data) stack. 
PET.stack <- raster("INSERT PostExpansion.PET.stack_1km.tif as listed on in Google Drive*")


#Shapefiles
#See zipped Zambia_shapefiles_multipartpolygons folder on GitHub.
zambia.pn <- "F:/Zambia"
zambia.fn.alwayscane <- "zambia_sugar_luc_extended_multipart_alwayscane"
zambia.fn.croptocane <- "zambia_sugar_luc_extended_multipart_croptocane"
zambia.fn.kascol <- "zambia_sugar_luc_extended_multipart_kascol"
zambia.fn.shrubtocane <- "zambia_sugar_luc_extended_multipart_shrubtocane"
zambia.fn.unknown <- "zambia_sugar_luc_extended_multipart_unknown"

zambia.alwayscane <- readOGR(zambia.pn,zambia.fn.alwayscane)
zambia.croptocane <- readOGR(zambia.pn,zambia.fn.croptocane)
zambia.kascol <- readOGR(zambia.pn,zambia.fn.kascol)
zambia.shrubtocane <- readOGR(zambia.pn,zambia.fn.shrubtocane)
zambia.unknown <- readOGR(zambia.pn,zambia.fn.unknown)



#### NDVI ####
#extract f(x) for NDVI values. 
TS.alwayscane <- extract(NDVI.stack,zambia.alwayscane,method = "simple", fun="mean",
                        cellnumbers = TRUE)/10000
TS.croptocane <- extract(NDVI.stack,zambia.croptocane,method = "simple", fun="mean",
                         cellnumbers = TRUE)/10000
TS.kascol <- extract(NDVI.stack,zambia.kascol,method = "simple", fun="mean",
                         cellnumbers = TRUE)/10000
TS.shrubtocane <- extract(NDVI.stack,zambia.shrubtocane,method = "simple", fun="mean",
                         cellnumbers = TRUE)/10000
TS.unknown <- extract(NDVI.stack,zambia.unknown,method = "simple", fun="mean",
                         cellnumbers = TRUE)/10000

#plot the five TS NDVI graphs. 
TS.labels <- substr(NDVI.list,51,60)
TS.labels <- data.frame(Modis.Date = TS.labels)
TS.labels$Month <- as.numeric(format(strptime(TS.labels$Modis.Date,format="%Y-%m-%d"),"%m"))
TS.labels$Month.name <- paste0(substr(months(as.Date(TS.labels$Modis.Date)),0,3)," '",
                               substr(as.character(format(strptime(TS.labels$Modis.Date,format="%Y-%m-%d"),"%Y")),3,4))


windows(width=8,height=10)
layout(matrix(c(1,2,3,4,5), nrow = 5, ncol = 1, byrow = TRUE))
par(mar = c(1,5,1,2), oma = c(1,3,1,3))
par(cex=0.5)

matplot(t(TS.alwayscane), type = "l", xlab = " ", ylab = "NDVI.AlwaysCane",xaxt = "n",ylim = c(0,1),yaxs = "i")
abline(v = 13,h = 0)
abline(v = 24,h = 0)
abline(v = 33,h = 0)
matplot(t(TS.croptocane), type = "l", xlab = " ", ylab = "NDVI.CropToCane",xaxt = "n",ylim = c(0,1),yaxs = "i")
abline(v = 13,h = 0)
abline(v = 24,h = 0)
abline(v = 33,h = 0)
matplot(t(TS.kascol), type = "l", xlab = " ", ylab = "NDVI.Kascol",xaxt = "n",ylim = c(0,1),yaxs = "i")
abline(v = 13,h = 0)
abline(v = 24,h = 0)
abline(v = 33,h = 0)
matplot(t(TS.shrubtocane), type = "l", xlab = " ", ylab = "NDVI.ShrubToCane",xaxt = "n",ylim = c(0,1),yaxs = "i")
abline(v = 13,h = 0)
abline(v = 24,h = 0)
abline(v = 33,h = 0)
matplot(t(TS.unknown), type = "l", xlab = " ", ylab = "NDVI.UnknownToCane",xaxt = "n",ylim = c(0,1),yaxs = "i")
abline(v = 13,h = 0)
abline(v = 24,h = 0)
abline(v = 33,h = 0)
axis(1, at=seq(1,35,1),labels = TS.labels$Month.name)









