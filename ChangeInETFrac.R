require(raster)
require(rgdal)

#Calculating ET fraction for zambia pre- and post-expansion periods 
#Change the Pre and Post images according to files in GitHub... same file names. 


#ExpansionET
PreET <- raster("F:/Zambia/PreExpansionAnnualETsum_Fields.tif")
PostET <- raster("F:/Zambia/PostExpansionAnnualETsum_Fields.tif")

#PreExpansionET 
PreETrF<- raster("F:/Zambia/PreExpansionAnnualETrF.tif")
PostETrF <- raster("F:/Zambia/PostExpansionAnnualETrF.tif")

#Difference in ETrF 
AnnualETrFDiff <- PostETrF - PreETrF
AnnualETrFChange <- 100* (AnnualETrFDiff/PreETrF)
AnnualETrFChgRatio <- PostETrF/PreETrF

#Overlay shp of fields
#Use the matching shp file from your zipped Zambia_Shapefiles folder
Zambia.shp <- readOGR("F:/Zambia","zambia_sugar_luc_extended")
Zambia.ext <- extent(Zambia.shp)

windows(width = 8, height = 10)
plot(AnnualETrFChange, ext = Zambia.ext)
plot(Zambia.shp, add = TRUE)

windows(width = 8, height = 10)
plot(AnnualETrFChgRatio, ext = Zambia.ext)
plot(Zambia.shp, add = TRUE)

AnnualETrFChgMeans <- extract(AnnualETrFChange,Zambia.shp, method = "simple", fun = mean)
AnnualETrFChgMeans <- data.frame(AnnualETrFChgMeans)

windows(width = 8, height = 10)
hist(AnnualETrFChgMeans$AnnualETrFChgMeans, xlab = "% Change in Annual ETrF", ylab = "Field Freq", main = "")


#PET changes
PrePET <- raster("F:/Zambia/PreExpansionAnnualPETsum.tif")
PostPET <- raster("F:/Zambia/PostExpansionAnnualPETsum.tif")

PETdiff <- PostPET - PrePET
PETchg <- 100  * (PETdiff/PrePET)
PETchgratio <- PostPET/PrePET

plot(PETchg,ext = Zambia.ext)
plot(Zambia.shp, add = TRUE)

plot(PETchgratio,ext = Zambia.ext)
plot(Zambia.shp, add = TRUE)

mean(extract(PETchg,Zambia.shp, method = "simple", fun = mean))
