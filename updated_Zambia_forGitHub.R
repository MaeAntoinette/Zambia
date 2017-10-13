#~*~*~*~*~*~*~*~*~ 2017 Sept 22 ~*~*~*~*~*~*~*~*~*~*~*~*
#~*~*~*~*~*~* salinasmaegan@gmail.com ~*~*~*~*~*~*~*~*~*

#Using Mod16, we can estimate ET across the landscape. Mod16 provides ET in 8-day, monthly, or yearly composites. 
#For more information view: http://files.ntsg.umt.edu/data/NTSG_Products/MOD16/MOD16_global_evapotranspiration_description.pdf 
#Script below includes: 1) 8-day maps per the crop calendary year (DOY 209,2011 - 201,2012), removal of pseudo values indicating areas in which ET is not calculated, 2) stacking of 8-day maps, and 3) crop year ET calculations using the provided Mod16 ET and the Kc-ET alternative using Mod16 PET and kc values. 
#For kc-ET you wont use pre PET files. However, they are in here.

#--------------- Code Begins Here
require(raster)
require(rgdal)

#------ Access maps --------- 
#Aleady projected to UTM, zone 35. Maps per req. could be on Google Drive.
fold.main <- "F:/Zambia/MOD_16A2_8Day/"
fold.PreExpansion <- "2006_2007"
fold.PostExpansion <- "2011_2012"

#ET files
list.ETfiles.pre <- list.files(paste0(fold.main,fold.PreExpansion), patt = ".tif$", all.files = TRUE, full.names = TRUE)
list.ETfiles.pre <- list.ETfiles.pre[grep("PET_1km",list.ETfiles.pre, invert = TRUE)]
list.ETfiles.post <- list.files(paste0(fold.main,fold.PostExpansion), patt = ".tif$", all.files = TRUE, full.names = TRUE)
list.ETfiles.post <- list.ETfiles.post[grep("PET_1km",list.ETfiles.post, invert = TRUE)]

#PET files
list.PETfiles.pre <- list.files(paste0(fold.main,fold.PreExpansion), patt = ".tif$", all.files = TRUE, full.names = TRUE)
list.PETfiles.pre <- list.PETfiles.pre[grep("PET_1km",list.PETfiles.pre)]
list.PETfiles.post <- list.files(paste0(fold.main,fold.PostExpansion), patt = ".tif$", all.files = TRUE, full.names = TRUE)
list.PETfiles.post <- list.PETfiles.post[grep("PET_1km",list.PETfiles.post)]


#--------- Create list containing crop year only. ---------
#Closest 8day date to beginning (Aug 01) is July 28 (DOY209)
#Closest 8day date to end (July 31) is July 20 (DOY201) or July 19 (leap year DOY201)

list.ETfiles.pre <- list.ETfiles.pre[27:72]
list.ETfiles.post <- list.ETfiles.post[27:72]
list.PETfiles.pre <- list.PETfiles.pre[27:72]
list.PETfiles.post <- list.PETfiles.post[27:72]

#------- Create raster stacks -------
stack.ETfiles.pre <- raster::stack(list.ETfiles.pre)
stack.ETfiles.post <- raster::stack(list.ETfiles.post)
stack.PETfiles.pre <- raster::stack(list.PETfiles.pre)
stack.PETfiles.post <- raster::stack(list.PETfiles.post)

#------- Remove invalid values (areas in which ET is not calculated) --------
#as stated in: http://files.ntsg.umt.edu/data/NTSG_Products/MOD16/MOD16_global_evapotranspiration_description.pdf
stack.ETfiles.pre[stack.ETfiles.pre > 32700] <- NA
stack.ETfiles.post[stack.ETfiles.post > 32700] <- NA
stack.PETfiles.pre[stack.PETfiles.pre > 32700] <- NA
stack.PETfiles.post[stack.PETfiles.post > 32700] <- NA

#------ Multiplier (x0.1) to get values in mm ----- 
stack.ETfiles.pre <- stack.ETfiles.pre * 0.1
stack.ETfiles.post <- stack.ETfiles.post * 0.1
stack.PETfiles.pre <- stack.PETfiles.pre * 0.1
stack.PETfiles.post <- stack.PETfiles.post * 0.1

#------ Calculating annual ET (mm) using MOD16 ET -------
dates.ETpre <- format(as.Date(substr(names(stack.ETfiles.pre),2,8), format = "%Y%j"))
dates.ETpost <- format(as.Date(substr(names(stack.ETfiles.post),2,8), format = "%Y%j"))

indices <- rep(1,times = 46)
annualET.pre <- stackApply(stack.ETfiles.pre,indices = indices, fun = sum, na.rm = FALSE)
annualET.post <- stackApply(stack.ETfiles.post,indices = indices, fun = sum)

writeRaster(annualET.pre, "F:/Zambia/MOD_16A2_8Day/AnnualET/CropAnnualET_Pre_MOD16ET.tif")
writeRaster(annualET.post, "F:/Zambia/MOD_16A2_8Day/AnnualET/CropAnnualET_Post_MOD16ET.tif")

#------ Calculating 8-day ET (mm) using MOD16 PET and kc-ET method -------

#Kc daily values
Kc.Daily <- data.frame(Date = seq.Date(from = as.Date.character("2011-07-28"), to = as.Date.character("2012-07-19"), by = "day"), Kc.Value = " ")
Kc.8day.2011 <- seq.Date(from =  as.Date.character("2011-07-28"), to = as.Date.character("2011-12-31"), by = 8)
Kc.8day.2012  <- seq.Date(from =  as.Date.character("2012-01-01"), to = as.Date.character("2012-07-19"), by = 8)
Kc.8day <- c(Kc.8day.2011,Kc.8day.2012)

#1km MOD16A2 8day-PET maps from Jan 2011 - Jul 2012 will be adjusted using sugarcane Kc values (FAO-56). 
#Senescent DOY 209: 0.20, Initial DOY 233
Kc.Daily$Kc.Value <- as.numeric(Kc.Daily$Kc.Value)
Kc.Daily$Kc.Value <- NA

Kc.Daily$Kc.Value[Kc.Daily$Date == c("2011-07-28")] <- 0.20
Kc.Daily$Kc.Value[Kc.Daily$Date == c("2011-08-21")] <- 0.30
Kc.Daily$Kc.Value[Kc.Daily$Date == c("2011-09-14")] <- 0.40
Kc.Daily$Kc.Value[Kc.Daily$Date == c("2011-11-17")] <- 0.75
Kc.Daily$Kc.Value[Kc.Daily$Date == c("2012-01-01")] <- 1.25
Kc.Daily$Kc.Value[Kc.Daily$Date == c("2012-03-29")] <- 0.75
Kc.Daily$Kc.Value[Kc.Daily$Date == c("2012-05-16")] <- 0.40
Kc.Daily$Kc.Value[Kc.Daily$Date == c("2012-07-19")] <- 0.20

Kc.8day.values <- approx(Kc.Daily$Date, y = Kc.Daily$Kc.Value, xout = Kc.8day)
Kc.8day.values <- setNames(data.frame(Kc.8day.values), c("Date", "Kc.Value"))
Kc.8day.values$Kc.Value <- round(Kc.8day.values$Kc.Value, digits = 2)

#Multiply Kc Values to raster stack 
KcET.ofile <- paste0(substr(list.PETfiles.post,0,34),"KcET/",substr(list.PETfiles.post,35,41),".KcET_1km")
extension(KcET.ofile) <- "tif"

for (i in (1:length(KcET.ofile))){
  PET.ras <- stack.PETfiles.post[[i]]
  KcET.ras <- PET.ras * Kc.8day.values$Kc.Value[i]
  writeRaster(KcET.ras, KcET.ofile[i], overwrite = TRUE)
  
  print(paste("Image", i , "of" ,length(KcET.ofile)))
  flush.console()
  
}

#------ Calculating annual ET (mm) using MOD16 PET and kc-ET method -------
list.KcETfiles <- list.files("F:/Zambia/MOD_16A2_8Day/2011_2012/KcET/",pattern = "tif$",full.names = TRUE)
stack.KcETfiles <- stack(list.KcETfiles)

annualKcET.post <- stackApply(stack.KcETfiles,indices = indices, fun = sum)
writeRaster(annualKcET.post, "F:/Zambia/MOD_16A2_8Day/AnnualET/CropAnnualET_Post_KcET.tif")

