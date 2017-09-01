#Zambia Kc ET estimation 

#### Zambia shapefiles by groups #### 
#Note these shapefiles are dissovled/multipart polygons. 

#use files in the Zambia_shapefiles_extended zip. Switch pathnames accordingly. 
z.kascol.shp <- readOGR("F:/Zambia","zambia_sugar_luc_extended_LUConversion_kascolownershipbdy")
z.unknown.shp <- readOGR("F:/Zambia","zambia_sugar_luc_extended_LUConversion_unknowntocane")
z.shrubtocane.shp <- readOGR("F:/Zambia","zambia_sugar_luc_extended_LUConversion_shrubtocane")
z.croptocane.shp <- readOGR("F:/Zambia","zambia_sugar_luc_extended_LUConversion_croptocane")
z.alwayscane.shp <- readOGR("F:/Zambia","zambia_sugar_luc_extended_LUConversion_alwayscane")

#### Kc daily values #### 
Kc.Daily <- data.frame(Date = seq.Date(from = as.Date.character("2011-06-26"), to = as.Date.character("2012-07-03"), by = "day"), Kc.Value = " ")
Kc.8day.2011 <- seq.Date(from =  as.Date.character("2011-06-26"), to = as.Date.character("2011-12-31"), by = 8)
Kc.8day.2012  <- seq.Date(from =  as.Date.character("2012-01-01"), to = as.Date.character("2012-07-02"), by = 8)
Kc.8day <- c(Kc.8day.2011,Kc.8day.2012)

#1km MOD16A2 8day-PET maps from Jul 2011 - Jul 2012 will be adjusted using sugarcane Kc values (FAO-56). 
#Dates (according to NDVI TS): Initial - July 01, 2011; Mid-dev - Jan 01, 2012; Late-dev - June 01, 2011 
#Initial Kc: 0.40, Middle Kc: 1.25, Late Kc: 0.75
Kc.Daily$Kc.Value <- as.numeric(Kc.Daily$Kc.Value)
Kc.Daily$Kc.Value <- NA

Kc.Daily$Kc.Value[Kc.Daily$Date == c("2011-06-26")] <- 0.40
Kc.Daily$Kc.Value[Kc.Daily$Date == c("2011-08-01")] <- 0.40
Kc.Daily$Kc.Value[Kc.Daily$Date == c("2012-01-01")] <- 1.25
Kc.Daily$Kc.Value[Kc.Daily$Date == c("2012-04-01")] <- 1.25
Kc.Daily$Kc.Value[Kc.Daily$Date == c("2012-06-01")] <- 0.75
Kc.Daily$Kc.Value[Kc.Daily$Date == c("2012-07-03")] <- 0.75

Kc.8day.values <- approx(Kc.Daily$Date, y = Kc.Daily$Kc.Value, xout = Kc.8day)
Kc.8day.values <- setNames(data.frame(Kc.8day.values), c("Date", "Kc.Value"))
Kc.8day.values$Kc.Value <- round(Kc.8day.values$Kc.Value, digits = 2)

#merge kc value to a daily df. 
Kc.Daily <- merge(Kc.Daily, Kc.8day.values, by = "Date", all = TRUE)



windows(width = 8, height = 10)
plot(Kc.Daily$Date,Kc.Daily$Kc.Value.y, xlab = " ", ylab = "Kc Value", pch = 20, ylim = c(0, 1.4))
abline(v = as.Date("2011-07-01"), lty = 2, col = "grey30")
abline(v = as.Date("2012-01-01"), lty = 2, col = "grey30")
abline(v = as.Date("2012-06-01"), lty = 2, col = "grey30")

text.default(x = as.Date("2011-09-01"), y = 0.8, labels = "Initial", 
             cex = 0.75, col = "red")
arrows(as.Date("2011-08-01"), 0.8,
       as.Date("2011-07-05"), 0.8, 
       col = "red", length = 0.15, code = 2)

text.default(x = as.Date("2012-02-25"), y = 1.0, labels = "Mid", 
             cex = 0.75, col = "red")
arrows(as.Date("2012-02-01"), 1.0,
       as.Date("2012-01-05"), 1.0, 
       col = "red", length = 0.15, code = 2)

text.default(x = as.Date("2012-03-28"), y = 0.8, labels = "Late", 
             cex = 0.75, col = "red")
arrows(as.Date("2012-04-20"), 0.8,
       as.Date("2012-05-15"), 0.8, 
       col = "red", length = 0.15, code = 2)


KcET.8day.values <- data.frame(Date = as.Date(Kc.8day.values$x), Post.Expan = NA,
                               Kc.Value = Kc.8day.values$y,
                               Always.Cane = "", Crop.ToCane = "", Kascol = "", Shrub.ToCane = "", Unknown.ToCane = "",
                               KcET.AlwaysCane = "", KcET.CropToCane = "", KcET.Kascol = "", KcET.ShrubToCane = "", KcET.UnknownToCane = "")

KcET.8day.values$Post.Expan <- ifelse(KcET.8day.values$Date >= "2011-11-01" & KcET.8day.values$Date <= "2012-04-30", "TRUE","FALSE")

KcET.8day.values$Always.Cane <- as.numeric(as.character(KcET.8day.values$Always.Cane)) 
KcET.8day.values$Crop.ToCane <- as.numeric(as.character(KcET.8day.values$Crop.ToCane)) 
KcET.8day.values$Kascol<- as.numeric(as.character(KcET.8day.values$Kascol)) 
KcET.8day.values$Shrub.ToCane <- as.numeric(as.character(KcET.8day.values$Shrub.ToCane)) 
KcET.8day.values$Unknown.ToCane<- as.numeric(as.character(KcET.8day.values$Unknown.ToCane)) 

KcET.8day.values$KcET.AlwaysCane <- as.numeric(as.character(KcET.8day.values$KcET.AlwaysCane)) 
KcET.8day.values$KcET.CropToCane <- as.numeric(as.character(KcET.8day.values$KcET.CropToCane)) 
KcET.8day.values$KcET.Kascol<- as.numeric(as.character(KcET.8day.values$KcET.Kascol)) 
KcET.8day.values$KcET.ShrubToCane <- as.numeric(as.character(KcET.8day.values$KcET.ShrubToCane)) 
KcET.8day.values$KcET.UnknownToCane<- as.numeric(as.character(KcET.8day.values$KcET.UnknownToCane)) 


#### PET ####
#Use files from the 2011_2012_MOD16PET_1Km zipped folder. Change pathnames accordingly. 
list.PET <- list.files("F:/Zambia/MOD_16A2_8Day/2011_2012/", patt = ".tif$", all.files = TRUE, full.names = TRUE)
list.PET <- list.PET[grep("PET_1km",list.PET)]

#### Loop to extract mean PET for group per image date ####
#Create a working df for the 8-day means. 
PET8day.df <- data.frame(JulianDate = NA, Mean.ET = NA)

#test one image. 
PET.8daytest <- raster(list.PET[1])
PET.8dayshp.test <- (raster::extract(PET.8daytest,z.kascol.shp,fun = mean,method="simple",na.rm=TRUE)) * 0.1
PET.kascol.df.test <- data.frame(JulianDate = paste0(substr(list.PET[1],35,38),"/",substr(list.PET[1],39,41)), Mean.PET = PET.8dayshp.test[1,1])
PET.kascol.df.test$Gregorian <- as.Date(strptime(PET.kascol.df.test$JulianDate, "%Y/%j"))


for (i in (1:length(list.PET))){
  PET.ras <- raster(list.PET[i])
  #kascol
  PET.kascol.val <- raster::extract(PET.ras,z.kascol.shp,fun = mean, method = "simple", na.rm = TRUE) * 0.1
  PET.kascol.df <- data.frame(JulianDate = paste0(substr(list.PET[i],35,38),"/",substr(list.PET[i],39,41)), Mean.PET = PET.kascol.val[1,1])
  PET.kascol.df$Gregorian <- as.Date(strptime(PET.kascol.df$JulianDate, "%Y/%j"))
 
  #alwayscane
  PET.alwayscane.val <- raster::extract(PET.ras,z.alwayscane.shp,fun = mean, method = "simple", na.rm = TRUE) * 0.1
  PET.alwayscane.df <- data.frame(JulianDate = paste0(substr(list.PET[i],35,38),"/",substr(list.PET[i],39,41)), Mean.PET = PET.alwayscane.val[1,1])
  PET.alwayscane.df$Gregorian <- as.Date(strptime(PET.alwayscane.df$JulianDate, "%Y/%j"))
  
  #croptocane
  PET.croptocane.val <- raster::extract(PET.ras,z.croptocane.shp,fun = mean, method = "simple", na.rm = TRUE) * 0.1
  PET.croptocane.df <- data.frame(JulianDate = paste0(substr(list.PET[i],35,38),"/",substr(list.PET[i],39,41)), Mean.PET = PET.croptocane.val[1,1])
  PET.croptocane.df$Gregorian <- as.Date(strptime(PET.croptocane.df$JulianDate, "%Y/%j"))
 
  
  #shrubtocane
  PET.shrubtocane.val <- raster::extract(PET.ras,z.shrubtocane.shp,fun = mean, method = "simple", na.rm = TRUE) * 0.1
  PET.shrubtocane.df <- data.frame(JulianDate = paste0(substr(list.PET[i],35,38),"/",substr(list.PET[i],39,41)), Mean.PET = PET.shrubtocane.val[1,1])
  PET.shrubtocane.df$Gregorian <- as.Date(strptime(PET.shrubtocane.df$JulianDate, "%Y/%j")) 
  
  #unknowntocane
  PET.unknown.val <- raster::extract(PET.ras,z.unknown.shp,fun = mean, method = "simple", na.rm = TRUE) * 0.1
  PET.unknown.df <- data.frame(JulianDate = paste0(substr(list.PET[i],35,38),"/",substr(list.PET[i],39,41)), Mean.PET = PET.unknown.val[1,1])
  PET.unknown.df$Gregorian <- as.Date(strptime(PET.unknown.df$JulianDate, "%Y/%j"))
  
  # for Kascol
  if(exists("PET.kascol.complete")){
    PET.kascol.complete = rbind(PET.kascol.complete,PET.kascol.df) #take scene.monthly.df iteration and bind pixel.monthly.value
  } else {
    PET.kascol.complete = PET.kascol.df
    #if it doesn't exist, the scene.monthly.value = the first iteration of 
    #pixel.monthly.df above. ie: pixel 1 to pixel n
    
  }
  
  PET.kascol.complete <- data.frame(PET.kascol.complete)
  PET.kascol.complete$Mean.PET <- as.numeric((as.character(PET.kascol.complete$Mean.PET))) 
  PET.kascol.complete$Mean.PET <- round(PET.kascol.complete$Mean.PET, digits = 2)
  
  
  # for AlwaysCane
  if(exists("PET.alwayscane.complete")){
    PET.alwayscane.complete = rbind(PET.alwayscane.complete,PET.alwayscane.df) #take scene.monthly.df iteration and bind pixel.monthly.value
  } else {
    PET.alwayscane.complete = PET.alwayscane.df
    #if it doesn't exist, the scene.monthly.value = the first iteration of 
    #pixel.monthly.df above. ie: pixel 1 to pixel n
    
  }
  
  PET.alwayscane.complete <- data.frame(PET.alwayscane.complete)
  PET.alwayscane.complete$Mean.PET <- as.numeric((as.character(PET.alwayscane.complete$Mean.PET))) 
  PET.alwayscane.complete$Mean.PET <- round(PET.alwayscane.complete$Mean.PET, digits = 2)
  
  
  #for Crop toCane
  if(exists("PET.croptocane.complete")){
    PET.croptocane.complete = rbind(PET.croptocane.complete,PET.croptocane.df) #take scene.monthly.df iteration and bind pixel.monthly.value
  } else {
    PET.croptocane.complete = PET.croptocane.df
    #if it doesn't exist, the scene.monthly.value = the first iteration of 
    #pixel.monthly.df above. ie: pixel 1 to pixel n
    
  }
  
  PET.croptocane.complete <- data.frame(PET.croptocane.complete)
  PET.croptocane.complete$Mean.PET <- as.numeric((as.character(PET.croptocane.complete$Mean.PET))) 
  PET.croptocane.complete$Mean.PET <- round(PET.croptocane.complete$Mean.PET, digits = 2)
  
  
  
  # for shrub to cane
  if(exists("PET.shrubtocane.complete")){
    PET.shrubtocane.complete = rbind(PET.shrubtocane.complete,PET.shrubtocane.df) #take scene.monthly.df iteration and bind pixel.monthly.value
  } else {
    PET.shrubtocane.complete = PET.shrubtocane.df
    #if it doesn't exist, the scene.monthly.value = the first iteration of 
    #pixel.monthly.df above. ie: pixel 1 to pixel n
    
  }
  
  PET.shrubtocane.complete <- data.frame(PET.shrubtocane.complete)
  PET.shrubtocane.complete$Mean.PET <- as.numeric((as.character(PET.shrubtocane.complete$Mean.PET))) 
  PET.shrubtocane.complete$Mean.PET <- round(PET.shrubtocane.complete$Mean.PET, digits = 2)
  
  # for unknown to cane
  if(exists("PET.unknown.complete")){
    PET.unknown.complete = rbind(PET.unknown.complete,PET.unknown.df) #take scene.monthly.df iteration and bind pixel.monthly.value
  } else {
    PET.unknown.complete = PET.unknown.df
    #if it doesn't exist, the scene.monthly.value = the first iteration of 
    #pixel.monthly.df above. ie: pixel 1 to pixel n
    
  }
  
  PET.unknown.complete <- data.frame(PET.unknown.complete)
  PET.unknown.complete$Mean.PET <- as.numeric((as.character(PET.unknown.complete$Mean.PET))) 
  PET.unknown.complete$Mean.PET <- round(PET.unknown.complete$Mean.PET, digits = 2)
  
  print(paste("Image", i , "of" ,length(list.PET)))
  flush.console()
  
}





#### KcPET 8 day values ####
#before pasting PET values per date, check to see if all dates match. 
all(PET.unknown.complete$Gregorian == KcET.8day.values$Date)
#paste the looped "completed" PET values (per category) into the KcET.8day.values df 
KcET.8day.values$Always.Cane <- PET.alwayscane.complete$Mean.PET
KcET.8day.values$Crop.ToCane <- PET.croptocane.complete$Mean.PET
KcET.8day.values$Kascol <- PET.kascol.complete$Mean.PET
KcET.8day.values$Shrub.ToCane <- PET.shrubtocane.complete$Mean.PET
KcET.8day.values$Unknown.ToCane <- PET.unknown.complete$Mean.PET


KcET.8day.values$KcET.AlwaysCane <- round(KcET.8day.values$Kc.Value * KcET.8day.values$Always.Cane, digits = 2)
KcET.8day.values$KcET.CropToCane <- round(KcET.8day.values$Kc.Value * KcET.8day.values$Crop.ToCane, digits = 2)
KcET.8day.values$KcET.Kascol <- round(KcET.8day.values$Kc.Value * KcET.8day.values$Kascol, digits = 2)
KcET.8day.values$KcET.ShrubToCane <- round(KcET.8day.values$Kc.Value * KcET.8day.values$Shrub.ToCane, digits = 2)
KcET.8day.values$KcET.UnknownToCane <- round(KcET.8day.values$Kc.Value * KcET.8day.values$Unknown.ToCane, digits = 2)




#### Plot 8 day values of KcET.postexpansion: This is your Figure 3####
at.labels <- as.character(KcET.8day.values$Date)


#Plot PET per category
windows(width=8,height=10)
layout(matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2, byrow = TRUE))
par(cex=0.5)
par(mar = c(2,2,2,2), oma = c(5,5,5,1))

plot(KcET.8day.values$Date,KcET.8day.values$KcET.Kascol,pch = 20,col = "orange", ylim = c(0, 100),
     xlab = "", ylab = "", cex.lab = 0.75)
legend("topleft",horiz = TRUE, c("Kascol", "Always Cane","Shrub to Cane", "Crop to Cane", "Unknown to Cane"), 
       pch = c(20,20,20,20,20), col = c("orange","blue","grey","gold","purple"), cex = 0.75)

plot(KcET.8day.values$Date,KcET.8day.values$KcET.AlwaysCane,pch = 20, col = "blue", ylim = c(0, 100),
     xlab = "", ylab = "", cex.lab = 0.75)
plot(KcET.8day.values$Date,KcET.8day.values$KcET.ShrubToCane,pch = 20, col ="grey", ylim = c(0, 100),
     xlab = "", ylab = "", cex.lab = 0.75)
plot(KcET.8day.values$Date,KcET.8day.values$KcET.CropToCane,pch = 20, col ="gold", ylim = c(0, 100),
     xlab = "", ylab = "", cex.lab = 0.75)
plot(KcET.8day.values$Date,KcET.8day.values$KcET.UnknownToCane,pch = 20, col ="purple", ylim = c(0, 100),
     xlab = "", ylab = "", cex.lab = 0.75)

mtext("Mean Kc-ET (mm/8day)",outer = TRUE, side = 2, cex = 0.75)
mtext("Kc-ET from Largest LU Conversions \n to Sugar Cane",outer = TRUE, cex = 0.75)





#### Summing Kc-ET #### 
#Full year 2011-177 to 2012-177
KcET.sums <- data.frame(Ann.AlwaysCane.ET =  sum(KcET.8day.values$KcET.AlwaysCane),
                        Ann.ShrubToCane.ET = sum(KcET.8day.values$KcET.ShrubToCane),
                        Ann.CropToCane.ET = sum(KcET.8day.values$KcET.CropToCane),
                        Ann.Kascol.ET = sum(KcET.8day.values$KcET.Kascol),
                        Ann.Unknown.ET = sum(KcET.8day.values$KcET.UnknownToCane),
                        PE.AlwaysCane.ET = sum(KcET.8day.values$KcET.AlwaysCane[KcET.8day.values$Post.Expan == "TRUE"]),
                        PE.ShrubToCane.ET = sum(KcET.8day.values$KcET.ShrubToCane[KcET.8day.values$Post.Expan == "TRUE"]),
                        PE.CropToCane.ET = sum(KcET.8day.values$KcET.CropToCane[KcET.8day.values$Post.Expan == "TRUE"]),
                        PE.Kascol.ET = sum(KcET.8day.values$KcET.Kascol[KcET.8day.values$Post.Expan == "TRUE"]),
                        PE.Unknown.ET = sum(KcET.8day.values$KcET.UnknownToCane[KcET.8day.values$Post.Expan == "TRUE"])
)

KcET.mean <- data.frame(Ann.AlwaysCane.ET =  mean(KcET.8day.values$KcET.AlwaysCane),
                        Ann.ShrubToCane.ET = mean(KcET.8day.values$KcET.ShrubToCane),
                        Ann.CropToCane.ET = mean(KcET.8day.values$KcET.CropToCane),
                        Ann.Kascol.ET = mean(KcET.8day.values$KcET.Kascol),
                        Ann.Unknown.ET = mean(KcET.8day.values$KcET.UnknownToCane),
                        PE.AlwaysCane.ET = mean(KcET.8day.values$KcET.AlwaysCane[KcET.8day.values$Post.Expan == "TRUE"]),
                        PE.ShrubToCane.ET = mean(KcET.8day.values$KcET.ShrubToCane[KcET.8day.values$Post.Expan == "TRUE"]),
                        PE.CropToCane.ET = mean(KcET.8day.values$KcET.CropToCane[KcET.8day.values$Post.Expan == "TRUE"]),
                        PE.Kascol.ET = mean(KcET.8day.values$KcET.Kascol[KcET.8day.values$Post.Expan == "TRUE"]),
                        PE.Unknown.ET = mean(KcET.8day.values$KcET.UnknownToCane[KcET.8day.values$Post.Expan == "TRUE"])
)




#### save files as csvs #### 
write.csv(KcET.8day.values, "F:/Zambia/KcET_8day_values.csv", row.names = FALSE)
write.csv(KcET.sums, "F:/Zambia/KcET_postexapnsion_sums.csv", row.names = FALSE)
write.csv(KcET.mean, "F:/Zambia/KcET_postexapnsion_mean.csv", row.names = FALSE)
