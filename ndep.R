




#################
#
# Takes NEON sites in use and extracts N depsoition from total N dep data
# Anika Staccone
# 3/30/2020
#
#################


require(rgdal)
require(raster)
require(fields)


######## load NEON site data to get lat/lon
setwd("/Users/Anika/Documents/GradSchool/")
all <- read.csv('NEON_2019/all.csv')
head(all) #x is lon, y is lat



######## for each NEON site get NDEP
#dry hno3
N_HNO3 <- raster("FIA_EstimateProj/Ndep_clim_nitrogen_730/data/NDDN_dry_deposition_hno3conc_hno3vd_0.5x0.5_grid_annual.txt")
#dry no3
N_NO3 <- raster("FIA_EstimateProj/Ndep_clim_nitrogen_730/data/NDDN_dry_deposition_no3conc_no3vd_0.5x0.5_grid_annual.txt")
#wet no3
N_NO3w <- raster("FIA_EstimateProj/Ndep_clim_nitrogen_730/data/NADP_wet_deposition_no3_0.5x0.5_grid_annual_R1.txt")
#dry nh4
N_NH4 <- raster("FIA_EstimateProj/Ndep_clim_nitrogen_730/data/NDDN_dry_deposition_nh4conc_particulatevd_0.5x0.5_grid_annual.txt")
#wet nh4
N_NH4w <- raster("FIA_EstimateProj/Ndep_clim_nitrogen_730/data/NADP_wet_deposition_nh4_0.5x0.5_grid_annual_R1.txt")
#total
N_tot <- N_HNO3 + N_NO3 + N_NO3w + N_NH4 + N_NH4w
proj4string(N_tot) <- crs("+init=epsg:4326") #defining coordinate system to the WGS84 lat/long
myCRS <- CRS(proj4string(N_tot))
N_tot <- projectRaster(N_tot, res = 1, crs = myCRS)


#extract values
xy <- all[,c("x","y")]
Ndep <- extract(N_tot, xy) #NAs are for sites in AK (e.g. row 73)

#put N dep back into df
all <- cbind(all, Ndep) #Ndep in kg N ha-1 yr-1

#plot data and lat/long points
png('NEON_2019/ndep_withpts.png')
plot(N_tot, xlab = "lon", ylab = "lat", main = "N Deposition")
points(xy)
dev.off()

#save results
write.csv(all, "NEON_2019/all_withNdep.csv")


###### for each NEON site get Du 2020 N lim
require(dplyr)
sites <- all %>% group_by(siteID) %>% summarize(x = first(x),
                                                y = first(y))
xy <- sites[,c("x","y")]
offset <- runif(nrow(sites), 0, 1)
plot(N_tot, xlab = "lon", ylab = "lat", main = "N Deposition")
points(x=sites$x, y=sites$y, col=sites$siteID, pch=17)
text(x = sites$x, y = sites$y+offset, labels = sites$siteID)

#write.csv(sites, "NEON_2019/sites.csv")
colnames(sites) <- c("siteID","lon","lat")
sites
write.csv(sites, "NEON_2019/sites.csv")
