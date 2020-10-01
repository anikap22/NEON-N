
require(dplyr)

setwd("/Users/Anika/Downloads")

# read csv files
litter <- read.csv("litter_chem.csv")
n_min <- read.csv("N_min_rates.csv")
soil <- read.csv("percent_soil_nitrogen.csv")
soil_distributed <- read.csv("soilN_TowerDistPlots.csv")
foliar <- read.csv("foliarCN.csv")
root <- read.csv("rootchem_NEON.csv")
clim <- read.csv("MAT_MAP_Allsites.csv")

# foliar cut cols
foliar <- foliar[,c("plotID","siteID.x","plotType.x","collectDate.x","sampleType.x","carbonPercent",
                    "nitrogenPercent","CNratio","geodeticDatum","decimalLatitude","decimalLongitude","coordinateUncertainty",
                    "elevation","subplotID","individualID","taxonID","scientificName","plantStatus")]
colnames(foliar) <- c("plotID","siteID.x","plotType","collectDate","sampleType","carbonPercent",
                      "nitrogenPercent","CNratio","geodeticDatum","decimalLatitude","decimalLongitude","coordinateUncertainty",
                      "elevation","subplotID","individualID","taxonID","scientificName","plantStatus")
foliar$collectDate <- as.Date(as.character(foliar$collectDate), format = "%d/%m/%y")

# change root date format
root$collectDate <- as.Date(root$collectDate, format = "%Y-%m-%dT%H:%MZ")
root$collectDate <- format(as.Date(root$collectDate),'%Y-%m')


# rename cols
colnames(litter) <- c("plotID","siteID","collectDate","namedLocation","nitrogenPercent_litter",
                      "carbonPercent_litter","CNratio_litter","dryMass_litter","ligninPercent_litter",
                      "cellulosePercent_litter")
colnames(soil_distributed) <- c("siteID","plotID","plotType","year","month","collectDate",
                                "nitrogenPercent_soil","organicCPercent_soil","CNratio_soil")

# date clip to first 7 characters ()
#substr(x, start, stop)
litter$collectDate <- substr(litter$collectDate, 1, 7)
n_min$collectDate <- substr(n_min$collectDate, 1, 7)
soil$collectDate <- substr(soil$collectDate, 1, 7)
soil_distributed$collectDate <- substr(soil_distributed$collectDate, 1, 7)
foliar$collectDate <- substr(foliar$collectDate, 1, 7)

# average foliar value per site
foliar_plot <- foliar %>% 
  group_by(plotID,collectDate) %>% 
  summarize(carbonPercent_foliar=mean(carbonPercent,na.rm=T),
            nitrogenPercent_foliar=mean(nitrogenPercent,na.rm=T),
            CNratio_foliar=mean(CNratio,na.rm=T),
            decimalLatitude=first(decimalLatitude),
            decimalLongitude=first(decimalLongitude),
            elevation=first(elevation),
            siteID=first(siteID.x))

# merge all data
all <- merge(litter, n_min, by=c("plotID","siteID","collectDate"), all.x=T, all.y=T)
all <- merge(all, soil, by=c("plotID","siteID","collectDate"), all.x=T, all.y=T)
all <- merge(all, soil_distributed, by=c("plotID","siteID","collectDate"), all.x=T, all.y=T)
all <- merge(all, foliar_plot, by=c("plotID","siteID","collectDate"), all.x=T, all.y=T)
all <- merge(all, root, by=c("plotID","siteID","collectDate"), all.x=T, all.y=T)
all <- merge(all, clim, by.x="siteID", by.y="SiteID", all.x=T, all.y=F)

# save 
write.csv(all, "all.csv", row.names=F)










# candidate plots
candidate <- all %>% filter(!is.na(nitrogenPercent_litter) & 
                            !is.na(soilAmmoniumNugPerGram) &
                            !is.na(CNratio_soil))

candidate <- all %>% filter(siteID=="BART")


# correlation matrix
require(corrplot)
all_small <- all %>% select(CNratio_litter, dryMass_litter, ligninPercent_litter,
                            cellulosePercent_litter, soilAmmoniumNugPerGram,
                            soilNitrateNitriteNugPerGram, netNminugPerGramPerDay,
                            netNitugPerGramPerDay, nitrogenTot, nitrogenPercent_soil,
                            CNratio_soil, CNratio_foliar, decimalLatitude,
                            decimalLongitude)
all_small_mat <- as.matrix(all_small)
cors <- cor(all_small_mat)
corrplot(cors)
