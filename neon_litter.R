setwd("/Users/Anika/Downloads/")

require(neonUtilities)
require(stringr)

#download data
litterchem <- loadByProduct(dpID="DP1.10031.001", site="all",package="basic",check.size=F)

#split tables
for(i in 1:length(litterchem)){
  assign(names(litterchem)[i], litterchem[[i]])
}

head(litterchem$ltr_litterCarbonNitrogen)

#massSampleMixtureID has .LVS for leaves and .NDL for needles
litter <- litterchem$ltr_litterCarbonNitrogen
litter$cnSampleID <- as.character(litter$cnSampleID)
litter$type <- str_sub(litter$cnSampleID, - 6, - 1)
litter <- litter %>% filter(type == "LVS.CN")

litter2 <- litterchem$ltr_litterLignin
litter2$ligninSampleID <- as.character(litter2$ligninSampleID)
litter2$type <- str_sub(litter2$ligninSampleID, - 7, - 1)
litter2 <- litter2 %>% filter(type == "LVS.LIG")
litter2 <- litter2 %>% group_by(plotID) %>% summarize(dryMass=mean(dryMass,na.rm=T),
                                                      ligninPercent=mean(ligninPercent,na.rm=T),
                                                      cellulosePercent=mean(cellulosePercent,na.rm=T))

#get relevant data
#plotID, collectDate, nitrogenPercent, carbonPercent, CNratio
litter <- litter[,c("siteID","plotID","collectDate","namedLocation","nitrogenPercent","carbonPercent","CNratio")]
litter2 <- litter2[,c("plotID","dryMass","ligninPercent","cellulosePercent")]
litter3 <- merge(litter, litter2, by="plotID")

#save output
saveRDS(litter3, "litter_chem.RDS")
write.csv(litter3, "litter_chem.csv", row.names = F)

#playing around
hist(litter$nitrogenPercent)
hist(litter$carbonPercent)
hist(litter$CNratio)

require(dplyr)
require(ggplot2)
litter %>% 
  ggplot(aes(x=siteID,y=CNratio)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) 
