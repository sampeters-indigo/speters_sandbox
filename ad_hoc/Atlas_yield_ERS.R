#######
#Script to create ZIP code level dataset of Atlas yields matched with ERS costs
#######

#Packages Used
library (data.table)

#Load data
setwd("/Users/samuelpeters/OneDrive - INDIGO AG, INC/Profitability")
atlas_yields = read.csv ("atlas_yields.csv")
ers_cost = read.csv("ers_cost.csv")
ers_region_county = read.csv("ers_region_to_county.csv")
zip_county = read.csv ("zip_to_census_tract_201909.csv")

#Preparing ZIP and county file for using all possible pairs of zip codes and counties
#Create county column from FIPS
zip_county = transform (zip_county, County = substr(tract, 1, 5))
#Cut to just ZIP and County
zip_county = subset(zip_county, select = c(zip, County))
#Get rid of duplicate rows (now ready for merging with other datasets)
zip_county <- unique( zip_county[ , 1:2 ] )

#Preparing ZIP and County file crosswalking with highest percentage country for each zip code
zip_county = transform (zip_county, County = substr(tract, 1, 5))
zip_county <- as.data.table(zip_county)
#Select only zip-county pairs with largest overlap
zip_county <- zip_county[zip_county[, .I[tot_ratio == max(tot_ratio)], by=zip]$V1]
#Cut to just ZIP and County, get rid of duplicates, and return to data frame (now ready for merging with other datasets)
zip_county = subset(zip_county, select = c(zip, County))
zip_county <- unique( zip_county[ , 1:2 ] )
zip_county <- as.data.frame(zip_county)

#Prepare ERS Region County File for Merging
#Cut to just region and county and make all counties 5 digits in ers
ers_region_county = subset(ers_region_county, select = c(Fips, ERS_Region))
ers_region_county$Fips = sprintf("%05d", ers_region_county$Fips)

#Get names to ERS Region beacuse the numeric isnt in the cost file
levels <- c(-Inf, 1, 2, 3, 4, 5, 6, 7, 8, 9)
labels <- c("Heartland", "Northern Crescent", "Northern Great Plains", "Prairie Gateway", "Eastern Uplands", "Southern Seaboard", 
            "Fruitful Rim", "Basin and Range", "Mississippi Portal")
ers_region_county =  ers_region_county %>%
  mutate (ERS_Region_Name = cut(ERS_Region, levels, labels = labels))

#Merge ZIP and ERS Region
ers_region_county$County = ers_region_county$Fips
merge1 = merge (x=zip_county, y=ers_region_county, by="County", all.x = T)

#Cut out Fips column because it's a repeat
merge1 = subset (merge1, select = -Fips)

#Trim ers cost to what we need and subset for corn and soy only
ers_cost = subset(ers_cost, select=c(COMMODITY, REGION, VALUE))
ers_cost = subset (ers_cost, COMMODITY == "Corn" | COMMODITY == "Soybean")

#Merge all ers costs to zip/county and ers costs
ers_cost$ERS_Region_Name <- ers_cost$REGION
merge2 = merge (x=merge1, y=ers_cost, by="ERS_Region_Name", all.x=T )
merge2 = subset (merge2, select = -REGION)

#Merge in Atlas results
atlas_yields$GEO_ID = sprintf("%05d", atlas_yields$GEO_ID)
atlas_yields$County <- atlas_yields$GEO_ID
atlas_yields$CROP <- revalue(atlas_yields$CROP, c("soy"="Soybean", "corn"="Corn"))
atlas_yields$COMMODITY <- atlas_yields$CROP
merge3 = merge (x=merge2, y=atlas_yields, by=c("County", "COMMODITY"), all.x=T)

#Clean up final data frame and write to csv
merge3$Atlas_yield = merge3$SMOOTHED_MODEL_VALUE
merge3$Ers_Cost <- merge3$VALUE
merge3 = subset(merge3, select = -c(SMOOTHED_MODEL_VALUE, CROP, VALUE, GEO_ID))
merge3 = merge3[order(merge3$zip),]
merge3 = merge3[, c(4,1,5,3,2,7,6)]

write.csv(merge3, "Atlas_ERS_Max_County_Ratio.csv")

#Check unique counts
merge3 %>% summarise_all(n_distinct)

