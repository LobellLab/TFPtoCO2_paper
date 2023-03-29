## Preprocess data on land use and carbon emissions from Chaopeng Hong
## et al. (https://www.nature.com/articles/s41586-020-03138-y)

## load:
library(readxl)
library(reshape2)
library(dplyr)
library(countrycode)
require(terra)
require(rnaturalearth)
require(tidyverse)

## --------------------------------------------------------------------------------- ##
## Committed Emissions;
## --------------------------------------------------------------------------------- ##
committed_emissions <- read_xlsx(path = "../data/LUE_Data_committed_ForDL.xlsx")

## Assign variable names, fix the year values:
committed_emissions <-     melt(committed_emissions) %>%
  dplyr::rename( "country" =  'Emis (Gt CO2)',
                 "year" = "variable",
                 "Gt_CO2" = "value") %>%
  mutate( year = substr( year, 2, 5 )) %>%
  dplyr::select( country, year, Gt_CO2 )

## --------------------------------------------------------------------------------- ##
## Agricultural land Use:
## --------------------------------------------------------------------------------- ##

## 9 Agricultural land area by country (two worksheets)
##     9.1 Total agricultural land 1961-2017 (hectares)
##     9.2 Agricultural land by product group 1961-2017 (hectares)

## worksheet "9.1.AgLand": Total agricultural land 1961-2017 (hectares)
total_agricultural_land <- read_xlsx(path = "../data/LUE_Data_CALUE.xlsx", sheet = "9.1.AgLand")

## Assign variable names, fix the year values:
total_agricultural_land <-  melt(total_agricultural_land) %>%
  dplyr::rename( "country" =  'Area',
                 "year" = "variable",
                 "totalag_ha" = "value") %>%
  mutate( year = substr( year, 2, 5 )) %>%
  dplyr::select( country, year, totalag_ha )

## worksheet "9.2.AgLand": Total Agricultural land by product group 1961-2017 (hectares)
total_agricultural_land_byproduct <- read_xlsx(path = "../data/LUE_Data_CALUE.xlsx", sheet = "9.2.AgLand")

## Assign variable names, fix the year values, and add up crop areas:
total_agricultural_land_byproduct <-  melt(total_agricultural_land_byproduct) %>%
  dplyr::rename( "country" =  'Area',
                 "product_group" =  'Product Group',
                 "year" = "variable",
                 "ha" = "value") %>%
  mutate( year = substr( year, 2, 5 )) %>%
  ## Selects crops to get cropland. Omits land in wood, pork, beef,
  ## chicken, other meat, dairy.
  filter(  product_group %in% c("Cereals", "Veg", "Fruit", "Pulses", "Oil Crops", "Fiber", "Spices", "Sugar") ) %>%
  group_by( country, year ) %>%
  summarize( crops_ha = sum(ha) ) %>%
  ungroup()

## Join datasets:
emission_data <- left_join( committed_emissions, total_agricultural_land, by = c( "country", "year") ) %>%
  left_join( ., total_agricultural_land_byproduct, by = c( "country", "year") )

## Add cropland data from FAO that was used as input for Hong
total_crop_land <- read.csv("../data/Inputs_Land_E_All_Data.csv")
total_crop_land <- total_crop_land %>% filter(Item == 'Cropland') %>% 
  dplyr::select(c('Area',paste0('Y',1961:2017)))
total_cropland_area <-  melt(total_crop_land) %>%
  dplyr::rename( "country" =  'Area',
                 "year" = "variable",
                 "cropland_ha" = "value") %>%
  mutate( year = substr( year, 2, 5 )) %>%
  mutate( cropland_ha = cropland_ha * 1e3) 

emission_data <- emission_data %>%
  left_join( ., total_cropland_area, by = c( "country", "year") )

## Add iso codes:
emission_data$iso  <- tolower(countrycode( emission_data$country, "country.name", "iso3c"))

## Read elasticities published in AJAE paper (replication data and
## code in ./AJAE_replication/) to get the countries (iso codes) with
## available elasticities:
load("../data/elasticities.Rdata")
iso.sample <- unique(elasticities$i)


## Keep only iso codes with available elasticities:
emission_data <- emission_data %>%
  dplyr::filter( iso %in% iso.sample ) %>%
  dplyr::select( iso, year, Gt_CO2, totalag_ha, crops_ha, cropland_ha )

## Check all iso with elasticities have emission/land use data:
sort(unique(emission_data$iso)) == iso.sample
save(emission_data, file = "../data/emission_data.RData")

#now calculate emissions intensity of LUC
#add yearly changes in ag and cropland area
emission_data  = emission_data %>% group_by(iso) %>% mutate(delag = c(NA,diff(totalag_ha)),
                                                            delcropland = c(NA,diff(cropland_ha)),
                                                            delcrops_ha = c(NA,diff(crops_ha))) 
## Average the emission factor over the last three years to smooth
## annual fluctuations:
emission_data.ave <- emission_data %>%
  filter( year %in% c(1998:2017) ) %>%
  group_by(iso) %>%
  summarise( crops_ha = mean(crops_ha), Gt_CO2 = sum(Gt_CO2),delcrops_ha = sum(delcrops_ha),) %>%
  mutate(EF = pmax(200e-9,pmin(800e-9,Gt_CO2/delcrops_ha)))
#for now, just cap values to between 200 and 800 t CO2/ha. will revisit later once can resolve gross vs net area change

## reset countries that have high rates of abandonment
setwd("~/Dropbox/CarbonAndTFP/data/")

#these are downloaded from https://glad.umd.edu/dataset/croplands
#values were calculated for 2003-2019
gain = rast('~/Downloads/Global_cropland_3km_netgain.tif')
loss = rast('~/Downloads/Global_cropland_3km_netloss.tif')

ww_ini <- ne_countries(scale = "medium",
                       type = 'countries',
                       returnclass = "sf")

areas =cellSize(gain,unit='km')

gain_area = gain * areas
loss_area = loss * areas

gain_byctry = terra::extract(gain_area,ww_ini,fun=sum,na.rm=T)
loss_byctry = terra::extract(loss_area,ww_ini,fun=sum,na.rm=T)

gain_byctry$iso = tolower(ww_ini$iso_a3)

gainloss = data.frame(gain_byctry[,3:2],loss_byctry[,2]) 
names(gainloss)[2:3] = c('gain','loss')
gainloss = gainloss %>% mutate(gainratio = gain/loss) %>% arrange(desc(gain))
head(gainloss,25)

#save gain loss data for use later
write.csv(gainloss,'../data/potapov.gainloss.summary.csv',row.names=F)
#add info on gain vs. loss in potatpov
gainloss = read.csv('../data/potapov.gainloss.summary.csv')

#for countries with lots of abandonment, replace estimate with 200 e-9
emission_data.ave = left_join(emission_data.ave,gainloss %>% select(c('iso','gainratio')),by='iso') 
emission_data.ave$EF[which(emission_data.ave$gainratio < 1)] = 200e-9
