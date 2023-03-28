##COMPARE EFFECTS OF TFP CHANGES WITH NATIONAL SEQUESTRATION POTENTIALS

##THIS SCRIPT LOOKS AT MODIFYING EMISSIONS FACTORS FOR LUC AND EFFECT ON RESULTS
#setwd('~/Dropbox/CarbonAndTFP/programs/')

require(TFPtoCO2)
require(countrycode)
require(dplyr)
require(tidyverse)

## Read in Roe estimates, where cumulative refers to 30 years, 2020-2050
roe.est <- read.csv("../data/Roe.ag.seq.csv")
roe.est$iso = tolower(roe.est[,1])
roe.est$eu = countrycode(roe.est$iso, "iso3c", "eu28")

##---------------------------------------------------------------------------##
## With bootstraped elasticities:
##---------------------------------------------------------------------------##

## Load elasticties included in TFPtoCO2:
data(elasticities)
eu <- unique(elasticities$i)[which(countrycode(unique(elasticities$i),
                                               "iso3c", "eu28") == 'EU')]
iso2use=c('ind','chn','usa','bra','arg', eu)

#define a new function that allows you to override the emiss factors
get.responses.newEF <- function(ctry, tfp.shock = 1,
                                boot=NULL, which.boot = "new"){
  ## Load data with elasticities:
  if( isTRUE(boot) ) {
    if( which.boot == "new" ){
      elasticities <- bootstrap.elasticities(ctry)
    }else{
      elasticities <- bootstrap.elasticities.old(ctry)
    }
  }else{
    data( list = "elasticities", envir=environment())
    ## assign("elasticities",get(elasticities.df))
  }
  
  e <- elasticities[elasticities$i == ctry,]
  ## By the definition of the elasticity, ## % change in cropland_k
  ## = % change in TFP_i X elasticity_ik
  e$perc.change.crop.area <- with( e, tfp.shock*e.ik )
  ## Load emission factors
  e <- left_join( e, physicaldata, by = c( "k" = "iso") )
  ## Translate the perc. change to ha using the average cropland:
  e$change.crops_ha <- with( e, perc.change.crop.area*crops_ha/100)
  ## convert area changes to emissions
  e$emissions_GtCO2  <- with( e, (change.crops_ha)*newEF )
  if( isTRUE(boot) ){
    e <- e[,c("i","k","boot",
              "perc.change.crop.area","change.crops_ha",
              "emissions_GtCO2")]
  }else{
    e <- e[,c("i","k","perc.change.crop.area","change.crops_ha",
              "emissions_GtCO2")]
  }
  return(e)
}

#DEFINE SOME ALTERNATIVE EF SCENARIOS
data( list = "emission_data.ave", envir=environment())

hold.byscen = list()
for (scen in 1:5){
#scen 1 = basecase
if (scen == 1) mod_emission_data.ave = emission_data.ave %>% mutate(newEF =EF)

#scen 2 = reduce all EF by 10%
if (scen == 2) mod_emission_data.ave = emission_data.ave %>% mutate(newEF =EF * 0.9)

#scen 3 = increase all EF by 10%
if (scen == 3) mod_emission_data.ave = emission_data.ave %>% mutate(newEF =EF * 1.1)

#scen 4 = tighten range to 300-700 instead of 200-800
if (scen == 4) mod_emission_data.ave = emission_data.ave %>% mutate(newEF = 
                                                       pmax(300e-9,pmin(700e-9,EF)))
#scen 5 = set to a constant of 500 instead of 200-800
if (scen == 5) {
  avEF = weighted.mean(emission_data.ave$EF,emission_data.ave$crops_ha)
  mod_emission_data.ave = emission_data.ave %>% mutate(newEF = avEF)
}

physicaldata <- mod_emission_data.ave

## Used ?get.responses for argument values (takes < 10 seconds):
system.time(
  responses.boot <- do.call(rbind,
                            lapply( iso2use, function(country){
                              e <- get.responses.newEF(
                                ctry = country,
                                tfp.shock=5,
                                boot = TRUE
                              )
                            }))
)
#pryr::object_size(responses.boot) ## 1.02 GB

## Domestic and global emissions by country:
emis.resp <- responses.boot %>%
  group_by( i , boot ) %>%
  summarise(Emis_D = emissions_GtCO2[k == i ],
            Emis_G = sum(emissions_GtCO2)
  ) %>%
  gather( key="variable", value="value", -i, - boot) %>%
  dplyr::rename( iso = i )

## Mitigation potential by country (from Roe et al.):
scs_feas.by.region <- roe.est %>%
  select(iso, scscrop_feascum ) %>%
  filter(iso %in% iso2use) %>%
  ## This estimate of mitigation potential is based on only 22
  ## EU countries that match where we have emissions estimates:
  mutate( region = ifelse( iso %in% eu, 'eu', iso ) ) %>%
  ## Group EU
  group_by( region ) %>%
  summarise( scs_feas = sum(scscrop_feascum)/ 1e3 )  #(convert from Mt to Gt)

## Group the EU and get the premia/penalties of TFP on mitigation
## potentials:
responses.boot.agg <- emis.resp %>%
  mutate( region = ifelse( iso %in% eu, 'eu', iso ) ) %>%
  select( !iso)  %>%
  ## Group EU
  group_by( region, boot, variable ) %>%
  summarise( value = sum(value) ) %>%
  ## Add substract global and domestic emissions from mitigation potentials
  mutate(scs_feas_wpos = value -
           scs_feas.by.region$scs_feas[scs_feas.by.region$region == region],
         scs_feas_wneg = value +
           scs_feas.by.region$scs_feas[scs_feas.by.region$region==region]
  ) %>%
  select( !value ) %>%
  gather(., key = "variable2", value = "value", -region, -boot, - variable) %>%
  unite( "varname" ,  variable, variable2, remove = TRUE)

## Quantile-based (1 - alpha/2)*100 confidence intervals of
## TFP-penalized mitigation potentials::
alpha <- 0.10
mitpot.ci <- aggregate(value ~ region + varname,
                       responses.boot.agg,
                       function(x) quantile(x,
                                            probs=c( alpha/2,0.5,(1-alpha/2)),
                                            names = TRUE))
mitpot.ci <- do.call(data.frame, mitpot.ci)

## Use David's var names to make plotting easier:
mitpot.ci$varname <- with(mitpot.ci,
                          ifelse(
                            varname == "Emis_D_scs_feas_wneg",
                            "scs_feas_wneg_dom",
                            ifelse(
                              varname == "Emis_D_scs_feas_wpos",
                              "scs_feas_wpos_dom",
                              ifelse(
                                varname == "Emis_G_scs_feas_wneg",
                                "scs_feas_wneg",
                                "scs_feas_wpos"
                              ))))

pd.ci <- mitpot.ci %>% filter(
  varname %in% c('scs_feas_wneg','scs_feas_wneg_dom') )
names(pd.ci)[3:5] <- c("ll","median","ul")


library(reshape2)
pd <- cbind(scs_feas.by.region,
            dcast(pd.ci, region ~ varname, value.var = "median")[,-1])

pd$id <- c(6,5,2,4,1,3)

pd <- pd[order(pd$id),-5]
pd$reduction = 1- pd$scs_feas_wneg/pd$scs_feas
hold.byscen[[scen]]=pd
}

#make table for summary
out = data.frame(pd[,1:2],sapply(hold.byscen,function(x) round(100*x$reduction)))
names(out) = c('region','C storage potential (Gt)',paste('Scen',1:5))
out[,2] = round(out[,2],2)
write.csv(out,'../manuscript/EFsens.table.csv',row.names = F)
