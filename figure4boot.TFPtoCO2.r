##COMPARE EFFECTS OF TFP CHANGES WITH NATIONAL SEQUESTRATION POTENTIALS
require(TFPtoCO2)
require(countrycode)
require(dplyr)
require(tidyverse)

## Read in Roe estimates, where cumulative refers to 30 years, 2020-2050
## columns correspond to
# Potential annual	Potential cumulative	Area	Density	Potential annual	Potential cumulative	Area	Density
# Technical 	Technical cumulative	Technical ha	Technical density	Cost-effective	Cost-effective cumulative	Cost-effective ha	Cost-effective density
# units are
# MtCO2e/yr	MtCO2e	ha total	tCO2/ha	MtCO2e/yr	MtCO2e	ha total	tCO2/ha

## scscrop_tech: Potential annual
## scscrop_techcum: Potential cumulative
## scscrop_tech_ha: Area
## scscrop_techden: Density
## scscrop_feas: Potential annual
## scscrop_feascum: Potential cumulative
## scscrop_feas_ha; Area
## scscrop_feasden: Density
## biochar_tech: Technical
## biochar_techcum: Technical cumulative
## biochar_tech_ha: Technical ha
## biochar_techden: Technical density
## biochar_feas: Cost-effective
## biochar_feascum: Cost-effective cumulative
## iochar_feas_ha: Cost-effective ha
## biochar_feasden: Cost-effective density

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

## Used ?get.responses for argument values (takes < 10 seconds):
system.time(
    responses.boot <- do.call(rbind,
                          lapply( iso2use, function(country){
                              e <- get.responses(
                                  ctry = country,
                                  tfp.shock=5,
                                  boot = TRUE
                                  )
                          }))
    )
pryr::object_size(responses.boot) ## 1.02 GB

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


## Figure 4 with confidence intervals:
labs = c('India','China','USA','EUR','Brazil','Argentina')

par(mar=c(5,8,2,2),las=1,mfrow=c(1,1))
barcols= c('#018571','#a6611a',gray(.7))
cols = c(0,0,gray(.7))
xlab=expression(paste('30-year Mitigation Potential (Gt CO'[2],')'))
ylim=c(-1,6.5)
a = barplot(
    t(as.matrix(pd[,c('scs_feas_wneg','scs_feas_wneg_dom','scs_feas')])),
            ylab='',xlab='',col=cols, axisnames = FALSE,
    xlim=ylim,beside=T,horiz=T,border=c(0,0,1))
axis(2,at=a[2,],labels=labs, las =2)
mtext(xlab,side=1,outer=F,line=2.5)
box()

for (i in 1:nrow(pd)){
    lines( rep(pd[i,'scs_feas'],2), a[2,i]+c(-1.5,1.5),lwd=3)

    rect(xleft=pd[i,'scs_feas'],xright=pd[i,'scs_feas_wneg_dom'],
         ybottom=a[2,i]+-.5,ytop=a[2,i]+.5,col=barcols[2])

    arrows(x0 = pd.ci[ pd.ci$region == pd$region[i] &
                       pd.ci$varname == 'scs_feas_wneg_dom' ,'ll'] ,
           y0 = a[2,i],
           x1 = pd.ci[ pd.ci$region == pd$region[i] &
                       pd.ci$varname == 'scs_feas_wneg_dom', 'ul'],
           y1 = a[2,i],
           angle = 90,
           code = 3,
           length = 0.01)
    rect(xleft=pd[i,'scs_feas'],xright=pd[i,'scs_feas_wneg'],
         ybottom=a[1,i]+-.5,ytop=a[1,i]+.5,col=barcols[1])
    arrows(x0 = pd.ci[ pd.ci$region == pd$region[i] &
                       pd.ci$varname == 'scs_feas_wneg' ,3] ,
           y0 = a[1,i],
           x1 = pd.ci[ pd.ci$region == pd$region[i] &
                       pd.ci$varname == 'scs_feas_wneg', 5],
           y1 = a[1,i],
           angle = 90,
           code = 3,
           length = 0.01)
}

legend(4,24,bty='n',leg=c('None','Domestic effects only','Global effects'),cex=.7,title='Accounting for TFP',
       title.adj=0.2,fill=rev(barcols))

## This is figure 4:
##
dev.print(png,'../figures/roe.comparison.dom.global.90CI.png',width=7,height=5.5,units='in',res=200)
dev.print(pdf,'../figures/Fig4.90CI.pdf',width=7,height=5.5)


## Land responses
# land.resp <- responses.boot %>%
#     group_by( i , boot ) %>%
#     summarise("Domestic effects" = -1*change.crops_ha[k == i ]/1e6,
#               "Global effects" = -1*sum(change.crops_ha)/1e6
#               ) %>%
#     gather( key="variable", value="value", -i, - boot) %>%
#     dplyr::rename( iso = i )
# 
# ## Group the EU and get the premia/penalties of TFP on mitigation
# ## potentials:
# land.responses.boot.agg <- land.resp %>%
#     mutate( region = ifelse( iso %in% eu, 'eu', iso ) ) %>%
#     select( !iso)  %>%
#     ## Group EU
#     group_by( region, boot, variable ) %>%
#     summarise( value = sum(value) )
# 
# ## Quantile-based (1 - alpha/2)*100 confidence intervals of
# ## TFP-penalized mitigation potentials:
# alpha <- 0.10
# landchange.ci <- aggregate(value ~ region + variable,
#                            land.responses.boot.agg,
#                            function(x) quantile(x,
#                                                 probs=c( alpha/2,0.5,(1-alpha/2)),
#                                                 names = TRUE))
# landchange.ci <- do.call(data.frame, landchange.ci)
# 
# landchange.ci$region.name  <- ifelse(
#     landchange.ci$region == 'eu', 'EUR',
#                               ifelse(landchange.ci$region == 'usa','USA',
#                                      countrycode(landchange.ci$region,"iso3c","country.name")))
# 
# landchange.ci$region.name <- factor(landchange.ci$region.name,
#                                 levels=labs)
# 
# ggplot(landchange.ci) +
#     geom_bar( aes(x=region.name, y=value.50., fill=variable),
#              stat="identity",  position="dodge",
#              alpha=0.7) +
#     geom_errorbar( aes(x=region.name, ymin=value.5.,
#                        ymax=value.95., group=variable),
#                   position=position_dodge(width = 0.9),
#                   width=0.1) +
#     ylab("Area Response (Mha)") +
#     xlab("") +
#     theme_minimal() +
#     theme(legend.position=c(0.9,0.9)) +
#     theme(legend.title=element_blank())
# 
# dev.print(png,'../figures/dom.global.changes.in.area.tfp-reduction.90CI.png',width=7,height=5.5,units='in',res=200)
