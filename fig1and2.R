#MAKE SUMMARY FIG OF DOMESTIC AND GLOBAL AREA AND EMISSIONS RESPONSES

#setwd('~/Dropbox/CarbonAndTFP/programs/')

require(TFPtoCO2)
require(countrycode)
require(dplyr)
require(tidyverse)

data( list = "emission_data.ave", envir=environment())

#function to sum area and emissions resp across countries
get.resp = function(ctry){
  e = get.responses(ctry)
  #for area, get change in area (in Mha) domestic, all foreign, and global
  area.resp = c(e$change.crops_ha[e$k == ctry ],sum(e$change.crops_ha[e$k != ctry ]),
                sum(e$change.crops_ha) )/1e6
  #emissions
  emis.resp = c(e$emissions_GtCO2[e$k == ctry ],sum(e$emissions_GtCO2[e$k != ctry ]),
                sum(e$emissions_GtCO2))
  c(area.resp,emis.resp)
}
isos = emission_data.ave$iso
responses = data.frame(iso=isos,t(sapply(isos,get.resp)))
names(responses) = c('iso','Area_D','Area_F','Area_G','Emis_D','Emis_F','Emis_G')

#FIGURE 1
par(mfrow=c(1,2),mar=c(5,4,1,1))
radius <- sqrt(emission_data.ave$crops_ha/ pi)
ylim=c(-1,1)
symbols(responses$Area_D,responses$Area_G,xlab='Domestic Area Response (Mha)',
        ylab='Global Area Response (Mha)',
        col=1,cex=.2,xlim=ylim,ylim=ylim,circles=radius,inches=0.25, fg="white", bg="gray")
ind = which(emission_data.ave$crops_ha >= 5e7)
text(responses$Area_D[ind],responses$Area_G[ind],emission_data.ave$iso[ind])
abline(v=0);abline(h=0);abline(c(0,1),lty=2)
text(-1,.95,lab='(a)',cex=1.5)

ylim=c(-.40,.40)
symbols(responses$Emis_D,responses$Emis_G,xlab=expression(paste('Domestic Emission Response (Gt ',CO[2],')')),
        ylab='Global Emission Response (Gt)',
        col=1,cex=.2,xlim=ylim,ylim=ylim,circles=radius,inches=0.25, fg="white", bg="gray")
text(responses$Emis_D[ind],responses$Emis_G[ind],emission_data.ave$iso[ind])
abline(v=0);abline(h=0);abline(c(0,1),lty=2)
text(-.4,.38,lab='(b)',cex=1.5)
dev.print(png,'../figures/fig1.dom.glob.areaandemis.png',width=10,height=4,units='in',res=300)
dev.print(pdf,'../figures/fig1.dom.glob.areaandemis.pdf',width=10,height=4)

#FIGURE 2
#first calc EU totals
responses$eu = countrycode(responses$iso, "iso3c", "eu28")
responses$cont = countrycode(responses$iso, "iso3c", "continent")
responses= left_join(responses,emission_data.ave,by='iso')
eu = responses %>% filter(eu=='EU')
#calc resp per ha
responses$emis_perha = -1e9 * responses$Emis_G / responses$crops_ha
eu = responses %>% filter(eu=='EU')

#show map of emissions
require(sf)
require(rnaturalearth)
require(classInt)
require(viridis)
require(plotrix)
cntries <- ne_countries(scale = 110,
                        type = 'countries',
                        continent = c('africa','europe','asia','north america','south america'),
                        returnclass = "sf")
cntries$iso = tolower(cntries$iso_a3)
cntries = left_join(cntries,responses,by='iso')

ext=st_bbox(c(xmin = -160, xmax = 160, ymax = 60, ymin = -60), crs = st_crs(4326))
breaks=c(-99,0,2,4,6,99)
pal=viridis(n=length(breaks)-1,begin=0,end=1)
layout(t(c(1,1,2)))
par(mar=c(5,4,1,1))
plot(cntries['emis_perha'],breaks=breaks,pal=pal,extent = ext,main='',
     key.pos = NULL, reset = FALSE)

# Add the legend
add.sfleg = function(labels,cols){
  axisLimits <- par()$usr
  xLength <- axisLimits[2] - axisLimits[1]
  yLength <- axisLimits[4] - axisLimits[3]
  plotrix::color.legend(xl=axisLimits[2] - 0.9*xLength, xr=axisLimits[2] - 0.1*xLength,
                        yb=axisLimits[3]+.28* yLength, yt=axisLimits[3] + 0.33 * yLength,
                        legend = labels, rect.col = cols, 
                        gradient="x")
}

add.sfleg(labels = c("<0", "0-2", "2-4", "4-6", ">6"),
          cols = pal)
mtext('Avoided Emissions',1,line=-4.2,adj=0.5,cex=1.2)
mtext(expression(paste('(t CO'[2],' per ha per % TFP)')),1,line=-2.6,adj=0.5)

isos = c('ind','chn','usa')
yslop = c(sapply(isos,function(x) responses$Emis_G[responses$iso == x]),sum(eu$Emis_G))*-1
xvals=seq(-5,5,1)
par(mar=c(9,5,7,1))
plot(xvals,xvals,col=0,axes=F,xlim=c(-5,5),ylim=c(-2,2),xlab='TFP effect (%)',
     ylab=expression(paste('Total avoided emissions (Gt CO'[2],')')),cex.lab=2)
abline(h=seq(-2,2,by=1),col=gray(.9))
cols=brewer.pal(4,'Dark2')
axis(1,cex.axis=2);axis(2,cex.axis=2);
abline(h=0,lwd=2,lty=2)
abline(v=0,lwd=2,lty=2)
for (i in 1:4) lines(xvals,xvals*yslop[i],col=cols[i],lwd=2)
box()
text(-4,c(2,1.8,1.6,1.4)-.05,labels=c('India','China','USA','Europe'),col=cols,cex=1.5,adj=c(0,1))
dev.print(png,'../figures/Fig2.map.wlines.png',width=12,height=6,units='in',res=300)
dev.print(pdf,'../figures/Fig2.map.wlines.pdf',width=12,height=6)
