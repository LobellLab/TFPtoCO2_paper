rm(list=ls())
library(readxl)
library(reshape2)
library(dplyr)
library(countrycode)

## setwd('~/Dropbox/CarbonAndTFP/programs/')
## -----------------------------------------------------------------##
## Read Data and calculate emission factors:
## -----------------------------------------------------------------##

## Read 2001-10 elasticities published in AJAE paper (replication data
## and code in ./AJAE_replication/)
load("../data/elasticities.Rdata")
#names(elasticities)
## e.ik = % change in cropland_k / % change in TFP_i, where i and k
## index countries. For domestic elasticities, i = k.

data( list = "emission_data.ave", envir=environment())

## add region to elasticities
elasticities$reg.i = countrycode( elasticities$i, "iso3c", "region")
elasticities$reg.k = countrycode( elasticities$k, "iso3c", "region")

load("../data/boot.elasticities.RData")

nrow(boot.elasticities)/86^2 ## Number of reps

get.byctry.ci = function(ctry = "bra", alpha = 0.10){
  tfp.shock <- 1 ## in percent
  e <- boot.elasticities %>% filter( i == ctry )
  e <- left_join(e, emission_data.ave, by = c("k" = "iso") )
  ## By the definition of the elasticity,
  ## % change in cropland_k = % change in TFP_i X elasticity_ik
  e$perc.change.crop.area <- with( e, tfp.shock*e.ik )
  ## Translate the perc. change to ha using the average cropland:
  e$change.crops_ha <- with( e, perc.change.crop.area*crops_ha/100)
  ## convert area changes to emissions
  e$emissions  <- with( e, (change.crops_ha)*EF )
  ## Quantile-based confidence intervals
  emission.ci <- aggregate(emissions ~ k,
                           e,
                           function(x) quantile(x,
                                                probs=c( alpha/2,0.5,(1-alpha/2)),
                                                names = TRUE))
  emission.ci <- do.call(data.frame, emission.ci)
  emission.ci$i <- ctry
  emission.ci %>% arrange(desc(abs(emissions.50.)))
}


#####
nctry=50
top.iso = (emission_data.ave %>% arrange(desc(crops_ha)))$iso[1:nctry]
labs = countrycode(top.iso,'iso3c','country.name')
#em.byctry = lapply(top.iso,get.byctry)
## NV:
em.byctry.ci = lapply(top.iso,function(x) get.byctry.ci(x, alpha = 0.1))

get.sens.ci = function(ctry='bra'){
  out = lapply(em.byctry.ci,function(x) {
    ##  x <- em.byctry.ci[[2]]
    k.id = which(x$k == ctry)
    subset(x[k.id,], select = - k)
  })## ))
  out <- do.call(rbind, out)
  names(out) = c('ll.Gt','Gt','ul.Gt','iso')
  out <- out[ , c('iso','ll.Gt','Gt','ul.Gt')]
  out[order(abs(out$Gt), decreasing = TRUE),]
}

### FIGURE 3 with CI
ploti = 1:10
par(mfrow=c(3,3),mar=c(4,.5,1,.5))
xlim = c(-.38,.38)
use=c(1,2,3,4,6,7,8,9,15)
pans = c('(a)','(b)','(c)','(d)','(e)','(f)','(g)','(h)','(i)')

for (i in 1:length(use)){
  # i <- 8
  ctry = top.iso[use[i]]
  pd = get.sens.ci(ctry)
  #make sure domestic effects show up first
  pd = rbind(pd %>% filter(iso==ctry),pd %>% filter(iso!=ctry))
  a= barplot(pd$Gt[ploti],xlab='',ylab='',
             xlim=xlim,horiz=T,col=gray(.8),
             cex.axis = 1.3)
  text(xlim[1]-.005,max(a)+1.2,paste(pans[i],labs[use[i]]),pos=4,xpd=NA,cex=1.7)
  
  xp <- (pmax( abs(pd$ll.Gt[ploti]),abs(pd$ul.Gt[ploti]))-0.001)*sign(pd$Gt[ploti])
  
  text(xp,a,pos=3+sign(pd$Gt[ploti]),pd$iso[ploti],cex=1.3)
  arrows(x0 = pd$ll.Gt[ploti],
         y0 = a,
         x1 = pd$ul.Gt[ploti],
         ##        y1 = a,
         angle = 90,
         code = 3,
         length = 0.01
  )
}

mtext(expression(paste('Emissions Response (GtCO'[2],')')),side=1,outer=T,line=-1)


dev.print(png,'../figures/Fig3.90CI.png',width=10,height=7,units='in',res=300)
dev.print(pdf,'../figures/Fig3.90CI.pdf',width=10,height=7)

