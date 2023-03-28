## R package built for this paper:
library(TFPtoCO2)
library(dplyr)

## get emissions for all the countries (takes < 1.40 sec.):
data(elasticities)
countries <- unique(elasticities$i)

system.time(
pe.responses.all.i <- data.table::rbindlist(
    lapply( countries , function(x){
    r <- get.responses(ctry=x, boot = FALSE)
    return(r)
    }))
)

## Prepare matrix for supporting information spreadsheet:
pe.responses.all.i$Country.i <- countrycode::countrycode(pe.responses.all.i$i, 'iso3c',
                                                    'country.name')
pe.responses.all.i$Country.k <- countrycode::countrycode(pe.responses.all.i$k, 'iso3c',
                                                    'country.name')

emission.matrix <- reshape2::acast( pe.responses.all.i,
                                   Country.i ~ Country.k, value.var = "emissions_GtCO2")


## get bootstrapped emissions for all the countries (took about 21
## seconds to complete)::
system.time(
responses.all.i <- data.table::rbindlist(
    lapply( countries , function(x){
    r <- get.responses(ctry=x, boot = TRUE)
    return(r)
    }))
)

## Post-processing of results: Table with domestic, foreign and total
## emissions from domestic TFP for SI spreadsheet:
alpha <- 0.1
table.totals.ci <- lapply( countries, function(x){
    d <- subset(responses.all.i, i == x)
    own <- aggregate(emissions_GtCO2 ~ k,
                           subset(d, k == x),
                           function(x) quantile(x,
                                                probs=c( alpha/2,0.5,(1-alpha/2)),
                                                names = TRUE))
    own <- data.frame(effect = 'Domestic',
                           stat=c('lower.bound','median','upper.bound'),
                           Emissions_GtCO2= t(own[,-1]))
    foreign <-  data.frame(effect = 'Foreign',
                           stat=c('lower.bound','median','upper.bound'),
                           Emissions_GtCO2=quantile(d$emissions_GtCO2[d$k !=x],
                                    probs=c( alpha/2,0.5,(1-alpha/2)),
                                    names = TRUE))
    total <- data.frame(effect = 'Total',
                        stat = c('lower.bound','median','upper.bound'),
                        Emissions_GtCO2=quantile(d$emissions_GtCO2,
                                                 probs=c( alpha/2,0.5,(1-alpha/2)),
                                                 names = TRUE) )
    results <- rbind( own, foreign, total)
    results$iso <- x
    results
    })
table.totals.ci <- data.table::rbindlist(table.totals.ci)

table.totals.ci$Country <- countrycode::countrycode(table.totals.ci$iso, 'iso3c',
                                                    'country.name')

table.totals.ci <- reshape2::acast( table.totals.ci,
                         Country ~ effect + stat,
                         value.var = "Emissions_GtCO2")


xlsx::write.xlsx(table.totals.ci, file ="../manuscript/TabulatedEffects_SI.xlsx",
           sheetName = "Global Emissions",
           col.names = TRUE, row.names = TRUE, append = FALSE)

xlsx::write.xlsx(emission.matrix, file ="../manuscript/TabulatedEffects_SI.xlsx",
           sheetName = "Country Emissions",
           col.names = TRUE, row.names = TRUE, append = TRUE)

## Write emission factors to SI
data(emission_data.ave)
?emission_data.ave
head(emission_data.ave)
EF.SI <- emission_data.ave[,c("iso","EF")]
EF.SI$EF <- EF.SI$EF*1e9 # convert to tons
EF.SI$country <- countrycode::countrycode(EF.SI$iso, "iso3c", "country.name")
EF.SI <- EF.SI[,c("country", "EF")]
xlsx::write.xlsx(EF.SI, file ="../manuscript/TabulatedEffects_SI.xlsx",
           sheetName = "Emission Factors",
           col.names = TRUE, row.names = TRUE, append = TRUE)



## 11/29/2022
## Domestic elasticities:
elasticities.boot.samples <- data.table::rbindlist(
    lapply( countries , function(x){
    r <- bootstrap.elasticities(x)
    return(r)
    }))

dom.elasticities <- elasticities.boot.samples %>%
    filter(i==k) %>%
    group_by(i) %>%
    summarise( e.ik = quantile(e.ik, c(0.05, .5, .95)), q=c(0.05, .5, .95)  )

dom.elasticities <-  as.data.frame(reshape2::acast(dom.elasticities, i ~ q, value.var="e.ik"))

dom.elasticities$Country <- countrycode::countrycode(row.names(dom.elasticities), 'iso3c',
                                                     'country.name')
dom.elasticities <- dom.elasticities %>%relocate(Country)

xlsx::write.xlsx(dom.elasticities, file ="../manuscript/TabulatedEffects_SI.xlsx",
           sheetName = "Domestic Elasticities",
           col.names = TRUE, row.names = FALSE, append = TRUE)



## Foreign (bilateral) elasticities:
foreign.elasticities <- elasticities.boot.samples %>%
#    filter(i!=k) %>%
    group_by(i,k) %>%
    summarise( e.ik = mean(e.ik) )
head(foreign.elasticities)
## Prepare matrix for supporting information spreadsheet:
foreign.elasticities$Country.i <- countrycode::countrycode(foreign.elasticities$i, 'iso3c',
                                                    'country.name')
foreign.elasticities$Country.k <- countrycode::countrycode(foreign.elasticities$k, 'iso3c',
                                                    'country.name')

foreign.elasticities.matrix <- reshape2::acast( foreign.elasticities,
                                   Country.i ~ Country.k, value.var = "e.ik")
head(foreign.elasticities.matrix)
foreign.elasticities.matrix[is.na(foreign.elasticities.matrix)] <- 0
xlsx::write.xlsx(foreign.elasticities.matrix, file ="../manuscript/TabulatedEffects_SI.xlsx",
           sheetName = "Bilateral Elasticities",
           col.names = TRUE, row.names = TRUE, append = TRUE)
