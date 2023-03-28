## Remove old package and install new one from Github:
remove.packages("LUEandAGTFP")
## remove.packages("TFPtoCO2")
devtools::install_github("nvilloria/TFPtoCO2",build_vignettes = TRUE)
library(TFPtoCO2)

## Data documentation
?elasticities
?emission_data.ave

## Looking at the data is convenient:
data(elasticities)
with( elasticities, elasticities[ i == "usa" & k == "mex",])

data(emission_data.ave)
with(emission_data.ave, emission_data.ave[iso %in% c("rus", "vnm", "usa", "col", "bra"),])

## Function documentation
?get.responses
?bootstrap.elasticities

## Take a look at the functions:
get.responses
bootstrap.elasticities

## Look at basic examples of usage:
vignette("brief_tutorial")

## Usage: The defaults is a 1% change in TFP using the point estimate
## elasticities, so these are equivalent:
test.1 <- get.responses(ctry="bra")
test.2 <- get.responses(ctry="bra", tfp.shock =  1)

## Here we get 10,000 estimates for a single country:
test.boot <- get.responses("bra", tfp.shock = -3, boot = TRUE)

## For all the countries
data(elasticities)
countries <- unique(elasticities$i)

## get emissions for all the countries (takes < 1.32 sec.)::
system.time(
pe.responses.all.i <- data.table::rbindlist(
    lapply( countries , function(x){
    r <- get.responses(ctry=x, boot = FALSE)
    return(r)
    }))
)

## get bootstrapped emissions for all the countries (took about 19.5
## seconds to complete)::
library(pryr)
mem.before <- mem_used() ## 46.4 MB before
system.time(
responses.all.i <- data.table::rbindlist(
    lapply( countries , function(x){
    r <- get.responses(ctry=x, boot = TRUE)
    return(r)
    }))
)
mem.after <- mem_used()

## Memory used by responses.all.i (3.26 GB for 86*1e4 responses):
mem.after - mem.before
