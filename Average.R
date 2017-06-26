rm(list = ls())
#####not to load in spotfire
og_select <- 1
prod_tbl <- read.csv("Updated_IHS.csv")
min_lateral <- 5000
####
WellId <- prod_tbl$Entity
EffLat <- prod_tbl$PerfIntervalGross
Time <- prod_tbl$c.ProductionDate
Oil <- prod_tbl$Liquid
Gas <- prod_tbl$Gas


os <- substitute(Oil)


##Michael Farr
#This script will average oil & gas rates based on the primary phase that is selected by the user

#----------------------------------------------------------------------------------

##install package if it is not already installed
list.of.packages <- c("dplyr", "tibble")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos =  "https://mran.revolutionanalytics.com/snapshot/2017-05-01/")

##load package
library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)



input <- data.frame(WellId, Time, Oil, Gas, EffLat) #create Average table 


pPhase <- ifelse(og_select == 1, quote(Oil), quote(Gas)) 
pPhase <- ifelse(og_select == 1, substitute(Oil), substitute(Gas)) 

input1 <- prod_tbl %>%
  select(WellId = Entity,
         Time = c.ProductionDate,
         Oil = Liquid, 
         Gas = Gas, 
         EffLat = PerfIntervalGross)

input1 <- data.frame(input1)
  
input2 <- filter(input, pPhase > 0 & EffLat < 5000)
filter(input1, pPhase > 0)

write.csv(input, "input.csv")
write.csv(input1, "input1.csv")

#Average <- data.frame(WellName, Time, Oil, Gas, EffLat) #create Average table 

##"docProd" document property control allows the user to select OIL (1) or GAS (2) as the primary phase 
pPhase <- ifelse(docProp == 1, quote(Oil), quote(Gas)) 

##remove downtime based upon primary phase and minimum laterla length 
#Average <- Average %>% filter(pPhase > 0 & EffLat > minLat)
input <- input %>% filter(pPhase > 0 & EffLat > min_lateral)



if(nrow(Average) < 1)
{
  Average <- data.frame(WellName = c("None"), Time = c(Sys.time()), 
                        Oil = c(0), Gas = c(0), EffLat = c(0), Months = c(0), WellCount = c(0))
}else{
  
  ##################dplyr package used for data wrangling
  Average <- Average %>%
    arrange(WellName,
            Time) %>%
    group_by(WellName) %>%
    mutate(RowCount = 1,
           Months = cumsum(RowCount), 
           #Oil1 = Oil / EffLat * nEffLat, #normalized to effective lateral 
           #Gas1 = Gas / EffLat * nEffLat, #normalized to effective lateral
           CUMOil = cumsum(Gas / EffLat * nEffLat),
           #CUMOil = cumsum(Oil1),
           CUMGas = cumsum(Gas1)) %>% 
    group_by(Months) %>%
    summarise(Gas = mean(Gas1), #mcf
              Oil = mean(Oil1), #bbl
              CUMGas = mean(CUMGas), #mcf
              CUMOil = mean(CUMOil), #bbl
              WellCount = sum(RowCount))
  #######################
  
}

save(list=ls(), file='C:/Users/MFARR/Documents/R_files/Spotfire.data/Average.RData', RFormat=TRUE)
TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/average.in.RData',sep=''), RFormat=T )))
