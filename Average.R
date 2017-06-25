list.of.packages <- c("dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(dplyr, warn.conflicts = FALSE)


Average <- data.frame(WellName, Time, Oil, Gas, EffLat)
#pPhase <- quote(Oil)
pPhase <- ifelse(docProp == 1, quote(Oil), quote(Gas))


##remove downtime based upon primary phase
Average <- Average %>% filter(pPhase > 0 & EffLat > minLat)

if(nrow(Average) < 1)
{
  Average <- data.frame(WellName = c("None"), Time = c(Sys.time()), 
                        Oil = c(0), Gas = c(0), EffLat = c(0), Months = c(0), WellCount = c(0))
}else{
  
  ##################  
  Average <- Average %>%
    arrange(WellName,
            Time) %>%
    group_by(WellName) %>%
    mutate(RowCount = 1,
           Months = cumsum(RowCount), 
           Oil1 = Oil / EffLat * nEffLat, #normalized to effective lateral 
           Gas1 = Gas / EffLat * nEffLat, #normalized to effective lateral
           CUMOil = cumsum(Oil1),
           CUMGas = cumsum(Gas1)) %>% 
    group_by(Months) %>%
    summarise(Gas = mean(Gas1),
              Oil = mean(Oil1),
              CUMOil = mean(CUMOil),
              CUMGas = mean(CUMGas),
              WellCount = sum(RowCount))
  #######################
  
}

save(list=ls(), file='C:/Users/MFARR/Google Drive/r scripts/Production/Average.RData', RFormat=TRUE)
TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Google Drive/r scripts/Spotfire' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(),
                                                                         file=paste(tdir,'/average.in.RData',sep=''), RFormat=T )))

