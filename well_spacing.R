
rm(list = ls())
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/tcgroup.RData")

library(dplyr)
library(purrr)
library(data.table)

###############checks on well count when you filter it out seperately
MULTI <- wellheader %>%
  filter(MULTI == "Contains Multi") %>%
  select(API) %>%
  n_distinct()


NOPROD <- wellheader %>%
  filter(is.na(DateProductionStart)) %>%
  select(API) %>%
  n_distinct()

API.DUP <- wellheader %>%
  select(API) %>%
  n_distinct()

API.DUP2 <- nrow(wellheader) - API.DUP
  
Total <- nrow(wellheader) - MULTI - NOPROD - API.DUP2


########check to see if you get the same well count...and you do :)
wh2 <- wellheader %>%
  filter(MULTI != "Contains Multi")

wh3 <- wh2 %>%
  filter(!is.na(DateProductionStart)) 

wh4 <- wh3 %>%
  select(API) %>%
  distinct()
#####################WHJOIN BEGIN--------------------


wh <- wellheader %>%
  filter(MULTI != "Contains Multi" & 
           !is.na(DateProductionStart)) %>%
  select(API, ProdDate_Well_0 = DateProductionStart) %>%
  group_by(API) %>%
  arrange(ProdDate_Well_0) %>%
  slice(1L)

##join wh and spacing table
whjoin <- left_join(wh, spacing, by = "API") 

##use match to lookup prod date for side wells and create new column
whjoin$SIDE_1_ProdDate <- whjoin$ProdDate_Well_0[match(whjoin$`SIDE_1 _API`, wh$API)]
whjoin$SIDE_2_ProdDate <- whjoin$ProdDate_Well_0[match(whjoin$`SIDE 2_API`, wh$API)]

##create calc column and rename columns
whjoin <- whjoin %>%
  mutate(Spacing_Avg = (NEW_SIDE_1_DISTANCE + NEW_SIDE_2_DISTANCE)/2, 
         Spacing_Category = ifelse(!is.na(Spacing_Avg), "Bounded",ifelse(!is.na(NEW_SIDE_1_DISTANCE) | 
                                                                           !is.na(NEW_SIDE_2_DISTANCE), "Partially Bounded", "Unbounded")),
         Infill_Time_Side1 = as.numeric(abs(ProdDate_Well_0 - SIDE_1_ProdDate)),
         Infill_Time_Side2 = as.numeric(abs(ProdDate_Well_0 - SIDE_2_ProdDate))) %>%
  select(API, ProdDate_Well_0 ,one_of('SIDE_1 _API'), SIDE_1_ProdDate, NEW_SIDE_1_DISTANCE,
         one_of('SIDE 2_API'), SIDE_2_ProdDate, NEW_SIDE_2_DISTANCE, Spacing_Avg, Spacing_Category, Infill_Time_Side1, 
         Infill_Time_Side2)
whjoin




TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/whjoin.RData',sep=''), RFormat=T )))
##########END------------------




wellheader %>%   mutate(ProdDate_Well_0 = as.POSIXct(as.Date(DateProductionStart, "%Y/%m/%d"), origin = "1970-01-01", tz = "UTC")) %>%
    select(DateProductionStart, ProdDate_Well_0)


wh <- wellheader %>%
  filter(MULTI != "Contains Multi" & 
           !is.na(DateProductionStart)) %>%
  mutate(ProdDate_Well_0 = as.POSIXct(as.Date(DateProductionStart, "%Y/%m/%d"), origin = "1970-01-01", tz = "UTC")) %>%
  select(API, ProdDate_Well_0) %>%
  group_by(API) %>%
  arrange(ProdDate_Well_0) %>%
  slice(1L)




write.csv(wh, "wh.csv")
write.csv(wh1, "wh1.csv")
write.csv(whjoin, "whjoin.csv")
write.csv(wellheader, "wellheader.csv")
write.csv(api.dup, "duplicate.csv")

##function to look for duplicates...it uses data.table
count.dups <- function(DF){
  DT <- data.table(DF)
  DT[,.N, by = names(DT)]
}

api.dup <- count.dups(wh1)

api.dup %>%
  group_by(N) %>%
  summarise(dups = n())

api.dup %>% filter(N >= 3)
head(api.dup)
