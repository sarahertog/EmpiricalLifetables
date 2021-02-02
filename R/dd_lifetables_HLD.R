library(tidyverse)
library(DDSQLtools)
library(DemoTools)

options(unpd_server = "https://popdiv.dfs.un.org/DemoData/api/")

## GBD: graduate and extend HLD 2018 empirical life tables

load('tests/Locations.RData')
locs <- Locations$LocID

## Extract HLD 2018 abridged mx from DemoDAta

lt_all <- list()
for (i in 1:length(locs)) {
  tryCatch({
    
    lt_values <- get_recorddata(locIds = locs[i],
                                #dataProcessIds = 36, # Register
                                indicatorIds = c(245), #m(x,n) - abridged
                                startYear = 1700,
                                endYear = 2020,
                                dataSourceShortNames = "HLD 2018",
                                locAreaTypeIds = 2, # whole area (as opposed to urban/rural or some other sub-national unit)
                                subGroupIds = 2) # Total or all groups (as opposed to some population
    
  }, error=function(e){cat("Error in file", conditionMessage(e), "\n")})
  
  if (exists('lt_values')) {
    
    lt_values <- lt_values 
    
  } else { lt_values <- NULL }
  
  lt_all[[i]] <- lt_values
  rm(lt_values)
  
}

lt_gbd <- do.call(rbind, lt_all) %>% 
  filter(SexID %in% c(1,2)) %>% 
  mutate(id = paste(LocID, LocName, TimeLabel, TimeDuration, SexID, sep = " - "))


## Graduate to single year of age using lt_abridged2single of DemoTools, and extend to 110+ (kannisto)
ids <- unique(lt_gbd$id)

lts_sgl_ext <- list()

for (i in 1:length(ids)) {
  
  df <- lt_gbd %>% 
    filter(id == ids[i]) %>% 
    arrange(AgeSort)
  
  # get keys needed for data loader
  keys <- df[1,] %>% 
    select(id, LocID, LocName, LocTypeName, LocAreaTypeName, SubGroupTypeName, SubGroupName,
           DataCatalogName, DataProcess, DataSourceName, DataSourceAuthor, DataSourceYear, DataSourceShortName, 
           DataStatusName, StatisticalConceptName, SexID, SexName, ModelPatternName, DataReliabilityName, 
           PeriodTypeName, PeriodGroupName, TimeUnit, TimeStart, TimeEnd)
  
  Sex <- ifelse(df$SexID == 1, "m", "f")
  
  lt_sgl <- lt_abridged2single(Age = df$AgeStart,
                               nMx = df$DataValue,
                               Sex = Sex,
                               OAG = FALSE,
                               OAnew = 110,
                               extrapLaw = "kannisto",
                               extrapFrom = 85,
                               extrapFit = seq(60,85,5)) %>% 
    rename(AgeStart = Age,
           AgeSpan = AgeInt) %>% 
    mutate(id = ids[i],
           AgeEnd = AgeStart + AgeSpan,
           AgeUnit = "Year",
           DataTypeName = "Direct - graduated and extended") %>% 
    left_join(keys, by = "id")
  
  lts_sgl_ext[[i]] <- lt_sgl
  rm(lt_sgl)
}

lt_gbd1 <- do.call(rbind, lts_sgl_ext)
save(lt_gbd1, file = "tests/lt_gbd1.RData")

## Plot the graduated/extended life table against original abridged
# compare the graduated/extended life tables to the abridged

pdf(file="tests/lt.GBD.pdf", width=8, height=11, onefile=TRUE, pointsize=9)

par(mfrow=c(3,2),oma = c(0, 0, 2, 0))

for (i in 1:length(ids)){
  
  df <- lt_gbd %>% 
    filter(id == ids[i]) %>% 
    arrange(AgeStart)
  
  df_abr <- lt_abridged(Age = df$AgeStart,
                        nMx = df$DataValue,
                        OAG = FALSE) 

 plot(c(0,110), c(-10,0),main=ids[i], ylab="mx", xlab="", type="n")
  lines(lt_gbd1$AgeStart[lt_gbd1$id == ids[i]] + (lt_gbd1$AgeSpan[lt_gbd1$id == ids[i]]/2), 
        log(lt_gbd1$nMx[lt_gbd1$id == ids[i]]),
        col = "blue")
  points(df_abr$Age + (df_abr$AgeInt/2), 
         log(df_abr$nMx),
         col = "red",
         pch = 19)
  legend("bottomright",
         c(paste0("original abridged: e0 = ", round(df_abr$ex[df_abr$Age==0],2)), 
           paste0("graduated - extended: e0 = ", round(lt_gbd1$ex[lt_gbd1$id == ids[i] & lt_gbd1$AgeStart == 0], 2))),
         col = c("red", "blue"),
         lty = c(NA, 1),
         pch = c(19, NA))
 
}
dev.off()





