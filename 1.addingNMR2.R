library(foreign)
library(dplyr)

nmr<-read.dta("inputs/allCovariatesTidy.dta") %>% 
  dplyr::select(ISO=iso, Year=year, nmr) %>% 
  filter(Year>=2010)
regionCodes<-readRDS("output/regionCodes.rds")
nmrs<-merge(x=nmr, y=regionCodes, by="ISO", all.y=TRUE) 

vnDataFinal<-merge(x=vnDataAll %>% 
                   dplyr::select(-c(regionName, regionIndex, 
                                    OfficialName)), 
                 y=nmrs, by=c("ISO", "Year"), all.y = TRUE) %>% 
  mutate(NMRgroup=ifelse(nmr<5, 1, 
                         ifelse(nmr>=5 & nmr<15, 2, 
                                ifelse(nmr>=15 & nmr<30, 3, 
                                       ifelse(nmr>=30, 4, NA)))))


saveRDS(vnDataFinal, "output/vnDataFinal.rds")
