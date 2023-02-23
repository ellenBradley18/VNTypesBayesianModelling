library(dplyr)
library(readxl)
library(foreign)
library(haven)
library(ggplot2)
source("0.fileNames.R")

regionCodes<-readRDS("output/regionCodes.RDS") 
#-------------------
#NATIONAL DATA

vnData=read_excel(paste0(vnName)) %>% 
  dplyr::select(-c("ISO3_Code", "C:\\Users\\ellen\\OneDrive - London School of Hygiene and Tropical Medicine\\Data Exploration\\Data merging test\\ARG_Baseline_Characteristics_ARG_2018_2019.xlsx",
                   "UNICEF_Region", "LGH_Paper","UN_Region","UN_Sub_Region" , "SDG_Region","Development_Regions","LLDCs_SIDS",
                   "WHO_Region", "World_Bank_Income_Class","World_Bank_Regions", "Low_Income_Food_Deficient"
  ))

vnDataClean<-vnData  %>% 
  mutate(Year=ifelse(Year=="2016-21", 2018, Year), 
         iso2=ifelse(Countries_Territories=="Chile", "CHL",
                     ifelse(Countries_Territories=="DK", "DNK",
                            ifelse(Countries_Territories=="Iran", "IRN",
                                   ifelse(Countries_Territories=="MALASYA", "MYS",
                                          ifelse(Countries_Territories=="NL", "NLD",
                                                 ifelse(Countries_Territories=="Peru", "PER",
                                                        ifelse(Countries_Territories=="Qatar", "QAT",
                                                               ifelse(Countries_Territories=="Scotland", "GBR",
                                                                      ifelse(Countries_Territories=="UK", "GBR",
                                                                             ifelse(Countries_Territories=="Uru", "URY",
                                                                                    ifelse(Countries_Territories=="Australia", "AUS",
                                                                                           ifelse(Countries_Territories=="Estonia", "EST",
                                                                                                  ifelse(Countries_Territories=="CzechRepublic", "CZE",
                                                                                                         ifelse(Countries_Territories=="Lebanon", "LBN",
                                                                                                                ifelse(Countries_Territories=="Nireland", "GBR",
                                                                                                                       ifelse(Countries_Territories=="Korea", "KOR", Countries_Territories
                                                                                                                       )))))))))))))))))%>%
  filter(Year>=2000 & Year<=2020) 


without<-vnDataClean%>% filter(iso2!="GBR")
gbr<-vnDataClean %>% filter(iso2=="GBR")
#gbr<-vnDataClean %>% filter(Countries_Territories %in% c("SCOT", "UK", "N_Ireland"))

gbr2<-gbr %>% dplyr::select(-c("Countries_Territories"
)) %>%
  group_by(Year) %>%
  mutate_all(funs(as.numeric)) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(iso2="GBR",
         Countries_Territories="UK")

ggplot(gbr2)+geom_point(aes(x=Year, y=Total_LB))


vnDataClean<-rbind(without, gbr2)


vnDataClean2<-vnDataClean %>% mutate(OfficialName=ifelse(nchar(iso2)!=3, iso2, NA))
vnDataClean_larger<-merge(x=vnDataClean2, y=regionCodes, by="OfficialName",all.x=TRUE) %>% 
  mutate(iso=ifelse(nchar(iso2)==3, iso2, ISO)) %>% 
  mutate(isoYear=paste0(iso, Year)) %>% 
  dplyr::select(-ISO) %>% rename(ISO=iso)

vnDataClean_larger2<-merge(x=vnDataClean_larger, y=regionCodes, by=c("OfficialName", "ISO"), all.x=TRUE)


medians1<-vnDataClean_larger %>% group_by(ISO) %>% summarise(MedianHeaping_index=median(Heaping_index))

vnDataClean<-merge(x=vnDataClean_larger, y=medians1, by="ISO", all.x=TRUE) %>% 
  dplyr::select(-c(OfficialName, regionName, regionIndex))

vnDataClean<-merge(x=vnDataClean, y=regionCodes, by="ISO", all.x=TRUE) %>% 
  mutate(level="National")

#----------
#STUDY DATA

vnDataStudy<-read_dta("inputs/allPhenotypes_WIDE_final_ 6 Sep 2022 (1).dta") %>% 
  rename(ISO=code) %>% mutate(startYear=as.numeric(substr(daterange, 1, 4)),
                              endYear=as.numeric(ifelse(substr(daterange, 5,5)=="-",
                                                        substr(daterange, 6, 10), NA)))%>% 
  mutate(midYear=ifelse(is.na(endYear), startYear,
                        ceiling(startYear+(endYear-startYear)/2))) %>% 
  mutate(Year=midYear) %>% 
  dplyr::select(-c(endYear, startYear, midYear)) %>% 
  rename(AGA_PT_LBW=AGA_LBW_P, AGA_T_LBW=AGA_LBW_T, AGA_PT_NBW=AGA_NBW_P, AGA_T_NBW=AGA_NBW_T,
         LGA_PT_LBW=LGA_LBW_P, LGA_PT_NBW=LGA_NBW_P, LGA_T_NBW=LGA_NBW_T,
         SGA_PT_LBW=SGA_LBW_P, SGA_T_LBW=SGA_LBW_T, SGA_T_NBW=SGA_NBW_T) %>% 
  mutate(SGA_PT_NBW=0, SGA_PT_NBW=0)

vnStudy2<-merge(x=vnDataStudy, y=regionCodes, by="ISO", all.x=TRUE) %>% 
  mutate(isoYear=paste0(ISO, Year))%>% 
  mutate(level="Study")

vnStudy3<-vnStudy2 %>% filter(!(OfficialName %in% unique(vnDataClean$OfficialName)))

#------------
missingStudy<-setdiff(names(vnDataClean), names(vnStudy3))
addOn<-(vnDataClean %>% dplyr::select(missingStudy))[1,] %>% mutate_all(function(x) (x=NA))
vnStudy4<-cbind(vnStudy3, addOn)

missingAdmin<-setdiff(names(vnStudy2),names(vnDataClean))
addOn2<-(vnStudy2 %>% dplyr::select(missingAdmin))[1,] %>% mutate_all(function(x) (x=NA))
vnDataClean2<-cbind(vnDataClean, addOn2)

#-------------

allVnData<-rbind(vnDataClean2, vnStudy4) %>% 
  mutate(Year=as.numeric(Year))

length(unique(vnDataClean2$ISO))
length(unique(vnStudy3$ISO))
length(unique(allVnData$ISO))

