preterm<-read.csv("inputs/finalPretermEstimates.csv")%>% 
  rename(Year=year, pretermRate=est, 
         pretermLower=estL, 
         pretermUpper=estU) 

addingPreterm<-merge(x=allVnData, y=preterm,
                     by=c("Year", "ISO"), all.y=TRUE)

source("0.fileNames.R")
wpp_2021 <- readxl::read_xlsx(paste0("inputs/", wpp2))
wpp<-wpp_2021 %>% filter(LocTypeName=="Country/Area" & Year>1994 & Year<=2020) %>%
  mutate(OfficialName=ifelse(LocationName=="North Macedonia", "Republic of North Macedonia",
                             ifelse(LocationName=="Micronesia (Fed. States of)", "Micronesia (Federated States of)",
                                    ifelse(LocationName=="Dem. People's Republic of Korea", 	"Democratic People's Republic of Korea",
                                           ifelse(LocationName=="Côte d'Ivoire", "Cote d'Ivoire", LocationName)))),
         wpp_lb=Total*1000)
wpp2<-merge(x=wpp, y=regionCodes, by="OfficialName", all.y=TRUE) %>% dplyr::select(ISO, Year, wpp_lb) %>%
  filter(Year>=2000)


vnDataAll<-merge(x=addingPreterm, y=wpp2, by=c("Year", "ISO"), all.x=TRUE) %>%
  mutate(LGA_PT_LBW=ifelse(is.na(LGA_PT_LBW), 0, LGA_PT_LBW), 
         LGA_T_LBW=ifelse(is.na(LGA_T_LBW), 0, LGA_T_LBW)) %>% 
  mutate(SGA_PT_NBW=0, SGA_PT_NBW=0) %>% 
  
  mutate(PT_NSGA=LGA_PT_LBW+LGA_PT_NBW+AGA_PT_LBW+AGA_PT_NBW,
         PT_SGA=SGA_PT_LBW+SGA_PT_NBW,
         T_NSGA=LGA_T_NBW+LGA_T_LBW+AGA_T_LBW+AGA_T_NBW,
         T_SGA=SGA_T_LBW+SGA_T_NBW,
         Preterm=LGA_PT_LBW+LGA_PT_NBW+AGA_PT_LBW+AGA_PT_NBW+SGA_PT_LBW+SGA_PT_NBW,
         Term=LGA_T_NBW+LGA_T_LBW+AGA_T_LBW+AGA_T_NBW+SGA_T_LBW+SGA_T_NBW,
         allN=PT_NSGA+PT_SGA+T_SGA+T_NSGA) %>% 
  mutate(PT_SGA_P=PT_SGA/allN,
         PT_NSGA_P=PT_NSGA/allN,
         T_SGA_P=T_SGA/allN,
         T_NSGA_P=T_NSGA/allN,
         
         PT_SGA_P1=PT_SGA/Preterm,
         PT_NSGA_P1=PT_NSGA/Preterm,
         T_SGA_P1=T_SGA/Term,
         T_NSGA_P1=T_NSGA/Term)

