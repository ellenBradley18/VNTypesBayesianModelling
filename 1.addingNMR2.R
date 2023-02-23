#vnDataClean
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
# output<-addingNMR %>% dplyr::select(OfficialName, NMRgroup) %>% distinct() %>% 
#   arrange(NMRgroup, OfficialName)
# 
# output2<-output %>% group_by(OfficialName) %>% summarise(n=n()) %>% 
#   filter(n>1)
# 
# #write.csv(output, "VN types/output/NMRgroups_all.csv")
# length(unique(output$OfficialName))
# #unique(addingNMR$ISO)
# #------------
# addingNMR %>% group_by(NMRgroup) %>% summarise(nC=length(unique(ISO)), nCY=n())
# addingNMR %>% group_by(level, NMRgroup) %>% summarise(nC=length(unique(ISO)), nCY=n())
# 
# 
# quantile(addingNMR$nmr, c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9), na.rm=TRUE)
# quantile(filter(addingNMR, level=="Study")$nmr, c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9), na.rm=TRUE)
# quantile(filter(addingNMR, level=="National")$nmr, c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9), na.rm=TRUE)
# 
# ggplot(addingNMR)+
#   geom_boxplot(aes(x=regionName, y=nmr, colour=level))+
#   theme(legend.position = "bottom")+  scale_x_discrete(labels = function(x) 
#     stringr::str_wrap(x, width = 15))
# 
# #-------------
# 
# addingNMR2<-addingNMR %>% mutate(LGA_PT_LBW=ifelse(is.na(LGA_PT_LBW), 0, LGA_PT_LBW), 
#                                  LGA_T_LBW=ifelse(is.na(LGA_T_LBW), 0, LGA_T_LBW)) %>% 
#   mutate(PT_LGA=LGA_PT_LBW+LGA_PT_NBW,
#          PT_AGA=AGA_PT_LBW+AGA_PT_NBW,
#          PT_SGA=SGA_PT_LBW+SGA_PT_NBW,
#          T_LGA=LGA_T_NBW+LGA_T_LBW,
#          T_AGA=AGA_T_LBW+AGA_T_NBW,
#          T_SGA=SGA_T_LBW+SGA_T_NBW,
#          Preterm=LGA_PT_LBW+LGA_PT_NBW+AGA_PT_LBW+AGA_PT_NBW+SGA_PT_LBW+SGA_PT_NBW,
#          Term=LGA_T_NBW+LGA_T_LBW+AGA_T_LBW+AGA_T_NBW+SGA_T_LBW+SGA_T_NBW) %>% 
#   mutate(allN=PT_LGA+PT_AGA+PT_SGA+T_LGA+T_AGA+T_SGA) %>% 
#   mutate(PT_LGA_P=PT_LGA/allN,
#          PT_AGA_P=PT_AGA/allN,
#          PT_SGA_P=PT_SGA/allN,
#          T_LGA_P=T_LGA/allN,
#          T_AGA_P=T_AGA/allN,
#          T_SGA_P=T_SGA/allN)
# 
# region<-addingNMR2 %>% group_by(regionName, NMRgroup) %>% 
#   summarise(nC=length(unique(ISO)),
#             nCY=n())
# 
# ggplot(addingNMR2)+
#   geom_boxplot(aes(x=regionName, y=nmr, colour=level))+
#   theme(legend.position = "bottom")+  
#   scale_x_discrete(labels = function(x) 
#     stringr::str_wrap(x, width = 15))+
#   scale_colour_discrete(name="",labels=c("National", 
#                                          "Study",
#                                          "No VN Data"))
# ggplot(addingNMR2)+
#   geom_boxplot(aes(x=regionName, y=nmr))+  
#   scale_x_discrete(labels = function(x) 
#     stringr::str_wrap(x, width = 15))

