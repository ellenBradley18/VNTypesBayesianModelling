library(dplyr)
library(ggplot2)
library(tidyr)
library(ggbreak)
library(boot)
library(gridExtra)

#mod<-readRDS("VN types/output/models/vn_timeNormalC_21.2e+08.rds")
#mod<-readRDS("VN types/output/models/newVNModelE2_4types2_2020_v35e+06.rds")
#mod<-vn_timeNormalC
vnFinal<-readRDS("output/vnDataFinal.RDS") 
# 
# countries<-vnFinal %>% filter(!is.na(level) & Year>=2010) %>%  dplyr::select(ISO) %>% distinct() %>% 
#   arrange(ISO) %>% mutate(country1=row_number())
# preterms<-merge(x=vnFinal, y=countries, by="ISO", all.x=TRUE) %>% 
#   filter(!is.na(country1) & Year==2020) %>% dplyr::select(ISO, pretermRate) 
# 
# dim<-dim(mod$BUGSoutput$median$p[,,])
# #mod$BUGSoutput$sims.list$p<-mod$BUGSoutput$sims.list$q
# 
# for (i in 1:dim[1]){
#   for (j in 1:dim[2]){
#     output1<-as.data.frame(list(country=i, year=j+2009, 
#                                 PT_SGA_Est_lower=quantile(mod$BUGSoutput$sims.list$q[,i,j,1], 0.025),
#                                 PT_SGA_Est=mod$BUGSoutput$median$q[i,j,1],
#                                 PT_SGA_Est_upper=quantile(mod$BUGSoutput$sims.list$q[,i,j,1], 0.975),
#                                 
#                                 PT_NSGA_Est_lower=quantile(mod$BUGSoutput$sims.list$q[,i,j,2], 0.025),
#                                 PT_NSGA_Est=mod$BUGSoutput$median$q[i,j,2], 
#                                 PT_NSGA_Est_upper=quantile(mod$BUGSoutput$sims.list$q[,i,j,2], 0.975),
#                                 
#                                 T_SGA_Est_lower=quantile(mod$BUGSoutput$sims.list$q[,i,j,3], 0.025),
#                                 T_SGA_Est=mod$BUGSoutput$median$q[i,j,3],
#                                 T_SGA_Est_upper=quantile(mod$BUGSoutput$sims.list$q[,i,j,3], 0.975),
#                                 
#                                 T_NSGA_Est_lower=quantile(mod$BUGSoutput$sims.list$q[,i,j,4], 0.025),
#                                 T_NSGA_Est=mod$BUGSoutput$median$q[i,j,4], 
#                                 T_NSGA_Est_upper=quantile(mod$BUGSoutput$sims.list$q[,i,j,4], 0.975)))
#     if (i==1 & j==1){
#       output<-output1
#     } else{
#       output<-rbind(output, output1)
#     }
#   }
# }
# 
# vnModel1<-merge(x=output, y=countries %>% rename(country=country1), by="country", all.x=TRUE) %>%
#   rename(Year=year) %>% dplyr::select(-country) %>% mutate(predicted=0)
# 

#-----------------------------------
#Adding predictions
# source("4.predictionV2E2_4types2regions2.R")
# regions<-readRDS("output/regionCodes.RDS") %>% 
#   mutate(countryIndex=row_number())
# 
# predict2<-predict%>% dplyr::select(-sum) %>% rename(Year=year) %>% mutate(predicted=1)
# 
# allTogether<-rbind(vnModel1, predict2)
# 
# allTogether2<-merge(x=allTogether, y=regions, by="ISO", all.x=TRUE) %>% 
#   mutate(isoYear=paste0(ISO, Year))
# 
# 
# #write.csv(allTogether2, "VN types/output/vn_timeNormalC_2regions2 predictions.csv")
# write.csv(allTogether2, 
#           paste0("output/",
#           fileName,((niter-nburnin)/nthin)*nchains, " Estimates.csv"))

allTogether2<-estimates %>% mutate(isoYear=paste0(ISO, year))

#--------
# allTogether2<-read.csv("VN types/output/vn_timeNormalC_2regions2 predictions.csv")
# second<-read.csv("VN types/output/vn_timeNormalC_2test predictionsF.csv") %>% 
#   dplyr::select(-c(predicted, regionName, regionIndex, OfficialName, countryIndex, isoYear))
# 
# names(second)[4:15]<-paste0(names(second)[4:15], "_Updated")
# 
# all<-merge(x=allTogether2, y=second, by=c("ISO", "Year"), all.x=TRUE)

#------------------------------------
# Adding the inputs in 

# vnFinalMeta<-readRDS("VN types/output/vnDataFinal_meta2.rds")
# vnDataFinal<-vnFinalMeta %>% filter(Year>=2010 & !is.na(level)) %>% 
#   mutate(sourceIndex=ifelse(level=="National", 1, 
#                             ifelse(level=="Study", 2, NA))) %>%
#   dplyr::select(-c(PT_SGA_P, PT_NSGA_P1, T_SGA_P, T_NSGA_P)) %>% 
#   mutate(PT_SGA2=ifelse(level=="Study", PT_SGA_meta ,PT_SGA),
#          T_SGA2=ifelse(level=="Study", T_SGA_meta ,T_SGA),
#          PT_NSGA2=ifelse(level=="Study", PT_NSGA_meta ,PT_NSGA),
#          T_NSGA2=ifelse(level=="Study", T_NSGA_meta ,T_NSGA),
#          Preterm2=ifelse(level=="Study", Preterm_meta ,Preterm),
#          Term2=ifelse(level=="Study", Term_meta ,Term),
#          allN=ifelse(level=="Study", wpp_lb ,allN)) %>% 
#   mutate(PT_SGA_P=PT_SGA2/allN, 
#          PT_NSGA_P=PT_NSGA2/allN,
#          T_SGA_P=T_SGA2/allN, 
#          T_NSGA_P=T_NSGA2/allN,
#          isoYear=paste0(ISO, Year))%>% 
#   dplyr::select(ISO, Year, OfficialName=OfficialName.x, isoYear, regionName, level, 
#                 "PT_NSGA_P", "PT_SGA_P" ,   "T_NSGA_P"  , "T_SGA_P")

vnFinalMeta<-readRDS("output/vnDataFinal_meta2.rds")
d<-vnFinalMeta %>% filter(Year>=2010 & !is.na(level)) %>%
  mutate(sourceIndex=ifelse(level=="National", 1,
                            ifelse(level=="Study", 2, NA)))%>%
  mutate(PT_SGA_Logit=ifelse(level=="Study",qlogis(PT_SGA_P1_meta), qlogis(PT_SGA_P1)),
         T_SGA_Logit=ifelse(level=="Study",qlogis(T_SGA_P1_meta), qlogis(T_SGA_P1)),
         PT_SGA_LogitSe=ifelse(level=="Study",
                               (qlogis(PT_SGA_P1_meta)-qlogis(PT_SGA_P1L_meta))/1.96,
                               sqrt(1/(Preterm*PT_SGA_P1*(1-PT_SGA_P1)))),
         T_SGA_LogitSe=ifelse(level=="Study",
                              (qlogis(T_SGA_P1_meta)-qlogis(T_SGA_P1L_meta))/1.96,
                              sqrt(1/(Term*T_SGA_P1*(1-T_SGA_P1))))) %>%
  mutate(PT_NSGA_P=ifelse(level=="Study",
                          (1-inv.logit(PT_SGA_Logit))*(pretermRate/100), PT_NSGA_P),
         PT_SGA_P=ifelse(level=="Study",
                         inv.logit(PT_SGA_Logit)*(pretermRate/100), PT_SGA_P),
         T_NSGA_P=ifelse(level=="Study",
                         (1-inv.logit(T_SGA_Logit))*(1-pretermRate/100), T_NSGA_P),
         T_SGA_P=ifelse(level=="Study",
                        inv.logit(T_SGA_Logit)*(1-pretermRate/100), T_SGA_P))%>% 
  dplyr::select(ISO, Year, OfficialName=OfficialName.x, isoYear, regionName, level, 
                "PT_NSGA_P", "PT_SGA_P" ,   "T_NSGA_P"  , "T_SGA_P")



studies<-readRDS("output/vnDataFinal.rds") %>% 
  filter(level=="Study") %>% 
  mutate(level="StudyRaw",
         PT_NSGA_P=PT_NSGA/allN, 
         T_NSGA_P=T_NSGA/allN) %>% 
  mutate(isoYear=paste0(ISO, Year)) %>% 
  dplyr::select(ISO, Year, isoYear,OfficialName, regionName, level,
                "PT_NSGA_P", "PT_SGA_P" ,   "T_NSGA_P"  , "T_SGA_P")
vnDataFinal2<-rbind(d, studies)

allData<-merge(x=vnDataFinal2, y=allTogether2 %>% rename(Year=year) , 
               by=c("ISO", "Year"), all.x=TRUE) %>% 
   mutate(#OfficialName=ifelse(is.na(OfficialName.x), OfficialName.y, OfficialName.x),
  #        regionName=ifelse(is.na(regionName.x), regionName.y, regionName.x), 
          isoYear=ifelse(is.na(isoYear.x), isoYear.y, isoYear.x)) %>% 
  dplyr::select(-c(isoYear.x, isoYear.y)) %>% rename(year=Year)

#allData2<-merge(x=allData, y=regions %>% dplyr::select(ISO, countryIndex), by=c("ISO"), all.x=TRUE)

regions<-readRDS("output/regionCodes.RDS") %>%
  mutate(countryIndex=row_number())

rest<-allTogether2 %>% filter(!(isoYear %in% allData$isoYear))
missing<-setdiff(names(allData), names(rest))
addOn<-(allData %>% dplyr::select(missing))[1,] %>% mutate_all(function(x) (x=NA))
rest2<-cbind(rest, addOn)

allData3<-merge(x=rbind(allData, rest2) %>% 
                  dplyr::select(-OfficialName), y=regions %>% dplyr::select(ISO, countryIndex, OfficialName), by=c("ISO"), all.x=TRUE) %>% 
  rename(Year=year)


#------------------------------------
# Line plots

plotCountriesNewV<-function(i){
  j= estyears%%2==0
  
  data2<-allData3 %>% filter(countryIndex==i)
  
  titleName<-data2$OfficialName[1]
  
  col<-c("c1"="red", "c2"="blue", 
         "c3"="green", "c4"="purple", "c5"="darkblue")
  shape=c("National"=16, "Study"=17, "StudyRaw"=2)
  
  plot1<-data2 %>% 
    ggplot() + theme_bw() +
    {if(nrow(data2 %>% filter(level=="National"))!=0)geom_point(data=data2 %>% filter(level=="National"),
                                                                aes(x=Year, y=PT_SGA_P*100, colour="c3", shape="National"))}+
    {if(nrow(data2 %>% filter(level=="National"))!=0)geom_point(data=data2 %>% filter(level=="National"),
                                                                aes(x=Year, y=PT_NSGA_P*100, colour="c4", shape="National"))}+
    {if(nrow(data2 %>% filter(level=="National"))!=0)geom_point(data=data2 %>% filter(level=="National"),
                                                                aes(x=Year, y=T_SGA_P*100, colour="c6", shape="National"))}+
    {if(nrow(data2 %>% filter(level=="National"))!=0)geom_point(data=data2 %>% filter(level=="National"),
                                                                aes(x=Year, y=T_NSGA_P*100, colour="c7", shape="National"))}+
    
    {if(nrow(data2 %>% filter(level=="Study"))!=0)geom_point(data=data2 %>% filter(level=="Study"),
                                                             aes(x=Year, y=PT_SGA_P*100, colour="c3", shape="Study"))}+
    {if(nrow(data2 %>% filter(level=="Study"))!=0)geom_point(data=data2 %>% filter(level=="Study"),
                                                             aes(x=Year, y=PT_NSGA_P*100, colour="c4", shape="Study"))}+
    {if(nrow(data2 %>% filter(level=="Study"))!=0)geom_point(data=data2 %>% filter(level=="Study"),
                                                             aes(x=Year, y=T_SGA_P*100, colour="c6", shape="Study"))}+
    {if(nrow(data2 %>% filter(level=="Study"))!=0)geom_point(data=data2 %>% filter(level=="Study"),
                                                             aes(x=Year, y=T_NSGA_P*100, colour="c7", shape="Study"))}+
    
    {if(nrow(data2 %>% filter(level=="StudyRaw"))!=0)geom_point(data=data2 %>% filter(level=="StudyRaw"),
                                                                aes(x=Year, y=PT_SGA_P*100, colour="c3", shape="StudyRaw"))}+
    {if(nrow(data2 %>% filter(level=="StudyRaw"))!=0)geom_point(data=data2 %>% filter(level=="StudyRaw"),
                                                                aes(x=Year, y=PT_NSGA_P*100, colour="c4", shape="StudyRaw"))}+
    {if(nrow(data2 %>% filter(level=="StudyRaw"))!=0)geom_point(data=data2 %>% filter(level=="StudyRaw"),
                                                                aes(x=Year, y=T_SGA_P*100, colour="c6", shape="StudyRaw"))}+
    {if(nrow(data2 %>% filter(level=="StudyRaw"))!=0)geom_point(data=data2 %>% filter(level=="StudyRaw"),
                                                                aes(x=Year, y=T_NSGA_P*100, colour="c7", shape="StudyRaw"))}+
    geom_line(aes(x=Year, y=PT_SGA_Est*100, colour="c3"))+
    geom_line(aes(x=Year, y=PT_SGA_Est_lower*100, colour="c3"), linetype="dashed")+
    geom_line(aes(x=Year, y=PT_SGA_Est_upper*100, colour="c3"), linetype="dashed")+
    geom_line(aes(x=Year, y=PT_NSGA_Est*100, colour="c4"))+
    geom_line(aes(x=Year, y=PT_NSGA_Est_lower*100, colour="c4"), linetype="dashed")+
    geom_line(aes(x=Year, y=PT_NSGA_Est_upper*100, colour="c4"), linetype="dashed")+
    geom_line(aes(x=Year, y=T_SGA_Est*100, colour="c6"))+
    geom_line(aes(x=Year, y=T_SGA_Est_lower*100, colour="c6"), linetype="dashed")+
    geom_line(aes(x=Year, y=T_SGA_Est_upper*100, colour="c6"), linetype="dashed")+
    geom_line(aes(x=Year, y=T_NSGA_Est*100, colour="c7"))+
    geom_line(aes(x=Year, y=T_NSGA_Est_lower*100, colour="c7"), linetype="dashed")+
    geom_line(aes(x=Year, y=T_NSGA_Est_upper*100, colour="c7"), linetype="dashed")+
    labs(title=titleName, y="Percentage of livebirths") +
    scale_colour_discrete(name="",
                          breaks=c(#"c1","c2", 
                            "c3", "c4", 
                            "c6", "c7"),
                          labels=c("PT_SGA", "PT_NSGA",
                                   "T_SGA", "T_NSGA"
                          ))+
    scale_shape_manual(name="",values=shape)+
    scale_x_continuous(name = 'Year', breaks = estyears[j], 
                       minor_breaks = NULL, limits = c(2009.5,2020.5))+
    theme(legend.position="bottom")
  plot1
  
  scale<-c(0,100)
  plot1<-plot1+coord_cartesian(ylim = scale, expand=FALSE)
  # plot2<-plot1+
  #   scale_y_break(c(round((order$count[which$row]*100-5)/5)*5,
  #                   round((order$count[which$row-1]*100-5)/5)*5), scales=0.5)
  
  #plot2
  return(plot1)
}

pdf_name<-paste0("output/", fileName,((niter-nburnin)/nthin)*nchains, "_countryPlots.pdf")
pdf(pdf_name, width = 10, height = 5)
c(1:195) %>% lapply(plotCountriesNewV)
dev.off()

#--------------------------------------------------------
#Splitting the axis


plotCountriesNewV2<-function(i){
  j= estyears%%2==0
  
  data2<-allData3 %>% filter(countryIndex==i)
  
  titleName<-data2$OfficialName[1]
  
  col<-c("c1"="red", "c2"="blue", 
         "c3"="green", "c4"="purple", "c5"="darkblue")
  shape=c("National"=16, "Study"=17)
  
  plot1<-data2 %>% 
    ggplot() + theme_bw() +
    {if(nrow(data2 %>% filter(level=="National"))!=0)geom_point(data=data2 %>% filter(level=="National"),
                                                                aes(x=Year, y=PT_SGA_P*100, colour="c3", shape="National"))}+
    {if(nrow(data2 %>% filter(level=="National"))!=0)geom_point(data=data2 %>% filter(level=="National"),
                                                                aes(x=Year, y=PT_NSGA_P*100, colour="c4", shape="National"))}+
    {if(nrow(data2 %>% filter(level=="National"))!=0)geom_point(data=data2 %>% filter(level=="National"),
                                                                aes(x=Year, y=T_SGA_P*100, colour="c6", shape="National"))}+
    
    {if(nrow(data2 %>% filter(level=="Study"))!=0)geom_point(data=data2 %>% filter(level=="Study"),
                                                             aes(x=Year, y=PT_SGA_P*100, colour="c3", shape="Study"))}+
    {if(nrow(data2 %>% filter(level=="Study"))!=0)geom_point(data=data2 %>% filter(level=="Study"),
                                                             aes(x=Year, y=PT_NSGA_P*100, colour="c4", shape="Study"))}+
    {if(nrow(data2 %>% filter(level=="Study"))!=0)geom_point(data=data2 %>% filter(level=="Study"),
                                                             aes(x=Year, y=T_SGA_P*100, colour="c6", shape="Study"))}+
    geom_line(aes(x=Year, y=PT_SGA_Est*100, colour="c3"))+
    geom_line(aes(x=Year, y=PT_SGA_Est_lower*100, colour="c3"), linetype="dashed")+
    geom_line(aes(x=Year, y=PT_SGA_Est_upper*100, colour="c3"), linetype="dashed")+
    geom_line(aes(x=Year, y=PT_NSGA_Est*100, colour="c4"))+
    geom_line(aes(x=Year, y=PT_NSGA_Est_lower*100, colour="c4"), linetype="dashed")+
    geom_line(aes(x=Year, y=PT_NSGA_Est_upper*100, colour="c4"), linetype="dashed")+
    geom_line(aes(x=Year, y=T_SGA_Est*100, colour="c6"))+
    geom_line(aes(x=Year, y=T_SGA_Est_lower*100, colour="c6"), linetype="dashed")+
    geom_line(aes(x=Year, y=T_SGA_Est_upper*100, colour="c6"), linetype="dashed")+
    labs(y="Percentage of livebirths") +
    scale_colour_manual(name="",                          values=c("#f8766d", "#7cae00", "#00bfc4","#c37cfb"),
                        breaks=c(#"c1","c2", 
                          "c3", "c4", 
                          "c6", "c7"),
                        labels=c("PT_SGA", "PT_NSGA",
                                 "T_SGA", "T_NSGA"
                        ))+
    scale_shape_manual(name="",values=shape)+
    scale_x_continuous(name = 'Year', breaks = estyears[j], 
                       minor_breaks = NULL, limits = c(2009.5,2020.5))+
    theme(legend.position="bottom")
  
  plot2<-data2 %>% 
    ggplot() + theme_bw() +
    {if(nrow(data2 %>% filter(level=="National"))!=0)geom_point(data=data2 %>% filter(level=="National"),
                                                                aes(x=Year, y=T_NSGA_P*100, colour="c7", shape="National"))}+
    {if(nrow(data2 %>% filter(level=="Study"))!=0)geom_point(data=data2 %>% filter(level=="Study"),
                                                             aes(x=Year, y=T_NSGA_P*100, colour="c7", shape="Study"))}+
    geom_line(aes(x=Year, y=T_NSGA_Est*100, colour="c7"))+
    geom_line(aes(x=Year, y=T_NSGA_Est_lower*100, colour="c7"), linetype="dashed")+
    geom_line(aes(x=Year, y=T_NSGA_Est_upper*100, colour="c7"), linetype="dashed")+
    labs(title=titleName, y="") +
    scale_colour_manual(name="",
                        values=c("#f8766d", "#7cae00", "#00bfc4","#c37cfb"),
                        breaks=c(#"c1","c2", 
                          "c3", "c4", 
                          "c6", "c7"),
                        labels=c("PT_SGA", "PT_NSGA",
                                 "T_SGA", "T_NSGA"
                        ))+
    scale_shape_manual(name="",values=shape)+
    scale_x_continuous(name = 'Year', breaks = estyears[j], 
                       minor_breaks = NULL, limits = c(2009.5,2020.5))+
    theme(legend.position="bottom")
  
  #plot1
  #plot2
  
  plot3<-grid.arrange(plot2, plot1, ncol=1)
  
  scale<-c(0,100)
  # plot1<-plot1+coord_cartesian(ylim = scale, expand=FALSE)
  
  return(plot3)
}

pdf_name <-  paste0("output/",
                    fileName,((niter-nburnin)/nthin)*nchains, "_countryPlots2.pdf")
pdf(pdf_name, width = 10, height = 5)
c(1:195) %>% lapply(plotCountriesNewV2)
dev.off()
