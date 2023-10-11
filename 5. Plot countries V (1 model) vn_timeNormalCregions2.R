library(dplyr)
library(ggplot2)
library(tidyr)
library(ggbreak)
library(boot)
library(gridExtra)

vnFinal<-readRDS("output/vnDataFinal.RDS") 

allTogether2<-estimates %>% mutate(isoYear=paste0(ISO, year))

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

  return(plot3)
}

pdf_name <-  paste0("output/",
                    fileName,((niter-nburnin)/nthin)*nchains, "_countryPlots2.pdf")
pdf(pdf_name, width = 10, height = 5)
c(1:195) %>% lapply(plotCountriesNewV2)
dev.off()
