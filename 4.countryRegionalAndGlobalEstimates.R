#PREDICT USING WHOLE ITERATIONS - AFTER ALEX CONSIDERATION
#Samples from the regional intercepts
#Takes the regional time from Z*delta rather than delta: NEED TO DO
#mod<-readRDS("VN types/output/models/vn_timeNormalC_2regions25e+06.rds")
source("0.loadPackages.R")

#------------------------------
vnDataFinal<-readRDS("output/vnDataFinal.RDS")

alpha.level=0.05
undo<-function(data){
  data<-as.data.frame(data)
  long<-gather(data, "chain", "name") %>% dplyr::select(-chain)
  return(long)
}

#-----
#Coeffs

for (m in 1:2){
  beta1.c<-cbind(undo(mod[["BUGSoutput"]][["sims.list"]][["mu.beta"]][,1,m]) %>% rename(mubeta1=name),
                 undo(mod[["BUGSoutput"]][["sims.list"]][["tau.beta"]][,1,m]) %>% rename(taubeta1=name))%>% 
    rowwise() %>% mutate(beta1=rnorm(1,mubeta1, 1/sqrt(taubeta1))) %>% dplyr::select(beta1)
  beta2.c<-cbind(undo(mod[["BUGSoutput"]][["sims.list"]][["mu.beta"]][,2,m]) %>% rename(mubeta1=name),
                 undo(mod[["BUGSoutput"]][["sims.list"]][["tau.beta"]][,2,m]) %>% rename(taubeta1=name))%>% 
    rowwise() %>% mutate(beta2=rnorm(1,mubeta1, 1/sqrt(taubeta1))) %>% dplyr::select(beta2)
  # beta3.c<-cbind(undo(mod[["BUGSoutput"]][["sims.list"]][["mu.beta"]][,3,m]) %>% rename(mubeta1=name),
  #                undo(mod[["BUGSoutput"]][["sims.list"]][["tau.beta"]][,3,m]) %>% rename(taubeta1=name))%>% 
  #   rowwise() %>% mutate(beta3=rnorm(1,mubeta1, 1/sqrt(taubeta1))) %>% dplyr::select(beta3)
  # beta4.c<-cbind(undo(mod[["BUGSoutput"]][["sims.list"]][["mu.beta"]][,4,m]) %>% rename(mubeta1=name),
  #                undo(mod[["BUGSoutput"]][["sims.list"]][["tau.beta"]][,4,m]) %>% rename(taubeta1=name))%>% 
  #   rowwise() %>% mutate(beta4=rnorm(1,mubeta1, 1/sqrt(taubeta1))) %>% dplyr::select(beta4)
  # beta5.c<-cbind(undo(mod[["BUGSoutput"]][["sims.list"]][["mu.beta"]][,5,m]) %>% rename(mubeta1=name),
  #                undo(mod[["BUGSoutput"]][["sims.list"]][["tau.beta"]][,5,m]) %>% rename(taubeta1=name))%>% 
  #   rowwise() %>% mutate(beta5=rnorm(1,mubeta1, 1/sqrt(taubeta1))) %>% dplyr::select(beta5)
  # beta6.c<-cbind(undo(mod[["BUGSoutput"]][["sims.list"]][["mu.beta"]][,6,m]) %>% rename(mubeta1=name),
  #                undo(mod[["BUGSoutput"]][["sims.list"]][["tau.beta"]][,6,m]) %>% rename(taubeta1=name))%>% 
  #   rowwise() %>% mutate(beta6=rnorm(1,mubeta1, 1/sqrt(taubeta1))) %>% dplyr::select(beta6)
  
  beta.c<-cbind(beta1.c,beta2.c
                #,beta3.c,beta4.c,beta5.c,beta6.c
                )
  names(beta.c)<-paste0(names(beta.c), m)
  
  if (m==1){
    betas<-beta.c
  } else{
    betas<-cbind(betas, beta.c)
  }
}


coeffs<-cbind(betas)

#----
#delta
regions<-mod$model[[1]]$data()$region.c
yearIndex<-as.data.frame(list(year=2010:2020, yearIndex=1:11))

#-----------
#adding for each of the countries

regionCodes<-readRDS("output/regionCodes.RDS")
toPred<-anti_join(regionCodes, vnDataFinal %>% 
                    filter(!is.na(level) & Year>=2010), by=c("ISO")) %>% 
  dplyr::select(ISO)
toPred2<-merge(x=vnDataFinal, y=toPred, all.y=TRUE, by=c("ISO")) %>% 
  mutate(pretermSe=(pretermRate-pretermLower)/1.96) %>% 
  dplyr::select(OfficialName, ISO, year=Year,
                pretermRate, pretermSe, NMRgroup,nmr, 
                regionIndex, regionName) %>% 
  #filter(year==2020)%>% 
  mutate(regionIndex=ifelse(regionIndex==1, 1,2))

# regionCodesOther=read.csv("input/sbr_regions.csv")%>%
#   dplyr::select(ISO3Code,SDGRegionrev1, sdg.rev1, OfficialName) %>% rename("regionIndex"="sdg.rev1",
#                                                                            "ISO"="ISO3Code", "regionName"="SDGRegionrev1")
# saveRDS(regionCodesOther, "output/regionCodesOther.RDS")

regionCodesOther<-readRDS("output/regionCodesOther.RDS") %>% 
  rename(regionIndex2=regionIndex, regionName2=regionName)

toPred3<-merge(x=toPred2, y=regionCodesOther%>% 
                 dplyr::select(-OfficialName),
               by="ISO", all.x=TRUE)

isos<-unique(toPred3$ISO)
nIsos<-length(isos)

predict<-c()
region1<-as.data.frame(list(ISO=NA, year=NA, p1=NA,
                            p2=NA, p3=NA, p4=NA, NMRgroup=NA, nmr=NA))
region2<-as.data.frame(list(ISO=NA, year=NA, p1=NA,
                            p2=NA, p3=NA, p4=NA, NMRgroup=NA, nmr=NA))
region3<-as.data.frame(list(ISO=NA, year=NA, p1=NA,
                            p2=NA, p3=NA, p4=NA, NMRgroup=NA, nmr=NA))
region4<-as.data.frame(list(ISO=NA, year=NA, p1=NA,
                            p2=NA, p3=NA, p4=NA, NMRgroup=NA, nmr=NA))
region5<-as.data.frame(list(ISO=NA, year=NA, p1=NA,
                            p2=NA, p3=NA, p4=NA, NMRgroup=NA, nmr=NA))
region6<-as.data.frame(list(ISO=NA, year=NA, p1=NA,
                            p2=NA, p3=NA, p4=NA, NMRgroup=NA, nmr=NA))
for (i in 1:nIsos){
  
  iso<-isos[i]
  cPred<-toPred3 %>% filter(ISO==iso)
  cPred2<-merge(x=cPred, y=coeffs)
  
  for (m in 1:2){
    #Country-intercept
    cPred4<-cPred2
    cPred4$intercept<-cPred2[,which(names(cPred2)== paste0("beta", unique(cPred2$regionIndex),m))]
    
    #Covariates: Alpha1 - Preterm, alpha2 - NMR
    cPred5<-cPred4
    cPred6<-cPred5
    
    cPredList2<-cPred6 %>% mutate(logitP=intercept) %>% 
      dplyr::select(ISO, year, logitP, pretermRate, pretermSe, NMRgroup, nmr)
    
    
    names(cPredList2)<-paste0(names(cPredList2),c("", "", m, "", "", ""))
    
    if (m==1){
      predictions<-cPredList2
    } else{
      predictions<-cbind(predictions, cPredList2 %>% 
                           dplyr::select(-c(ISO, year, pretermRate, pretermSe, NMRgroup,
                                            nmr)))
    }
    
  } #end of m
  
  predictions2<-predictions %>% rowwise() %>% 
    mutate(pretermEst=rnorm(1,pretermRate, pretermSe)) %>% 
    ungroup() %>% mutate(p1S=inv.logit(logitP1), 
                         p2S=inv.logit(logitP2)) %>% 
    mutate(p1=p1S*(pretermRate/100), #PT_SGA
           p2=(1-p1S)*(pretermRate/100),#PT_NSGA
           p3=p2S*(1-(pretermRate/100)), #T_SGA
           p4=(1-p2S)*(1-(pretermRate/100))) %>% 
    dplyr::select(ISO, year, p1, p2, p3, p4, NMRgroup, nmr)
  
  cPredList3<-predictions2 %>% group_by(ISO, year) %>% summarise(PT_SGA_Est_lower=quantile(p1, 0.025), 
                                                                 PT_SGA_Est=quantile(p1, 0.5), 
                                                                 PT_SGA_Est_upper=quantile(p1, 0.975),
                                                                 
                                                                 PT_NSGA_Est_lower=quantile(p2, 0.025), 
                                                                 PT_NSGA_Est=quantile(p2, 0.5), 
                                                                 PT_NSGA_Est_upper=quantile(p2, 0.975),
                                                                 
                                                                 T_SGA_Est_lower=quantile(p3, 0.025), 
                                                                 T_SGA_Est=quantile(p3, 0.5), 
                                                                 T_SGA_Est_upper=quantile(p3, 0.975),
                                                                 
                                                                 T_NSGA_Est_lower=quantile(p4, 0.025), 
                                                                 T_NSGA_Est=quantile(p4, 0.5), 
                                                                 T_NSGA_Est_upper=quantile(p4, 0.975)) %>% 
    mutate(sum=PT_SGA_Est+PT_NSGA_Est+T_SGA_Est+T_NSGA_Est)
  
  region<-unique(cPred2$regionIndex2)
  
  if(region==1){
    region1<-rbind(region1, predictions2)
  } else if(region==2){
    region2<-rbind(region2, predictions2)
  } else if(region==3){
    region3<-rbind(region3, predictions2)
  } else if(region==4){
    region4<-rbind(region4, predictions2)
  } else if(region==5){
    region5<-rbind(region5, predictions2)
  } else{
    region6<-rbind(region6, predictions2)
  } 
  
  if (i==1){
    predict<-cPredList3
  } else{
    predict<-rbind(predict, cPredList3)
  }
  
}

# length(unique(region1$ISO))+length(unique(region2$ISO))+length(unique(region3$ISO))+
#   length(unique(region4$ISO))+length(unique(region5$ISO))+length(unique(region6$ISO))+
#   length(unique(region1A$ISO))+length(unique(region2A$ISO))+length(unique(region3A$ISO))+
#   length(unique(region4A$ISO))+length(unique(region5A$ISO))+length(unique(region6A$ISO))
# 
# length(unique(region1$ISO))+  length(unique(region1A$ISO))
# length(unique(region2$ISO))+  length(unique(region2A$ISO))

#---------------------
#Country-level estimates

source("0.fileNames.R")
wpp_2021 <- readxl::read_xlsx(paste0("inputs/", wpp2))
wpp<-wpp_2021 %>% filter(LocTypeName=="Country/Area" & Year>1994 & Year<=2020) %>% 
  mutate(OfficialName=ifelse(LocationName=="North Macedonia", "Republic of North Macedonia",
                             ifelse(LocationName=="Micronesia (Fed. States of)", "Micronesia (Federated States of)",
                                    ifelse(LocationName=="Dem. People's Republic of Korea", 	"Democratic People's Republic of Korea",
                                           ifelse(LocationName=="Côte d'Ivoire", "Cote d'Ivoire", LocationName)))),
         wpp_lb=Total*1000)
wpp2<-merge(x=wpp, y=regionCodesOther, by="OfficialName", all.y=TRUE) %>% dplyr::select(ISO, year=Year, wpp_lb, 
                                                                                        regionIndex2, regionName2)


countries<-vnDataFinal %>% filter(!is.na(level) & Year>=2010) %>%  
  dplyr::select(ISO) %>% distinct() %>% 
  arrange(ISO) %>% mutate(country1=row_number())
preterms<-merge(x=vnDataFinal, y=countries, by="ISO", all.x=TRUE) %>% 
  filter(!is.na(country1) & Year==2020) %>% dplyr::select(ISO, pretermRate) 

dim<-dim(mod$BUGSoutput$median$p[,,])
for (i in 1:dim[1]){
  for (j in 1:dim[2]){
    output1<-as.data.frame(list(country=i, year=j+2009, 
                                PT_SGA_Est_lower=quantile(mod$BUGSoutput$sims.list$q[,i,j,1], 0.025),
                                PT_SGA_Est=mod$BUGSoutput$median$q[i,j,1],
                                PT_SGA_Est_upper=quantile(mod$BUGSoutput$sims.list$q[,i,j,1], 0.975),
                                
                                PT_NSGA_Est_lower=quantile(mod$BUGSoutput$sims.list$q[,i,j,2], 0.025),
                                PT_NSGA_Est=mod$BUGSoutput$median$q[i,j,2], 
                                PT_NSGA_Est_upper=quantile(mod$BUGSoutput$sims.list$q[,i,j,2], 0.975),
                                
                                T_SGA_Est_lower=quantile(mod$BUGSoutput$sims.list$q[,i,j,3], 0.025),
                                T_SGA_Est=mod$BUGSoutput$median$q[i,j,3],
                                T_SGA_Est_upper=quantile(mod$BUGSoutput$sims.list$q[,i,j,3], 0.975),
                                
                                T_NSGA_Est_lower=quantile(mod$BUGSoutput$sims.list$q[,i,j,4], 0.025),
                                T_NSGA_Est=mod$BUGSoutput$median$q[i,j,4], 
                                T_NSGA_Est_upper=quantile(mod$BUGSoutput$sims.list$q[,i,j,4], 0.975)))
    if (i==1 & j==1){
      output<-output1
    } else{
      output<-rbind(output, output1)
    }
  }
}

allData<-merge(x=output, y=countries %>% rename(country=country1), by="country", all.x=TRUE) %>% 
  dplyr::select(-country) %>% mutate(predicted=0)

predict2<-predict %>% dplyr::select(-sum) %>% 
  mutate(predicted=1)

rates<-rbind(allData, predict2)%>% 
  filter(if_any(everything(), ~ !is.na(.))) %>% 
  filter(year>=2000)

estimates<-merge(x=rates, 
                 y=wpp2,
                 by=c("ISO", "year"), 
                 all.x=TRUE) %>% 
  mutate(PT_SGA_Est_lowerN=round(PT_SGA_Est_lower*wpp_lb),
         PT_SGA_EstN=round(PT_SGA_Est*wpp_lb),     
         PT_SGA_Est_upperN=round(PT_SGA_Est_upper*wpp_lb), 
         PT_NSGA_Est_lowerN=round(PT_NSGA_Est_lower*wpp_lb),
         PT_NSGA_EstN=round(PT_NSGA_Est*wpp_lb),
         PT_NSGA_Est_upperN=round(PT_NSGA_Est_upper*wpp_lb),
         T_SGA_Est_lowerN=round(T_SGA_Est_lower*wpp_lb),
         T_SGA_EstN=round(T_SGA_Est*wpp_lb),
         T_SGA_Est_upperN=round(T_SGA_Est_upper*wpp_lb),
         T_NSGA_Est_lowerN=round(T_NSGA_Est_lower*wpp_lb), 
         T_NSGA_EstN=round(T_NSGA_Est*wpp_lb),
         T_NSGA_Est_upperN=round(T_NSGA_Est_upper*wpp_lb))

write.csv(estimates,
          paste0("output/" ,fileName,"_", 
                 ((niter-nburnin)/nthin)*nchains,"_Estimates", ".csv"))
rm(predict, cPredList, cPred, cPred2, 
   cPred3, cPredList, cPredList2, cPredList3,
   cPredTime, 
   cPredTime2, d, allCovars)


#----------------------
#Getting regional and global estimates
mu<-mod[["BUGSoutput"]][["sims.list"]][["q"]]
isos<-vnDataFinal %>% filter(!is.na(level) & Year>=2010) %>% 
  dplyr::select(ISO) %>% 
  arrange(ISO) %>% 
  distinct() %>% mutate(countryIndex=row_number())
isos<-merge(x=isos, y=regionCodesOther, by="ISO", all.x=TRUE)
for (r in 1:6){
  isos1<-isos %>% filter(regionIndex2==r)
  for (i in 1:nrow(isos1)){
    datas<-as.data.frame(list(ISO=rep(isos1$ISO[i], dim(mu)[1]),
                              year=rep(2020, dim(mu)[1]),
                              PT_SGA_Est=mu[,isos1$countryIndex[i],11,1],
                              PT_NSGA_Est=mu[,isos1$countryIndex[i],11,2], 
                              T_SGA_Est=mu[,isos1$countryIndex[i],11,3],
                              T_NSGA_Est=mu[,isos1$countryIndex[i],11,4],
                              NMRgroup=as.numeric(vnDataFinal %>% 
                                                    filter(ISO==isos1$ISO[i] & Year==2020) %>% 
                                                    dplyr::select(NMRgroup)),
                              nmr=as.numeric(vnDataFinal %>% 
                                               filter(ISO==isos1$ISO[i] & Year==2020) %>% 
                                               dplyr::select(nmr))))
    
    if (i==1){
      otherPs<-datas
    }else{
      otherPs<-rbind(otherPs, datas)
    }
    #}
    print(i)
  }
  assign(paste0("region", r, "A"), otherPs)
}


#wpp2 %>% filter(year==2020) %>% group_by(regionIndex2) %>% summarise(wpp_lb=sum(wpp_lb))

#------------------------
#REGIONS

for (i in 1:7){
  
  if(i==1){ data1<-region1
  data2<-region1A
  } else if (i==2){ data1<-region2
  data2<-region2A
  } else if (i==3){data1<-region3
  data2<-region3A
  } else if (i==4){ data1<-region4
  data2<-region4A
  }else if (i==5){ data1<-region5
  data2<-region5A
  }else if (i==6){ data1<-region6
  data2<-region6A
  }else if(i==7){
    data3<-regionsAll
  }
  
  if (i<7){
    data3<-rbind(x=data1 %>% filter(!ISO %in% region1A$ISO) %>% 
                   rename(PT_SGA_Est=p1, PT_NSGA_Est=p2, 
                          T_SGA_Est=p3, T_NSGA_Est=p4),
                 y=data2)
  }
  data4<-merge(x=data3, y=wpp2 %>% dplyr::select(-c(regionIndex2, regionName2)), 
               by=c("ISO", "year"), all.x=TRUE)
  data5<-data4 %>% filter(!is.na(ISO))%>% 
    mutate(numbers1=wpp_lb*PT_SGA_Est,
           numbers2=wpp_lb*PT_NSGA_Est,
           numbers3=wpp_lb*T_SGA_Est,
           numbers4=wpp_lb*T_NSGA_Est,
           
    ) %>% 
    mutate(TNSGA_mR=ifelse(NMRgroup==1, 0.6, 
                           ifelse(NMRgroup==2, 6.3, 
                                  ifelse(NMRgroup==3, 10.1, 
                                         ifelse(NMRgroup==4, 13.8, NA)))),
           PTSGA_mR=ifelse(NMRgroup==1,35.1 , 
                           ifelse(NMRgroup==2, 76.6, 
                                  ifelse(NMRgroup==3, 147.6, 
                                         ifelse(NMRgroup==4, 134.3, NA)))),
           PTNSGA_mR=ifelse(NMRgroup==1, 20.7, 
                            ifelse(NMRgroup==2, 23.3, 
                                   ifelse(NMRgroup==3, 85.9, 
                                          ifelse(NMRgroup==4, 88.1, NA)))),
           TSGA_mR=ifelse(NMRgroup==1, 3.0, 
                          ifelse(NMRgroup==2, 10.2, 
                                 ifelse(NMRgroup==3, 28.8, 
                                        ifelse(NMRgroup==4, 31.3, NA))))) %>% 
    mutate(T_NSGA_M_N=numbers4*(TNSGA_mR/1000),
           T_SGA_M_N=numbers3*(TSGA_mR/1000),
           PT_NSGA_M_N=numbers2*(PTNSGA_mR/1000),
           PT_SGA_M_N=numbers1*(PTSGA_mR/1000),
           #Any_M=PT_SGA_M+PT_NSGA_M+T_SGA_M,
           NM=wpp_lb*(nmr/1000)) %>% 
    mutate(all_SVN=numbers1+numbers2+numbers3+numbers4,
           all_SVN_M=T_NSGA_M_N+T_SGA_M_N+PT_NSGA_M_N+PT_SGA_M_N) %>% 
    mutate(PT_SGA_RR=PTSGA_mR/TNSGA_mR,
           PT_NSGA_RR=PTNSGA_mR/TNSGA_mR, 
           T_SGA_RR=TSGA_mR/TNSGA_mR) %>% 
    mutate(T_NSGA_M=(T_NSGA_M_N/all_SVN_M), 
           T_SGA_M=(T_SGA_M_N/all_SVN_M),
           PT_NSGA_M=(PT_NSGA_M_N/all_SVN_M),
           PT_SGA_M=(PT_SGA_M_N/all_SVN_M),
           
           # T_NSGA_MPercFit=(T_NSGA_M/all_SVN_M)*NM, 
           # T_SGA_MPercFit=(T_SGA_M/all_SVN_M)*NM,
           # PT_NSGA_MPercFit=(PT_NSGA_M/all_SVN_M)*NM,
           # PT_SGA_MPercFit=(PT_SGA_M/all_SVN_M)*NM
    ) %>% 
    
    mutate(PT_SGA_PAR=(PT_SGA_Est*(PT_SGA_RR-1))/(PT_NSGA_Est*PT_NSGA_RR +
                                                    T_SGA_Est*T_SGA_RR +
                                                    T_NSGA_Est*1+
                                                    PT_SGA_Est*PT_SGA_RR),
           PT_NSGA_PAR=(PT_NSGA_Est*(PT_NSGA_RR-1))/(PT_NSGA_Est*PT_NSGA_RR +
                                                       T_SGA_Est*T_SGA_RR+
                                                       T_NSGA_Est*1+
                                                       PT_SGA_Est*PT_SGA_RR),
           T_SGA_PAR=(T_SGA_Est*(T_SGA_RR-1))/(PT_NSGA_Est*PT_NSGA_RR +
                                                 T_SGA_Est*T_SGA_RR +
                                                 T_NSGA_Est*1+
                                                 PT_SGA_Est*PT_SGA_RR)) %>% 
    mutate(PT_SGA_PAR_N=PT_SGA_PAR*NM, 
           PT_NSGA_PAR_N=PT_NSGA_PAR*NM, 
           T_SGA_PAR_N=T_SGA_PAR*NM
           #,
           #Any_PAR_N=PT_SGA_PAR_N+PT_NSGA_PAR_N+T_SGA_PAR_N
    ) %>%
    group_by(ISO,year) %>% mutate(iteration=row_number())  %>% 
    group_by(iteration, year)%>% 
    summarise(PT_SGA_N=sum(numbers1),
              PT_NSGA_N=sum(numbers2),
              T_SGA_N=sum(numbers3),
              T_NSGA_N=sum(numbers4),
              all_SVN_N=PT_SGA_N+PT_NSGA_N+T_SGA_N+T_NSGA_N,
              #sga=sum(sga),
              #Any=sum(Any),
              
              
              PT_SGA_M_N=sum(PT_SGA_M_N),
              PT_NSGA_M_N=sum(PT_NSGA_M_N),
              T_SGA_M_N=sum(T_SGA_M_N),
              T_NSGA_M_N=sum(T_NSGA_M_N),
              #Any_M=sum(Any_M),
              all_SVN_M=sum(all_SVN_M),
              
              PT_SGA_PAR_N=sum(PT_SGA_PAR_N),
              PT_NSGA_PAR_N=sum(PT_NSGA_PAR_N),
              T_SGA_PAR_N=sum(T_SGA_PAR_N),
              #Any_PAR_N=sum(Any_PAR_N),

              wppR=sum(wpp_lb),
              NM=sum(NM)) %>% 
    mutate(              SGA_N=PT_SGA_N+T_SGA_N,
                         Any_N=PT_SGA_N+PT_NSGA_N+T_SGA_N,
                         SGA_M_N=PT_SGA_M_N+T_SGA_M_N,
                         Any_M_N=PT_SGA_M_N+PT_NSGA_M_N+T_SGA_M_N,
                         SGA_PAR_N=PT_SGA_PAR_N+T_SGA_PAR_N,
                         Any_PAR_N=PT_SGA_PAR_N+PT_NSGA_PAR_N+T_SGA_PAR_N,
      
      PT_SGA_Est=PT_SGA_N/wppR,
           PT_NSGA_Est=PT_NSGA_N/wppR,
           T_SGA_Est=T_SGA_N/wppR,
           T_NSGA_Est=T_NSGA_N/wppR,
           SGA_Est=SGA_N/wppR,
           Any_Est=Any_N/wppR,
           
           
           T_NSGA_M=(T_NSGA_M_N/all_SVN_M), 
           T_SGA_M=(T_SGA_M_N/all_SVN_M),
           PT_NSGA_M=(PT_NSGA_M_N/all_SVN_M),
           PT_SGA_M=(PT_SGA_M_N/all_SVN_M),
           SGA_M=SGA_M_N/all_SVN_M,
           Any_M=Any_M_N/all_SVN_M,
           
           PT_SGA_PAR=PT_SGA_PAR_N/all_SVN_M,
           PT_NSGA_PAR=PT_NSGA_PAR_N/all_SVN_M,
           T_SGA_PAR=T_SGA_PAR_N/all_SVN_M,
           SGA_PAR=SGA_PAR_N/all_SVN_M,
           Any_PAR=Any_PAR_N/all_SVN_M) %>% 
    ungroup() %>% 
    group_by(year) %>% 
    summarise(PT_SGA_Est_lower=quantile(PT_SGA_Est, 0.25, na.rm=TRUE),
              PT_SGA_Est_upper=quantile(PT_SGA_Est, 0.75, na.rm=TRUE),
              PT_NSGA_Est_lower=quantile(PT_NSGA_Est, 0.25, na.rm=TRUE),
              PT_NSGA_Est_upper=quantile(PT_NSGA_Est, 0.75, na.rm=TRUE),
              T_SGA_Est_lower=quantile(T_SGA_Est, 0.25, na.rm=TRUE),
              T_SGA_Est_upper=quantile(T_SGA_Est, 0.75, na.rm=TRUE),
              T_NSGA_Est_lower=quantile(T_NSGA_Est, 0.25, na.rm=TRUE),
              T_NSGA_Est_upper=quantile(T_NSGA_Est, 0.75, na.rm=TRUE),
              SGA_Est_lower=quantile(SGA_Est, 0.25, na.rm=TRUE),
              SGA_Est_upper=quantile(SGA_Est, 0.75, na.rm=TRUE),
              Any_Est_lower=quantile(Any_Est, 0.25, na.rm=TRUE),
              Any_Est_upper=quantile(Any_Est, 0.75, na.rm=TRUE),
              
              PT_SGA_M_lower=quantile(PT_SGA_M, 0.25, na.rm=TRUE),
              PT_SGA_M_upper=quantile(PT_SGA_M, 0.75, na.rm=TRUE),
              PT_NSGA_M_lower=quantile(PT_NSGA_M, 0.25, na.rm=TRUE),
              PT_NSGA_M_upper=quantile(PT_NSGA_M, 0.75, na.rm=TRUE),
              T_SGA_M_lower=quantile(T_SGA_M, 0.25, na.rm=TRUE),
              T_SGA_M_upper=quantile(T_SGA_M, 0.75, na.rm=TRUE),
              T_NSGA_M_lower=quantile(T_NSGA_M, 0.25, na.rm=TRUE),
              T_NSGA_M_upper=quantile(T_NSGA_M, 0.75, na.rm=TRUE),
              SGA_M_lower=quantile(SGA_M, 0.25, na.rm=TRUE),
              SGA_M_upper=quantile(SGA_M, 0.75, na.rm=TRUE),
              Any_M_lower=quantile(Any_M, 0.25, na.rm=TRUE),
              Any_M_upper=quantile(Any_M, 0.75, na.rm=TRUE),
              
              PT_SGA_PAR_lower=quantile(PT_SGA_PAR, 0.25, na.rm=TRUE),
              PT_SGA_PAR_upper=quantile(PT_SGA_PAR, 0.75, na.rm=TRUE),
              PT_NSGA_PAR_lower=quantile(PT_NSGA_PAR, 0.25, na.rm=TRUE),
              PT_NSGA_PAR_upper=quantile(PT_NSGA_PAR, 0.75, na.rm=TRUE),
              T_SGA_PAR_lower=quantile(T_SGA_PAR, 0.25, na.rm=TRUE),
              T_SGA_PAR_upper=quantile(T_SGA_PAR, 0.75, na.rm=TRUE),
              SGA_PAR_lower=quantile(SGA_PAR, 0.25, na.rm=TRUE),
              SGA_PAR_upper=quantile(SGA_PAR, 0.75, na.rm=TRUE),
              Any_PAR_lower=quantile(Any_PAR, 0.25, na.rm=TRUE),
              Any_PAR_upper=quantile(Any_PAR, 0.75, na.rm=TRUE),
              
              wppR=mean(wppR),
              NM=mean(NM)) %>% 
    
    mutate(PT_SGA_N_lower=round(PT_SGA_Est_lower*wppR,0),
           PT_SGA_N_upper=round(PT_SGA_Est_upper*wppR,0), 
           PT_NSGA_N_lower=round(PT_NSGA_Est_lower*wppR,0),
           PT_NSGA_N_upper=round(PT_NSGA_Est_upper*wppR,0),
           T_SGA_N_lower=round(T_SGA_Est_lower*wppR,0),
           T_SGA_N_upper=round(T_SGA_Est_upper*wppR,0),
           T_NSGA_N_lower=round(T_NSGA_Est_lower*wppR,0),
           T_NSGA_N_upper=round(T_NSGA_Est_upper*wppR,0),
           SGA_N_lower=round(SGA_Est_lower*wppR,0),
           SGA_N_upper=round(SGA_Est_upper*wppR,0),
           Any_N_lower=round(Any_Est_lower*wppR,0),
           Any_N_upper=round(Any_Est_upper*wppR,0),
           
           PT_SGA_M_N_lower=round(PT_SGA_M_lower*NM,0),
           PT_SGA_M_N_upper=round(PT_SGA_M_upper*NM,0),
           PT_NSGA_M_N_lower=round(PT_NSGA_M_lower*NM,0),
           PT_NSGA_M_N_upper=round(PT_NSGA_M_upper*NM,0),
           T_SGA_M_N_lower=round(T_SGA_M_lower*NM,0),
           T_SGA_M_N_upper=round(T_SGA_M_upper*NM,0),
           T_NSGA_M_N_lower=round(T_NSGA_M_lower*NM,0),
           T_NSGA_M_N_upper=round(T_NSGA_M_upper*NM,0),
           SGA_M_N_lower=round(SGA_M_lower*NM,0),
           SGA_M_N_upper=round(SGA_M_upper*NM,0),
           Any_M_N_lower=round(Any_M_lower*NM,0),
           Any_M_N_upper=round(Any_M_upper*NM,0),
           
           PT_SGA_PAR_N_lower=round(PT_SGA_PAR_lower*NM,0),
           PT_SGA_PAR_N_upper=round(PT_SGA_PAR_upper*NM,0),
           PT_NSGA_PAR_N_lower=round(PT_NSGA_PAR_lower*NM,0),
           PT_NSGA_PAR_N_upper=round(PT_NSGA_PAR_upper*NM,0),
           T_SGA_PAR_N_lower=round(T_SGA_PAR_lower*NM,0),
           T_SGA_PAR_N_upper=round(T_SGA_PAR_upper*NM,0),
           SGA_PAR_N_lower=round(SGA_PAR_lower*NM,0),
           SGA_PAR_N_upper=round(SGA_PAR_upper*NM,0),
           Any_PAR_N_lower=round(Any_PAR_lower*NM,0),
           Any_PAR_N_upper=round(Any_PAR_upper*NM,0)) %>% 
    mutate(regionIndex=i)
  
  
  if (i==1){
    regionsAll<-data3
    all<-data5
  } else if (i<7){
    regionsAll<-rbind(regionsAll, data3)
    all<-rbind(all, data5)
  }
  else{
    all<-rbind(all, data5)
  }
  
}

regionsJ<-regionCodesOther %>% dplyr::select(regionIndex=regionIndex2, 
                                             regionName=regionName2) %>% 
  distinct()

all2<-merge(x=all, y=regionsJ, by="regionIndex", all.x=TRUE) %>% 
  mutate(regionName=ifelse(is.na(regionName), "Global", regionName)) 

country<-estimates %>% rename(Year=year) %>% 
  dplyr::select(-wpp_lb)

others<-vnDataFinal %>% dplyr::select(ISO, Year, NMRgroup, nmr) %>% distinct()
others2<-merge(x=others, y=wpp2 %>% rename(Year=year), by=c("ISO", "Year"), all.x=TRUE)
countryNumbers<-merge(x=country, y=others2, 
                      by=c("ISO", "Year"), all.x=TRUE) %>% 
  mutate(
    PT_SGA_N=PT_SGA_Est*wpp_lb,
    PT_NSGA_N=PT_NSGA_Est*wpp_lb, 
    T_SGA_N=T_SGA_Est*wpp_lb,
    T_NSGA_N=T_NSGA_Est*wpp_lb) %>% 
  mutate(TNSGA_mR=ifelse(NMRgroup==1, 0.6, 
                         ifelse(NMRgroup==2, 6.3, 
                                ifelse(NMRgroup==3, 10.1, 
                                       ifelse(NMRgroup==4, 13.8, NA)))),
         PTSGA_mR=ifelse(NMRgroup==1,35.1 , 
                         ifelse(NMRgroup==2, 76.6, 
                                ifelse(NMRgroup==3, 147.6, 
                                       ifelse(NMRgroup==4, 134.3, NA)))),
         PTNSGA_mR=ifelse(NMRgroup==1, 20.7, 
                          ifelse(NMRgroup==2, 23.3, 
                                 ifelse(NMRgroup==3, 85.9, 
                                        ifelse(NMRgroup==4, 88.1, NA)))),
         TSGA_mR=ifelse(NMRgroup==1, 3.0, 
                        ifelse(NMRgroup==2, 10.2, 
                               ifelse(NMRgroup==3, 28.8, 
                                      ifelse(NMRgroup==4, 31.3, NA))))) %>% 
  mutate(T_NSGA_M_N=T_NSGA_N*(TNSGA_mR/1000),
         T_SGA_M_N=T_SGA_N*(TSGA_mR/1000),
         PT_NSGA_M_N=PT_NSGA_N*(PTNSGA_mR/1000),
         PT_SGA_M_N=PT_SGA_N*(PTSGA_mR/1000),
         NM=wpp_lb*(nmr/1000)) %>% 
  mutate(PT_SGA_RR=PTSGA_mR/TNSGA_mR,
         PT_NSGA_RR=PTNSGA_mR/TNSGA_mR, 
         T_SGA_RR=TSGA_mR/TNSGA_mR) %>% 
  
  mutate(PT_SGA_PAR=(PT_SGA_Est*(PT_SGA_RR-1))/(PT_NSGA_Est*PT_NSGA_RR +
                                                  T_SGA_Est*T_SGA_RR +
                                                  T_NSGA_Est*1+
                                                  PT_SGA_Est*PT_SGA_RR),
         PT_NSGA_PAR=(PT_NSGA_Est*(PT_NSGA_RR-1))/(PT_NSGA_Est*PT_NSGA_RR +
                                                     T_SGA_Est*T_SGA_RR+
                                                     T_NSGA_Est*1+
                                                     PT_SGA_Est*PT_SGA_RR),
         T_SGA_PAR=(T_SGA_Est*(T_SGA_RR-1))/(PT_NSGA_Est*PT_NSGA_RR +
                                               T_SGA_Est*T_SGA_RR +
                                               T_NSGA_Est*1+
                                               PT_SGA_Est*PT_SGA_RR)) %>% 
  mutate(PT_SGA_PAR_N=PT_SGA_PAR*NM, 
         PT_NSGA_PAR_N=PT_NSGA_PAR*NM, 
         T_SGA_PAR_N=T_SGA_PAR*NM) %>% 
  mutate(regionName2=ifelse(is.na(regionName2.x), regionName2.y, regionName2.x),
         regionIndex2=ifelse(is.na(regionIndex2.x), regionIndex2.y, regionIndex2.x)) %>% 
  dplyr::select(-c(regionName2.x, regionName2.y, regionIndex2.x, regionIndex2.y))


regionNumbers<-countryNumbers %>% 
  group_by(regionName2, Year) %>% 
  summarise(PT_SGA_N=sum(PT_SGA_N), 
            PT_NSGA_N=sum(PT_NSGA_N), 
            T_SGA_N=sum(T_SGA_N),
            Any_N=PT_SGA_N+PT_NSGA_N+T_SGA_N,
            T_NSGA_N=sum(T_NSGA_N),
            wppR=sum(wpp_lb),
            
            
            PT_SGA_M_N=sum(PT_SGA_M_N),
            PT_NSGA_M_N=sum(PT_NSGA_M_N),
            T_SGA_M_N=sum(T_SGA_M_N),
            T_NSGA_M_N=sum(T_NSGA_M_N),
            
            NM=sum(NM),
            
            PT_SGA_PAR_N=sum(PT_SGA_PAR_N),
            PT_NSGA_PAR_N=sum(PT_NSGA_PAR_N),
            T_SGA_PAR_N=sum(T_SGA_PAR_N)) %>% 
  mutate(SGA_N=PT_SGA_N+T_SGA_N,
         SGA_M_N=PT_SGA_M_N+T_SGA_M_N,
         Any_M_N=PT_SGA_M_N+PT_NSGA_M_N+T_SGA_M_N,
         all_SVN_M=PT_SGA_M_N+PT_NSGA_M_N+T_SGA_M_N+T_NSGA_M_N,
         Any_PAR_N=PT_SGA_PAR_N+PT_NSGA_PAR_N+T_SGA_PAR_N,
         SGA_PAR_N=PT_SGA_PAR_N+T_SGA_PAR_N,
         
         PT_SGA_Est=PT_SGA_N/wppR, 
         PT_NSGA_Est=PT_NSGA_N/wppR, 
         T_SGA_Est=T_SGA_N/wppR, 
         Any_Est=Any_N/wppR,
         T_NSGA_Est=T_NSGA_N/wppR,
         SGA_Est=SGA_N/wppR,
         
         PT_SGA_M=(PT_SGA_M_N/all_SVN_M), 
         PT_NSGA_M=(PT_NSGA_M_N/all_SVN_M), 
         T_SGA_M=(T_SGA_M_N/all_SVN_M), 
         Any_M=(Any_M_N/all_SVN_M),
         T_NSGA_M=(T_NSGA_M_N/all_SVN_M),
         SGA_M=SGA_M_N/all_SVN_M,
         
         #Retrofitting so they fit into the subenv
         PT_SGA_M_N=(PT_SGA_M_N/all_SVN_M)*NM, 
         PT_NSGA_M_N=(PT_NSGA_M_N/all_SVN_M)*NM, 
         T_SGA_M_N=(T_SGA_M_N/all_SVN_M)*NM, 
         Any_M_N=(Any_M_N/all_SVN_M)*NM,
         T_NSGA_M_N=(T_NSGA_M_N/all_SVN_M)*NM,
         SGA_M_N=(SGA_M_N/all_SVN_M)*NM,
         
         PT_SGA_PAR=(PT_SGA_PAR_N/all_SVN_M),
         PT_NSGA_PAR=(PT_NSGA_PAR_N/all_SVN_M),
         T_SGA_PAR=(T_SGA_PAR_N/all_SVN_M),
         Any_PAR=(Any_PAR_N/all_SVN_M),
         SGA_PAR=SGA_PAR_N/all_SVN_M,
         
         #Retrofitting so they fit into the subenv
         PT_SGA_PAR_N=(PT_SGA_PAR_N/all_SVN_M)*NM,
         PT_NSGA_PAR_N=(PT_NSGA_PAR_N/all_SVN_M)*NM,
         T_SGA_PAR_N=(T_SGA_PAR_N/all_SVN_M)*NM,
         Any_PAR_N=(Any_PAR_N/all_SVN_M)*NM,
         SGA_PAR_N=(SGA_PAR_N/all_SVN_M)*NM)


globalNumbers<-countryNumbers %>% 
  group_by(Year) %>% 
  summarise(PT_SGA_N=sum(PT_SGA_N), 
            PT_NSGA_N=sum(PT_NSGA_N), 
            T_SGA_N=sum(T_SGA_N),
            Any_N=PT_SGA_N+PT_NSGA_N+T_SGA_N,
            T_NSGA_N=sum(T_NSGA_N),
            wppR=sum(wpp_lb),
            
            
            PT_SGA_M_N=sum(PT_SGA_M_N),
            PT_NSGA_M_N=sum(PT_NSGA_M_N),
            T_SGA_M_N=sum(T_SGA_M_N),
            T_NSGA_M_N=sum(T_NSGA_M_N),
            
            NM=sum(NM),
            
            PT_SGA_PAR_N=sum(PT_SGA_PAR_N),
            PT_NSGA_PAR_N=sum(PT_NSGA_PAR_N),
            T_SGA_PAR_N=sum(T_SGA_PAR_N)) %>% 
  mutate(SGA_N=PT_SGA_N+T_SGA_N,
         SGA_M_N=PT_SGA_M_N+T_SGA_M_N,
         Any_M_N=PT_SGA_M_N+PT_NSGA_M_N+T_SGA_M_N,
         all_SVN_M=PT_SGA_M_N+PT_NSGA_M_N+T_SGA_M_N+T_NSGA_M_N,
         Any_PAR_N=PT_SGA_PAR_N+PT_NSGA_PAR_N+T_SGA_PAR_N,
         SGA_PAR_N=PT_SGA_PAR_N+T_SGA_PAR_N,
         
         PT_SGA_Est=PT_SGA_N/wppR, 
         PT_NSGA_Est=PT_NSGA_N/wppR, 
         T_SGA_Est=T_SGA_N/wppR, 
         Any_Est=Any_N/wppR,
         T_NSGA_Est=T_NSGA_N/wppR,
         SGA_Est=SGA_N/wppR,
         
         PT_SGA_M=(PT_SGA_M_N/all_SVN_M), 
         PT_NSGA_M=(PT_NSGA_M_N/all_SVN_M), 
         T_SGA_M=(T_SGA_M_N/all_SVN_M), 
         Any_M=(Any_M_N/all_SVN_M),
         T_NSGA_M=(T_NSGA_M_N/all_SVN_M),
         SGA_M=SGA_M_N/all_SVN_M,
         
         #Retrofitting so they fit into the subenv
         PT_SGA_M_N=(PT_SGA_M_N/all_SVN_M)*NM, 
         PT_NSGA_M_N=(PT_NSGA_M_N/all_SVN_M)*NM, 
         T_SGA_M_N=(T_SGA_M_N/all_SVN_M)*NM, 
         Any_M_N=(Any_M_N/all_SVN_M)*NM,
         T_NSGA_M_N=(T_NSGA_M_N/all_SVN_M)*NM,
         SGA_M_N=(SGA_M_N/all_SVN_M)*NM,
         
         PT_SGA_PAR=(PT_SGA_PAR_N/all_SVN_M),
         PT_NSGA_PAR=(PT_NSGA_PAR_N/all_SVN_M),
         T_SGA_PAR=(T_SGA_PAR_N/all_SVN_M),
         Any_PAR=(Any_PAR_N/all_SVN_M),
         SGA_PAR=SGA_PAR_N/all_SVN_M,
         
         #Retrofitting so they fit into the subenv
         PT_SGA_PAR_N=(PT_SGA_PAR_N/all_SVN_M)*NM,
         PT_NSGA_PAR_N=(PT_NSGA_PAR_N/all_SVN_M)*NM,
         T_SGA_PAR_N=(T_SGA_PAR_N/all_SVN_M)*NM,
         Any_PAR_N=(Any_PAR_N/all_SVN_M)*NM,
         SGA_PAR_N=(SGA_PAR_N/all_SVN_M)*NM) %>% 
  mutate(regionName2="Global")

both<-rbind(regionNumbers, globalNumbers)

allRG<-merge(x=both %>% filter(Year==2020) %>% rename(regionName=regionName2), 
             y=all2 %>% rename(Year=year), 
             by=c("regionName", "Year"), 
             all.x=TRUE) %>% 
  mutate(OfficialName=regionName, 
         countryIndex=NA, 
         predicted=NA,
         ISO=NA, 
         isoYear=NA) %>% rename(wpp_lb=wppR.x) %>% dplyr::select(-wppR.y) %>% 
  arrange(regionIndex)

allRG2<-allRG %>%
  dplyr::select(regionName, Year, 
                PT_SGA_Est_lower, PT_SGA_Est, PT_SGA_Est_upper,
                PT_NSGA_Est_lower, PT_NSGA_Est, PT_NSGA_Est_upper,
                T_SGA_Est_lower, T_SGA_Est, T_SGA_Est_upper,
                T_NSGA_Est_lower, T_NSGA_Est, T_NSGA_Est_upper,
                SGA_Est_lower, SGA_Est, SGA_Est_upper,
                Any_Est_lower, Any_Est, Any_Est_upper,
                
                PT_SGA_N_lower, PT_SGA_N, PT_SGA_N_upper,
                PT_NSGA_N_lower, PT_NSGA_N, PT_NSGA_N_upper,
                T_SGA_N_lower, T_SGA_N, T_SGA_N_upper,
                T_NSGA_N_lower, T_NSGA_N, T_NSGA_N_upper,
                SGA_N_lower, SGA_N, SGA_N_upper,
                Any_N_lower, Any_N, Any_N_upper,

                PT_SGA_M_N_lower, PT_SGA_M_N,PT_SGA_M_N_upper,
                PT_NSGA_M_N_lower, PT_NSGA_M_N, PT_NSGA_M_N_upper,
                T_SGA_M_N_lower, T_SGA_M_N, T_SGA_M_N_upper,
                Any_M_N_lower, Any_M_N, Any_M_N_upper,
                T_NSGA_M_N_lower, T_NSGA_M_N, T_NSGA_M_N_upper,
                SGA_M_N_lower, SGA_M_N, SGA_M_N_upper,

                PT_SGA_M_lower, PT_SGA_M,PT_SGA_M_upper,
                PT_NSGA_M_lower, PT_NSGA_M, PT_NSGA_M_upper,
                T_SGA_M_lower, T_SGA_M, T_SGA_M_upper,
                Any_M_lower, Any_M, Any_M_upper,
                T_NSGA_M_lower, T_NSGA_M, T_NSGA_M_upper,
                SGA_M_lower, SGA_M, SGA_M_upper,

                PT_SGA_PAR_N_lower, PT_SGA_PAR_N, PT_SGA_PAR_N_upper,
                PT_NSGA_PAR_N_lower, PT_NSGA_PAR_N, PT_NSGA_PAR_N_upper,
                T_SGA_PAR_N_lower, T_SGA_PAR_N, T_SGA_PAR_N_upper,
                Any_PAR_N_lower, Any_PAR_N, Any_PAR_N_upper,
                SGA_PAR_N_lower, SGA_PAR_N, SGA_PAR_N_upper,

                PT_SGA_PAR_lower, PT_SGA_PAR, PT_SGA_PAR_upper,
                PT_NSGA_PAR_lower, PT_NSGA_PAR, PT_NSGA_PAR_upper,
                T_SGA_PAR_lower, T_SGA_PAR, T_SGA_PAR_upper,
                Any_PAR_lower, Any_PAR, Any_PAR_upper,
                SGA_PAR_lower, SGA_PAR, SGA_PAR_upper)

write.csv(allRG2, 
          paste0("output/",
                 fileName,((niter-nburnin)/nthin)*nchains, "_regionalAndGlobalEstimates.csv"))


table1<-allRG %>% 
  mutate(PT_NSGA_Est=paste0(round(PT_NSGA_Est*100,1), " [", round(PT_NSGA_Est_lower*100,1),
                            ", ", round(PT_NSGA_Est_upper*100,1),"]"),
         PT_NSGA_N=paste0(round(PT_NSGA_N/1000,1), " [", round(PT_NSGA_N_lower/1000,1),
                          ", ", round(PT_NSGA_N_upper/1000,1),"]"),
         T_SGA_Est=paste0(round(T_SGA_Est*100,1), " [", round(T_SGA_Est_lower*100,1),
                          ", ", round(T_SGA_Est_upper*100,1),"]"),
         T_SGA_N=paste0(round(T_SGA_N/1000,1), " [", round(T_SGA_N_lower/1000,1),
                        ", ", round(T_SGA_N_upper/1000,1),"]"),
         PT_SGA_Est=paste0(round(PT_SGA_Est*100,1), " [", round(PT_SGA_Est_lower*100,1),
                           ", ", round(PT_SGA_Est_upper*100,1),"]"),
         PT_SGA_N=paste0(round(PT_SGA_N/1000,1), " [", round(PT_SGA_N_lower/1000,1),
                         ", ", round(PT_SGA_N_upper/1000,1),"]"),
         Any_Est=paste0(round(Any_Est*100,1), " [", round(Any_Est_lower*100,1),
                        ", ", round(Any_Est_upper*100,1),"]"),
         Any_N=paste0(round(Any_N/1000,1), " [", round(Any_N_lower/1000,1),
                      ", ", round(Any_N_upper/1000,1),"]")) %>% 
  arrange(factor(regionIndex, levels=c(5,6,3,1,2,4,7))) %>% 
  dplyr::select(regionName, Year, PT_NSGA_Est, PT_NSGA_N, T_SGA_Est,T_SGA_N, PT_SGA_Est, PT_SGA_N,
                Any_Est, Any_N)

write.csv(table1, paste0("output/",
                         fileName,((niter-nburnin)/nthin)*nchains, "_Table1.csv"))
#write.csv(table1, "VN types/output/table1regions2.csv")


s11<-allRG %>% 
  mutate(PT_NSGA_M=paste0(round(PT_NSGA_M*100,1), " [", round(PT_NSGA_M_lower*100,1),
                          ", ", round(PT_NSGA_M_upper*100,1),"]"),
         PT_NSGA_M_N=paste0(round(PT_NSGA_M_N,-2), " [", round(PT_NSGA_M_N_lower,-2),
                            ", ", round(PT_NSGA_M_N_upper,-2),"]"),
         T_SGA_M=paste0(round(T_SGA_M*100,1), " [", round(T_SGA_M_lower*100,1),
                        ", ", round(T_SGA_M_upper*100,1),"]"),
         T_SGA_M_N=paste0(round(T_SGA_M_N,-2), " [", round(T_SGA_M_N_lower,-2),
                          ", ", round(T_SGA_M_N_upper,-2),"]"),
         PT_SGA_M=paste0(round(PT_SGA_M*100,1), " [", round(PT_SGA_M_lower*100,1),
                         ", ", round(PT_SGA_M_upper*100,1),"]"),
         PT_SGA_M_N=paste0(round(PT_SGA_M_N,-2), " [", round(PT_SGA_M_N_lower,-2),
                           ", ", round(PT_SGA_M_N_upper,-2),"]"),
         Any_M=paste0(round(Any_M*100,1), " [", round(Any_M_lower*100,1),
                      ", ", round(Any_M_upper*100,1),"]"),
         Any_M_N=paste0(round(Any_M_N,-2), " [", round(Any_M_N_lower,-2),
                        ", ", round(Any_M_N_upper,-2),"]")) %>% 
  arrange(factor(regionIndex, levels=c(5,6,3,1,2,4,7))) %>% 
  dplyr::select(regionName, Year, PT_NSGA_M, PT_NSGA_M_N, T_SGA_M,T_SGA_M_N, PT_SGA_M, PT_SGA_M_N,
                Any_M, Any_M_N)

write.csv(s11, paste0("output/",
                      fileName,((niter-nburnin)/nthin)*nchains, "_TableS11.csv"))
#write.csv(s11, "VN types/output/s11regions2.csv")


s12<-allRG %>% 
  mutate(
    PT_NSGA_PAR=paste0(round(PT_NSGA_PAR*100,1), " [", round(PT_NSGA_PAR_lower*100,1),
                       ", ", round(PT_NSGA_PAR_upper*100,1),"]"),
    PT_NSGA_PAR_N=paste0(round(PT_NSGA_PAR_N,-2), " [", round(PT_NSGA_PAR_N_lower,-2),
                         ", ", round(PT_NSGA_PAR_N_upper,-2),"]"),
    T_SGA_PAR=paste0(round(T_SGA_PAR*100,1), " [", round(T_SGA_PAR_lower*100,1),
                     ", ", round(T_SGA_PAR_upper*100,1),"]"),
    T_SGA_PAR_N=paste0(round(T_SGA_PAR_N,-2), " [", round(T_SGA_PAR_N_lower,-2),
                       ", ", round(T_SGA_PAR_N_upper,-2),"]"),
    PT_SGA_PAR=paste0(round(PT_SGA_PAR*100,1), " [", round(PT_SGA_PAR_lower*100,1),
                      ", ", round(PT_SGA_PAR_upper*100,1),"]"),
    PT_SGA_PAR_N=paste0(round(PT_SGA_PAR_N,-2), " [", round(PT_SGA_PAR_N_lower,-2),
                        ", ", round(PT_SGA_PAR_N_upper,-2),"]"),
    Any_PAR=paste0(round(Any_PAR*100,1), " [", round(Any_PAR_lower*100,1),
                   ", ", round(Any_PAR_upper*100,1),"]"),
    Any_PAR_N=paste0(round(Any_PAR_N,-2), " [", round(Any_PAR_N_lower,-2),
                     ", ", round(Any_PAR_N_upper,-2),"]"),
  ) %>% 
  arrange(factor(regionIndex, levels=c(5,6,3,1,2,4,7))) %>% 
  dplyr::select(regionName, Year, PT_NSGA_PAR, PT_NSGA_PAR_N, T_SGA_PAR,
                T_SGA_PAR_N, PT_SGA_PAR, PT_SGA_PAR_N,
                Any_PAR, Any_PAR_N)

write.csv(s12, paste0("output/",
                      fileName,((niter-nburnin)/nthin)*nchains, "_TableS12.csv"))
#write.csv(s12, "VN types/output/s12regions2.csv")



#Get the regional preterm rate
# 
# preterms<-vnDataFinal %>% filter(Year==2020) %>% dplyr::select(ISO, pretermRate, wpp_lb) %>% 
#   mutate(preterm_N=wpp_lb*(pretermRate/100))
# #NEED TO PUT CIV BACK IN
# global<-preterms %>% summarise(preterm_N=sum(preterm_N, na.rm=TRUE),
#                                wpp_lb=sum(wpp_lb, na.rm=TRUE)) %>% 
#   mutate(pretermR=preterm_N/wpp_lb)

