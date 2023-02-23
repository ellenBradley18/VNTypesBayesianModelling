#PREDICT USING WHOLE ITERATIONS - AFTER ALEX CONSIDERATION
#Samples from the regional intercepts
#Takes the regional time from Z*delta rather than delta: NEED TO DO

#source("0.loadPackages.R")

#------------------------------
vnDataFinal<-readRDS("output/vnDataFinal.RDS") %>% 
  filter(Year>=2010)

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
#Z

Z<-list()
for (i in 1:11){
  for (j in 1:3){
    z<-as.data.frame(list(coefficient=paste0("Z.tk[", i,",",j, "]"), 
                          z=undo(mod$BUGSoutput$sims.array[,,paste0("Z.tk[", i,",",j, "]")]))) %>% 
      mutate(id=row_number())
    Z<-rbind(Z,z)
  }
}

Zbig<-reshape(Z, idvar="id", timevar="coefficient", direction = "wide")

#----
#delta
regions<-mod$model[[1]]$data()$region.c
yearIndex<-as.data.frame(list(year=2010:2020, yearIndex=1:11))

delta<-list()

for (k in 1:2){
  
  counts<-which(regions==k)
  
  for (m in 1:2){
    
    for (i in 1:3){
      b<-list()
      for (j in 1:length(counts)){
        
        d<-as.data.frame(list(z1=undo(mod$BUGSoutput$sims.array[,,paste0("delta.hc[", i,",",counts[j], ",",m, "]")])))
        
        if (j==1){
          b<-d
        } else {
          b<-cbind(b,d)
        }
        
      }
      
      names(b)<-paste0(names(b), c(1:length(names(b))))
      bMean<-b %>% mutate(mean=rowMeans(.)) %>% dplyr::select(mean)
      names(bMean)<-paste0("r", k, "mean", i, m)
      
      assign(paste0("b", i,m ), bMean)
      
    }
    
  }
  
  all<-cbind(b11,b12, 
             b21,b22,
             b31,b32)
  
  if (k==1){
    delta<-all
  } else {
    delta<-cbind(delta,all)
  }
  
}

#------------------
#adding all together, rows for each iteration.

allCovars<-cbind(coeffs, cbind(Zbig, delta)) %>% dplyr::select(-id)
rm(alpha1, 
   alpha2NMR1, alpha2NMR2, alpha2NMR3, alpha2NMR4, 
   b, b11,b12,b13,b14,b15, 
   b21,b22,b23,b24,
   b31,b32,b33,b34,
   beta1.c, beta2.c, beta3.c, beta4.c, beta5.c, beta6.c,
   bMean, coeffs, z, Z, Zbig)
#-----------
#adding for each of the countries

regionCodes<-readRDS("output/regionCodes.RDS")
toPred<-anti_join(regionCodes, vnDataFinal %>% 
                    filter(!is.na(level)&Year>=2010), by=c("ISO")) %>% dplyr::select(ISO)
toPred2<-merge(x=toPred, y=regionCodes, by=c("ISO"), all.x=TRUE) %>% 
  mutate(regionIndex=ifelse(regionIndex==1, 1,2))
#toPred<-regionCodes %>% dplyr::select(ISO)
toPred3<-merge(x=vnDataFinal, y=toPred2, all.y=TRUE, by=c("ISO")) %>% 
  mutate(pretermSe=(pretermRate-pretermLower)/1.96) %>% 
  dplyr::select(OfficialName=OfficialName.y, ISO, year=Year, regionName=regionName.y, regionIndex=regionIndex.y,
                pretermRate, pretermSe)

isos<-unique(toPred2$ISO)
nIsos<-length(isos)

predict<-c()
for (i in 1:nIsos){
  
  iso<-isos[i]
  cPred<-toPred3 %>% filter(ISO==iso)
  cPred2<-merge(x=cPred, y=allCovars)
  
  for (m in 1:2){
    
    #Delta   
    deltaCol1<-which(names(cPred2)== paste0("r", unique(cPred2$regionIndex), "mean", 1, m))
    deltaCol2<-which(names(cPred2)== paste0("r", unique(cPred2$regionIndex), "mean", 2, m))
    deltaCol3<-which(names(cPred2)== paste0("r", unique(cPred2$regionIndex), "mean", 3, m))
    
    cPred3<-cPred2
    cPred3$deltaCol1<-cPred2[,deltaCol1]
    
    
    cPred3$deltaCol2<-cPred2[,deltaCol2]
    cPred3$deltaCol3<-cPred2[,deltaCol3]
    
    #Country-intercept
    cPred4<-cPred3
    cPred4$intercept<-cPred3[,which(names(cPred3)== paste0("beta", unique(cPred3$regionIndex),m))]
    
    #Covariates: Alpha1 - Preterm, alpha2 - NMR
    cPred5<-cPred4
    # cPred5$alpha1<-cPred4[,which(names(cPred4)== paste0("alpha1",m))]
    # cPred5$alpha2NMR1<-cPred4[,which(names(cPred4)== paste0("alpha2",m, "NMR", 1))]
    # cPred5$alpha2NMR2<-cPred4[,which(names(cPred4)== paste0("alpha2",m, "NMR", 2))]
    # cPred5$alpha2NMR3<-cPred4[,which(names(cPred4)== paste0("alpha2",m, "NMR", 3))]
    # cPred5$alpha2NMR4<-cPred4[,which(names(cPred4)== paste0("alpha2",m, "NMR", 4))]
    cPred6<-cPred5
    # cPred6<-cPred5 %>% mutate(nmr=ifelse(NMRgroup==1, alpha2NMR1, 
    #                                      ifelse(NMRgroup==2, alpha2NMR2,
    #                                             ifelse(NMRgroup==3, alpha2NMR3,
    #                                                    ifelse(NMRgroup==4, alpha2NMR4, NA)))),
    #                           preterm=pretermRate*alpha1)
    
    cPredList<-list()
    for (j in 1:11){
      cPredTime<-cPred6 %>% filter(year==yearIndex$year[j])
      
      zCol1<-which(names(cPredTime)== paste0("name.Z.tk[", j, ",", 1, "]"))
      zCol2<-which(names(cPredTime)== paste0("name.Z.tk[", j, ",", 2, "]"))
      zCol3<-which(names(cPredTime)== paste0("name.Z.tk[", j, ",", 3, "]"))
      
      cPredTime2<-cPredTime
      cPredTime2$zCol1<-cPredTime[,zCol1]
      cPredTime2$zCol2<-cPredTime[,zCol2]
      cPredTime2$zCol3<-cPredTime[,zCol3]
      
      if (j==1){
        cPredList<-cPredTime2
      } else{
        cPredList<-rbind(cPredList, cPredTime2)
      }
      
    }
    
    cPredList2<-cPredList %>% mutate(time=(deltaCol1*zCol1)+(deltaCol2*zCol2)+(deltaCol3*zCol3),
                                     logitP=intercept+time) %>% 
      dplyr::select(ISO, year, logitP, pretermRate, pretermSe)
    
    
    names(cPredList2)<-paste0(names(cPredList2),c("", "", m))
    
    if (m==1){
      predictions<-cPredList2
    } else{
      predictions<-cbind(predictions, cPredList2 %>% 
                           dplyr::select(-c(ISO, year, pretermRate, pretermSe)))
    }
    
  } #end of m
  
  predictions2<-predictions %>% rowwise() %>% 
    mutate(pretermEst=rnorm(1,pretermRate, pretermSe)) %>% 
    ungroup() %>% mutate(p1S=inv.logit(logitP1), 
                         p2S=inv.logit(logitP2)) %>% 
    mutate(p1=p1S*(pretermRate/100),
           p2=(1-p1S)*(pretermRate/100),
           p3=p2S*(1-(pretermRate/100)),
           p4=(1-p2S)*(1-(pretermRate/100)))
  
  
  
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
  
  
  if (i==1){
    predict<-cPredList3
  } else{
    predict<-rbind(predict, cPredList3)
  }
  
}


