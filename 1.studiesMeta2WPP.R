#https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/subgroup.html

library(meta)
library(dplyr)
library(boot)
library(grid)
#?metaprop

vnFinal<-readRDS("output/vnDataFinal.rds") %>% 
  mutate(isoYear=paste0(ISO, Year))

subRegions=read.csv(paste0("inputs/","sbr_regions v2.csv"))%>% 
  dplyr::select(ISO=ISO3Code,subRegion=M49Region2, regionName=SDGRegionrev1) %>% 
  mutate(subRegion=ifelse(ISO=="LKA", "Western Asia", subRegion))

vnFinal2<-merge(x=vnFinal, y=subRegions %>% dplyr::select(-regionName), 
                by="ISO", all.x=TRUE)


studies<-vnFinal2 %>%  
  filter(Year>=2010 & level=="Study") %>% 
  mutate(#studyID=row_number(),
    studyID=paste0(ISO, Year))


studies %>% summarise(sumE=sum(T_SGA),
                      mean=mean(T_SGA_P1))

cells<-studies %>% mutate(T_SGA_5=ifelse(T_SGA<5, 1, 0),
                          PT_SGA_5=ifelse(PT_SGA<5, 1, 0), 
                          Preterm_5=ifelse(Preterm<5, 1, 0))
cells %>% summarise(T_SGA=sum(T_SGA_5), 
                    PT_SGA=sum(PT_SGA_5), 
                    Preterm=sum(Preterm_5)) %>% 
  mutate(T_SGA_Perc=T_SGA/nrow(cells), 
         PT_SGA_Perc=PT_SGA/nrow(cells), 
         Preterm_Perc=Preterm/nrow(cells))

metaPlot<-metaprop(event=T_SGA, n=Term, studlab = studyID,
                   subgroup=subRegion, data=studies, random=TRUE, overall = FALSE)
#Two alt methods from review
# metaPlot<-metaprop(event=T_SGA, n=Term, studlab = studyID,
#                    subgroup=subRegion, data=studies, random=TRUE, overall = FALSE, 
#                    method.random.ci = "HK",
#                    method="GLMM")
# metaPlot<-metaprop(event=T_SGA, n=Term, studlab = studyID,
#                    subgroup=subRegion, data=studies, random=TRUE, overall = FALSE, 
#                    method.random.ci = "HK", 
#                    method="inverse", adhoc.hakn.ci = "se")

metaPlot2<-metaprop(event=PT_SGA, n=Preterm, studlab = studyID,
                    subgroup=subRegion, data=studies, random=TRUE, overall = FALSE)
# metaPlot2<-metaprop(event=PT_SGA, n=Preterm, studlab = studyID,
#                     subgroup=subRegion, data=studies, random=TRUE, overall = FALSE,
#                     method.random.ci = "HK")
# metaPlot2<-metaprop(event=PT_SGA, n=Preterm, studlab = studyID,
#                     subgroup=subRegion, data=studies, random=TRUE, overall = FALSE,
#                     method.random.ci = "HK",
#                    method="inverse", adhoc.hakn.ci = "se")

metaPlot3<-metaprop(event=Preterm, n=allN, studlab = studyID,
                    subgroup=subRegion, data=studies, random=TRUE, overall = FALSE)

summary(metaPlot3)
#"Forest plot of random-effects meta-analysis of proportion of T_SGA within Term (T_SGA/Term)"
pdf(file = "output/subRegion_RandomMetaForest2.pdf", width = 10, height = 12)
forest.meta(metaPlot, layout="JAMA", common=FALSE, pooled.totals = FALSE, pooled.events = FALSE)
grid.text("Random-effects meta-analysis of proportion of T_SGA within Term (T_SGA/Term)", 0.5, 0.97, gp=gpar(cex=1))

forest.meta(metaPlot2, layout="JAMA", common=FALSE, pooled.totals = FALSE)
grid.text("Random-effects meta-analysis of proportion of PT_SGA within Preterm (PT_SGA/Preterm)", 0.5, 0.97, gp=gpar(cex=1))

forest.meta(metaPlot3, layout="JAMA", common=FALSE, pooled.totals = FALSE)
grid.text("Random-effects meta-analysis of proportion of Pretern within all livebirths (Preterm/allN)", 0.5, 0.97, gp=gpar(cex=1))


dev.off()




metaEsts<-as.data.frame(list(T_SGA_P1L_meta=inv.logit(metaPlot$TE.random.w-1.96*metaPlot$seTE.random.w),
                             T_SGA_P1_meta=inv.logit(metaPlot$TE.random.w),
                             T_SGA_P1U_meta=inv.logit(metaPlot$TE.random.w+1.96*metaPlot$seTE.random.w),
                             PT_SGA_P1L_meta=inv.logit(metaPlot2$TE.random.w-1.96*metaPlot2$seTE.random.w),
                             PT_SGA_P1_meta=inv.logit(metaPlot2$TE.random.w),
                             PT_SGA_P1U_meta=inv.logit(metaPlot2$TE.random.w+1.96*metaPlot2$seTE.random.w),
                             Preterm_P1L_meta=inv.logit(metaPlot3$TE.random.w-1.96*metaPlot3$seTE.random.w),
                             Preterm_P1_meta=inv.logit(metaPlot3$TE.random.w),
                             Preterm_P1U_meta=inv.logit(metaPlot3$TE.random.w+1.96*metaPlot3$seTE.random.w))) %>% 
  mutate(level="Study")
metaEsts$subRegion=rownames(metaEsts)

metaEstsalt<-as.data.frame(list(T_SGA_P1L_meta=inv.logit(metaPlot$TE.random.w-1.96*metaPlot$seTE.random.w),
                                T_SGA_P1_meta=inv.logit(metaPlot$TE.random.w),
                                T_SGA_P1U_meta=inv.logit(metaPlot$TE.random.w+1.96*metaPlot$seTE.random.w),
                                PT_SGA_P1L_meta=inv.logit(metaPlot2$TE.random.w-1.96*metaPlot2$seTE.hakn),
                                PT_SGA_P1_meta=inv.logit(metaPlot2$TE.random.w),
                                PT_SGA_P1U_meta=inv.logit(metaPlot2$TE.random.w+1.96*metaPlot2$seTE.random.w),
                                Preterm_P1L_meta=inv.logit(metaPlot3$TE.random.w-1.96*metaPlot3$seTE.random.w),
                                Preterm_P1_meta=inv.logit(metaPlot3$TE.random.w),
                                Preterm_P1U_meta=inv.logit(metaPlot3$TE.random.w+1.96*metaPlot3$seTE.random.w))) %>% 
  mutate(level="Study")
metaEsts$subRegion=rownames(metaEsts)

wpp2<-readRDS("output/wpp2.rds") %>% dplyr::select(ISO, Year=year, wpp_lb)

studies2<-merge(x=merge(x=vnFinal2, y=wpp2,
                        by=c("ISO", "Year"), all.x=TRUE) %>% 
                  filter(Year==2020 & ISO %in% studies$ISO),
                y=metaEsts %>% 
                  dplyr::select(subRegion, T_SGA_P1_meta, T_SGA_P1L_meta, T_SGA_P1U_meta,
                                PT_SGA_P1_meta, PT_SGA_P1L_meta, PT_SGA_P1U_meta, 
                                Preterm_P1_meta, Preterm_P1L_meta, Preterm_P1U_meta), 
                by="subRegion", all.x=TRUE) %>% 
  mutate(Preterm_meta=round(Preterm_P1_meta*wpp_lb,0), 
         Term_meta=wpp_lb-Preterm_meta,
         PT_SGA_meta=round(Preterm_meta*PT_SGA_P1_meta,0),
         PT_NSGA_meta=round(Preterm_meta*(1-PT_SGA_P1_meta),0),
         T_SGA_meta=round(Term_meta*T_SGA_P1_meta, 0),
         T_NSGA_meta=round(Term_meta*(1-T_SGA_P1_meta), 0)) %>% 
  mutate(Year=2020, level="Study")


vnFinal2<-rbind(vnFinal %>% filter(level=="National" & Year>=2010) %>% 
                  mutate(Preterm_meta=NA, Term_meta=NA, PT_SGA_meta=NA, T_SGA_meta=NA,
                         PT_NSGA_meta=NA, T_NSGA_meta=NA,
                         T_SGA_P1_meta=NA, T_SGA_P1L_meta=NA,T_SGA_P1U_meta=NA,
                         PT_SGA_P1_meta=NA,PT_SGA_P1L_meta=NA,PT_SGA_P1U_meta=NA,
                         Preterm_P1_meta=NA,Preterm_P1L_meta=NA,Preterm_P1U_meta=NA, subRegion=NA),
                studies2 %>% dplyr::select(-wpp_lb)) %>% dplyr::select(-c(regionName, regionIndex))

vnFinal3<-merge(x=vnFinal2, y=regionCodes, by="ISO", all.x=TRUE)

help<-vnFinal3 %>% filter(!is.na(level) & Year>=2010) %>% dplyr::select(ISO) %>% distinct()

saveRDS(vnFinal3, "output/vnDataFinal_meta2.RDS")

