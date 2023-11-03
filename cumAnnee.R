

#periode du cumul
cum=statSem %>% filter(annee==2023 | (annee==2022 & semaine>m))

cum2=cum %>% ungroup() %>% select(semaine,heures, minutes, TSS )%>%  arrange(semaine) %>% 
  mutate(CumTSS=cumsum(TSS),
         Cumtps=(cumsum(heures*60)+cumsum(minutes))/60,
         minutes=cumsum(minutes),heures=cumsum(heures))
cum2$heures=ifelse(cum2$minutes>59,cum2$heures+floor(cum2$minutes/60),cum2$heures)
cum2$minutes=ifelse(cum2$minutes>59,cum2$minutes-floor(cum2$minutes/60)*60,cum2$minutes)

cum2$minutes=ifelse(nchar(cum2$minutes)==1,paste0("0",cum2$minutes),cum2$minutes)
cum2$Cumduree=paste0(cum2$heures,"h",cum2$minutes)

