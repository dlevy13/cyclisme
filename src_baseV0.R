library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)

## chargement des fichiers

creat_fic=function(){
  #1 charger base
  velo_0=readRDS("./bases/velo.RDS")
  #2 ajouter nouveaux CSV
  liste <- list.files(path="./bases",pattern="*.csv")
  if (length(liste !=0)){ 
    for (k in 1:length(liste)) {
      a <- read.csv2(paste0("./bases/",liste[k]),as.is = T,sep=",",
                                   encoding = "UTF-8")
      a=a %>% select(c("Type.d.activité","Date","Distance","Calories","Durée",
                             "Fréquence.cardiaque.moyenne","Fréquence.cardiaque.maximale",
                             "Vitesse.moyenne","Ascension.totale","Cadence.de.vélo.moyenne","Puissance.moy."))
      names(a)=c("type","date","dist","calor","duree","fc_moy","fc_max","vm","dplus","cadence","puissance")
      
      if(k==1){
        velo=a
      }else{
        velo=rbind(velo,a)
      }
      #renomer CSV
      #file.rename(from = paste0(rep,liste[k]), to = paste0(rep,liste[k],"0"))
      file.copy(from = paste0("./bases/",liste[k]), to = paste0("./bases/archiveCSV/",Sys.Date(),liste[k]))
      file.remove(paste0("./bases/",liste[k]))
    } 

    #Elimination des doublons
    velo=velo %>% distinct(type,date,dist,.keep_all = T)
    velo$dplus=as.numeric(velo$dplus)
    velo$cadence=as.numeric(velo$cadence)
    
    #création des var heures et minutes
    velo$minutes=as.numeric(substr(velo$duree,4,5))
    velo$heures=as.numeric(substr(velo$duree,1,2))
    velo$tps=(velo$heures*60+velo$minutes)/60
    velo$date2=as.Date(substr(velo$date,1,10))
    #velo=velo %>% distinct(date2,.keep_all = T)
    
    #création des var semaine, mois et année
    velo$semaine=format(velo$date2,format="%U")
    #en r semaine commence le dimanche (jour=1) -> réaffecter le dimanche à la semaine précédente
    velo$semaine=as.integer(velo$semaine)
    velo$semaine=ifelse(wday(velo$date2)==1,velo$semaine-1,velo$semaine)
    velo$semaine=ifelse(velo$semaine<10,paste0("0",velo$semaine),paste0(velo$semaine))
    
    velo$annee=format(velo$date2,format="%Y")
    velo$mois=format(velo$date2,format="%m")
    velo$puissance=as.numeric(velo$puissance)
    
    #correction Dplus
    velo$dplus=ifelse(velo$dplus-floor(velo$dplus)==0,velo$dplus,
                      velo$dplus*1000)
    
    velo=rbind(velo_0,velo) %>% mutate(dist0=round(dist/10,0)*10) %>% 
      distinct(type,date,dist0,.keep_all = T) %>% select(-dist0)
    
    ## sauvegarde 
    saveRDS(velo,"./bases/velo.RDS")
  }else{
    print("Aucun fichier de données nouveau")
  }
}


base_stat=function(filtre_an){
  
    # filtre période et activités
    velo=velo %>% filter(annee>filtre_an)
   
   
    #stat par période
    Getstat=function(per){
      s=which(colnames(velo) == per)
      names(velo)[s]="periode"
        stat1=velo %>% 
          group_by(annee,periode) %>% summarise(
          dplus=sum(dplus,na.rm=T),dist=sum(dist,na.rm=T),heures=sum(heures,na.rm=T),
          minutes=sum(minutes,na.rm=T))
        stat1$heures=ifelse(stat1$minutes>59,stat1$heures+floor(stat1$minutes/60),stat1$heures)
        stat1$minutes=ifelse(stat1$minutes>59,stat1$minutes-floor(stat1$minutes/60)*60,stat1$minutes)
        names(stat1)[2]=per
    return(stat1)
    }
    
    ## pour la puissance moyenne pondérée donc filtre supp
    GetstaPuis=function(per){
      s=which(colnames(velo) == per)
      names(velo)[s]="periode"
      stat1=velo %>% filter(annee>"2020" & !is.na(dist)) %>% 
        group_by(annee,periode) %>% summarise(
          puis=weighted.mean(puissance,dist,na.rm=T),
          cad=weighted.mean(cadence,dist,na.rm = T)
        )
      names(stat1)[2]=per
      return(stat1)
    }
    
    puisSem=GetstaPuis("semaine")
    statSem=Getstat("semaine") %>% left_join(puisSem,by=c("annee","semaine"))
    
    puisMois=GetstaPuis("mois") %>% rename("puis_M"=puis,"cad_M"=cad)
    statMois=Getstat("mois") %>% rename("dplus_M"=dplus,"dist_M"=dist,"heures_M"=heures,"minutes_M"=minutes) %>% 
      left_join(puisMois,by=c("annee","mois"))
    
    
    ##lien semaine & mois --- attention semaine sur 2 mois
    ## boucle sur les années
    lien=data.frame()
    t=0
    
      for (an in unique(velo$annee)) {
        t=t+1
        lie=velo %>% filter(annee==an) %>% select(annee,semaine,mois) %>% unique()
        #repérer les semaines sur 2 mois
        a1=lie %>% group_by(annee,semaine) %>% summarise(n=n()) %>% filter(n==2)
        lie=lie %>% left_join(a1,by=c("annee","semaine"))
        #affecter le 1er mois
        for (m in a1$semaine) {
          min(lie[lie$semaine==m,]$mois)
          lie$mois=ifelse(lie$semaine==m,min(lie[lie$semaine==m,]$mois),lie$mois)
        }
        lie=lie %>% select(-n) %>% unique()
        if (t==1){
          lien=lie
        }else {
          lien=rbind(lien,lie)
        }
       # return(lie)
    }
    
    
    statSem=statSem %>% left_join(lien,by=c("annee","semaine"))
    statSem=statSem %>% left_join(statMois,by=c("annee","mois"))
    
    #durée en min
    statSem$tps=(statSem$heures*60+statSem$minutes)/60
    statSem$tps_M=(statSem$heures_M*60+statSem$minutes_M)/60
    #durée en hm
    statSem$minutes=ifelse(nchar(statSem$minutes)==1,paste0("0",statSem$minutes),statSem$minutes)
    statSem$minutes_M=ifelse(nchar(statSem$minutes_M)==1,paste0("0",statSem$minutes_M),statSem$minutes_M)
    statSem$duree=paste0(statSem$heures,"h",statSem$minutes)
    statSem$duree_M=paste0(statSem$heures_M,"h",statSem$minutes_M)
    
    
    ## graph résultat
    
    statSem=statSem %>% select(mois,semaine,annee,dplus,dist,duree,tps,puis,cad,
                               dplus_M,dist_M,duree_M,tps_M,puis_M,cad_M,heures,minutes)
    
    ### fichier cumulés
    cumul=statSem %>% group_by(annee) %>% arrange(annee,semaine) %>% 
      mutate(Cumdist = cumsum(dist),
             Cumdplus=cumsum(dplus),
             Cumtps=(cumsum(heures*60)+cumsum(minutes))/60,
             minutes=cumsum(minutes),heures=cumsum(heures))
    
    cumul$heures=ifelse(cumul$minutes>59,cumul$heures+floor(cumul$minutes/60),cumul$heures)
    cumul$minutes=ifelse(cumul$minutes>59,cumul$minutes-floor(cumul$minutes/60)*60,cumul$minutes)
    
    cumul$minutes=ifelse(nchar(cumul$minutes)==1,paste0("0",cumul$minutes),cumul$minutes)
    cumul$Cumduree=paste0(cumul$heures,"h",cumul$minutes)

  ## Ajout des cumuls dans le fichiers statsem
   
     statSem=statSem %>% inner_join(cumul %>% select(semaine,annee,Cumdist,Cumduree,Cumtps,Cumdplus),
                                    by=c("annee","semaine"))
    
  #return(list("statsem"=statSem,"cumul"=cumul))
     return(statSem)
  
}
### graph
getGraphMois=function(df,donn){
  s=which(colnames(df) == donn)
  names(df)[s]="var"
  gm=ggplot(data=df, aes(x=mois, y=var, fill=annee)) +
    geom_bar(stat="identity", position=position_dodge()) +
    labs(y = donn) +
    geom_text(aes(label = round(var,1)),
              vjust = 1.5, position = position_dodge(.9),
              colour = "black",size = 3)
  return(gm)
}


getGraph=function(df,sem,donn){
  bbas=df %>% filter(semaine<=sem)
  s=which(colnames(bbas) == donn)
  names(bbas)[s]="var"
  gs=ggplot(data=bbas, aes(x=semaine, y=var, fill=annee)) +
    geom_bar(stat="identity", position=position_dodge()) +
    labs(y = donn) #+
    #geom_text(aes(label = round(var,1)),
              #vjust = 1.5, position = position_dodge(.9),
              #colour = "black",size = 3)
  return(gs)
}

NuagePoints=function(filtre="annee",seuil=-1,var1,var2){
  names(velo)[which(colnames(velo) == var1)]="var1"
  names(velo)[which(colnames(velo) == var2)]="var2"
  
  np=ggplot(data=velo %>% filter((!!as.name(filtre)>seuil)), aes(x=var1, y=var2,color=annee)) +
    geom_point() +
    labs(x = var1,y = var2) +
    stat_ellipse()
  return(np)
}

GetCourbe=function(filtre1="annee",seuil1=-1,filtre2="dist",seuil2=-1,var1,var2){
  names(velo)[which(colnames(velo) == var1)]="var1"
  names(velo)[which(colnames(velo) == var2)]="var2"
  if(filtre1==var1)
    filtre1="var1"
  if(filtre1==var2)
    filtre1="var2"
  if(filtre2==var1)
    filtre2="var1"
  if(filtre2==var2)
    filtre2="var2"
  
  np=ggplot(data=velo %>% filter((!!as.name(filtre1)>seuil1) & (!!as.name(filtre2)>seuil2)),
            aes(x=var1, y=var2,color=annee)) +
    geom_line() +
    labs(x = var1,y = var2) 
  return(np)
}

getCourbeCum=function(df,sem,donn){
  bbas=df %>% filter(semaine<=sem)
  s=which(colnames(bbas) == donn)
  names(bbas)[s]="var"
    #pour afficher valeur max de l'année
      m23=round(max(bbas[bbas$annee=="2023" & bbas$semaine==m,]$var),0)
      m22=round(max(bbas[bbas$annee=="2022" & bbas$semaine==m,]$var),0)
      m21=round(max(bbas[bbas$annee=="2021" & bbas$semaine==m,]$var),0)
   
    if(donn == "Cumtps"){
       m23lib=max(bbas[bbas$annee=="2023" & bbas$semaine==m,]$Cumduree)
       m22lib=max(bbas[bbas$annee=="2022" & bbas$semaine==m,]$Cumduree)
       m21lib=max(bbas[bbas$annee=="2021" & bbas$semaine==m,]$Cumduree)
    }
    if(donn == "Cumdist"){
      m23lib=paste0(format(m23, big.mark=" ")," Km")
      m22lib=paste0(format(m22, big.mark=" ")," Km")
      m21lib=paste0(format(m21, big.mark=" ")," Km")
    }
      if(donn == "Cumdplus"){
        m23lib=paste0(format(m23, big.mark=" ")," m")
        m22lib=paste0(format(m22, big.mark=" ")," m")
        m21lib=paste0(format(m21, big.mark=" ")," m")
      }
      
  gs=ggplot(data=bbas, aes(x=semaine, y=var, group=annee,color=annee)) +
    geom_line(aes(linewidth = 0.8)) +
    labs(y = donn) + 
    annotate(geom="text", x=3, y=m23, label=paste0("2023 : ",m23lib))+ 
    annotate(geom="text", x=3, y=m23*.95, label=paste0("2022 : ",m22lib))+ 
    annotate(geom="text", x=3, y=m23*.90, label=paste0("2021 : ",m21lib))+
    guides(linewidth = "none") #supprimer la légende de "linewidth"
  return(gs)
}


## nolio
creat_ficNolio=function(){
  #1 charger base
  nolio_0=readRDS("./bases/nolio.RDS")
  #2 ajouter nouveaux CSV
  liste <- list.files(path="./bases/nolio",pattern="*.csv")
  if(length(liste)!=0) {
      for (k in 1:length(liste)) {
        a <- read.csv2(paste0("./bases/nolio/",liste[k]),as.is = T,sep=";",
                       encoding = "UTF-8")
        if(k==1){
          nolio=a
        }else{
          nolio=rbind(nolio,a)
        }
        #renomer CSV
        #file.rename(from = paste0(rep,liste[k]), to = paste0(rep,liste[k],"0"))
        file.copy(from = paste0("./bases/nolio/",liste[k]), to = paste0("./bases/archiveCSV/",Sys.Date(),liste[k]))
        file.remove(paste0("./bases/nolio/",liste[k]))
      } 
      
      
      
      names(nolio)=c("date","fatigue","condPhysiq","forme","chargeCoggan")
      #Elimination des doublons
      nolio=nolio %>% distinct(date,chargeCoggan,.keep_all = T)
      
      
      nolio$date2=as.Date(substr(nolio$date,1,10))
      nolio$annee=format(nolio$date2,format="%Y")
      
      nolio=rbind(nolio_0 %>% select(-c(sm_chargeCoggan,sm_fatigue,sm_condPhysiq,sm_forme)),nolio)  %>% distinct(date,chargeCoggan,.keep_all = T)
      
      ##lissage des données
    
      nolio$sm_chargeCoggan=ets(nolio$chargeCoggan,na.action = "na.interp")$fitted
      nolio$sm_fatigue=ets(nolio$fatigue,na.action = "na.interp")$fitted
      nolio$sm_condPhysiq=ets(nolio$condPhysiq,na.action = "na.interp")$fitted
      nolio$sm_forme=ets(nolio$forme,na.action = "na.interp")$fitted
      ## sauvegarde 
      saveRDS(nolio,"./bases/nolio.RDS")
  }else{
    print("Aucun fichier de données nouveau")
  }
}


## grap nolio
graph_nolio=function(){
ggplot(data=nolio,aes(x=date2)) +
  geom_line(aes(y=sm_chargeCoggan,color="sm_chargeCoggan")) +
  geom_line(aes(y=sm_forme,color="sm_forme")) +
  geom_line(aes(y=sm_condPhysiq,color="sm_condPhysiq")) +
  geom_line(aes(y=sm_fatigue,color="sm_fatigue")) +
  scale_color_manual(values = c("darkred", "steelblue","orange","green"),
                     labels=c("charge Coggan","forme","condition physique","fatigue"),
                     name="")
}
