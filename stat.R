library(dplyr)
library(ggplot2)
library(lubridate)


## chargement des fichiers
rep="C:/Users/N0LIR9/Documents/David/cyclisme/stat/bases/"

creat_fic=function(){
  #1 charger base
  velo_0=readRDS(paste0(rep,"velo.RDS"))
  #2 ajouter nouveaux CSV
  liste <- list.files(path=rep,pattern="*.csv")
    
    for (k in 1:length(liste)) {
      a <- read.csv2(paste0(rep,liste[k]),as.is = T,sep=",",
                                   encoding = "UTF-8")
      if(k==1){
        velo=a
      }else{
        velo=rbind(velo,a)
      }
      #renomer CSV
      #file.rename(from = paste0(rep,liste[k]), to = paste0(rep,liste[k],"0"))
      file.copy(from = paste0(rep,liste[k]), to = paste0(rep,"archiveCSV/",Sys.Date(),liste[k]))
      file.remove(paste0(rep,liste[k]))
    } 
  
    
    velo=velo %>% select(c(1,2,5,6:9,11,13,15))
    names(velo)=c("type","date","dist","calor","duree","fc_moy","fc_max","cadence","vm","dplus")
    #élimination des doublons
    velo=velo %>% distinct(type,date,dist,.keep_all = T)
    velo$dplus=as.numeric(velo$dplus)
    velo=rbind(velo_0,velo) %>% mutate(dist0=round(dist/10,0)*10) %>% 
      distinct(type,date,dist0,.keep_all = T) %>% select(-dist0)
    
    ## sauvegarde 
    saveRDS(velo,paste0(rep,"velo.RDS"))

}
creat_fic()
velo=readRDS(paste0(rep,"velo.RDS"))


#création des var heures et minutes
velo$minutes=as.numeric(substr(velo$duree,4,5))
velo$heures=as.numeric(substr(velo$duree,1,2))

velo$date2=as.Date(substr(velo$date,1,10))
velo=velo %>% distinct(date2,.keep_all = T)
#création des var semaine, mois et année
velo$semaine=format(velo$date2,format="%U")
#en r semaine commence le dimanche (jour=1) -> réaffecter le dimanche à la semaine précédente
    velo$semaine=as.integer(velo$semaine)
    velo$semaine=ifelse(wday(velo$date2)==1,velo$semaine-1,velo$semaine)
    velo$semaine=ifelse(velo$semaine<10,paste0("0",velo$semaine),paste0(velo$semaine))

velo$annee=format(velo$date2,format="%Y")
velo$mois=format(velo$date2,format="%m")

# filtre période et activités
velo=velo %>% filter(annee>2020)

#stat par période
Getstat=function(per){
  s=which(colnames(velo) == per)
  names(velo)[s]="periode"
    stat1=velo %>% 
      group_by(annee,periode) %>% summarise(
      dplus=sum(dplus,na.rm=T),dist=sum(dist,na.rm=T),heures=sum(heures,na.rm=T),minutes=sum(minutes,na.rm=T)
    )
    stat1$heures=ifelse(stat1$minutes>59,stat1$heures+floor(stat1$minutes/60),stat1$heures)
    stat1$minutes=ifelse(stat1$minutes>59,stat1$minutes-floor(stat1$minutes/60)*60,stat1$minutes)
    names(stat1)[2]=per
return(stat1)
}
statSem=Getstat("semaine")
statMois=Getstat("mois") %>% rename("dplus_M"=dplus,"dist_M"=dist,"heures_M"=heures,"minutes_M"=minutes)


##lien semaine & mois --- attention semaine sur 2 mois
lien=function(an){
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
return(lie)
}
lie21=lien("2021")
lie22=lien("2022")
lie23=lien("2023")
lie=rbind(lie21,lie22,lie23)
statSem=statSem %>% left_join(lie,by=c("annee","semaine"))
statSem=statSem %>% left_join(statMois,by=c("annee","mois"))

#durée en min
statSem$tps=(statSem$heures*60+statSem$minutes)/60
statSem$tps_M=(statSem$heures_M*60+statSem$minutes_M)/60
#durée en hm
statSem$minutes=ifelse(nchar(statSem$minutes)==1,paste0("0",statSem$minutes),statSem$minutes)
statSem$minutes_M=ifelse(nchar(statSem$minutes_M)==1,paste0("0",statSem$minutes_M),statSem$minutes_M)
statSem$duree=paste0(statSem$heures,"h",statSem$minutes)
statSem$duree_M=paste0(statSem$heures_M,"h",statSem$minutes_M)


## a faire tableau

statSem=statSem %>% select(mois,semaine,annee,dplus,dist,duree,tps,dplus_M,dist_M,duree_M,tps_M)

### graph
getGraphMois=function(donn){
  s=which(colnames(statSem) == donn)
  names(statSem)[s]="var"
  ggplot(data=statSem, aes(x=mois, y=var, fill=annee)) +
    geom_bar(stat="identity", position=position_dodge()) +
    labs(y = donn)
}
getGraphMois("tps_M")
getGraphMois("dist_M")


getGraph=function(sem,donn){
  bbas=statSem %>% filter(semaine<sem)
  s=which(colnames(bbas) == donn)
  names(bbas)[s]="var"
  ggplot(data=bbas, aes(x=semaine, y=var, fill=annee)) +
    geom_bar(stat="identity", position=position_dodge()) +
    labs(y = donn)
}

getGraph("06","tps")
getGraph("07","dist")
getGraph("07","dplus")

mean(statSem$duree2)


ggplot(data=statSem, aes(x=semaine, y=as.Date(duree2), fill=annee)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(y = donn)






endur=velo %>% filter(dist>50)
plot(endur$fc_moy,endur$dplus)


library(sf)
test <- st_read("C:/Users/N0LIR9/Downloads/activity_6483456547.gpx",stringsAsFactors = F,layer="tracks")
test2 <- st_read("C:/Users/N0LIR9/Downloads/activity_6483456547.gpx",stringsAsFactors = F,layer="track_points")

st_layers("C:/Users/N0LIR9/Downloads/activity_6483456547.gpx")

plot(st_geometry(test))

library(leaflet)
track2 <- test %>%
  st_combine() %>%
  st_cast(to = "LINESTRING") %>%
  st_sf()
map_leaflet <- leaflet() %>%
  addProviderTiles("OpenStreetMap.France") #Stamen.Watercolor

map_leaflet %>%
  addPolylines(data = track2)

