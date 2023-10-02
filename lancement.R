



source("./src_base.R")
FCseuil=170
    # creat_fic()
    # creat_ficNolio()

velo=readRDS("./bases/velo.RDS")
nolio=readRDS("./bases/nolio.RDS")

statSem=base_stat("2020")
m=max(velo[velo$annee=="2023",]$semaine)

getGraphMois(statSem,"tps_M")
getGraphMois(statSem,"dist_M")
getGraphMois(statSem,"dplus_M")
getGraphMois(statSem,"puis_M")
getGraphMois(statSem,"cad_M")



getGraph(statSem,m,"tps")
getGraph(statSem,m,"dist")
getGraph(statSem,m,"dplus")
getGraph(statSem,m,"puis")
getGraph(statSem,m,"cad")
getGraph(statSem,m,"TSS")


NuagePoints("var2",seuil=120,var1="tps",var2="fc_moy")
NuagePoints("var2",seuil=100,var1="tps",var2="puissance")
NuagePoints("annee",seuil=2021,var1="tps",var2="cadence")
GetCourbe(filtre1="dist",seuil1=30,filtre2="fc_moy",seuil2=120,
          var1="date2",var2="fc_moy")
GetCourbe(filtre1="dist",seuil1=15,filtre2="cadence",seuil2=0,
          var1="date2",var2="cadence")
GetCourbe(filtre1="dist",seuil1=15,filtre2="puissance",seuil2=0,
          var1="date2",var2="puissance")
GetCourbe(filtre1="annee",seuil1=2021,filtre2="TSS",seuil2=0,
          var1="date2",var2="TSS")

getCourbeCum(statSem,m,"Cumdist")
getCourbeCum(statSem,m,"Cumtps")
getCourbeCum(statSem,m,"Cumdplus")
getCourbeCum(statSem,m,"CumTSS")

df=statSem
sem=m
donn="CumTSS"

#Nolio

graph_nolio()

## tableau de bord
rmarkdown::render("./tabBord/dashboard.Rmd",quiet = TRUE)




