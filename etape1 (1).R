##############################################################
################# CHARGEMENT DES DONNEES #####################
##############################################################
library(foreign)
dataset2 = read.dta("C:/Users/David/Documents/ENSAE/2A/Stat'app/Données/avecydeathmeandep(2).dta")

##############################################################
################## OBSERVATION DES DONNEES ###################
##############################################################

n=length(dataset2)
summary(dataset2)
attach(dataset2)

##############################################################
################## DATE DU CONTRAT ###########################
##############################################################

Dateact = as.factor(dataset2[,37]) #on prend les dates des actes


#construction de la date de contrat en num
#la date origine utillisée est 1960-01-01 

datecontrat=as.Date(Dateact,"%Y-%m-%d")-as.Date(0,origin="1960-01-01")
datecontrat=as.numeric(datecontrat)
dataset2[,154]=datecontrat
names(dataset2)[154]="datecontrat"

##############################################################
################## DUREE DE VIE RESIDUELLE ###################
##############################################################
#definition de delta_age
delta_age=T1deathdate - datecontrat
summary(delta_age)
dataset2[,155]=delta_age
names(dataset2)[155]="delta_age"

delta_age_clone=delta_age-ecartddeath 
summary(delta_age_clone)


age_at_viager_clone = (dayoflifemean - delta_age_clone)/365

dataset2<-dataset2[-274,]


T1ydeath[264]<-2005
T1mdeath[264]<-04
T1ddeath[264]<-24
T1dayoflife[264]<-34257
T1deathdate[264]<-16550
ecartydeath[264]<- -13648.51
ecartddeath[264]<- 4058.4933               
age_at_death[264]<-94






#Création des indicatrices Région 
m=length(T1depbirth)
Nord_Ouest<-rep(0,m)
Ouest<-rep(0,m)
Est<-rep(0,m)
Sud<-rep(0,m)
Paris_n<-rep(0,m)
couronne_n<-rep(0,m)
Outre_Mer<-rep(0,m)

for (i in 1:m) {
  if (T1depbirth[i]==97){Outre_Mer[i]<-1}
  if (T1depbirth[i]==75){Paris_n[i]<-1}
  if (T1depbirth[i]==92){couronne_n[i]<-1}
  if (T1depbirth[i]==93){couronne_n[i]<-1}
  if (T1depbirth[i]==94){couronne_n[i]<-1}
  if (T1depbirth[i]==95){couronne_n[i]<-1}
  if (T1depbirth[i]==78){couronne_n[i]<-1}
  if (T1depbirth[i]==91){couronne_n[i]<-1}
  if (T1depbirth[i]==77){couronne_n[i]<-1}
  if (T1depbirth[i]==63){Sud[i]<-1}
  if (T1depbirth[i]==15){Sud[i]<-1}
  if (T1depbirth[i]==43){Sud[i]<-1}
  if (T1depbirth[i]==03){Sud[i]<-1}
  if (T1depbirth[i]==23){Sud[i]<-1}
  if (T1depbirth[i]==87){Sud[i]<-1}
  if (T1depbirth[i]==19){Sud[i]<-1}
  if (T1depbirth[i]==36){Ouest[i]<-1}
  if (T1depbirth[i]==18){Ouest[i]<-1}
  if (T1depbirth[i]==37){Ouest[i]<-1}
  if (T1depbirth[i]==41){Ouest[i]<-1}
  if (T1depbirth[i]==28){Ouest[i]<-1}
  if (T1depbirth[i]==45){Ouest[i]<-1}
  if (T1depbirth[i]==33){Sud[i]<-1}
  if (T1depbirth[i]==40){Sud[i]<-1}
  if (T1depbirth[i]==64){Sud[i]<-1}
  if (T1depbirth[i]==47){Sud[i]<-1}
  if (T1depbirth[i]==24){Sud[i]<-1}
  if (T1depbirth[i]==66){Sud[i]<-1}
  if (T1depbirth[i]==11){Sud[i]<-1}
  if (T1depbirth[i]==34){Sud[i]<-1}
  if (T1depbirth[i]==30){Sud[i]<-1}
  if (T1depbirth[i]==48){Sud[i]<-1}
  if (T1depbirth[i]==09){Sud[i]<-1}
  if (T1depbirth[i]==65){Sud[i]<-1}
  if (T1depbirth[i]==31){Sud[i]<-1}
  if (T1depbirth[i]==32){Sud[i]<-1}
  if (T1depbirth[i]==82){Sud[i]<-1}
  if (T1depbirth[i]==81){Sud[i]<-1}
  if (T1depbirth[i]==12){Sud[i]<-1}
  if (T1depbirth[i]==46){Sud[i]<-1}
  if (T1depbirth[i]==67){Est[i]<-1}
  if (T1depbirth[i]==68){Est[i]<-1}
  if (T1depbirth[i]==89){Est[i]<-1}
  if (T1depbirth[i]==21){Est[i]<-1}
  if (T1depbirth[i]==58){Est[i]<-1}
  if (T1depbirth[i]==71){Est[i]<-1}
  if (T1depbirth[i]==08){Est[i]<-1}
  if (T1depbirth[i]==51){Est[i]<-1}
  if (T1depbirth[i]==10){Est[i]<-1}
  if (T1depbirth[i]==52){Est[i]<-1}
  if (T1depbirth[i]==54){Est[i]<-1}
  if (T1depbirth[i]==55){Est[i]<-1}
  if (T1depbirth[i]==57){Est[i]<-1}
  if (T1depbirth[i]==88){Est[i]<-1}
  if (T1depbirth[i]==70){Est[i]<-1}
  if (T1depbirth[i]==90){Est[i]<-1}
  if (T1depbirth[i]==25){Est[i]<-1}
  if (T1depbirth[i]==39){Est[i]<-1}
  if (T1depbirth[i]==29){Ouest[i]<-1}
  if (T1depbirth[i]==22){Ouest[i]<-1}
  if (T1depbirth[i]==56){Ouest[i]<-1}
  if (T1depbirth[i]==35){Ouest[i]<-1}
  if (T1depbirth[i]==53){Ouest[i]<-1}
  if (T1depbirth[i]==72){Ouest[i]<-1}
  if (T1depbirth[i]==49){Ouest[i]<-1}
  if (T1depbirth[i]==44){Ouest[i]<-1}
  if (T1depbirth[i]==85){Ouest[i]<-1}
  if (T1depbirth[i]==79){Ouest[i]<-1}
  if (T1depbirth[i]==86){Ouest[i]<-1}
  if (T1depbirth[i]==17){Ouest[i]<-1}
  if (T1depbirth[i]==16){Ouest[i]<-1}
  if (T1depbirth[i]==20){Sud[i]<-1}
  if (T1depbirth[i]==1){Sud[i]<-1}
  if (T1depbirth[i]==13){Sud[i]<-1}
  if (T1depbirth[i]==83){Sud[i]<-1}
  if (T1depbirth[i]==84){Sud[i]<-1}
  if (T1depbirth[i]==04){Sud[i]<-1}
  if (T1depbirth[i]==05){Sud[i]<-1}
  if (T1depbirth[i]==06){Sud[i]<-1}
  if (T1depbirth[i]==07){Sud[i]<-1}
  if (T1depbirth[i]==26){Sud[i]<-1}
  if (T1depbirth[i]==42){Sud[i]<-1}
  if (T1depbirth[i]==69){Sud[i]<-1}
  if (T1depbirth[i]==74){Sud[i]<-1}
  if (T1depbirth[i]==38){Sud[i]<-1}
  if (T1depbirth[i]==73){Sud[i]<-1}
  if (T1depbirth[i]==61){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==50){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==14){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==27){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==76){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==59){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==60){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==62){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==80){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==02){Nord_Ouest[i]<-1}
}



#Département et région de décès (2A004 et remplacé par 20004 (Corse))
#159, 196, 313 : individus problématiques
T1codedeath[415]<-20004
T1codedeath=as.numeric(T1codedeath)
depdeath<-floor(T1codedeath/1000)

#Nouvelle région de décès
Paris_birth<-rep(0,length(depdeath))
Petite_couronne_deces<-rep(0,length(depdeath))
Grande_couronne_deces<-rep(0,length(depdeath))
Reste_du_monde_deces<-rep(0,length(depdeath))




p<-length(depdeath)
Nord_Ouest_d<-rep(0,p)
Ouest_d<-rep(0,p)
Est_d<-rep(0,p)
Sud_d<-rep(0,p)
Paris_d<-rep(0,p)
couronne_d<-rep(0,p)
Outre_Mer_d<-rep(0,p)
Etranger<-rep(0,p)

for (i in 1:p) {
  if(!is.na(depdeath[i])){
  
  if (depdeath[i]==99){Etranger[i]<-1}
  if (depdeath[i]==75){Paris_d[i]<-1}
  if (depdeath[i]==92){couronne_d[i]<-1}
  if (depdeath[i]==93){couronne_d[i]<-1}
  if (depdeath[i]==94){couronne_d[i]<-1}
  if (depdeath[i]==95){couronne_d[i]<-1}
  if (depdeath[i]==78){couronne_d[i]<-1}
  if (depdeath[i]==91){couronne_d[i]<-1}
  if (depdeath[i]==77){couronne_d[i]<-1}
  if (depdeath[i]==63){Sud_d[i]<-1}
  if (depdeath[i]==15){Sud_d[i]<-1}
  if (depdeath[i]==43){Sud_d[i]<-1}
  if (depdeath[i]==03){Sud_d[i]<-1}
  if (depdeath[i]==23){Sud_d[i]<-1}
  if (depdeath[i]==87){Sud_d[i]<-1}
  if (depdeath[i]==19){Sud_d[i]<-1}
  if (depdeath[i]==36){Ouest_d[i]<-1}
  if (depdeath[i]==18){Ouest_d[i]<-1}
  if (depdeath[i]==37){Ouest_d[i]<-1}
  if (depdeath[i]==41){Ouest_d[i]<-1}
  if (depdeath[i]==28){Ouest_d[i]<-1}
  if (depdeath[i]==45){Ouest_d[i]<-1}
  if (depdeath[i]==33){Sud_d[i]<-1}
  if (depdeath[i]==40){Sud_d[i]<-1}
  if (depdeath[i]==64){Sud_d[i]<-1}
  if (depdeath[i]==47){Sud_d[i]<-1}
  if (depdeath[i]==24){Sud_d[i]<-1}
  if (depdeath[i]==66){Sud_d[i]<-1}
  if (depdeath[i]==11){Sud_d[i]<-1}
  if (depdeath[i]==34){Sud_d[i]<-1}
  if (depdeath[i]==30){Sud_d[i]<-1}
  if (depdeath[i]==48){Sud_d[i]<-1}
  if (depdeath[i]==09){Sud_d[i]<-1}
  if (depdeath[i]==65){Sud_d[i]<-1}
  if (depdeath[i]==31){Sud_d[i]<-1}
  if (depdeath[i]==32){Sud_d[i]<-1}
  if (depdeath[i]==82){Sud_d[i]<-1}
  if (depdeath[i]==81){Sud_d[i]<-1}
  if (depdeath[i]==12){Sud_d[i]<-1}
  if (depdeath[i]==46){Sud_d[i]<-1}
  if (depdeath[i]==67){Est_d[i]<-1}
  if (depdeath[i]==68){Est_d[i]<-1}
  if (depdeath[i]==89){Est_d[i]<-1}
  if (depdeath[i]==21){Est_d[i]<-1}
  if (depdeath[i]==58){Est_d[i]<-1}
  if (depdeath[i]==71){Est_d[i]<-1}
  if (depdeath[i]==08){Est_d[i]<-1}
  if (depdeath[i]==51){Est_d[i]<-1}
  if (depdeath[i]==10){Est_d[i]<-1}
  if (depdeath[i]==52){Est_d[i]<-1}
  if (depdeath[i]==54){Est_d[i]<-1}
  if (depdeath[i]==55){Est_d[i]<-1}
  if (depdeath[i]==57){Est_d[i]<-1}
  if (depdeath[i]==88){Est_d[i]<-1}
  if (depdeath[i]==70){Est_d[i]<-1}
  if (depdeath[i]==90){Est_d[i]<-1}
  if (depdeath[i]==25){Est_d[i]<-1}
  if (depdeath[i]==39){Est_d[i]<-1}
  if (depdeath[i]==29){Ouest_d[i]<-1}
  if (depdeath[i]==22){Ouest_d[i]<-1}
  if (depdeath[i]==56){Ouest_d[i]<-1}
  if (depdeath[i]==35){Ouest_d[i]<-1}
  if (depdeath[i]==53){Ouest_d[i]<-1}
  if (depdeath[i]==72){Ouest_d[i]<-1}
  if (depdeath[i]==49){Ouest_d[i]<-1}
  if (depdeath[i]==44){Ouest_d[i]<-1}
  if (depdeath[i]==85){Ouest_d[i]<-1}
  if (depdeath[i]==79){Ouest_d[i]<-1}
  if (depdeath[i]==86){Ouest_d[i]<-1}
  if (depdeath[i]==17){Ouest_d[i]<-1}
  if (depdeath[i]==16){Ouest_d[i]<-1}
  if (depdeath[i]==20){Sud_d[i]<-1}
  if (depdeath[i]==1){Sud_d[i]<-1}
  if (depdeath[i]==13){Sud_d[i]<-1}
  if (depdeath[i]==83){Sud_d[i]<-1}
  if (depdeath[i]==84){Sud_d[i]<-1}
  if (depdeath[i]==04){Sud_d[i]<-1}
  if (depdeath[i]==05){Sud_d[i]<-1}
  if (depdeath[i]==06){Sud_d[i]<-1}
  if (depdeath[i]==07){Sud_d[i]<-1}
  if (depdeath[i]==26){Sud_d[i]<-1}
  if (depdeath[i]==42){Sud_d[i]<-1}
  if (depdeath[i]==69){Sud_d[i]<-1}
  if (depdeath[i]==74){Sud_d[i]<-1}
  if (depdeath[i]==38){Sud_d[i]<-1}
  if (depdeath[i]==73){Sud_d[i]<-1}
  if (depdeath[i]==61){Nord_Ouest_d[i]<-1}
  if (depdeath[i]==50){Nord_Ouest_d[i]<-1}
  if (depdeath[i]==14){Nord_Ouest_d[i]<-1}
  if (depdeath[i]==27){Nord_Ouest_d[i]<-1}
  if (depdeath[i]==76){Nord_Ouest_d[i]<-1}
  if (depdeath[i]==59){Nord_Ouest_d[i]<-1}
  if (depdeath[i]==60){Nord_Ouest_d[i]<-1}
  if (depdeath[i]==62){Nord_Ouest_d[i]<-1}
  if (depdeath[i]==80){Nord_Ouest_d[i]<-1}
  if (depdeath[i]==02){Nord_Ouest_d[i]<-1}
  
  }
}


for (i in 1:length(depdeath)){
  if(!is.na(depdeath[i])){
  
  if (depdeath[i]==75){Paris_deces[i]<-1}
  if (depdeath[i]==92){Petite_couronne_deces[i]<-1}
  if (depdeath[i]==93){Petite_couronne_deces[i]<-1}
  if (depdeath[i]==94){Petite_couronne_deces[i]<-1}
  if (depdeath[i]==95){Grande_couronne_deces[i]<-1}
  if (depdeath[i]==78){Grande_couronne_deces[i]<-1}
  if (depdeath[i]==91){Grande_couronne_deces[i]<-1}
  if (depdeath[i]==77){Grande_couronne_deces[i]<-1}
  if (Nord_Ouest_d[i]==1){Reste_du_monde_deces[i]<-1}
  if (Ouest_d[i]==1){Reste_du_monde_deces[i]<-1}
  if (Sud_d[i]==1){Reste_du_monde_deces[i]<-1}
  if (Est_d[i]==1){Reste_du_monde_deces[i]<-1}
  if (Etranger[i]==1){Reste_du_monde_deces[i]<-1}
  
  }
  
}

Ind_Region_deces = NULL
New_Region_deces<-NULL
for (i in 1:p) {
  if (Paris_deces[i]==1){New_Region_deces[i]<-"Paris"}
  if (Petite_couronne_deces[i]==1){New_Region_deces[i]<-"Petite couronne"}
  if (Grande_couronne_deces[i]==1){New_Region_deces[i]<-"Grande couronne"}
  if (Reste_du_monde_deces[i]==1){New_Region_deces[i]<-"Reste du monde"}
  
  if (Paris_deces[i]==1){Ind_Region_deces[i]=0}
  if (Petite_couronne_deces[i]==1){Ind_Region_deces[i]=1}
  if (Grande_couronne_deces[i]==1){Ind_Region_deces[i]=2}
  if (Reste_du_monde_deces[i]==1){Ind_Region_deces[i]=3}
  
}


#Département de naissance
T1codebirth<-as.numeric(T1codebirth)
T1depbirth<-floor(T1codebirth/1000)

#Age à la signature du contrat de la tete
T1age_at_contrat<-annee-T1ybirth

# Nord-Ouest : régions Basse-Normandie (14 50 61 ), Haute-Normandie (27 76), Nord-Pas-de-Calais (59 62) et Picardie (60 80 02).
# Ouest :  Bretagne (29 22 56 35), Pays de la Loire (53 72 49 44 85) et Poitou-Charentes(79 86 17 16).
# Est :  Alsace(67 68), Bourgogne(89 21 58 71), Champagne-Ardenne (08 51 10 52), Lorraine (54 55 57 88) et Franche-Comté (70 90 25 39).
# Sud-Ouest : Aquitaine (33 40 64 47 24), Languedoc-Roussillon (66 11 34 30 48)et Midi-Pyrénées (09 65 31 32 82 81 12 46).
#Sud-Est :  Corse (2A 2B), Provence-Alpes-Côte d'Azur (13 83 84 04 05 06) et Rhône-Alpes(07 26 42 69 01 74 38 73).
#   Auvergne (63 15 43 03), Limousin (23 87 19) et Centre (36 18 37 41 28 45).
# Île-de-France : 75 92 93 94 95 78 91 77 
# Outre-Mers : 971 972 973 974 976


#Nouvelle région de décès
Paris_birth<-rep(0,length(T1depbirth))
Petite_couronne_birth<-rep(0,length(T1depbirth))
Grande_couronne_birth<-rep(0,length(T1depbirth))
Reste_du_monde_birth<-rep(0,length(T1depbirth))



#Création des indicatrices Région 
m=length(T1depbirth)
Nord_Ouest<-rep(0,m)
Ouest<-rep(0,m)
Est<-rep(0,m)
Sud<-rep(0,m)
Paris_n<-rep(0,m)
couronne_n<-rep(0,m)
Outre_Mer<-rep(0,m)

for (i in 1:m) {
  if (T1depbirth[i]==97){Outre_Mer[i]<-1}
  if (T1depbirth[i]==75){Paris_n[i]<-1}
  if (T1depbirth[i]==92){couronne_n[i]<-1}
  if (T1depbirth[i]==93){couronne_n[i]<-1}
  if (T1depbirth[i]==94){couronne_n[i]<-1}
  if (T1depbirth[i]==95){couronne_n[i]<-1}
  if (T1depbirth[i]==78){couronne_n[i]<-1}
  if (T1depbirth[i]==91){couronne_n[i]<-1}
  if (T1depbirth[i]==77){couronne_n[i]<-1}
  if (T1depbirth[i]==63){Sud[i]<-1}
  if (T1depbirth[i]==15){Sud[i]<-1}
  if (T1depbirth[i]==43){Sud[i]<-1}
  if (T1depbirth[i]==03){Sud[i]<-1}
  if (T1depbirth[i]==23){Sud[i]<-1}
  if (T1depbirth[i]==87){Sud[i]<-1}
  if (T1depbirth[i]==19){Sud[i]<-1}
  if (T1depbirth[i]==36){Ouest[i]<-1}
  if (T1depbirth[i]==18){Ouest[i]<-1}
  if (T1depbirth[i]==37){Ouest[i]<-1}
  if (T1depbirth[i]==41){Ouest[i]<-1}
  if (T1depbirth[i]==28){Ouest[i]<-1}
  if (T1depbirth[i]==45){Ouest[i]<-1}
  if (T1depbirth[i]==33){Sud[i]<-1}
  if (T1depbirth[i]==40){Sud[i]<-1}
  if (T1depbirth[i]==64){Sud[i]<-1}
  if (T1depbirth[i]==47){Sud[i]<-1}
  if (T1depbirth[i]==24){Sud[i]<-1}
  if (T1depbirth[i]==66){Sud[i]<-1}
  if (T1depbirth[i]==11){Sud[i]<-1}
  if (T1depbirth[i]==34){Sud[i]<-1}
  if (T1depbirth[i]==30){Sud[i]<-1}
  if (T1depbirth[i]==48){Sud[i]<-1}
  if (T1depbirth[i]==09){Sud[i]<-1}
  if (T1depbirth[i]==65){Sud[i]<-1}
  if (T1depbirth[i]==31){Sud[i]<-1}
  if (T1depbirth[i]==32){Sud[i]<-1}
  if (T1depbirth[i]==82){Sud[i]<-1}
  if (T1depbirth[i]==81){Sud[i]<-1}
  if (T1depbirth[i]==12){Sud[i]<-1}
  if (T1depbirth[i]==46){Sud[i]<-1}
  if (T1depbirth[i]==67){Est[i]<-1}
  if (T1depbirth[i]==68){Est[i]<-1}
  if (T1depbirth[i]==89){Est[i]<-1}
  if (T1depbirth[i]==21){Est[i]<-1}
  if (T1depbirth[i]==58){Est[i]<-1}
  if (T1depbirth[i]==71){Est[i]<-1}
  if (T1depbirth[i]==08){Est[i]<-1}
  if (T1depbirth[i]==51){Est[i]<-1}
  if (T1depbirth[i]==10){Est[i]<-1}
  if (T1depbirth[i]==52){Est[i]<-1}
  if (T1depbirth[i]==54){Est[i]<-1}
  if (T1depbirth[i]==55){Est[i]<-1}
  if (T1depbirth[i]==57){Est[i]<-1}
  if (T1depbirth[i]==88){Est[i]<-1}
  if (T1depbirth[i]==70){Est[i]<-1}
  if (T1depbirth[i]==90){Est[i]<-1}
  if (T1depbirth[i]==25){Est[i]<-1}
  if (T1depbirth[i]==39){Est[i]<-1}
  if (T1depbirth[i]==29){Ouest[i]<-1}
  if (T1depbirth[i]==22){Ouest[i]<-1}
  if (T1depbirth[i]==56){Ouest[i]<-1}
  if (T1depbirth[i]==35){Ouest[i]<-1}
  if (T1depbirth[i]==53){Ouest[i]<-1}
  if (T1depbirth[i]==72){Ouest[i]<-1}
  if (T1depbirth[i]==49){Ouest[i]<-1}
  if (T1depbirth[i]==44){Ouest[i]<-1}
  if (T1depbirth[i]==85){Ouest[i]<-1}
  if (T1depbirth[i]==79){Ouest[i]<-1}
  if (T1depbirth[i]==86){Ouest[i]<-1}
  if (T1depbirth[i]==17){Ouest[i]<-1}
  if (T1depbirth[i]==16){Ouest[i]<-1}
  if (T1depbirth[i]==20){Sud[i]<-1}
  if (T1depbirth[i]==1){Sud[i]<-1}
  if (T1depbirth[i]==13){Sud[i]<-1}
  if (T1depbirth[i]==83){Sud[i]<-1}
  if (T1depbirth[i]==84){Sud[i]<-1}
  if (T1depbirth[i]==04){Sud[i]<-1}
  if (T1depbirth[i]==05){Sud[i]<-1}
  if (T1depbirth[i]==06){Sud[i]<-1}
  if (T1depbirth[i]==07){Sud[i]<-1}
  if (T1depbirth[i]==26){Sud[i]<-1}
  if (T1depbirth[i]==42){Sud[i]<-1}
  if (T1depbirth[i]==69){Sud[i]<-1}
  if (T1depbirth[i]==74){Sud[i]<-1}
  if (T1depbirth[i]==38){Sud[i]<-1}
  if (T1depbirth[i]==73){Sud[i]<-1}
  if (T1depbirth[i]==61){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==50){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==14){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==27){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==76){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==59){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==60){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==62){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==80){Nord_Ouest[i]<-1}
  if (T1depbirth[i]==02){Nord_Ouest[i]<-1}
}

for (i in 1:length(T1depbirth)){
  if(!is.na(T1depbirth[i])){
    
    if (T1depbirth[i]==75){Paris_birth[i]<-1}
    if (T1depbirth[i]==92){Petite_couronne_birth[i]<-1}
    if (T1depbirth[i]==93){Petite_couronne_birth[i]<-1}
    if (T1depbirth[i]==94){Petite_couronne_birth[i]<-1}
    if (T1depbirth[i]==95){Grande_couronne_birth[i]<-1}
    if (T1depbirth[i]==78){Grande_couronne_birth[i]<-1}
    if (T1depbirth[i]==91){Grande_couronne_birth[i]<-1}
    if (T1depbirth[i]==77){Grande_couronne_birth[i]<-1}
    if (Nord_Ouest_d[i]==1){Reste_du_monde_birth[i]<-1}
    if (Ouest_d[i]==1){Reste_du_monde_birth[i]<-1}
    if (Sud_d[i]==1){Reste_du_monde_birth[i]<-1}
    if (Est_d[i]==1){Reste_du_monde_birth[i]<-1}
    if (Etranger[i]==1){Reste_du_monde_birth[i]<-1}
    
  }
  
}


#Nouvelle région den naissance
Ind_Region_birth = NULL

for (i in 1:length(T1depbirth)){
  
  if (Paris_birth[i]==1){Ind_Region_birth[i]=0}
  if (Petite_couronne_birth[i]==1){Ind_Region_birth[i]=1}
  if (Grande_couronne_birth[i]==1){Ind_Region_birth[i]=2}
  if (Reste_du_monde_birth[i]==1){Ind_Region_birth[i]=3}
  
}

#################
## FIN ETAPE 1 ##
#################