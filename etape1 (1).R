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