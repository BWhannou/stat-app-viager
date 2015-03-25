
library(foreign)
dataset2 = read.dta("//paradis/eleves/ATOULLET/Stat'app/Donnees/avecydeathmeandep(2).dta")
#dataset2=read.dta("C:/Users/Alexis/Documents/ENSAE/Stat'app/avecydeathmeandep(2).dta")

library(UsingR)
library(nlme)
library(BioStatR)

summary(dataset2)

#Suppression du 274ième individu
dataset2<-dataset2[-274,]

attach(dataset2)

#Modification des données pour le 264ième individu
T1ydeath[264]<-2005
T1mdeath[264]<-04
T1ddeath[264]<-24
T1dayoflife[264]<-34257
T1deathdate[264]<-16550
ecartydeath[264]<- -13648.51
ecartddeath[264]<- 4058.4933               
age_at_death[264]<-94

#attach(dataset2)
n=length(dataset2)
 
#simple.hist.and.boxplot(rel_annuity,20,main="Distribution de la rente relative")

#Création de l'indicatrice Homme 

sexe<-NULL
sexe[T1sex=="1"]<-1
sexe[T1sex=="2"]<-0

#Création de l'indicatrice Homme 
#sexev1<-rep(1,564)
#sexev1[male1=="First head is female"]<-0

#On prend r=0.05
#alpha<-(0.05*rel_downp+rel_annuity)/(0.05+rel_annuity)
#summary(alpha)

#Création de l'indicatrice LIBRE

Libre<-NULL
Libre[libre=="Free"]<-1
Libre[libre=="Occupied"]<-0

#Création de l'indicatrice Homme2 (tête 2)

sexe2<-NULL
sexe2[male2=="Second head is male"]<-1
sexe2[male2=="Second head is female"]<-0

#Création de l'indicatrice Paris
Paris<-NULL
Paris[bien_paris=="Located in Paris"]<-1
Paris[bien_paris=="Not located in Paris"]<-0

#Création de la durée de vie résiduelle
Dateact = as.factor(dataset2[,37])
datecontrat=as.Date(Dateact,"%Y-%m-%d")-as.Date(0,origin="1960-01-01")
datecontrat=as.numeric(datecontrat)
dataset2[,154]=datecontrat
names(dataset2)[154]="datecontrat"
Delta_age_jour<-T1deathdate-datecontrat
age_at_viager_tete1<-floor((datecontrat-T1datebirthday)/365)

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

#Region de naissance
Region_naissance<-NULL
for (i in 1:m) {
if (Paris_n[i]==1){Region_naissance[i]<-"Paris"}
if (couronne_n[i]==1){Region_naissance[i]<-"Couronne"}
if (Nord_Ouest[i]==1){Region_naissance[i]<-"Nord-Ouest"}
if (Ouest[i]==1){Region_naissance[i]<-"Ouest"}
if (Sud[i]==1){Region_naissance[i]<-"Sudt"}
if (Est[i]==1){Region_naissance[i]<-"Est"}
if (Sud[i]==1){Region_naissance[i]<-"Sud"}
if (Outre_Mer[i]==1){Region_naissance[i]<-"Outre-Mer"}
}

#Region de décès
Region_deces<-NULL
for (i in 1:p) {
if (Paris_d[i]==1){Region_deces[i]<-"Paris"}
if (couronne_d[i]==1){Region_deces[i]<-"Couronne"}
if (Nord_Ouest_d[i]==1){Region_deces[i]<-"Nord-Ouest"}
if (Ouest_d[i]==1){Region_deces[i]<-"Ouest"}
if (Sud_d[i]==1){Region_deces[i]<-"Sud"}
if (Est_d[i]==1){Region_deces[i]<-"Est"}
if (Etranger[i]==1){Region_deces[i]<-"Etranger"}
}

##Idf_deces
Idf_deces<-rep(0,564)
Idf_deces[which(Region_deces=="Paris")]<-1
Idf_deces[which(Region_deces=="Couronne")]<-1

#Tete1
#Création d'une indicatrice qui vaut 1 quand la tête 1 est une tierce personne
Tete_1_ind=rep(1,p)
for (i in 1:p) {
if (Tete_1[i]=="Vendeur 1"){Tete_1_ind[i]<-0}
if (Tete_1[i]=="Vendeur 2"){Tete_1_ind[i]<-0}
}


#Indicatrice ecartddeath positif
ecartddeath_positif<-rep(0,length(ecartddeath))
for (i in 1:length(ecartddeath)){
if (ecartddeath[i]>0){ecartddeath_positif[i]<-1}
}

#Indicatrice de mobilité
Ind_mob<-rep(1,p)
for (i in 1:p){
if (depdeath[i]==T1depbirth[i]){Ind_mob[i]<-0}
}


#Nouvelle région de décès
Paris_deces<-rep(0,length(depdeath))
Petite_couronne_deces<-rep(0,length(depdeath))
Grande_couronne_deces<-rep(0,length(depdeath))
Reste_du_monde_deces<-rep(0,length(depdeath))

for (i in 1:length(depdeath)){
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


New_Region_deces<-NULL
for (i in 1:p) {
if (Paris_deces[i]==1){New_Region_deces[i]<-"Paris"}
if (Petite_couronne_deces[i]==1){New_Region_deces[i]<-"Petite couronne"}
if (Grande_couronne_deces[i]==1){New_Region_deces[i]<-"Grande couronne"}
if (Reste_du_monde_deces[i]==1){New_Region_deces[i]<-"Reste du monde"}
}

#Nouvelle région de naissance
IdF_naissance<-rep(0,length(T1depbirth))
Reste_du_monde_naissance<-rep(0,length(T1depbirth))

for (i in 1:length(T1depbirth)){
if (Paris_n[i]==1){IdF_naissance[i]<-1}
if (couronne_n[i]==1){IdF_naissance[i]<-1}
if (Nord_Ouest[i]==1){Reste_du_monde_naissance[i]<-1}
if (Ouest[i]==1){Reste_du_monde_naissance[i]<-1}
if (Sud[i]==1){Reste_du_monde_naissance[i]<-1}
if (Est[i]==1){Reste_du_monde_naissance[i]<-1}
if (Outre_Mer[i]==1){Reste_du_monde_naissance[i]<-1}
}
New_Region_naissance<-NULL
for (i in 1:length(T1depbirth)) {
if (IdF_naissance[i]==1){New_Region_naissance[i]<-"Ile-de-France"}
if (Reste_du_monde_naissance[i]==1){New_Region_naissance[i]<-"Reste du monde"}
}

#logit2<-glm(ecartddeath_positif~T1ybirth+sexe+Ind_mob_region+IdF_naissance,family=binomial)
#summary(logit2)
#exp(coef(logit2)) #odd ratio
#exp(cbind(OR = coef(logit2), confint(logit2)))
#Proba_2<-1/(1+exp(-coef(logit2)))

#Plot régression logit
#plot(age_at_death,ecartddeath_positif,xlab="age_at_death",ylab="ecartddeath")
#g<-glm(ecartddeath_positif~age_at_death,family=binomial) 
#curve(predict(g,data.frame(age_at_death=x),type="resp"),add=TRUE) 
#points(age_at_death,fitted(g),pch=20) 

##Régression précise

Tete=rep(0,p)
for (i in 1:p) {
if (Tete_1[i]=="Vendeur 1"){Tete[i]<-1}
if (Tete_1[i]=="Vendeur 2"){Tete[i]<-1}
}

#Qu'est-ce qui influence l'âge de décès ?

vl_mil_ind<-vl_mil*Tete
annuity_ind<-annuity*Tete
downp_ind<-downp*Tete

#logit4<-glm(ecartddeath_positif~T1ybirth+sexe+Ind_mob_region+Paris_deces+Petite_couronne_deces+Grande_couronne_deces,family=binomial)
#summary(logit4)
#exp(cbind(OR = coef(logit4), confint(logit4)))
#Proba_4<-1/(1+exp(-coef(logit4)))

#Impact du temps
Date_contrat=as.Date(Dateact,"%Y-%m-%d")
annee_contrat=format(Date_contrat, format = "%Y") 
annee_contrat<-as.numeric(annee_contrat)

#Indicatrice annee_naissance
q<-length(T1ybirth)
naissance_avant_06<-rep(0,q)
naissance_06_11<-rep(0,q)
naissance_11_17<-rep(0,q)
naissance_apres_17<-rep(1,q)
for (i in 1:q){
if (T1ybirth[i]-1905<=0){naissance_avant_06[i]<-1}
}
for (i in 1:q){
if (T1ybirth[i]-1906>=0){naissance_06_11[i]<-1}
if (T1ybirth[i]-1911>=0){naissance_06_11[i]<-0}
}
for (i in 1:q){
if (T1ybirth[i]-1911>=0){naissance_11_17[i]<-1}
if (T1ybirth[i]-1917>=0){naissance_11_17[i]<-0}
if (T1ybirth[i]==1916){naissance_11_17[i]<-1}
}
for (i in 1:q){
if (T1ybirth[i]-1916<=0){naissance_apres_17[i]<-0}
}

annee_naissance<-NULL
for (i in 1:q){
if (naissance_avant_06[i]==1){annee_naissance[i]<-"Avant 1906"}
if (naissance_06_11[i]==1){annee_naissance[i]<-"Entre 1906 et 1911"}
if (naissance_11_17[i]==1){annee_naissance[i]<-"Entre 1911 et 1917"}
if (naissance_apres_17[i]==1){annee_naissance[i]<-"Apres 1917"}
}


#Indicatrice annee_naissance
q<-length(T1ydeath)
mort_avant_94<-rep(0,q)
mort_94_00<-rep(0,q)
mort_00_04<-rep(0,q)
mort_apres_04<-rep(1,q)

for (i in 1:q){
if (T1ydeath[i]-1993<=0){mort_avant_94[i]<-1}
}
for (i in 1:q){
if (T1ydeath[i]-1994>=0){mort_94_00[i]<-1}
if (T1ydeath[i]-2000>=0){mort_94_00[i]<-0}
}
for (i in 1:q){
if (T1ydeath[i]-2000>=0){mort_00_04[i]<-1}
if (T1ydeath[i]-2004>=0){mort_00_04[i]<-0}
if (T1ydeath[i]==2003){mort_00_04[i]<-1}
}
for (i in 1:q){
if (T1ydeath[i]-2003<=0){mort_apres_04[i]<-0}
}

annee_deces<-NULL
for (i in 1:q){
if (mort_avant_94[i]==1){annee_deces[i]<-"Avant 1994"}
if (mort_94_00[i]==1){annee_deces[i]<-"Entre 1994 et 2000"}
if (mort_00_04[i]==1){annee_deces[i]<-"Entre 2000 et 2004"}
if (mort_apres_04[i]==1){annee_deces[i]<-"Apres 2004"}
}


#Je dédouble la variable pour me débarasser des NA remplacer par l'annéee 2015
annee_contrat_bis<-annee_contrat
annee_contrat_bis[96]<-2015
annee_contrat_bis[124]<-2015
annee_contrat_bis[189]<-2015
annee_contrat_bis[319]<-2015
annee_contrat_bis[556]<-2015
annee_contrat_bis[349]<-2015
annee_contrat_bis[439]<-2015
annee_contrat_bis[514]<-2015
annee_contrat_bis[539]<-2015
annee_contrat_bis[542]<-2015

#Indicatrice annee_viager
q=length(annee_contrat)
annee_viager<-NULL

contrat_avant_84<-rep(0,q)
contrat_84_88<-rep(0,q)
contrat_88_92<-rep(0,q)
contrat_apres_92<-rep(1,q)
for (i in 1:q){
if (annee_contrat_bis[i]-1983<=0){contrat_avant_84[i]<-1}
}
for (i in 1:q){
if (annee_contrat_bis[i]-1984>=0){contrat_84_88[i]<-1}
if (annee_contrat_bis[i]-1988>=0){contrat_84_88[i]<-0}
}
for (i in 1:q){
if (annee_contrat_bis[i]-1988>=0){contrat_88_92[i]<-1}
if (annee_contrat_bis[i]-1992>=0){contrat_88_92[i]<-0}
if (annee_contrat_bis[i]==1991){contrat_88_92[i]<-1}
}
for (i in 1:q){
if (annee_contrat_bis[i]-1991<=0){contrat_apres_92[i]<-0}
if (annee_contrat_bis[i]==2015){contrat_apres_92[i]<-0}
}
for (i in 1:q){
if (contrat_avant_84[i]==1){annee_viager[i]<-"Avant 1984"}
if (contrat_84_88[i]==1){annee_viager[i]<-"Entre 1984 et 1988"}
if (contrat_88_92[i]==1){annee_viager[i]<-"Entre 1988 et 1992"}
if (contrat_apres_92[i]==1){annee_viager[i]<-"Apres 1992"}
}

#Indicatrice Delta

#Je dédouble la variable pour me débarasser des NA remplacer par l'annéee 2015
Delta_bis<-Delta_age_jour
Delta_bis[96]<-9534
Delta_bis[124]<-9534
Delta_bis[189]<-9534
Delta_bis[319]<-9534
Delta_bis[556]<-9534
Delta_bis[349]<-9534
Delta_bis[439]<-9534
Delta_bis[514]<-9534
Delta_bis[539]<-9534
Delta_bis[542]<-9534

q<-length(Delta_bis)
Delta_moins_1803<-rep(0,q)
Delta_1803_3490<-rep(0,q)
Delta_3490_5293<-rep(0,q)
Delta_plus_5293<-rep(1,q)

for (i in 1:q){
if (Delta_bis[i]-1802<=0){Delta_moins_1803[i]<-1}
}
for (i in 1:q){
if (Delta_bis[i]-1803>=0){Delta_1803_3490[i]<-1}
if (Delta_bis[i]-3490>=0){Delta_1803_3490[i]<-0}
}
for (i in 1:q){
if (Delta_bis[i]-3490>=0){Delta_3490_5293[i]<-1}
if (Delta_bis[i]-5294>=0){Delta_3490_5293[i]<-0}
}
for (i in 1:q){
if (Delta_bis[i]-5293<=0){Delta_plus_5293[i]<-0}
}

q<-length(Delta_moins_1803)
Vie_residuelle<-NULL
for (i in 1:q){
if (Delta_moins_1803[i]==1){Vie_residuelle[i]<-"Au plus 1803j"}
if (Delta_1803_3490[i]==1){Vie_residuelle[i]<-"Entre 1803j et 3490j"}
if (Delta_3490_5293[i]==1){Vie_residuelle[i]<-"Entre 3490j et 5293j"}
if (Delta_plus_5293[i]==1){Vie_residuelle[i]<-"Au moins 5293j"}
}

#Indicatrice ecart 

q<-length(ecartddeath)
Ecart_moins_1084<-rep(0,q)
Ecart_1084_0<-rep(0,q)
Ecart_0_1059<-rep(0,q)
Ecart_1059_2225<-rep(0,q)
Ecart_plus_2225<-rep(1,q)

for (i in 1:q){
if (ecartddeath[i]+1083<=0){Ecart_moins_1084[i]<-1}
}
for (i in 1:q){
if (ecartddeath[i]+1084>=0){Ecart_1084_0[i]<-1}
if (ecartddeath[i]>=0){Ecart_1084_0[i]<-0}
}
for (i in 1:q){
if (ecartddeath[i]>=0){Ecart_0_1059[i]<-1}
if (ecartddeath[i]-1060>=0){Ecart_0_1059[i]<-0}
}
for (i in 1:q){
if (ecartddeath[i]>=1059){Ecart_1059_2225[i]<-1}
if (ecartddeath[i]-2226>=0){Ecart_1059_2225[i]<-0}
}
for (i in 1:q){
if (ecartddeath[i]-2225<=0){Ecart_plus_2225[i]<-0}
}

q<-length(ecartddeath)
Ecart<-NULL
for (i in 1:q){
if (Ecart_moins_1084[i]==1){Ecart[i]<-"Au plus -1084j"}
if (Ecart_1084_0[i]==1){Ecart[i]<-"Entre -1084j et 0j"}
if (Ecart_0_1059[i]==1){Ecart[i]<-"Entre 0j et 1059j"}
if (Ecart_1059_2225[i]==1){Ecart[i]<-"Entre 1059j et 2225j"}
if (Ecart_plus_2225[i]==1){Ecart[i]<-"Au moins 2225j"}
}

#Age de décès : quartile

C_age_deces<-cut(age_at_death, breaks=quantile(age_at_death))
C_age_deces[336]<-"(82,88]"
###################################FACTOMINER#############################################
######Commission

Commission_<-Commission
Commission_[352]="4573.47 euros"
Commission_[453]="3049.95 euros"
Commission_[394]="NA NA"
Commission_[468]="NA NA"
Commission_[56]="NA NA"

length(Commission_)
a=unlist(strsplit(Commission_, ' '))
#Problématique : 902 903 904 905 puis 701 702 703 704
#a<-a[-903]

unite=NULL
valeur=NULL
for (i in 1:564)
{
	valeur[i]=a[2*i-1]
	unite[i]=a[2*i]
}

valeur_com=as.numeric(valeur)
for (i in 1:564)
{
	if (unite[i]=="francs" || unite[i]=="Francs")
	{valeur_com[i]=valeur_com[i]/6.55957
	unite[i]="euros"}
}

lin_vl<-lm(vl_mil~valeur_com+Paris)


###type
###En raison d'effectifs trop faibles, on rassemble v1t2 et v1t2 et d'autre part t1 et t1t2 
type[which(type=="v1t2")]<-"v1vt2"
type[which(type=="v1v2")]<-"v1vt2"
type[which(type=="t1t2")]<-"t1t"
type[which(type=="t1")]<-"t1t"
type[which(type=="v2")]<-"v1"

library(FactoMineR)

#################Prédiction de vl_mil à partir de la régression linéaire lin_vl
plot(vl_mil_pr ~valeur_com,main="Valeur du bien (exacte ou prédite) en fonction de la commission")
#abline(lin_vl, col='red')
abline(lm(vl_mil ~valeur_com),col="blue")

ind_vl_mil<-rep(0,564)
ind_vl_mil[which(is.na(vl_mil))]<-1

vl_mil_pr<-vl_mil
for (i in 1:564){
if (ind_vl_mil[i]==1){vl_mil_pr[i]<-lin_vl$coeff[1]+lin_vl$coeff[2]*valeur_com[i]+lin_vl$coeff[3]*Paris[i]}
}

#which(is.na(valeur_com))  : 56 394 468
vl_mil_pr[56]<-lin_vl$coeff[1]+lin_vl$coeff[3]*Paris[56]
vl_mil_pr[394]<-lin_vl$coeff[1]+lin_vl$coeff[3]*Paris[394]
vl_mil_pr[468]<-lin_vl$coeff[1]+lin_vl$coeff[3]*Paris[468]



#which(vl_mil_pr<=0) : 56 107 394 468 

C_vl_mil_pr<-NULL
for (i in 1:564){
if (vl_mil_pr[i]>0 && vl_mil_pr[i]<75.84){C_vl_mil_pr[i]<-"<75.84"}
if (vl_mil_pr[i]>=75.84 && vl_mil_pr[i]<128.80){C_vl_mil_pr[i]<-"<128.80"}
if (vl_mil_pr[i]>=128.80 && vl_mil_pr[i]<216.50){C_vl_mil_pr[i]<-"<216.50"}
if (vl_mil_pr[i]>=216.50){C_vl_mil_pr[i]<-"<1313.00"}
if (vl_mil_pr[i]<=0){C_vl_mil_pr[i]<-"<75.84"}
}


######Intervalle de confiance
Variance<-rep(0,564)
####Je saute les trois individus dont la commission n'est pas renseigné (on fait ce qu'on peut)
for (i in 1:55){
com<-valeur_com-valeur_com[i]
Paris_<-Paris-Paris[i]
reg<-lm(vl_mil~com+Paris_)
Variance[i]<-coef(summary(reg))[1,2]
}
for (i in 57:393){
com<-valeur_com-valeur_com[i]
Paris_<-Paris-Paris[i]
reg<-lm(vl_mil~com+Paris_)
Variance[i]<-coef(summary(reg))[1,2]
}
for (i in 395:467){
com<-valeur_com-valeur_com[i]
Paris_<-Paris-Paris[i]
reg<-lm(vl_mil~com+Paris_)
Variance[i]<-coef(summary(reg))[1,2]
}
for (i in 469:564){
com<-valeur_com-valeur_com[i]
Paris_<-Paris-Paris[i]
reg<-lm(vl_mil~com+Paris_)
Variance[i]<-coef(summary(reg))[1,2]
}


Var<-Variance*ind_vl_mil
sigma_2_hat<-(summary(lin_vl)$sigma)**2

Erreur_standard<-rep(0,564)
for (i in 1:564){
Erreur_standard[i]<-sqrt(Var[i]+sigma_2_hat)
}
Erreur_standard<-Erreur_standard*ind_vl_mil

####Dans les régressions on a 202 degrés de liberté
####On construit donc les intervalles de confiance avec une St(202)
t<-qt(0.025, 202, lower.tail = TRUE, log.p = FALSE)
t<-as.numeric(t) ####Très bizarrement, t est négatif

Borne_inf<-rep(0,564)
Borne_sup<-rep(0,564)
for (i in 1:564){
if (ind_vl_mil[i]==1){Borne_inf[i]<-vl_mil_pr[i]+t*Erreur_standard[i]}
if (ind_vl_mil[i]==0){Borne_inf[i]<-vl_mil[i]}
}
for (i in 1:564){
if (ind_vl_mil[i]==1){Borne_sup[i]<-vl_mil_pr[i]-t*Erreur_standard[i]}
if (ind_vl_mil[i]==0){Borne_sup[i]<-vl_mil[i]}
}

rel_annuity_pr<-annuity/(vl_mil_pr*1000)
rel_downp_pr<-downp/(vl_mil_pr*1000)
rel_annuity_pr_inf<-annuity/(Borne_sup*1000)
rel_annuity_pr_sup<-annuity/(Borne_inf*1000)
rel_downp_pr_inf<-downp/(Borne_sup*1000)
rel_downp_pr_sup<-downp/(Borne_inf*1000)


rel_downp_pr[which(rel_downp_pr<0)]<-0
rel_annuity_pr[which(rel_annuity_pr<0)]<-0

#lin_annuity<-gls(rel_annuity~Libre+deuxtete+age_at_viager+Paris_deces+Petite_couronne_deces+Grande_couronne_deces+annee_contrat+vl_mil,na.action=na.omit)
#summary(lin_annuity)

#lin_downp<-gls(rel_downp~Libre+age_at_viager+reprise+vl_mil,na.action=na.omit)
#summary(lin_downp)


##########Régression linéaire pour rel_downp et rel_annuity



#################################Commandes utiles########################"
#Fréquences bivariées
#tab<-table(annee_naissance,Classe_info)
#Profil ligne du tableau 
#ligne_tab<-round(tab/apply(tab,1,sum),2)
#barplot(ligne_tab)



#####Corrélation NA 
#cor(na.omit(valeur_com)[1:min(length(na.omit(valeur_com)),length(na.omit(vl_mil)))],na.omit(vl_mil)[1:min(length(na.omit(valeur_com)),length(na.omit(vl_mil)))])


Idf_deces<-rep(1,564)
Idf_deces[which(New_Region_deces=="Reste du monde")]<-0

gls_age<-gls(age_at_death~age_at_viager+T1ybirth+sexe+Idf_deces+Outre_Mer+couronne_n+Sud+Ouest+Est+Nord_Ouest)
summary(gls_age)


gls_delta<-gls(Delta_age_jour~sexe+quality+valeur_com+Outre_Mer+couronne_n+Sud+Ouest+Est+Nord_Ouest+Paris_deces+Petite_couronne_deces+Grande_couronne_deces,na.action=na.omit)
summary(gls_delta)

gls_ecart<-gls(ecartddeath~sexe+quality+valeur_com+Outre_Mer+couronne_n+Sud+Ouest+Est+Nord_Ouest+Paris_deces+Petite_couronne_deces+Grande_couronne_deces,na.action=na.omit)
summary(gls_ecart)

####Arrondissement
departement<-as.numeric(departement)
code_post<-floor(departement/1000)
code_post
c<-which(code_post!=75)
arrond_<-arrond
arrond_<-as.numeric(arrond_)
arrond_[which(code_post!=75)]<-NA
code_post


###########################Regression polytomique : Classe_Ecart
library(VGAM)
library(MASS)

cbind(annee_contrat,rel_downp_pr,rel_annuity_pr,valeur_com,reprise
,deuxtete,Ind_mob,vl_mil_pr,Reste_du_monde_deces
,Petite_couronne_deces,Grande_couronne_deces
,Outre_Mer,couronne_n,Sud,Ouest,Est,Nord_Ouest,Paris,Mettres_carres,Nb_pieces
,Delta_age_jour,age_buyer,annuity,downp)->dataset_ecart
dataset_ecart<-as.data.frame(dataset_ecart)

Ecart_ord<-NULL
Ecart_ord[which(Ecart=="Au plus -1084j")]<-1
Ecart_ord[which(Ecart=="Entre -1084j et 0j")]<-2
Ecart_ord[which(Ecart=="Entre 0j et 1059j")]<-3
Ecart_ord[which(Ecart=="Entre 1059j et 2225j")]<-4
Ecart_ord[which(Ecart=="Au moins 2225j")]<-5

modele <- vglm(Ecart_ord ~ ., data = dataset_ecart, family = acat())
summary(modele)

polr(Ecart~., data=dataset_ecart,
     contrasts = NULL, Hess = FALSE, model = TRUE,
     method = c("logistic"))


#####Polytomique cumulatif
#modele_cum <- vglm(Ecart ~., data = dataset_ecart, family = cumulative(parallel=TRUE,reverse=TRUE))
#summary(modele_cum)



######Test sur les régressions vl_mil_pr
##http://www.statmethods.net/stats/rdiagnostics.html

library(gmodels)
CrossTable(Classe_info_0,Classe_info_pr)

library(car)
ncvTest(lin_delta) ##On peut rejeter aux seuils de confiance usuels l'hypothèse d'homoscedasticité


sresid <- studres(lin_vl) 
hist(sresid, freq=FALSE, 
   main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

#aggregate(age_at_death~New_Region_naissance,dataset2,sd)

#cbind(age_at_death,sexe,Libre,T1ybirth,downp,annuity,valeur_com,vl_mil_pr,Ind_mob,Paris,age_at_viager,annee_contrat,deuxtete,deuxtete)->foutoir
#foutoir<-as.data.frame(foutoir)
#condes(foutoir,1,weights=NULL,proba = 0.1)

eta2( age_at_death,Region_naissance)
rcorr(as.matrix(dataset_ecart))
