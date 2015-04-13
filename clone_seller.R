  library(stats4)  # mle

t_end=21000
papa=NULL
    init0=c(0.1,0.2,0.001,0.5,0.6,0.007) # initialiseur de la fonction mle
##############################################################
################# CHARGEMENT DES DONNEES #####################
##############################################################
library(foreign)
dataset2 = read.dta("C:/Users/Brian/Desktop/statapp/avecydeathmeandep.dta")

##############################################################
################## OBSERVATION DES DONNEES ###################
##############################################################
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


##############################################################
################## constitution des individus seller #########
##############################################################



carac=age_at_viager

length(age_at_viager)
length(delta_age)
contrat=NULL
sex=NULL
resi=NULL
age=NULL
for (i in 1:length(datecontrat))
{
if (!(is.na(datecontrat[i])) &  delta_age[i]>=0 & !(is.na(carac[i])) )
{
#print(i)
contrat=c(contrat,datecontrat[i])
sex=c(sex,T1sex[i])
age=c(age,carac[i])
resi=c(resi,delta_age[i])}
}
newdata=data.frame(residu=resi,age,sex,contrat)


caracteristique=matrix(data=NA,nrow=length(sex),ncol=2)
caracteristique[,1]=sex-1
caracteristique[,2]=age

caracteristique_s=caracteristique
resi_s=resi
##############################################################
################## constitution des individus clone #########
##############################################################



carac=(dayoflifemean-delta_age_clone)/365


contrat=NULL
sex=NULL
resi=NULL
age=NULL
for (i in 1:length(datecontrat))
{
if (!(is.na(datecontrat[i])) &  delta_age_clone[i]>=0 & !(is.na(carac[i])) )
{
#print(i)
contrat=c(contrat,datecontrat[i])
sex=c(sex,T1sex[i])
age=c(age,carac[i])
resi=c(resi,delta_age_clone[i])}
}
newdata=data.frame(residu=resi,age,sex,contrat)


caracteristique=matrix(data=NA,nrow=length(sex),ncol=2)
caracteristique[,1]=sex-1
caracteristique[,2]=age

caracteristique_c=caracteristique
resi_c=resi
##############################################################
################## la log-likelihood #########################
##############################################################

log_like=function (alpha,beta,resi_, carac, contrat_)
{


	Psi0 = function (x)
	{
	# x est un nombre (une date en numérique)
	
	return (x)
	}

	Psi1 = function (d)
	{
	# d est un nombre (date en numérique)
	
	return ( exp(alpha) * d)
	}

	lambda = function (d,x,t)
	{
	res = Psi0(t+d) * Psi1(d) * exp(crossprod(beta , x))
	return (res)
	}

	loglambda = function (d,x,t)
	{
	res = log(Psi0(t+d)) + log(d) + crossprod(beta , x) + alpha
	return (res)
	}

	S=function(d,x,t)
	{
		integral = exp(alpha)*d*d*(0.5*t+(1/3)*d)
		expint = exp(crossprod(beta , x)) * integral
		res = exp(-expint)
		return (res)
	}
	
	logS=function(d,x,t)
	{
		integral = exp(alpha)*d*d*(0.5*t+(1/3)*d)
		expint = exp(crossprod(beta , x)) * integral
		res =-expint
		return (res)
	}
	
	log_density = function (d,x,t)
	{
		res = loglambda(d,x,t) + logS(d,x,t)
		return (res)
	}


	###############################################################
############# contribution du seller

	logcontribution=function(d,x,t)
	{
		if (abs(S(t_end-t,x,t))>10^-7){
		return(log_density(d,x,t)-log(1-S(t_end-t,x,t)))
		}
		else
		{
		return (log_density(d,x,t)+S(t_end-t,x,t))
		}
	}
	
	contrib=NULL
	for (i in 1:length(contrat_))
	{
		contrib[i]=logcontribution(resi_[i],carac[i,],contrat_[i])
	}


	return (sum(contrib,na.rm=TRUE))	


}


##############################################################
################## la minusLogLike ###########################
##############################################################


        Vminuslike=function (alpha_s,beta1_s,beta2_s,alpha_c,beta1_c,beta2_c)
{
  beta_s=c(beta1_s,beta2_s)
  beta_c=c(beta1_c,beta2_c)
	res=-log_like(alpha_s,beta_s,resi_s,caracteristique_s,contrat)- log_like(alpha_c, beta_c, resi_c, caracteristique_c, contrat)
  print(res)
return (res)
}
##############################################################
##################estimation ###########################
##############################################################

	init=init0
	papatry = try(mle(Vminuslike,start=list(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[4],beta1_c=init[5],beta2_c=init[6]),method="BFGS"),silent = T)
	

	nbessais = 0
	nbessais_max = 100
	
	while( (class(papatry)=="try-error") & (nbessais <=nbessais_max) ){

		init[1] = init[1] + ((-1)^(nbessais))*0.1*(nbessais/3)
		init[2] = init[2] + ((-1)^(nbessais))*0.05*(nbessais)
		init[3] = init[3] + ((-1)^(nbessais))* 0.2*(nbessais/3)
		init[4] = init[4] + ((-1)^(nbessais))*0.1*(nbessais/3)
		init[5] = init[5] + ((-1)^(nbessais))*0.05*(nbessais)
		init[6] = init[6] + ((-1)^(nbessais))* 0.2*(nbessais/3)
	print(nbessais)
		papatry = try(mle(Vminuslike,start=list(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[4],beta1_c=init[5],beta2_c=init[6]),method="BFGS"),silent = T)
		nbessais = nbessais+1
	
	}

      papa=mle(Vminuslike,start=list(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[4],beta1_c=init[5],beta2_c=init[6]))  #,method="BFGS"

summary(papa)
