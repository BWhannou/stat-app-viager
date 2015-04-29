
library(stats4)  # mle
t_end=21000
init0=c(1,2,10^-7,5,4*10^-1,3*10^-2,10^-6)  # initialiseur de la fonction mle

papa=NULL
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
################## constitution des individus ###########################
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
caracteristique[,1]=sex
caracteristique[,2]=age


##############################################################
################## la log-likelihood ###########################
##############################################################

log_like=function (alpha,beta,gamma,resi_, carac, contrat_)
{
	lambda0=function(d)
	{	
		res=alpha[1] + alpha[2] * d + alpha[3] * d * d
		#print(res)
		return (res)
	}
	
	intLambda0 = function(d)
	{
		res=alpha[1]*d + 0.5*alpha[2] * d*d + alpha[3] * d * d * d * (1/3)
	}


	lambda = function (d,x,t)
	{
		res = lambda0(d) * exp(crossprod(beta , x) + gamma[1]*t+gamma[2]*t*t)
		return (res)
	}

	loglambda = function (d,x,t)
	{
		res = log(lambda0(d)) + crossprod(beta , x) + gamma[1]*t+gamma[2]*t*t
		return (res)
	}

	S=function(d,x,t)
	{
		int= intLambda0(d)*exp(crossprod(beta , x) + gamma[1]*t+gamma[2]*t*t)
		res= exp(-int)
		return (res)
	}
	
	logS=function(d,x,t)
	{
		res=-exp(crossprod(beta , x) + gamma[1]*t+gamma[2]*t*t)*intLambda0(d)
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
		
		#if (abs(S(t_end-t,x,t))>10^-7){
		return(log_density(d,x,t)-log(1-S(t_end-t,x,t)))
		#}
		#else
		#{
		#return (log_density(d,x,t)+S(t_end-t,x,t))
		#}
	}
	
	contrib=NULL
	for (i in 1:length(contrat_))
	{
		contrib[i]=logcontribution(resi_[i],carac[i,],contrat_[i])
	}
	res=sum(contrib,na.rm=TRUE)
	#print(res)

	return (res)	


}



##############################################################
################## la minusLogLike ###########################
##############################################################


Vminuslike=function (alpha1,alpha2,alpha3,beta1,beta2,gamma1,gamma2)
{	
	alpha=c(alpha1,alpha2,alpha3)
	beta=c(beta1,beta2)
	gamma=c(gamma1,gamma2)
	return (-log_like(alpha,beta,gamma,resi,caracteristique,contrat))
}


##############################################################
##################estimation ###########################
##############################################################

	init=init0
	papatry = try(mle(Vminuslike,start=list(alpha1=init[1],alpha2=init[2],alpha3=init[3],beta1=init[4],beta2=init[5],gamma1=init[6],gamma2=init[7]),method="BFGS"),silent = T)
	

	nbessais = 0
	nbessais_max = 100

	while( (class(papatry)=="try-error") & (nbessais <=nbessais_max) ){

		init[1] = init[1] + ((-1)^(nbessais))*1*(nbessais/3)
		init[2] = init[2] + ((-1)^(nbessais))*0.05*(nbessais)
		init[3] = init[3] + ((-1)^(nbessais))* 0.2*(nbessais/3)
		init[4] = init[4] + ((-1)^(nbessais))*0.1*(nbessais/3)
		init[5] = init[5] + ((-1)^(nbessais))*0.05*(nbessais)
		init[6] = init[6] + ((-1)^(nbessais))* 0.2*(nbessais/3)
		print(nbessais)
		papatry = try(mle(Vminuslike,start=list(alpha1=init[1],alpha2=init[2],alpha3=init[3],beta1=init[4],beta2=init[5],gamma1=init[6],gamma2=init[7]),method="BFGS"),silent = T)
		nbessais = nbessais+1

	}

      papa=mle(Vminuslike,start=list(alpha1=init[1],alpha2=init[2],alpha3=init[3],beta1=init[4],beta2=init[5],gamma1=init[6],gamma2=init[7]),method="BFGS")


summary(papa)


