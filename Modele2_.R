#############################################################
########################## PACKAGES #########################
#############################################################


  library(maxLik) 	   #mle


#############################################################
########################## FENETRE D'OBSERVATION ############
#############################################################

t_end=20000  # max(T1deathdate)=18092

#############################################################
########################## INITIATION #######################
#############################################################

init0=c(10^-5,10^-2,0.1,10^-4,6*10^-4,10^-5,3*10^-4,0.21,0.0014,10^-4,6*10^-5,10^-5,3*10^-4)
maxl=NULL
#############################################################
################# CONSTRUCTION DE LA BASE DE DONNEES ########
######################## REDUITE ET SANS na #################
#############################################################

n_seller=2 # le nombre de caracteristique utilisé pour le seller
n_clone=2 # le nombre de caracteristique utilisé pour le clone 

caracteristique_clone=NULL
caracteristique_seller=NULL

caracteristique_seller=age_at_viager
caracteristique_seller=c(caracteristique_seller,T1sex-1)   # pour ajouter des caracteristiques

carac_seller=matrix(caracteristique_seller,ncol=n_seller)

caracteristique_clone=(dayoflifemean-delta_age_clone)/365
#caracteristique_clone=Ind_Region_birth
caracteristique_clone=c(caracteristique_clone,T1sex-1)

carac_clone=matrix(caracteristique_clone,ncol=n_clone)

contrat=datecontrat

resi_seller=delta_age
resi_clone=delta_age_clone

data_seller=data.frame(residu=resi_seller,carac=carac_seller,date=contrat)
data_clone=data.frame(residu=resi_clone,carac=carac_seller,date=contrat)

              ####################################
              ######## nettoyage #################
individu_enleve_s=NULL
for (i in 1:length(data_seller$carac.1))
{
	if (is.na(data_seller$carac.1[i]) || is.na(data_seller$carac.2[i]) || data_seller$residu[i]<0 || is.na(contrat[i]) )
	{
		individu_enleve_s=c(individu_enleve_s,i)
	}
}	



individu_enleve_c=NULL
for (i in 1:length(datecontrat))
{
	if (is.na(data_clone$carac.1[i]) || is.na(data_clone$carac.2[i]) || data_clone$residu[i]<0 || is.na(contrat[i]) )
	{
		individu_enleve_c=c(individu_enleve_c,i)
	}
}

data_clone=data_clone[-c(individu_enleve_s,individu_enleve_c),]
data_seller=data_seller[-c(individu_enleve_s,individu_enleve_c),]
# individu_enleve_c==individu_enleve_s
length(individu_enleve_c)
length(individu_enleve_s)



#############################################################
#################### LES INDICATRICES #######################
#############################################################

resi_tot=c(data_seller$residu,data_clone$residu)
a=quantile(resi_tot)
limits=c(a[[1]],a[[2]],a[[3]],a[[4]],a[[5]])

ind1= function (d)
{
	res=0
	#print(d)
	if (d<=limits[2])
	{
		res=1
	}
	return (res)
}

ind2= function (d)
{
	res=0
	if (d> limits[2] & d<=limits[3])
	{
		res=1
	}
	return (res)
}

ind3= function (d)
{
	res=0
	if (d> limits[3] & d<=limits[4])
	{
		res=1
	}
	return (res)
}

ind4= function (d)
{
	res=0
	if (d> limits[4])
	{
		res=1
	}
	return (res)
}


#############################################################
#################### LOG-LIKELIHOOD #########################
#############################################################

#steps=c(1,2,3,4)
log_like=function (alpha, steps, beta,resi_,carac_,contrat_)
{
	Psi1 = function(d)
	{	
		print(d)
		ind=c(ind1(d),ind2(d),ind3(d),ind4(d))
		
		res=t(ind)%*%steps
		return(res)
	}

	lambda = function(d,x,t)
	{
		return(Psi1(d)*exp(beta%*%x+alpha*t))
	}

	logLambda = function (d,x,t)
	{
		return (log(Psi1(d))+beta%*%x+alpha*t)
	}

	intLambda= function(d,x,t)
	{
		res=NULL
		expo=exp(beta%*%x+alpha*t)
		ind=c(ind1(d),ind2(d),ind3(d),ind4(d))
		interval=which(ind==1)                   #reperer l'intervalle dans lequel se trouve d
		if (interval==1)
		{
			return (steps[1]*d*expo)
		}
		else
		{
			for (i in 1:(interval-1))
			{
				res=c(res,steps[i]*(limits[i+1]-limits[i]))
			}
			return ((sum(res)+steps[interval]*(d-limits[interval]))*expo)
		}
	}

	S= function (d,x,t)
	{
		return (exp(-intLambda(d,x,t)))
	}

	logS =function (d,x,t)
	{
		return (-intLambda(d,x,t))
	}

	logDensite = function (d,x,t)
	{
		res = logLambda(d,x,t) + logS(d,x,t)
		return (res)
	}


	###############################################################
       ############# contribution du seller

	logcontribution=function(d,x,t)
	{
		if (abs(S(t_end-t,x,t))>10^-7){
		return(logDensite(d,x,t)-log(1-S(t_end-t,x,t)))
		}
		else
		{
		return (logDensite(d,x,t)+S(t_end-t,x,t))
		}
	}
	
	contrib=NULL
	for (i in 1:length(contrat_))
	{
		contrib[i]=logcontribution(resi_[i],carac_[i,],contrat_[i])
	}

	res=sum(contrib,na.rm=TRUE)
	#print(res)
	return (res)	


}


##############################################################
################## la minusLogLike ###########################
##############################################################

resi_s=data_seller$residu
caracteristique_s=matrix(c(data_seller$carac.1,data_seller$carac.2),ncol=n_seller)
contrat=data_seller$date

resi_c=data_clone$residu
caracteristique_c=matrix(c(data_seller$carac.1,data_seller$carac.2),ncol=n_seller)

        Vminuslike=function (alpha,beta1_s,beta2_s,step1_s,step2_s,step3_s,step4_s,beta1_c,beta2_c,step1_c,step2_c,step3_c,step4_c)
{
  beta_s=c(beta1_s,beta2_s)
  step_s=c(exp(step1_s),exp(step2_s),exp(step3_s),exp(step4_s))
  step_c=c(exp(step1_c),exp(step2_c),exp(step3_c),exp(step4_c))
  beta_c=c(beta1_c,beta2_c)

	res=-log_like(alpha,step_s,beta_s,resi_s,caracteristique_s,contrat)- log_like(alpha,step_c,  beta_c, resi_c, caracteristique_c, contrat)
  #print(res)
return (res)
}

        loglike=function (param) #(alpha,beta1_s,beta2_s,step1_s,step2_s,step3_s,step4_s,beta1_c,beta2_c,step1_c,step2_c,step3_c,step4_c)
{
  beta_s=c(param[2],param[3])
  step_s=c(exp(param[4]),exp(param[5]),exp(param[6]),exp(param[7]))
  step_c=c(exp(param[10]),exp(param[11]),exp(param[12]),exp(param[13]))
  beta_c=c(param[8],param[9])
	alpha=param[1]
	res=log_like(alpha,step_s,beta_s,resi_s,caracteristique_s,contrat)+ log_like(alpha,step_c,  beta_c, resi_c, caracteristique_c, contrat)

return (res)
}

##############################################################
##################estimation ###########################
##############################################################

	init=init0
	papatry = try(mle(Vminuslike,start=list(alpha=init[1],beta1_s=init[2],beta2_s=init[3],step1_s=init[4],step2_s=init[5],step3_s=init[6],step4_s=init[7],beta1_c=init[8],beta2_c=init[9],step1_c=init[10],step2_c=init[11],step3_c=init[12],step4_c=init[13]),method="BFGS"),silent = T)
	

	nbessais = 0
	nbessais_max = 1
	
	while( (class(papatry)=="try-error") & (nbessais <=nbessais_max) ){

		init[1] = init[1] + ((-1)^(nbessais))*0.1*(nbessais/3)
		init[2] = init[2] + ((-1)^(nbessais))*0.05*(nbessais)
		init[3] = init[3] + ((-1)^(nbessais))* 0.2*(nbessais/3)
		init[4] = init[4] + ((-1)^(nbessais))*0.1*(nbessais/3)
		init[5] = init[5] + ((-1)^(nbessais))*0.05*(nbessais)
		init[6] = init[6] + ((-1)^(nbessais))* 0.2*(nbessais/3)
		init[7] = init[7] + ((-1)^(nbessais))*0.1*(nbessais/3)
		init[8] = init[8] + ((-1)^(nbessais))*0.05*(nbessais)
		init[9] = init[9] + ((-1)^(nbessais))* 0.2*(nbessais/3)
		init[10] = init[10] + ((-1)^(nbessais))*0.1*(nbessais/3)
		init[11] = init[11] + ((-1)^(nbessais))*0.05*(nbessais)
		init[12] = init[12] + ((-1)^(nbessais))* 0.2*(nbessais/3)
		init[13] = init[13] + ((-1)^(nbessais))* 0.2*(nbessais/3)

	print(nbessais)
		papatry = try(mle(Vminuslike,start=list(alpha=init[1],beta1_s=init[2],beta2_s=init[3],step1_s=init[4],step2_s=init[5],step3_s=init[6],step4_s=init[7],beta1_c=init[8],beta2_c=init[9],step1_c=init[10],step2_c=init[11],step3_c=init[12],step4_c=init[13]),method="BFGS"),silent = T)
		nbessais = nbessais+1
	
	}

      papa=mle(Vminuslike,start=list(alpha=init[1],beta1_s=init[2],beta2_s=init[3],step1_s=init[4],step2_s=init[5],step3_s=init[6],step4_s=init[7],beta1_c=init[8],beta2_c=init[9],step1_c=init[10],step2_c=init[11],step3_c=init[12],step4_c=init[13]))  #,method="BFGS"
	
summary(papa)
maxl = maxLik(loglike , start = c(alpha=init[1],beta1_s=init[2],beta2_s=init[3],step1_s=init[4],step2_s=init[5],step3_s=init[6],step4_s=init[7],beta1_c=init[8],beta2_c=init[9],step1_c=init[10],step2_c=init[11],step3_c=init[12],step4_c=init[13]),method="NM")
summary(maxl)


	##############################################
## On essaie d'entraîner l'algo en updatant ##
## le point de départ ########################
##############################################


nbupdate = 3
while ((nbupdate>0) ){
  
init[1] = maxl$estimate[[1]]
init[2] = maxl$estimate[[2]]
init[3] = maxl$estimate[[3]]
init[4] = maxl$estimate[[4]]
init[5] = maxl$estimate[[5]]
init[6] = maxl$estimate[[6]]
init[7] = maxl$estimate[[7]]
init[8] = maxl$estimate[[8]]
init[9] = maxl$estimate[[9]]
init[10] = maxl$estimate[[10]]
init[11] = maxl$estimate[[11]]
init[12] = maxl$estimate[[12]]
init[13] = maxl$estimate[[13]]
	maxl = maxLik(loglike , start = c(alpha=init[1],beta1_s=init[2],beta2_s=init[3],step1_s=init[4],step2_s=init[5],step3_s=init[6],step4_s=init[7],beta1_c=init[8],beta2_c=init[9],step1_c=init[10],step2_c=init[11],step3_c=init[12],step4_c=init[13]),method="NM")

print(nbupdate)
summary(maxl)
nbupdate = nbupdate -1
}

summary(maxl)

