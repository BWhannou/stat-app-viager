  library(stats4)  # mle
  library(pracma)  # fsolve
  library(maxLik)  #autre package pour mle de 2011
 t_end=20000
    NbExp=10
    NbInd=500
    Nbrej=rep(0,NbExp) 

   inf=5000   #borne de la date de contrat: 
   sup=20000

    init=c(-10,-1/9,-4) # initialiseur de la fonction mle
	x0=1.2    #initialisation de fsolve

resou= function (f)   # trouver le zero d'une fonction croissante
{
	a=0
	b=15000
while((abs(a-b))>(10^-5)){
	if (f(b)>0)
	{
		if(f((a+b)/2)>0)
		{
			b=(a+b)/2
		}
		else
		{a=(a+b)/2}
	}
	if (f(b)<0)
	{
		b=2*b
	}

}
return (b)
}


##################### CHOIX CLONE/SELLER#################################
clone = 0 #On met 0 pour évaluer les seller et 1 pour évaluer les clones
#########################################################################



#################### CHOIX PSI ##################

######## PSI1 #######
expo = 1
step = 0
#####################

##################################################




Psi0 = function (x)
	{
	# x est un nombre (une date en numérique)
	
	return (x)
	}



Psi0_Tcheby=function(d,n){

	vec = NULL
	for (k in 1:(floor(n/2)+1)){
		vec[k]= ((-1)^(k-1)*(choose(n-k+1,k-1)) *(2*d)^(n-2*(k-1)))
	}
	return (sum(vec))
	
}


Psi1_exp = function (d,alpha,beta,k=0)
	{
	# d est un nombre (date en numérique)
	
	return ( exp(alpha[k+1]) * d)
	}


Psi1_step_v = function(d,beta,step){   #,resi_quartiles){
	
	#resi_quartiles doit être un vecteur de taille 3 avec le 1er quartile, la médiane et le troisième quartile
	#step doit être un vecteur de longueur 4 contenant les valeurs des "marches" de l'escalier

	if(d<=resi_quartiles[1]){
		return (step[1])
	}

	if( (d>resi_quartiles[1]) & (d<= resi_quartiles[2]) ){
		return (step[2])
	}

	if( (d>resi_quartiles[2]) & (d<= resi_quartiles[3]) ){
		return (step[3])
	}

	if(d>resi_quartiles[3]){
		return (step[4])
	}


}

Psi1_step = function(d,beta,step1,step2,step3,step4){

		return ( Psi1_step_v(d,beta,c(step1,step2,step3,step4) ))

}

if (expo ==1){
	Psi1 = Psi1_exp
}

if (step ==1){
	Psi1 = Psi1_step
}

if (expo==1){

lambda = function (d,x,t,alpha,beta)
	{
	res = Psi0(t+d) * Psi1(d,alpha,beta) * exp(crossprod(beta , x))
	return (res)
	}


S=function(d,x,t,alpha,beta,k=0)
	{
		integral = exp(alpha[k+1])*d*d*(0.5*t+(1/3)*d)
		expint = exp(crossprod(beta , x)) * integral
		res = exp(-expint)
		return (res)
	}

log_S = function(d,x,t,alpha,beta,k=0){
	
		integral = exp(alpha[k+1])*d*d*(0.5*t+(1/3)*d)
		expint = exp(crossprod(beta , x)) * integral
		res = (-expint)
		return (res)

	}

log_lambda = function(d,x,t,alpha,beta,k=0){

		res = (log(Psi0(t+d))+ log(Psi1(d,alpha,beta)) + crossprod(beta,x))
		return (res)

	}

log_density = function (d,x,t,alpha,beta,k=0)
	{
		res = log_lambda(d,x,t,alpha,beta) + log_S(d,x,t,alpha,beta)
		return (res)
	}



log_like=function (alpha,beta,resi_, carac, contrat_, k=0)
{


	###############################################################
############# contribution du seller

	logcontribution=function(d,x,t)
	{
		if (abs(log_S(t_end-t,x,t,alpha,beta))>log(10^-7)){
		return(log_density(d,x,t,alpha,beta)-log(1-S(t_end-t,x,t,alpha,beta)))
		}
		else
		{
		return (log_density(d,x,t,alpha,beta)+S(t_end-t,x,t,alpha,beta))
		}
	}
	contrib=NULL
	for (i in 1:length(contrat_))
	{
		contrib[i]=logcontribution(resi_[i],carac[i,],contrat_[i])
	}


	return (sum(contrib,na.rm=TRUE))	


}


} #Fin du if Psi1 == Psi1_exp


if (step==1){

lambda = function (d,x,t,beta,step1,step2,step3,step4)
	{
	res = Psi0(t+d) * Psi1(d,beta,step1,step2,step3,step4) * exp(crossprod(beta , x))
	return (res)
	}


S=function(d,x,t,beta,step1,step2,step3,step4)
	{
		f= function (x){
			return (0.5*x^2)
		}


		integral = (1/2)*(d*d)
		expint = exp(crossprod(beta , x)) * integral
		exp_b_x = exp(crossprod(beta,x))
		

		if (d<= resi_quartiles[1]){
			return ( exp(-step1*expint) )
		}


		if ( (d> resi_quartiles[1]) &(d<= resi_quartiles[2]) ){
			return (exp(- exp_b_x*(step1*f(resi_quartiles[1]) + step2*f(d-resi_quartiles[1]) )  ) )
		}

		if ( (d> resi_quartiles[2]) &(d<= resi_quartiles[3]) ){
			return (exp(- exp_b_x*(step1*f(resi_quartiles[1]) + step2*f(resi_quartiles[2]-resi_quartiles[1]) +step3*f(d-resi_quartiles[2]) )    ))
		}

		if (d> resi_quartiles[3]){
			return (exp(- exp_b_x*(step1*f(resi_quartiles[1]) + step2*f(resi_quartiles[2]-resi_quartiles[1]) +step3*f(resi_quartiles[3]-resi_quartiles[2]) +step4*f(d-resi_quartiles[3]) ) ))
		}

		
	}

log_S = function(d,x,t,beta,step1,step2,step3,step4){
		f= function (x){
			return (0.5*x^2)
		}


		integral = (1/2)*(d*d)
		expint = exp(crossprod(beta , x)) * integral
		exp_b_x = exp(crossprod(beta,x))
		

		if (d<= resi_quartiles[1]){
			return ( (-step1*expint) )
		}


		if ( (d> resi_quartiles[1]) &(d<= resi_quartiles[2]) ){
			return ((- exp_b_x*(step1*f(resi_quartiles[1]) + step2*f(d-resi_quartiles[1]) )  ) )
		}

		if ( (d> resi_quartiles[2]) &(d<= resi_quartiles[3]) ){
			return ((- exp_b_x*(step1*f(resi_quartiles[1]) + step2*f(resi_quartiles[2]-resi_quartiles[1]) +step3*f(d-resi_quartiles[2]) )    ))
		}

		if (d> resi_quartiles[3]){
			return ((- exp_b_x*(step1*f(resi_quartiles[1]) + step2*f(resi_quartiles[2]-resi_quartiles[1]) +step3*f(resi_quartiles[3]-resi_quartiles[2]) +step4*f(d-resi_quartiles[3]) ) ))
		}


}

log_lambda = function(d,x,t,beta,step1,step2,step3,step4){

		res = (log(Psi0(t+d))+ log(Psi1(d,beta,step1,step2,step3,step4)) + crossprod(beta,x))
		return (res)

	}

log_density = function (d,x,t,beta,step1,step2,step3,step4)
	{
		res = log_lambda(d,x,t,beta,step1,step2,step3,step4) + log_S(d,x,t,beta,step1,step2,step3,step4)
		return (res)
	}



log_like=function (beta,resi_, carac, contrat_,step1,step2,step3,step4)
{


	###############################################################
############# contribution du seller

	logcontribution=function(d,x,t)
	{
		if (abs(log_S(t_end-t,x,t,beta,step1,step2,step3,step4))>log(10^-7)){
		return(log_density(d,x,t,beta,step1,step2,step3,step4)-log(1-S(t_end-t,x,t,beta,step1,step2,step3,step4)))
		}
		else
		{
		return (log_density(d,x,t,beta,step1,step2,step3,step4)+S(t_end-t,x,t,beta,step1,step2,step3,step4))
		}
	}
	contrib=NULL
	for (i in 1:length(contrat_))
	{
		contrib[i]=logcontribution(resi_[i],carac[i,],contrat_[i])
	}


	return (sum(contrib,na.rm=TRUE))	


}

}#Fin du if Psi1 == Psi_step









invFdr=function(u,x,t)
	{	
		f=function (d)
		{
			A=d^2*(t*(exp(alpha))*0.5+(d*exp(alpha))/3)
			B=-exp(t(x)%*%beta)
			return (log(1-u)-A*B)
		}

		d=fsolve(f,x0)$x
            return (d)
        }


invFdr=function(u,x,t)
        {	
		f=function (d)
		{
			A=d^2*(t*(exp(alpha))*0.5+(d*exp(alpha))/3)

			B=-exp(crossprod(beta , x))

			return ((log(1-u)-A*B))
		}

		d=resou(f)
            return (d)
        }
Fdr = function (x,t,d,alpha,beta){
			A=d^2*(t*(exp(alpha))*0.5+(d*exp(alpha))/3)

			B=exp(crossprod(beta , x))
			return (1-exp(-A*B))

}



resi_seller = delta_age
resi_clone = delta_age_clone


resi = resi_clone

if (clone ==0){
	resi =resi_seller
}

#On va stocker les quartiles des resi selectionnées

resi_quartiles = c(summary(resi)[[2]],summary(resi)[[3]],summary(resi)[[5]])

nb_carac =2

x1 = T1sex-1
x2_seller = age_at_viager/10
x2_clone = age_at_viager_clone/10

x2 = x2_clone

if (clone ==0){
	x2 = x2_seller
}

#Ces deux caractéristiques ont même longueur

caracteristique = matrix(0,length(x1),nb_carac)
caracteristique[,1] = x1
caracteristique[,2] = x2

contrat = (datecontrat)

#Il faut nettoyer ces données pour n'avoir que des individus pour
#lesquels toutes les variables mentionnées ci-dessus sont renseignées

datamatrix = matrix(0,length(resi),2+nb_carac)
datamatrix[,1]= resi
for (i in 1:nb_carac ){
	datamatrix[,i+1]=caracteristique[,i]
}	 

datamatrix[,nb_carac+2]= contrat

n_datamatrix = size(datamatrix)[1]

  

for (i in 1:n_datamatrix){
	if( i <=size(datamatrix)[1]){
		#Si la ligne a un NA, on la supprime
  
  if ( prod(!is.na(datamatrix[i,])) ==0) #on utilise le fait que TRUE*TRUE =1, TRUE*FAlSE =0 et FALSE*FALSE=0 pour tester s'il ya un NA
  {
    datamatrix = datamatrix[-i,] #on enlève la ligne avec le NA
  }
    
  else 
		{
			
		}
	}


}
  

#On enleve les valeurs negatives de resi
for (k in 1:  size(datamatrix)[1]){
	if(k<= size(datamatrix)[1]){
	if (datamatrix[k,1]<0){
		datamatrix = datamatrix[-k,]
	}
	}
}

n_datamatrixclean = size(datamatrix)[1]

resi_clean = datamatrix[,1]
x1_clean = datamatrix[,2]
x2_clean = datamatrix[,3]
contrat_clean = datamatrix[,4]

caracteristique_clean = matrix(0,length(x1_clean),nb_carac)
caracteristique_clean[,1] = x1_clean
caracteristique_clean[,2] = x2_clean

datamatrix_data = data.frame(resi_clean,contrat_clean,x1_clean,x2_clean)


Vminuslike=function (alpha,beta1,beta2)
        {
            beta=c(beta1,beta2)
            return (-log_like(alpha,beta,resi_clean,caracteristique_clean,contrat_clean))
        }

Vminuslikev=function (X)
        {
	return (Vminuslike(X[1],X[2],X[3]))

      }
#On veut avoir resi_clean comme une matrice avec deux colonnes : 1 pour les sellers et 2 pour les clones

#il faut avoir les caracteristiques sous deux matrices différentes pour clones et sellers
#caracteristique_clean_s et #caracteristique_clean_c

if (clone == 0){
  resi_clean_s=resi_clean 
	caracteristique_clean_s = caracteristique_clean
  contrat_clean_s = contrat_clean
}

if (clone == 1){
  resi_clean_c =resi_clean
	caracteristique_clean_c = caracteristique_clean
  contrat_clean_c =contrat_clean
}



###############################################################
###### !! Fin de la différence entre clone =0 et clone =1 !! ##
###############################################################
  


#Vminuslike_tot=function (alpha_s,beta1_s,beta2_s,alpha_c,beta1_c,beta2_c)
 #       {
  #          beta_s=c(beta1_s,beta2_s)
	#	beta_c=c(beta1_c,beta2_c)
   #         return (-log_like(alpha_s,beta_s,resi_clean[,1],caracteristique_clean_s,contrat_clean[,1])
		#		- log_like(alpha_c, beta_c, resi_clean[,2], caracteristique_clean_c, contrat_clean[,2]
		#		)
     #   }

#on ne stocke pas les résultats dans les mêmes variables suivant la valeur de clone

#if( clone ==1){
#	estimle_clone = estimle
#	estimlem_clone = estimlem
#	alpha_clone = mean(c(alphaestim,alphaestimm))
#	beta1_clone = mean(c(beta1estim, beta1estimm))
#	beta2_clone = mean(c(beta2estim, beta2estimm))
#
#	beta_clone = c(beta1_clone, beta2_clone)
#	
#	#cox_clone = coxph(formula = Surv(resi_clean) ~ contrat_clean + x1_clean + x2_clean, data = datamatrix_data)

#}

#if ( clone ==0){
#	estimle_seller = estimle
#	estimlem_seller = estimlem
#
#	alpha_seller = mean(c(alphaestim,alphaestimm))
#	beta1_seller = mean(c(beta1estim, beta1estimm))
#	beta2_seller = mean(c(beta2estim, beta2estimm))

#	beta_seller = c(beta1_seller, beta2_seller)
#
	#cox_seller = coxph(formula = Surv(resi_clean) ~ contrat_clean + x1_clean + x2_clean, data = datamatrix_data)

#}



#kstest =NULL

#n_kstest = 100

#matrix_seller = matrix(0,n_datamatrixclean,n_kstest)
#matrix_clone =  matrix(0,n_datamatrixclean,n_kstest)

#for (i in 1:n_datamatrixclean){
#	tirage_seller = NULL
#	tirage_clone = NULL
#	u = runif(n_kstest)
#
#	for(j in 1:n_kstest){
#
#		tirage_seller[j] = invFdrpara( u[j],caracteristique_clean[i,],contrat_clean[i],alpha_seller,beta_seller )
#		tirage_clone[j] = invFdrpara(u[j],caracteristique_clean[i,],contrat_clean[i],alpha_clone,beta_clone)
#	}
#
#	matrix_seller[i,]= (tirage_seller)
#	matrix_clone[i,]= (tirage_clone)
#
#	kstest[i] = ks.test(tirage_seller,tirage_clone)$p.value
#}
#kstest


