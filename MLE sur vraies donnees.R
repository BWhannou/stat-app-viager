  library(stats4)  # mle
  library(pracma)  # fsolve
 t_end=20000
    NbExp=10
    NbInd=500
    Nbrej=rep(0,NbExp) 

   inf=5000   #borne de la date de contrat: 
   sup=20000

    init=c(10,-1/9,-4) # initialiseur de la fonction mle
	x0=1.2    #initialisation de fsolve

resou= function (f)   # trouver le zero d'une fonction croissante
{
	a=0
	b=15000
while((abs(a-b))>(10^-3)){
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

	S=function(d,x,t)
	{
		integral = exp(alpha)*d*d*(0.5*t+(1/3)*d)
		expint = exp(crossprod(beta , x)) * integral
		res = exp(-expint)
		return (res)
	}

	log_S = function(d,x,t){
	
		integral = exp(alpha)*d*d*(0.5*t+(1/3)*d)
		expint = exp(crossprod(beta , x)) * integral
		res = (-expint)
		return (res)

	}

	log_lambda = function(d,x,t){

		res = (log(Psi0(t+d))+ log(Psi1(d)) + crossprod(beta,x))
		return (res)

	}

	log_density = function (d,x,t)
	{
		res = log_lambda(d,x,t) + log_S(d,x,t)
		return (res)
	}


	###############################################################
############# contribution du seller

	logcontribution=function(d,x,t)
	{
		if (abs(log_S(t_end-t,x,t))>log(10^-7)){
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

resi = (delta_age)


nb_carac =2

x1 = T1sex
x2 = age_at_viager/100

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
	if ( length(na.omit(datamatrix[i,]))==2+nb_carac )
		{
		}
	else 
		{
			datamatrix = datamatrix[-i,] #on enlève la ligne avec le NA
		}
	}


}
#n_datamatrixclean = size(datamatrix)[1]

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

Vminuslike=function (alpha,beta1,beta2)
        {
            beta=c(beta1,beta2)
            return (-log_like(alpha,beta,resi_clean,caracteristique_clean,contrat_clean))
        }

Vminuslikev=function (X)
        {
	return (Vminuslike(X[1],X[2],X[3]))

      }




	papatry = try(mle(Vminuslike,start=list(alpha=init[1],beta1=init[2],beta2=init[3]),method="BFGS"),silent = T)
	

	nbessais = 0
	nbessais_max = 100
	
	while( (class(papatry)=="try-error") & (nbessais <=nbessais_max) ){

		init[1] = init[1] + ((-1)^(nbessais))*0.1*(nbessais/3)
		init[2] = init[2] + ((-1)^(nbessais))*0.05*(nbessais)
		init[3] = init[3] + ((-1)^(nbessais))* 0.2*(nbessais/3)
	
		papatry = try(mle(Vminuslike,start=list(alpha=init[1],beta1=init[2],beta2=init[3]),method="BFGS"),silent = T)
		nbessais = nbessais+1
	
	}

      papa=mle(Vminuslike,start=list(alpha=init[1],beta1=init[2],beta2=init[3]),method="BFGS")

	mama = optim(init, Vminuslikev)


Vminuslike(init[1],init[2],init[3])
log_like(init[1],init[2:3],resi_clean,caracteristique_clean,contrat_clean)


estimle = NULL
        estimle[1]=papa@coef[[1]]
        estimle[2]=papa@coef[[2]]
        estimle[3]=papa@coef[[3]]

varestim = NULL
	varestim[1]=vcov(papa)[1,1]
	varestim[2]=vcov(papa)[2,2]
	varestim[3]=vcov(papa)[3,3]

    alphaestim=(estimle[1])
    beta1estim=(estimle[2])
    beta2estim=(estimle[3])


   alphaestim
sqrt(varestim[1])

    beta1estim
sqrt(varestim[2])

	beta2estim
sqrt(varestim[3])


	estimlem = mama$par
alphaestimm = estimlem[1]
beta1estimm = estimlem[2]
beta2estimm = estimlem[3]
