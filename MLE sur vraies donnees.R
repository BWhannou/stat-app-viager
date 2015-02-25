  library(stats4)  # mle
  library(pracma)  # fsolve
 t_end=20000
    NbExp=10
    NbInd=500
    Nbrej=rep(0,NbExp) 

   inf=5000   #borne de la date de contrat: 
   sup=20000

    init=c(1,1,1)  # initialiseur de la fonction mle
	x0=1.2    #initialisation de fsolve

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

	log_density = function (d,x,t)
	{
		res = log(lambda(d,x,t)) + log(S(d,x,t))
		return (res)
	}


	###############################################################
############# contribution du seller

	logcontribution=function(d,x,t)
	{
		return(log_density(d,x,t)-log(1-S(t_end-t,x,t)))
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





	papatry = try(mle(Vminuslike,start=list(alpha=init[1],beta1=init[2],beta2=init[3]),method="BFGS"),silent = T)
	

	nbessais = 0
	nbessais_max = 10
	
	while( (class(papatry)=="try-error") & (nbessais <=nbessais_max) ){

		init[1] = init[1] + 0.01
		init[2] = init[2] + 0.05
		init[3] = init[3] - 0.02
	
		papatry = try(mle(Vminuslike,start=list(alpha=init[1],beta1=init[2],beta2=init[3]),method="BFGS"),silent = T)
		nbessais = nbessais+1
	
	}

      papa=mle(Vminuslike,start=list(alpha=init[1],beta1=init[2],beta2=init[3]))  #,method="BFGS"


estimle = NULL
        estimle[1]=papa@coef[[1]]
        estimle[2]=papa@coef[[2]]
        estimle[3]=papa@coef[[3]]


    alphaestim=(estimle[1])
    beta1estim=(estimle[2])
    beta2estim=(estimle[3])


   alphaestim
    beta1estim
	beta2estim