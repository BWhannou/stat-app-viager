# stat-app-viager

le modèle choisit est: lambda(d,x,t)=(t+d)*d*exp(x*beta+alpha)
# les librairies

library(stats4)  // mle
library(pracma)  // fsolve

# Initialisation:

alpha, beta, t_end,
nombre d'itération total de l'expérience=10000, nombre d'individu retenu par expérience=500,
nombre d'individu rejeté par expérience=rep(0,nombre d'itération total de l'expérience), 



borne de la date de contrat: inf=5000, sup=20000

init=c(1,1.1,3)  // initialiseur de la fonction mle

# Definition de la log_likelihood

log_like=function (alpha,beta,resi_, carac, contrat_)
{
	...
	return
}


# Boucle for 

for (boucle1=0; boucle1<=nombre d'itération total de l'expérience;boucle1++)
{
		resi=c(0,nombre d'individu retenu par expérience)
		caracteristique= matrice à 2 colonnes et le nombre de lignes est égale au nombre d'individu retenu par experience
		contrat=c(0,nombre d'individu retenu par expérience)
		# définition de la fonction inverse
		invFdr=function(u,x,t)
		{
			
			return d
		}
		# boucle while
		
		while (boucle2<=nombre d'individu retenu par expérience)
		{
			# simulation du vecteur x
			x=NULL
			x[1]=runif(60,90)            // c'est l'âge
			x[2]=sample(1:2,1,0.5)      // c'est le sexe
		
			# simulation de la durée du contrat
			t=runif(1,inf,sup)          // simulation suivant une loi uniforme. On pourra toujours modifier
			u=runif(n_tirage,10^-10,(1-10^-7))
			d=invFdr(u,x,t)
			
			# le cas de la censure
			if (t+d<= t_end)            // condition d'acceptation
			{
				resi[boucle2]=d						// quand on accepte on incrémente le compteur
				contrat[boucle2]=t
				caracteristique[boucle2,]=x
				boucle2=boucle2+1
			}
			else
			{
				nombre d'individu rejeté par experience[boucle1]++   // on incrémente à la position boucle1
			}
		}
		
		# definition de Vminuslike
		
		Vminuslike=function (alpha,beta1,beta2)
		{
			beta=c(beta1,beta2)
			return (-log_like(alpha,beta,resi,caracteristique,contrat))
		}
		
		papa=mle(Vminuslike,start=list(alpha=init[1],beta1=init[2],beta2=init[3]),method="BFGS")
	
		
		
			#
