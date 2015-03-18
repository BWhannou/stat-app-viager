library(Epi)
library(survival)

	K=20
  alpha=-1/1000
  beta=c(-(1/9),-1)
  t_end=21000
    NbExp=1
    NbInd=500
    Nbrej=rep(0,NbExp)
 
		age=NULL
		sex=NULL
		resi=NULL
		contrat=NULL
		status=NULL

	var=NULL
	meanresi=NULL
	medresi=NULL
	minresi=NULL
	maxresi=NULL
	
	a_alphestim_v = NULL
	a_beta1estim_v = NULL
	a_beta2estim_v = NULL


	b_alphestim_v = NULL
	b_beta1estim_v = NULL
	b_beta2estim_v = NULL

   inf=5000   #borne de la date de contrat: 
   sup=20000

    init0=c(-8,-0.5,-3)  # initialiseur de la fonction mle
	x0=1.2    #initialisation de fsolve




estimle=matrix(rep(0,3*NbExp),ncol=3)
moyenne_successive=matrix(rep(0,3*NbExp),ncol=3)
varestim=matrix(rep(0,3*NbExp),ncol=3)

for (boucle1 in 1:NbExp)
    {

        # définition de la fonction inverse
        
	  invFdr=function(u,x,t)
        {	
		B=exp(-crossprod(beta , x)-alpha*t)	
		A=-(2/K)*log(1-u)
		
		d=sqrt(A*B)
            return (d)
        }
        # boucle while
        boucle2=1
        while (boucle2<=NbInd)
        {
            # simulation du vecteur x
            x=NULL
            x[1]=runif(1,60,90)            # c'est l'âge
            x[2]=sample(1:2,1,0.5)      # c'est le sexe

            # simulation de la durée du contrat
            t=runif(1,inf,sup)         # simulation suivant une loi uniforme. On pourra toujours      modifier
            u=runif(1,0,(1-10^-7))
            d=invFdr(u,x,t)

            # le cas de la censure
            if (t+d<= t_end)            # condition d'acceptation
            {
			status=c(status,1)
			boucle2=boucle2+1
            }
            else
            {
			status=c(status,0)
                Nbrej[boucle1]=Nbrej[boucle1]+1   # on incrémente à la position  boucle1
            
		}

		    resi=c(resi,d)                     # quand on accepte on      incrémente le compteur
                contrat=c(contrat,t)
                age=c(age,x[1])
			sex=c(sex,x[2])
                
        }

	resultat <- data.frame(resi,status,contrat,age,sex)

	
	meanresi[boucle1]=mean(resi)
	var[boucle1]=sqrt(mean(resi^2)-mean(resi)^2)
	medresi[boucle1]=median(resi)
	minresi[boucle1]=min(resi)
	maxresi[boucle1]=max(resi)
	a=coxph(formula = Surv(resi, status) ~ contrat + age + sex, data = resultat)
	a_alphaestim_v[boucle1] = a$ceof[[1]]
	a_beta1estim_v[boucle1] = a$coef[[2]]
	a_beta2estim_v[boucle1] = a$coef[[3]]


	resi_=resi[which(status==1)]
	contrat_=contrat[which(status==1)]
	age_=age[which(status==1)]
	sex_=sex[which(status==1)]
	status_=status[which(status==1)]
	resultat_ <- data.frame(resi_,status_,contrat_,age_,sex_)
	b=coxph(formula = Surv(resi_, status_) ~ contrat_ + age_ + sex_, data = resultat_)
	b_alphaestim_v[boucle1] = b$ceof[[1]]
	b_beta1estim_v[boucle1] = b$coef[[2]]
	b_beta2estim_v[boucle1] = b$coef[[3]]
}

<<<<<<< HEAD
plot(basehaz(a, centered = TRUE),ylim=c(-0.1,1000))
}
=======
a_alphaestim = mean(a_alphaestim_v)
a_beta1estim = mean(a_beta1estim_v)
a_beta2estim = mean(a_beta2estim_v)

b_alphaestim = mean(b_alphaestim_v)
b_beta1estim = mean(b_beta1estim_v)
b_beta2estim = mean(b_beta2estim_v)

    #alphaestim=mean(estimle[,1])
    #beta1estim=mean(estimle[,2])
    #beta2estim=mean(estimle[,3])
>>>>>>> origin/master

a_alphaestim 
a_beta1estim 
a_beta2estim 

b_beta2estim 
b_beta1estim 
b_alphaestim 


log(-(2/(K*t*t))*log(1-u))
