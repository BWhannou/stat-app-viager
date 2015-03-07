  library(stats4)  # mle
  library(pracma)  # fsolve


  alpha=-10
  beta=c(-(1/9),-4)
  t_end=21000
    NbExp=10
    NbInd=500
    Nbrej=rep(0,NbExp)
 


	var=NULL
	meanresi=NULL
	medresi=NULL
	minresi=NULL
	maxresi=NULL

   inf=5000   #borne de la date de contrat: 
   sup=20000

    init0=c(-8,-0.5,-3)  # initialiseur de la fonction mle
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

estimle=matrix(rep(0,3*NbExp),ncol=3)
moyenne_successive=matrix(rep(0,3*NbExp),ncol=3)
varestim=matrix(rep(0,3*NbExp),ncol=3)

for (boucle1 in 1:NbExp)
    {
        resi=rep(0,NbInd)
        caracteristique= matrix(rep(0,2*NbInd),ncol=2)
        contrat=rep(0,NbInd)
        # définition de la fonction inverse
        
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
                resi[boucle2]=d                     # quand on accepte on      incrémente le compteur
                contrat[boucle2]=t
                caracteristique[boucle2,]=x
                boucle2=boucle2+1
            }
            else
            {
                Nbrej[boucle1]=Nbrej[boucle1]+1   # on incrémente à la position  boucle1
            }
        }

        # definition de Vminuslike

        Vminuslike=function (alpha,beta1,beta2)
        {
            beta=c(beta1,beta2)
            return (-log_like(alpha,beta,resi,caracteristique,contrat))
        }

	meanresi[boucle1]=mean(resi)
	var[boucle1]=sqrt(mean(resi^2)-mean(resi)^2)
	medresi[boucle1]=median(resi)
	minresi[boucle1]=min(resi)
	maxresi[boucle1]=max(resi)
	

	init=init0
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

      papa=mle(Vminuslike,start=list(alpha=init[1],beta1=init[2],beta2=init[3]))  #,method="BFGS"

        estimle[boucle1,1]=papa@coef[[1]]
        estimle[boucle1,2]=papa@coef[[2]]
        estimle[boucle1,3]=papa@coef[[3]]

	moyenne_successive[boucle1,1]=mean(estimle[1:boucle1,1])
	moyenne_successive[boucle1,2]=mean(estimle[1:boucle1,2])
	moyenne_successive[boucle1,3]=mean(estimle[1:boucle1,3])

	varestim[boucle1,1]=vcov(papa)[1,1]
	varestim[boucle1,2]=vcov(papa)[2,2]
	varestim[boucle1,3]=vcov(papa)[3,3]

plot(Nbrej,type='l',col='blue')

den=Nbrej+500
Probrej=Nbrej/den
cumNbrej=cumsum(Probrej)
a=c(1:1000)
suc_Nbrej=cumNbrej/a


print(boucle1)
par(mfrow=c(2,2))

plot(moyenne_successive[,1], type='l', lwd=2, col='blue', 
     ylab='estimation moyenne', xlab='expérience', main="estimation du paramètre alpha")
plot(moyenne_successive[,2], type='l', lwd=2, 
     ylab='estimation moyenne', xlab='expérience', main="estimation du paramètre beta1")
 plot(moyenne_successive[,3], type='l', lwd=2, 
     ylab='estimation moyenne', xlab='expérience', main="estimation du paramètre beta2")

   plot(suc_Nbrej,type='l',lwd=2,col='blue',
     ylab='Probabilité', xlab='expérience', main="Probabilité de rejet moyen")


}

    alphaestim=mean(estimle[,1])
    beta1estim=mean(estimle[,2])
    beta2estim=mean(estimle[,3])


   alphaestim
    beta1estim
beta2estim
mean(meanresi)

plot(moyenne_successive[,1])
resultat <- data.frame(alpha=estimle[,1],moyenne_alpha=moyenne_successive[,1],beta1=estimle[,2],moyenne_beta1=moyenne_successive[,2],beta2=estimle[,3],moyenne_beta2=moyenne_successive[,3],var=varestim,nombre_rejete=Nbrej)
stat_simule <- data.frame(moyenne=meanresi,ecarttype=var,mediane=medresi,min=minresi,max=maxresi)


#################################################################################
###################  enregistrement des resultats #################################
################################################################################
setwd("C:/Users/Brian/Desktop/statapp/stat")
getwd()
write.table(resultat,file="resultat.csv",append=FALSE,quote=TRUE,sep="\t",eol="\n",
na="NA",dec=".",row.names=TRUE,col.names=TRUE)

write.table(stat_simule,file="simulation.csv",append=FALSE,quote=TRUE,sep="\t",eol="\n",
na="NA",dec=".",row.names=TRUE,col.names=TRUE)
#################################################################################
###################                                #################################
################################################################################

simulation= read.table("simulation.csv")
resultat = read.table("resultat.csv")

#################################################################################
###################    analyse des simulations     #################################
################################################################################

plot(simulation$moyenne,ylim=c(0,2800),type='l')
lines(simulation$max,col='red')
lines(simulation$min,col='green')
lines(simulation$med,col='blue')
title(main="la simulation")

mean(simulation$ecarttype)
a=simulation$moyenne-simulation$med
summary(a)

plot(simulation$moyenne, type='l', lwd=1, 
     ylab='durée de vie résiduelle', xlab='expérience',ylim=c(0,2800), main="comparaison moyenne et médiane")
#abline(h=0,lty=3)
#abline(v=0,lty=3)
lines(simulation$med, type='l', lwd=1, col='red')
legend(0,500, yjust=0,
       c("moyenne", "med"),
       lwd=3, lty=1, col=c(par('fg'), 'red'),
      )

#################################################################################
###################    resultat                     #################################
################################################################################
resultat[1,]

plot(resultat$moyenne_alpha, type='l', lwd=2, col='blue', 
     ylab='estimation moyenne', xlab='expérience',ylim=c(-10.5,-8), main="estimation du paramètre alpha")
abline(h=-10,lty=3)
#abline(v=0,lty=3)



###########
##########  il y a des variances négatives je sais pas pourquoi
###########
varN=NULL
varP=NULL
for (i in 1:1000)
{
if(resultat$var.3[i]<=0)
{varN=c(varN,resultat$var.3[i])}
else
{varP=c(varP,resultat$var.3[i])}
}
sqrt(abs(mean(varN)))
sd(varP)
mean(sqrt(varP))
resultat$var.1
plot(resultat$moyenne_beta1, type='l', lwd=2, 
     ylab='estimation moyenne', xlab='expérience',ylim=c(-0.5,-0.10), main="estimation du paramètre beta1")
abline(h=-0.11,lty=3)
#abline(v=0,lty=3)


plot(resultat$moyenne_beta2, type='l', lwd=2, 
     ylab='estimation moyenne', xlab='expérience', main="estimation du paramètre beta2")
abline(h=-4,lty=3)
#abline(v=0,lty=3)

plot(resultat$nombre_rejete,type='l',col='blue')
mean(resultat$nombre_rejete)
den=resultat$nombre_rejete+500
Probrej=resultat$nombre_rejete/den
cumNbrej=cumsum(Probrej)
a=c(1:1000)
suc_Nbrej=cumNbrej/a
plot(suc_Nbrej,type='l',lwd=2,col='blue',
     ylab='Probabilité', xlab='expérience', main="Probabilité de rejet moyen")
abline(h=0.1465,lty=3)
ecarttype=matrix(rep(0,3*1000),ncol=3)

for (i in 1:1000)
{
ecarttype[i,1]=sqrt(resultat$var.1[i])
ecarttype[i,2]=sqrt(resultat$var.2[i])
ecarttype[i,3]=sqrt(resultat$var.3[i])
}
a=c(1:length(ecarttype[,1]))
ecartsumcum_alpha=cumsum(ecarttype[,1],na.rm=TRUE)
ecartsumcum_beta1=cumsum(ecarttype[,2],na.rm=TRUE)
ecartsumcum_beta2=cumsum(ecarttype[,3],na.rm=TRUE)

ecartmoy_alpha=ecartsumcum_alpha/a
ecartmoy_beta1=ecartsumcum_beta1/a
ecartmoy_beta2=ecartsumcum_beta2/a




################################################################################
########################################################################################
#################### A NE PAS EXECUTER ########################################
#############################################################################
plot(ecartmoy_alpha)
plot(0,0, xlim=c(1,5), ylim=c(-.5,4), 
     axes=F, xlab='', ylab='',
     main="Symboles disponibles")
for (i in 0:4) {
  for (j in 1:5) {
    n <- 5*i+j
    points(j,i, pch=n, cex=3)
    text(j,i-.25, as.character(n))
  }
}?

?

