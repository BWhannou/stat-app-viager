#########################################################
## On va estimer par MLE les coeff présents pour ########
##                PSI0 = Identité                ########
##                PSI1 = PSI1_exp                ########
#########################################################

#########################################################
## Pour pouvoir être exécuté correctement, on doit  #####
## d'abord lancer Preparation_MLE avec expo ==1     #####
#########################################################


#on rédéfinit le point de départ de mle et optim 
init=c(-10,-1/9,-4)


Vminuslike_tot=function (alpha_s,beta1_s,beta2_s,alpha_c,beta1_c,beta2_c)
{
  beta_s=c(beta1_s,beta2_s)
  beta_c=c(beta1_c,beta2_c)
  return (-log_like(alpha_s,beta_s,resi_clean_mat[,1],caracteristique_clean_s,contrat_clean)
          - log_like(alpha_c, beta_c, resi_clean_mat[,2], caracteristique_clean_c, contrat_clean)
          )
}

Vminuslikev_tot=function (X)
{
  X_s  = X[1:3]
  X_c = X[4:6]
  return (Vminuslike(X_s[1],X_s[2],X_s[3],X_c[1],X_c[2],X_c[3]))
  
}



papatry = try(mle(Vminuslike_tot,start=list(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[1],beta1_c=init[2],beta2_c=init[3]),method="BFGS"),silent = T)


nbessais = 0
nbessais_max = 20

while( (class(papatry)=="try-error") & (nbessais <=nbessais_max) ){
  
  init[1] = init[1] + ((-1)^(nbessais))*0.1*(nbessais/3)
  init[2] = init[2] + ((-1)^(nbessais))*0.05*(nbessais)
  init[3] = init[3] + ((-1)^(nbessais))* 0.2*(nbessais/3)
  
  papatry = try(mle(Vminuslike_tot,start=list(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[1],beta1_c=init[2],beta2_c=init[3]),method="BFGS"),silent = T)
  nbessais = nbessais+1
  
}

papa=mle(Vminuslike_tot,start=list(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[1],beta1_c=init[2],beta2_c=init[3]),method="BFGS")

mama = optim(c(init,init), Vminuslikev)

#on compare mle et optim en comparant la valeur de la log-like
if(logLik(papa)>mama$value){
  
}


Vminuslike_tot(init[1],init[2],init[3],init[1],init[2],init[3])
#log_like(init[1],init[2:3],resi_clean,caracteristique_clean,contrat_clean)


estimle = NULL
estimle[1]=papa@coef[[1]]
estimle[2]=papa@coef[[2]]
estimle[3]=papa@coef[[3]]
estimle[4]=papa@coef[[4]]
estimle[5]=papa@coef[[5]]
estimle[6]=papa@coef[[6]]


varestim = NULL
varestim[1]=vcov(papa)[1,1]
varestim[2]=vcov(papa)[2,2]
varestim[3]=vcov(papa)[3,3]
varestim[4]=vcov(papa)[4,4]
varestim[5]=vcov(papa)[5,5]
varestim[6]=vcov(papa)[6,6]

alphaestim=c(estimle[1],estimle[4]) #alphaestim = alpha_s,alpha_c
beta1estim=c(estimle[2],estimle[5])
beta2estim=c(estimle[3],estimle[6])


alphaestim
sqrt(c(varestim[1],varestim[4]))

beta1estim
sqrt(c(varestim[2],estimle[5]))

beta2estim
sqrt(c(varestim[3],estimle[6]))


estimlem = mama$par
alphaestimm = c(estimlem[1],estimlem[4])
beta1estimm = c(estimlem[2],estimlem[5])
beta2estimm = c(estimlem[3],estimlem[6])
estimlem 

#on ne stocke pas les résultats dans les mêmes variables suivant la valeur de clone


