#########################################################
## On va estimer par MLE les coeff présents pour ########
##                PSI0 = Identité                ########
##                PSI1 = PSI1_step               ########
#########################################################

#########################################################
## Pour pouvoir être exécuté correctement, on doit  #####
## d'abord lancer Preparation_MLE avec step ==1     #####
#########################################################



#on rédéfinit le point de départ de mle et optim 
init0=c(-10,-1/9,-4)
init_tot=abs(c(-5,-6,10,12,45,64,-4,-7,32,37,48,59)) #j'ai limpression qu'il faut des trucs très très négatifs pour ne pas aller à l'infini

init = init_tot

#log_like=function (beta,resi_, carac, contrat_, k=0,step1,step2,step3,step4)

Vminuslike_tot=function (beta1_s,beta2_s,step1_s,step2_s,step3_s,step4_s,beta1_c,beta2_c,step1_c,step2_c,step3_c,step4_c)
{
  beta_s=c(beta1_s,beta2_s)
  beta_c=c(beta1_c,beta2_c)
  return (-log_like(beta_s,resi_clean_s,caracteristique_clean_s,contrat_clean_s,step1_s,step2_s,step3_s,step4_s)
          - log_like(beta_c, resi_clean_c, caracteristique_clean_c, contrat_clean_c,step1_c,step2_c,step3_c,step4_c)
  )
}

Vminuslikev_tot=function (X)
{
  X_s  = X[1:6]
  X_c = X[7:12]
  return (Vminuslike_tot(X_s[1],X_s[2],X_s[3],X_s[4],X_s[5],X_s[6],X_c[1],X_c[2],X_c[3],X_c[4],X_c[5],X_c[6]))
  
}

fun = function(X)
{
  X_s  = X[1:6]
  X_c = X[7:12]
  return (-Vminuslike_tot(X_s[1],X_s[2],X_s[3],X_s[4],X_s[5],X_s[6],X_c[1],X_c[2],X_c[3],X_c[4],X_c[5],X_c[6]))
  
}



papatry = try(mle(Vminuslike_tot,start=list(beta1_s=init[1],beta2_s=init[2],step1_s=init[3],step2_s=init[4],step3_s=init[5],step4_s=init[6],beta1_c=init[7],beta2_c=init[8],step1_c=init[9],step2_c=init[10],step3_c=init[11],step4_c=init[12]),method="BFGS"),silent = T)


nbessais = 0
nbessais_max = 50

while( (class(papatry)=="try-error") & (nbessais <=nbessais_max) ){
  
  init[1] = init[1] + ((-1)^(nbessais))*1*(nbessais/3)
  init[2] = init[2] + ((-1)^(nbessais))*5*(nbessais)
  init[3] = init[3] + ((-1)^(nbessais))* 2*(nbessais/3)
  init[4] = init[4] + ((-1)^(nbessais))*1*(nbessais/3)
  init[5] = init[5] + ((-1)^(nbessais))*5*(nbessais)
  init[6] = init[6] + ((-1)^(nbessais))*2*(nbessais/3)
  init[7] = init[7] + ((-1)^(nbessais))*2*(nbessais/3)
  init[8] = init[8] + ((-1)^(nbessais))*2*(nbessais/3)
  init[9] = init[9] + ((-1)^(nbessais))*2*(nbessais/3)
  init[10] = init[10] + ((-1)^(nbessais))*2*(nbessais/3)
  init[11] = init[11] + ((-1)^(nbessais))*2*(nbessais/3)
  init[12] = init[12] + ((-1)^(nbessais))*2*(nbessais/3)
  
  papatry = try(mle(Vminuslike_tot,start=list(beta1_s=init[1],beta2_s=init[2],step1_s=init[3],step2_s=init[4],step3_s=init[5],step4_s=init[6],beta1_c=init[7],beta2_c=init[8],step1_c=init[9],step2_c=init[10],step3_c=init[11],step4_c=init[12]),method="BFGS"),silent = T)
  nbessais = nbessais+1
  
}

papa=mle(Vminuslike_tot,start=list(beta1_s=init[1],beta2_s=init[2],step1_s=init[3],step2_s=init[4],step3_s=init[5],step4_s=init[6],beta1_c=init[7],beta2_c=init[8],step1_c=init[9],step2_c=init[10],step3_c=init[11],step4_c=init[12]),method="BFGS")

mama = optim(init, Vminuslikev_tot)


#on compare mle et optim en comparant la valeur de la log-like
if(-logLik(papa)<mama$value){
  print("MLE meilleur qu'optim")
  
}

if(-logLik(papa)>=mama$value){
  print("optim meilleur que MLE")
}


estimle = NULL
estimle[1]=papa@coef[[1]]
estimle[2]=papa@coef[[2]]
estimle[3]=papa@coef[[3]]
estimle[4]=papa@coef[[4]]
estimle[5]=papa@coef[[5]]
estimle[6]=papa@coef[[6]]
estimle[7]=papa@coef[[7]]
estimle[8]=papa@coef[[8]]
estimle[9]=papa@coef[[9]]
estimle[10]=papa@coef[[10]]
estimle[11]=papa@coef[[11]]
estimle[12]=papa@coef[[12]]





varestim = NULL
varestim[1]=vcov(papa)[1,1]
varestim[2]=vcov(papa)[2,2]
varestim[3]=vcov(papa)[3,3]
varestim[4]=vcov(papa)[4,4]
varestim[5]=vcov(papa)[5,5]
varestim[6]=vcov(papa)[6,6]
varestim[7]=vcov(papa)[7,7]
varestim[8]=vcov(papa)[8,8]
varestim[9]=vcov(papa)[9,9]
varestim[10]=vcov(papa)[10,10]
varestim[11]=vcov(papa)[11,11]
varestim[12]=vcov(papa)[12,12]


beta1estim = c(estimle[1],estimle[7])
beta2estim = c(estimle[2],estimle[8])
step1estim = c(estimle[3],estimle[9])
step2estim = c(estimle[4],estimle[10])
step3estim = c(estimle[5],estimle[11])
step4estim = c(estimle[6],estimle[12])

beta1estim
sqrt(c(varestim[1],varestim[7]))

beta2estim
sqrt(c(varestim[2],varestim[8]))

step1estim
sqrt(c(varestim[3],varestim[9]))

step2estim
sqrt(c(varestim[4],varestim[10]))

step3estim
sqrt(c(varestim[5],varestim[11]))

step4estim
sqrt(c(varestim[6],varestim[12]))

estimlem = mama$par

beta1estimm = c(estimlem[1],estimlem[7])
beta2estimm = c(estimlem[2],estimlem[8])
step1estimm = c(estimlem[3],estimlem[9])
step2estimm = c(estimlem[4],estimlem[10])
step3estimm = c(estimlem[5],estimlem[11])
step4estimm = c(estimlem[6],estimlem[12])

estimlem


#############################################
## Autre méthode de MLE qui semble marcher ##
## Le summary est plutot joli !##############
#############################################

maxl = maxLik(logLik = fun, start = c(beta1_s=init[1],beta2_s=init[2],step1_s=init[3],step2_s=init[4],step3_s=init[5],step4_s=init[6],beta1_c=init[7],beta2_c=init[8],step1_c=init[9],step2_c=init[10],step3_c=init[11],step4_c=init[12]),method="NM")
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
maxl = maxLik(logLik = fun, start = c(beta1_s=init[1],beta2_s=init[2],step1_s=init[3],step2_s=init[4],step3_s=init[5],step4_s=init[6],beta1_c=init[7],beta2_c=init[8],step1_c=init[9],step2_c=init[10],step3_c=init[11],step4_c=init[12]),method="NM")


nbupdate = nbupdate -1
}

summary(maxl)