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
init0=c(-10,-1/9,-4)
init_tot=c(-10,-2,-5,-171,-2,-3) #j'ai limpression qu'il faut des trucs très très négatifs pour ne pas aller à l'infini

init = c(init0,init0)


#on indique si on souhaite procéder sur des sous-bases de la base initiale
sous_base=0
nbase=200


############################################################################################
##En cas de bug,on peut essayer de tirer des sous-bases de la base départ via une uniforme##
############################################################################################
if(sous_base==1){

#on tire le numéro des individus à retenir
a_tirer_s = floor(runif(nbase,0,length(contrat_clean_s)+1))
a_tirer_c = floor(runif(nbase,0,length(contrat_clean_c)+1))

resi_clean_s = resi_clean_s[a_tirer_s]
resi_clean_c = resi_clean_c[a_tirer_c]

caracteristique_clean_s = caracteristique_clean_s[a_tirer_s]
caracteristique_clean_c = caracteristique_clean_c[a_tirer_c]

contrat_clean_s = contrat_clean_s[a_tirer_s]
contrat_clean_c = contrat_clean_c[a_tirer_c]
}


Vminuslike_tot=function (alpha_s,beta1_s,beta2_s,alpha_c,beta1_c,beta2_c)
{
  beta_s=c(beta1_s,beta2_s)
  beta_c=c(beta1_c,beta2_c)
  return (-log_like(alpha_s,beta_s,resi_clean_s,caracteristique_clean_s,contrat_clean_s)
          - log_like(alpha_c, beta_c, resi_clean_c, caracteristique_clean_c, contrat_clean_c)
          )
}

Vminuslikev_tot=function (X)
{
  X_s  = X[1:3]
  X_c = X[4:6]
  return (Vminuslike_tot(X_s[1],X_s[2],X_s[3],X_c[1],X_c[2],X_c[3]))
  
}

fun = function(X)
{
  X_s  = X[1:3]
  X_c = X[4:6]
  return (-Vminuslike_tot(X_s[1],X_s[2],X_s[3],X_c[1],X_c[2],X_c[3]))
  
}
  
papatry = try(mle(Vminuslike_tot,start=list(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[4],beta1_c=init[5],beta2_c=init[6]),method="BFGS"),silent = T)


nbessais = 0
nbessais_max = 50

while( (class(papatry)=="try-error") & (nbessais <=nbessais_max) ){
  
  init[1] = init[1] + ((-1)^(nbessais))*1*(nbessais/3)
  init[2] = init[2] + ((-1)^(nbessais))*5*(nbessais)
  init[3] = init[3] + ((-1)^(nbessais))* 2*(nbessais/3)
  init[4] = init[4] + ((-1)^(nbessais))*1*(nbessais/3)
  init[5] = init[5] + ((-1)^(nbessais))*5*(nbessais)
  init[6] = init[6] + ((-1)^(nbessais))*2*(nbessais/3)
  
  papatry = try(mle(Vminuslike_tot,start=list(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[4],beta1_c=init[5],beta2_c=init[6]),method="BFGS"),silent = T)
  nbessais = nbessais+1
  
}

papa=mle(Vminuslike_tot,start=list(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[4],beta1_c=init[5],beta2_c=init[6]),method="BFGS")

mama = optim(init, Vminuslikev_tot)

#on compare mle et optim en comparant la valeur de la log-like
if(-logLik(papa)<mama$value){
  print("MLE meilleur qu'optim")
  
}

if(-logLik(papa)>=mama$value){
  print("optim meilleur que MLE")
}



Vminuslike_tot(init[1],init[2],init[3],init[4],init[5],init[6])
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

#############################################
## Autre méthode de MLE qui semble marcher ##
## Le summary est plutot joli !##############
#############################################

maxl = maxLik(logLik = fun, start = c(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[4],beta1_c=init[5],beta2_c=init[6]),method="NM")
summary(maxl)


nbupdate = 5
while ((nbupdate>0) ){
  
  init[1] = maxl$estimate[[1]]
  init[2] = maxl$estimate[[2]]
  init[3] = maxl$estimate[[3]]
  init[4] = maxl$estimate[[4]]
  init[5] = maxl$estimate[[5]]
  init[6] = maxl$estimate[[6]]

  maxl = maxLik(logLik = fun, start = c(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[4],beta1_c=init[5],beta2_c=init[6]),method="NM")
  
  
  nbupdate = nbupdate -1
}

summary(maxl)

papa=mle(Vminuslike_tot,start=list(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[4],beta1_c=init[5],beta2_c=init[6]),method="BFGS")
summary(papa)

for (i in 1:length(papa@coef)){
  init[i] = papa@coef[[i]]
}

#on finit par maxLik pour avoir directement les test de significativité

maxl = maxLik(logLik = fun, start = c(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[4],beta1_c=init[5],beta2_c=init[6]),method="NM")
summary(maxl)
vcov(maxl)
confint(maxl)



####################################################################
## Tous les coeff sont significatifs!!!!!!!!!#######################
####################################################################

##############################################################
## Il faut maintenant tester l'égalité seller/clone ##########
##############################################################

##D'après les auteurs du package on peut utiliser all.equal##
##S'il ne renvoie pas TRUE, on peut consiérer que c'est différent##

all.equal(maxl$estimate[[1]],maxl$estimate[[4]])
all.equal(maxl$estimate[[2]],maxl$estimate[[5]])
all.equal(maxl$estimate[[3]],maxl$estimate[[6]])

##On essaie également un test similaire à celui d'économétrie pour tester l'égalité de deux coeff d'une regresssion linéaire##

ttest1 = abs((maxl$estimate[[1]]-maxl$estimate[[4]])/sqrt(  vcov(maxl)[1,1]+vcov(maxl)[4,4] - 2*vcov(maxl)[1,4]  ))
ttest2 = abs((maxl$estimate[[2]]-maxl$estimate[[5]])/sqrt(  vcov(maxl)[2,2]+vcov(maxl)[5,5] - 2*vcov(maxl)[2,5]  ))  
ttest3 = abs((maxl$estimate[[3]]-maxl$estimate[[6]])/sqrt(  vcov(maxl)[3,3]+vcov(maxl)[6,6] - 2*vcov(maxl)[3,6]  ))

## On se place au niveau alpha##
alpha  = 0.05

rejet = qnorm(1-alpha/2)


if(ttest1>rejet){
  print("##alpha seller !!!=!!! alpha clone##")
  
}

if (ttest1<=rejet){
  print("##alpha seller ~~~~~ alpha clone##")
  
}


if(ttest2>rejet){
  print("##beta1 seller !!!=!!! beta1 clone##")
  
}

if (ttest2<=rejet){
  print("##beta1 seller ~~~~~ beta1 clone##")
  
}

if(ttest3>rejet){
  print("##beta2 seller !!!=!!! beta2 clone##")
  
}

if (ttest3<=rejet){
  print("##beta2 seller ~~~~~ beta2 clone##")
  
}

##On rejette toutes les égalités##