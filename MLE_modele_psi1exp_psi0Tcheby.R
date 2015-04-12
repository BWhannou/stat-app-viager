#########################################################
## On va estimer par MLE les coeff présents pour ########
##                PSI0 = PolTcheb                ########
##                PSI1 = PSI1_exp                ########
#########################################################

#########################################################
## Pour pouvoir être exécuté correctement, on doit  #####
## d'abord lancer Preparation_MLE avec expo ==1     #####
## et avec Tcheby =1                                #####
#########################################################

## On définit le degré du polynôme de Tchebychev a priori
n_Tcheby = 5 


#on rédéfinit le point de départ de mle et optim 
init0=c(-10,-1/9,-4)
init_tot=c(-10,-2,-5,-171,-2,-3) #j'ai limpression qu'il faut des trucs très très négatifs pour ne pas aller à l'infini

init = c(init0,init0)

nb_param = length(init)


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



estimle = NULL
varestim = NULL


for (i in 1:nb_param){
  estimle[i]=papa@coef[[i]]
  varestim[i]=vcov(papa)[i,i]
}


alphaestim=c(estimle[1],estimle[4]) #alphaestim = alpha_s,alpha_c
beta1estim=c(estimle[2],estimle[5])
beta2estim=c(estimle[3],estimle[6])



estimlem = mama$par
alphaestimm = c(estimlem[1],estimlem[4])
beta1estimm = c(estimlem[2],estimlem[5])
beta2estimm = c(estimlem[3],estimlem[6])
estimlem 

#############################################
## Autre méthode de MLE qui semble marcher ##
## Le summary est plutot joli !##############
#############################################

#init  = estimle

maxl = maxLik(logLik = fun, start = c(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[4],beta1_c=init[5],beta2_c=init[6]),method="NM")
summary(maxl)


nbupdate = 3
while ((nbupdate>0) ){
  
  for (i in 1:nb_param){
    init[i] = maxl$estimate[[i]]
  }
  
  maxl = maxLik(logLik = fun, start = c(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[4],beta1_c=init[5],beta2_c=init[6]),method="NM")
  
  
  nbupdate = nbupdate -1
}

summary(maxl)

papa=mle(Vminuslike_tot,start=list(alpha_s=init[1],beta1_s=init[2],beta2_s=init[3],alpha_c=init[4],beta1_c=init[5],beta2_c=init[6]),method="BFGS")
summary(papa)

for (i in 1:nb_param{
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

allequal = NULL
for(i in 1:(nb_param/2)){
  allequal[i]=all.equal(maxl$estimate[[i]],maxl$estimate[[i+ (nb_param/2)]])
}

allequal


##On essaie également un test similaire à celui d'économétrie pour tester l'égalité de deux coeff d'une regresssion linéaire##

ttest = NULL
for( i in 1:(nb_param/2)){
  
  ttest[i] = abs((maxl$estimate[[i]]-maxl$estimate[[i+(nb_param)/2]])/sqrt(  vcov(maxl)[i,i]+vcov(maxl)[i+(nb_param)/2,i+(nb_param)/2] - 2*vcov(maxl)[i,i+(nb_param)/2]  ))

}



#ttest1 = abs((maxl$estimate[[1]]-maxl$estimate[[4]])/sqrt(  vcov(maxl)[1,1]+vcov(maxl)[4,4] - 2*vcov(maxl)[1,4]  ))

## On se place au niveau alpha##
alpha  = 0.05

rejet = qnorm(1-alpha/2)

liste_nom_coef = c("alpha seller","beta1 seller", "beta2 seller", "alpha clone","beta1 clone", "beta2 clone" )
non_egal = "!!!=!!!"
egal = ~~~~~


for (i in 1:length(ttest)){

if(ttest[i]>rejet){
  print(c(liste_nom_coef[i],non_egal,liste_nom_coef[i+(nb_param/2)]))  
}

if (ttest[i]<=rejet){
  print(c(liste_nom_coef[i],egal,liste_nom_coef[i+(nb_param/2)]))
  
}
}



