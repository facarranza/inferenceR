"
***************************************************************************
KruskalWallis.R
---------------------
Date                 : December 2019
Copyright            : (C) 2019 by Felipe Carranza
Email                : fcarranza@protonmail.com
***************************************************************************
*                                                                         *
*   This program is free software; you can redistribute it and/or modify  *
*   it under the terms of the MIT  License *
*                                                                         *
*                                                                         *
***************************************************************************

__author__ = 'Felipe Carranza'
__date__ = 'December 2019'
__copyright__ = '(C) 2019, Felipe Carranza'"

#Kruskal-Wallis

#When three or more populations are compared, the known test is used as the H-test or Kruskal-Wallis test

####################################
##First it is necessary to join the input vectors, 
#and create a new vector with the category to which each element belongs, Example:

#Inputs
val=c(17,20,40,31,25,8,7,9,8,7,5,4,7)
group = c(1,1,1,1,1,2,2,2,2,3,3,3,3)
df=length(as.numeric(levels(factor(group))))-1 #total groups -1
#confidence level
probaconf=0.95
#####################################
#Create a dataframe
(mx=data.frame(val, group))


#my own test
pos=1
id=c(1:length(val))

#Order
mx=mx[order(mx$val),]

#Add id 
for(i in 1:length(val)){
  if(i>1){
    if(mx$val[i]==mx$val[i-1]){
      id[i]=pos
    
    }
    else{
      pos=i
      id[i]=pos
      }
    }
  else{
   id[i]=pos
    
  }
  
}
Nh=dim(mx)[1]
id2=c(1:Nh)
#add id to mx
mx = data.frame(mx,id,id2)
#Updating id2 with mean
D=by(mx$id2,mx$id, mean)
#transforming
D2=as.matrix(D)
D3=rownames(D)
#New matrix with two col id and mean
D2=cbind(as.numeric(D3),D2)

N2=dim(D2)[1]
#Searching and replacing old id2  by the mean 
for(i in 1:N){
  for(j in 1:N2){
    if(mx$id[i]==D2[j]){
      mx$id2[i]=as.numeric(D2[j,2])
    }
  }
}
mx

#R value
R=by(mx$id2,mx$group, sum)
R[1]
#L value
L=by(mx$id,mx$group, length)

#Formula:
factorH1 = (12 / (Nh*(Nh+1)))
factorH2 = sum((R^2)/L)
factorH3 = (3*(Nh+1))
#H value
H = (factorH1*factorH2) - factorH3

ifelse(H>qchisq(probaconf,df=df),print("The Null hypothesis is rejected"),print("The Null Hypothesis is accepted"))

#Comparing the result test  with Rcommand
kruskal.test(val~group)