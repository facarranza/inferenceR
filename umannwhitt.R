"
***************************************************************************
umannwhitt.R
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
__copyright__ = '(C) 2019, Felipe Carranza'
#U MANN WHITNEY TEST aproximate by the NORMAL  R STEPS GUIDE
"

#Nonparametric test to compare two independent samples

"When the conditions of application of the t-test for
compare the means of two populations based on samples
independent, you can apply the Mann-Whitney U test.
This test is for. study differences between the central tendency of
two populations where the two samples come from" 

install.packages("tigerstats")
library(tigerstats)

#First it is necessary to join the input vectors, 
#and create a new vector with the category to which each element belongs, Example:
ni=c(86 , 69 , 72 , 65 , 113 , 65 , 118 , 45 , 141 , 104 , 40, 50,55 , 40 , 22 , 58 , 16 , 7 , 9 , 16 , 26 , 36 , 20 , 13)
#two categorys:
grp= c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2)
#confidence level
p=0.95  #95%
#Create a dataframe

M=data.frame(ni,grp)
#Order asc
M= M[order(M$ni),]
N=dim(M)[1]
#create the rank
(rank=c(1:dim(M)[1]))
M = data.frame(M,rank)
#to find the mean
D=by(M$rank,M$ni, mean)
D2=as.matrix(D)
D3=rownames(D)
#New matrix with two col: id and mean
D2=cbind(as.numeric(D3),D2)
N2=dim(D2)[1]

#Searching and replacing old rank  by the mean 
for(i in 1:N){
  for(j in 1:N2){
    if(M$ni[i]==D2[j]){
      M$rank[i]=as.numeric(D2[j,2])
    }
  }
}
M
#Sum by groups
Sumbygroup=by(M$rank,M$grp,sum)
#Finding the R values
(R1=as.numeric(Sumbygroup[1]))
(R2=as.numeric(Sumbygroup[2]))

#The U values
(U1=(n1*n2 ) + (n1*(n1+1)/2) -R1)
(U2=(n1*n2 ) + (n2*(n2+1)/2) -R2)

#aproximate to normal
(media=(n1*n2)/2)
(sd=sqrt(((n1*n2)*(n1+n2+1))/12))
#Use the min value to find Z value
(z=(min(U1,U2) - media)/sd)

#is condition for EQUAL
c=(1-p)
ct=(1-(c/2))
#z value for normal distribution
(zt=qnorm(ct))
#zt=tinv(ct,n)

#Now comparing the two probability grahps
pnormGC(zt, region="below",graph=TRUE)
pnormGC(abs(z), region="below",graph=TRUE)

#Aprove or reject Ho
ifelse(abs(z)>abs(zt),print("The Null hypothesis is rejected"),print("The Null Hypothesis is accepted"))

#Comparing the result test  with Rcommand
wilcox.test(ni~grp, data=M, paired = FALSE)
