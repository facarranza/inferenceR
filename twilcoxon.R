"
***************************************************************************
twilcoxon.R
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

#T Wilconxon  TEST aproximate by the NORMAL  R STEPS GUIDE
"
#Nonparametric test to compare two paired samples

"This is a non-parametric test that replaces the t-test of paired data
to study the central tendency of the populations origin of the samples."

#install.packages("tigerstats")
library(tigerstats)

#Paired data, for example two treatments
A=c(5,4,3,5,2,4,2,2,4,4,3,1,5,3,1)
B=c(2,2,0,3,3,2,3,1,1,3,4,2,2,4,0)
order = seq(1:length(A))

#confidence level
p=0.95
n=length(A)
#diff
resta=A-B
resta
#absolute value
restaab=abs(resta)
#rank=c(rep(0,length(sindescarga)))

#Create a dataframe
M = data.frame(order,A,B,resta,restaab)
#Order asc
M=M[order(restaab),]
#Not 0 values
M=(M[M$restaab != "0",])
N=dim(M)[1]
#Create the rank
(rank=c(1:dim(M)[1]))
M = data.frame(M,rank)
#to find the mean
D=by(M$rank,M$restaab, mean)
D2=as.matrix(D)
D3=rownames(D)

#New matrix with two col id and mean
D2=cbind(as.numeric(D3),D2)
N2=dim(D2)[1]

#Searching and replacing old rank by the mean 
for(i in 1:N){
  for(j in 1:N2){
    if(M$restaab[i]==D2[j]){x
      M$rank[i]=as.numeric(D2[j,2])
    }
  }
}

#preparing for group by signal positive or negative
M$signal = ifelse(M$resta > 0,1,-1) 
Sumbygroup=by(M$rank,M$signal,sum)
Comparecases=by(M$rank,M$signal,length)
T=ifelse(Comparecases[1]>Comparecases[2], Sumbygroup[2],Sumbygroup[1])

#aproximate to normal
u=(n*(n+1))/4
ds=sqrt(  (n*(n+1)*((2*n)+1))   /   24 )
z=(T-u)/ds

#is condition for EQUAL
c=(1-p)
ct=(1-(c/2))
#z value for normal distribution
zt=qnorm(ct)

#Now comparing the two probability grahps
pnormGC(zt, region="below",graph=TRUE)
pnormGC(abs(z), region="below",graph=TRUE)


#Aprove or reject Ho
ifelse(abs(z)>abs(zt),print("The Null hypothesis is rejected"),print("The Null Hypothesis is accepted"))

#Comparing the result test  with Rcommand
wilcox.test(A,B, paired = TRUE,conf.level = 0.90)

#https://www.rdocumentation.org/packages/stats/versions/3.6.1/topics/wilcox.test
#https://bookdown.org/dietrichson/metodos-cuantitativos/wilcoxon-signed-rank-test.html