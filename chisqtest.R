"
***************************************************************************
chisqtest.R
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

#Chi-square independence Test
"

#Simple form to explain the test, maybe you can  would be improve it for stantandar use
"Among all the applications offered by the chi-square distribution, is the
proof of independence the one with the highest employment."
#Creating inputs
c1=c(27,47,52)
c2=c(42,32,48)


cn=rbind(c1,c2)
m= as.table((cn))
m
#Adding total
(m2=addmargins(m))
#totalrow=sum()            
#r=2
#k=3

n=sum(m)
#The new matrix
e=matrix()
total=matrix()
#find values
"It can be shown that if He is true, that is, if there is independence between R and C, the frequencies
expected from the different boxes can be estimated by multiplying the total row by the total of
column and dividing by the grand total"
for(i in 1:2){
  for(j in 1:3){
    e[i,j]=(m2[3,j]*m2[i,4])/n
  }
}
"The contrast statistic used measures the agreement between the observed and expected frequencies.
and is calculated by the formula"
#find chi i j
for(i in 1:2){
  for(j in 1:3){
    total[i,j]=(m[i,j]-e[i,j])^2/ e[i,j]
  }
}

#find the chi square by confidences 
chi=sum(total)
n=length(c1)
alpha <- 0.05
q <- qchisq(1 - alpha/2, df = n - 1)


#Aprove or reject Ho
ifelse(abs(chi)>abs(q),print("The Null hypothesis is rejected"),print("The Null Hypothesis is accepted"))

#Comparing the result test  with Rcommand
chisq.test(c1,c2)
