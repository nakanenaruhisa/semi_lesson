library(survival)
install.packages("compound.Cox")
library(compound.Cox)
install.packages("uni.survival.tree")
library(uni.survival.tree)
library(tidyverse)

data(Lung,package="compound.Cox") 
dat=Lung[which(Lung[,"train"]==TRUE),]

view(dat)

t.vec=dat[,1]
d.vec=dat[,2]
X.mat=dat[,-c(1,2,3)]
test=Lung[which(Lung[,"train"]==FALSE),]
t.test=test[,1]
d.test=test[,2]
X.test=test[,-c(1,2,3)]

KMplot=function(group){ t.vec=group[,1]
d.vec=group[,2]
fit=survfit(Surv(t.vec,d.vec)~1)
plot(fit,conf.int=F,mark.time=T,lwd=3,xlim=c(0,50)) }
tree=uni.tree(t.vec,d.vec,X.mat,P.value=0.01,d0=0.01,score=TRUE)

tree


G1=dat[X.mat[,"IRF4"]>3,]
G2=dat[X.mat[,"IRF4"]<=3&X.mat[,"PON3"]>3,]
G3=dat[X.mat[,"IRF4"]<=3&X.mat[,"PON3"]<=3&X.mat[,"ME3"]<=2 &X.mat[,"HCK"]>3,]
G4=dat[X.mat[,"IRF4"]<=3&X.mat[,"PON3"]<=3&X.mat[,"ME3"]<=2 &X.mat[,"HCK"]<=3,]
G5=dat[X.mat[,"IRF4"]<=3&X.mat[,"PON3"]<=3&X.mat[,"ME3"]>2 &X.mat[,"ODC1"]<=3,]
G6=dat[X.mat[,"IRF4"]<=3&X.mat[,"PON3"]<=3&X.mat[,"ME3"]>2 &X.mat[,"ODC1"]>3&X.mat[,"CYP1B1"]<=1,]
G7=dat[X.mat[,"IRF4"]<=3&X.mat[,"PON3"]<=3&X.mat[,"ME3"]>2 &X.mat[,"ODC1"]>3&X.mat[,"CYP1B1"]>1,]

par(mfrow=c(1,7))
KMplot(G1);KMplot(G2);KMplot(G3);KMplot(G4);KMplot(G5); KMplot(G6);KMplot(G7)
risk.classification(tree,X.mat)# Allocations to Groups 1-7
