#analisis korespondensi

#kepuasan informasi x1
kores = matrix(c(2,3,0,0,12,34,0,0,8,0,0,1,0,1,1),nrow = 5,byrow = TRUE)
dimnames(kores)=list(c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5"),c("Netral","Puas","Sangat Puas"))
names(dimnames(kores))=c("Cluster Responden","Indikator Kepuasan")
kores

#Chi Square
chisq.test(kores)

prop.table(kores,1)
prop.table(kores,2)

library(ca)
fit=ca(kores)
fit

summary(fit)

ankor=function(data){
  total=sum(data)
  totBaris=apply(data, 1,sum)
  totKolom=apply(data, 2,sum)
  p=data/total
  r=totBaris/total
  R=diag(r)
  c=totKolom/total
  C=diag(c)
  S=solve(R)^(0.5)%*%(p-t(t(r))%*%c)%*%solve(C)^(0.5)
  lamda=eigen(S%*%t(S))$values[1:(min(nrow(data),ncol(data))-
                                    1)]
  D=diag(sqrt(lamda))
  prop=c()
  propcum=c()
  for(i in (1:length(lamda))){
    prop=c(prop,lamda[i]/sum(lamda))
    propcum=c(propcum,sum(prop[1:i]))
  }
  if(nrow(data)<ncol(data)){
    U=(eigen(S%*%t(S))$vectors)[,1:(min(nrow(data),ncol(data))-
                                      1)]
    V=solve(D)%*%U%*%S
    Y=solve(R)^(0.5)%*%U%*%D
    Z=solve(C)^(0.5)%*%t(V)%*%D
  }else {
    V=(eigen(S%*%t(S))$vectors)[,1:(min(nrow(data),ncol(data))-
                                      1)]
    U=t(S)%*%V%*%solve(D)
    Y=solve(R)^(0.5)%*%V%*%D
    Z=solve(C)^(0.5)%*%U%*%D
  }
  hasil=list("Inersia"=lamda,"Proporsi
Inersia"=prop,"Proporsi Kumulatif Inersia"=propcum,"Estimasi
Koordinat Utama dari Baris"=Y,"Estimasi Koordinat Utama dari
Kolom"=Z,"Matriks_residual_standar"=S)
  library(ca)
  plot=plot(ca(data))
  return(c(hasil,plot))
}
P=ankor(kores)
P

#kepuasan identitas diri x2
kores = matrix(c(2,3,0,15,29,2,0,0,8,0,0,1,1,0,1),nrow = 5,byrow = TRUE)
dimnames(kores)=list(c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5"),c("Netral","Puas","Sangat Puas"))
names(dimnames(kores))=c("Cluster Responden","Indikator Kepuasan")
kores

#Chi Square
chisq.test(kores)

prop.table(kores,1)
prop.table(kores,2)

library(ca)
fit=ca(kores)
fit

summary(fit)

ankor=function(data){
  total=sum(data)
  totBaris=apply(data, 1,sum)
  totKolom=apply(data, 2,sum)
  p=data/total
  r=totBaris/total
  R=diag(r)
  c=totKolom/total
  C=diag(c)
  S=solve(R)^(0.5)%*%(p-t(t(r))%*%c)%*%solve(C)^(0.5)
  lamda=eigen(S%*%t(S))$values[1:(min(nrow(data),ncol(data))-
                                    1)]
  D=diag(sqrt(lamda))
  prop=c()
  propcum=c()
  for(i in (1:length(lamda))){
    prop=c(prop,lamda[i]/sum(lamda))
    propcum=c(propcum,sum(prop[1:i]))
  }
  if(nrow(data)<ncol(data)){
    U=(eigen(S%*%t(S))$vectors)[,1:(min(nrow(data),ncol(data))-
                                      1)]
    V=solve(D)%*%U%*%S
    Y=solve(R)^(0.5)%*%U%*%D
    Z=solve(C)^(0.5)%*%t(V)%*%D
  }else {
    V=(eigen(S%*%t(S))$vectors)[,1:(min(nrow(data),ncol(data))-
                                      1)]
    U=t(S)%*%V%*%solve(D)
    Y=solve(R)^(0.5)%*%V%*%D
    Z=solve(C)^(0.5)%*%U%*%D
  }
  hasil=list("Inersia"=lamda,"Proporsi
Inersia"=prop,"Proporsi Kumulatif Inersia"=propcum,"Estimasi
Koordinat Utama dari Baris"=Y,"Estimasi Koordinat Utama dari
Kolom"=Z,"Matriks_residual_standar"=S)
  library(ca)
  plot=plot(ca(data))
  return(c(hasil,plot))
}
P=ankor(kores)
P

#kepuasan interaksi sosial x3
kores = matrix(c(2,3,0,0,24,22,0,2,6,0,0,1,2,0,0),nrow = 5,byrow = TRUE)
dimnames(kores)=list(c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5"),c("Netral","Puas","Sangat Puas"))
names(dimnames(kores))=c("Cluster Responden","Indikator Kepuasan")
kores

#Chi Square
chisq.test(kores)

prop.table(kores,1)
prop.table(kores,2)

library(ca)
fit=ca(kores)
fit

summary(fit)

ankor=function(data){
  total=sum(data)
  totBaris=apply(data, 1,sum)
  totKolom=apply(data, 2,sum)
  p=data/total
  r=totBaris/total
  R=diag(r)
  c=totKolom/total
  C=diag(c)
  S=solve(R)^(0.5)%*%(p-t(t(r))%*%c)%*%solve(C)^(0.5)
  lamda=eigen(S%*%t(S))$values[1:(min(nrow(data),ncol(data))-
                                    1)]
  D=diag(sqrt(lamda))
  prop=c()
  propcum=c()
  for(i in (1:length(lamda))){
    prop=c(prop,lamda[i]/sum(lamda))
    propcum=c(propcum,sum(prop[1:i]))
  }
  if(nrow(data)<ncol(data)){
    U=(eigen(S%*%t(S))$vectors)[,1:(min(nrow(data),ncol(data))-
                                      1)]
    V=solve(D)%*%U%*%S
    Y=solve(R)^(0.5)%*%U%*%D
    Z=solve(C)^(0.5)%*%t(V)%*%D
  }else {
    V=(eigen(S%*%t(S))$vectors)[,1:(min(nrow(data),ncol(data))-
                                      1)]
    U=t(S)%*%V%*%solve(D)
    Y=solve(R)^(0.5)%*%V%*%D
    Z=solve(C)^(0.5)%*%U%*%D
  }
  hasil=list("Inersia"=lamda,"Proporsi
Inersia"=prop,"Proporsi Kumulatif Inersia"=propcum,"Estimasi
Koordinat Utama dari Baris"=Y,"Estimasi Koordinat Utama dari
Kolom"=Z,"Matriks_residual_standar"=S)
  library(ca)
  plot=plot(ca(data))
  return(c(hasil,plot))
}
P=ankor(kores)
P

#kepuasan hiburan x4
kores = matrix(c(1,3,1,0,18,28,0,0,8,0,1,0,0,2,0),nrow = 5,byrow = TRUE)
dimnames(kores)=list(c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5"),c("Netral","Puas","Sangat Puas"))
names(dimnames(kores))=c("Cluster Responden","Indikator Kepuasan")
kores

#Chi Square
chisq.test(kores)

prop.table(kores,1)
prop.table(kores,2)

library(ca)
fit=ca(kores)
fit

summary(fit)

ankor=function(data){
  total=sum(data)
  totBaris=apply(data, 1,sum)
  totKolom=apply(data, 2,sum)
  p=data/total
  r=totBaris/total
  R=diag(r)
  c=totKolom/total
  C=diag(c)
  S=solve(R)^(0.5)%*%(p-t(t(r))%*%c)%*%solve(C)^(0.5)
  lamda=eigen(S%*%t(S))$values[1:(min(nrow(data),ncol(data))-
                                    1)]
  D=diag(sqrt(lamda))
  prop=c()
  propcum=c()
  for(i in (1:length(lamda))){
    prop=c(prop,lamda[i]/sum(lamda))
    propcum=c(propcum,sum(prop[1:i]))
  }
  if(nrow(data)<ncol(data)){
    U=(eigen(S%*%t(S))$vectors)[,1:(min(nrow(data),ncol(data))-
                                      1)]
    V=solve(D)%*%U%*%S
    Y=solve(R)^(0.5)%*%U%*%D
    Z=solve(C)^(0.5)%*%t(V)%*%D
  }else {
    V=(eigen(S%*%t(S))$vectors)[,1:(min(nrow(data),ncol(data))-
                                      1)]
    U=t(S)%*%V%*%solve(D)
    Y=solve(R)^(0.5)%*%V%*%D
    Z=solve(C)^(0.5)%*%U%*%D
  }
  hasil=list("Inersia"=lamda,"Proporsi
Inersia"=prop,"Proporsi Kumulatif Inersia"=propcum,"Estimasi
Koordinat Utama dari Baris"=Y,"Estimasi Koordinat Utama dari
Kolom"=Z,"Matriks_residual_standar"=S)
  library(ca)
  plot=plot(ca(data))
  return(c(hasil,plot))
}
P=ankor(kores)
P

