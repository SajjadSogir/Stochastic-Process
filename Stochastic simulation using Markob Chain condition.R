library(diagram)
# 1.Generation of a realization of the process
n=1000
x=matrix(0,n,1)
set.seed(2022)
u=runif(n,0,1)
a=b=c=1/3
x[1]=ifelse(u[1]<a,1,ifelse(u[1]<(a+b),2,3))
for(i in 1:(n-1)){
  if(x[i]==1){
    a=2/5
    b=1/2
    c=1/10
  }
  if(x[i]==2){
    a=1/5
    b=7/10
    c=1/10
  }
  if(x[i]==3){
    a=2/5
    b=2/5
    c=1/5
  }
  x[i+1]=ifelse(u[i+1]<a,1,ifelse(u[i+1]<(a+b),2,3))
}
U<-round(u,3)
colnames(x)[1]<-"X"
head(cbind(U,x))
#2.Transition  matrix
n=length(x)
TM<-matrix(0,3,3,1)
for(i in 1:(n-1)){
  if(x[i]==1 & x[i+1]==1){TM[1,1]=TM[1,1]+1}
  if(x[i]==1 & x[i+1]==2){TM[1,2]=TM[1,2]+1}
  if(x[i]==1 & x[i+1]==3){TM[1,3]=TM[1,3]+1}
  if(x[i]==2 & x[i+1]==1){TM[2,1]=TM[2,1]+1}
  if(x[i]==2 & x[i+1]==2){TM[2,2]=TM[2,2]+1}
  if(x[i]==2 & x[i+1]==3){TM[2,3]=TM[2,3]+1}
  if(x[i]==3 & x[i+1]==1){TM[3,1]=TM[3,1]+1}
  if(x[i]==3 & x[i+1]==2){TM[3,2]=TM[3,2]+1}
  if(x[i]==3 & x[i+1]==3){TM[3,3]=TM[3,3]+1}
}
TM
# 3.Transition probability matrix
k<-NULL
for( i in 1:3){
  k[i]<-sum(TM[i,])
  TM[i,]<-TM[i,]/k[i]
}
TM
# 4.Diagram
plotmat(TM[1:3,1:3],main="Diagram")