#Explanatory variables
traits<-matrix(nrow=5,ncol=5,abs(rnorm(25)))
habitat<-matrix(nrow=5,ncol=5,abs(rnorm(25)))

#Network
birds_flowers<-matrix(nrow=5,ncol=5,rpois(25,40))


models<-list(traits,habitat)
AICt<-lapply(models,function(x){
  LogLiklihood<-dmultinom(birds_flowers,prob=x,log=TRUE)
  -2*LogLiklihood + 2*1
})
names(AICt)<-c("traits","habitat")
AICt

