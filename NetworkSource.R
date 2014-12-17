#Network Source Functions
#Create function to compute network parameters
#The general strategy is to write all metrics to file, and develop call statements at the end to retrieve them
NetworkC<-function(datf,naming,plots=F){
  
  #get a directory
  toset<-paste(gitpath,"/Figures/",naming,sep="","/")
  
  dir.create(toset,recursive=TRUE)
  
  setwd(toset)
  #remove species without
  #Drop any observations without 2 names in Iplant_Double
  datf<-droplevels(datf[!datf$Iplant_Double %in% c("",NA),])
  
  #drop singletons
  #datf<-datf[!datf$Iplant_Double %in% names(which(table(datf$Iplant_Double)==1)),]
  #camera data needs to be standardized
  camera<-datf[!is.na(datf$ID),]
  
  #Sampling evaluation
  #How many repeat sampling events per month
  #dplyr doesn't like this date column
  a<-camera[,!colnames(camera) %in% "DateP"]
  if(!nrow(a)==0){
  h<-group_by(a,Iplant_Double,Hummingbird) %>%  dplyr::summarize(sum=length(Hummingbird))
  n<-group_by(a,Iplant_Double) %>% dplyr::summarise(N=nlevels(droplevels(ID)))

  camdat<-merge(h,n)
  camdat$mean<-camdat$sum/camdat$N
  }
  
  #standardize for # of transects.
  tran<-datf[is.na(datf$ID),]
  a<-tran[,!colnames(tran) %in% "DateP"]
  
  if(!nrow(a)==0){
  h<-group_by(a,Iplant_Double,Hummingbird) %>%  dplyr::summarize(sum=length(Hummingbird))
  n<-group_by(a,Iplant_Double) %>% dplyr::summarise(N=nlevels(droplevels(as.factor(Date))))
  
  trandat<-merge(h,n)
  trandat$mean<-trandat$sum/trandat$N
}
  if(exists("camdat")){
    finaldat<-merge(camdat,trandat,by=c("Iplant_Double","Hummingbird"),all=T)
} else {
  #make a dummy variable to match syntax
  finaldat<-trandat
  colnames(finaldat)[  colnames(finaldat) %in% "mean"]<-"mean.y"
  finaldat$mean.x<-NA
}
  #fill Na's with 0's
  finaldat$mean.x[is.na(finaldat$mean.x)]<-0
  finaldat$mean.y[is.na(finaldat$mean.y)]<-0
  
  finalmatrix<-select(finaldat,Iplant_Double,Hummingbird,mean.x,mean.y) %>% dplyr::mutate(value=mean.x+mean.y) %>%
  select(Iplant_Double,Hummingbird,value)
  F_H<-acast(finalmatrix,Iplant_Double~Hummingbird,fill=0)

#Turn into a binary network
F_H<- (F_H>0) * 1

  #Below the line is more detectable on the cameras
  ggplot(finaldat,aes(x=mean.x,y=mean.y)) + geom_point() + geom_smooth(method="lm")
  
#Create Interaction of flowers and birds matrix

#Uncorrected for sampling:
#F_H<-as.data.frame.array(table(datf$Iplant_Double,datf$Hummingbird))
  
  #Save Input Matrix
  write.csv(F_H,"BirdXFlower.csv")
  
  #create a order for hummingbirds
  toOrd<-merge(clades,data.frame(English=colnames(F_H)),sort=FALSE,all.y=TRUE)$English
  
  #create a order for plants
  Hlab<-names(which(!apply(F_H,2,sum)==0))
  
  toOrd<-as.character(merge(clades,data.frame(English=Hlab),sort=FALSE,all.y=TRUE)$English)
  
  Plab<-names(which(!apply(F_H,1,sum)==0))
  
  sequ<-list(seq.high=toOrd,seq.low=Plab)

  #Interaction matrix
  
  orderflowers<-names(sort(apply(F_H,1,sum),decreasing=FALSE))
  
  orderbirds<-names(sort(apply(F_H,2,sum),decreasing=TRUE))
  
  a<-melt(as.matrix(F_H))
  
  colnames(a)<-c("Flowers","Birds","value")
  
  a$Flowers<-factor(a$Flowers,levels=orderflowers)
  a$Birds<-factor(a$Birds,levels=orderbirds)
  
  p<-ggplot(a[a$value>0,],aes(x=Birds,y=Flowers,fill=value)) + geom_tile()+ theme_bw() + scale_fill_continuous(low="blue",high="red") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(fill="# of Visits/Sample")
  ggsave("MatrixPlot.jpeg",dpi=300,height=10,width=8)
  if(plots){print(p)}  
#ggsave("MatrixPlot.eps",dpi=300,height=10,width=8)

  jpeg("plotweb.jpg",res=300,height=10,width=15,units="in")
  plotweb(F_H)
  dev.off()

  #Metrics across entire
  tryCatch(birds.prop<-data.frame(HummingbirdNetwork=networklevel(F_H,level="higher")),error=function(e)
    print(e))
  
  #Add in a flag if the network is just too small. 
  if(!exists("birds.prop")){
    return("Not Enough Hummingbird Species")
  }
  
  #Network prop for plants
  #Metrics across entire
  tryCatch(plants.prop<-data.frame(HummingbirdNetwork=networklevel(F_H,level="lower")),error=function(e)
    print(e))
  
  #Add in a flag if the network is just too small. 
  if(!exists("plants.prop")){
    return("Not Enough Plant Species")
  }
  
  #Merge networks
  NetworkProp<-data.frame(birds.prop,plants.prop)
  NetworkProp["Raw.Camera",]<-nrow(camera)
  NetworkProp["Raw.Transect",]<-nrow(tran)

  #Write to file
  write.csv(NetworkProp,"NetworkProperties.csv")
  
  #Metrics across species, write to file
  tryCatch(H.species.prop<-specieslevel(F_H,level="higher"),error = function(e) {
    print(paste("Not enough Species:",e))})
  
  if(!exists("H.species.prop")){
    return("Not Enough Hummingbird Species")
  }
  
  #Hummingbird Properties
  
  write.csv(H.species.prop,"HummingbirdMetrics.csv")
  
    #Plant Network Metrics  
  #Metrics across species, write to file
  tryCatch(P.species.prop<-specieslevel(F_H,level="lower"),error = function(e) {
    print(paste("Not enough plant Species:",e))})
  
  if(!exists("P.species.prop")){
    return("Not Enough Plant Species")
  }
  
  write.csv(P.species.prop,"PlantMetrics.csv")

setwd(gitpath)

 }

