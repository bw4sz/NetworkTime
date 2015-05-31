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
  
  #Uncorrected for sampling:
  rawdat<-as.data.frame.array(table(datf$Iplant_Double,datf$Hummingbird))
  
  #Interaction matrix
  
  orderflowers<-names(sort(apply(rawdat,1,sum),decreasing=FALSE))
  orderbirds<-names(sort(apply(rawdat,2,sum),decreasing=TRUE))
  
  rawdat<-melt(as.matrix(rawdat))
  
  colnames(rawdat)<-c("Flowers","Birds","value")
  
  rawdat$Flowers<-factor(rawdat$Flowers,levels=orderflowers)
  rawdat$Birds<-factor(rawdat$Birds,levels=orderbirds)
  
  p<-ggplot(rawdat[rawdat$value>0,],aes(x=Birds,y=Flowers,fill=value)) + geom_tile()+ theme_bw() + scale_fill_continuous(low="blue",high="red") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(fill="# of Visits")
  ggsave("MatrixPlotCount.jpeg",dpi=300,height=9,width=8)
  ggsave("MatrixPlotCount.svg",height=9,width=8)
  
  #sort by bill length and corolla length
  humord<-hum.morph[order(hum.morph$Bill),"English"]
  flowerord<-fl.morph[order(fl.morph$TotalCorolla),"Group.1"]
  
  r<-names(sort(sapply(levels(rawdat$Birds),function(x) which(x == humord))))
  o<-sapply(levels(rawdat$Flowers),function(x) which(x == flowerord))
  o<-melt(o[!sapply(o,length)==0])
  o<-o[order(o$value),"L1"]
  
  #make a copy of the data to refactor
  refac<-rawdat
  refac$Birds<-factor(refac$Birds,levels=r)
  refac$Flowers<-factor(refac$Flowers,levels=o)
  
  refac<-refac[!is.na(refac$Flowers),]
  p<-ggplot(refac[refac$value>0,],aes(x=Birds,y=Flowers,fill=value)) + geom_tile()+ theme_bw() + scale_fill_continuous(low="blue",high="red") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(fill="# of Visits")
  
  #Make data.frame for analysis
  F_H<-acast(rawdat,Flowers~Birds,fill=0)

#Create Interaction of flowers and birds matrix

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
  
  p<-ggplot(a[a$value>0,],aes(x=Birds,y=Flowers,fill=value)) + geom_tile()+ theme_bw()  + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(fill="Visitation")
  p + scale_fill_continuous(low="blue",high="red")
  ggsave("MatrixPlot.jpeg",dpi=300,height=9,width=8)
  
  #order by bill length
  
  if(plots){print(p)}  
#ggsave("MatrixPlot.eps",dpi=300,height=10,width=8)

  jpeg("plotweb.jpg",res=300,height=10,width=15,units="in")
  plotweb(F_H)
  dev.off()
  
  
  svg("plotweb.svg",height=10,width=15)
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

