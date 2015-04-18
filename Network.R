###############################################################################
#Hummingbird Flower Interactions at the Santa Lucia and Maquipucuna EcoReserves
###############################################################################
#Ben Weinstein - Stony Brook University - Department of Ecology and Evolution

require(chron)
require(bipartite)
require(ggplot2)
require(ape)
require(reshape2)
require(sna)
require(stringr)
require(maptools)
require(taxize)
require(picante)
require(dplyr)
library(scales)

###################
#Source Functions
###################
gitpath<-"C:/Users/Ben/Documents/NetworkTime/"

source(paste(gitpath,"NetworkSource.R",sep=""))

############################################
##############Read In Data##################
############################################

#bring in clade data
clades<-read.csv(paste(gitpath,"InputData//CladeList.txt",sep=""),header=FALSE)[,-1]
colnames(clades)<-c("Clade","Genus","Species","double","English")
clades<-clades[,1:5]

#Bring in Phylogenetic Data
trx<-read.tree(paste(gitpath,"InputData\\hum294.tre",sep=""))

#format tips
new<-str_extract(trx$tip.label,"(\\w+).(\\w+)")
#get duplicates
trx<-drop.tip(trx,trx$tip.label[duplicated(new)])

#name tips.
trx$tip.label<-str_extract(trx$tip.label,"(\\w+).(\\w+)")

#Read in trait distance between species, run from Morphology.R
sp.dist<-read.csv("C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Results/HummingbirdDist.csv",row.names=1)

#Read in plant phylogeny 
pco<-read.csv(paste(gitpath,"InputData/PlantRelatedness.csv",sep=""))


#read in interaction data
datf<-read.csv("C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv",row.names=1)

#remove flower piercer
datf<-droplevels(datf[!datf$Hummingbird %in% "White-lined Flowerpiercer",])

############################################
#Run Network Function for the entire dataset
NetworkC(datf=datf,naming="Total",plots=T)
############################################

#############################################
#Temporal Change in Network Structure
#############################################
dat.split<-split(datf,list(datf$Month,datf$Year),drop=TRUE)

#which months have been run?
torun<-dat.split[!names(dat.split) %in% list.files(paste("Figures","Temporal",sep="/"))]

#not 1.2013
torun<-torun[!names(torun) %in% "1.2013"]

#need atleast twenty observations
torun<-torun[sapply(torun,nrow)>25]

if(length(torun)>0){
for (x in 1:length(torun)){
  NetworkC(datf=torun[[x]],naming=paste("Temporal",names(torun)[[x]],sep="/"))
}}

#By every two months
#############################################
#Temporal Change in Network Structure
#############################################
#cut into seasonal chunks
datf[datf$Month %in% c(12,1,2,3,4,5),"Season"]<-"Rainy"
datf[datf$Month %in% c(6,7,8),"Season"]<-"Summer"
datf[datf$Month %in% c(9,10,11),"Season"]<-"Dry"

dat.split<-split(datf,list(datf$Season,datf$Year),drop=TRUE)

#which months have been run?
torun<-dat.split[!names(dat.split) %in% list.files(paste("Figures","TwoMonth",sep="/"))]

if(length(torun)>0){
  for (x in 1:length(torun)){
    NetworkC(datf=torun[[x]],naming=paste("TwoMonth",names(torun)[[x]],sep="/"))
  }}


###########################
#Elevation split
###########################

##Broken by elevation
dat.split<-split(datf,list(datf$Month,datf$Year),drop=TRUE)

#which months have been run?
torun<-dat.split[!names(dat.split) %in% list.files(paste("Figures","Temporal",sep="/"))]

#not 1.2013
torun<-torun[!names(torun) %in% "1.2013"]

#############################################
#Compute metrics for each month and elevation
#############################################

#cut into elevation pieces
datf$ElevT<-cut(datf$ele,c(0,1700,2500),c("Low","High"))
##Broken by elevation
dat.split<-split(datf,list(datf$Month,datf$Year,datf$ElevT),drop=TRUE)

#which months have been run?
torun<-dat.split[!names(dat.split) %in% list.files(paste("Figures","Elevation",sep="/"))]

#need more than 10 records

torun<-torun[sapply(torun,nrow) > 10]
if(length(torun)>0){
  for (x in 1:length(torun)){
    NetworkC(datf=torun[[x]],naming=paste("Elevation",names(torun)[[x]],sep="/"))
  }}

###Compute metrics for two months and elevation

#############################################
#Compute metrics for each month and elevation
#############################################

#cut into elevation pieces
datf$ElevT<-cut(datf$ele,c(0,1700,2500),c("Low","High"))
##Broken by elevation
dat.split<-split(datf,list(datf$chunk,datf$Year,datf$ElevT),drop=TRUE)

#which months have been run?
torun<-dat.split[!names(dat.split) %in% list.files(paste("Figures","TwoMonthElevation",sep="/"))]

if(length(torun)>0){
  for (x in 1:length(torun)){
    NetworkC(datf=torun[[x]],naming=paste("TwoMonthElevation",names(torun)[[x]],sep="/"))
  }}
############################################
##############Networks Created##############
############################################

##################################
#Retrieve all Classes, name them, melt 
#Start with networkwide properties
##################################

#Get the desired files from paths - within time
fil.dir<-list.dirs("Figures",full.names=F,recursive = F)

fil.dir<-fil.dir[!fil.dir %in% "Total"]
#lapply(fil.dir,function(y){
#Kill the loop for the moment
y<-"Temporal"
#for each split of the data, plot the results.
  fil.list<-list.files(paste("Figures",sep="/",y),pattern="NetworkProperties.csv",recursive=TRUE,full.names=TRUE)
  
  fil<-list()
  #Read and name each file
  for (x in 1:length(fil.list)){
    fil[[x]]<-read.csv(fil.list[[x]])
    names(fil)[x]<-strsplit(fil.list[[x]],"/")[[1]][3]
  }
  
  #melt the outputs to a single dataframe
  m.Prop<-melt(fil)
  colnames(m.Prop)<-c("Metric","Level","value","Time")
  
  #Correct the naming of levels
  levels(m.Prop$Level)<-c("Hummingbirds","Plants")
  
  #If you want to remove overall metrics
  month.Prop<-m.Prop[!m.Prop$Time=="Total",]
  
  #write csv to file
  write.csv(month.Prop,paste(y,"TimeMetrics.csv",sep=""))

  month.Prop$Time<-as.character(month.Prop$Time)
  
  #format time
  month.Prop$Month<-as.numeric(sapply(month.Prop$Time,function(x){  strsplit(x,"\\.")[[1]][1]}))
  month.Prop$Year<-as.numeric(sapply(month.Prop$Time,function(x){  strsplit(x,"\\.")[[1]][2]}))
  #j<-str_match(month.Prop$Time,pattern="\\d+")
  month.Prop$Month.A<-factor(month.abb[month.Prop$Month],levels=month.abb)
  #month.Prop$Month<-apply(j,1,function(x){
   # round(mean(as.numeric(x)))
  #})
  
  #view all the metrics
  
  #just level of hummingbirds for now
  month.Prop<-month.Prop[month.Prop$Level=="Hummingbirds",]
  month.Prop$Year<-as.numeric(sapply(month.Prop$Time,function(x){  strsplit(x,"\\.")[[1]][2]}))
  month.Prop$Elev<-as.character(sapply(month.Prop$Time,function(x){  strsplit(x,"\\.")[[1]][3]}))
  month.Prop$Date<-paste("1",month.Prop$Month,month.Prop$Year,sep="/")
  month.Prop$Date<-as.Date(month.Prop$Date,format="%d/%m/%Y")
  
  metricskeep<-c("connectance","cluster.coefficient.HL","niche.overlap.HL","Raw.Transect","Raw.Camera","togetherness.HL","linkage density","partner.diversity.HL","mean.number.of.shared.partners.HL","nestedness","number of compartments")
  
  m<-month.Prop[month.Prop$Metric %in% metricskeep,]
  
  m<-dcast(m,...~Metric)
  
  m<-melt(m,measure.vars=metricskeep[!metricskeep %in% c("Raw.Transect","Raw.Camera")])
  
  #m<-group_by(m,variable) %>% mutate(stand=value/max(value,na.rm=TRUE)) 
  
  #Plot metrics over time
  p<-ggplot(m[,],aes(x=Date,y=value,col=variable,shape=Level))+ geom_point(aes(size=Raw.Camera+Raw.Transect)) + geom_line() + facet_wrap(~variable,scales="free_y",ncol=2) 
  p<-p + theme_bw() + scale_x_date(labels = date_format("%b'%y"),breaks= "3 months") 
  p + stat_smooth() + theme_bw()
  ggsave("TimeMetrics.jpeg",dpi=300,height=8,width=10)

#})


#Save image to file
save.image("NetworkData.Rdata")
setwd(gitpath)

