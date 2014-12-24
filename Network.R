###############################################################################
#Hummingbird Flower Interactions at the Santa Lucia and Maquipucuna EcoReserves
###############################################################################
#Ben Weinstein - Stony Brook University - Department of Ecology and Evolution

require(chron)
require(bipartite)
require(ggplot2)
require(ape)
require(reshape)
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

###Read in Flower Camera Dataset####
dat<-read.csv("C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Data2013/csv/FlowerVideoClean.csv",row.names=1)

#Get desired columns
dat<-dat[,colnames(dat) %in% c("ID","Video","Date","Iplant_Double","Time","Hummingbird","Sex","Temp","Pierce","lon","lat","ele")]

#Fix date format
dat$Month<-as.numeric(format(as.Date(dat$Date,"%m/%d/%Y"),"%m"))


####Bring in interaction matrix for the flower transects, see FlowerTransects.R
transect.FL<-read.csv("C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Results/HummingbirdTransects/HumTransectRows.csv",row.names=1)

#make the columns as similiar as possible to videodata
colnames(transect.FL)<-c("GPS.ID","TransectID","Hummingbird","Date","Month","Transect_R","Iplant_Double","lat","lon","ele")

transect.FL$Iplant_Double<-gsub("_"," ",transect.FL$Iplant_Double)
##############Data Imported#####################

#Bind in the transect rows to the bottom of dat?
dat<-as.data.frame(rbind_list(dat,transect.FL))

dat$Iplant_Double<-as.factor(dat$Iplant_Double)
############################################
##############Data Import Complete##########
############################################

#Create Universal Date Stamp

dat$DateP<-sapply(dat$Date,function(x){
  if(is.na(x)){
    return(NA)
  }
  if(str_detect(x,"-")){
    toR<-as.character(strptime(x,"%Y-%m-%d"))
    #print(toR)
    return(toR)
  }
  
  if(str_detect(x,"/")){
    toR<-as.character(strptime(x,format="%m/%d/%Y"))
    #print(toR)
    return(toR)
  }
})

dat$DateP<-as.POSIXlt(dat$DateP)

# year position
dat$Year<-years(dat$DateP)

#one error
dat[dat$Year %in% 2012,"Year"]<-2013

###########################
#Hummingbird Data Cleaning 
###########################
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

#Caps Hummingbird
dat$Hummingbird<-factor(sapply(dat$Hummingbird,function(x) {.simpleCap(as.character(x))}))

#make a object, just to save typing
h<-levels(dat$Hummingbird)

missp<-h[!h %in% clades$English]

paste("misspelled levels",missp)
h[h %in% missp]

spellC<-c("Booted Racket-tail","Green-crowned Woodnymph","Rufous-tailed Hummingbird","UKWN","UKWN","UKWN","Violet-tailed Sylph","White-sided Flowerpiercer")

paste("Spelling Correction",spellC)

h[h %in% missp]<-spellC

head(clades)
#can taxize do english names? 

levels(dat$Hummingbird) <- h

#Take our any bad data
dat_e<-droplevels(dat[!dat$Hummingbird %in% c("","NANA","UKWN","Ukwn","Western Emerald"),])
# 
# # #Remove out piercing events for now?
# # table(dat$Pierce)
# # datPierce<-dat_e[dat_e$Pierce%in% c("Yes","YES","y","Y"),]
# 
# #Piercing over time
# ggplot(datPierce,aes(x=DateP,y=Hummingbird)) + geom_point(size=3)
# 
# dat_e<-dat_e[!dat_e$Pierce %in% c("Yes","YES","y","Y"),]

#Drop any unused factors?
dat_e<-droplevels(dat_e)

#Drop any observations without plants
dat_e<-droplevels(dat_e[!dat_e$Iplant_Double %in% c("",NA),])

#drop species without double name

sp_l<-levels(dat_e$Iplant_Double)

l<-sapply(sp_l,function(x){
  str2 <- gsub(' {2,}',' ',x)
  length(strsplit(str2,' ')[[1]])
})

sp_r<-names(which(l==1))

datf<-droplevels(dat_e[!dat_e$Iplant_Double %in% sp_r,])

write.csv(datf,"C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv")

#################Data Cleaning Complete################

############################################
#Run Network Function for the entire dataset
NetworkC(datf=datf,naming="Total",plots=F)
############################################

#############################################
#Temporal Change in Network Structure
#############################################
dat.split<-split(datf,list(datf$Month,datf$Year),drop=TRUE)

#which months have been run?
torun<-dat.split[!names(dat.split) %in% list.files(paste("Figures","Temporal",sep="/"))]

#not 1.2013
torun<-torun[!names(torun) %in% "1.2013"]

if(length(torun)>0){
for (x in 1:length(torun)){
  NetworkC(datf=torun[[x]],naming=paste("Temporal",names(torun)[[x]],sep="/"))
}}

#By every two months
#############################################
#Temporal Change in Network Structure
#############################################
#cut into two month chunks
datf$chunk<-cut(datf$Month,c(0,4,9,12))

dat.split<-split(datf,list(datf$chunk,datf$Year),drop=TRUE)

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

#need more than 10 records


torun<-torun[sapply(torun,nrow) > 10]
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
lapply(fil.dir,function(y){
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
  #month.Prop$Month<-as.numeric(sapply(month.Prop$Time,function(x){  strsplit(x,"\\.")[[1]][1]}))
  j<-str_match(month.Prop$Time,pattern="\\d+")

  month.Prop$Month<-apply(j,1,function(x){
    round(mean(as.numeric(x)))
  })
  
  #just level of hummingbirds for now
  month.Prop<-month.Prop[month.Prop$Level=="Hummingbirds",]
  month.Prop$Year<-as.numeric(sapply(month.Prop$Time,function(x){  strsplit(x,"\\.")[[1]][2]}))
  month.Prop$Elev<-as.character(sapply(month.Prop$Time,function(x){  strsplit(x,"\\.")[[1]][3]}))
  month.Prop$Date<-paste("1",month.Prop$Month,month.Prop$Year,sep="/")
  month.Prop$Date<-as.Date(month.Prop$Date,format="%d/%m/%Y")
  
  metricskeep<-c("connectance","H2","cluster.coefficient.HL","niche.overlap.HL","Raw.Transect","Raw.Camera")
  
  m<-month.Prop[month.Prop$Metric %in% metricskeep,]
  
  m<-dcast(m,...~Metric)
  m$H2i<-1-m$H2
  
  m<-melt(m,measure.vars=c("H2i",c("connectance","H2","cluster.coefficient.HL","niche.overlap.HL")))
  
  m<-m[!m$variable %in% "H2",]
  
  m<-group_by(m,variable) %>% mutate(stand=value/max(value,na.rm=TRUE)) 
  
  #Plot metrics over time
  p<-ggplot(m[,],aes(x=Date,y=value,col=variable,shape=Level))+ geom_point(aes(size=Raw.Camera+Raw.Transect)) + geom_line() + facet_wrap(Elev~variable,scales="free_y",ncol=1) 
  p<-p + theme_bw() + scale_x_date(labels = date_format("%b"),breaks= "3 months") 
  p
  ggsave("TimeMetrics.jpeg",dpi=300,height=8,width=10)

})


#Save image to file
save.image("NetworkData.Rdata")
setwd(gitpath)

