#Phenotypic Matching Among Plants and Birds
require(reshape)
require(ggplot2)
require(chron)
require(stringr)
require(scales)
require(taxize)
require(plyr)

#Setwd if not run globally
#Set Dropbox Location
#setwd to dropbox
droppath<-"C:/Users/Ben/Dropbox/"
setwd(droppath)
#Set github path
gitpath<-"C:/Users/Ben/Documents/Maquipicuna/"

#read in flower morphology data, comes from Nectar.R
fl.morph<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/FlowerMorphology.csv",sep=""))

#First row is empty
fl.morph<-fl.morph[-1,]

#Bring in Hummingbird Morphology Dataset, comes from
hum.morph<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/HummingbirdMorphology.csv")

#Bring in Interaction Matrix from the Network.R script
int<-read.csv("Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv",row.names=1)

#Melt the interaction frame and match it with the traits
m.dat<-int[colnames(int) %in% c("ID","Video","Time","Hummingbird","Sex","TransectID","Transect_R","Iplant_Double","Pierce","DateP","Month","ele")]

m.dat$Year<-years(as.Date(m.dat$DateP))
#Fix spacing to match clades

#Which are matching
hum.morph$English
levels(m.dat$Hummingbird)[!levels(m.dat$Hummingbird) %in% hum.morph$English]

#This needs to be checked
#print(paste(levels(m.dat$Hummingbird)[!levels(m.dat$Hummingbird) %in% hum.morph$English],"not matched"))

#levels(m.dat$Hummingbird)[!levels(m.dat$Hummingbird) %in% hum.morph$English]<-c("Green-crowned Woodnymph")
m.datH<-merge(m.dat,hum.morph, by.x="Hummingbird",by.y="English")

#Merge to flowers
int.FLlevels<-levels(factor(m.datH$Iplant_Double))
int.FLlevels

#Which flowers are we missing info for?
missingTraits<-int.FLlevels[!int.FLlevels %in% fl.morph$X]

print(paste("Missing Trait Information:",missingTraits))
m.datH<-merge(m.datH,fl.morph, by.x="Iplant_Double",by.y="X")

#Time Cycles

#Any data without months?
nrow(m.datH[is.na(m.datH$Month),])

#Start by plotting monthly breaks of corolla matching

p<-ggplot(m.datH[!is.na(m.datH$Month),],aes(x=factor(Bill),TotalCorolla,col=Hummingbird)) + geom_point() 
p + geom_smooth(aes(group=1),method="lm") + facet_wrap(~Month) + theme_bw()
#ggsave("Thesis/Maquipucuna_SantaLucia/Results/Phenotype/Matching_TimeGroup.svg")

p<-ggplot(m.datH[!is.na(m.datH$Month),],aes(x=Bill,TotalCorolla,col=Month)) + geom_point() 
p + geom_smooth(aes(group=Month),method="lm") + theme_bw() + scale_color_continuous(low="blue",high="red")


#####################################################################
#Difference Between Corolla and Bill Length of interactions measured
#####################################################################

m.datH$BD<-m.datH$Bill-m.datH$TotalCorolla
p<-ggplot(m.datH,aes(y=BD,x=Hummingbird)) + geom_boxplot(position="dodge")
p + coord_flip()

####################################################################
#Compared usage to available resources
####################################################################

full.fl<-read.csv("C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerTransectClean.csv",row.names=1)

#month should be capital 
colnames(full.fl)[colnames(full.fl) %in% "month"]<-"Month"
flower.month<-group_by(full.fl,Month,Year) %>% dplyr::summarise(Flowers=sum(Total_Flowers,na.rm=TRUE))

sdat<-split(m.datH,list(m.datH$Month,m.datH$Year))

#atleast 10 observations
sdat<-sdat[sapply(sdat,nrow)>50]
######Correlation coefficient, which method to use?
coeff<-rbind.fill(lapply(sdat,function(x){
  mod.c<-cor.test(x$Bill,x$TotalCorolla)
  data.frame(Month=unique(x$Month),Year=as.numeric(as.character(unique(x$Year))),cor=mod.c$estimate,p=mod.c$p.value)
}))

nStats<-merge(flower.month,coeff,by=c("Month","Year"))

p<-ggplot(data=nStats,aes(x=log(Flowers),y=cor,shape=as.factor(Year))) + geom_point(size=3) + geom_smooth(method="lm",aes(group=1)) #+ geom_text(size=3.5,vjust=-.5)
p<-p+ theme_bw()  + labs(y="Cor (Bill~Corolla)",x="Available Resources") + labs(shape="Year")
p<-p + geom_text(aes(label=month.abb[Month]),size=3.5,vjust=-.5)
print(p)

setwd(gitpath)
