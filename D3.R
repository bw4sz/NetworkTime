library(d3Network)
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

#moprh data
#read in flower morphology data, comes from Nectar.R
fl.morph<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/FlowerMorphology.csv",sep=""))

#First row is empty
fl.morph<-fl.morph[-1,]

#Bring in Hummingbird Morphology Dataset, comes from
hum.morph<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/HummingbirdMorphology.csv",sep=""))

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
datf<-droplevels(datf[!datf$Hummingbird %in% c("White-lined Flowerpiercer","White-sided Flowerpiercer"),])

#reformatted names
missing<-levels(datf$Hummingbird)[!levels(datf$Hummingbird) %in% clades$English]

#correct spelling mistakes
levels(datf$Hummingbird)[levels(datf$Hummingbird) %in% "Booted Racketail"]<-"Booted Racket-tail" 
levels(datf$Hummingbird)[levels(datf$Hummingbird) %in% "Crowned Woodnymph"]<-"Green-crowned Woodnymph" 
levels(datf$Hummingbird)[levels(datf$Hummingbird) %in% "Violet-tailed Slyph"]<-"Violet-tailed Sylph"

datf<-droplevels(datf[!datf$Iplant_Double %in% c("",NA),])

#Uncorrected for sampling:
rawdat<-as.data.frame.array(table(datf$Iplant_Double,datf$Hummingbird))

#dissim
a<-melt(
  as.matrix(
    1-vegdist(
      t(rawdat),method="horn"
      )
    )
  )

colnames(a)<-c("To","From","value")

a<-a[a$value>.1,]

d3Network::d3SimpleNetwork(a[a$value<.4,],file="humbipartite.html",charge = -00)

#append id to data
#append to data
a$Source<-Nodes[match(a$Flowers,Nodes$Name),"Node"]
a$Target<-Nodes[match(a$Birds,Nodes$Name),"Node"]

#Old script
d3Network::d3ForceNetwork(Links=a,NodeID="Name",Nodes=Nodes,Target="Target",Value="value",Source="Source",file="humbipartiteForce.html",Group="group",charge = -400,d3Script = "http://d3js.org/d3.v3.min.js",opacity=.6,linkDistance = "function(d){return d.value * 5}",zoom=T)

#new script
networkD3::forceNetwork(Links=a,NodeID="Name",Nodes=Nodes,Target="Target",Value="value",Source="Source",Group="group",charge = -500,colourScale = "d3.scale.category10()") %>% networkD3::saveNetwork("humbipartiteForceNew.html",selfcontained=T)
