# Trait-matching and Available Resources
Ben Weinstein - Stony Brook University  




```
## [1] "Run Completed at 2016-01-14 17:24:38"
```


```r
#reload if needed
#load("Observed.Rdata")
```

#Load in data


```r
#read in flower morphology data, comes from Nectar.R
droppath<-"C:/Users/Ben/Dropbox/"
fl.morph<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/FlowerMorphology.csv",sep=""))

#First row is empty
fl.morph<-fl.morph[-1,]

#Bring in Hummingbird Morphology Dataset, comes from
hum.morph<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/HummingbirdMorphology.csv",sep=""))

#taxonomy change, we are calling them Crowned Woodnymph's now.
hum.morph$English<-as.character(hum.morph$English)

hum.morph$English[hum.morph$English %in% "Green-crowned Woodnymph"]<-"Crowned Woodnymph"

#Bring in Interaction Matrix
int<-read.csv(paste(droppath,"Thesis/Maquipucuna_SantaLucia/Results/Network/HummingbirdInteractions.csv",sep=""),row.names=1)

#one date error
int[int$DateP %in% '2013-07-25',"Month"]<-7

#one duplicate camera error, perhaps two GPS records.
int<-int[!(int$ID %in% "FH1108" & int$Date_F %in% '2014-12-01'),]

#Correct known taxonomic disagreements, atleast compared to traits
int[int$Iplant_Double=="Alloplectus purpureus","Iplant_Double"]<-"Glossoloma purpureum"
int[int$Iplant_Double=="Capanea affinis","Iplant_Double"]<-"Kohleria affinis"
int[int$Iplant_Double=="Columnea cinerea","Iplant_Double"]<-"Columnea mastersonii"
int[int$Iplant_Double=="Alloplectus teuscheri","Iplant_Double"]<-"Drymonia teuscheri"
int[int$Iplant_Double=="Drymonia collegarum","Iplant_Double"]<-"Alloplectus tetragonoides"

#Some reasonable level of presences, 25 points
keep<-names(which(table(int$Hummingbird) > 25))

int<-int[int$Hummingbird %in% keep & !int$Hummingbird %in% c("Sparkling Violetear"),]

m.dat<-droplevels(int[colnames(int) %in% c("ID","Video","Time","Hummingbird","Sex","TransectID","Transect_R","Iplant_Double","Pierce","DateP","Month","ele","Type")])

#Does the data come from camera or transect?
m.dat$Type<-(is.na(m.dat$TransectID))*1

m.dat$Year<-years(as.Date(m.dat$DateP))
#one missing date
m.dat$Year[m.dat$Year %in% 2012]<-2013

#Number of bird species
h_species<-nlevels(m.dat$Hummingbird)

#Number of plant species
plant_species<-nlevels(m.dat$Iplant_Double)

#Get english name
dath<-merge(m.dat,hum.morph, by.x="Hummingbird",by.y="English",keep=all)

#Merge to flowers
int.FLlevels<-levels(factor(dath$Iplant_Double))

#Which flowers are we missing info for?
missingTraits<-int.FLlevels[!int.FLlevels %in% fl.morph$X]

#print(paste("Missing Trait Information:",missingTraits))
dath<-merge(dath,fl.morph, by.x="Iplant_Double",by.y="X")

#Drop piercing events, since they don't represent correlation
dath<-dath[!dath$Pierce %in% c("y","Y"),]
```

##Match Species to Morphology


```r
#observed traitmatching
traitmatchF<-abs(t(sapply(hum.morph$Bill,function(x){x-fl.morph$TotalCorolla})))

rownames(traitmatchF)<-hum.morph$English
colnames(traitmatchF)<-fl.morph$Group.1
```


```r
#match names #Round to 2 decimals #Convert to cm for winbugs, avoids numerical underflow
traitmatchT<-round(traitmatchF[rownames(traitmatchF) %in% dath$Hummingbird,colnames(traitmatchF) %in% dath$Iplant_Double],2)/10

traitmatchT<-traitmatchT[sort(rownames(traitmatchT)),sort(colnames(traitmatchT))]
```

##Elevation ranges

Create a binary variable whether each observation was in a low elevation or high elevation transect. We have some species that just occur at the top of the gradient, and are not present in the sampling window of flowers at the low elevation.

Accounting for non-availability.
We have to figure out which plants were sampled in which periods, and if it was sampled, the non-detection are 0 if it wasn't the non-detection are NA. then remove all the Na's.


```r
elevH<-read.csv("InputData/HummingbirdElevation.csv",row.names=1)
head(elevH)
```

```
##                 Hummingbird  Low        m   High Index
## 1            Andean Emerald 1378 1378.632 1380.0     1
## 2    White-whiskered Hermit 1340 1437.024 1614.2     1
## 3    Stripe-throated Hermit 1360 1455.084 1527.4     1
## 4         Crowned Woodnymph 1360 1523.420 2049.0     1
## 5 Rufous-tailed Hummingbird 1370 1531.929 1862.0     3
## 6  Wedge-billed Hummingbird 1331 1624.850 2003.0     3
```

```r
colnames(elevH)[5]<-"Elevation"
elevH$Bird<-1:nrow(elevH)

#high elevation or low elevation
elevP<-read.csv("InputData/PlantElevation.csv",row.names=1)
colnames(elevP)[5]<-"Elevation"
elevP$Plant<-1:nrow(elevP)
elevP$Iplant_Double<-as.character(elevP$Iplant_Double)

#Correct known taxonomic errors
elevP[elevP$Iplant_Double %in% "Alloplectus purpureus","Iplant_Double"]<-"Glossoloma purpureum"
elevP[elevP$Iplant_Double %in% "Capanea affinis","Iplant_Double"]<-"Kohleria affinis"
elevP[elevP$Iplant_Double %in% "Alloplectus teuscheri","Iplant_Double"]<-"Drymonia teuscheri"
elevP[elevP$Iplant_Double %in% "Columnea cinerea","Iplant_Double"]<-"Columnea mastersonii"
elevP[elevP$Iplant_Double %in% "Alloplectus tenuis","Iplant_Double"]<-"Drymonia tenuis"

#Merge to observed Data
#plants
dathp<-merge(dath,elevP,by="Iplant_Double")

#birds
datph<-merge(dathp,elevH,by="Hummingbird")
```

What elevation transect is each observation in?
The camera data need to be inferred from the GPS point.


```r
#cut working best on data.frame
datph<-as.data.frame(datph)

#which elevation bin is each observation within
labs<-paste(seq(1300,2500,200),seq(1500,2700,200),sep="_")

#for the couple points that have 1290 elevation, round up to 300 for convienance
datph$ele[datph$ele < 1300]<-1301
datph$Transect_R[is.na(datph$Transect_R)]<-as.character(cut(datph[is.na(datph$Transect_R),]$ele,seq(1300,2700,200),labels=labs))

#Elev for the transects is the midpoint
tran_elev<-datph[datph$Survey_Type=='Transect',"Transect_R"]
datph[datph$Survey_Type=='Transect',"ele"]<-sapply(tran_elev,function(x){
  mean(as.numeric(str_split(x,"_")[[1]]))
})
```

### Summarize Observations


```r
#ID for NA is holger transects, make the id's 1:n for each day of transect at each elevation, assuming no elevation was split across days.
datph$ID<-as.character(datph$ID)

noid<-datph[is.na(datph$ID),]

id_topaste<-paste(noid$Transect_R,noid$DateP,"Transect",sep="_")
datph[which(is.na(datph$ID)),"ID"]<-id_topaste

#Create year month combination
datph$Time<-paste(datph$Month,datph$Year,datph$Transect_R,sep="_")

#Label survey type
datph$Survey_Type<-NA

mt<-!is.na(datph$TransectID)*1
datph$Survey_Type[mt==1]<-"Transect"
datph$Survey_Type[!datph$Survey_Type %in% "Transect"]<-"Camera"

indatraw<- datph %>% group_by(Hummingbird,Iplant_Double,Bird,Plant,Time,ID,DateP,Survey_Type) %>% summarize(Yobs=n(),Elev=mean(ele,na.rm=T),Transect_R=unique(Transect_R),Month=unique(Month),Year=unique(Year)) 

#add day ID
sdat<-split(indatraw,list(indatraw$ID),drop = T)

sdat<-lapply(sdat,function(x){
  x<-droplevels(x)
  x$Day<-as.numeric(as.factor(x$DateP))
  return(x)
})

indatraw<-rbind_all(sdat)

#Species names
for (x in 1:nrow(indatraw)){
  indatraw$Hummingbird[x]<-as.character(elevH[elevH$Bird %in% indatraw$Bird[x],"Hummingbird"])
  indatraw$Iplant_Double[x]<-as.character(elevP[elevP$Plant %in% indatraw$Plant[x],"Iplant_Double"])
}
```


```r
#match the traits
traitmelt<-melt(traitmatchT)
colnames(traitmelt)<-c("Hummingbird","Iplant_Double","Traitmatch")
```

##Absences - accounting for non-detection

We have more information than just the presences, given species elevation ranges, we have absences as well. Absences are birds that occur at the elevation of the plant sample, but were not recorded feeding on the flower.


```r
indatlong<-acast(indatraw,Bird~Plant~ID~Day,value.var="Yobs")

indatlong[is.na(indatlong)]<-0
```


```r
#Only non-detections are real 0's, the rest are NA's and are removed.
#Plants not surveyed in that time period
#Hummingbirds not present at that elevation
  for(x in 1:dim(indatlong)[3]){
  
  #Remove non sampled plants 
  a<-indatlong[,,x,]
  
  #No observations at that plant
  toNA<-as.numeric(names(which(apply(a,2,sum)==0)))
  pres<-as.numeric(names(which(!apply(a,2,sum)==0)))
  indatlong[,colnames(a) %in% toNA,x,]<-NA

  if(length(pres)==0){next} else {
  #IDs are unique to time periods.

  #Get elevation point of that sampling event
  cam<-dimnames(indatlong)[[3]][x]
  camelev<-indatraw %>% filter(ID==cam)  %>% .$Elev %>% mean()
  
  #for each hummingbird, was that camera within elevation?
  for(i in 1:dim(a)[1]){
      low<-elevH[elevH$Bird == rownames(a)[i],"Low"]
      high<-elevH[elevH$Bird == rownames(a)[i],"High"]
        
        #if not in elev range, set to NA
        
        if(!((low < camelev) & (camelev < high))){
            if(sum(indatlong[i,,x,],na.rm=T)>0){next}
          #if you had a wandering individual outside range, allow interaction to occur.
                indatlong[i,,x,]<-NA
        }
      }
    }
  }

### There can't be absences in days that weren't sampled.
for (x in 1:dim(indatlong)[3]){
  cam<-indatlong[,,x,]
  for (y in 1:dim(cam)[3]){
    sc<-sum(cam[,,y],na.rm=T)
    if (sc ==0){
      indatlong[,,x,y]<-NA
    }
  }
}



#get only absence data
for(x in 1:dim(indatlong)[1]){
  for (y in 1:dim(indatlong)[2]){
    if(sum(indatlong[x,y,,],na.rm=T)==0){
      indatlong[x,y,,]<-NA
    }
  }
}

#melt and remove Na's
indat<-melt(indatlong)

indat<-indat[indat$value %in% 0,]

colnames(indat)<-c("Bird","Plant","ID","Day","Yobs")
```


```r
#remerge the time period data for absences
Timelookup<-indatraw %>% dplyr::select(ID,Transect_R,Month,Survey_Type,Year,Day,DateP,Time) %>% unique()

#Get time information
indat<-merge(indat,Timelookup,by=c("ID","Day"))

#bind to presence data
indat<-as.data.frame(rbind_all(list(indat,indatraw)))

#Species names
for (x in 1:nrow(indat)){
  indat$Hummingbird[x]<-as.character(elevH[elevH$Bird %in% indat$Bird[x],"Hummingbird"])
  indat$Iplant_Double[x]<-as.character(elevP[elevP$Plant %in% indat$Plant[x],"Iplant_Double"])
}

#Get trait information
#match the traits
indat<-merge(indat,traitmelt,by=c("Hummingbird","Iplant_Double"))
```

Reformat index for jags.
Jags needs a vector of input species 1:n with no breaks.


```r
#Easiest to work with jags as numeric ordinal values
indat$Hummingbird<-as.factor(indat$Hummingbird)
indat$Iplant_Double<-as.factor(indat$Iplant_Double)
indat$jBird<-as.numeric(indat$Hummingbird)
indat$jPlant<-as.numeric(indat$Iplant_Double)

jagsIndexBird<-data.frame(Hummingbird=levels(indat$Hummingbird),jBird=1:length(levels(indat$Hummingbird)))
 
jagsIndexPlants<-data.frame(Iplant_Double=levels(indat$Iplant_Double),jPlant=1:length(levels(indat$Iplant_Double)))

#Similiarly, the trait matrix needs to reflect this indexing.
jTraitmatch<-traitmatchT[rownames(traitmatchT) %in% unique(indat$Hummingbird),colnames(traitmatchT) %in% unique(indat$Iplant_Double)]
```

#Resources at each month

In our model the covariate is indexed at the scale at which the latent count is considered fixed. This means we need the resource availability per month across the entire elevation gradient for each point.


```r
#Get flower transect data
full.fl<-read.csv("C:/Users/Ben/Dropbox/Thesis/Maquipucuna_SantaLucia/Results/FlowerTransects/FlowerTransectClean.csv")[,-1]

 #month should be capital 
colnames(full.fl)[colnames(full.fl) %in% "month"]<-"Month"

#group by month and replicate, remove date errors by making a max of 10 flowers, couple times where the gps places it in wrong transect by 1 to 2 meters. 
flower.month<-group_by(full.fl,Month,Year,Transect_R,Date_F) %>% dplyr::summarise(Flowers=sum(Total_Flowers,na.rm=TRUE))  %>% filter(Flowers>20)
  
#Make month abbreviation column, with the right order
flower.month$Month.a<-factor(month.abb[flower.month$Month],month.abb[c(1:12)])

#Make year factor column
flower.month$Year<-as.factor(flower.month$Year)

#get quantile for each transect
thresh<-melt(group_by(flower.month,Transect_R) %>% summarize(Threshold=quantile(log(Flowers),0.5)) )
flower.month<-merge(flower.month,thresh)
flower.month$High<-log(flower.month$Flowers)>flower.month$value

#fix the levels
levels(flower.month$Transect_R)<-c("1300m - 1500m", "1500m - 1700m","1700m - 1900m","1900m - 2100m","2100m - 2300m","2300m - 2500m")
#plot
ggplot(flower.month,aes(x=Month.a,log(Flowers),col=High,shape=as.factor(Year))) + geom_point(size=3) + theme_bw()  + geom_smooth(aes(group=1)) + ylab("Flowers") + xlab("Month") + facet_wrap(~Transect_R,scales="free_y") + labs(shape="Year", y= "Log Available Flowers") + scale_x_discrete(breaks=month.abb[seq(1,12,2)]) + scale_color_manual(labels=c("Low","High"),values=c("black","red")) + labs(col="Resource Availability")
```

<img src="figureObserved/unnamed-chunk-15-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
#turn min and max elvation into seperate columns for the range
flower.month$minElev<-as.numeric(str_extract(flower.month$Transect_R,"(\\d+)"))
flower.month$maxElev<-as.numeric(str_match(flower.month$Transect_R,"(\\d+)_(\\d+)")[,3])
```


```r
indat$All_Flowers<-NA
indat$Used_Flowers<-NA
indat$FlowerA<-NA

#Resource list for each species.
slist<-int %>% group_by(Hummingbird,Iplant_Double) %>% distinct() %>% dplyr::select(Hummingbird,Iplant_Double) %>% arrange(Hummingbird)

#Create time ID for flower transects
full.fl$Time<-paste(full.fl$Month,full.fl$Year,full.fl$Transect_R,sep="_")

for (x in 1:nrow(indat)){
 event_date<-indat[x,"Time"] 

 #Flowers used by the species
 sp_list<-slist[slist$Hummingbird %in% indat$Hummingbird[x],"Iplant_Double"]
  
 #Species of flower
 sp <-  indat[x,"Iplant_Double"]
 
 #Filter by Date
 bydate<-full.fl[full.fl$Time %in% event_date,]
 
 #average number of flowers at each elevation within that time frame.
 
 #count number of all flowers
 indat$All_Flowers[x]<- bydate%>% group_by(Date_F,Transect_R) %>% summarize(n=sum(Total_Flowers,na.rm=T)) %>% group_by(Transect_R) %>% summarize(mn=mean(n)) %>% summarize(F=sum(mn)) %>% .$F
 
 #filter by species
  byspecies<-bydate[bydate$Iplant_Double %in% sp_list$Iplant_Double,]
 
  indat$Used_Flowers[x]<-byspecies%>% group_by(Date_F,Transect_R) %>% summarize(n=sum(Total_Flowers,na.rm=T)) %>% group_by(Transect_R) %>% summarize(mn=mean(n)) %>% summarize(F=sum(mn)) %>% .$F
  
  #just the abundance of that species
  bys <-  bydate[bydate$Iplant_Double %in% sp,]
  indat$FlowerA[x] <-  bys %>% group_by(Date_F,Transect_R) %>% summarize(n=sum(Total_Flowers,na.rm=T)) %>% group_by(Transect_R) %>% summarize(mn=mean(n)) %>% summarize(F=sum(mn)) %>% .$F

}
```

###Relationship between resource measures


```r
ggplot(indat,aes(x=All_Flowers,y=Used_Flowers)) + geom_point() + facet_wrap(~Hummingbird,scales="free")
```

<img src="figureObserved/unnamed-chunk-17-1.png" title="" alt="" style="display: block; margin: auto;" />

##Binary Measures of Resources


```r
#All Resources
indat$BAll_Flowers<-(indat$All_Flowers > quantile(indat$All_Flowers,0.75))*1
#mnth<-sapply(indat$Time,function(x){
 # as.numeric(str_split(x,"_")[[1]][1])})
#indat$BAll_Flowers<-(mnth  %in% c(6,7,8,9,10))*1

qthresh<-indat %>% group_by(Hummingbird) %>% summarize(UThresh=mean(Used_Flowers))

#save a copy in case you need it here
tosave<-indat

indat<-merge(indat,qthresh)
indat$BUsed_Flowers<-(indat$Used_Flowers > indat$UThresh)*1

fthresh<-indat %>% group_by(Hummingbird) %>% summarize(FThresh=mean(FlowerA))
indat<-merge(indat,fthresh)
indat$BFlowerA<-(indat$FlowerA > indat$FThresh)*1
```


```r
#need to correctly weight boxplots
boxp<-list() 
for (x in 1:nrow(indat)){
 boxp[[x]]<- indat[rep(x,max(indat$Yobs[x],1)),]
}
weightb<-rbind_all(boxp)

#View by Corolla Length and Bill Length
weightb<-merge(weightb,fl.morph,by.x="Iplant_Double",by.y="Group.1")

weightb<-merge(weightb,hum.morph,by.x="Hummingbird",by.y="English")

ggplot(weightb,aes(y=TotalCorolla,x=Yobs>0,fill=as.factor(BAll_Flowers))) + geom_boxplot() + scale_fill_manual(name="Resource Availability",labels=c("Low","High"),values=c("Blue","Red")) + ggtitle("Observed Niche Breadth: All Flowers") + facet_wrap(~Hummingbird) + scale_x_discrete("Visited",labels=c("Unused","Used")) + geom_hline(aes(yintercept=Total_Culmen),linetype='dashed') + theme_bw() + ylab("Corolla Length")
```

<img src="figureObserved/unnamed-chunk-19-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
ggplot(weightb,aes(y=TotalCorolla,x=Yobs>0,fill=as.factor(BUsed_Flowers))) + geom_boxplot() + scale_fill_manual(name="Resource Availability",labels=c("Low","High"),values=c("Blue","Red")) + ggtitle("Observed Niche Breadth: Used Flowers") + facet_wrap(~Hummingbird) + scale_x_discrete("Visited",labels=c("Unused","Used")) + geom_hline(aes(yintercept=Total_Culmen),linetype='dashed') + theme_bw()
```

<img src="figureObserved/unnamed-chunk-19-2.png" title="" alt="" style="display: block; margin: auto;" />

```r
ggsave("Figures/ObservedNicheBreadth.jpeg",height=8,width=8)

ggplot(weightb,aes(y=TotalCorolla,x=Yobs>0,fill=as.factor(BFlowerA))) + geom_boxplot() + scale_fill_manual(name="Resource Availability",labels=c("Low","High"),values=c("Blue","Red")) + ggtitle("Observed Niche Breadth: Flower Abundance") + facet_wrap(~Hummingbird) + scale_x_discrete("Visited",labels=c("Unused","Used")) + geom_hline(aes(yintercept=Total_Culmen),linetype='dashed') + theme_bw()
```

<img src="figureObserved/unnamed-chunk-19-3.png" title="" alt="" style="display: block; margin: auto;" />


```r
#Combine resources with observed data
#As matrix
indat<-dcast(indat,...~Survey_Type,value.var="Yobs",fill=NA)

indat<-droplevels(indat)
#Turn Time and ID into numeric indexes
indat$jTime<-as.numeric(as.factor(indat$Time))
indat$jID<-as.numeric(as.factor(indat$ID))

#index resources
resourceMatrix<-indat %>% group_by(jBird,jPlant,jTime) %>% summarize(v=unique(BUsed_Flowers)) %>% acast(jBird ~ jPlant ~ jTime,value.var='v',fill=0)

#bind to indat for later
mr<-melt(resourceMatrix)
colnames(mr)<-c("jBird","jPlant","jTime","scaledR")
indat<-merge(indat,mr,by=c("jBird","jPlant","jTime"))

##melted version for plotting
mindat<-melt(indat,measure.vars=c("Camera","Transect"))
```

View species identity in resource splits.


```r
#Count of species in both time sets
splist<-mindat %>% filter(value>0) %>% group_by(Hummingbird,Resource= BUsed_Flowers) %>% distinct(Iplant_Double) %>% dplyr::select(Iplant_Double)

#relevel text
splist$Resource<-factor(as.factor(as.character(splist$Resource)),labels = c("Low","High"))

splist<-split(splist,splist$Hummingbird)
p<-list()
for (x in 1:length(splist)){
  split_sp<-split(splist[[x]]$Iplant_Double,splist[[x]]$Resource)
  p[[x]]<-venn.diagram(split_sp,filename=NULL,scaled=T,main =unique(splist[[x]]$Hummingbird),fill=c("Blue","Red"),alpha=c(.25,.75),cat.cex=1.5,cex=.75,main.cex=2)

  #get index
  labs<-lapply(p[[x]],function(i) i$label)
  
  in_low<-which(labs == "Low")

  in_high<-which(labs == "High")

#edit labels, depends on length
#which is low
  if(length(p[[x]])==10){
  p[[x]][[in_low-3]]$label<-paste(split_sp$Low[!split_sp$Low %in% split_sp$High],collapse="\n")

  p[[x]][[in_high-3]]$label<-paste(split_sp$High[!split_sp$High %in% split_sp$Low],collapse="\n")
  }
  
  if(length(p[[x]])==9 & !is.null(labs[[4]])){
      p[[x]][[in_low-3]]$label<-paste(split_sp$Low[!split_sp$Low %in% split_sp$High],collapse="\n")

  p[[x]][[in_high-1]]$label<-paste(split_sp$High[!split_sp$High %in% split_sp$Low],collapse="\n")
  }
  if(length(p[[x]])==9 & is.null(labs[[4]])){
      p[[x]][[in_low-2]]$label<-paste(split_sp$Low[!split_sp$Low %in% split_sp$High],collapse="\n")

  p[[x]][[in_high-2]]$label<-paste(split_sp$High[!split_sp$High %in% split_sp$Low],collapse="\n")
  }
  grid.newpage()
  grid.draw(p[[x]])
}
```

<img src="figureObserved/unnamed-chunk-21-1.png" title="" alt="" style="display: block; margin: auto;" /><img src="figureObserved/unnamed-chunk-21-2.png" title="" alt="" style="display: block; margin: auto;" /><img src="figureObserved/unnamed-chunk-21-3.png" title="" alt="" style="display: block; margin: auto;" /><img src="figureObserved/unnamed-chunk-21-4.png" title="" alt="" style="display: block; margin: auto;" /><img src="figureObserved/unnamed-chunk-21-5.png" title="" alt="" style="display: block; margin: auto;" /><img src="figureObserved/unnamed-chunk-21-6.png" title="" alt="" style="display: block; margin: auto;" /><img src="figureObserved/unnamed-chunk-21-7.png" title="" alt="" style="display: block; margin: auto;" /><img src="figureObserved/unnamed-chunk-21-8.png" title="" alt="" style="display: block; margin: auto;" /><img src="figureObserved/unnamed-chunk-21-9.png" title="" alt="" style="display: block; margin: auto;" /><img src="figureObserved/unnamed-chunk-21-10.png" title="" alt="" style="display: block; margin: auto;" /><img src="figureObserved/unnamed-chunk-21-11.png" title="" alt="" style="display: block; margin: auto;" /><img src="figureObserved/unnamed-chunk-21-12.png" title="" alt="" style="display: block; margin: auto;" /><img src="figureObserved/unnamed-chunk-21-13.png" title="" alt="" style="display: block; margin: auto;" /><img src="figureObserved/unnamed-chunk-21-14.png" title="" alt="" style="display: block; margin: auto;" />

```r
#venn diagram writes a silly set of log files
file.remove(list.files(pattern="*.log"))
```

```
## [1] TRUE TRUE TRUE TRUE TRUE
```

###Calculate Sampling effort


```r
#At each data point there is a camera a transect or both
cam_effort<-(!is.na(indat$Camera))*1
trans_effort<-(!is.na(indat$Transect))*1
```

#Hierarchical Occupancy Model

For hummingbird species i feeding on plant species j observed at time k and sampling event d. 

$$ YTransect_{i,j,k,d} \sim B(N_{i,j,k},\omega_{Transect}) $$

$$ YCamera_{i,j,k,d} \sim B(N_{i,j,k},\omega_{Camera}) $$
$$ \omega_{Camera} <- \phi_{Camera} * EffortCamera_k $$
$$ \omega_{Transect} <- \phi_{Transect}* EffortTransect_k $$
$$ N_{i,j,k} \sim Pois(\lambda_{i,j,k}) $$
$$ log(\lambda_{i,j,k}) = \alpha_i + \beta_{1,i} * Traitmatch_{i,j} + \beta_{2,i} *Resources_k + \beta_{3,i} * Traitmatch_{i,j} * Resources_k $$

**Priors**

$$ \phi_{Camera} \sim U(0,1) $$
$$ \phi_{Transect} \sim U(0,1) $$
$$\alpha_i \sim N(intercept,\tau_{\alpha})$$
$$\beta_{1,i} \sim N(\gamma_1i,\tau_{\beta_1})$$
$$\beta_{2,i} \sim N(\gamma_2i,\tau_{\beta_2})$$
$$\beta_{3,i} \sim N(\gamma_3i,\tau_{\beta_3})$$

**Hyperpriors**

Group Level Means

$$\gamma_{1,i} \sim N(0,0.0001)$$
$$\gamma_{2,i} \sim N(0,0.0001)$$
$$\gamma_{3,i} \sim N(0,0.0001)$$
$$ intercept \sim N(0,0.0001)$$

Group Level Variance

$$\tau_{\alpha} \sim Gamma(0.0001,0.0001)$$
$$\tau_\beta1 \sim Gamma(0.0001,0.0001)$$
$$\tau_\beta2 \sim Gamma(0.0001,0.0001)$$
$$\tau_\beta3 \sim Gamma(0.0001,0.0001)$$

**Derived quantities**

$$\sigma_{intercept} = \sqrt[2]{\frac{1}{\tau_\alpha}}$$
$$\sigma_{slope1} = \sqrt[2]{\frac{1}{\tau_{\beta_1}}}$$
$$\sigma_{slope2} = \sqrt[2]{\frac{1}{\tau_{\beta_2}}}$$
$$\sigma_{slope3} = \sqrt[2]{\frac{1}{\tau_{\beta_3}}}$$


```r
#Source model
source("Bayesian/NmixturePoissonRagged2m.R")

#print model
writeLines(readLines("Bayesian/NmixturePoissonRagged2m.R"))
```

```
## 
## sink("Bayesian/NmixturePoissonRagged2m.jags")
## 
## cat("
##     model {
##     #Compute intensity for each pair of birds and plants
##     for (i in 1:Birds){
##     for (j in 1:Plants){
##     for (k in 1:Times){
##     
##     #Process Model
##     log(lambda[i,j,k])<-alpha[i] + beta1[i] * Traitmatch[i,j] + beta2[i] * resources[i,j,k] + beta3[i] * Traitmatch[i,j] * resources[i,j,k]
##     
##     #True number of interactions
##     N[i,j,k] ~ dpois(lambda[i,j,k])
##     }
##     }
##     }
## 
##     #Observed counts for each day of sampling
##     for (x in 1:Nobs){
##     
##     #Observation Process for cameras
##     detect_cam[x]<-dcam[Bird[x]] * cam_surveys[x]
## 
##     #Observation Process for transects
##     detect_transect[x]<-dtrans[Bird[x]] * trans_surveys[x]
## 
##     Yobs_camera[x] ~ dbin(detect_cam[x],N[Bird[x],Plant[x],Time[x]])    
##     Yobs_transect[x] ~ dbin(detect_transect[x],N[Bird[x],Plant[x],Time[x]])    
## 
##     #Assess Model Fit
## 
##     #Fit discrepancy statistics
##     #eval[x]<-detect[Bird[x]]*N[Bird[x],Plant[x],Camera[x]]
##     #E[x]<-pow((Yobs[x]-eval[x]),2)/(eval[x]+0.5)
##     
##     #ynew[x]~dbin(detect[Bird[x]],N[Bird[x],Plant[x],Camera[x]])
##     #E.new[x]<-pow((ynew[x]-eval[x]),2)/(eval[x]+0.5)
##     
##     }
##     
## 
##     #Species level priors
##     
##     for (i in 1:Birds){
##     #Detect priors, logit transformed
## 
##     #For Cameras
##     logit(dcam[i]) <- dcam_logit[i]
##     dcam_logit[i] ~ dnorm(dprior_cam,tau_dcam)
## 
##     #For Transects
##     logit(dtrans[i]) <- dtrans_logit[i]
##     dtrans_logit[i] ~ dnorm(dprior_trans,tau_dtrans)
## 
##     alpha[i] ~ dnorm(intercept,tau_alpha)
##     beta1[i] ~ dnorm(gamma1,tau_beta1)    
##     beta2[i] ~ dnorm(gamma2,tau_beta2)    
##     beta3[i] ~ dnorm(gamma3,tau_beta3)    
##     }
## 
##     #Hyperpriors
##     #Slope grouping
##     gamma1~dnorm(0,0.0001)
##     gamma2~dnorm(0,0.0001)
##     gamma3~dnorm(0,0.0001)
##     
##     #Intercept grouping
##     intercept~dnorm(0,0.0001)
##     
##     #Detection group prior
##     dprior_cam ~ dnorm(0,0.5)
##     dprior_trans ~ dnorm(0,0.5)
## 
##     # Group intercept variance
##     tau_alpha ~ dgamma(0.0001,0.0001)
##     sigma_int<-pow(1/tau_alpha,2) 
##     
##     #Group effect detect camera
##     tau_dcam ~ dunif(0,10)
##     sigma_dcam<-pow(1/tau_dcam,.5)
##     
##     #Group effect detect camera
##     tau_dtrans ~ dunif(0,10)
##     sigma_dtrans<-pow(1/tau_dtrans,.5)
## 
##     #Group Effect of traits
##     tau_beta1 ~ dgamma(0.0001,0.0001)
##     sigma_slope1<-pow(1/tau_beta1,.5)
##     
##     #Group Effect of Resources
##     tau_beta2 ~ dgamma(0.0001,0.0001)
##     sigma_slope2<-pow(1/tau_beta2,.5)
##     
##     #Group Effect of Resources * Traits
##     tau_beta3 ~ dgamma(0.0001,0.0001)
##     sigma_slope3<-pow(1/tau_beta3,.5)
## }
##     ",fill=TRUE)
## 
## sink()
```

```r
#Input Data
Dat <- c('Yobs_camera','Yobs_transect','Birds','Bird','Plant','Time','Plants','Times','resources','Nobs','cam_surveys','trans_surveys','Traitmatch')

#Inits
InitStage <- function(){
  #A blank Y matrix - all present
  initY<-array(dim=c(Birds,Plants,Times),30)
  initB<-as.numeric(matrix(nrow=Birds,ncol=1,data=0))
  initD<-as.numeric(matrix(nrow=Birds,ncol=1,data=.5))

list(beta1=initB,beta2=initB,beta3=initB,alpha=rep(.5,Birds),intercept=0,tau_alpha=0.1,tau_beta1=0.1,tau_beta2=0.1,tau_beta3=0.1,gamma1=0,gamma2=0,gamma3=0,N=initY)}

#Parameters to track
ParsStage <- c("alpha","beta1","beta2","beta3","intercept","sigma_int","sigma_slope1","sigma_slope2","sigma_slope3","gamma1","gamma2","gamma3","dtrans","dcam")

#MCMC options
ni <- 70000  # number of draws from the posterior
nt <- max(c(2,ni*.0001))  #thinning rate
nb <- ni*.95 # number to discard for burn-in
nc <- 2  # number of chains

#Jags

  Yobs_camera = indat$Camera
  Yobs_transect = indat$Transect
  Birds=max(indat$jBird)
  Bird=indat$jBird
  Plant=indat$jPlant
  Time=indat$jTime
  Plants=max(indat$jPlant)
  Times=max(indat$jTime)
  resources=resourceMatrix
  Nobs=nrow(indat)
  cam_surveys= cam_effort
  trans_surveys= trans_effort
  Traitmatch=jTraitmatch

  m2<-do.call(jags.parallel,list(Dat,InitStage,ParsStage,model.file="Bayesian/NmixturePoissonRagged2m.jags",n.thin=nt, n.iter=ni,n.burnin=nb,n.chains=nc))
```


```r
#recompile if needed
load.module("dic")
runs<-40000
recompile(m2)
m2<-update(m2,n.iter=runs,n.burnin=runs*.9,n.thin=3)
```


```r
#extract par to data.frame
pars_detect<-extract_par(m2,data=indat,Bird="jBird",Plant="jPlant")
```

##Assess Convergence


```r
###Chains
ggplot(pars_detect[pars_detect$par %in% c("alpha","beta1"," beta2","beta3"),],aes(x=Draw,y=estimate,col=as.factor(Chain))) + geom_line() + facet_grid(par~species,scale="free") + theme_bw() + labs(col="Chain") + ggtitle("Species Level")
```

<img src="figureObserved/unnamed-chunk-26-1.png" title="" alt="" style="display: block; margin: auto;" />


```r
ggplot(pars_detect[pars_detect$par %in% c("dcam","dtrans"),],aes(x=Draw,y=estimate,col=as.factor(Chain))) + geom_line() + facet_wrap(species~par,scale="free",ncol=4) + theme_bw() + labs(col="Chain") + ggtitle("Species Level")
```

<img src="figureObserved/unnamed-chunk-27-1.png" title="" alt="" style="display: block; margin: auto;" />


```r
ggplot(pars_detect[pars_detect$par %in% c("gamma1","intercept","sigma_int","sigma_slope1","gamma2","gamma3","sigma_slope2","sigma_slope3"),],aes(x=Draw,y=estimate,col=as.factor(Chain))) + geom_line() + theme_bw() + labs(col="Chain") + ggtitle("Group Level Parameters") + facet_wrap(~par,scales="free")
```

<img src="figureObserved/unnamed-chunk-28-1.png" title="" alt="" style="display: block; margin: auto;" />

#Posteriors


```r
###Posterior Distributions
ggplot(pars_detect[pars_detect$par %in% c("alpha","beta1","beta2","beta3"),],aes(x=estimate)) + geom_histogram(position='identity') +  facet_grid(species~par,scales="free") + theme_bw() + ggtitle("Species Parameters")
```

<img src="figureObserved/unnamed-chunk-29-1.png" title="" alt="" style="display: block; margin: auto;" />


```r
#Detection figure
ggplot(pars_detect[pars_detect$par %in% c("dtrans","dcam"),],aes(x=par,y=estimate)) + geom_violin(fill='black') + theme_bw() + ggtitle("Detection Probability") + scale_x_discrete(labels=c("Camera","Transect")) + facet_wrap(~species,ncol=3)
```

<img src="figureObserved/unnamed-chunk-30-1.png" title="" alt="" style="display: block; margin: auto;" />


```r
ggplot(pars_detect[pars_detect$par %in% c("gamma1","gamma2","gamma3","intercept","sigma_int","sigma_slope1","sigma_slope2","sigma_slope3"),],aes(x=estimate)) + geom_histogram() + ggtitle("Group Level Posteriors") + facet_wrap(~par,scale="free",nrow=2) + theme_bw() 
```

<img src="figureObserved/unnamed-chunk-31-1.png" title="" alt="" style="display: block; margin: auto;" />

#Predicted Relationship 


```r
#Expand out pars
castdf<-dcast(pars_detect[pars_detect$par %in% c("gamma1","gamma2","gamma3","intercept"),], Chain + Draw~par,value.var="estimate")
```

Hold resource relationship at 0, what does trait-matching look like?


```r
#Trajectories from posterior
predy<-trajF(alpha=castdf$intercept,beta1=castdf$gamma1,x=indat$Traitmatch,resources=indat$scaledR,beta2=0,beta3=0,type='hdi')

ggplot(data=predy,aes(x=x)) + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.1,fill="red")  + geom_line(aes(y=mean),size=.5,col="red",linetype="dashed") + theme_bw() + ylab("Interactions") + xlab("Difference between Bill and Corolla Length") + geom_point(data=indat,aes(x=Traitmatch,y=Camera)) + geom_point(data=indat,aes(x=Traitmatch,y=Transect))
```

<img src="figureObserved/unnamed-chunk-33-1.png" title="" alt="" style="display: block; margin: auto;" />

##Full posterior prediction


```r
#Trajectories from posterior
predy<-trajF(alpha=castdf$intercept,beta1=castdf$gamma1,x=indat$Traitmatch,resources=indat$scaledR,beta2=castdf$gamma3,beta3=castdf$gamma3,type='hdi')

ggplot(data=predy,aes(x=x)) + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.2,fill="red")  +  theme_bw() + ylab("Interactions") + xlab("Difference between Bill and Corolla Length") + geom_point(data=indat,aes(x=Traitmatch,y=Camera)) + geom_line(aes(y=mean)) + geom_point(data=indat,aes(x=Traitmatch,y=Transect)) 
```

<img src="figureObserved/unnamed-chunk-34-1.png" title="" alt="" style="display: block; margin: auto;" />

## At High and Low Resource Availability


```r
#Trajectories from posterior
predH<-trajF(alpha=castdf$intercept,beta1=castdf$gamma1,x=indat[indat$ BUsed_Flowers==1,"Traitmatch"],resources=indat[indat$ BUsed_Flowers==1,"scaledR"],beta2=castdf$gamma2,beta3=castdf$gamma3,type='quantile')

predL<-trajF(alpha=castdf$intercept,beta1=castdf$gamma1,x=indat[indat$ BUsed_Flowers==0,"Traitmatch"],resources=indat[indat$ BUsed_Flowers==0,"scaledR"],beta2=castdf$gamma2,beta3=castdf$gamma3,type='hdi')

predhl<-melt(list(High=predH,Low=predL),id.vars=colnames(predH))

colnames(predhl)[5]<-"BFlowerL"

indat$BFlowerL<-factor(as.character(indat$ BUsed_Flowers))
levels(indat$BFlowerL)<-c("Low","High")

ggplot(data=predhl,aes(x=x)) + geom_ribbon(aes(ymin=lower,ymax=upper,fill=BFlowerL),alpha=0.2)  + geom_line(aes(y=mean,col=BFlowerL),size=.8) + theme_bw() + ylab("Interactions") + xlab("Difference between Bill and Corolla Length") + geom_point(data=mindat,aes(x=Traitmatch,y=value))+ labs(fill="Resource Availability",col="Resource Availability") 
```

<img src="figureObserved/unnamed-chunk-35-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
ggsave("Figures/AllRegression.jpeg",height=5,width=7)
```


##Species Predictions


```r
castdf<-dcast(pars_detect[pars_detect$par %in% c("beta1","beta2","beta3","alpha"),], species +Chain +Draw ~par ,value.var="estimate")

#Turn to 
castdf$species<-factor(castdf$species,levels=1:max(as.numeric(castdf$species)))

species.split<-split(castdf,list(castdf$species),drop = T)

species.traj<-list()

for(d in 1:length(species.split)){
  x<-species.split[[d]]
  index<-unique(x$species)
  
  #get data for those species
  billd<-indat[indat$jBird %in% index,]

  #scale resources
  species.traj[[d]]<-trajF(alpha=x$alpha,beta1=x$beta1,beta2=x$beta2,beta3=x$beta3,resources=billd$scaledR,x=billd$Traitmatch,type='hdi')
  }

names(species.traj)<-names(species.split)

species.traj<-melt(species.traj,id.var=colnames(species.traj[[1]]))

#split out names and model
species.traj[,c("Index")]<-colsplit(species.traj$L1,"\\.",c("Index"))

spe<-merge(species.traj,jagsIndexBird,by.x="Index",by.y="jBird")

#plot and compare to original data
ggplot(data=spe,aes(x=x)) + geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.2,fill='red') + geom_line(aes(y=mean),size=.5) + theme_bw() + ylab("Occurrence Probability")+ xlab("Difference between Bill and Corolla Length") + facet_wrap(~Hummingbird,scales="free",ncol=4) + geom_point(data=mindat,aes(x=Traitmatch,y=value,shape=variable),size=2.5) + labs(shape="Sampling Method")
```

<img src="figureObserved/unnamed-chunk-36-1.png" title="" alt="" style="display: block; margin: auto;" />

###Species Predictions: High and Low Availability


```r
castdf<-dcast(pars_detect[pars_detect$par %in% c("beta1","beta2","beta3","alpha"),], species +Chain +Draw ~par ,value.var="estimate")

#Turn to 
castdf$species<-factor(castdf$species,levels=1:max(as.numeric(castdf$species)))

species.split<-split(castdf,list(castdf$species),drop = T)

species.traj<-list()

for(d in 1:length(species.split)){
  x<-species.split[[d]]
  index<-unique(x$species)
  
  #get data for those species
  billd<-indat[indat$jBird %in% index,]

  sl<-trajF(alpha=x$alpha,beta1=x$beta1,beta2=x$beta2,beta3=x$beta3,resources=billd[billd$ BUsed_Flowers==0,"scaledR"],x=billd[billd$ BUsed_Flowers==0,"Traitmatch"],type='hdi')
  sh<-trajF(alpha=x$alpha,beta1=x$beta1,beta2=x$beta2,beta3=x$beta3,resources=billd[billd$ BUsed_Flowers==1,"scaledR"],x=billd[billd$ BUsed_Flowers==1,"Traitmatch"],type='hdi')
  sm<-melt(list(High=sh,Low=sl),id.vars=colnames(sl))
  colnames(sm)[5]<-"Resources"
  species.traj[[d]]<-sm
  }

names(species.traj)<-names(species.split)

species.traj<-melt(species.traj,id.var=colnames(species.traj[[1]]))

#split out names and model
species.traj[,c("Index")]<-colsplit(species.traj$L1,"\\.",c("Index"))

spe<-merge(species.traj,jagsIndexBird,by.x="Index",by.y="jBird")

#plot and compare to original data
ggplot(data=spe,aes(x=x)) + geom_ribbon(aes(ymin=lower,ymax=upper,fill=Resources),alpha=0.2) + geom_line(aes(y=mean,col=Resources),size=.5) + theme_bw() + ylab("Occurrence Probability")+ xlab("Difference between Bill and Corolla Length") + facet_wrap(~Hummingbird,scales="free",ncol=3) + geom_point(data=mindat,aes(x=Traitmatch,y=value,shape=variable),size=1.5,alpha=.5)
```

<img src="figureObserved/unnamed-chunk-37-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
ggsave("Figures/SpeciesRegression.jpeg",height=6,width=7)
```

##Species Level Interaction


```r
castdf<-dcast(pars_detect[pars_detect$par %in% c("beta1","beta2","beta3","alpha"),], species +Chain + Draw~par,value.var="estimate")

#Turn to 
castdf$species<-factor(castdf$species,levels=1:max(as.numeric(castdf$species)))

species.split<-split(castdf,list(castdf$species),drop = T)

species.traj<-list()

for(d in 1:length(species.split)){
  dat<-species.split[[d]]
  index<-unique(dat$species)
  
  #get data for those species
  billd<-mindat[mindat$jBird %in% index,]

  #Calculate interaction effect
  species.traj[[d]]<-intF(alpha=dat$alpha,beta1=dat$beta1,x=billd[billd$value > 0 & !is.na(billd$value),'Traitmatch'],resources=billd[billd$value > 0 & !is.na(billd$value),'scaledR'],beta2=dat$beta2,beta3=dat$beta3,type='hdi')
  
  }

names(species.traj)<-names(species.split)
species.traj<-melt(species.traj,id.var=colnames(species.traj[[1]]))

#split out names and model
species.traj[,c("Index")]<-colsplit(species.traj$L1,"\\.",c("Index"))

spe<-merge(species.traj,jagsIndexBird,by.x="Index",by.y="jBird")

#match colnames

#plot and compare to original data
ggplot(data=spe,aes(x=x)) + geom_ribbon(aes(ymin=lower,ymax=upper,fill=Hummingbird),alpha=0.3) + geom_line(aes(y=mean,col=Hummingbird),size=1) + theme_bw() + xlab("Difference between Bill and Corolla Length")  + ylab("Effect of Resources on Trait Difference") + facet_wrap(~Hummingbird,scales="free",ncol=3)
```

<img src="figureObserved/unnamed-chunk-38-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
ggsave("Figures/SpeciesInteraction.jpeg",height=6,width=7)
```

These plots can be tricky to interpret if one forgets that trait matching as a covariate is a distance. Therefore remember that a positive slope in the plot above indiciates, "As resources increase species use flowers less similiar to their bill lengths". 

##Interaction density functions
Let's take a closer look at distribution of interaction effect posteriors values for each species.


```r
post<-pars_detect %>% filter(par %in% "beta3") %>% group_by(species) %>% summarize(mean=mean(estimate),median=median(estimate),lower=quantile(probs=0.025,estimate),upper=quantile(probs=0.975,estimate),hdi_l=hdi(estimate)[[1]],hdi_u=hdi(estimate)[[2]]) %>% melt(id.vars='species')
ggplot(pars_detect[pars_detect$par %in% "beta3",],aes(x=estimate)) + geom_histogram() + facet_wrap(~species,scales='free',ncol=3) + geom_vline(data=post,aes(xintercept=value,col=variable))
```

<img src="figureObserved/unnamed-chunk-39-1.png" title="" alt="" style="display: block; margin: auto;" />

##Traitmatching and Bill Length

Do species with long bill lengths have positive traitmatching effects?


```r
#species names
b<-pars_detect[pars_detect$par %in% "beta1",]
b<-merge(b,jagsIndexBird,by.x="species",by.y="jBird")

#traits
b<-merge(b,hum.morph,by.x="Hummingbird",by.y="English")

post<-b %>% filter(par %in% "beta1") %>% group_by(Hummingbird) %>% summarize(mean=mean(estimate),median=median(estimate),lower=quantile(probs=0.025,estimate),upper=quantile(probs=0.975,estimate),hdi_l=hdi(estimate)[[1]],hdi_u=hdi(estimate)[[2]]) %>% melt(id.vars='Hummingbird')

#get order of mean posterior
ord<-post %>% filter(variable=="mean") %>% arrange(value) %>% .$Hummingbird

b$Hummingbird<-factor(b$Hummingbird,levels=ord)
ggplot(b,aes(y=estimate,x=Hummingbird,fill=Total_Culmen)) + geom_violin() + coord_flip() + scale_fill_continuous(low='blue',high='red') + ggtitle("Traitmatching and Bill Length") + theme_bw()
```

<img src="figureObserved/unnamed-chunk-40-1.png" title="" alt="" style="display: block; margin: auto;" />

##Interaction and Bill Length

Do species with long bill lengths have positive interaction effects?


```r
#species names
b<-pars_detect[pars_detect$par %in% "beta3",]
b<-merge(b,jagsIndexBird,by.x="species",by.y="jBird")

#traits
b<-merge(b,hum.morph,by.x="Hummingbird",by.y="English")

post<-b %>% filter(par %in% "beta3") %>% group_by(Hummingbird) %>% summarize(mean=mean(estimate),median=median(estimate),lower=quantile(probs=0.025,estimate),upper=quantile(probs=0.975,estimate),hdi_l=hdi(estimate)[[1]],hdi_u=hdi(estimate)[[2]]) %>% melt(id.vars='Hummingbird')

#get order of mean posterior
ord<-post %>% filter(variable=="mean") %>% arrange(value) %>% .$Hummingbird

b$Hummingbird<-factor(b$Hummingbird,levels=ord)
ggplot(b,aes(y=estimate,x=Hummingbird,fill=Total_Culmen)) + geom_violin() + coord_flip() + scale_fill_continuous(low='blue',high='red') + ggtitle("Interaction Effect and Bill Length") + theme_bw()
```

<img src="figureObserved/unnamed-chunk-41-1.png" title="" alt="" style="display: block; margin: auto;" />

#Estimated niche breadth


```r
castdf<-dcast(pars_detect[pars_detect$par %in% c("beta1","beta2","beta3","alpha"),], species +Chain + Draw~par,value.var="estimate")

#Turn to 
castdf$species<-factor(castdf$species,levels=1:max(as.numeric(castdf$species)))

species.split<-split(castdf,list(castdf$species),drop = T)

species.traj<-lapply(species.split,function(dat){
  index<-unique(dat$species)
  
  #get data for those species
  billd<-indat[indat$jBird %in% index,]
  
  d<-data.frame(alpha=dat$alpha,beta1=dat$beta1,beta2=dat$beta2,beta3=dat$beta3)
  
  #fit regression for each input estimate
  sampletraj<-list()
  
  for (y in 1:nrow(d)){
    v=exp(d$alpha[y] + d$beta1[y] * billd$Traitmatch + d$beta2[y] * billd$scaledR + d$beta3[y] * billd$Traitmatch*billd$scaledR)
    
    sampletraj[[y]]<-data.frame(x=as.numeric(billd$Traitmatch),y=as.numeric(v),r=as.numeric(billd$scaledR),jBird=billd$jBird,jPlant=billd$jPlant,jTime=billd$jTime)
  }
  
  sample_all<-rbind_all(sampletraj)
})
  
species.traj<-rbind_all(species.traj)
```

Mean Estimates for Corolla Sizes


```r
species.mean<-species.traj %>% group_by(jBird,jPlant,r) %>% summarize(Traitmatch=unique(x),lambda=mean(y))

species.mean<-merge(species.mean,indat[,colnames(indat) %in% c("jBird","jPlant","jTime","Hummingbird","Iplant_Double")])

#get corolla sizes
species.mean<-merge(species.mean,fl.morph,by.x="Iplant_Double", by.y="Group.1")

ggplot(species.mean) + geom_density2d(aes(x=TotalCorolla,y=lambda,col=Hummingbird)) + theme_bw() + facet_wrap(~r,scales="free") 
```

<img src="figureObserved/unnamed-chunk-43-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
#bill order
ord<-hum.morph %>% arrange(Total_Culmen) %>% .$English
species.mean$Hummingbird<-factor(species.mean$Hummingbird,levels=ord)

#add level to hum.morph to match naming convention
species.mean<-merge(species.mean,hum.morph[,c("English","Total_Culmen")],by.x="Hummingbird",by.y="English")

p<-ggplot(species.mean,aes(x=TotalCorolla,y=lambda,col=as.factor(r))) + geom_line(size=.9) + geom_vline(aes(xintercept=Total_Culmen),linetype='dashed') + facet_wrap(~Hummingbird,ncol=3,scales="free_y")  + theme_bw() + ylab("Estimated Daily Interaction Rate") + scale_color_manual("Resource Availability",labels=c("Low","High"),values=c("Blue","Red")) + xlab("Flower Corolla Length (mm)") 
p
```

<img src="figureObserved/unnamed-chunk-43-2.png" title="" alt="" style="display: block; margin: auto;" />

```r
ggsave("Figures/ResponseCurves.jpeg",height=6.5,width=9)
```

Density curves


```r
ggplot(species.mean) + geom_density2d(aes(x=TotalCorolla,y=lambda,col=as.factor(r))) + theme_bw() + facet_wrap(~Hummingbird,scales="free",ncol=3)+ scale_color_manual("Resources Availability",labels=c("Low","High"),values=c("Blue","Red")) + ggtitle("2D Density Plots") + geom_vline(aes(xintercept=Total_Culmen),linetype='dashed')
```

<img src="figureObserved/unnamed-chunk-44-1.png" title="" alt="" style="display: block; margin: auto;" />

#Niche Breadth 


```r
species.mean<-species.traj %>% group_by(jBird,jPlant,r) %>% summarize(Traitmatch=unique(x),lambda=mean(y),lambda_low=quantile(y,0.05),lambda_high=quantile(y,0.95))

species.mean<-merge(species.mean,indat[,colnames(indat) %in% c("jBird","jPlant","jTime","Hummingbird","Iplant_Double"," BUsed_Flowers")])

#get corolla sizes
species.mean<-merge(species.mean,fl.morph,by.x="Iplant_Double", by.y="Group.1")

#bill order
ord<-hum.morph %>% arrange(Total_Culmen) %>% .$English
species.mean$Hummingbird<-factor(species.mean$Hummingbird,levels=ord)

#add level to hum.morph to match naming convention
species.mean<-merge(species.mean,hum.morph[,c("English","Total_Culmen")],by.x="Hummingbird",by.y="English")

ggplot(species.mean) + geom_ribbon(alpha=0.2,aes(x=TotalCorolla,ymin=lambda_low,ymax=lambda_high,fill=as.factor(r))) + theme_bw() + facet_wrap(~Hummingbird,scales="free",ncol=3)+ scale_fill_manual("Resources Availability",labels=c("Low","High"),values=c("Blue","Red")) + ggtitle("Niche Breadth") + geom_vline(aes(xintercept=Total_Culmen),linetype='dashed') + geom_line(aes(x=TotalCorolla,y=lambda,fill=as.factor(r))) + ylab("Estimated Daily Intensity") + xlab("Corolla Length (mm)")
```

<img src="figureObserved/unnamed-chunk-45-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
ggsave("Figures/NicheBreadth.jpeg",height=6,width=9)
```

Standardized intensity of niche breadth


```r
sm<-split(species.mean,species.mean$Hummingbird,drop=T)
sm<-lapply(sm,function(x){
  x$lambda<-x$lambda/max(x$lambda)
  x$lambda_low<-x$lambda_low/max(x$lambda_low)
  x$lambda_high<-x$lambda_high/max(x$lambda_high)
  return(x)
})
species.max<-rbind_all(sm)

ggplot(species.max,aes(x=TotalCorolla,y=lambda,col=as.factor(r))) + geom_density2d() + theme_bw() + scale_color_manual("Resources Availability",labels=c("Low","High"),values=c("Blue","Red")) + scale_y_continuous(breaks=c(0,1),labels=c("Low","High")) + xlab("Flower Corolla Length") + ylab("Hummingbird Visitation")
```

<img src="figureObserved/unnamed-chunk-46-1.png" title="" alt="" style="display: block; margin: auto;" />

#Model Criticism

##Residuals


```r
chisq<-function(o,e){(o-e)^2/(e+0.5)}

spres<-split(species.mean,list(species.mean$Hummingbird,species.mean$Iplant_Double,species.mean$r),drop = T)

obs<-indat %>% filter(Hummingbird %in% x$Hummingbird,Iplant_Double %in% x$Iplant_Double,BUsed_Flowers %in% x$r)

resids<-c(sapply(na.exclude(c(obs$Camera,obs$Transect)),function(y){
  chisq(o=x$lambda,e=y)
}))

data.frame(chisq=resids,Hummigbird=unique(x$Hummingbird),Iplant_Double=unique(x$Iplant_Double),r=unique(x$r))
```


```r
gc()
```

```
##            used  (Mb) gc trigger  (Mb) max used  (Mb)
## Ncells  1933985 103.3    3205452 171.2  3205452 171.2
## Vcells 35171537 268.4   73658852 562.0 73658451 562.0
```

```r
save.image("Observed.Rdata")
```


