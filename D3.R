library(d3Network)

a<-rawdat[rawdat$value>0,]
d3Network::d3SimpleNetwork(rawdat[rawdat$value>0,],file="humbipartite.html")

Flowers<-levels(a$Flowers)
oFlowers<-order(levels(a$Flowers))-1


Birds<-levels(a$Birds)
oBirds<-max(oFlowers) + order(levels(a$Birds))

Nodes<-data.frame(Name=c(Flowers,Birds),
                  Node=c(oFlowers,oBirds),
                  group=c(rep("Flowers2",length(Flowers)),rep("Birds",length(Birds))))

#append id to data
#append to data
a$Source<-Nodes[match(a$Flowers,Nodes$Name),"Node"]
a$Target<-Nodes[match(a$Birds,Nodes$Name),"Node"]

#Old script
d3Network::d3ForceNetwork(Links=a,NodeID="Name",Nodes=Nodes,Target="Target",Value="value",Source="Source",file="humbipartiteForce.html",Group="group",charge = -400,d3Script = "http://d3js.org/d3.v3.min.js",opacity=.6,linkDistance = "function(d){return d.value * 5}" )

#new script
networkD3::forceNetwork(Links=a,NodeID="Name",Nodes=Nodes,Target="Target",Value="value",Source="Source",Group="group",charge = -500,colourScale = "d3.scale.category10()") %>% networkD3::saveNetwork("humbipartiteForceNew.html")
