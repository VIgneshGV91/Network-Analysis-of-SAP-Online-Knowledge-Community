#Vigneshwaran Giri Velumani
#Read in the hs0 data over the internet using the read.table() function.
getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
setwd("~setyourworkingdirectory")

# clear everything out of memory
rm(list=ls()) 

# This is a 10% random sample for class exercises
infile_sub<-"SAPFull_SubGraph_EdgeList.csv"

## Load package
library(igraph)
el=read.csv(infile_sub, header = TRUE, sep = ",")
class(el)
# ---
# [1] "data.frame"
# ---
# Describe the data frame
str(el)

# Create the directed graph object
g_SAPSub=graph.data.frame(el, directed = TRUE, vertices= NULL)

# Edges
ecount(g_SAPSub) #6090
## Vertices
vcount(g_SAPSub)#3415


## Check whether Self_loops exist, as do multiple edges
is.simple(g_SAPSub)
#Is it a simple graph? No!
# ---
#[1] FALSE
# ---

# Create edge weights
E(g_SAPSub)$weight <-1
E(g_SAPSub)$weight 
g_SAPSub_simpl<-simplify(g_SAPSub, edge.attr.comb="sum")
is.simple(g_SAPSub_simpl)


# Edges
ecount(g_SAPSub_simpl)#4120
## Vertices
vcount(g_SAPSub_simpl)#3415

# Use the inverse of log weight for some of the network measure calculations
inv_weight<-1/log(E(g_SAPSub_simpl)$weight  + 1)
View(inv_weight)
num_weight<-E(g_SAPSub_simpl)$weight 
View(num_weight)
length(inv_weight)
length(num_weight)

E(g_SAPSub_simpl)$weight <-inv_weight

# You can see the neighbors of some selected nodes
neighbors(g_SAPSub_simpl, v=c('900'))
neighbors(g_SAPSub_simpl, v=c('592540'))

class(g_SAPSub_simpl)
plot(g_SAPSub_simpl,vertex.label=NA, main="Plot of SAP Network")
plot(g_SAPSub_simpl,vertex.label=V(g_SAPSub_simpl)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2)

l <- layout.fruchterman.reingold(g_SAPSub_simpl)
plot(g_SAPSub_simpl, layout=l,vertex.label=NA,main="fruchterman.reingold Algorithm")
#Plotting the same with some parameters
plot(g_SAPSub_simpl,vertex.label=V(g_SAPSub_simpl)$name, vertex.label.dist=1.5,vertex.size=8,layout = layout.reingold.tilford,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2)

## For example, there is only a path to 814 from 511; not from it. So there are outpaths from 511 to 814, but no in paths. (or the other vectors)
E(g_SAPSub_simpl)$weight <- inv_weight

reciprocity(g_SAPSub_simpl)
#The very less reciprocity value of 0.005825243 shows that the whole network is heavily unidirectional which is understandable because in any 
#knowledge sharing community forum/platform there will be very less people who have more knowledge than others in multiple domains.
#It also signifies that most of them are not mutually linked with each other except for the very few active responders of question
is.connected(g_SAPSub_simpl)

is.connected(g_SAPSub_simpl, mode="strong")
is.connected(g_SAPSub_simpl, mode="weak")

# Diameter with both kinds of weights

# Clustering
transitivity(g_SAPSub_simpl, weights = inv_weight)
#A low traisitivity value of 0.009985725 which is 399429 out of 40000000 indirect connections are transitive. This could be interpreted in our
#scenaio like clustering happens only within groups of people in the same domain and only a very few active responders serve as the common nodes 
#in the entire network

# Avg. path length and diameter
average.path.length(g_SAPSub_simpl, directed=TRUE)
#The average path length value is about 3.983

diameter(g_SAPSub_simpl)
diameter(g_SAPSub_simpl, weights= num_weight)
#Using Regular weights method, the diameter is found to be about 26

diameter(g_SAPSub_simpl, weights= inv_weight)
#Using the inverse of log weight, the diameter is found to be about 14.27 which seems reasonable for such a large SAP knowledge sharing community
#network considering the number of nodes and edges in the network

# Summarize the graph structure
summary(g_SAPSub_simpl)

# Clique structure: 5 cliques of size 5, 39 cliques of size 4, 335 triangles
table(sapply(maximal.cliques(g_SAPSub_simpl), length))

cliques <- maximal.cliques(g_SAPSub_simpl)

a <- largest.cliques(g_SAPSub_simpl)

cliques <- c(a[[1]],a[[2]],a[[3]],a[[4]],a[[5]])
g2 <- induced.subgraph(graph=g_SAPSub_simpl,vids=(cliques))
plot(g2,main="cliques of size 5")

fourcliques <- max_cliques(g_SAPSub_simpl, min = 4, max = 4, subset = NULL,file = NULL)

fourcliquesall<- c(fourcliques[[1]],
                   fourcliques[[2]],
                   fourcliques[[3]],
                   fourcliques[[4]],
                   fourcliques[[5]],
                   fourcliques[[6]],
                   fourcliques[[7]],
                   fourcliques[[8]],
                   fourcliques[[9]],
                   fourcliques[[10]],
                   fourcliques[[11]],
                   fourcliques[[12]],
                   fourcliques[[13]],
                   fourcliques[[14]],
                   fourcliques[[15]],
                   fourcliques[[16]],
                   fourcliques[[17]],
                   fourcliques[[18]],
                   fourcliques[[19]],
                   fourcliques[[20]],
                   fourcliques[[21]],
                   fourcliques[[22]],
                   fourcliques[[23]],
                   fourcliques[[24]],
                   fourcliques[[25]],
                   fourcliques[[26]],
                   fourcliques[[27]],
                   fourcliques[[28]],
                   fourcliques[[29]],
                   fourcliques[[30]],
                   fourcliques[[31]],
                   fourcliques[[32]],
                   fourcliques[[33]],
                   fourcliques[[34]],
                   fourcliques[[35]],
                   fourcliques[[36]],
                   fourcliques[[37]],
                   fourcliques[[38]],
                   fourcliques[[39]])

g2 <- induced.subgraph(graph=g_SAPSub_simpl,vids=(fourcliquesall))
plot(g2,main="cliques of size 4")
plot(g2,vertex.label=V(g2)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2)



threecliques <- max_cliques(g_SAPSub_simpl, min = 3, max = 3, subset = NULL,file = NULL)

threecliquesall <- c(threecliques[[1]],
                     threecliques[[2]],
                     threecliques[[3]],
                     threecliques[[4]],
                     threecliques[[5]],
                     threecliques[[6]],
                     threecliques[[7]],
                     threecliques[[8]],
                     threecliques[[9]],
                     threecliques[[10]],
                     threecliques[[11]],
                     threecliques[[12]],
                     threecliques[[13]],
                     threecliques[[14]],
                     threecliques[[15]],
                     threecliques[[16]],
                     threecliques[[17]],
                     threecliques[[18]],
                     threecliques[[19]],
                     threecliques[[20]],
                     threecliques[[21]],
                     threecliques[[22]],
                     threecliques[[23]],
                     threecliques[[24]],
                     threecliques[[25]],
                     threecliques[[26]],
                     threecliques[[27]],
                     threecliques[[28]],
                     threecliques[[29]],
                     threecliques[[30]],
                     threecliques[[31]],
                     threecliques[[32]],
                     threecliques[[33]],
                     threecliques[[34]],
                     threecliques[[35]],
                     threecliques[[36]],
                     threecliques[[37]],
                     threecliques[[38]],
                     threecliques[[39]],
                     threecliques[[40]],
                     threecliques[[41]],
                     threecliques[[42]],
                     threecliques[[43]],
                     threecliques[[44]],
                     threecliques[[45]],
                     threecliques[[46]],
                     threecliques[[47]],
                     threecliques[[48]],
                     threecliques[[49]],
                     threecliques[[50]],
                     threecliques[[51]],
                     threecliques[[52]],
                     threecliques[[53]],
                     threecliques[[54]],
                     threecliques[[55]],
                     threecliques[[56]],
                     threecliques[[57]],
                     threecliques[[58]],
                     threecliques[[59]],
                     threecliques[[60]],
                     threecliques[[61]],
                     threecliques[[62]],
                     threecliques[[63]],
                     threecliques[[64]],
                     threecliques[[65]],
                     threecliques[[66]],
                     threecliques[[67]],
                     threecliques[[68]],
                     threecliques[[69]],
                     threecliques[[70]],
                     threecliques[[71]],
                     threecliques[[72]],
                     threecliques[[73]],
                     threecliques[[74]],
                     threecliques[[75]],
                     threecliques[[76]],
                     threecliques[[77]],
                     threecliques[[78]],
                     threecliques[[79]],
                     threecliques[[80]],
                     threecliques[[81]],
                     threecliques[[82]],
                     threecliques[[83]],
                     threecliques[[84]],
                     threecliques[[85]],
                     threecliques[[86]],
                     threecliques[[87]],
                     threecliques[[88]],
                     threecliques[[89]],
                     threecliques[[90]],
                     threecliques[[91]],
                     threecliques[[92]],
                     threecliques[[93]],
                     threecliques[[94]],
                     threecliques[[95]],
                     threecliques[[96]],
                     threecliques[[97]],
                     threecliques[[98]],
                     threecliques[[99]],
                     threecliques[[100]],
                     threecliques[[101]],
                     threecliques[[102]],
                     threecliques[[103]],
                     threecliques[[104]],
                     threecliques[[105]],
                     threecliques[[106]],
                     threecliques[[107]],
                     threecliques[[108]],
                     threecliques[[109]],
                     threecliques[[110]],
                     threecliques[[111]],
                     threecliques[[112]],
                     threecliques[[113]],
                     threecliques[[114]],
                     threecliques[[115]],
                     threecliques[[116]],
                     threecliques[[117]],
                     threecliques[[118]],
                     threecliques[[119]],
                     threecliques[[120]],
                     threecliques[[121]],
                     threecliques[[122]],
                     threecliques[[123]],
                     threecliques[[124]],
                     threecliques[[125]],
                     threecliques[[126]],
                     threecliques[[127]],
                     threecliques[[128]],
                     threecliques[[129]],
                     threecliques[[130]],
                     threecliques[[131]],
                     threecliques[[132]],
                     threecliques[[133]],
                     threecliques[[134]],
                     threecliques[[135]],
                     threecliques[[136]],
                     threecliques[[137]],
                     threecliques[[138]],
                     threecliques[[139]],
                     threecliques[[140]],
                     threecliques[[141]],
                     threecliques[[142]],
                     threecliques[[143]],
                     threecliques[[144]],
                     threecliques[[145]],
                     threecliques[[146]],
                     threecliques[[147]],
                     threecliques[[148]],
                     threecliques[[149]],
                     threecliques[[150]],
                     threecliques[[151]],
                     threecliques[[152]],
                     threecliques[[153]],
                     threecliques[[154]],
                     threecliques[[155]],
                     threecliques[[156]],
                     threecliques[[157]],
                     threecliques[[158]],
                     threecliques[[159]],
                     threecliques[[160]],
                     threecliques[[161]],
                     threecliques[[162]],
                     threecliques[[163]],
                     threecliques[[164]],
                     threecliques[[165]],
                     threecliques[[166]],
                     threecliques[[167]],
                     threecliques[[168]],
                     threecliques[[169]],
                     threecliques[[170]],
                     threecliques[[171]],
                     threecliques[[172]],
                     threecliques[[173]],
                     threecliques[[174]],
                     threecliques[[175]],
                     threecliques[[176]],
                     threecliques[[177]],
                     threecliques[[178]],
                     threecliques[[179]],
                     threecliques[[180]],
                     threecliques[[181]],
                     threecliques[[182]],
                     threecliques[[183]],
                     threecliques[[184]],
                     threecliques[[185]],
                     threecliques[[186]],
                     threecliques[[187]],
                     threecliques[[188]],
                     threecliques[[189]],
                     threecliques[[190]],
                     threecliques[[191]],
                     threecliques[[192]],
                     threecliques[[193]],
                     threecliques[[194]],
                     threecliques[[195]],
                     threecliques[[196]],
                     threecliques[[197]],
                     threecliques[[198]],
                     threecliques[[199]],
                     threecliques[[200]],
                     threecliques[[201]],
                     threecliques[[202]],
                     threecliques[[203]],
                     threecliques[[204]],
                     threecliques[[205]],
                     threecliques[[206]],
                     threecliques[[207]],
                     threecliques[[208]],
                     threecliques[[209]],
                     threecliques[[210]],
                     threecliques[[211]],
                     threecliques[[212]],
                     threecliques[[213]],
                     threecliques[[214]],
                     threecliques[[215]],
                     threecliques[[216]],
                     threecliques[[217]],
                     threecliques[[218]],
                     threecliques[[219]],
                     threecliques[[220]],
                     threecliques[[221]],
                     threecliques[[222]],
                     threecliques[[223]],
                     threecliques[[224]],
                     threecliques[[225]],
                     threecliques[[226]],
                     threecliques[[227]],
                     threecliques[[228]],
                     threecliques[[229]],
                     threecliques[[230]],
                     threecliques[[231]],
                     threecliques[[232]],
                     threecliques[[233]],
                     threecliques[[234]],
                     threecliques[[235]],
                     threecliques[[236]],
                     threecliques[[237]],
                     threecliques[[238]],
                     threecliques[[239]],
                     threecliques[[240]],
                     threecliques[[241]],
                     threecliques[[242]],
                     threecliques[[243]],
                     threecliques[[244]],
                     threecliques[[245]],
                     threecliques[[246]],
                     threecliques[[247]],
                     threecliques[[248]],
                     threecliques[[249]],
                     threecliques[[250]],
                     threecliques[[251]],
                     threecliques[[252]],
                     threecliques[[253]],
                     threecliques[[254]],
                     threecliques[[255]],
                     threecliques[[256]],
                     threecliques[[257]],
                     threecliques[[258]],
                     threecliques[[259]],
                     threecliques[[260]],
                     threecliques[[261]],
                     threecliques[[262]],
                     threecliques[[263]],
                     threecliques[[264]],
                     threecliques[[265]],
                     threecliques[[266]],
                     threecliques[[267]],
                     threecliques[[268]],
                     threecliques[[269]],
                     threecliques[[270]],
                     threecliques[[271]],
                     threecliques[[272]],
                     threecliques[[273]],
                     threecliques[[274]],
                     threecliques[[275]],
                     threecliques[[276]],
                     threecliques[[277]],
                     threecliques[[278]],
                     threecliques[[279]],
                     threecliques[[280]],
                     threecliques[[281]],
                     threecliques[[282]],
                     threecliques[[283]],
                     threecliques[[284]],
                     threecliques[[285]],
                     threecliques[[286]],
                     threecliques[[287]],
                     threecliques[[288]],
                     threecliques[[289]],
                     threecliques[[290]],
                     threecliques[[291]],
                     threecliques[[292]],
                     threecliques[[293]],
                     threecliques[[294]],
                     threecliques[[295]],
                     threecliques[[296]],
                     threecliques[[297]],
                     threecliques[[298]],
                     threecliques[[299]],
                     threecliques[[300]],
                     threecliques[[301]],
                     threecliques[[302]],
                     threecliques[[303]],
                     threecliques[[304]],
                     threecliques[[305]],
                     threecliques[[306]],
                     threecliques[[307]],
                     threecliques[[308]],
                     threecliques[[309]],
                     threecliques[[310]],
                     threecliques[[311]],
                     threecliques[[312]],
                     threecliques[[313]],
                     threecliques[[314]],
                     threecliques[[315]],
                     threecliques[[316]],
                     threecliques[[317]],
                     threecliques[[318]],
                     threecliques[[319]],
                     threecliques[[320]],
                     threecliques[[321]],
                     threecliques[[322]],
                     threecliques[[323]],
                     threecliques[[324]],
                     threecliques[[325]],
                     threecliques[[326]],
                     threecliques[[327]],
                     threecliques[[328]],
                     threecliques[[329]],
                     threecliques[[330]],
                     threecliques[[331]],
                     threecliques[[332]],
                     threecliques[[333]],
                     threecliques[[334]],
                     threecliques[[335]])
g2 <- induced.subgraph(graph=g_SAPSub_simpl,vids=(threecliquesall))
plot(g2,main="cliques of size 3",vertex.label=NA)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
plot(g2,vertex.label=V(g2)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2)


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#The network has 5 Five Cliques, 39 Four Cliques, 335 Three Cliques and 3320 Two Cliques.
#So the number of Triadic Closure is 335

#A <- get.adjacency(g_SAPSub_simpl, sparse=FALSE)

# Can try either of these weighting schemes for various measures; they change the interpretation of the measures
# Inverse weight
E(g_SAPSub_simpl)$weight <- inv_weight
# Regular weight
E(g_SAPSub_simpl)$weight <- num_weight

# Embeddedness/ inverse of structural hole access (see Burt 2004)
constraints_SAP <- round(constraint(g_SAPSub_simpl, nodes=V(g_SAPSub_simpl)), digits=4)
View(constraints_SAP)
#To find the nodes which act as the structural holes in this network we have to look at the nodes which have the lowest constraint Value. So by this
#definition the top 5 nodes which act as structural holes are: 592540, 821176, 3583224, 131143, 44034
#According to our network nodes with high Structural holes or Lowest Embeddedness stated that, the ideologies and knowledge of these people are unique 
#and have very little overlap with others. So in this knowledge sharing community they act as nodes that transfer novel information among many clusters
#in the communities in the whole network

#Makes a distinct color for the nodes '511','541'.'518','519'.
sub_net<-induced.subgraph(g_SAPSub_simpl, v=c("592540",
                    "821176",
                    "3583224",
                    "131143",
                    "44034",
                    "2711777",
                    "1979898",
                    "695093",
                    "1384451",
                    "3496408",
                    "337106",
                    "393756",
                    "3609769",
                    "4183082",
                    "5420",
                    "3713327",
                    "3588893",
                    "3519026",
                    "3461421",
                    "3701705",
                    "1666938",
                    "4166841",
                    "303331",
                    "1806379",
                    "307549",
                    "3907626",
                    "900",
                    "3621960",
                    "479914",
                    "3988816",
                    "3907737",
                    "3821274",
                    "577986",
                    "3581732",
                    "3075411",
                    "3677174",
                    "3704501",
                    "3568264",
                    "3949974",
                    "3477685",
                    "2470693",
                    "1195854",
                    "3552437",
                    "3621372",
                    "1866982",
                    "4229010",
                    "3564578",
                    "2201361",
                    "660123",
                    "3277425"))

#Makes a distinct color for the nodes
V(sub_net)$color="yellow"
V(sub_net)[c("592540","821176","3583224","131143","44034","2711777","1979898","695093","1384451","3496408")]$color="dodgerblue"


#Changes the shape of nodes '511','541'.'518','519' to square while keeping the rest of the nodes in circle.
V(sub_net)$shape<-"circle"
V(sub_net)[c("592540","821176","3583224","131143","44034","2711777","1979898","695093","1384451","3496408")]$shape<-"square"

# Makes the width of the edges propotional to the (logarithm of) edge weights
E(sub_net)$width = log(E(sub_net)$weight + 1)

#Sets the node size proportional to the degree of the node
V(sub_net)$size <- degree(sub_net)*.8

plot(sub_net,vertex.label=V(sub_net)$name,main="Plot of 50 nodes with highest Structual hole access", vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2)

dev.off()



# Degree centrality
degree_sap <- degree(g_SAPSub_simpl)

View(degree_sap)
#Since Degree centrality measures the counts how many neighbors a node has, especially in a directed network,we consider in-degree which is the number of in-coming links.
#So the more the degree centrality value of a particular node, the more is its inteaction with its neighboring nodes. If a person answers a lot of questions
#related to different topics or creates several threads to help people gain more knowlegde by hosting several Q&A sessions, then that node's(person's)
#Degree Centrality value will be very high. In our case, the following 5 nodes have highest degree centrality: 592540, 821176, 131143, 3583224, 22328	

#############################################
#Creates an induced subgraph that includes top 50 nodes with highest degree centrality
sub_net<-induced.subgraph(g_SAPSub_simpl, v=c('592540',
                                              '821176',
                                              '131143',
                                              '3583224',
                                              '22328',
                                              '3519026',
                                              '5420',
                                              '44034',
                                              '2711777',
                                              '3552437',
                                              '2704623',
                                              '3461421',
                                              '701187',
                                              '4183082',
                                              '695093',
                                              '3510478',
                                              '900',
                                              '1384451',
                                              '3621372',
                                              '1195854',
                                              '1666938',
                                              '3609769',
                                              '3713327',
                                              '3496408',
                                              '337106',
                                              '1979898',
                                              '653026',
                                              '3588893',
                                              '4166841',
                                              '393756',
                                              '2470693',
                                              '983891',
                                              '953610',
                                              '1675631',
                                              '303331',
                                              '3988816',
                                              '307549',
                                              '2271172',
                                              '3701705',
                                              '623434',
                                              '3075411',
                                              '1806379',
                                              '479914',
                                              '2380592',
                                              '3704501',
                                              '4229010',
                                              '3949974',
                                              '3465069',
                                              '1043',
                                              '1866982'))

#Makes a distinct color for the nodes '511','541'.'518','519'.
V(sub_net)$color="yellow"
V(sub_net)[c("592540","821176","131143","3583224","22328","3519026","5420","44034","2711777","3552437")]$color="dodgerblue"


#Changes the shape of nodes '511','541'.'518','519' to square while keeping the rest of the nodes in circle.
V(sub_net)$shape<-"circle"
V(sub_net)[c("592540","821176","131143","3583224","22328","3519026","5420","44034","2711777","3552437")]$shape<-"square"

# Makes the width of the edges propotional to the (logarithm of) edge weights
E(sub_net)$width = log(E(sub_net)$weight + 1)

#Sets the node size proportional to the degree of the node
V(sub_net)$size <- degree(sub_net)*.8

#plot(sub_net,layout=layout.fruchterman.reingold,main="Fruchterman Reingold Layout",vertex.label.dist=1)
plot(sub_net,vertex.label=V(sub_net)$name,main="Plot of 50 nodes with highest degree centrality", vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2)

dev.off()


#############################################
# Node betweenness
betweens_SAP <- round(betweenness(g_SAPSub_simpl, v=V(g_SAPSub_simpl), directed = TRUE, nobigint =TRUE, normalized = FALSE))
View(betweens_SAP)
#Betweenness centrality measures the extent to which a vertex lies on paths between other vertices. Vertices with high betweenness may have considerable influence within 
#a network by virtue of their control over information passing between others. They are also the ones whose removal from the network will most disrupt communications between other vertices because they lie on the largest number of paths taken by messages.
#So according to he above definition, we can find that 5 of our nodes :1195854,4183082,592540,44034,2711777 have the highest betweenness centrality
#which means they control majority of the information exchange in the network, also removal of them from the network might disrupt the communications in the whole network.


SAP_1195854<-induced.subgraph(g_SAPSub_simpl, v=c('1195854',
                                                  '1119',
                                              '5420',
                                              '131143',
                                              '162',
                                              '489785',
                                              '4009065',
                                              '3685584',
                                              '2711777',
                                              '3597304',
                                              '4183082',
                                              '3477685',
                                              '583637',
                                              '978762',
                                              '176325',
                                              '289930',
                                              '1308348',
                                              '1731516',
                                              '3638646'))

V(SAP_1195854)$color="yellow"                                              
V(SAP_1195854)["1195854"]$color="dodgerblue"
plot(SAP_1195854,main="Plot of the node having highest betweenness centrality")
plot(SAP_1195854,vertex.label=V(SAP_1195854)$name,main="Plot of 20 nodes with highest betweenness centrality", vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2)


# Edge betwenness
edgebetweens_SAP<-edge.betweenness(g_SAPSub_simpl, e=E(g_SAPSub_simpl), directed = TRUE)
View(edgebetweens_SAP)
#Similarly edge betweenness is a centrality measure using the edges where majority of the information flows through these edges. If we remove 
#these edges from the network then it might disupt the communication in the whol network. Following are the 5 edges having the highest 
# edge betweenness centrality in our SAP network: 2153,10,1272,8,769	

# Local clustering coefficients
clustering_SAP <- transitivity(g_SAPSub_simpl, type="local", vids=V(g_SAPSub_simpl)) 
View(clustering_SAP)
# Transitivity refers to the extent to which the relation that relates two nodes in a network that are connected by an edge is transitive, which implies 
#that each component is a clique. So it measures how strong the cluster is created in those cliques. In our SAP network, any node which has transitivity = 1 
#means that those nodes are deeply embedded in a cluster that they share very similar ideas and it is very rare that someone in this cluster will think different.

# Plots 1 and 2: Can run them together
par(mfrow=c(1, 2))
edge_frame<-data.frame(edgebetweens_SAP, num_weight, inv_weight)
View(edge_frame)
a_edge<-aggregate(edgebetweens_SAP ~ inv_weight, data=edge_frame, mean)
View(a_edge)
numweight_edge<-aggregate(edgebetweens_SAP ~ num_weight, data=edge_frame, mean)
View(numweight_edge)

plot(a_edge, col="blue", log="xy", xlab="Weight of edge (Inverse Weight)", ylab="Average Betweenness of edges")
#From the plot we can observe that lower Average Betweenness of 
#edges velues are observed for lower values of Edge weights, but
#at the same time we can also observe a few Average Betweenness values 
#exists for lower values of Edge weights as well. But the overall plot
#can be generalised as direct relationship with the increasing edge weights, the average 
#betweenness value also increases linearly with a positive slope.

plot(numweight_edge, col="blue", log="xy", xlab="Weight of edge (Regular Weight)", ylab="Average Betweenness of edges")
#From the plot we can observe that higher Average Betweenness of 
#edges velues are observed for lower values of Edge weights, but
#at the same time we can also observe a few Average Betweenness values 
#exists for higher values of Edge weights as well. But the overall plot
#can be generalised as inverse relationship with the increasing edge weights, the average 
#betweenness value decreases linearly with a negative slope.

node_frame<-data.frame(betweens_SAP, constraints_SAP, clustering_SAP, degree_sap)
View(node_frame)
a_node<-aggregate(betweens_SAP ~ clustering_SAP, data=node_frame, mean)
View(a_node)
plot(a_node, col="blue", log="xy", xlab="Clustering", ylab="Average Betweenness of nodes")
#Using the above definitons and combining them together, it can be understood that nodes which have less clustering values 
#have the highest betweenness because those people who are not tightly connected to any cluster serve as the structural hole 
#and act as the main node for information flow through them. Moreover the plot also proves this inverse relation
#between the average betweenness and the clustering coefficient values that nodes which have less clustering coefficients have higher 
#average betweenness and the relationship is linear with a negative slope.

dev.off()

# Plot set 2: Four plots 
par(mfrow=c(2, 2))
a_node<-aggregate(betweens_SAP ~ degree_sap, data=node_frame, mean)
View(a_node)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Betweenness")
#Betweenness Centrality and Degree centrality measure almost the same thing - the importance of the node to its 
#neighbours, except degree measures th number of incoming links to itself whereas betweenness measures how important is the 
#node for information exchange in the network. A node may have very high betweenness, but that does not necessarily mean that 
#it should have a high degree and vice versa, but in genera; if we aggregate the values for mean, we can find 
#from the plot that there is a direct relationship with the linear line with positive slope.
#The top 5 betweenness values and their degrees are given as:
#Deg  Betweenness
#34	  29145.0
#454	24991.0
#49	  11846.0
#29	  11167.3
#40	  9143.0

a_edge<-aggregate(edgebetweens_SAP ~ num_weight, data=edge_frame, mean)
plot(a_edge, col="blue", log="xy", xlab="Weight of edge", ylab="Average Betweenness of edges")
#this relationship has been already explained above #172

a_node<-aggregate(clustering_SAP ~ degree_sap, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Clustering")
#Clustering coefficient is a measure to denote how tightly knotted the nodes are in their cluster 
#whereas degree denotes how many neighbors both internal and external nodes of the cluster the node is in are connected to it.
#So any cluster of people sharing similar ideas in the same domain wilh have high clustering coefficient, whereas less degree.
#This inference is evident from the plot that with increasing degree centrality, the average clustering coefficient value 
#decreases showing an inverse relationship with a linear line with a negative slope.
#The more knowledgable people are usually located outside those clusters and connected to nodes from different clusters.

a_node<-aggregate(constraints_SAP ~ degree_sap, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Constraint (Embeddedness)")
#Embeddedness denotes absense of new ideologies and high overlap of thoughts and ideas of nodes in a cluster
#So a node having high degree will definitely have less embeddedness. This is evident in the plot that with 
#increasing degree centrality, the Embeddedness coefficient of the nodes decreases showing a negative relationship
#with the regression line having a negative slope.

# Log-log degree distribution
par(mfrow=c(1, 2))
d.net <-degree(g_SAPSub_simpl)
View(d.net)
#Total degrees of the SAP network
dd.net <- degree.distribution(g_SAPSub_simpl)
View(dd.net)
#Total degree distribution of the SAP network
d <- 1:max(d.net)-1
View(d)
ind <- (dd.net != 0)
#Classifying a binary index with values >0 as True and values <0 as False
View(ind)
d[ind]
# picking the True Index Degree values
dd.net[ind]
#picking the True Index Degree Distribution values
ds_df<- data.frame(d[ind],dd.net[ind])
#The dataframe after sorting shows that for low values of dd, there are higher values of d and vice versa

plot(d[ind], dd.net[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")
#Hence there is an inverse relation between the degree & its distribution with a regression line with a negative slope.

# CHUNK 8# Average neighbor degree versus vertex degree
a.nn.deg <- graph.knn(g_SAPSub_simpl,V(g_SAPSub_simpl))$knn
View(a.nn.deg)
plot(d.net, a.nn.deg, log="xy", 
     col="goldenrod", xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"))
#From the plot we can observe that there are huge values of Log vertex degree which are 
#within 1 to 15 and the low average neighbor values become very less for values of Log Vertex more than 20
#which again shows us an inverse relationship.

#Running Community Detection Algorithms
# Community detection using the Walktrap Algorithm
comm_walk <- walktrap.community(g_SAPSub_simpl,weights=E(g_SAPSub_simpl)$weight)
plot(comm_walk, g_SAPSub_simpl, vertex.label= NA, vertex.size=6,main="Walktrap Algorithm")

#Strength of the nodes
dfstrength <- as.data.frame(sort(strength(g_SAPSub_simpl))) 
#the top 5 strongest nodes are:
#Node     Strength
#592540	  906
#131143	  230
#821176	  152
#3583224	146
#22328	  108
#Strength is a weighted measure of degree that takes into account the number of edges that go from one 
#node to another. In this network, it will be the total number of interactions of each character with anybody else.
#In SAP network this means that these top 5 contributors have been answering to the questions(edges) threads created by others

SAP_strength <- induced.subgraph(g_SAPSub_simpl, v= c('592540',
                                                      '131143',
                                                      '821176',
                                                      '3583224',
                                                      '22328',
                                                      '5420',
                                                      '983891',
                                                      '3621372',
                                                      '653026',
                                                      '3519026',
                                                      '1675631',
                                                      '2704623',
                                                      '44034',
                                                      '1195854',
                                                      '3510478',
                                                      '701187',
                                                      '2711777',
                                                      '3552437',
                                                      '900',
                                                      '695093',
                                                      '3713327',
                                                      '3461421',
                                                      '4183082',
                                                      '1119',
                                                      '3609769',
                                                      '2470693',
                                                      '19',
                                                      '1384451',
                                                      '1666938',
                                                      '4166841',
                                                      '2380592',
                                                      '337106',
                                                      '1826796',
                                                      '3588893',
                                                      '3694418',
                                                      '3496408',
                                                      '953610',
                                                      '393756',
                                                      '2271172',
                                                      '3694236',
                                                      '3988816',
                                                      '4066510',
                                                      '1979898',
                                                      '2353102',
                                                      '1497185',
                                                      '3465069',
                                                      '3810543',
                                                      '3907737',
                                                      '3704501',
                                                      '4229010'))

V(SAP_strength)$color="yellow"                                              
V(SAP_strength)[c("592540","131143","821176","3583224","22328")]$color="dodgerblue"
plot(SAP_strength,main="Plot of the top 50 Nodes having highest strength",vertex.label.dist=1)
plot(SAP_strength,vertex.label=V(SAP_strength)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2)

dev.off()


#Finding Authority Score
dfauthscore <- as.data.frame(sort(authority_score(g_SAPSub_simpl)$vector))

SAP_auth<- induced.subgraph(g_SAPSub_simpl, v= c('983891',
                                                      '2704623',
                                                      '653026',
                                                      '3694418',
                                                      '3510478',
                                                      '1826796',
                                                      '3489965',
                                                      '3471596',
                                                      '3428914',
                                                      '3838763',
                                                      '3555747',
                                                      '3890539',
                                                      '1497185',
                                                      '22328',
                                                      '3850268',
                                                      '953610',
                                                      '3777840',
                                                      '3707268',
                                                      '3457401',
                                                      '3633216',
                                                      '2271172',
                                                      '3552904',
                                                      '3601495',
                                                      '3716687',
                                                      '351895',
                                                      '3907589',
                                                      '2250999',
                                                      '3519558',
                                                      '1030495',
                                                      '2210029',
                                                      '3588298',
                                                      '3559299',
                                                      '3463621',
                                                      '3518407',
                                                      '3581779',
                                                      '1274560',
                                                      '3750135',
                                                      '3482290',
                                                      '3979360',
                                                      '1542244',
                                                      '3882293',
                                                      '1875521',
                                                      '3704115',
                                                      '3526883',
                                                      '623434',
                                                      '1671460',
                                                      '3887887',
                                                      '3794917',
                                                      '3528148',
                                                      '2051878'))
V(SAP_auth)$color="yellow"                                              
V(SAP_auth)[c("2704623",
              "653026",
              "3694418",
              "3510478",
              "1826796")]$color="dodgerblue"
plot(SAP_auth,main="Plot of the top 50 Nodes having highest Authority Score",vertex.label.dist=1)
plot(SAP_auth,vertex.label=V(SAP_auth)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2)


dev.off()
#Authority score is another measure of centrality initially applied to the Web. 
#A node has high authority when it is linked by many other nodes that are linking many 
#other nodes.So in SAP network high authority nodes in blue denote that any new and 
#innovative ideas originate from someone, these high authority nodes make sure that the reach
#to other people in the network

#Using ego function to visualise highly important nodes in the network
ego.list <- make_ego_graph(g_SAPSub_simpl, order=1, nodes=V(g_SAPSub_simpl)['592540'])

desired_subset <- NULL
for (i in seq_along(ego.list)){
  x <- ego.list[[i]]
  desired_subset <- graph.union(desired_subset, x)
}
V(desired_subset)$color<-"yellow"
V(desired_subset)['592540']$color<-"blue"
plot(desired_subset,vertex.label=NA, main="Ego Network of Node-592540")

plot(desired_subset,vertex.label=V(desired_subset)$name, vertex.label.dist=1.5,vertex.size=8,
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.2)











