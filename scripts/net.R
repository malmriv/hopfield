#Load library, declare number of nodes and color palette
library(igraph)
nodes = 12
colrs = colorRampPalette(c(rgb(1,0.2,0.2),"white")) 

#Create graph
graph = make_full_graph(nodes)
plot(graph, vertex.size=15, vertex.label=NA, edge.curved = 0.02,
     edge.width=seq(0.1,1.4,len=nodes*(nodes-1)/2),vertex.color=colrs(12))

