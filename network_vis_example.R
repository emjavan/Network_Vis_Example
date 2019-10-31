#*####################################
### Network Visualization Example ###
#*####################################

####### Quick igraph example ########

library(igraph)

# Define a graph by the edges connecting nodes
example_graph = graph( edges =  c(1,2, 2,3, 2,4, 3,6, 3,4, 
                                  4,5, 3,7, 7,8, 7,9, 8,9,
                                  8,10, 9,10),
                       n=10, directed = FALSE)

# assign weights to all edges
E(example_graph)$weight = seq(0.1, 1.2, 0.1)

# turn the graph into an adjacency matrix
example_adj_mat=as.matrix(get.adjacency(example_graph))

# quick easy visualization
plot(example_graph)

# gives you an estimate of which nodes are the most well-connected
ec=eigen_centrality(example_graph)
ec[["vector"]]


####### Longer GGally example ########

library(GGally)
# requires ggplot2

# now to make nicer quality graphs use ggnet2
ggnet2(example_graph)
ggnet2(example_adj_mat)

library(sna) # needed for gplot.layout
library(intergraph) # needed for asNetwork

# convert igraph object to network object
example_network=asNetwork(example_graph)
ggnet2(example_network)

# If we like one layout and don't want it to change we'll have to assign the node locations
# gplot.layout has a large list of possible layouts we can use
# gplot.layout doesn't work with the igraph output
layout1 = gplot.layout.fruchtermanreingold(example_network, NULL)
example_network %v% "x" = layout1[, 1]
example_network %v% "y" = layout1[, 2]

# to use V(network) won't work, so you'll have to use the original graph object
ggnet2(example_network, 
       label=V(example_graph),
        mode = c("x", "y")) +
  ggtitle("Awesome Example Network")

ggnet2(example_network, 
       label=V(example_graph),
       mode = c("y", "x")) +
  ggtitle("Equally Awesome Example Network")

layout2=gplot.layout.circle(example_network, NULL)
example_network %v% "u" = layout2[, 1]
example_network %v% "v" = layout2[, 2]

ggnet2(example_network, 
       label=V(example_graph),
       mode = c("u", "v")) +
  ggtitle("Another Awesome Example Network")

# We can change node/edge colors and size to represent some aspect of the data

V(example_graph)$color = c(rep("steelblue", 5), rep("tomato", 5))
ggnet2(example_network, 
       label=V(example_graph),
       mode = c("u", "v"),
       color= V(example_graph)$color) +
  ggtitle("Colorful Example Network")

# set the degree of the edges to deg to make colorful graph
V(example_graph)$deg = lengths(as_adj_list(example_graph))

# We can assign the color to change based on the node degree
for(i in 1:10){
  if(V(example_graph)$deg[i]==1){
    V(example_graph)$color[i]="steelblue4"
  }else if(V(example_graph)$deg[i]==2){
    V(example_graph)$color[i]="steelblue"
  }else if(V(example_graph)$deg[i]==3){
    V(example_graph)$color[i]="tomato3"
  }else if(V(example_graph)$deg[i]==4){
    V(example_graph)$color[i]="tomato4"
  }
}
ggnet2(example_network, 
       label=V(example_graph),
       mode = c("u", "v"),
       color= V(example_graph)$color) +
  ggtitle("Degree Colored Example Network")


# Sometime its nice to print your pictures to a png
type="layout1" # this is just to show how a variable name goes in the title
network_name = paste0("example_graph_", type, ".png", sep='')
png(network_name, width=8, height=6, units='in', res=300)
our_example=ggnet2(example_network,
                   mode = c("x", "y"),
                   color = V(example_graph)$color,
                   edge.size = E(example_graph)$weight,
                   size = 0) +
  ggtitle("Degree Colored Example with Edge Weights") +
  geom_point(aes(color = color), size = 7, color = "white") +
  geom_point(aes(color = color), size = 7, alpha = 0.75) +
  geom_point(aes(color = color), size = 4, alpha=1) +
  guides(color = FALSE) + 
  theme(panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA))
print(our_example)
dev.off()


type="layout2" # this is just to show how a variable name goes in the title
network_name = paste0("example_graph_", type, ".png", sep='')
png(network_name, width=8, height=6, units='in', res=300)
our_example=ggnet2(example_network,
                   mode = c("u", "v"),
                   color = V(example_graph)$color,
                   edge.size = E(example_graph)$weight,
                   size = 0) +
  ggtitle("Degree Colored Example with Edge Weights") +
  geom_point(aes(color = color), size = 7, color = "white") +
  geom_point(aes(color = color), size = 7, alpha = 0.75) +
  geom_point(aes(color = color), size = 4, alpha=1) +
  guides(color = FALSE) + 
  theme(panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA))
print(our_example)
dev.off()

# To summarize: We can represent the same information in multiple ways
#   1) Graph 
#   2) Adjacency Matrix 
#   3) Network
# Different packages will want different inputs, so use variable names to your advantage
# naming things lazily/poorly will make it hard to debug later




