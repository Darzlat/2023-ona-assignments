# Create a data frame using the edges
edgesEX2 = data.frame(
  source = c("1", "2", "3", "3", "3", "3", "3", "4", "5", "5", "6", "6", "A", "A", "B", "B", "C"),
  target = c("2", "A", "4", "5", "B", "C", "D", "C", "6", "D", "B", "D", "B", "C", "C", "D", "D")
)

# Create the network object
library(igraph)
networkEX2 = graph_from_data_frame(d=edgesEX2, directed=F) 

# Adjust node size based on degree centrality
degree_centrality <- degree(networkEX2, mode="all")

# Plot the network
plot(networkEX2, vertex.size=degree_centrality*6, vertex.color=my_color)

# Create a color palette based on betweenness centrality
library(RColorBrewer)
coul <- brewer.pal(nlevels(as.factor(c(0, 0.222222222, 0.128703704, 0, 0.014814815, 0.025925926, 0.388888889, 0.250925926, 0.241666667, 0.090740741))), "YlOrRd")

# Map it to the network
my_color <- coul[as.numeric(as.factor(c(0, 0.222222222, 0.128703704, 0, 0.014814815, 0.025925926, 0.388888889, 0.250925926, 0.241666667, 0.090740741)))]

k <- as_data_frame(networkEX2, what = "vertices")

# Add a legend for degree centrality
legend(x=-2.5, y=-0.12, 
       legend=paste( levels(as.factor(c(0, 0.222222222, 0.128703704, 0, 0.014814815, 0.025925926, 0.388888889, 0.250925926, 0.241666667, 0.090740741))), " BC", sep=""), 
       col = coul , 
       bty = "n", pch=20 , pt.cex = 2, cex = 1,
       text.col="black" , horiz = F)


# Idem for closeness centrality
legend(x=1, y=-0.12, 
       legend=paste(c(0.3,
                                        0.409090909,
                                        0.5625,
                                        0.45,
                                        0.428571429,
                                        0.473684211,
                                        0.5625,
                                        0.642857143,
                                        0.642857143,
                                        0.5625
       ), " CC", sep=""), 
        
       bty = "n", pch=20 , pt.cex = 2, cex = 1,
       text.col="black" , horiz = F)

# Idem for degree centrality
legend(x=-2.5, y=1.5, 
       legend=paste(c(0.111111111,
                                        0.222222222,
                                        0.555555556,
                                        0.222222222,
                                        0.333333333,
                                        0.333333333,
                                        0.333333333,
                                        0.555555556,
                                        0.555555556,
                                        0.555555556
       ), " DC", sep=""), 
       
       bty = "n", pch=20 , pt.cex = 2, cex = 1,
       text.col="black" , horiz = F)
