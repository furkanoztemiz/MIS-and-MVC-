library(igraph)


g <- sample_bipartite(n1 = 10,n2 = 6,p = 1)  


g <- graph(edges <- c(1 ,6, 1 ,8, 1 ,11, 1 ,15, 2 ,7, 2 ,8, 2 ,9, 2 ,11, 2 
                      ,12, 2 ,13, 2 ,15, 2 ,16, 2 ,19, 3 ,10, 3 ,13, 3 ,15, 
                      3 ,16, 3 ,17, 3 ,18, 4 ,12, 4 ,15, 4 ,16, 4 ,19, 5 ,
                      10, 6 ,7, 6 ,19, 7 ,9, 7 ,10, 7 ,11, 7 ,13, 7 ,14, 7 ,
                      16, 8 ,10, 8 ,13, 8 ,14, 8 ,17, 8 ,18, 8 ,19, 9 ,11, 9 
                      ,14, 11 ,12, 11 ,14, 11 ,15, 12 ,13, 14 ,15, 14 ,18, 14 
                      ,19, 15 ,16, 15 ,19, 16 ,17),directed = FALSE )

plot(g)

vector = c()
MalatyaCentrality <- function(g){
  vertexList <- c(V(g))
  for (i in vertexList) { 
    Vdegree <-degree(g,v = V(g)[i])
    KomsuDegree <- degree(g,v = neighbors(g,v = V(g)[i]))
    Value <- Vdegree/KomsuDegree
    vector <- c(vector, sum(Value))
  }
  return(vector)
}
######## Find Minimum Vertex
FindMinimum <- function(graph){
  data <- data.frame(MalatyaCentrality(graph))
  return (order(data$MalatyaCentrality.graph.,decreasing = FALSE)[1])
}

################### Maksimum Independent Set Vertex ###########

IndependentSet = c()
while(vcount(g) > 0 ){
  silinecekNodes =c()
  komsu <- NULL 
  IndependentSet <- append(IndependentSet,V(g)[FindMinimum(g)])
  minNode <- V(g)[FindMinimum(g)]
  komsu <- neighbors(g,v = minNode)
  silinecekNodes <- append(silinecekNodes,minNode)
  silinecekNodes <- append(silinecekNodes,komsu)
  g <- delete_vertices(g,silinecekNodes )
}
length(IndependentSet)


plot(g, vertex.size = 8, vertex.label.cex = 1.2,edge.width = 2.4,layout= layout.bipartite(g))
