
knitr::opts_chunk$set(echo = TRUE)
install.packages("igraph")
install.packages("tnet")
library(igraph)
library(tnet)
library(ergm)


## Projecting two mode network into one mode network(primary network is users)

wd <- "~/Documents"
setwd(wd)

#read the two-modes files
edges.file <- "Media.users.final.csv"
edges.dir <- paste(wd, edges.file, sep="/")
edges <- read.csv(file=edges.dir, header=T, row.names=1, stringsAsFactors =F)

net<- graph_from_incidence_matrix(edges, directed = F, mode = "out") 
net.eg <- as_data_frame(net, what="edges")
write.csv(net.eg,"net_media.csv", row.names = F)

#tnet can only work on network whose node is numerical labelled,
#so I manually changed all nodes names into numbers (in Excel)
net.eg <- read.csv("net_media.csv")

user.df <- projecting_tm(net.eg[,c(1,2)], method="sum")
write.csv(user.df, "user_shared_media.csv", row.names = F)

user.ig <- graph_from_data_frame(user.df[,c(1,2)],directed = FALSE)
E(user.ig)$weight <- user.df[,3]

cut.off <- mean(user.df[,3]) 
user.ig.2 <- delete_edges(user.ig, E(user.ig)[weight < cut.off])

attr.file <- "only.users.attributes.csv"
attr.dir <- paste(wd, edges.file, sep="/")
attr <- read.csv(file=attr.file, header=T, row.names=1, stringsAsFactors =T)
user.name<-as.character(attr$name)
user.age<-attr$user.age
user.gender<-attr$user.gender
user.married<-attr$user.married


user.ig.2.m <- get.adjacency(user.ig.2,sparse=FALSE)
for (i in 1:nrow(user.ig.2.m)){
  for (j in 1:ncol(user.ig.2.m)){
    if (user.ig.2.m[i,j] > 0) user.ig.2.m[i,j] = 1
  }
}

user.ig.m <- graph_from_adjacency_matrix(user.ig.2.m,mode = "undirected")
V(user.ig.m)$name <- user.name
V(user.ig.m)$marital.status <- (as.numeric(user.married)) 
#1for unmarried and 2 for married
V(user.ig.m)$user.gender <- (as.character(user.gender))
V(user.ig.m)$user.age <- user.age
deg <- degree(user.ig.m)

l <- layout_in_circle(user.ig.m)
plot(user.ig.m,
     vertex.color=c(rgb(0,0.8,0.9,0.6),rgb(1, 0.6, 0.6, 0.75, 0.6))[as.numeric(user.married)], 
     vertex.color = c("lightgoldenrod1","light blue")[as.numeric(user.married)],
     vertex.label = V(user.ig.m)$user.married,
     vertex.label.cex=0.5,
     vertex.size=(deg+1))

text(1,1.1, "1 unmarried", cex=.6)
text(1,1, "2 married", cex=.6)


plot(user.net.2, vertex.label = marital.status )

