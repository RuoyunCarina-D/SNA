
library(ergm)

# ergm models to find if there is relationship with attributes of users and their choice of media

user.ig.m <- get.adjacency(user.ig,sparse=FALSE)
for (i in 1:nrow(user.ig.m)){
  for (j in 1:ncol(user.ig.m)){
    if (user.ig.m[i,j] > 0) user.ig.m[i,j] = 1
  }
}

user.net <- network(user.ig.m, directed = FALSE)
user.net %v% "marital.status" <- (as.numeric(user.married)-1) #0for unmarried and 1 for married
user.net %v% "user.gender" <- (as.numeric(user.gender)-1) #0 for female and 1 for male
user.net %v% "user.age" <- user.age
m.all<-ergm(user.net ~ edges + 
              nodematch("user.age")+
              nodematch("user.gender")+
              nodematch("marital.status",diff=T))
summary(m.all)
