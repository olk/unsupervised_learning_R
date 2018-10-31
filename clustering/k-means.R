library(dplyr)
library(ggplot2)
library(magrittr)
library(mlbench)

data(iris)

clusters <- iris %>%
    select(-Species) %>%
    kmeans(centers=3)

# $centers: vectors with the center of mass for each of the three computed clusters
clusters$centers

# $cluster: integer vector with a number for each data point
#          specifying to which cluster that data point is assigned to
clusters$cluster

pca <- iris %>%
    select(-Species) %>%
    prcomp

mapped_iris <- pca %>%
    predict(iris)

mapped_centers <- pca %>%
    predict(clusters$centers)

# plot the mapped data point, PC1 against PC2
# cluter informations must be added
mapped_iris %>%
    as.data.frame %>%
    cbind(Species=iris$Species,
          Clusters=as.factor(clusters$cluster)) %>%
    ggplot() +
        geom_point(aes(x=PC1, y=PC2, colour=Species, shape=Clusters)) +
        geom_point(aes(x=PC1, y=PC2), size=5, shape="X", data=as.data.frame(mapped_centers))
       

# confusion matrix
tbl <- table(iris$Species, clusters$Cluster)

