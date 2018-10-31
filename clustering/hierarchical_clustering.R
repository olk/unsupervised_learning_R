library(dplyr)
library(ggplot2)
library(magrittr)
library(mlbench)
library(ggdendro)

data(iris)

iris_dist <- iris %>%
    select(-Species) %>%
    scale %>%
    dist

clustering <- hclust(iris_dist)

#ggdendrogram(clustering) + theme_dendro()

clusters <- clustering %>%
    cutree(k=3)

iris %>%
    cbind(Cluster=clusters) %>%
    ggplot() +
        geom_bar(aes(x=Species, fill=as.factor(Cluster)), position="dodge") +
        scale_fill_discrete("Cluster")

pca <- iris %>%
    select(-Species) %>%
    prcomp

mapped_iris <- pca %>%
    predict(iris)

mapped_iris %>%
    as.data.frame %>%
    cbind(Species=iris$Species,
          Clusters=as.factor(clusters)) %>%
    ggplot() +
        geom_point(aes(x=PC1, y=PC2, shape=Species, colour=Clusters))
