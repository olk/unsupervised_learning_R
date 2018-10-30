library(dplyr)
library(ggplot2)
library(magrittr)
library(mlbench)

data(iris)
data(HouseVotes84)

# PCA maps data from one vector space to another of the same dimensionality as the first
# number of dimension is NOT reduced
# coordinate system: first coordinate contains the most information, second coordinate
# contains the second most inforamtion ...

pca <- iris %>% select(-Species) %>% prcomp
pca

pca %>% plot

# map data to new space spanned by the PCA using predict()
# can be done with data that wasn't used to create the PCA
mapped_iris <- pca %>% predict(iris)
mapped_iris %>% head

mapped_iris %>% 
    as.data.frame %>%
    cbind(Species=iris$Species) %>%
    ggplot() + geom_point(aes(x=PC1, y=PC2, colour=Species))


HouseVotes84 %>% head

vote_pattern <- HouseVotes84 %>%
    select(-Class) %>%
    apply(c(1, 2), . %>% { iflese("n"==as.character(.), 0, 1) }) %>% # data must be numeric, not a factor
    apply(c(1, 2), . %>% { iflese(is.na(.), 0.5, .) }) %>% # missing data
    prcomp

pca <- vote_pattern %>% prcomp

mapped_votes <- pca %>% predict(vote_pattern)
mapped_votes %>%
    as.data.frame %>%
    cbind(Class=HouseVotes84$Class) %>%
    ggplot() + geom_point(aes(x=PC1, y=PC2, colour=Class))
