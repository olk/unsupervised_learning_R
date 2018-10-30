library(dplyr)
library(ggplot2)
library(magrittr)
library(mlbench)
library(stringdist)

data(iris)
data(HouseVotes84)

# measure of distance between objects
# represented a distance matrix; contains all pair-wise distances
# multi-dim. scaling maps the matrix of all pair-wise distances
# inti a linear space while preserving the distances

iris_dist <- iris %>% select(-Species) %>% dist

# cmdscale() creates a 2-dim representation

mds_iris <- iris_dist %>% cmdscale(k=2)
mds_iris %>% head

mds_iris %>% 
    as.data.frame %>%
    cbind(Species=iris$Species) %>%
    ggplot() + geom_point(aes(x=V1, y=V2, colour=Species))


vote_pattern <- HouseVotes84 %>%
    select(-Class) %>%
    apply(c(1, 2), . %>% { ifelse("n"==as.character(.), 0, 1) }) %>% # data must be numeric, not a factor
    apply(c(1, 2), . %>% { ifelse(is.na(.), 0.5, .) }) # missing data

mds_votes <- vote_pattern %>% dist %>% cmdscale(k=2)
mds_votes %>%
    as.data.frame %>%
    cbind(Class=HouseVotes84$Class) %>%
    ggplot() + geom_point(aes(x=V1, y=V2, colour=Class))



random_ngram <- function(n) {
    sample(c('A', 'C', 'G', 'T'), size=n, replace=TRUE) %>% paste0(collapse="")
}

random_string <- function(m) {
    n <- max(1, m + sample(c(-1, 1), size=1) * rgeom(1, 1/2))
    random_ngram(n)
}

strings <- replicate(10, random_string(5))
string_dist <- stringdistmatrix(strings)
string_dist %>%
    cmdscale(k=2) %>%
    as.data.frame %>%
    cbind(String=strings) %>%
    ggplot(aes(x=V1, y=V2))+
    geom_point() +
    geom_label(aes(label=String), hjust=0, nudge_y=-0.1)
