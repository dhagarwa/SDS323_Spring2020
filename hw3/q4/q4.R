setwd("/Users/dhwanit/Google Drive/UT Austin Courses/SDS323_Spring2020/hw3/q4")
library(hrbrthemes)
library(kableExtra)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(LICORS)  # for kmeans++
library(ggcorrplot)
library(reshape2)
library(ggplot2)
library(streamgraph)
library(viridis)
library(DT)
library(plotly)


sm_data <- read.csv("social_marketing.csv")
summary(sm_data)
head(sm_data, 10)

sm_feat_raw <- sm_data[,2:length(sm_data)]



# Creating a correlation plot 

cormat <- round(cor(sm_feat_raw), 2)
head(cormat[, 1:6])
ggcorrplot(cormat, hc.order = TRUE, type = "lower", outline.color = "white") + theme( axis.text.x = element_text(angle=90, hjust=1))

##Some very strong +ve correlations can be seen, like between  health nutrition and personal fitness, cooking and fashion, online_gaming and college_uni.
##A rather peculiar one is between sports_fandom and religion (Probably if you are a fan of God, you are likely to be a fan of sports team :) ). 


sm_user <- sm_data[, 1]
#Scaling of the data to get frequency of a particular of type of tweet.
sm_feat = sm_feat_raw/(rowSums(sm_feat_raw)) 

#Clustering on original scaled centered data
sm_sca = scale(sm_feat_raw, center=TRUE, scale=TRUE)
#sm_sca = sm_feat #not doing scaling as each value represents a proportion of tweet in each category by a user




set.seed(100)

# function to compute total within-cluster sum of square
wss <- function(k) {
  kmeans(sm_sca, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



fviz_nbclust(sm_sca, kmeans, method = "silhouette")

# Run k-means with 8 clusters and 25 starts
clust1 = kmeanspp(sm_sca, 8, nstart=25)
 
fviz_cluster(clust1, data = sm_sca,  stand = FALSE, 
            ellipse.type = "t", geom=c("point"), main="Kmeans++ clusters visualization on first two principal components") + theme_bw()

mu = attr(sm_sca,"scaled:center")
sigma = attr(sm_sca,"scaled:scale")




rs      <- as.data.frame(t(clust1$center))
rs$category <- rownames(rs)
rs      <- melt(rs, id.vars=c("category"), variable.name = "cluster")


#ggplot(rs, aes(x=category, y=value, group=cluster, color=cluster)) + geom_line() + theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(rs, aes(x = category, y = value)) + geom_line(aes(color = cluster, group=cluster)) + 
  facet_grid(cluster ~ ., scales = "free_y") + theme(legend.position = "none", axis.text.x = element_text(angle=90, hjust=1))



tmp <- rs %>%
  mutate(cluster2=cluster)

tmp %>%
  ggplot( aes(x=category, y=value)) +
  geom_line( data=tmp %>% dplyr::select(-cluster), aes(group=cluster2), color="grey", size=0.5, alpha=0.5) +
  geom_line( aes(color=cluster, group=cluster), color="#69b3a2", size=1.2 )+
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  ggtitle("A spaghetti chart of category prominence in each cluster") +
  facet_wrap(~cluster) + theme(axis.text.x = element_text(angle=90, hjust=1))


# clust1$center[1,]
# clust1$center[2,]*sigma + mu
# clust1$center[4,]*sigma + mu


fviz_cluster(clust1, data = sm_sca, choose.vars = c("cooking", "online_gaming"), stand = FALSE,
             ellipse.type = "norm", geom=c("point")) + theme_classic()

# fviz_cluster(clust1, data = sm_sca, choose.vars = c("cooking", "business"), stand = FALSE, 
#              ellipse.type = "norm", geom=c("point")) + theme_classic()
# 
# fviz_cluster(clust1, data = sm_sca, choose.vars = c("cooking", "fashion"), stand = FALSE, 
#              ellipse.type = "norm", geom=c("point")) + theme_classic()
# 
# fviz_cluster(clust1, data = sm_sca, choose.vars = c("cooking", "dating"), stand = FALSE, 
#              ellipse.type = "norm", geom=c("point")) + theme_classic()
# 
# fviz_cluster(clust1, data = sm_sca, choose.vars = c("spam", "adult"), stand = FALSE,
#              ellipse.type = "norm", geom=c("point")) + theme_classic()
#
# fviz_cluster(clust1, data = sm_sca, choose.vars = c("cooking", "computers"), stand = FALSE, 
#              ellipse.type = "norm", geom=c("point")) + theme_classic()
# 
# fviz_cluster(clust1, data = sm_sca, choose.vars = c("online_gaming", "college_uni"), stand = FALSE, 
#              ellipse.type = "norm", geom=c("point")) + theme_classic()
# 
# fviz_cluster(clust1, data = sm_sca, choose.vars = c("religion", "family"), stand = FALSE, 
#              ellipse.type = "norm", geom=c("point")) + theme_classic()
# 
# fviz_cluster(clust1, data = sm_sca, choose.vars = c("religion", "politics"), stand = FALSE, 
#              ellipse.type = "norm", geom=c("point")) + theme_classic()
# 
# fviz_cluster(clust1, data = sm_sca, choose.vars = c("religion", "business"), stand = FALSE, 
#              ellipse.type = "norm", geom=c("point")) + theme_classic()
# 
# fviz_cluster(clust1, data = sm_sca, choose.vars = c("religion", "outdoors"), stand = FALSE, 
#              ellipse.type = "norm", geom=c("point")) + theme_classic()


hclust1 <- hcut(sm_sca, 6, hc_method ="ward" , hc_metric="euclidian" )

fviz_cluster(hclust1, data = sm_sca, stand = FALSE, 
                         ellipse.type = "norm", geom=c("point")) + theme_classic()



# set.seed(123)
# gap_stat <- clusGap(sm_sca, FUN = kmeans, nstart = 10,
#                     K.max = 18, B = 10)
# # Print the result
# print(gap_stat, method = "firstmax")
# 
# fviz_gap_stat(gap_stat)


###Applying PCA to data to find if PCA gives any interpretable combination of fetaures that can help reduce dimensionality and make market segmentation more interpretable

# apply PCA
sm_pca <- prcomp(sm_feat_raw,
                 center = TRUE,
                 scale = TRUE) 

#summary(sm_pca)
print(sm_pca)
# Screeplot
pr_var <-  sm_pca$sdev ^ 2
pve <- pr_var / sum(pr_var)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')

# Cumulative PVE plot
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim =c(0,1), type = 'b')


##PCA results: Total 36 PCs. 10 components explain about 60% of variance, 18 components about 80% of variance, 25 components explain 90% of variance

# Rotate loadings
rot_loading <- varimax(sm_pca$rotation[, 1:15])
rot_loading

##Varimax transformations helps simplify the interpretation of PC. For ex: PC1 is negatively associated with  religion, parenting, sports_fandom, family, school. 
##PC2 is negatively associated with cooking, beauty and fashion. PC3 is negatively associated with politics, news while positively with automotive.
##PC 4 is negatively associated with health_nutrition, outdoors, personal_fitness. PC5 is more about online_gaming, college_uni, sports_playing. PC7 is about tv_film, crafts and art.  Some other intepretable
## Another notable PC is PC9 which negatively associated with spam and adult content.
## So any entry scoring highly negatively on this would be be a good candidate for spam and bot. 






