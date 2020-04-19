setwd("/Users/dhwanit/Google Drive/UT Austin Courses/SDS323_Spring2020/hw3/q4")
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

sm_data <- read.csv("social_marketing.csv")
summary(sm_data)
head(sm_data, 10)

sm_feat_raw <- sm_data[, 2:37]
sm_user <- sm_data[, 1]
sm_feat = sm_feat_raw/(rowSums(sm_feat_raw))

#Clustering on original scaled centered data
sm_sca = scale(sm_feat, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(sm_sca,"scaled:center")
sigma = attr(sm_sca,"scaled:scale")

# Run k-means with 6 clusters and 25 starts
clust1 = kmeans(sm_sca, 4, nstart=25)

clust1

# What are the clusters?
clust1$center  # not super helpful
clust1$center[1,]*sigma + mu
clust1$center[2,]*sigma + mu
clust1$center[4,]*sigma + mu

fviz_cluster(clust1, data = sm_sca)


set.seed(123)

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

set.seed(123)
gap_stat <- clusGap(sm_sca, FUN = kmeans, nstart = 10,
                    K.max = 18, B = 10)
# Print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)


###Applying PCA to data to find if PCA gives any interpretable combination of fetaures that can help reduce dimensionality and make market segmentation more interpretable

# apply PCA
sm_pca <- prcomp(sm_feat,
                 center = TRUE,
                 scale. = TRUE) 

summary(sm_pca)
print(sm_pca)
# Screeplot
pr_var <-  sm_pca$sdev ^ 2
pve <- pr_var / sum(pr_var)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')

# Cumulative PVE plot
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim =c(0,1), type = 'b')


##PCA results: Total 36 PCs. 10 components explain about 50% of variance, 19 components about 75% of variance, 22 components explain 81% of variance

# Rotate loadings
rot_loading <- varimax(sm_pca$rotation[, 1:15])
rot_loading

##Varimax transformations helps simplify the interpretation of PC. For ex: PC1 is positively associated with  religion, parenting, sports_fandom, family and negatively associated 
##with travel.
##PC2 is negatively associated with health_nutrition, outdoors, personal_fitness. PC3 is negatively associated with shopping while positively associated with politics, 
##news and automotive.
##PC 4 is negatively associated with college_uni, online_gaming and sports_playing while +vely with news and politics. PC5 is more about fashion, cooking and beauty. Some other intepretable
##PCs: PC9 is about shopping, music, family, travel.PC11 is dominated by business. PC14 is about eco. Another notable PC is PC8 which negatively associated with spam and adult content.
## So any entry scoring highly negatively on this would be be a good candidate for spam and bot. 

# A biplot I like a bit better
loadings = sm_pca$rotation
scores = sm_pca$x
ggplot2::qplot(scores[,1], scores[,2], xlab='Component 1', ylab='Component 2')





# Predict PCs
predict(ir.pca, 
        newdata=tail(log.ir, 2))



# library(devtools)
# install_github("ggbiplot", "vqv")
# 
# library(ggbiplot)
# g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
#               groups = ir.species, ellipse = TRUE, 
#               circle = TRUE)
# g <- g + scale_color_discrete(name = '')
# g <- g + theme(legend.direction = 'horizontal', 
#                legend.position = 'top')
# print(g)

require(caret)
trans = preProcess(iris[,1:4], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC = predict(trans, iris[,1:4])
summary(PC)
# Retained PCs
head(PC, 3)

