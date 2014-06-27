# ################################################ #
# USER-Recommendation Engine Development, profile- #
# based search                                     #
#                                                  #
# Author: Yi Zhang                                 #
# Date: Jun/22/2014                                #
# ################################################ #
# ---- toolbox ------ #
library(rattle)       # toolkit box
library(reshape2)     # Data Preparating
library(plyr)         # Data Wrangling
# ----- visual ------ #
library(ggplot2)      # Data Visualization
library(rCharts)      # Javascript dataVisual
library(gridExtra)    # Multiple sub-plots
# ----- modeling ---- #
library(randomForest) # Utitlize na.roughFix() to impute missing data
library(animation)    # Demonstrate kmeans
library(fpc)          # Tuning Clustering with kmeansuns() and clusterboot()
library(wskm)         # Entropy Weighted subspace clustering
library(amap)         # hclusterpar
# ---- database ----- #
library(rjson)        # Access to JSON objects
library(rmongodb)     # Communicate with MongoDB

.lSys
# img <- ggplot(data=db, aes(id, location, genre1, genre2, genre3, genre4, genre5, instruments1, instruments2, instruments3)) + 
#       geom_tile(colour = "white")                                                                                          + 
#       scale_fill_gradient(low = "white", high = "steelblue")       

# ##################################### #
# CLUSTERING ANALYSIS: K-MEANS CLUSTERS ----
# ##################################### #
model         <- list()
model$km_10   <- kmeans(x=db[, -1], centers=10) # kmean clustering, after removing id variable
model$km_6    <- kmeans(x=db[, -1], centers=6)  # kmean clustering, after removing id variable
model$km_6_cb <- clusterboot(data = db[, -1], 
                             clustermethod=kmeansCBI, 
                             runs   = 100, 
                             krange = 6, 
                             seed   = 42)
temp$nk        <- 1:50
model$km_sel_c    <- kmeansruns(scale(db[, -1]), krange=temp$nk, criterion="ch")
model$km_sel_a    <- kmeansruns(scale(db[, -1]), krange=temp$nk, criterion="asw")
model$km_sel_c_gi <- kmeansruns(scale(db[, -c(1:3)]), krange=temp$nk, criterion="ch")
model$km_sel_a_gi <- kmeansruns(scale(db[, -c(1:3)]), krange=temp$nk, criterion="asw")

# ############################# #
# EVALUATION THE MODEL ---------
# ############################# #
# RadialPlot: Cluster Center (High-dimensional)
temp$dscm         <- melt(model$km_10$centers)
names(temp$dscm)  <- c("Cluster", "Variable", "Value")
temp$dscm$Cluster <- factor(temp$dscm$Cluster)
temp$dscm$Order   <- as.vector(sapply(1:length(unique(temp$dscm$Variable)), rep, 10))
img$rp_kmc        <- ggplot(temp$dscm, aes(x = reorder(Variable, Order), 
                                           y = Value, group = Cluster, colour = Cluster))
img$rp_kmc   <- img$rp_kmc + coord_polar()
img$rp_kmc   <- img$rp_kmc + geom_point() + geom_path()
img$rp_kmc   <- img$rp_kmc + labs(x=NULL, y=NULL)
img$rp_kmc   <- img$rp_kmc + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank())

# Special Toolkit
source("http://onepager.togaware.com/CreateRadialPlot.R")
temp$dsc_c10 <- data.frame(group=factor(1:10), model$km_10$centers)
CreateRadialPlot(temp$dsc_c10, grid.min=-2, grid.max=2, plot.extent.x=1.5)

# ############################ #
# Viualize kmeans(centers = 4) #
# ############################ #
temp$dsc_c6    <- data.frame(group=factor(1:6), model$km_6mode$centers)
img$rp_kmc6_p1 <- CreateRadialPlot(subset(temp$dsc_c6, group == 1), grid.min = -2, grid.max = 2, plot.extent.x = 2)
img$rp_kmc6_p2 <- CreateRadialPlot(subset(temp$dsc_c6, group == 2), grid.min = -2, grid.max = 2, plot.extent.x = 2)
img$rp_kmc6_p3 <- CreateRadialPlot(subset(temp$dsc_c6, group == 3), grid.min = -2, grid.max = 2, plot.extent.x = 2)
img$rp_kmc6_p4 <- CreateRadialPlot(subset(temp$dsc_c6, group == 4), grid.min = -2, grid.max = 2, plot.extent.x = 2)
img$rp_kmc6_p5 <- CreateRadialPlot(subset(temp$dsc_c6, group == 5), grid.min = -2, grid.max = 2, plot.extent.x = 2)
img$rp_kmc6_p6 <- CreateRadialPlot(subset(temp$dsc_c6, group == 6), grid.min = -2, grid.max = 2, plot.extent.x = 2)

grid.arrange(img$rp_kmc6_p1 + ggtitle("Cluster #1"), 
             img$rp_kmc6_p2 + ggtitle("Cluster #2"),
             img$rp_kmc6_p3 + ggtitle("Cluster #3"),
             img$rp_kmc6_p4 + ggtitle("Cluster #4"),
             img$rp_kmc6_p5 + ggtitle("Cluster #5"),
             img$rp_kmc6_p6 + ggtitle("Cluster #6"))

# ####################################### #
# Visualize the selection of k (#centers) #
# ####################################### #
# TYPE1 MODEL: age + location + genre + instrument 
# Optimal K = 21, chosen value of K = 20
temp$dsc  <- data.frame(k=temp$nk, ch=scale(model$km_sel_c$crit), asw=scale(model$km_sel_a$crit))
temp$dscm <- melt(temp$dsc, id.vars="k", variable.name="Measure")
img$k_sel <- ggplot(temp$dscm, aes(x=k, y=value, colur = Measure)) +
             geom_point(aes(shape=Measure))                        +
             geom_line(aes(linetype=Measure))                      +
             scale_x_continuous(breaks=temp$nk, labels = temp$nk)  +
             ggtitle("K Selection for Type 1 Model")
# find the optimal size
temp$diff  <- scale(model$km_sel_c$crit) - scale(model$km_sel_a$crit)
temp$opt_k <- temp$nk[which(abs(temp$diff) == min(abs(temp$diff)))]
cat(paste("*** OPTIMAL K = ", temp$opt_k, "    ****\n"))
# ####################################### #
# TYPE2 MODEL: genre + instrument 
# Optimal k = 
temp$dsc  <- data.frame(k=temp$nk, ch=scale(model$km_sel_c_gi$crit), asw=scale(model$km_sel_a_gi$crit))
temp$dscm <- melt(temp$dsc, id.vars="k", variable.name="Measure")
img$k_sel <- ggplot(temp$dscm, aes(x=k, y=value, colur = Measure)) +
  geom_point(aes(shape=Measure))                        +
  geom_line(aes(linetype=Measure))                      +
  scale_x_continuous(breaks=temp$nk, labels = temp$nk)  +
  ggtitle("K Selection for Type 2 Model") 
# find the optimal size
temp$diff  <- scale(model$km_sel_c$crit) - scale(model$km_sel_a$crit)
temp$opt_k <- temp$nk[which(abs(temp$diff) == min(abs(temp$diff)))]
cat(paste("*** OPTIMAL K = ", temp$opt_k, "    ****\n"))

# ####################################### #
# TRAIN TYPE1 & TYPE 2 MODEL --------------
model       <- list()
model$km_t1 <- kmeans(x=db[, -1],      centers=20) # kmean clustering, after removing id variable
model$km_t2 <- kmeans(x=db[, -c(1:3)], centers=20) # kmean clustering, after removing id variable

# ####################################### #
# RECOMMENDATION ENGINE -------------------
# ####################################### #
# S1: Predict observation cluster
# S2: Find nearest neighbor within cluster
# data(unclustred) --> kmeans -- (+)cluster ---> search neigbor within cluster --> give recommendation
Recomend <- function(new_obs, model, train_data, topN = 5) {
  # ################################# #
  # Return prediction                 #
  # ################################# #
  temp$pred_c <- predict(model, new_obs)
  temp$nb_idx <- which(model$cluster == temp$pred_c)
  temp$dist   <- sapply(temp$nb_idx, 
                        FUN=function(idx){
                          ds  <- rbind(db[idx, ], new_obs)
                          res <- dist(x=ds, method="manhattan")
                          return(res)
                        })
  recom_all <- train_data$id[ temp$nb_idx[order(temp$dist, decreasing=F)] ]
  if( new_obs['id'] %in% train_data$id ) recom_all <- recom_all[-which(recom_all == new_obs$id)]
  res       <- recom_all[1:topN]
  return(res)
}

DisplayUser <- function(user_id, user_db, fields = NA){
  # ###################################### #
  # Return user profile with given user_id #
  # ###################################### #
  if( !(is.na(fields)) ){
    fields  <- fields[fields %in% colnames(user_db)]
    user_db <- user_db[, fields]
  }
  res <- subset(user_db, id = user_id)
  return(res)
}

