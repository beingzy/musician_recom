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

# ################## #
# SET UP Environment #
# ################## #
repo        <- list()
repo$data   <- "./data"
repo$output <- "./output"

sim        <- list()
sim$loc    <- c("new york city", "san francisico", "chengdu", "beijing", "shanghai", "taipei")
sim$genres <- c("pop", "blues", "avant-garde", "red dirt", "zydeco", "classic country", "progressive folk", "american folk revival",
                "sung poetry", "indie folk", "techno-folk", "hip pop", "grime", "jazz")
sim$instru <- c("clapsticks", "drum kit", "hang", "piano", "steelpan", "triangle", "wood block", "guitar", 
                "wheelharp", "MIDI keyboard")

# ##################### #
# FUNCTION DEFINITIONS  #
# ##################### #
UserProfileRanGen <- function(id, db = sim){
  # ######################## #
  # simulate a user profile  # 
  # ######################## #
  id     <- id
  gender <- sample(x=c("female", "male"), size=1)
  age    <- sample(x=12:45, size=1)
  loc    <- sample(x = sim$loc, size=1)
  genres <- sample(sim$genres, size=5)
  instru <- sample(sim$instru, size = 3)
  res    <- c("id" = id, "age" = age, "location" = loc, "genre" = genres, "instruments" = instru)
  return(res)
}

# ##################### #
# SIMULATION DATA -------
# ##################### #
# Profile:
#   age:
#   gender:
#   location:
#   genres(music):
#   instruments:
set       <- list()
set$size  <- 1000
set$ratio <- .7

temp     <- list()
temp$IDs <- sample(x=1:100000, size=set$size, replace=F)

# Generate the simulated data
db         <- as.data.frame(t(sapply(temp$IDs, UserProfileRanGen)), stringsAsFactor = TRUE)
names(db)  <- normVarNames(names(db))
db$id      <- levels(db$id)[as.numeric(db$id)]
db$age     <- as.numeric(db$age)

# Process factor/chara variables
db$location      <- as.numeric(db$location)
db$genre_1       <- as.numeric(db$genre_1)
db$genre_2       <- as.numeric(db$genre_2)
db$genre_3       <- as.numeric(db$genre_3)
db$genre_4       <- as.numeric(db$genre_4)
db$genre_5       <- as.numeric(db$genre_5)
db$instruments_1 <- as.numeric(db$instruments_1)
db$instruments_2 <- as.numeric(db$instruments_2)
db$instruments_3 <- as.numeric(db$instruments_3)

# scale(normalization)
db[, -1] <- scale(db[, -1])

# ############## #
# Visualization  #
# ############## #
img <- list()
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
model$km_sel_c <- kmeansruns(scale(db[, -1]), krange=temp$nk, criterion="ch")
model$km_sel_a <- kmeansruns(scale(db[, -1]), krange=temp$nk, criterion="asw")


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
temp$dsc  <- data.frame(k=temp$nk, ch=scale(model$km_sel_c$crit), aws=scale(model$km_sel_c$crit))
temp$dscm <- melt(temp$dsc, id.vars="k", variable.name="Measure")
img$k_sel <- ggplot(temp$dscm, aes(x=k, y=value, colur = Measure)) +
             geom_point(aes(shape=Measure))                        +
             geom_line(aes(linetype=Measure))                      +
             scale_x_continuous(breaks=temp$nk, labels = temp$nk)          

              
             