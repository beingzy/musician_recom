# ################################################ #
# USER-Recommendation Engine Development, profile- #
# based search                                     #
#                                                  #
# Author: Yi Zhang                                 #
# Date: Jun/22/2014                                #
# ################################################ #
library(reshape2)     # Data Preparating
library(plyr)         # Data Wrangling
library(ggplot2)      # Data Visualization
library(randomForest) # Utitlize na.roughFix() to impute missing data
library(animation)    # Demonstrate kmeans
library(fpc)          # Tuning Clustering with kmeansuns() and clusterboot()
library(wskm)         # Weighted subspace clustering
library(amap)         # hclusterpar

# ################## #
# SET UP Environent  #
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

# ################ #
# SIMULATION DATA  #
# ################ #
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

db <- as.data.frame(t(sapply(temp$IDs, UserProfileRanGen)))
#
db$age          <- as.numeric(db$age)
db$location     <- as.factor(db$location)
db$genre1       <- as.factor(db$genre1)
db$genre2       <- as.factor(db$genre2)
db$genre3       <- as.factor(db$genre3)
db$genre4       <- as.factor(db$genre4)
db$genre5       <- as.factor(db$genre5)
db$instruments1 <- as.factor(db$instruments1)
db$instruments2 <- as.factor(db$instruments2)
db$instruments3 <- as.factor(db$instruments3)

# ############## #
# Visualization  #
# ############## #
img <- list()
img <- ggplot(data=db, aes(id, location, genre1, genre2, genre3, genre4, genre5, instruments1, instruments2, instruments3)) + 
       geom_tile(colour = "white")                                                                                          + 
       scale_fill_gradient(low = "white", high = "steelblue")       


