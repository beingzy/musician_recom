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
library(ggplot2)      # Data Visualization
# ----- modeling ---- #
library(randomForest) # Utitlize na.roughFix() to impute missing data
library(animation)    # Demonstrate kmeans
library(fpc)          # Tuning Clustering with kmeansuns() and clusterboot()
library(wskm)         # Weighted subspace clustering
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

# Generate the simulated data
db               <- as.data.frame(t(sapply(temp$IDs, UserProfileRanGen)))
names(db)        <- normVarNames(names(db))
db$age           <- as.numeric(db$age)
db$location      <- as.factor(db$location)
db$genre_1       <- as.factor(db$genre_1)
db$genre_2       <- as.factor(db$genre_2)
db$genre_3       <- as.factor(db$genre_3)
db$genre_4       <- as.factor(db$genre_4)
db$genre_5       <- as.factor(db$genre_5)
db$instruments_1 <- as.factor(db$instruments_1)
db$instruments_2 <- as.factor(db$instruments_2)
db$instruments_3 <- as.factor(db$instruments_3)

# ############## #
# Visualization  #
# ############## #
img <- list()
img <- ggplot(data=db, aes(id, location, genre1, genre2, genre3, genre4, genre5, instruments1, instruments2, instruments3)) + 
       geom_tile(colour = "white")                                                                                          + 
       scale_fill_gradient(low = "white", high = "steelblue")       

# ##################################### #
# CLUSTERING ANALYSIS: K-MEANS CLUSTERS #
# ##################################### #

