# All imports-------------------
import random, json, datetime, time
from sklearn import metrics, cluster
import numpy as np
import pylab as pl
 
## Clustering Facility ------------
import sklearn
from sklearn import metrics
from sklearn.cluster import KMeans
from sklearn.datasets import load_digits
from sklearn.decomposition import PCA
from sklearn.preprocessing import scale
from sklearn.feature_extraction import DictVectorizer

## DB/Data --------------
import pymongo
import json
from bson.objectid import ObjectId

## ############################### #
## CLASS DEFINITION                #
## ############################### #
class Musician:
    ## user(musician) class definition ----------------
    ## (*)class variables are set for random generation sample users
    
    # Class variable for contraining the random generation
    gender_range4rnd     = ['female', 'male'] #  + ['bigender', 'undisclosed']
    # loc_range4rnd        = ["new york city", "san francisico", "chengdu", "beijing", "shanghai", "taipei"]
    music_range4rnd      = ["pop", "blues", "avant-garde", "red dirt", "zydeco", "classic country",
                            "progressive folk", "american folk revival", "sung poetry", "indie folk",
                            "techno-folk", "hip pop", "grime", "jazz"]
    instrument_range4rnd = ["clapsticks", "drum kit", "hang", "piano", "steelpan", "triangle", "wood block",
                            "guitar", "wheelharp", "MIDI keyboard"]
   
    ## instance initiation function -----------
    def __init__(self, _id = None, DoB=None, gender=None, music_pref=[], instrument=[]):
        #n__init__(self, _id = None, DoB=None, gender=None, loc=None, music_pref=[], instrument=[]):
        self._id         = _id
        self.DoB         = DoB        if DoB    is not None  else self.rndSetDoB()
        self.gender      = gender     if gender is not None  else self.rndSetGender()
        # self.loc         = loc        if loc    is not None  else self.rndSetLoc()
        self.music_pref  = music_pref if len(music_pref) > 0 else self.rndSetMusicPref()
        self.instrument  = instrument if len(instrument) > 0 else self.rndSetInstrument()
       
    ## random value setter --------------------  
    def rndSetDoB(self):
        return random.sample(range(1970, 2005), 1)[0] # Yaer only for development  
    
    def rndSetGender(self):
        return random.sample(Musician.gender_range4rnd, 1)[0]
       
    #def rndSetLoc(self):
    #    return random.sample(Musician.loc_range4rnd, 1)
       
    def rndSetMusicPref(self):
        return random.sample(Musician.music_range4rnd, random.randint(4, 8))
       
    def rndSetInstrument(self):
        return random.sample(Musician.instrument_range4rnd, random.randint(1, 4))
   
    ## data representation formatter --------------       
    def toFlat(self, rect = True):
        if rect:
            res = [self.DoB, self.gender] + self.music_pref[:4] + self.instrument[:1]
        else:
            res = [self.DoB, self.gender] + self.music_pref + self.instrument
        return res
   
    def toDict(self, rect = True):
        if rect:
            res = {"DoB": self.DoB,
                   "gender": self.gender,
                   "music_pref_0":self.music_pref[0], "music_pref_1":self.music_pref[1],
                   "music_pref_2":self.music_pref[2], "music_pref_3":self.music_pref[3],
                   "instrument":self.instrument[0]}
        else:
            res = {"DoB": self.DoB[0], "gender": self.gender[0], "music_pref":self.music_pref, "instrument":self.instrument}
        return res
   
    def toJSON(self):
        self_json = json.dumps({"DoB":self.DoB, "gender":self.gender, "music_pref":self.music_pref, "instrument":self.instrument})
        return self_json
 
## Data Manipulation ---------------------------
def jsonToMusicianObj(user_json):
    # Convert json object from MongoDB to Musician object
    _id        = str(user_json['_id'])
    DoB        = user_json['DoB']
    gender     = user_json['gender']
    music_pref = user_json['music_pref']
    instrument = user_json['instrument']
    res        = Musician(_id=_id, DoB=DoB, gender=gender, music_pref=music_pref, instrument=instrument)
    return res

def ConvertToDictDB(ds, idx=None):
    # Return [dict, dict, dict]
    if idx == None:
        res = [ ds[i].toDict() for i in range(len(ds)) ]
    else:
        res = [ ds[i].toDict() for i in idx ]
    return res

## #################################################### #
## DATA OPERATION:                                      #
##   a. Data Simulation, b. Data Uploading              #
## #################################################### #
## a. Generate the simulated data ----------------------
random.seed(2014)
musician_db = []
for i in range(1000):
    # generate sample data
    temp_user = Musician()
    musician_db.append(temp_user)
## b. Data Uploading -----------------------------------
## Connect to database
mongoClient = pymongo.MongoClient('54.88.134.182', port = 27017)
db          = mongoClient.user_v1
## Upload Data ----------------------------
for i in range(len(musician_db)):
    print "Inserting %dth records" % i
    db.profile.save(musician_db[i].toDict(False))

## #################################################### #
## DEVELOPING K-MEANS MODEL                             #
## a. Data Processing (Vectorization, Normalization)    #
## b. Data Training, Evaluation, Selection              #
## c. Deploy selected model                             #
## #################################################### #
## a. Data Processing
## Data Wrangling (vectorization, normalization, slicing) -------------------
vec                = DictVectorizer()
db_dict            = ConvertToDictDB(musician_db)
db_dict_coded      = vec.fit_transform(db_dict).toarray()
# remove "DoB" and "gender" from modeling
col_features_train = [i for i, f in enumerate(vec.feature_names_) if f not in ['DoB', 'gender=female', 'gender=male']]
db_dict_coded      = db_dict_coded[:, col_features_train]
## Do not need normalization for all binary variables

## #################################################### #
## b-1. Splitting data into (train, test) ----------------------------------------
tot_obs     = len(db_dict_coded)
train_ratio = .7
train_idx   = random.sample(range(tot_obs), int(tot_obs * train_ratio) )
test_idx    = [i for i in range(tot_obs) if i not in train_idx]
train_set   = db_dict_coded[train_idx]
test_set    = db_dict_coded[test_idx]

## b-2.Train Model --------------------------------------------------------------
kmeans_k20 = KMeans(init='k-means++', n_clusters = 20, n_init = 1000)
kmeans_k20.fit(train_set

## b-3. Evaluation Model    -----------------------------------------------------
test_labels = kmeans_k20.predict(test_set)
train_error = metrics.silhouette_score(train_set, kmeans_k20.labels_, metric='euclidean')
test_error  = metrics.silhouette_score(test_set,  test_labels,        metric='euclidean')
print "test error(general error): ", test_error, " & train error: ", train_error

## #################################################### #
## c.Train Production Model -----------------------------------------------------------
kmeans_final = KMeans(init='k-means++', n_clusters = 20, n_init = 1000)
kmeans_final.fit(db_dict_coded)