# All imports-------------------
import random, json, datetime, time
from sklearn import metrics, cluster
import numpy as np
import pylab as pl
## Clustering Facility ------------
import sklearn
from sklearn import metrics
from sklearn.metrics import pairwise
from sklearn.cluster import KMeans
from sklearn.datasets import load_digits
from sklearn.decomposition import PCA
from sklearn.preprocessing import scale
from sklearn.feature_extraction import DictVectorizer
 
## DB/Data ------------------------
import pymongo
import json
from bson.objectid import ObjectId
 
## Data Vis -----------------------
import pylab as pl
from matplotlib import pyplot as plt
 
## ############################### #
## CLASS DEFINITION                #
## ############################### #
class Musician(object):
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
            # Change
            res = {"DoB": self.DoB, "gender": self.gender, "music_pref":self.music_pref, "instrument":self.instrument}
        return res
  
    def toJSON(self):
        str_json = json.dumps({"DoB":self.DoB, "gender":self.gender, "music_pref":self.music_pref, "instrument":self.instrument})
        res      = json.loads(str_json)
        return res
 
 
## Searcher Filter -------------------------
# class  UserFilter(object):
    """ Filter object to store searching conditions """
#       def __init__(self):
#        super( , self).__init__()
#        self.arg = arg
 
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
 
def convertToDictDB(ds, idx=None):
    # Return [dict, dict, dict]
    if idx == None:
        res = [ ds[i].toDict() for i in range(len(ds)) ]
    else:
        res = [ ds[i].toDict() for i in idx ]
    return res
 
## Recommendation function -----------------------
def recommendationMaker(user_idx, processed_db, cluster_estimator, DoB=None, gender=None, must_like_music=None, must_master_instrument=None, recommendation_size = 5):
    # Make recommendation for users
    user_class = cluster_estimator.labels_[user_idx]
    tot_user   = len(processed_db)
    # Apply searching filter --------------------------
    label_filter = [i for i in range(tot_user) if kmeans_final.labels_[i] == user_class]
    age_filter = gender_filter = music_filter = instrument_filter = range(tot_user)
    if DoB is not None:
        age_filter         = [i for i in range(tot_user) if musician_db[i].toDict(rect=False)['DoB'] < DoB]
    if gender is not None:
        gender_filter      = [i for i in range(tot_user) if musician_db[i].toDict(rect=False)['gender'] is gender]
    if must_like_music is not None:
        music_filter       = [i for i in range(tot_user) if must_like_music in musician_db[i].toDict(rect=False)['music_pref']]
    if must_master_instrument is not None:
        instrument_filter  = [i for i in range(tot_user) if must_master_instrument in musician_db[i].toDict(rect=False)['instrument']]
    # Filerting the users
    candidates_idx = list(set(age_filter).intersection(gender_filter).intersection(music_filter).intersection(instrument_filter))
    if len(candidates_idx) > recommendation_size:
        candidates_idx_sameLabel = list(set(candidates_idx).intersection(label_filter))
        if len(candidates_idx_sameLabel) > recommendation_size:
            candidates_idx = candidates_idx_sameLabel
    # Sorting the candidates with distance to user ------------------------
    toUser_dist = [pairwise.pairwise_distances(processed_db[[user_idx, c_idx],:], metric="manhattan")[0, 1] for c_idx in candidates_idx]
    sorted_idx  = [idx for (dist, idx) in sorted(zip(toUser_dist, candidates_idx))]
    sorted_idx  = sorted_idx[:recommendation_size]
    return sorted_idx
 
## #################################################### #
## DATA OPERATION:                                      #
##   a. Data Simulation (or Downlaod data from server)  # 
#    b. Data Uploading                                  #
## #################################################### #
## a. Generate the simulated data ----------------------
random.seed(2014)
musician_db = []
for i in range(1000):
    # generate sample data
    temp_user = Musician()
    musician_db.append(temp_user)
## Connect to database
mongoClient = pymongo.MongoClient('54.88.134.182', port = 27017)
db          = mongoClient.user_v1
## Upload Data ----------------------------


## #################################################### #
## DEVELOPING K-MEANS MODEL                             #
## a. Data Processing (Vectorization, Normalization)    #
## b. Data Training, Evaluation, Selection              #
## c. Deploy selected model                             #
## #################################################### #
## a. Data Processing
## Data Wrangling (vectorization, normalization, slicing) -------------------
vec                = DictVectorizer()
db_dict            = convertToDictDB(musician_db)
db_dict_coded      = vec.fit_transform(db_dict).toarray()
# remove "DoB" and "gender" from modeling
col_features_train = [i for i, f in enumerate(vec.feature_names_) if f not in ['DoB', 'gender=female', 'gender=male']]
db_dict_coded      = db_dict_coded[:, col_features_train]
## Do not need normalization for all binary variables
## Display DB information
print db_dict_coded.shape, np.sum(db_dict_coded, axis = 0).shape, np.sum(db_dict_coded, axis = 1).shape
 
## #################################################### #
## c.Train Production Model -----------------------------------------------------------
print "Classifier is fitting into the data......"
kmeans_final = KMeans(init='k-means++', n_clusters = 20, n_init = 1000)
kmeans_final.fit(db_dict_coded)
print "Classifier had succeeded in revolutionzing itself!!!!"
## #################################### #
## Make a recommendation ----------------
recom_list = recommendationMaker(user_idx = 3, processed_db=db_dict_coded, cluster_estimator=kmeans_final, gender="female")
print "user(idx:3)\'s recommended friends:", recom_lists





