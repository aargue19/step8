rm(list=ls())
setwd("C:/Users/gaoan/Desktop/step8")

info = read.csv("games_basic_info.csv")
words = read.csv("step7/step7_20210705.csv")
scores = read.csv("gen7_metacritic_scores_all_platforms.csv")
this = read.csv("step7/step7_5_6.csv")

#MERGE DATASETS
full_df = merge(words, info, by.x = "id", by.y="game_id")

full_df = merge(full_df, scores, by.x = "id", by.y = "game_id")

#REMOVE ALL GAMES WITH NO MC RATING
full_df = full_df[full_df$Check != 0,]

#REMOVE ALL "DELETE" WORDS
full_df = full_df[full_df$std_word != "DELETE",]

#COMBINE 2014, 15, AND 16
full_df$release_year[full_df$release_year == "2015" | full_df$release_year == "2016"] = "2014"

full_df$std_word[full_df$std_word == "2D"] = "x2d"
full_df$std_word[full_df$std_word == "3D"] = "x3d"
full_df$std_word[full_df$std_word == "JUST_NAME"] = "just_name"

################################################################################
# CREATE SUBSETS FOR COMBOS OF WORDFREQUENCY, METACRITICSCORE, YEAR

# WORDFREQUENCY: WORDS INCLUDED AT LEAST 0%, 1%, 5% OF GAMES
# MC SCORE: GAMES WITH SCORE ABOVE 0%, 75%, 90%
# YEAR: 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2013, 2014

sub_df = full_df
################################################################################
# WORD COUNTS

sub_df$wrd_counts = NA

all_stds = unique(sub_df$std_word) #1157 words
all_ids = unique(sub_df$id) # 2681 game ids

count = 0

for(i in all_stds){
  
  print(sprintf("checking word %s", i))
  
  for(j in all_ids){
    #print(sprintf("checking game %s", j))
    
    if(i %in% sub_df$std_word[sub_df$id == j]){
      #print(sprintf("the word %s, is in game %s", i, j))
      
      count = count+1
    }
    
  }
  
  #print(sprintf("the word %s, appears in %s games", i, count))
  sub_df$wrd_counts[sub_df$std_word == i] = count
  count = 0
  
}

# WORD FREQUENCIES THRESHOLDS

freq_df = data.frame(std_word = sub_df$std_word, count = sub_df$wrd_counts)
freq_df = freq_df[!duplicated(freq_df),]

freq_df$frequency = freq_df$count / length(unique(sub_df$id))

nrow(freq_df) #1157 std_words
freq_df$top99 = 0
freq_df$top99[freq_df$frequency >=0.01] = 1

freq_df$top95 = 0
freq_df$top95[freq_df$frequency >=0.05] = 1

nrow(freq_df[freq_df$top95 == 1,]) #57 words          #USE THIS VECTOR OF STD_WORDS TO FILTER OUT THE BOTTOM 1%
nrow(freq_df[freq_df$top99 == 1,]) #342 words         #USE THIS VECTOR OF STD_WORDS TO FILTER OUT THE BOTTOM 5%

top95_words = freq_df$std_word[freq_df$top95 == 1]
top99_words = freq_df$std_word[freq_df$top99 == 1]

sub_df$pct_full = 1   #ALL GAMES
sub_df$pct_95p = 0
sub_df$pct_99p = 0

sub_df$pct_95p[sub_df$std_word %in% top95_words] = 1
sub_df$pct_99p[sub_df$std_word %in% top99_words] = 1



################################################################################
# CREATE TWO NEW COLUMNS FOR METACRITIC SCORE THRESHOLDS

df = sub_df
df = df[ , -which(names(df) %in% c("game_name.x","game_name.y","Check","X","index","num","duplicate"))]   #redundant columns due to merge

#SOME PS3 SCORES ARE "" OR "ESRB"

df$ps3_score[df$ps3_score == ""] = NA
df$ps3_score[df$ps3_score == "ESRB"] = NA
df$ps3_score[df$ps3_score == "0"] = NA
table(as.numeric(df$ps3_score))
summary(as.numeric(df$ps3_score))

df$wii_score[df$wii_score == ""] = NA
df$wii_score[df$wii_score == "ESRB"] = NA
df$wii_score[df$wii_score == "0"] = NA
table(as.numeric(df$wii_score))
summary(as.numeric(df$wii_score))

df$xbox360_score[df$xbox360_score == ""] = NA
df$xbox360_score[df$xbox360_score == "ESRB"] = NA
df$xbox360_score[df$xbox360_score == "0"] = NA
table(as.numeric(df$xbox360_score))
summary(as.numeric(df$xbox360_score))

# ARE THERE ANY ROWS WITH NAS IN ALL CRITIC SCORE COLUMNS?
df[is.na(df$ps3_score) & is.na(df$wii_score) & is.na(df$xbox360_score),]  #nope

#CALCULATE AVERAGE SCORES
#FIRST GET A COUNT OF THE NUMBER OF REVIEWS FOR DENOMINATOR
library(dplyr)
score_cols = c("ps3_score", "wii_score", "xbox360_score")
df = df %>%  mutate(missing_scores = rowSums(is.na(.[score_cols])))
df$number_of_scores = 3 - df$missing_scores
df$missing_scores = NULL

df$ps3_score[is.na(df$ps3_score)] = 0
df$ps3_score[is.na(df$ps3_score)] = 0
df$ps3_score[is.na(df$ps3_score)] = 0

#CHANGE NAS INTO ZEROS FOR CALCULATION OF AVERAGE
df$avg_mc_score = (ifelse(is.na(df$ps3_score), 0, as.integer(df$ps3_score))  + ifelse(is.na(df$wii_score), 0, as.integer(df$wii_score)) + ifelse(is.na(df$xbox360_score), 0, as.integer(df$xbox360_score))) / as.integer(df$number_of_scores)

df$mc_allscore = 1    #ALL GAMES
df$mc_75score = 0
df$mc_90score = 0

df$mc_75score[df$avg_mc_score >= 75] = 1  #ONLY THOSE WITH SCORES OVER 75
df$mc_90score[df$avg_mc_score >= 90] = 1  #ONLY THOSE WITH SCORES OVER 90

################################################################################
# CREATE DUMMIES FOR WORDTYPES

df$wordtype_all = 1
df$wordtype_wm = 0
df$wordtype_world = 0
df$wordtype_mech = 0

df$wordtype_wm[df$remark == "world" | df$remark == "event"] = 1
df$wordtype_world[df$remark == "world" | df$remark == "avatar" | df$remark == "name" | df$remark == "perspective"] = 1
df$wordtype_mech[df$remark == "event"] = 1