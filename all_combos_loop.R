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


################################################################################
# CREATE SUBSETS FOR COMBOS OF WORDFREQUENCY, METACRITICSCORE, YEAR

# WORDFREQUENCY: WORDS INCLUDED AT LEAST 0%, 1%, 5% OF GAMES
# MC SCORE: GAMES WITH SCORE ABOVE 0%, 75%, 90%
# YEAR: 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2013, 2014+

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

sub_df$words_top0 = 1   #ALL GAMES
sub_df$words_top95 = 0
sub_df$words_top99 = 0

sub_df$words_top95[sub_df$std_word %in% top95_words] = 1
sub_df$words_top99[sub_df$std_word %in% top99_words] = 1



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

df$mc_top0 = 1    #ALL GAMES
df$mc_top75 = 0
df$mc_top90 = 0

df$mc_top75[df$avg_mc_score >= 75] = 1  #ONLY THOSE WITH SCORES OVER 75
df$mc_top90[df$avg_mc_score >= 90] = 1  #ONLY THOSE WITH SCORES OVER 90


################################################################################
# CREATE DUMMIES FOR RELEASE YEARS

library(fastDummies)

df = dummy_cols(df, select_columns = "release_year")


################################################################################



# DO SOMETHING HERE THAT CREATES THE 90 SUBSETs YOU WANT THEN RUNS THE QUANTEDA SHIT

word_freq_options = c("words_top95", "words_top99", "words_top0")
avg_score_options = c("mc_top75", "mc_top90", "mc_top0")
year_options = c("release_year_2005", "release_year_2006", "release_year_2007", "release_year_2008", "release_year_2009", "release_year_2010", "release_year_2011", "release_year_2012", "release_year_2013", "release_year_2014", "release_year_2015", "release_year_2016")


count = 1

for(x in word_freq_options){
  
  for(y in avg_score_options){
    
    for(z in year_options){
      
      print(sprintf("subset #%s, word: %s, score: %s, year: %s", count, x, y, z))
      current_subset = df[df[[x]] == 1 & df[[y]] == 1 & df[[z]] == 1,]
    
      #MAKE A DF OF IDS AND STRING OF STD WORDS
      strings_df = data.frame(game_id=NA, std_string=NA)
      
      for(current_id in unique(current_subset$id)){
        current_vec = unique(df$std_word[current_subset$id == current_id])
        current_string = paste(current_vec,collapse=" ")
        line_to_add = data.frame(game_id = current_id, 
                                 std_string = current_string)
        
        strings_df = rbind(strings_df, line_to_add)
      }
      
      #REMOVE THE FIRST ROW OF THE DATASET WITH NA
      strings_df = strings_df[2:nrow(strings_df),]
      
      #MERGE THE DATASETS TO GET THE STRING COLUMN 
      current_subset = merge(current_subset, strings_df, by.x = "id", by.y = "game_id")
      
      new_df = current_subset[, c("id", "std_string")]
      new_df = new_df[!duplicated(new_df),]
      
      #https://github.com/ccs-amsterdam/r-course-material/blob/master/tutorials/R_text_3_quanteda.md
      
      #CREATE CORPUS
      library(quanteda)
      
      corp = corpus(new_df, text_field = 'std_string')
      
      toks <- corp %>% tokens()
      
      dtm = dfm(toks)
      
      # reassign the document names
      docnames(dtm) <- dtm$id
      
      mat = as.matrix(dtm)
      
      #TRANSPOSE MATRIX BEFORE SAVING
      mat = t(mat)
      
      output_df = mat
      
      output_df <- output_df[order(row.names(output_df)), ]
      
      # write.csv(output_df,"test_matrix2.csv")
      
      write.csv(output_df, sprintf("%s.csv", paste(x,y,z, sep="_")) )      
      
      
      count= count + 1
    }
    
  }
  
}


################################################################################
# CREATE A COLUMN OF THE JOINED STD WORDS FOR QUANTEDA TO USE









##### PUT ALL THIS IN THE LOOP ABOVE ##########

# current_subset = df[df[["words_top95"]] == 1 & df[["mc_top75"]] == 1 & df[["release_year_2005"]] == 1,]

#unique(current_subset$std_word)











# #MAKE A DF OF IDS AND STRING OF STD WORDS
# strings_df = data.frame(game_id=NA, std_string=NA)
# 
# for(current_id in unique(words$id)){
#   current_vec = unique(words$std_word[words$id == current_id])
#   current_string = paste(current_vec,collapse=" ")
#   line_to_add = data.frame(game_id = current_id, 
#                            std_string = current_string)
#   
#   strings_df = rbind(strings_df, line_to_add)
# }
# 
# #REMOVE THE FIRST ROW OF THE DATASET WITH NA
# strings_df = strings_df[2:nrow(strings_df),]
# 
# #MERGE THE DATASETS TO GET THE STRING COLUMN 
# full_df = merge(full_df, strings_df, by.x = "id", by.y = "game_id")




################################################################################






new_df = full_df[, c("id", "std_string")]
new_df = new_df[!duplicated(new_df),]

#https://github.com/ccs-amsterdam/r-course-material/blob/master/tutorials/R_text_3_quanteda.md

#CREATE CORPUS
library(quanteda)

corp = corpus(new_df, text_field = 'std_string')

toks <- corp %>%
  tokens()

# removal options
toks <- tokens_remove(toks, "delete", padding = TRUE)

dtm = dfm(toks)    #orignially we have #1156 features

dtm = dfm_remove(dtm, "")

# reassign the document names
docnames(dtm) <- dtm$id


#We can use the dfm_trim function to remove columns based on criteria
#specified in the arguments. Here we say that we want to remove all 
#terms for which the frequency (i.e. the sum value of the column in 
#the DTM) is below 10.

# REMOVE ALL WORDS THAT APPEAR IN LESS THAN 2% of GAMES
# dtm  = dfm_trim(dtm, min_termfreq = 2)   #1078 columns now

mat = as.matrix(dtm)
write.csv(mat, "matrix_test.csv")
