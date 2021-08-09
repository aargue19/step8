rm(list=ls())
setwd("C:/Users/gaoan/Desktop/step8")

info = read.csv("games_basic_info.csv")
words = read.csv("step7/step7_20210705.csv")
scores = read.csv("gen7_metacritic_scores_all_platforms.csv")
this = read.csv("step7/step7_5_6.csv")

#REMOVE ALL GAMES WITH NO MC RATING
scores = scores[scores$Check != 0,]
words = words[words$id %in% scores$game_id,]

#MERGE DATASETS
full_df = merge(words, info, by.x = "id", by.y="game_id")

#MAKE A DF OF IDS AND STRING OF STD WORDS
strings_df = data.frame(game_id=NA, std_string=NA)

for(current_id in unique(words$id)){
  current_vec = unique(words$std_word[words$id == current_id])
  current_string = paste(current_vec,collapse=" ")
  line_to_add = data.frame(game_id = current_id, 
                           std_string = current_string)
  
  strings_df = rbind(strings_df, line_to_add)
}

#REMOVE THE FIRST ROW OF THE DATASET WITH NA
strings_df = strings_df[2:nrow(strings_df),]

#MERGE THE DATASETS TO GET THE STRING COLUMN 
full_df = merge(full_df, strings_df, by.x = "id", by.y = "game_id")

#######################################################################
# CREATE SUBSETS FOR COMBOS OF WORDFREQUENCY, METACRITICSCORE, YEAR

# WORDFREQUENCY: WORDS INCLUDED AT LEAST 0%, 1%, 5% OF GAMES
# MC SCORE: GAMES WITH SCORE ABOVE 0%, 75%, 90%
# YEAR: 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2013, 2014+

sub_df = full_df

# WORD COUNTS
library(dplyr)
all_counts = count(sub_df, std_word)

for(i in 1:nrow(sub_df)){
  sub_df$wrd_counts[i] = all_counts$n[all_counts$std_word == sub_df$std_word[i]]
}

# WORD FREQS



#######################################################################
#


#MATRIX FOR TOP 98% OF WORDS

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

