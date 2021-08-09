# rm(list=ls())
setwd("C:/Users/gaoan/Desktop/step8/subsets_untransposed")

'%!in%' <- Negate('%in%')

# list_of_all_gameids = read.csv("C:/Users/gaoan/Desktop/step8/gen7_metacritic_scores_all_platforms.csv")$game_id
list_of_all_gameids = all_ids

################################################################################
# CREATE DF OF SIMILARITY SCORES FOR ALL GAME IN SUBSET BASED ON PREV. 1 YEAR PROTOTYPE

list_of_years = rev(as.numeric(c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")))

full_cos_sim_df = data.frame()

for (year in list_of_years[1:(length(list_of_years)-1)]){

  df1 = read.csv(sprintf("action_adventure_wordtype_all_pct_full_mc_allscore_%s.csv", year))
  df2 = read.csv(sprintf("action_adventure_wordtype_all_pct_full_mc_allscore_%s.csv",year-1))
  
  #NOT SURE HOW TO LABEL THE ROWNAMES COLUMN WHEN WRITING CSV SO ID COLUMN IS BLANK ("X" WHEN READ)
  colnames(df1)[1] = "id"
  df1$id = as.character(df1$id)
  
  colnames(df2)[1] = "id"
  df2$id = as.character(df2$id)
  
  ################################################################################
  # CALCULATE AVERAGES FOR PROTOTYPE
  df2 = rbind(df2, c("Total",colSums(df2[,2:ncol(df2)])))
  
  library(dplyr)
  df2[,2:ncol(df2)] <- mutate_all(df2[,2:ncol(df2)], function(x) as.numeric(as.character(x)))
  
  df2 = rbind(df2, c("avg", colSums(df2[,2:ncol(df2)]) / (nrow(df2) - 1)))                    # IS THIS RIGHT????
  
  df2 = df2[-c(nrow(df2)-1),]
  
  rownames(df2) <- NULL
  
  # df1 = rbind(df1, as.data.frame(df2[nrow(df2),1:ncol(df2)]))
  
  #####################################################
  # CALCULATE COSINE SIMILARITY
  
  library(lsa)
  
  rownames(df1) = df1$id
  
  prototype = as.numeric(df2[nrow(df2), 2:ncol(df2)])
  
  cosine_vals = vector()
  
  for(i in 1:nrow(df1)){
    print(i)
    v1 = as.numeric(df1[i,2:ncol(df1)])
    c_mat = cosine(v1, prototype)
    cosine_vals = c(cosine_vals, (c_mat[1]))
  }
  
  current_cos_sim_df = data.frame(year = year, id = df1$id, cos_sim = cosine_vals)

  missing_ids_df = data.frame()

  list_of_all_gameids[1] %!in% df1$id
    
  for(gameid in list_of_all_gameids){
    if(gameid %!in% df1$id){
      missing_ids_df = rbind(missing_ids_df, data.frame(year = year, id = gameid, cos_sim = NA))
    }
  }
  
  current_cos_sim_df = rbind(current_cos_sim_df, missing_ids_df)
  
  full_cos_sim_df = rbind(full_cos_sim_df, current_cos_sim_df)

}

full_cos_sim_df_1yr = full_cos_sim_df

write.csv(full_cos_sim_df, "simlilarities_1yr_action_adventure_wordtype_all_pct_full_mc_allscore.csv", row.names = F)

################################################################################

################################################################################
# CREATE DF OF SIMILARITY SCORES FOR ALL GAME IN SUBSET BASED ON PREV. 2 YEARs PROTOTYPE

list_of_years = rev(as.numeric(c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")))

full_cos_sim_df = data.frame()

for (year in list_of_years[1:(length(list_of_years)-2)]){
  
  df1 = read.csv(sprintf("action_adventure_wordtype_all_pct_full_mc_allscore_%s.csv", year))
  
  yr_minus_1 = read.csv(sprintf("action_adventure_wordtype_all_pct_full_mc_allscore_%s.csv",year-1))
  yr_minus_2 = read.csv(sprintf("action_adventure_wordtype_all_pct_full_mc_allscore_%s.csv",year-2))
   
  df2 = rbind(yr_minus_1, yr_minus_2)
  # df2 = df2[!duplicated(df2),]
  
  #NOT SURE HOW TO LABEL THE ROWNAMES COLUMN WHEN WRITING CSV SO ID COLUMN IS BLANK ("X" WHEN READ)
  colnames(df1)[1] = "id"
  df1$id = as.character(df1$id)
  
  colnames(df2)[1] = "id"
  df2$id = as.character(df2$id)
  
  ################################################################################
  # CALCULATE AVERAGES FOR PROTOTYPE
  
  df2 = rbind(df2, c("Total",colSums(df2[,2:ncol(df2)])))
  
  library(dplyr)
  df2[,2:ncol(df2)] <- mutate_all(df2[,2:ncol(df2)], function(x) as.numeric(as.character(x)))
  
  df2 = rbind(df2, c("avg", colSums(df2[,2:ncol(df2)]) / (nrow(df2) - 1)))                    # IS THIS RIGHT????
  
  df2 = df2[-c(nrow(df2)-1),]
  
  rownames(df2) <- NULL
  
  # df1 = rbind(df1, as.data.frame(df2[nrow(df2),1:ncol(df2)]))
  
  #####################################################
  # CALCULATE COSINE SIMILARITY
  
  library(lsa)
  
  rownames(df1) = df1$id
  
  prototype = as.numeric(df2[nrow(df2), 2:ncol(df2)])
  
  cosine_vals = vector()
  
  for(i in 1:nrow(df1)){
    print(i)
    v1 = as.numeric(df1[i,2:ncol(df1)])
    c_mat = cosine(v1, prototype)
    cosine_vals = c(cosine_vals, (c_mat[1]))
  }
  
  current_cos_sim_df = data.frame(year = year, id = df1$id, cos_sim = cosine_vals)
  
  missing_ids_df = data.frame()
  
  list_of_all_gameids[1] %!in% df1$id
  
  for(gameid in list_of_all_gameids){
    if(gameid %!in% df1$id){
      missing_ids_df = rbind(missing_ids_df, data.frame(year = year, id = gameid, cos_sim = NA))
    }
  }
  
  current_cos_sim_df = rbind(current_cos_sim_df, missing_ids_df)
  
  full_cos_sim_df = rbind(full_cos_sim_df, current_cos_sim_df)
  
}

full_cos_sim_df2yr = full_cos_sim_df

write.csv(full_cos_sim_df, "simlilarities_2yr_action_adventure_wordtype_all_pct_full_mc_allscore.csv", row.names = F)


#MERGE THE TWO TOGETHER

together_full_cos_sim_df = merge(full_cos_sim_df_1yr, full_cos_sim_df2yr, by=c("year","id"))






























################################################################################
################################################################################
# TEST ONE OFF CASE HERE

df1 = read.csv("action_adventure_wordtype_all_pct_full_mc_allscore_2006.csv")
df2 = read.csv("action_adventure_wordtype_all_pct_full_mc_allscore_2005.csv")

#NOT SURE HOW TO LABEL THE ROWNAMES COLUMN WHEN WRITING CSV SO ID COLUMN IS BLANK ("X" WHEN READ)
colnames(df1)[1] = "id"
df1$id = as.character(df1$id)

colnames(df2)[1] = "id"
df2$id = as.character(df2$id)

################################################################################
# CALCULATE AVERAGES FOR PROTOTYPE

df2 = rbind(df2, c("Total",colSums(df2[,2:ncol(df2)])))

library(dplyr)
df2[,2:ncol(df2)] <- mutate_all(df2[,2:ncol(df2)], function(x) as.numeric(as.character(x)))

df2 = rbind(df2, c("avg", colSums(df2[,2:ncol(df2)]) / (nrow(df2) - 1)))                    # IS THIS RIGHT????

df2 = df2[-c(nrow(df2)-1),]

rownames(df2) <- NULL

# df1 = rbind(df1, as.data.frame(df2[nrow(df2),1:ncol(df2)]))

#####################################################
# CALCULATE COSINE SIMILARITY

library(lsa)

rownames(df1) = df1$id

prototype = as.numeric(df2[nrow(df2), 2:ncol(df2)])

cosine_vals = vector()

for(i in 1:nrow(df1)){
  print(i)
  v1 = as.numeric(df1[i,2:ncol(df1)])
  c_mat = cosine(v1, prototype)
  cosine_vals = c(cosine_vals, (c_mat[1]))
}

cos_sim_df = data.frame(id = df1$id, cos_sim = cosine_vals)
