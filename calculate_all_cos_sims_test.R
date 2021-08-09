# rm(list=ls())
setwd("C:/Users/gaoan/Desktop/step8/subsets_untransposed")

'%!in%' <- Negate('%in%')

# list_of_all_gameids = read.csv("C:/Users/gaoan/Desktop/step8/gen7_metacritic_scores_all_platforms.csv")$game_id
list_of_all_gameids = all_ids

list_of_genres = c("action adventure","action","role-playing","puzzle",
                   "racing","miscellaneous","simulation","adventure",
                   "sports","strategy")

# list_of_genres = c("action adventure","action")

list_of_years = rev(as.numeric(c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")))

final_cos_sim_df = data.frame()


###### ADD FOR LOOP HERE TO GO THROUGH GENRES 1 BY 1
for(current_genre in list_of_genres){
  
  ################################################################################
  # CREATE DF OF SIMILARITY SCORES FOR ALL GAME IN SUBSET BASED ON PREV. 1 YEAR PROTOTYPE
  
  full_cos_sim_df = data.frame()
  
  for (year in list_of_years[1:(length(list_of_years)-1)]){
    
    # CHECK IF CURRENT YEAR OR PREVIOUS YEAR IS EMPTY
    
    if(file.size(sprintf("%s_wordtype_all_pct_full_mc_allscore_%s.csv", sub(" ", "_", current_genre), year)) > 4 &
       file.size(sprintf("%s_wordtype_all_pct_full_mc_allscore_%s.csv", sub(" ", "_", current_genre), year-1)) > 4){
    
      df1 = read.csv(sprintf("%s_wordtype_all_pct_full_mc_allscore_%s.csv", sub(" ", "_", current_genre), year))
      df2 = read.csv(sprintf("%s_wordtype_all_pct_full_mc_allscore_%s.csv", sub(" ", "_", current_genre), year-1))
      
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
    
      # missing_ids_df = data.frame()                                              #DONT NEED THIS ANYMORE
      # 
      # list_of_all_gameids[1] %!in% df1$id
      #   
      # for(gameid in list_of_all_gameids){
      #   if(gameid %!in% df1$id){
      #     missing_ids_df = rbind(missing_ids_df, data.frame(year = year, id = gameid, cos_sim = NA))
      #   }
      # }
      # 
      # current_cos_sim_df = rbind(current_cos_sim_df, missing_ids_df)
      
      full_cos_sim_df = rbind(full_cos_sim_df, current_cos_sim_df)
    
    }
  
    # IF EITHER THE CURRENT YEAR OR THE PREVIOUS YEAR IS EMPTY DO THIS
    
    if(file.size(sprintf("%s_wordtype_all_pct_full_mc_allscore_%s.csv", sub(" ", "_", current_genre), year)) <= 4 |
       file.size(sprintf("%s_wordtype_all_pct_full_mc_allscore_%s.csv", sub(" ", "_", current_genre), year-1)) <= 4){
    
      full_cos_sim_df = data.frame(year = year, id = 999999, cos_sim = NA)
  
    }
  
    full_cos_sim_df_1yr = full_cos_sim_df
    # write.csv(full_cos_sim_df, "simlilarities_1yr_action_adventure_wordtype_all_pct_full_mc_allscore.csv", row.names = F)
    
  }

  
  ################################################################################
  
  ################################################################################
  # CREATE DF OF SIMILARITY SCORES FOR ALL GAME IN SUBSET BASED ON PREV. 2 YEARs PROTOTYPE
  
  full_cos_sim_df = data.frame()
  
  # CHECK IF ANY OF THE 
  # CHECK IF CURRENT YEAR OR EITHER OF 2 PREVIOUS YEARs ARE EMPTY
  

  
  
    
    
  for (year in list_of_years[1:(length(list_of_years)-2)]){
    
    if(file.size(sprintf("%s_wordtype_all_pct_full_mc_allscore_%s.csv", sub(" ", "_", current_genre), year)) > 4 &
       file.size(sprintf("%s_wordtype_all_pct_full_mc_allscore_%s.csv", sub(" ", "_", current_genre), year-1)) > 4 &
       file.size(sprintf("%s_wordtype_all_pct_full_mc_allscore_%s.csv", sub(" ", "_", current_genre), year-2)) > 4){
      
      
      df1 = read.csv(sprintf("%s_wordtype_all_pct_full_mc_allscore_%s.csv", sub(" ", "_", current_genre), year))
      
      yr_minus_1 = read.csv(sprintf("%s_wordtype_all_pct_full_mc_allscore_%s.csv", sub(" ", "_", current_genre), year-1))
      yr_minus_2 = read.csv(sprintf("%s_wordtype_all_pct_full_mc_allscore_%s.csv", sub(" ", "_", current_genre), year-2))
       
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
      
      # missing_ids_df = data.frame()                                             #DONT NEED THIS ANYMORE
      # 
      # for(gameid in list_of_all_gameids){
      #   if(gameid %!in% df1$id){
      #     missing_ids_df = rbind(missing_ids_df, data.frame(year = year, id = gameid, cos_sim = NA))
      #   }
      # }
      # 
      # current_cos_sim_df = rbind(current_cos_sim_df, missing_ids_df)
      
      full_cos_sim_df = rbind(full_cos_sim_df, current_cos_sim_df)

    }
    
    # IF EITHER THE CURRENT YEAR OR THE PREVIOUS YEAR IS EMPTY DO THIS
    
    if(file.size(sprintf("%s_wordtype_all_pct_full_mc_allscore_%s.csv", sub(" ", "_", current_genre), year)) <= 4 |
       file.size(sprintf("%s_wordtype_all_pct_full_mc_allscore_%s.csv", sub(" ", "_", current_genre), year-1)) <= 4 |
       file.size(sprintf("%s_wordtype_all_pct_full_mc_allscore_%s.csv", sub(" ", "_", current_genre), year-2)) <= 4){
      
      full_cos_sim_df = data.frame(year = year, id = 999999, cos_sim = NA)
      
    }

  }

  full_cos_sim_df_2yr = full_cos_sim_df
  # write.csv(full_cos_sim_df, "simlilarities_2yr_action_adventure_wordtype_all_pct_full_mc_allscore.csv", row.names = F) 
  

  
  ################################################################################
  # MERGE THE TWO TOGETHER
  
  together_full_cos_sim_df = merge(full_cos_sim_df_1yr, full_cos_sim_df_2yr, by=c("year","id"), all.x = T)
  colnames(together_full_cos_sim_df) = c("year","id","sim_prev1yr","sim_prev2yr")
  
  # write.csv(together_full_cos_sim_df, "simlilarities_1yr_and_2yr_action_adventure_wordtype_all_pct_full_mc_allscore.csv", row.names = F)
  
  final_cos_sim_df = rbind(final_cos_sim_df, together_full_cos_sim_df)

  

} 


################################################################################
# WRITE THE RESULT TO FILE


# final_cos_sim_df = na.omit(final_cos_sim_df)        #CHANGE THIS TO REMOVE 999999 ??

final_cos_sim_df = final_cos_sim_df[, c("id", "sim_prev1yr", "sim_prev2yr")]

write.csv(final_cos_sim_df, "test.csv", row.names = F)













# JUST CHECKING THAT ALL THE GAMES ARE ACCOUNTED FOR (E.G., 2007-2014 THERE WERE 260 AA GAMES)
# test_df = together_full_cos_sim_df[, c("id", "sim_previous1year", "sim_previous2years")]
# test_df = na.omit(test_df)
# test_df = test_df[test_df$id %in% all_ids, ]
# test_df2 = info[info$game_id %in% all_ids, ]
# test_df2 =test_df2[test_df2$genre_wide == "action adventure", ]
# test_df2 = test_df2[test_df2$release_year > 2006, ]                   #260

# test_df = together_full_cos_sim_df[, c("id", "sim_previous1year", "sim_previous2years")]
# test_df = na.omit(test_df)
# test_df = test_df[test_df$id %in% all_ids, ]
# test_df2 = info[info$game_id %in% all_ids, ]
# test_df2 =test_df2[test_df2$genre_wide == "action", ]
# test_df2 = test_df2[test_df2$release_year > 2006, ]                   #1060

















################################################################################
################################################################################
# TEST ONE OFF CASE HERE
# 
# df1 = read.csv("action_adventure_wordtype_all_pct_full_mc_allscore_2006.csv")
# df2 = read.csv("action_adventure_wordtype_all_pct_full_mc_allscore_2005.csv")
# 
# #NOT SURE HOW TO LABEL THE ROWNAMES COLUMN WHEN WRITING CSV SO ID COLUMN IS BLANK ("X" WHEN READ)
# colnames(df1)[1] = "id"
# df1$id = as.character(df1$id)
# 
# colnames(df2)[1] = "id"
# df2$id = as.character(df2$id)
# 
# ################################################################################
# # CALCULATE AVERAGES FOR PROTOTYPE
# 
# df2 = rbind(df2, c("Total",colSums(df2[,2:ncol(df2)])))
# 
# library(dplyr)
# df2[,2:ncol(df2)] <- mutate_all(df2[,2:ncol(df2)], function(x) as.numeric(as.character(x)))
# 
# df2 = rbind(df2, c("avg", colSums(df2[,2:ncol(df2)]) / (nrow(df2) - 1)))                    # IS THIS RIGHT????
# 
# df2 = df2[-c(nrow(df2)-1),]
# 
# rownames(df2) <- NULL
# 
# # df1 = rbind(df1, as.data.frame(df2[nrow(df2),1:ncol(df2)]))
# 
# #####################################################
# # CALCULATE COSINE SIMILARITY
# 
# library(lsa)
# 
# rownames(df1) = df1$id
# 
# prototype = as.numeric(df2[nrow(df2), 2:ncol(df2)])
# 
# cosine_vals = vector()
# 
# for(i in 1:nrow(df1)){
#   print(i)
#   v1 = as.numeric(df1[i,2:ncol(df1)])
#   c_mat = cosine(v1, prototype)
#   cosine_vals = c(cosine_vals, (c_mat[1]))
# }
# 
# cos_sim_df = data.frame(id = df1$id, cos_sim = cosine_vals)
