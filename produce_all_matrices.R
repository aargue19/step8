################################################################################

# USE THIS TO PRODUCE UNTRANSPOSED VERSIONS

################################################################################
setwd("C:/Users/gaoan/Desktop/step8")
library(quanteda)

wide_genre_options = c("action adventure","action","role-playing","puzzle",
                       "racing","miscellaneous","simulation","adventure",
                       "sports","strategy")
word_type_options = c("wordtype_all", "wordtype_wm", "wordtype_world", "wordtype_mech")
word_freq_options = c("pct_full", "pct_99p", "pct_95p")
avg_score_options = c("mc_allscore", "mc_90score", "mc_75score")

year_options = c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")

# wide_genre_options = c("action adventure")
# word_type_options = c("wordtype_world")
# word_freq_options = c("pct_full")
# avg_score_options = c("mc_90score")
# year_options = c("2014")

count = 1

current_dir = paste(sub(" ", "_",word_type_options[1]),word_freq_options[1],avg_score_options[1], sep="_")

dir.create(sprintf("C:/Users/gaoan/Desktop/step8/subset_%s", paste(sub(" ", "_",word_type_options[1]),word_freq_options[1],avg_score_options[1], sep="_")))

setwd(sprintf("C:/Users/gaoan/Desktop/step8/subset_%s", paste(sub(" ", "_",word_type_options[1]),word_freq_options[1],avg_score_options[1], sep="_")))

for(i in wide_genre_options){
  for(j in word_type_options){
    for(x in word_freq_options){
      for(y in avg_score_options){
        for(z in year_options){
          
          current_subset = df[df$genre_wide == i & df[[j]] == 1 & df[[x]] == 1 & df[[y]] == 1 & df$release_year == z,]
          games_n = length(unique(df$id[df$genre_wide == i & df[[j]] == 1 & df[[x]] == 1 & df[[y]] == 1 & df$release_year == z]))
          
          print(sprintf("#%s, genre: %s, word type: %s, word pct: %s, score: %s, year: %s, n: %s", count, i, j, x, y, z, games_n))
          
          if(games_n >0){
            
            temp = current_subset[,c("id","std_word")]
            
            strings_df = data.frame()
            
            for(current_id in unique(temp$id)){
              current_vec = unique(temp$std_word[temp$id == current_id])
              current_string = paste(current_vec,collapse=" ")
              line_to_add = data.frame(id = current_id, 
                                       std_string = current_string)
              
              strings_df = rbind(strings_df, line_to_add)
            }
            
            temp = merge(temp, strings_df, by="id")
            temp$std_word = NULL
            temp = temp[!duplicated(temp),]
            
            library(quanteda)
            
            #CREATE CORPUS
            
            corp = corpus(temp, text_field = 'std_string')
            
            toks <- corp %>% tokens()
            
            dtm = dfm(toks)
            
            # reassign the document names
            docnames(dtm) <- dtm$id
            
            mat = as.matrix(dtm)                #AROUND HERE x2D gets changed to lowercase
            
            
            
            #TRANSPOSE MATRIX BEFORE SAVING
            # mat = t(mat)
            
            #CREATE DF TO SAVE TO CSV
            mat_df = data.frame(mat)
            
            # colnames(mat_df)[colnames(mat_df) == "x2d"] = "x2D"   # STILL NEED THIS????
            # colnames(mat_df)[colnames(mat_df) == "x3d"] = "x3D"
            
            list_of_all_std_words = unique(df$std_word)
            
            '%!in%' <- Negate('%in%')
            
            names_of_cols = colnames(mat)
            
            for(std in list_of_all_std_words){
              # print(sprintf("checking %s", std))
              if(std %!in% names_of_cols){
                
                col_to_add = c(rep(0,nrow(mat_df)))
                mat_df[[std]] = col_to_add
                
              }
            }
            
            final_mat_df <- mat_df[,order(colnames(mat_df))]
            
            #SAVE AS CSV
            write.csv(final_mat_df, sprintf("%s.csv",paste(sub(" ", "_",i),sub(" ", "_",j),x,y,z, sep="_")),row.names = T)

          }
          
          if(games_n ==0){
            empty_df = data.frame()
            write.csv(empty_df, sprintf("%s.csv",paste(sub(" ", "_",i),sub(" ", "_",j),x,y,z, sep="_")),row.names = T)

          }
          
          count= count + 1
        }
      }
    }
  }
}

################################################################################
library(beepr)
beep(4)
