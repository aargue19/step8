# rm(list=ls())
setwd("C:/Users/gaoan/Desktop/step8/subsets_untransposed")

# list_of_years = rev(as.numeric(c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")))
# 
# for (year in list_of_years){
  


df1 = read.csv("action_adventure_wordtype_all_pct_full_mc_allscore_2012.csv")
df2 = read.csv("action_adventure_wordtype_all_pct_full_mc_allscore_2011.csv")

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

prototype = as.numeric(df2[nrow(df1), 2:ncol(df1)])

cosine_vals = vector()

for(i in 1:nrow(df1)){
  print(i)
  v1 = as.numeric(df1[i,2:ncol(df1)])
  c_mat = cosine(v1, prototype)
  cosine_vals = c(cosine_vals, (c_mat[1]))
}

cos_sim_df = data.frame(id = df1$id, cos_sim = cosine_vals)

