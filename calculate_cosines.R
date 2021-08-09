# rm(list=ls())
setwd("C:/Users/gaoan/Desktop/step8/subsets_untransposed")

df1 = read.csv("action_adventure_wordtype_all_pct_full_mc_allscore_2012_31.csv")

df2 = read.csv("action_adventure_wordtype_all_pct_full_mc_allscore_2011_42.csv")

#NOT SURE HOW TO LABEL THE ROWNAMES COLUMN WHEN WRITING CSV SO ID COLUMN IS BLANK ("X" WHEN READ)
colnames(df2)[1] = "id"

df2$id = as.character(df2$id)

################################################################################
# CALCULATE AVERAGES FOR PROTOTYPE

df2 = rbind(df2, c("Total",colSums(df2[,2:ncol(df2)])))

library(dplyr)
df2[,2:ncol(df2)] <- mutate_all(df2[,2:ncol(df2)], function(x) as.numeric(as.character(x)))

df2 = rbind(df2, c("avg", colSums(df2[,2:ncol(df2)]) / (nrow(df2) - 1)))

df2 = df2[-c(nrow(df2)-1),]

rownames(df2) <- NULL

df1 = rbind(df1, df2[nrow(df2),])


################################################################################

corp2 = corpus(df2, text_field = "id")

temp = colSums(df2[,2:ncol(df2)])

dataset$Count[is.nan(dataset$Count)]<-0
