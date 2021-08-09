rm(list=ls())
setwd("C:/Users/gaoan/Documents/Thijs/step 7/final files")

step5_df = read.csv("step5_3_18.csv", header=T)
orig_changes = read.csv("step_6_combined_changes_all_5_6_doubles_removed.csv", header=T)
changes_df = read.csv("step_6_changes_problems_5_6.csv", header=T)

#make all changes from step 6 unproblematic file

for (i in 1:nrow(orig_changes)){
  id_to_change = orig_changes$game_id[i]
  orig_word_to_change = orig_changes$word[i]
  change_to_this_std_word = orig_changes$new_std_word[i]
  
  step5_df$std_word[step5_df$id==id_to_change &
                      step5_df$word==orig_word_to_change] = change_to_this_std_word
}

#make all normal changes to std_words

normal_changes_df = changes_df[is.na(changes_df$add_line) &
                               is.na(changes_df$change_remark) &
                               is.na(changes_df$double_change),]

for (i in 1:nrow(normal_changes_df)){
  id_to_change = normal_changes_df$game_id[i]
  orig_word_to_change = normal_changes_df$word[i]
  change_to_this_std_word = normal_changes_df$new_std_word[i]
  
  step5_df$std_word[step5_df$id==id_to_change &
                      step5_df$word==orig_word_to_change] = change_to_this_std_word
}

change_remarks_df = changes_df[!is.na(changes_df$change_remark) &
                                is.na(changes_df$add_line),]

for (i in 1:nrow(change_remarks_df)){
  id_to_change = change_remarks_df$game_id[i]
  orig_word_to_change = change_remarks_df$word[i]
  change_to_this_remark = change_remarks_df$remark[i]
  
  if(change_remarks_df$changed_word[i] == ""){
    
    step5_df$remark[step5_df$id == id_to_change &
                    step5_df$word == orig_word_to_change] = change_to_this_remark
    }
  
  if(change_remarks_df$changed_word[i] != ""){
    changed_word_to_specify = change_remarks_df$changed_word[i]
    
    step5_df$remark[step5_df$id == id_to_change &
                      step5_df$word == orig_word_to_change &
                      step5_df$changed_word == changed_word_to_specify] = change_to_this_remark
    
  }
}

#make double changes

double_changes_df = changes_df[!is.na(changes_df$double_change),]

for (i in 1:nrow(double_changes_df)){
  id_to_change = double_changes_df$game_id[i]
  orig_word_to_change = double_changes_df$word[i]
  change_to_this_remark = double_changes_df$remark[i]
  orig_std_word = double_changes_df$current_std_word[i]
  
  if(double_changes_df$changed_word[i] == ""){
    
    step5_df$std_word[step5_df$id == id_to_change &
                      step5_df$word == orig_word_to_change &
                      step5_df$std_word == orig_std_word] = change_to_this_remark
  }
  
  if(double_changes_df$changed_word[i] != ""){
    changed_word_to_specify = double_changes_df$changed_word[i]
    
    step5_df$remark[step5_df$id == id_to_change &
                      step5_df$word == orig_word_to_change &
                      step5_df$std_word == orig_std_word &
                      step5_df$changed_word == changed_word_to_specify] = change_to_this_remark
    
  }
}

#add new lines to dataset

new_lines_df = changes_df[!is.na(changes_df$add_line),]

for(i in 1: nrow(new_lines_df)){
  
  id_to_add = new_lines_df$game_id[i]
  word_to_add = new_lines_df$word[i]
  remark_to_add = new_lines_df$remark[i]
  std_word_to_add = new_lines_df$new_std_word[i]
  
  temp_df = data.frame("index" = 999999, "id" = id_to_add, "num" = 999999, 
                       "game_num" = 999999, "word" = word_to_add,
                       "leo" = 999999, "andrew" = 999999, "thijs"= 999999, 
                       "picked" = 999999, "final" = 999999, "changed_word" = "",
                       "changed_word2" = "", "remark" = remark_to_add, 
                       "game_name" = "", "game_description" = "", 
                       "stemmed_word" = "", "std_word" = std_word_to_add, 
                       "occurances" = 999999, "duplicate" = 999999)
  
  step5_df = rbind(step5_df,temp_df)
  
  
}

#change all "winter" from "win" to "arctic_frozen_icy" and remarks from event to world

step5_df$std_word[step5_df$word == "winter"] = "arctic_frozen_icy"
step5_df$remark[step5_df$word == "winter"] = "world"


#delete a duplicate game with different ID

step5_df = step5_df[step5_df$id!=664238,]  #remove 664238 b/c its same as 664240

#delete a duplicate game with same IDs

list_of_dup_ids = c(932650,936978,945273,950572,981375,997544,999091)

for (dup_id in list_of_dup_ids){
  
  temp_dup_df = step5_df[step5_df$id==dup_id,]
  
  no_dup_temp_dup_df = temp_dup_df[!duplicated(temp_dup_df[c(2,5,13,17)]),]
  
  step5_df = step5_df[step5_df$id!=dup_id,]
  
  step5_df = rbind(step5_df, no_dup_temp_dup_df)
  
}

#change names of some std_words
# sort(unique(step5_df$std_word))
#unique(sort(step5_df$std_word))[1000:length(unique(sort(step5_df$std_word)))]

step5_df$std_word[step5_df$std_word == "perform"] = "perform_entertain"
step5_df$std_word[step5_df$std_word == "gang_criminal_organization_maffia"] = "gang_criminal_organization_mafia"
step5_df$std_word[step5_df$std_word == "monk_priest"] = "monk_priest_devotee"
step5_df$std_word[step5_df$std_word == "name"] = "JUST_NAME"
step5_df$std_word[step5_df$std_word == "land_realm"] = "worlds_realm_land_world"

#write to file

write.csv(step5_df, "step7_5_1.csv")