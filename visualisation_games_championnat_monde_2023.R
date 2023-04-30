library(jsonlite)
library(data.table)
library(ggplot2)
library(ggh4x)
library(ggrepel)
library(ggthemes)

# read in the nested json file
json_data <- jsonlite::fromJSON("./data/games_championnat_monde_echecs_2023.json")

create_datatable_moves <- function(data_to_pass_to_dataframe){
  if (!length(data_to_pass_to_dataframe)) {
    
  }
  else {
    dd  <-  as.data.frame(matrix(unlist(data_to_pass_to_dataframe), nrow=length(unlist(data_to_pass_to_dataframe[1]))))
    setDT(dd)
    
    dd <- transpose(dd, keep.names = "col")
    setnames(dd, c("id", "move_number", "move", "turn", "cp_white"))
    dd <- dd[, move_number:=as.integer(move_number)]
    dd <- dd[, cp_white:=as.integer(cp_white)]
    return(dd)
  }
}

# access the nested data

list_rounds = list()
top_level_keys <- names(json_data)
for (i in seq_along(top_level_keys)) {
  key <- top_level_keys[i]
  data <- json_data[[key]]
  
  # extract the second level data and create a data.table
  second_level_data <- data$moves
  dt <- create_datatable_moves(second_level_data)
  setDT(dt)
  dt[, round := data$round]
  dt[, white := data$white]
  dt[, black := data$black]
  dt[, resultat := data$resultat]
  dt[, type_game:= data$type_game]
  # assign the data.table in our list
  list_rounds[[i]] = dt
}

# Concatenate the data frames in the list
combined_df <- setDT(rbindlist(list_rounds, fill=T))
combined_df[, round:=factor(round, levels = gsub("round_", "", top_level_keys))]
combined_df = combined_df[!resultat=="*"]
combined_df[cp_white > 600, cp_white:=600]
combined_df[cp_white < -600, cp_white:=-600]
combined_df[, round := paste0(round," (", resultat, ")")]
combined_df[, round := factor(round, levels=c("Ronde 1 (1/2-1/2)", "Ronde 2 (0-1)", "Ronde 3 (1/2-1/2)", "Ronde 4 (1-0)", "Ronde 5 (1-0)", "Ronde 6 (1-0)", "Ronde 7 (1-0)", "Ronde 8 (1/2-1/2)", "Ronde 9 (1/2-1/2)", "Ronde 10 (1/2-1/2)", "Ronde 11 (1/2-1/2)", "Ronde 12 (1-0)", "Ronde 13 (1/2-1/2)", "Ronde 14 (1/2-1/2)", "Ronde 15 (1/2-1/2)", "Ronde 16 (1/2-1/2)", "Ronde 17 (1/2-1/2)", "Ronde 18 (0-1)"))]

# Dataframe to add labels of playes with colors
# round_colors = unique(combined_df[, .(white,black, move_number=max(move_number)/3), by=round])
# round_colors[, y := 3]
# round_colors[, white:=paste("\u26AA", white)]
# round_colors[, black:=paste("\u26AB", black)]

# Dataframe to add result
round_result = combined_df[, .(resultat, cp_white, move_number, white, black, type_game, name_move_numer=max(move_number)/3), by=round]
round_result[, winner := ifelse(resultat=="1/2-1/2", "Match Nul", ifelse(resultat=="1-0", paste("Victoire", white, sep = "\n"), paste("Victoire", black, sep = "\n")))]
round_result[, white_result := ifelse(resultat=="1/2-1/2", "Draw", ifelse(resultat=="1-0", "Win", "Lose"))]
round_result[, black_result := ifelse(resultat=="1/2-1/2", "Draw", ifelse(resultat=="1-0", "Lose", "Win"))]

round_result = round_result[, .SD[c(.N)], by=round]
round_result[, nudge_y := ifelse(resultat=="0-1", 2, -2)]
# round_result[, round_number_result := paste(round, winner)]
round_result[, round := factor(round, levels=c("Ronde 1 (1/2-1/2)", "Ronde 2 (0-1)", "Ronde 3 (1/2-1/2)", "Ronde 4 (1-0)", "Ronde 5 (1-0)", "Ronde 6 (1-0)", "Ronde 7 (1-0)", "Ronde 8 (1/2-1/2)", "Ronde 9 (1/2-1/2)", "Ronde 10 (1/2-1/2)", "Ronde 11 (1/2-1/2)", "Ronde 12 (1-0)", "Ronde 13 (1/2-1/2)", "Ronde 14 (1/2-1/2)", "Ronde 15 (1/2-1/2)", "Ronde 16 (1/2-1/2)", "Ronde 17 (1/2-1/2)", "Ronde 18 (0-1)"))]
round_result[, white:=paste0("\u26AA ", white, ' (', white_result, ')')]
round_result[, black:=paste0("\u26AB ", black, ' (', black_result, ')')]
round_result[, name_y:=4]


# combined_df <- merge(combined_df, round_result[, .(round, round_number_result)], by="round")

#Plot
ggplot(combined_df[type_game=="classical"], aes(x=move_number/2, y = cp_white/100)) +
  ggh4x::stat_difference(aes(ymin = 0, ymax = cp_white/100), show.legend = F, levels = c("-", "+"), alpha = 0.8) +
  scale_fill_manual(values = c("#EEEED2", 'black', "blue")) +
  geom_line(aes(y = cp_white/100)) +
  labs(fill = NULL) + 
  theme_minimal() + 
  xlab("Numéro de coup") + ylab("Evaluation (Sotckfish 14)") + 
  facet_wrap(~round, scales = "free_x") + 
  geom_text(aes(name_move_numer/2, name_y, label=white),
            data=round_result[type_game=="classical"]) +
  geom_text(aes(name_move_numer/2, -name_y, label=black),
            data=round_result[type_game=="classical"]) +
  # geom_label_repel(aes(move_number/2, cp_white/100, label = winner), data=round_result, nudge_y = -2, nudge_x=15)+
  coord_cartesian(clip = "off") + 
  theme(
    strip.text.x = element_text(
      size = 10, face = "bold.italic"
    )) + 
  ggtitle("Championnat du Monde 2023 - Classical Games : Ian Nepomniachtchi 7-7 Ding Liren")

#Save last plot to png
ggsave("./output/classical_games_championnat_monde_23.png", width = 1200, height = 675, units = "px", bg = "white", scale = 3)


# Tiebreaks

round_result[, name_y:=2]


#Plot
ggplot(combined_df[type_game=="tiebreak"], aes(x=move_number/2, y = cp_white/100)) +
  ggh4x::stat_difference(aes(ymin = 0, ymax = cp_white/100), show.legend = F, levels = c("-", "+"), alpha = 0.8) +
  scale_fill_manual(values = c("#EEEED2", 'black', "blue")) +
  geom_line(aes(y = cp_white/100)) +
  labs(fill = NULL) + 
  theme_minimal() + 
  xlab("Numéro de coup") + ylab("Evaluation (Sotckfish 14)") + 
  facet_wrap(~round, scales = "free_x", ncol = 2) + 
  geom_text(aes(name_move_numer/2, name_y, label=white),
            data=round_result[type_game=="tiebreak"]) +
  geom_text(aes(name_move_numer/2, -name_y, label=black),
            data=round_result[type_game=="tiebreak"]) +
  # geom_label_repel(aes(move_number/2, cp_white/100, label = winner), data=round_result, nudge_y = -2, nudge_x=15)+
  coord_cartesian(clip = "off") + 
  theme(
    strip.text.x = element_text(
      size = 10, face = "bold.italic"
    )) + 
  ggtitle("Championnat du Monde 2023 - Tiebreaks Games")

#Save last plot to png
ggsave("./output/tiebreaks_games_championnat_monde_23.png", width = 1200, height = 675, units = "px", bg = "white", scale = 3)
