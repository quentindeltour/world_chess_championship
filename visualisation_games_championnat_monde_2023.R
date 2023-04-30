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

# Dataframe to add labels of playes with colors
round_colors = unique(combined_df[, .(white,black, move_number=max(move_number)/3), by=round])
round_colors[, y := 3]
round_colors[, white:=paste("\u2654", white)]
round_colors[, black:=paste("\u265A", black)]

# Dataframe to add result
round_result = combined_df[, .(round, resultat, cp_white, move_number, white, black)]
round_result[, winner := ifelse(resultat=="1/2-1/2", "Match Nul", ifelse(resultat=="1-0", paste("Victoire", white, sep = "\n"), paste("Victoire", black, sep = "\n")))]
round_result = round_result[, .SD[c(.N)], by=round]
round_result[, nudge_y := ifelse(resultat=="0-1", 2, -2)]
round_result[, round_number_result := paste(round, winner)]
round_result[, round := factor(round, levels=c("Ronde 1", "Ronde 2", "Ronde 3", "Ronde 4", "Ronde 5", "Ronde 6", "Ronde 7", "Ronde 8", "Ronde 9", "Ronde 10", "Ronde 11", "Ronde 12", "Ronde 13", "Ronde 14", "Ronde 15", "Ronde 16", "Ronde 17", "Ronde 18"))]

combined_df <- merge(combined_df, round_result[, .(round, round_number_result)], by="round")

#Plot
ggplot(combined_df, aes(x=move_number/2, y = cp_white/100)) +
  ggh4x::stat_difference(aes(ymin = 0, ymax = cp_white/100), show.legend = F, levels = c("-", "+"), alpha = 0.8) +
  scale_fill_manual(values = c("#EEEED2", 'black', "blue")) +
  geom_line(aes(y = cp_white/100)) +
  labs(fill = NULL) + 
  theme_minimal() + 
  xlab("Numéro de coup") + ylab("Evaluation (Sotckfish 14)") + 
  facet_wrap(~round, scales = "free_x") + 
  geom_text(aes(move_number/2, y, label=white),
            data=round_colors) +
  geom_text(aes(move_number/2, -y, label=black),
            data=round_colors) +
  geom_label_repel(aes(move_number/2, cp_white/100, label = winner), data=round_result, nudge_y = -2, nudge_x=15)+
  coord_cartesian(clip = "off") + 
  theme(
    strip.text.x = element_text(
      size = 10, face = "bold.italic"
    ))

#Save last plot to png
ggsave("./output/games_championnat_monde_23.png", width = 1200, height = 675, units = "px", bg = "white", scale = 3)
