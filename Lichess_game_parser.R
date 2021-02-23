library(data.table)
library(tidyverse)
library(lubridate)
library(doParallel)
library(foreach)
library(foreach)
library(tictoc)
options(scipen=999)
rm(list = ls())


### This function extracts information from the annotated moves of each game

process_moves <- function(moves) {
  
  indiv_move_long = moves[, strsplit(V1, "[[:digit:]]*\\.{1,3}\\s"), by = .(GAME)][V1 != ""]
  indiv_move_long[, move:= gsub("(\\S*).*", "\\1", V1)]
  indiv_move_long[grepl("eval", V1), eval:= gsub(".*%eval\\s(.*?)\\].*", "\\1", V1)]
  indiv_move_long[grepl("clk", V1), clocktime:= gsub(".*%clk\\s(.*?)\\].*", "\\1", V1)]
  
  #When the evaluation is missing the game is over
  
  #indiv_move_long[, clocktime := hms(clocktime)]
  indiv_move_long[, V1:= NULL]
  indiv_move_long[, white_move := seq_len(.N)%%2==1, by = .(GAME)]
  
  # Converting the evaluation into something numeric
  # Mate is converted to a really high evaluation 
  
  indiv_move_long[grepl("(#-)", eval), eval_num:= -200]
  indiv_move_long[grepl("(#\\d)", eval), eval_num:= 200]
  #indiv_move_long[grepl("\\d-\\d", eval), eval_num:= -as.numeric(str_sub(eval, -1))*1000] 
  
  #stopifnot(!grepl("1/2-1/2", indiv_move_long$eval))
  indiv_move_long[is.na(eval_num), eval_num := as.numeric(eval)]
  
  # Using the special symbols for assessing the moves 
  # ?? blunder, ? mistake, ?! inaccuracy 
  
  indiv_move_long[, blunder := grepl("\\?\\?$", move), by = .(GAME)]
  indiv_move_long[, mistake := grepl("[^\\?]\\?$", move), by = .(GAME)]
  indiv_move_long[, inaccuracy := grepl("\\?!", move), by = .(GAME)]
  indiv_move_long[, inferior_move := grepl("\\?", move), by = .(GAME)]
  
  # Converting move time to percent and calculating the percentage of total time the move took
  indiv_move_long[, clocktime_sec := period_to_seconds(hms(clocktime))]
  indiv_move_long[, prev_ct_sec := shift(clocktime_sec, 2, type = "lag"), by = .(GAME)]
  #indiv_move_long[, c("clocktime", "prev_ct") := lapply(.(clocktime, prev_ct), hms)]
  indiv_move_long[, move_time_pct := (prev_ct_sec - clocktime_sec)/clocktime_sec[2- .N %% 2], by = .(GAME)]
  indiv_move_long[, time_scramble := clocktime_sec <= 0.1*clocktime_sec[2- .N %% 2], by = .(GAME)]
  
  
  
  # Evaluation brackets
  indiv_move_long[between(eval_num, -1, 1) == T, eval_bracket := "About equal"]
  indiv_move_long[eval_num > 1, eval_bracket := "White is winning"]
  indiv_move_long[eval_num < -1, eval_bracket := "Black is winning"]
  
  # Game flips. If the evaluation bracket changes is a flip unless the actual evaluation does not change by more than 0.5 
  indiv_move_long[!is.na(eval_bracket), game_flip := c(0, diff(rleid(eval_bracket))), by = .(GAME)]
  indiv_move_long[!is.na(eval_num) & abs(eval_num - shift(eval_num, 1, type = "lag")) < 0.5 & game_flip == 1, game_flip := 0, by = .(GAME)]
  
  #indiv_move_long[, switch := !sign(eval_num * shift(eval_num, 1, type = "lag"))]
  
  indiv_move_long[, .(Total_moves = .N, 
                        Black_blunders = sum(blunder[white_move == F], na.rm = T),
                        White_blunders = sum(blunder[white_move == T], na.rm = T),
                        Black_mistakes = sum(mistake[white_move == F], na.rm = T),
                        White_mistakes = sum(mistake[white_move == T], na.rm = T),
                        Black_inaccuracies = sum(inaccuracy[white_move == F], na.rm = T),
                        White_inaccuracies = sum(inaccuracy[white_move == T], na.rm = T),
                        Black_inferior_moves = sum(inferior_move[white_move == F], na.rm = T),
                        White_inferior_moves = sum(inferior_move[white_move == T], na.rm = T),
                        Black_ts_moves = sum(time_scramble[white_move == F], na.rm = T),
                        White_ts_moves = sum(time_scramble[white_move == T], na.rm = T),
                        Black_ts_blunders = sum(blunder[white_move == F & time_scramble == T], na.rm = T),
                        White_ts_blunders = sum(blunder[white_move == T & time_scramble == T], na.rm = T),
                        Black_ts_mistakes = sum(mistake[white_move == F & time_scramble == T], na.rm = T),
                        White_ts_mistake = sum(mistake[white_move == T & time_scramble == T], na.rm = T),
                        Black_long_moves = sum(white_move == F & move_time_pct >= 0.1, na.rm = T),
                        White_long_moves = sum(white_move == T & move_time_pct >= 0.1, na.rm = T),
                        Black_bad_long_moves = sum(white_move == F & move_time_pct >= 0.1 & inferior_move == T, na.rm = T),
                        White_bad_long_moves = sum(white_move == T & move_time_pct >= 0.1 & inferior_move == T, na.rm = T),
                        Game_flips = sum(game_flip, na.rm = T),
                        Game_flips_ts = sum(game_flip[time_scramble == T], na.rm = T)), by = .(GAME)]
  
}


process_game <- function(info){
  
  info[, "Var_name" := gsub("\\[([[:alpha:]]*)\\s.*", "\\1", V1)]
  info[, "Var_value" := gsub("\\[[[:alpha:]]*\\s(.*)\\]", "\\1", V1)]
  
  
  info_wide = data.table::dcast(info, GAME ~ Var_name, value.var = "Var_value")
  rm(info)
  
  
  numeric_cols = c("BlackElo", "BlackRatingDiff", "WhiteElo", "WhiteRatingDiff")
  info_wide[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
  
  
  info_wide[BlackElo <= 1900, Black_elo_category:= "Low rating"]
  info_wide[data.table::between(BlackElo, 1901, 2400), Black_elo_category:= "High rating"]
  info_wide[BlackElo >= 2401, Black_elo_category:= "GM rating"]
  
  info_wide[WhiteElo <= 1900, White_elo_category:= "Low rating"]
  info_wide[data.table::between(WhiteElo, 1901, 2400), White_elo_category:= "High rating"]
  info_wide[WhiteElo >= 2401, White_elo_category:= "GM rating"]
  
  
  info_wide[, c("starting_time", "increment") := tstrsplit(TimeControl, "\\+")]
  info_wide[, starting_time := as.numeric(starting_time)] #The nas here are the correspondence games
  info_wide[, increment := as.numeric(increment)]
  
  
  info_wide[starting_time <= 120, Game_type := "Bullet"]
  info_wide[data.table::between(starting_time, 121, 599), Game_type := "Blitz"]
  info_wide[data.table::between(starting_time, 600, 900), Game_type := "Rapid"]
  info_wide[starting_time > 900, Game_type := "Classical"]
  info_wide[is.na(starting_time), Game_type := "Correspondence"]
  
  info_wide[increment >= 120, Game_type := "Classical"] ## If you are getting 2 minutes or move per move, this increases rapidly
  
  return(info_wide)
}




con = file("lichess_db_standard_rated_2020-09.pgn.bz2", "r")

## The file with the data is too big for me to read in memory. Therefore I read it in chuncks of 3000000 lines at a time via a connection

result_list = list()
lines_read = lines_expected = 3000000
counter = 1

while (lines_read == lines_expected) {
  temp = data.table(read.csv(con, nrows = lines_read, stringsAsFactors = F, header = F, colClasses = "character", fill = F))
  lines_read = nrow(temp)
  temp[, GAME := cumsum(grepl("\\[Event", V1))]
  temp[, has_eval := any(grepl("%eval", V1)), by = GAME]
  temp = if(temp[.N, grepl("}", V1)]) temp else temp[GAME != max(temp$GAME)] # Remove the last game if it is not full information
  temp = temp[GAME != 0] # Remove the first game for the same reason
  temp = temp[has_eval == T]
  temp[, has_eval := NULL]

  
  moves = temp[grepl("^1\\.", V1), .(V1, GAME)]
  info = temp[!grepl("^1\\.", V1)]
  info = info[!grepl("^\\[White\\s|^\\[Black\\s|^\\[Round\\s|^\\[UTCDate\\s|WhiteTitle|BlackTitle", V1)]
  
  rm(temp)
  
  comb = merge(process_game(info), process_moves(moves), by = "GAME")
  result_list[[counter]] = comb
  
  print(paste("Read", counter*lines_read, "lines"))
  counter = counter + 1
  
}



analysis_result = rbindlist(result_list)


saveRDS(analysis_result, "Sept_20_analysis.RDS")
fwrite(analysis_result, "Sept_20_analysis.csv")
