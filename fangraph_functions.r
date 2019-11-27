

# Required Packages -------------------------------------------------------

library("rvest")

# Load Name CSV -----------------------------------------------------------

player_names_and_ids <- read.csv("C:/Users/Nicholas/Documents/MLB_Player_Names_and_IDs.csv", stringsAsFactors = FALSE)

get_fangraphs_id <- function(last, first = NULL, team_abr = NULL, team_name = NULL, pos = NULL){
  
  player_id <- player_names_and_ids[grepl(paste(last), player_names_and_ids$mlb_name), 'fg_id']
  
  return(player_id)
}


# Fangraphs Functions -----------------------------------------------------


f_name <- function(player_id){
  
  url <- paste("https://www.fangraphs.com/statss.aspx?playerid=", player_id, sep = "")
  
  player_name <- url %>%
    xml2::read_html() %>%
    html_nodes("[class='player-info-box-name']") %>%
    html_node("h1") %>%
    html_text()
  
  return(player_name)
}

f_dash <- function(player_id){
  
  url <- paste("https://www.fangraphs.com/statss.aspx?playerid=", player_id, sep = "")
  
  b_table_list <- url %>%
    xml2::read_html() %>%
    html_nodes(xpath='//*[@id="SeasonStats1_dgSeason11_ctl00"]') %>%
    html_table()
  
  b_table_df <- b_table_list[[1]]
  
  b_table_df$`K%` <- as.numeric(gsub('%', "", b_table_df$`K%`))/100
  b_table_df$`BB%` <- as.numeric(gsub('%', "", b_table_df$`BB%`))/100
  b_table_df <- b_table_df[!grepl("Postseason", b_table_df$Season), ]
  b_table_df <- b_table_df[!grepl("Total", b_table_df$Season), ]
  b_table_df$Season <- as.numeric(b_table_df$Season)
  
  b_table_df <- b_table_df[!duplicated(b_table_df[c('Season', 'Team')]), ]
  rownames(b_table_df) <- NULL
  
  return(b_table_df)
}

f_adv <- function(player_id){
  
  url <- paste("https://www.fangraphs.com/statss.aspx?playerid=", player_id, sep = "")
  
  b_table_list <- url %>%
    xml2::read_html() %>%
    html_nodes(xpath='//*[@id="SeasonStats1_dgSeason2_ctl00"]') %>%
    html_table()
  
  b_table_df <- b_table_list[[1]]
  
  b_table_df$`K%` <- NULL
  b_table_df$`BB%` <- NULL
  b_table_df$AVG <- NULL
  b_table_df$OBP <- NULL 
  b_table_df$SLG <- NULL
  b_table_df$ISO <- NULL
  b_table_df$BABIP <- NULL
  b_table_df$`wRC+` <- NULL
  b_table_df$wOBA <- NULL
  b_table_df <- b_table_df[!grepl("Postseason", b_table_df$Season), ]
  b_table_df <- b_table_df[!grepl("Total", b_table_df$Season), ]
  b_table_df$Season <- as.numeric(b_table_df$Season)
  
  b_table_df <- b_table_df[!duplicated(b_table_df[c('Season', 'Team')]), ]
  rownames(b_table_df) <- NULL
  
  return(b_table_df)
}

f_bat_ball <- function(player_id){
  
  url <- paste("https://www.fangraphs.com/statss.aspx?playerid=", player_id, sep = "")
  
  b_table_list <- url %>%
    xml2::read_html() %>%
    html_nodes(xpath='//*[@id="SeasonStats1_dgSeason3_ctl00"]') %>%
    html_table()
  
  b_table_df <- b_table_list[[1]]
  
  b_table_df <- b_table_df[!grepl("Postseason", b_table_df$Season), ]
  b_table_df <- b_table_df[!grepl("Total", b_table_df$Season), ]
  b_table_df$Season <- as.numeric(b_table_df$Season)
  
  b_table_df$`LD%` <- as.numeric(gsub('%', "", b_table_df$`LD%`))/100
  b_table_df$`GB%` <- as.numeric(gsub('%', "", b_table_df$`GB%`))/100
  b_table_df$`FB%` <- as.numeric(gsub('%', "", b_table_df$`FB%`))/100
  b_table_df$`IFFB%` <- as.numeric(gsub('%', "", b_table_df$`IFFB%`))/100
  b_table_df$`HR/FB` <- as.numeric(gsub('%', "", b_table_df$`HR/FB`))/100
  b_table_df$`IFH%` <- as.numeric(gsub('%', "", b_table_df$`IFH%`))/100
  b_table_df$`BUH%` <- as.numeric(gsub('%', "", b_table_df$`BUH%`))/100
  b_table_df$`Pull%` <- as.numeric(gsub('%', "", b_table_df$`Pull%`))/100
  b_table_df$`Cent%` <- as.numeric(gsub('%', "", b_table_df$`Cent%`))/100
  b_table_df$`Oppo%` <- as.numeric(gsub('%', "", b_table_df$`Oppo%`))/100
  b_table_df$`Soft%` <- as.numeric(gsub('%', "", b_table_df$`Soft%`))/100
  b_table_df$`Med%` <- as.numeric(gsub('%', "", b_table_df$`Med%`))/100
  b_table_df$`Hard%` <- as.numeric(gsub('%', "", b_table_df$`Hard%`))/100
  
  b_table_df <- b_table_df[!duplicated(b_table_df[c('Season', 'Team')]), ]
  rownames(b_table_df) <- NULL
  
  return(b_table_df)
}

f_more_bat_ball <- function(player_id){
  
  url <- paste("https://www.fangraphs.com/statss.aspx?playerid=", player_id, sep = "")
  
  b_table_list <- url %>%
    xml2::read_html() %>%
    html_nodes(xpath='//*[@id="SeasonStats1_dgSeason4_ctl00"]') %>%
    html_table()
  
  b_table_df <- b_table_list[[1]]
  
  b_table_df <- b_table_df[!grepl("Postseason", b_table_df$Season), ]
  b_table_df <- b_table_df[!grepl("Total", b_table_df$Season), ]
  b_table_df$Season <- as.numeric(b_table_df$Season)
  
  b_table_df <- b_table_df[!duplicated(b_table_df[c('Season', 'Team')]), ]
  rownames(b_table_df) <- NULL
  
  return(b_table_df)
}

f_data_merge <- function(df1, df2, df3, df4){
  
  merge_df1 <- merge(df1, df2, by = c('Season', 'Team'), all.x = TRUE)
  merge_df2 <- merge(merge_df1, df3, by = c('Season', 'Team'), all.x = TRUE)
  merge_df3 <- merge(merge_df2, df4, by = c('Season', 'Team'), all.x = TRUE)
  
  return(merge_df3)
}

fangraphs_get_data <- function(player_id){
  
  dash_df <- f_dash(player_id)
  adv_df <- f_adv(player_id)
  bat_ball_df <- f_bat_ball(player_id)
  more_bat_ball_df <- f_more_bat_ball(player_id)
  
  fangraph_df <- f_data_merge(dash_df, adv_df, bat_ball_df, more_bat_ball_df)
  
  return(fangraph_df)
}

