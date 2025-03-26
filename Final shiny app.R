library(rvest)
library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(readr)
library(robotstxt)
library(tidyr)

# Scrape data of current season from https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures
# previous season 2022-2023 https://fbref.com/en/comps/9/2022-2023/schedule/2022-2023-Premier-League-Scores-and-Fixtures
print(paths_allowed('https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures'))

page <- read_html('https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures')
# page <- read_html('https://fbref.com/en/comps/9/2022-2023/schedule/2022-2023-Premier-League-Scores-and-Fixtures')

# get the score
scores <- page %>%
  html_nodes('.center a')%>%
  html_text()
# fill in the entry of the games yet to kick off
# scores[length(scores)+1:(380-length(scores))]= 'Await kick off'

# get the home and away team names
teams <- page %>%
  html_nodes('.right+ .left a , .right a')%>%
  html_text()%>%
  matrix(ncol = 2, byrow = T)%>%
  .[1:length(scores),] # Keep the number of rows the same
colnames(teams) <- c('Home','Away')
teams[teams == "Nott'ham Forest"] <- 'Nottingham Forest'
# The kick off date
dates <- page %>%
  html_nodes('.left:nth-child(3) a')%>%
  html_text()%>%
  .[1:length(scores)] # Keep the number of rows the same

basic_data <- as.data.frame(cbind(dates,teams,scores))

# to get more performance data, we need to get the url for each game report
links <- page %>%
  html_nodes('.center a')%>%
  html_attr('href')
domain_url <- c('https://fbref.com')
# Add the domain
whole_urls <- links%>%
  paste(domain_url,.,sep = '')

# loop for all the match report url to get the performance data
data_performance <- data.frame()
for (g in 1:length(scores)) {
  st_list <- list()
  # request every 10 seconds to relieve the pressure on server (make sure won't be blocked)
  Sys.sleep(5)
  # loop for home and away team data
  for (i in 1:2) {
    url <- whole_urls[g]
    summary_stat <- curl::curl(url) %>%
      xml2::read_html() %>%
      rvest::html_nodes('table') %>%
      rvest::html_table() %>%
      .[[ifelse(i==1,4,11)]] #(4,11) is the number of table in the website
    
    # Gls(Goals scored or allowed) Sh(Total shots) SoT(shots on target) Touches Cmp(passes completed)
    # Cmp% (pass completion) Carries (the number of times players control the ball with feet)
    # TkAt Succ (take on attempt and success) SCA(shot-creating actions)
    
    # Create new column names
    newnames <- summary_stat[1, ]
    # avoid replicates of Att
    newnames[length(newnames)-1] <- 'TkAT'
    # get the index of categories of interest
    index_in <- match(c('Gls','Sh','SoT','Touches','Cmp%','Carries','Succ'),newnames)
    # Add team name
    colnames(summary_stat) <- newnames
    # the performance data of interest for one team
    stats <- summary_stat[dim(summary_stat)[1],index_in]
    stats$Team <- paste(teams[g,i],ifelse(i==1,'Home','Away'),sep = '.')
    stats$Matchindex <- g
    # combine it into the basic_data
    data_performance <- rbind(data_performance,stats)
  }
}


# Cleaning basic_data

# Split 'scores' into separate columns ('Home_Score' and 'Away_Score')

basic_data <- basic_data %>%
  separate(scores, into = c("Home_Score", "Away_Score"), sep = "–")

# Normalize team names
basic_data$Home <- gsub("Nott'ham Forest", "Nottingham Forest", basic_data$Home)
# Add similar substitutions for other inconsistent team names if needed

# Cleaning data_performance

# Convert character columns to numeric types
num_cols <- c("Gls", "Sh", "SoT", "Touches", "Cmp%", "Carries", "Succ")
data_performance[num_cols] <- lapply(data_performance[num_cols], as.numeric)

# Separate 'Team' column into 'Team' and 'Home/Away' columns
data_performance <- data_performance %>%
  separate(Team, into = c("Team", "Home_Away"), sep = "\\.")

# Cleaning summary_stat
# Remove the first row (column description row)
summary_stat <- summary_stat[-1, ]

# Convert character columns to numeric types
num_cols_summary <- c("Min", "Gls", "Ast", "PK", "PKatt", "Sh", "SoT", "CrdY", "CrdR", "Touches", "Tkl", "Int", "Blocks", "xG", "npxG", "xAG", "SCA", "GCA", "Cmp", "Att", "PrgP", "Carries", "PrgC", "TkAT", "Succ")
summary_stat[num_cols_summary] <- lapply(summary_stat[num_cols_summary], function(x) as.numeric(as.character(x)))

# Convert 'Age' to a consistent format or extract age values
# Example: Extract age values (assuming it's in the format 'XX-XXX' with XX representing years and XXX representing days)
summary_stat$Age <- as.integer(sub("-(.*)", "", summary_stat$Age))


# Save the data frames as RDS files
saveRDS(basic_data, "basic_data_cleaned.RDS")
saveRDS(data_performance, "data_performance_cleaned.RDS")
saveRDS(summary_stat, "summary_stat_cleaned.RDS")


head(basic_data)
# g=1
# game_data_MC <- substr(MC_urls[g], start = 39, stop = nchar(MC_urls[g])-15)
# 
# ## Format the date with same length but day is a little tricky, I will leave it to the next step
# # day_o <- paste('0',1:9,sep = '')
# # day_p <- as.character(1:9)
# for (ii in 1:12){
#    game_data_MC <- str_replace(game_data_MC, month.name[ii], month.abb[ii])
# }
# ## for this step, we can include the dash but it will be removed in the following step
# date <- substr(game_data_MC, nchar(game_data_MC)-10, nchar(game_data_MC))
# date <- ifelse(substr(date,1,1) =='-',substr(date,2,nchar(date)),date)
# nchar(date)
# teams <- substr(game_data_MC, 1, nchar(game_data_MC)-nchar(date)-1)
# derby_name <- c('North-West-London-Derby-','Merseyside-Derby-','North-London-Derby-','Manchester-Derby-',
#                 'North-West-Derby-')
# team_name <- c('Manchester-United','Manchester-City','Leeds-United','Crystal-Palace','Leicester-City',
#                'Aston-Villa','Norwich-City','Newcastle-United','Wolverhampton-Wanderers',
#                'West-Ham-United','Brighton-and-Hove-Albion','Tottenham-Hotspur')
# team_name_re <- str_replace_all(team_name,'-',' ')
# team_name_re[9] <- 'Wolves'
# team_name_re[11] <- 'Brighton'
# for (ii in 1:length(team_name_re)){
#   teams <- str_replace(teams,team_name[ii],team_name_re[ii])
# }
# teamA <- sub("-.*", "", teams)
# teamB <- sub(".*-", "", teams)

## Read data



