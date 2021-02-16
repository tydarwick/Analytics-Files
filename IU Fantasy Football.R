# ggplot2 examples
library(ggplot2) 
library(readr)
library(dplyr)
library(jsonlite)
library(purrr)
library(tidyr)

#Pull in League Info
base = "http://fantasy.espn.com/apis/v3/games/ffl/seasons/"
year = "2020"
mid = "/segments/0/leagues/"
leagueID = "197350"
tail = "?view=mDraftDetail&view=mLiveScoring&view=mMatchupScore&view=mPendingTransactions&view=mPositionalRatings&view=mSettings&view=mTeam&view=modular&view=mNav&view=mMatchupScore"
url = paste0(base,year,mid,leagueID,tail)

#Convert from JSON to List
ESPNGet <- httr::GET(url = url)
ESPNGet$status_code

ESPNRaw <- rawToChar(ESPNGet$content)
ESPNFromJSON <- jsonlite::fromJSON(ESPNRaw)

#Join teams and members tables
Teams <- ESPNFromJSON$teams %>%
left_join(ESPNFromJSON$members, by = c("primaryOwner"="id"))

# Create DF for Team Records and stats
TeamRecords = 
  tibble(
    location = Teams$location,
    nickname = Teams$nickname,
    first = Teams$firstName,
    last = Teams$lastName,
    teamID = Teams$id,
    wins = Teams$record$overall$wins,
    losses = Teams$record$overall$losses,
    Win_Percentage = paste(round(Teams$record$overall$percentage*100,2),"%"),
    Points_For = Teams$record$overall$pointsFor,
    Points_Against = Teams$record$overall$pointsAgainst
) %>%
unite(Team, c(location, nickname), sep = " ") %>%
unite(Owner, c(first,last),sep= " ")
TeamRecords[order(TeamRecords$wins,TeamRecords$Points_For,decreasing=TRUE),]

#Create DF for Season Schedule and Winnners
Schedule =
  tibble(
    winning_team = ESPNFromJSON$schedule$winner,
    Week = ESPNFromJSON$schedule$matchupPeriodId,
    AwayTeam = ESPNFromJSON$schedule$away$teamId,
    AwayPoints = ESPNFromJSON$schedule$away$totalPoints,
    HomeTeam = ESPNFromJSON$schedule$home$teamId,
    HomePoints = ESPNFromJSON$schedule$home$totalPoints
  ) %>%
  left_join(TeamRecords %>% select(teamID, Team), by = c("AwayTeam" = "teamID")) %>%
  select(-AwayTeam) %>%
  rename(Away_Team = Team) %>%
  left_join(TeamRecords %>% select(teamID, Team), by = c("HomeTeam" = "teamID")) %>%
  select(-HomeTeam) %>%
  rename(Home_Team = Team) %>%
  left_join(TeamRecords %>% select(Team, Owner), by = c("Away_Team" = "Team")) %>%
  rename(Away_Owner = Owner) %>%
  left_join(TeamRecords %>% select(Team, Owner), by = c("Home_Team" = "Team")) %>%
  rename(Home_Owner = Owner) %>%
  mutate(Winning_Points= ifelse(HomePoints>AwayPoints,HomePoints,AwayPoints)) %>% #winning team points
  mutate(Losing_Points= ifelse(HomePoints>AwayPoints,AwayPoints,HomePoints)) %>% #losing team points
  mutate(Winner = ifelse(winning_team=="HOME",Home_Owner,Away_Owner)) %>% #Winner (persons name)
  mutate(Loser = ifelse(winning_team=="HOME",Away_Owner,Home_Owner)) %>% #Loser (persons name)
  mutate(Winning_Team = ifelse(winning_team=="HOME",Home_Team,Away_Team)) %>% #Winning Team Name
  mutate(Losing_Team = ifelse(winning_team=="HOME",Away_Team,Home_Team)) %>% #Losing Team Name
  rename(Home_Road=winning_team) %>%
  filter(Home_Road!="UNDECIDED")%>% #filter out games that haven't been decided yet
  select(Week,Winning_Points:Losing_Team) %>%
  mutate(Matchup_ID=row_number()) #create unique matchup ID
  
    
#List Datasets and export to Excel with each DF on separate sheet
League_Data<- list("Records"=TeamRecords,"Schedule"=Schedule)
write.xlsx(League_Data,"IU_League.xlsx")