head(NBA_Shot_Locations_1997_2020)
NBA_Shot_Locations_1997_2020 %>%
filter(`Team Name` == "Los Angeles Lakers" & `Player Name`== "Dwight Howard") 

NBA_Shot_Data<- 
NBA_Shot_Locations_1997_2020 %>%
filter(`Game Date` >= "20101020" & `Shot Zone Basic` != "Backcourt") %>%
select( `Player Name`,`Team Name`:`Minutes Remaining`,`Action Type`:`Away Team`)

## output final file
write.csv(NBA_Shot_Data,"NBA_Shot_Data.csv")