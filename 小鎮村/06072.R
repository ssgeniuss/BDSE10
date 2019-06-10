library(jsonlite)
library(dplyr)
FF <-fromJSON("C:/Users/USER/Desktop/FemaleKeyWord.json")


fill_season <- function(x){
  if(x=='春'){
    season1 <- 1
  }else if(x=='夏'){
    season1 <- 2
  }else if(x=='秋'){
    season1 <- 3
  }else{
    season1 <- 4
  }
  return(season1)
}

FF$s111 <- sapply(FF$Season, FUN = fill_season)

a=FF %>%
  select(Year, s111, c6,Area) %>% 
  group_by(s111, Year) %>%
  summarise(total = sum(c6))

a=a[order(a$Year),]
write.csv(a,"C:/Users/USER/Desktop/a.csv",row.names = F)
