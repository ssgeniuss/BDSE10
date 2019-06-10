library(jsonlite)
dcard_area <-fromJSON("C:/Users/USER/Desktop/FemaleKeyWord.json")

library(dplyr)
library(ggplot2)
dcard_area$ID <- as.factor(dcard_area$ID)
FF_index <- which(dcard_area$Gender == "F")
MM_index <- which(dcard_area$Gender == "M")

F_data = dcard_area[FF_index, ]
M_data = dcard_area[MM_index, ]

#-----------------------------F--------------------

library(stringr)
F_data$chocolate=as.integer(str_count(F_data$Content,"巧克力"))
F_data$mocha=as.integer(str_count(F_data$Content,"抹茶"))
F_data$cheese=as.integer(str_count(F_data$Content,"起司"))
F_data$noodle=as.integer(str_count(F_data$Content,"麵"))
F_data$butter=as.integer(str_count(F_data$Content,"奶油"))
F_data$strawberry=as.integer(str_count(F_data$Content,"草莓"))
F_data$chicken=as.integer(str_count(F_data$Content,"雞"))
F_data$milktea=as.integer(str_count(F_data$Content,"奶茶"))
F_data$coffee=as.integer(str_count(F_data$Content,"咖啡"))
F_data$c1=as.integer(str_count(F_data$Content,"乳酪蛋糕"))
F_data$c2=as.integer(str_count(F_data$Content,"鮮奶茶"))
F_data$c3=as.integer(str_count(F_data$Content,"檸檬塔"))
F_data$c4=as.integer(str_count(F_data$Content,"酥雞"))
F_data$c5=as.integer(str_count(F_data$Content,"杏鮑菇"))
F_data$c6=as.integer(str_count(F_data$Content,"雞蛋糕"))

write_json(F_data ,"C:/Users/USER/Desktop/FemaleKeyWord.json")


dcard_area %>% 
  select(Season, like, Year) %>% 
  group_by(Season, Year) %>% 
  summarise(total = sum(like))





a=F11 %>%
  select(Year, s111, chocolate,Area) %>% 
  group_by(s111, Year) %>%
  summarise(total = sum(chocolate))

a=a[order(a$Year),]
