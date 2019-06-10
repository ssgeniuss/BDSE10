# 累累累
# 欄位切割
dcard_area <- cSplit(dcard_area, "Time_1", "-")
# column重新命名
colnames(dcard_area) <- c("University","Content","Department","Gender","ID","Title","Tag","laugh","cry","like",    
                          "angry","跪","驚訝","Time","Area" ,     
                          "City","Year","Month","Date")


# 選出有巧克力的文章
choco_text <- dcard_area[grepl("巧克力", dcard_area$Content) == TRUE,]
# 欄位切割
library(splitstackshape)
choco_text <- cSplit(choco_text, "Time_1", "-")
# column重新命名
colnames(choco_text) <- c("University","Content","Department","Gender","ID","Title","Tag","laugh","cry","like",    
                          "angry","跪","驚訝","Time","Area" ,     
                          "City","Year","Month","Date")

# 添加一季節欄位
fill_season <- function(x) {
  if (x %in% c(3,4,5)) {
    season <- "春"
  } else if (x %in% c(6,7,8)){
    season <- "夏"
  } else if (x %in% c(9,10,11)){
    season <- "秋"
  } else {
    season <- "東"
  }
  return(season)
}

# 複製月份欄位
dcard_area$Season <- dcard_area$Month
# 對月份欄位進行 fill_season
dcard_area$Season <- sapply(dcard_area$Month, FUN = fill_season)

# Write JSON
write_json(dcard_area,"/Users/lialee/Desktop/dcard_final.json")

library(dplyr)
library(ggplot2)

dcard_area$ID <- as.factor(dcard_area$ID)


dcard_area %>% 
  select(ID, Season, like, Gender) %>% 
  filter(Season %in% c("春","夏"), like > 12000) %>% 
  ggplot(aes(x = Season, y= like, fill = Gender)) +
  geom_bar(stat = "identity") +
  coord_flip()+
  #labs(x = "hahaha",
  #y = "idddd",
  #title = "this is a book") +
  theme_bw() +
  theme(text = element_text(family = "Heiti TC Light"))


dcard_area$Year <- as.numeric(dcard_area$Year)

dcard_area %>% 
  select(Season, like, Year) %>% 
  group_by(Season, Year) %>% 
  summarise(total = sum(like)) %>% 
  ggplot(aes(x = Year, y= total, color=Season)) +
  geom_line()+
  theme(text = element_text(family = "Heiti TC Light"))
