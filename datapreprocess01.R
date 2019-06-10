library(stringr)
library(jsonlite)
library(jiebaR)
library(dplyr)
library(ggplot2)


dcard_with_city <- fromJSON("/Users/derek/Desktop/小鎮村/dcard_with_city_and_area.json")
dcard_final <- fromJSON("/Users/derek/Desktop/小鎮村/dcard_final.json")
food_mix <- read.csv("/Users/derek/Desktop/小鎮村/this_food_mix.csv")
View(head(dcard_lemoncake,400))
class(seq_doc)
length(seq_doc)
dcard_final$Season <- str_replace_all(dcard_final$Season,"東","冬")

dcard_F <- dcard_final[dcard_final$Gender == 'F',]
dcard_M <- dcard_final[dcard_final$Gender == 'M',]

dcard_cheesecake <- dcard_F[grepl("乳酪蛋糕", dcard_F$Content)== TRUE,]
dcard_milktea <- dcard_F[grepl("鮮奶茶", dcard_F$Content)== TRUE,]
dcard_lemoncake <- dcard_F[grepl("檸檬塔", dcard_F$Content)== TRUE,]
dcard_friedchicken <- dcard_F[grepl("酥雞", dcard_F$Content)== TRUE,]
dcard_mushroom <- dcard_F[grepl("杏鮑菇", dcard_F$Content)== TRUE,]
dcard_eggcake <- dcard_F[grepl("雞蛋糕", dcard_F$Content)== TRUE,]

#關鍵字詞出現次數
seq_doc <- dcard_eggcake[,c(2,5)]

for(i in length(seq_doc$Content)){
  wordfeq <- str_count(seq_doc$Content[i], c("雞蛋糕"))
  seq_doc <- cbind(seq_doc,wordfeq)
}

test <- right_join(dcard_eggcake,seq_doc, by = "ID")
test <- test[,-21]
colnames(test)[2] <- c("Content")
dcard_eggcake <- test

#作圖
dcard_eggcake %>%
  select(Season, like, Area, City, Year, Feq=wordfeq) %>%
  filter(Area != "NA") %>%
  ggplot(aes(x = Year, y= Feq, fill = Area)) +
  geom_bar(stat = "identity") +
  #coord_flip()+
  #labs(x = "hahaha",
  #y = "idddd",
  #title = "this is a book") +
  theme_bw() +
  theme(text = element_text(family = "Heiti TC Light"))

dcard_eggcake %>% 
  select(Season, Area, City,wordfeq, Year) %>% 
  group_by(Year,Season) %>% 
  summarise(Feq = sum(wordfeq)) %>% 
  ggplot(aes(x = Year, y= Feq, color = Season)) +
  geom_line()+
  theme(text = element_text(family = "Heiti TC Light"))

