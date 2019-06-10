library(timeSeries)
library(dplyr)
library(ggplot2)
a1 <-read.csv("C:/Users/USER/Desktop/F關鍵字/巧克力.csv")

a1=a1[-c(1,2,3,4),]
t0=ts(a1[,3],start=c(2014,4),freq=4)
#class(t1)
t1=as.timeSeries(t0) #轉成時間序列物件
plot(t1,xlab="時間(季)", ylab="詞語出現次數",main="巧克力",lwd=1.1,col="red",xaxt ="n")

names(a1)
a1 %>% 
  select(year, season, n_Fchocolate) %>% 
  #group_by(Season, Year) %>% 
  #summarise(total = sum(like)) %>% 
  ggplot(aes(x = year, y= n_Fchocolate, color=season)) +
  geom_line()+
  theme(text = element_text(family = "Heiti TC Light"))
