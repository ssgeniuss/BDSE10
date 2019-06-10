library(stringr)

food01 <- read.csv("/Users/derek/Desktop/小鎮村/food01.csv")
food02 <- read.csv("/Users/derek/Desktop/小鎮村/food02.csv")
food03 <- read.csv("/Users/derek/Desktop/小鎮村/food03.csv")
food04 <- read.csv("/Users/derek/Desktop/小鎮村/food04.csv")
food05 <- read.csv("/Users/derek/Desktop/小鎮村/food05.csv")
food06 <- read.csv("/Users/derek/Desktop/小鎮村/food06.csv")
food07 <- read.csv("/Users/derek/Desktop/小鎮村/food07.csv")
foodclean07 <- read.csv("/Users/derek/Desktop/小鎮村/foodclean07.csv")


this_food <- read.csv("/Users/derek/Desktop/小鎮村/this_food.csv")
food_mix <- read.csv("/Users/derek/Desktop/小鎮村/this_food_mix.csv")



View(head(test,10000))
length(this_food$term)

food01_trim <- str_trim(food01$煙燻雞腿)
test <- food_mix
test1 <- test
test <- test1
test <- str_replace_all(test$term,"引起唾液收斂的","")
test1 <- cSplit(test$term," ")
head(foodclean07,100)
foodclean07 <- foodclean07[,2]
class(test)
length(table_dup07)
table(test$test)
View(head(table(food07$雞肉河粉),20))

test <- test[-105:-120,]
View(head(test,1000))
test <- as.data.frame(test)
table_dup07 <- test[!duplicated(test$test),]
View(head(table_dup07))


write.csv(foodclean07,file = "/Users/derek/Desktop/小鎮村/foodclean07_1.csv", row.names = F)


