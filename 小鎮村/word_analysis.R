# - https://medium.com/@pblcollect/r%E8%AA%9E%E8%A8%80%E6%96%87%E5%AD%97%E6%8E%A2%E5%8B%98-%E8%AA%9E%E6%84%8F%E5%88%86%E6%9E%90applying-on-mooc-%E5%AF%A6%E4%BD%9C-9a3e14523998

library(jiebaR)
library(stringr)
library(jsonlite)

install.packages("Rwordseg",repos = "http://R-Forge.R-project.org")
library(Rwordseg) 
library(rJava) 
library(tm) 
library(slam) 
library(topicmodels) 
library(igraph)

seg_words <- sapply(dcard$Content, segmentCN)
doc.list <- strsplit(as.character(seg_words), split=" ")

dg.corpus <- gsub("'", "", doc.list)
dg.corpus <- gsub("[[:punct:]]", " ", dg.corpus) 
dg.corpus <- gsub("[[:cntrl:]]", " ", dg.corpus) 
dg.corpus <- gsub("^[[:space:]]+", "", dg.corpus) 
dg.corpus <- gsub("[[:space:]]+$", "", dg.corpus)
dg.corpus <- gsub("[[0-9]]", " ", dg.corpus)
dg.corpus <- gsub('[a-zA-Z]', "", dg.corpus)
dg.corpus <- gsub('#', "", dg.corpus)
dg.corpus <- gsub('[ ️   ︎   ﹏ ︵ ︶ ︿ ﹃ ꒳]' ,"", dg.corpus)
dg.corpus <- gsub('[︴ ︹ ︺ ꒦ ꒪ ꒫"]' ,"",dg.corpus)
wordcorpus <- VCorpus(VectorSource(dg.corpus))

tdm <- TermDocumentMatrix(wordcorpus, control = list(wordLengths = c(2, Inf)))
tdm.tfidf <- weightTfIdf(tdm, normalize = T) 
# 向量記憶體已用完 17858344 bytes
dtm <- as.matrix(tdm.tfidf)
v <- sort(rowSums(dtm), decreasing = T) 
d <- data.frame(word=names(v),tfidf=v)

# - - - - - 奇異值分解的語意分析技術
res<- svd(tdm.tfidf) 
nrow(res$u)
ncol(res$v)
datau<- data.frame(res$u[,2:3]) 
datav<- data.frame(res$v[,2:3])
p<-ggplot()+geom_point(data= datav, aes(X1, X2), size=2, color ='red')+geom_text(data= datav, aes(X1, X2),label=1:5000, vjust=1.5)+theme_bw()
print(p)

# Working Directory
dcard 

# 看個欄位型別
sapply(dcard, typeof)
sapply(dcard, summary)
Sys.time()

# as_datetime(dcard$Time[1],origin = "1970-01-01", tz = "CST")
library(listviewer)
topics <- toJSON(dcard$Topic, auto_unbox = T)
topics
jsonedit(topics)

# combine words
library(stringr)
str_c(dcard$Topic[[1]], collapse = "")

dcard_content <- dcard[,c(1,3,10)]
dim(dcard_content)

M_post <- sum(dcard_content$Gender == 'M')
F_post <- sum(dcard_content$Gender == 'F')
post_sum <- M_post + F_post
M_post_ratio <- M_post/post_sum #30.4%
F_post_ratio <- F_post/post_sum #69.5%

# 發現性別有四種"M" "F" "D" "" 
unique(dcard_content$Gender)

dcard_M <-  dcard_content[dcard_content$Gender == 'M',]
dcard_F <-  dcard_content[dcard_content$Gender == 'F',]

stop_word <- '/Users/lialee/Desktop/Programming/TextMining/Data/stop.txt'
user_dic <- "/Users/lialee/Desktop/foodclean.csv"

mixseg <- worker(stop_word = stop_word, user = user_dic, type = "tag")

# 測試文本集路径
telibrAd <- paste(wd,'/data/textClassification/',chineseType,'/test',sep = '')

# ---------------------------------------------------------------------------
# 訓練文本集

# - - - 全部男女文章
trlib_M <- data.frame()
trlib_F <- data.frame()

# - - - 愛心數區分的男女文章
total_M <- data.frame()
total_F <- data.frame()

counter <- 41659
counter <- 2000

seq_doc <- NULL # Word Segmentation Results
seq_tag <- NULL # POS Tagging Results

top1000_M <- sort(dcard_M$愛心, decreasing = TRUE)[1:1000]
index_M <- which(dcard_M$愛心 %in% top1000_M)
dcard_M_1000 <- dcard_M[index_M,]

top1000_F <- sort(dcard_F$愛心, decreasing = TRUE)[1:1000]
index_F <- which(dcard_F$愛心 %in% top1000_F)
dcard_F_1000 <- dcard_F[index_F,]

# 愛心數top100    
k = dcard_M_1000
k = dcard_F_1000
k = dcard_M
k = dcard_F

#去除数字
k <- gsub('[0-9]+', "", k)
k <- gsub('[[:space:]]', "", k)
k <- gsub('[a-zA-Z]', "", k)
k <- gsub('#', "", k)
k <- gsub('[ ️   ︎   ﹏ ︵ ︶ ︿ ﹃ ꒳]',"",k)
k <- gsub('[︴ ︹ ︺ ꒦ ꒪ ꒫"]' ,"",k)

# 中文分词
w <- segment(as.vector(k), mixseg)

seq_doc <- c(seq_doc, w)

# 词性标注
t <- names(tagging(as.vector(k), mixseg))

seq_tag <- c(seq_tag , t) 

# 分词 & 词性 Print
print(paste(str_c(w, t, sep="|"), collapse = "  "))

# 分词 & 词性 Save
seq <- data.frame(seq_doc, seq_tag) 

table(seq$seq_tag)
#词性过滤 (Just Reserve Noun)
seq <- seq[seq$seq_tag %in% c('n','nr','nrt','ns','nt','nz'),]

#统计词频
seq_doc <- table(as.character(seq$seq_doc))

#文本名、类别
seq_doc <- data.frame(seq_doc, clas = 'M')
seq_doc <- data.frame(seq_doc, clas = 'F')

trlib_M <- rbind(trlib_M, seq_doc)
trlib_F <- rbind(trlib_F, seq_doc)
tribe <- rbind(trlib_F, trlib_M)

total_M <- rbind(total_M, seq_doc)
total_F <- rbind(total_F, seq_doc)
gender_tribe <- rbind(total_F, total_M)

# trlib <- tbl_df(trlib) 
names(tribe)[1] <- 'Keywords'
names(tribe)[2] <- 'Frequency'

DF <- c(table(tribe$Keywords))

FM <- unique(tribe$clas)

install.packages("reshape2")
library(reshape2)

TCM <- acast(tribe, Keywords ~ clas, value.var='Frequency', fill = 0, drop = FALSE, sum)
TCB <- ifelse(TCM > 0, 1, 0)

selectedKW <- rowSums(TCM) >= 1000

TCM <- as.data.frame(TCM[selectedKW,])
TCB <- as.data.frame(TCB[selectedKW,])

DF <- DF[selectedKW]

IDF <- log10(counter / DF)
cbind(rownames(TCM), IDF)

TTF <- colSums(TCM)

TCM_IDF <- t(t(TCM) / TTF) * IDF

TCM <- data.frame(Keywords = rownames(TCM), TCM)
rownames(TCM) <- NULL

TCM_IDF <- data.frame(Keywords = rownames(TCM_IDF), TCM_IDF)
rownames(TCM_IDF) <- NULL

TCB <- data.frame(Keywords = rownames(TCB), TCB)
rownames(TCB) <- NULL

# 測試詞彙是否存在
("不要巧克力" %in% TCM$Keywords) 

# 训练资料词向量矩阵
colnam <- TCM$Keywords
TCM$Keywords <- NULL
TCM <- as.data.frame(t(TCM))
colnames(TCM) <- colnam
rownames(TCM) <- c("F","M")
TCM$clas <- FM

# Build CART Model without Pruning
install.packages("rpart")
library(rpart)
library(rpart.plot)

CART.tree <- rpart(clas ~ ., data=TCM, 
                   control=rpart.control(minsplit=2, cp=0))
rpart.plot(CART.tree)

write_json(dcard ,"/Users/lialee/Desktop/dcard_new.json")


# 選出有巧克力的文章
choco_text <- dcard[grepl("巧克力", dcard$Content) == TRUE,]

stop_word <- '/Users/lialee/Desktop/Programming/TextMining/Data/stop.txt'
user_dic <- "/Users/lialee/Desktop/foodclean.csv"

mixseg <- worker(stop_word = stop_word, user = user_dic, type = "tag")

choco <- data.frame()
seq_doc <- NULL 
seq_tag <- NULL 

k <- choco_text
k <- gsub('[0-9]+', "", k)
k <- gsub('[[:space:]]', "", k)
k <- gsub('[a-zA-Z]', "", k)
k <- gsub('#', "", k)
k <- gsub('[ ️   ︎   ﹏ ︵ ︶ ︿ ﹃ ꒳]',"",k)
k <- gsub('[︴ ︹ ︺ ꒦ ꒪ ꒫"]',"",k)
# 中文分词
w <- segment(as.vector(k), mixseg)
seq_doc <- c(seq_doc, w)

# 詞性標注
t <- names(tagging(as.vector(k), mixseg))
seq_tag <- c(seq_tag , t) 

# 分词 & 词性 Save
seq <- data.frame(seq_doc, seq_tag) 

# 詞性過濾 (Just Reserve Noun)
seq <- seq[seq$seq_tag %in% c('n','nr','nrt','ns','nt','nz'),]

# 詞頻
seq_doc <- as.data.frame(table(as.character(seq$seq_doc)))

choco <- rbind(choco, seq_doc)

names(choco)[1] <- 'Keywords'
names(choco)[2] <- 'Frequency'

# 有巧克力的文章中，選出有蛋糕或甜點的文章 ＃403則都有蛋糕
choco_cake <- choco_text[grepl(c("蛋糕","甜點"), choco_text$Content)== TRUE,]

# 有巧克力的文章中，選出有蛋糕或甜點的文章 ＃85則有蛋糕又有甜點
sum(grepl("甜點", choco_cake$Content)== TRUE)

# 有巧克力的文章中，選出有餅乾或食譜的文章 ＃250則都有餅乾
choco_cookie <- choco_text[grepl(c("餅乾","食譜"), choco_text$Content)== TRUE,]

# 有巧克力的文章中，選出有餅乾和食譜的文章 ＃23則有餅乾又有食譜
sum(grepl("食譜", choco_cookie$Content)== TRUE)
