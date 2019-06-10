library(jiebaR)
library(jiebaRD)
library(stringr)

# 繁简体
chineseType <- 'traditionalChinese'

# Working Directory
wd <- '/Users/derek/Dropbox/Big Data/data mining/TextMining'

#可自行維護
user_dic <- '/Users/derek/Desktop/小鎮村/foodclean07_1.csv'
stop_word <- paste(wd,'/Data/stop.txt', sep = '')

#以worker這個函數建立分詞系統
mixseg <- worker(stop_word = stop_word, user = user_dic, type = "tag")#"tag"表示之後出來的分詞系統要連詞性標注都要有

# 训练文本集路径
trlibrAd <- '/Users/derek/Desktop/小鎮村/0_1000.json'

# 测试文本集路径
telibrAd <- paste(wd,'/data/textClassification/',chineseType,'/test',sep = '')

# ---------------------------------------------------------------------------
# 训练文本集处理
trlib <- data.frame()

counter <- 1 #表示現在要讀第一篇數據進來

#利用兩層for迴圈針對每個目錄下的每個檔案做分詞及詞性標注的動作
for(i in dir(trlibrAd))
{
  # 类别路径
  classAd <- paste(trlibrAd, '/', i, sep = '')
  
  for(j in dir(classAd))
  {
    # 文档路径
    docAd <- paste(classAd, '/', j, sep = '')
    
    # 文档读取
    doc <- readLines(docAd, encoding = 'UTF-8')
    print(docAd)
    
    seq_doc <- NULL # Word Segmentation Results(斷詞)
    seq_tag <- NULL # POS Tagging Results(詞性)
    
    for(k in 1:length(doc))
    {
      # 中文分词
      w <- segment(as.vector(doc[k]), mixseg)
      
      seq_doc <- c(seq_doc, w)
      
      # 词性标注
      t <- names(tagging(as.vector(doc[k]), mixseg))
      
      seq_tag <- c(seq_tag , t) 
      
      # 分词 & 词性 Print
      print(paste(str_c(w, t, sep="|"), collapse = "  "))
    }
    
    print('-----------------------------------------------------------------')
    
    # 分词 & 词性 Save
    seq <- data.frame(seq_doc, seq_tag) 
    
    #词性过滤 (Just Reserve Noun)
    seq <- seq[seq_tag=='n'|seq_tag=='nr'|seq_tag=='nrt'|seq_tag=='ns'|seq_tag=='nt'|seq_tag=='nz',]
    
    #统计词频
    seq_doc <- table(as.character(seq$seq_doc))
    
    #去除数字
    seq_doc <- seq_doc[!grepl('[0-9]+', names(seq_doc))]
    
    #文本名、类别
    seq_doc <- data.frame(seq_doc, textName = j, clas = i)
    
    trlib <- rbind(trlib, seq_doc)
    
    counter <- counter + 1
  }
}

#文章總數，之後要算DF or IDF
counter <- counter - 1
    
# trlib <- tbl_df(trlib) 
names(trlib)[1] <- 'Keywords'
names(trlib)[2] <- 'Frequency'

DF <- c(table(trlib$Keywords))

FM <- unique(trlib[c("textName", "clas")])

library(reshape2)
#要將關鍵字出現在各文章檔案中的頻率列出
TCM <- acast(trlib, Keywords ~ textName, value.var='Frequency', fill=0, drop=FALSE, sum)
TCB <- ifelse(TCM>0, 1, 0)#將上述出現次數改成有或沒有(1 or 0)

#設定總次數大於2次者保留
selectedKW <- rowSums(TCM) >= 2

TCM <- as.data.frame(TCM[selectedKW,])
TCB <- as.data.frame(TCB[selectedKW,])

DF <- DF[selectedKW]

IDF <- log10(counter / DF)
cbind(rownames(TCM), IDF)

TTF <- colSums(TCM)#每篇文章出現關鍵字的總次數

TCM_IDF <- t(t(TCM) / TTF) * IDF

TCM <- data.frame(Keywords = rownames(TCM), TCM)
rownames(TCM) <- NULL

TCM_IDF <- data.frame(Keywords = rownames(TCM_IDF), TCM_IDF)
rownames(TCM_IDF) <- NULL

TCB <- data.frame(Keywords = rownames(TCB), TCB)
rownames(TCB) <- NULL

# ---------------------------------------------------------------------------
# 测试资料处理
telib <- data.frame()

counter <- 1

for(i in dir(telibrAd))
{
  # 类别路径
  classAd <- paste(telibrAd, '/', i, sep = '')
  
  for(j in dir(classAd))
  {
    # 文档路径
    docAd <- paste(classAd, '/', j, sep = '')
    
    # 文档读取
    doc <- readLines(docAd, encoding = 'UTF-8')
    print(docAd)
    
    seq_doc <- NULL # Word Segmentation Results
    seq_tag <- NULL # POS Tagging Results
    
    for(k in 1:length(doc))
    {
      # 中文分词
      w <- segment(as.vector(doc[k]), mixseg)
      
      seq_doc <- c(seq_doc, w)
      
      # 词性标注
      t <- names(tagging(as.vector(doc[k]), mixseg))
      
      seq_tag <- c(seq_tag , t) 
      
      # 分词 & 词性 Print
      print(paste(str_c(w, t, sep="|"), collapse = "  "))
    }
    
    print('-----------------------------------------------------------------')
    
    # 分词 & 词性 Save
    seq <- data.frame(seq_doc, seq_tag) 
    
    #词性过滤 (Just Reserve Noun)
    seq <- seq[seq_tag=='n'|seq_tag=='nr'|seq_tag=='nrt'|seq_tag=='ns'|seq_tag=='nt'|seq_tag=='nz',]
    
    #统计词频
    seq_doc <- table(as.character(seq$seq_doc))
    
    #去除数字
    seq_doc <- seq_doc[!grepl('[0-9]+', names(seq_doc))]
    
    #文本名、类别
    seq_doc <- data.frame(seq_doc, textName = j, clas = i)
    
    telib <- rbind(telib, seq_doc)
    
    counter <- counter + 1
  }
}

counter <- counter - 1

names(telib)[1] <- 'Keywords'
names(telib)[2] <- 'Frequency'

DF <- c(table(telib$Keywords))

FM <- unique(telib[c("textName", "clas")])

library(reshape2)

eTCM <- acast(telib, Keywords ~ textName, value.var='Frequency', fill=0, drop=FALSE, sum)
eTCB <- ifelse(eTCM>0, 1, 0)

selectedKW <- rowSums(eTCM) >= 2

eTCM <- as.data.frame(eTCM[selectedKW,])
eTCB <- as.data.frame(eTCB[selectedKW,])

DF <- DF[selectedKW]

IDF <- log10(counter / DF)
cbind(rownames(eTCM), IDF)

TTF <- colSums(eTCM)

eTCM_IDF <- t(t(eTCM) / TTF) * IDF

eTCM <- data.frame(Keywords = rownames(eTCM), eTCM)
rownames(eTCM) <- NULL

eTCM_IDF <- data.frame(Keywords = rownames(eTCM_IDF), eTCM_IDF)
rownames(eTCM_IDF) <- NULL

eTCB <- data.frame(Keywords = rownames(eTCB), eTCB)
rownames(eTCB) <- NULL

# ---------------------------------------------------------------------------
# 训练资料词向量矩阵
colnam <- TCM$Keywords #將column name取出
TCM$Keywords <- NULL #將原本資料去除column name
TCM <- as.data.frame(t(TCM)) #將資料轉置("t")
colnames(TCM) <- colnam
rownames(TCM) <- FM$textName
TCM$clas <- FM$clas


# Build CART Model without Pruning
library(rpart)
library(rpart.plot)

CART.tree <- rpart(clas ~ ., data=TCM, 
                   control=rpart.control(minsplit=2, cp=0))
rpart.plot(CART.tree)

# 测试资料词向量矩阵阵
colnam <- eTCM$Keywords
eTCM$Keywords <- NULL
eTCM <- as.data.frame(t(eTCM))
colnames(eTCM) <- colnam
rownames(eTCM) <- FM$textName
eTCM$clas <- FM$clas

# Make Predictions
CART.Prediction <- predict(CART.tree, newdata=eTCM, type='class')
cbind(CART.Prediction, predict(CART.tree, newdata=eTCM, type='prob'), eTCM$clas)

# 文本分类与评价
results <- table(Prediction=CART.Prediction, Actual=eTCM$clas)
results

Correct_Rate <- sum(diag(results)) / sum(results) 
Correct_Rate

