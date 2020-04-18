#Jane Austen vs John Muir. Each 400 lines x 1000 words divided into 100 documents.
#so each document 4000 words and total 200 documents
#use tm's dtm

#set working directory and load package tm
setwd(dirname(file.choose()))
getwd()
library(tm)

#input data and form two dataframes
dfVictorianEraAA <- read.table('Gungor_2018_VictorianAuthorAttribution_data_train.csv', header = TRUE, sep = (','))
dfJA26674_27073 <- dfVictorianEraAA[26674:27073,]
dfJM31420_31819 <- dfVictorianEraAA[31420:31819,]

#form corpa with dataframes. Texts already cleaned
#package tm is required
JA26674_27073_corpus <- VCorpus(VectorSource(dfJA26674_27073$text))
JA26674_27073_corpus <- tm_map(JA26674_27073_corpus, stripWhitespace)
JM31420_31819_corpus <- VCorpus(VectorSource(dfJM31420_31819$text))
JM31420_31819_corpus <- tm_map(JM31420_31819_corpus, stripWhitespace)

#form dtm. Each line(1000 words) a document
#change minimum word length to 1 from 3
JA26674_27073_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(JA26674_27073_corpus, control=list(wordLengths = c(1, Inf)))))
JM31420_31819_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(JM31420_31819_corpus, control=list(wordLengths = c(1, Inf)))))

#retain columns of words which can found both from JA and JM's texts
common_cols <- intersect(colnames(JA26674_27073_dtDf), colnames(JM31420_31819_dtDf))
JAAndJMdtDf <- rbind(JA26674_27073_dtDf[common_cols], JM31420_31819_dtDf[common_cols])

#further retain columns of words each of which are at least appeared 400 times 0.05%
#(i.e. 0.05% of the total number of words: 800000)  
JAAndJMdtDf400OrMore <- JAAndJMdtDf[, colSums(JAAndJMdtDf) >=400]#231 columns

#delete single character columns (but not m: reason: i'm/ t: can't/ etc.)
JAAndJMdtDf400OrMore[ ,c('â', 'e', 'f', 'h', 'j', 'l', 'n', 'o', 'r', 'u', 'v')] <- list(NULL)#231 to 220

#delete this for loop method
##a function for summing a fixed number of lines of a document term df
#dfEach4RWdNoOfOccu = function (dtDf, noOfRowsADoc) {
#     for (i in 1: nrow(dtDf)) {
#         if (i%%noOfRowsADoc == 1) {
#             df <- dtDf[i,]
#         }
#         else if (i%%noOfRowsADoc != 0) {
#             df <- rbind(df, dtDf[i,])
#         }
#         else {
#             if ( i == noOfRowsADoc) {
#                 z = colSums(rbind(df, dtDf[i,]))
#             }
#             else {
#                 df <- colSums(rbind(df, dtDf[i,]))
#                 z = rbind(z, df)
#             }
#         }
#     }
#     return(z)
# }
##use the above function, number of lines is 4. add label JA and JM 
#dfJaAndJmWdFeqDf <- dfEach4RWdNoOfOccu(JAAndJMdtDf400OrMore, 4)
#dfJaAndJmWdFeqDf <- as.data.frame(dfJaAndJmWdFeqDf)

#aggreate and sum every four lines
JAAndJMdtDf400OrMore$textNo <- rep(1:200, each = 4)
dfJaAndJmWdFeqDf <- aggregate(. ~ textNo, JAAndJMdtDf400OrMore, sum)
dfJaAndJmWdFeqDf$textNo <- NULL

#add labels JA and JM
dfJaAndJmWdFeqDf$JAOrJM <- c(rep('JA', 100), rep('JM', 100))
dfJaAndJmWdFeqDfLabled <- dfJaAndJmWdFeqDf[,c(221,1:220)]

#shuffling rows:
set.seed(12345)
rrowNos <- sample(nrow(dfJaAndJmWdFeqDfLabled))
dfJaAndJmWdFeqDfLabledRandm <- dfJaAndJmWdFeqDfLabled[rrowNos,]#ok

#normalising columns
data_norm <- function(x) {(x- min(x))/ (max(x)- min(x))}
dfJaAndJmWdFeqDfLabledRandm_norm <- as.data.frame(lapply(dfJaAndJmWdFeqDfLabledRandm[,-1], data_norm))#ok

#KNN!
library(class)
dfJaAndJmWdFeqDfLabledRandm_norm_train <- dfJaAndJmWdFeqDfLabledRandm_norm[1:160,]
dfJaAndJmWdFeqDfLabledRandm_norm_test <- dfJaAndJmWdFeqDfLabledRandm_norm[161:200,]
JaOrJm_pred <- knn(dfJaAndJmWdFeqDfLabledRandm_norm_train, dfJaAndJmWdFeqDfLabledRandm_norm_test, dfJaAndJmWdFeqDfLabledRandm[1:160,1], k= 13)
#show cross table
table(JaOrJm_pred, dfJaAndJmWdFeqDfLabledRandm[161:200,1])
