#Three popular female novelists all born in the 1850s: 17 Helen Mathers 1853-1920 (18010- 18669 in the kaggle csv file), 32 Lucas Malet 1852-1931 (33861-34563), 33 Marie Corelli 1855-1924 (34564-36305)
#200 lines each
#there is a â in the code. If this code is loaded to RStudio, the encoding of which should be changed to UTF-8

#set working directory and load package tm
setwd(dirname(file.choose()))
getwd()
library(tm)

#input data and form three dataframes
#be careful of the correctness of the filenames
#for example, hyphen or underscore?
dfVictorianEraAA <- read.table('Gungor_2018_VictorianAuthorAttribution_data_train.csv', header = TRUE, sep = (','))
dfHelen_Mathers18009_18208 <- dfVictorianEraAA[18009:18208,]
dfLucas_Malet33860_34059 <- dfVictorianEraAA[33860:34059,]
dfMarie_Corelli34563_34762 <- dfVictorianEraAA[34563:34762,]

#form corpa with dataframes. Texts already cleaned
#package tm is required
dfHelen_Mathers18009_18208_corpus <- VCorpus(VectorSource(dfHelen_Mathers18009_18208$text))
dfHelen_Mathers18009_18208_corpus <- tm_map(dfHelen_Mathers18009_18208_corpus, stripWhitespace)
dfLucas_Malet33860_34059_corpus <- VCorpus(VectorSource(dfLucas_Malet33860_34059$text))
dfLucas_Malet33860_34059_corpus <- tm_map(dfLucas_Malet33860_34059_corpus, stripWhitespace)
dfMarie_Corelli34563_34762_corpus <- VCorpus(VectorSource(dfMarie_Corelli34563_34762$text))
dfMarie_Corelli34563_34762_corpus <- tm_map(dfMarie_Corelli34563_34762_corpus, stripWhitespace)

#form dtm. Each line(1000 words) a document
#change minimum word length to 1 from 3
dfHelen_Mathers18009_18208_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfHelen_Mathers18009_18208_corpus, control=list(wordLengths = c(1, Inf)))))
dfLucas_Malet33860_34059_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfLucas_Malet33860_34059_corpus, control=list(wordLengths = c(1, Inf)))))
dfMarie_Corelli34563_34762_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfMarie_Corelli34563_34762_corpus, control=list(wordLengths = c(1, Inf)))))

#retain columns of words which can found both in HM, LM and MC's texts
common_cols <- intersect(intersect(colnames(dfHelen_Mathers18009_18208_dtDf), colnames(dfLucas_Malet33860_34059_dtDf)), colnames(dfMarie_Corelli34563_34762_dtDf))
HmLmMcDtDf <- rbind(dfHelen_Mathers18009_18208_dtDf[common_cols], dfLucas_Malet33860_34059_dtDf[common_cols], dfMarie_Corelli34563_34762_dtDf[common_cols])#5228 cols

#further retain columns of words each of which are at least appeared
#300 times 0.05%
HmLmMcTtl300OrMore <- HmLmMcDtDf[, colSums(HmLmMcDtDf) >=300] #237
#why number of occurrence of â so high? HM 2077, LM 1743, MC 6280
HmLmMcTtl300OrMore$â <- NULL #236

#aggreate and sum every four lines (reduced to 150 lines)
#add and delete column textNO
HmLmMcTtl300OrMore$textNo <- rep(1:150, each = 4)
dfHmLmMcWdFeqDf <- aggregate(. ~ textNo, HmLmMcTtl300OrMore, sum)
dfHmLmMcWdFeqDf$textNo <- NULL

#add labels HM, LM and MC and put the column to the front
dfHmLmMcWdFeqDf$HmOrLmOrMc <- c(rep('HM', 50), rep('LM', 50), rep('MC', 50))
dfHmLmMcWdFeqDfLabled = dfHmLmMcWdFeqDf[,c(237,1:236)] #236+1

#shuffling rows:
set.seed(12345)
rrowNos <- sample(nrow(dfHmLmMcWdFeqDfLabled))
dfHmLmMcWdFeqDfLabledRandm <- dfHmLmMcWdFeqDfLabled[rrowNos,]

#normalisation
data_norm <- function(x) {(x- min(x))/ (max(x)- min(x))}
dfHmLmMcWdFeqDfLabledRandm_norm <- as.data.frame(lapply(dfHmLmMcWdFeqDfLabledRandm[,-1], data_norm))
summary(dfHmLmMcWdFeqDfLabledRandm_norm[,1:4]) #see whether normalised

#KNN!
library(class)
dfHmLmMcWdFeqDfLabledRandm_norm_train <- dfHmLmMcWdFeqDfLabledRandm_norm[1:120,]
dfHmLmMcWdFeqDfLabledRandm_norm_test <- dfHmLmMcWdFeqDfLabledRandm_norm[121:150,]
HmOrLmOrMc_pred <- knn(dfHmLmMcWdFeqDfLabledRandm_norm_train, dfHmLmMcWdFeqDfLabledRandm_norm_test, dfHmLmMcWdFeqDfLabledRandm[1:120,1], k= 11)
table(pred = HmOrLmOrMc_pred, true_HelenMathers_LucasMalet_MarieCorelli_KNN = dfHmLmMcWdFeqDfLabledRandm[121:150,1]) #all correct
#confusion tables see photos. sqrt(120) = 10.954 . Therefore use k =11. 
#k = 11 perform the best, only one error: 1 MC was misjudged as LM

#SVM! simple: no tunning
library("e1071")
HmOrLmOrMc_svm_model <- svm(dfHmLmMcWdFeqDfLabledRandm_norm_train, dfHmLmMcWdFeqDfLabledRandm[1:120,1], type = 'C')
pred <- predict(HmOrLmOrMc_svm_model, dfHmLmMcWdFeqDfLabledRandm_norm_test)
table(pred, true_HelenMathers_LucasMalet_MarieCorelli_SVM = dfHmLmMcWdFeqDfLabledRandm[121:150,1])
#all correct

#tune to find optimal costs
dfHmLmMcWdFeqDfLabledRandm1To120AsFactors = as.factor(dfHmLmMcWdFeqDfLabledRandm[1:120,1])
set.seed(12345)
svm_tune <- tune(svm, train.x = dfHmLmMcWdFeqDfLabledRandm_norm_train,
						train.y = dfHmLmMcWdFeqDfLabledRandm1To120AsFactors,
						kernel = 'linear',
						#type = 'C',
						ranges = list(cost = c(.001,.01,.1,1,5,10,100)))
print(svm_tune) #no sufficient information. Just provide the best cost 
# use svm$best.model
#besides best cost, also best number of support vectors, etc.
pred_svm_after_tune <- predict(svm_tune$best.model, dfHmLmMcWdFeqDfLabledRandm_norm_test)
table(pred = pred_svm_after_tune, true_HelenMathers_LucasMalet_MarieCorelli_TunedSVM = dfHmLmMcWdFeqDfLabledRandm[121:150,1])

