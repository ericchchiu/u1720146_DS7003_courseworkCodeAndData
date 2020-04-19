#uel 19/20 DS7003
#WARNING! because there is a non-ascii character â in the code, after importing this code to RStudio, one should select File -> Reopen with encoding... then select UTF-8
#further to code DickensEliotHardyAndFourOthersDtmMethod75LinesEach.r
#here 30 texts of 3000 words (90 lines) each all for training extracted from the beginning of each author's lines for finding whether 20 texts of 3000 words (60 lines) each extracted from the end of George Eliot's lines were written by her.

#set working directory and load package tm
setwd(dirname(file.choose()))
getwd()
#time code start:
ptm <- proc.time()
library(tm)

#input data and form seven dataframes
#please pay attention to file names. e.g. hyphen or underscore?
dfVictorianEraAA <- read.table('Gungor_2018_VictorianAuthorAttribution_data_train.csv', header = TRUE, sep = (','))
dfCharles_Dickens3398_3487 <- dfVictorianEraAA[3398:3487,]
dfFergue_Hume12558_12647 <- dfVictorianEraAA[12558:12647,]
dfGeorge_Eliot13670_13759 <- dfVictorianEraAA[13670:13759,]
dfHelen_Mathers18009_18098 <- dfVictorianEraAA[18009:18098,]
dfLucas_Malet33860_33949 <- dfVictorianEraAA[33860:33949,]
dfMarie_Corelli34563_34652 <- dfVictorianEraAA[34563:34652,]
dfThomas_Hardy48023_48112 <- dfVictorianEraAA[48023:48112,]

#form corpa with dataframes. Texts already cleaned
#package tm is required here
dfCharles_Dickens3398_3487_corpus <- VCorpus(VectorSource(dfCharles_Dickens3398_3487$text))
dfCharles_Dickens3398_3487_corpus <- tm_map(dfCharles_Dickens3398_3487_corpus, stripWhitespace)
dfFergue_Hume12558_12647_corpus <- VCorpus(VectorSource(dfFergue_Hume12558_12647$text))
dfFergue_Hume12558_12647_corpus <- tm_map(dfFergue_Hume12558_12647_corpus, stripWhitespace)
dfGeorge_Eliot13670_13759_corpus <- VCorpus(VectorSource(dfGeorge_Eliot13670_13759$text))
dfGeorge_Eliot13670_13759_corpus <- tm_map(dfGeorge_Eliot13670_13759_corpus, stripWhitespace)
dfHelen_Mathers18009_18098_corpus <- VCorpus(VectorSource(dfHelen_Mathers18009_18098$text))
dfHelen_Mathers18009_18098_corpus <- tm_map(dfHelen_Mathers18009_18098_corpus, stripWhitespace)
dfLucas_Malet33860_33949_corpus <- VCorpus(VectorSource(dfLucas_Malet33860_33949$text))
dfLucas_Malet33860_33949_corpus <- tm_map(dfLucas_Malet33860_33949_corpus, stripWhitespace)
dfMarie_Corelli34563_34652_corpus <- VCorpus(VectorSource(dfMarie_Corelli34563_34652$text))
dfMarie_Corelli34563_34652_corpus <- tm_map(dfMarie_Corelli34563_34652_corpus, stripWhitespace)
dfThomas_Hardy48023_48112_corpus <- VCorpus(VectorSource(dfThomas_Hardy48023_48112$text))
dfThomas_Hardy48023_48112_corpus <- tm_map(dfThomas_Hardy48023_48112_corpus, stripWhitespace)

#form dtm. Each line(1000 words) a document
#change minimum word length to 1 from 3
dfCharles_Dickens3398_3487_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfCharles_Dickens3398_3487_corpus, control=list(wordLengths = c(1, Inf)))))
dfFergue_Hume12558_12647_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfFergue_Hume12558_12647_corpus, control=list(wordLengths = c(1, Inf)))))
dfGeorge_Eliot13670_13759_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfGeorge_Eliot13670_13759_corpus, control=list(wordLengths = c(1, Inf)))))
dfHelen_Mathers18009_18098_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfHelen_Mathers18009_18098_corpus, control=list(wordLengths = c(1, Inf)))))
dfLucas_Malet33860_33949_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfLucas_Malet33860_33949_corpus, control=list(wordLengths = c(1, Inf)))))
dfMarie_Corelli34563_34652_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfMarie_Corelli34563_34652_corpus, control=list(wordLengths = c(1, Inf)))))
dfThomas_Hardy48023_48112_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfThomas_Hardy48023_48112_corpus, control=list(wordLengths = c(1, Inf)))))

#retain columns of words which can found in every of the seven authors' texts
common_cols <- Reduce(intersect, list(colnames(dfCharles_Dickens3398_3487_dtDf), colnames(dfFergue_Hume12558_12647_dtDf), colnames(dfGeorge_Eliot13670_13759_dtDf), colnames(dfHelen_Mathers18009_18098_dtDf), colnames(dfLucas_Malet33860_33949_dtDf), colnames(dfMarie_Corelli34563_34652_dtDf), colnames(dfThomas_Hardy48023_48112_dtDf)))
BindAll7NoOfWdsInAll7UniqWdLst <- rbind(dfCharles_Dickens3398_3487_dtDf[common_cols], dfFergue_Hume12558_12647_dtDf[common_cols], dfGeorge_Eliot13670_13759_dtDf[common_cols], dfHelen_Mathers18009_18098_dtDf[common_cols], dfLucas_Malet33860_33949_dtDf[common_cols], dfMarie_Corelli34563_34652_dtDf[common_cols],
dfThomas_Hardy48023_48112_dtDf[common_cols])

#further retain columns of words each of which appearing in the texts at least
#315 times(30 x 3000 x 7 x 0.05% = 315)
SevenAuthsTtl315OrMore <- BindAll7NoOfWdsInAll7UniqWdLst[, colSums(BindAll7NoOfWdsInAll7UniqWdLst) >=315] #229

#after inspection of the dataframe,
#remove columns â, d, o (but not t(can't), m(i'm), etc.)
#be careful! â is not an ascii character
SevenAuthsTtl315OrMore[,c('â', 'd', 'o')] <- list(NULL) #226

#add textNo column
#aggreate and sum every three lines (reduce to 210 lines from 630 lines)
#delete column textNO
SevenAuthsTtl315OrMore$textNo <- rep(1:210, each = 3)
dfAll7WdFeqDf <- aggregate(. ~ textNo, SevenAuthsTtl315OrMore, sum)
dfAll7WdFeqDf$textNo <- NULL

#add labels and move the label column to the first column
dfAll7WdFeqDf$Label = c(rep('CD', 30), rep('FH', 30), rep('GE', 30), rep('HM', 30), rep('LM', 30), rep('MC', 30), rep('TH', 30)) # 226+1 = 227
dfAll7WdFeqDfLabled = dfAll7WdFeqDf[,c(227,1:226)]

# shuffling rows:
set.seed(12345)
rrowNos <- sample(nrow(dfAll7WdFeqDfLabled))
dfAll7WdFeqDfLabledRandm <- dfAll7WdFeqDfLabled[rrowNos,]

#rbind George Eliot's 20 documents extracted from her lines at end
#and the 210 documents of the seven author's texts
#(3 lines * 20 = 60 lines) with Labels GE
dfGeorge_Eliot16306_16365End <- dfVictorianEraAA[16306:16365,]
dfGeorge_Eliot16306_16365End_corpus <- VCorpus(VectorSource(dfGeorge_Eliot16306_16365End$text))
dfGeorge_Eliot16306_16365End_corpus <- tm_map(dfGeorge_Eliot16306_16365End_corpus, stripWhitespace)
dfGeorge_Eliot16306_16365End_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfGeorge_Eliot16306_16365End_corpus, control=list(wordLengths = c(1, Inf)))))
dfGeorge_Eliot16306_16365End_dtDf$textNo <- rep(1:20, each = 3)

dfGeEndWdFeqAll3000WrdsDf <- aggregate(. ~ textNo, dfGeorge_Eliot16306_16365End_dtDf, sum)
dfGeEndWdFeqAll3000WrdsDf$textNo <- NULL
dfGeEndWdFeqAll3000WrdsDf[setdiff(colnames(dfAll7WdFeqDfLabled), colnames(dfGeEndWdFeqAll3000WrdsDf))] <- 0
dfGeEndWdFeqDf <- dfGeEndWdFeqAll3000WrdsDf[colnames(dfAll7WdFeqDfLabled)]
dfGeEndWdFeqDf$Label <- rep('GE', 20)
dfAll7WdFeqDfLabledRandm <- rbind(dfAll7WdFeqDfLabledRandm, dfGeEndWdFeqDf)

#normalisation:
data_norm <- function(x) {(x- min(x))/ (max(x)- min(x))}
dfAll7WdFeqDfLabledRandm_norm <- as.data.frame(lapply(dfAll7WdFeqDfLabledRandm[,-1], data_norm))
summary(dfAll7WdFeqDfLabledRandm_norm[,1:4]) #see whether normalised

#KNN!
#number of k: usually start from sqrt of data points of
#the training set. So sqrt(210): 14
#then trail and error
library(class)
dfAll7WdFeqDfLabledRandm_norm_train <- dfAll7WdFeqDfLabledRandm_norm[1:210,]
dfAll7WdFeqDfLabledRandm_norm_test <- dfAll7WdFeqDfLabledRandm_norm[211:230,]
whthrAllEliot_pred <- knn(dfAll7WdFeqDfLabledRandm_norm_train, dfAll7WdFeqDfLabledRandm_norm_test, dfAll7WdFeqDfLabledRandm[1:210,1], k= 14)
table(whthrAllEliot_pred, dfAll7WdFeqDfLabledRandm[211:230,1])#mistake rate 9/20 very high!

#SVM!
library("e1071")
# simple: no tunning
whthrAllEliot_svm_model <- svm(dfAll7WdFeqDfLabledRandm_norm_train, dfAll7WdFeqDfLabledRandm[1:210,1], type = 'C')
pred <- predict(whthrAllEliot_svm_model, dfAll7WdFeqDfLabledRandm_norm_test)
table(pred, dfAll7WdFeqDfLabledRandm[211:230,1]) #mistake rate 4/20

#use tunning to find costs
dfAll7WdFeqDfLabledRandmLabel1To210AsFactors = as.factor(dfAll7WdFeqDfLabledRandm[1:210,1])
set.seed(12345)
svm_tune <- tune(svm, train.x = dfAll7WdFeqDfLabledRandm_norm_train,
						train.y = dfAll7WdFeqDfLabledRandmLabel1To210AsFactors,
						kernel = 'linear',
						#type = 'C',
						ranges = list(cost = c(.001,.01,.1,1,5,10,100)))
print(svm_tune) #no sufficient information. Just provide the best cost 
# use svm$best.model
#besides best cost, also best number of support vectors, etc.
pred_svm_after_tune <- predict(svm_tune$best.model, dfAll7WdFeqDfLabledRandm_norm_test)
table(pred_svm_after_tune, dfAll7WdFeqDfLabledRandm[211:230,1]) #mistake rate 3/20

#---------------------------------------------------------------
#Check whether the knn and svm models developed above (using the first 75 lines of each of the seven authors) can recognise the 20 documents (each x 3000 words) complied with George Eliot's lines extracted from end of her lines were written by her.
#need to use package tm
dfGeorge_Eliot16306_16365End <- dfVictorianEraAA[16306:16365,]
dfGeorge_Eliot16306_16365End_corpus <- VCorpus(VectorSource(dfGeorge_Eliot16306_16365End$text))
dfGeorge_Eliot16306_16365End_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfGeorge_Eliot16306_16365End_corpus, control=list(wordLengths = c(1, Inf)))))
dfGeorge_Eliot16306_16365End_dtDf$textNo <- rep(1:20, each = 3)

dfGeEndWdFeqAll3000WrdsDf <- aggregate(. ~ textNo, dfGeorge_Eliot16306_16365End_dtDf, sum)
dfGeEndWdFeqAll3000WrdsDf$textNo <- NULL
dfGeEndWdFeqAll3000WrdsDf[setdiff(colnames(dfAll7WdFeqDfLabled), colnames(dfGeEndWdFeqAll3000WrdsDf))] <- 0
dfGeEndWdFeqDf <- dfGeEndWdFeqAll3000WrdsDf[colnames(dfAll7WdFeqDfLabled)]
dfGeEndWdFeqDf$Label <- NULL #delete Label col

#'normalisation': use max and min values of the data for building the models
dfGeEndWdFeqDf_addMaxMin = rbind(dfGeEndWdFeqDf, apply(dfAll7WdFeqDfLabledRandm[,-1], 2, max), apply(dfAll7WdFeqDfLabledRandm[,-1], 2, min))
normGeEtc = function(x, y) {
for (i in 2: (nrow(y)-2)) {
x = rbind(x, (y[i,] - y[nrow(y),]) / (y[(nrow(y)-1),] - y[nrow(y),]))
}
return(x)
}
#'normalise' the first row
dfGeEndWdFeqDf_normNotReal = (dfGeEndWdFeqDf_addMaxMin[1,] - dfGeEndWdFeqDf_addMaxMin[22,]) / (dfGeEndWdFeqDf_addMaxMin[21,] - dfGeEndWdFeqDf_addMaxMin[22,])
#'normalise' the rest 
dfGeEndWdFeqDf_normNotReal = normGeEtc(dfGeEndWdFeqDf_normNotReal, dfGeEndWdFeqDf_addMaxMin)

#KNN! 
all20GeEnd_knn_pred <- knn(dfAll7WdFeqDfLabledRandm_norm_train, dfGeEndWdFeqDf_normNotReal, dfAll7WdFeqDfLabledRandm[1:140,1], k= 12)
table(all20GeEnd_knn_pred, rep('GE', 20)) #mistake rate 5/20: quite high!

#svm_no_tune
pred_svm_GeEnd <- predict(whichOfThe7_svm_model, dfGeEndWdFeqDf_normNotReal)
table(pred_svm_GeEnd, rep('GE', 20)) #mistake rate 4/20: quite high!
#svm_tuned
pred_svm_after_tune_GeEnd <- predict(svm_tune$best.model, dfGeEndWdFeqDf_normNotReal)
table(pred_svm_after_tune_GeEnd, rep('GE', 20)) #mistake rate 4/20: quite high!

#---------------------------------------------------------------
#rbind George Eliot's 20 documents extracted from her lines from 13760
#and the 210 documents of the seven author's texts
#(3 lines * 20 = 60 lines) with Labels GE
dfGeorge_Eliot13760_13819ImmBlw <- dfVictorianEraAA[13760:13819,]
dfGeorge_Eliot13760_13819ImmBlw_corpus <- VCorpus(VectorSource(dfGeorge_Eliot13760_13819ImmBlw$text))
dfGeorge_Eliot13760_13819ImmBlw_corpus <- tm_map(dfGeorge_Eliot13760_13819ImmBlw_corpus, stripWhitespace)
dfGeorge_Eliot13760_13819ImmBlw_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfGeorge_Eliot13760_13819ImmBlw_corpus, control=list(wordLengths = c(1, Inf)))))
dfGeorge_Eliot13760_13819ImmBlw_dtDf$textNo <- rep(1:20, each = 3)

dfGeEndWdFeqAll3000WrdsDf <- aggregate(. ~ textNo, dfGeorge_Eliot13760_13819ImmBlw_dtDf, sum)
dfGeEndWdFeqAll3000WrdsDf$textNo <- NULL
dfGeEndWdFeqAll3000WrdsDf[setdiff(colnames(dfAll7WdFeqDfLabled), colnames(dfGeEndWdFeqAll3000WrdsDf))] <- 0
dfGeEndWdFeqDf <- dfGeEndWdFeqAll3000WrdsDf[colnames(dfAll7WdFeqDfLabled)]
dfGeEndWdFeqDf$Label <- rep('GE', 20)
dfAll7WdFeqDfLabledRandm <- rbind(dfAll7WdFeqDfLabledRandm, dfGeEndWdFeqDf)

#normalisation:
data_norm <- function(x) {(x- min(x))/ (max(x)- min(x))}
dfAll7WdFeqDfLabledRandm_norm <- as.data.frame(lapply(dfAll7WdFeqDfLabledRandm[,-1], data_norm))
summary(dfAll7WdFeqDfLabledRandm_norm[,1:4]) #see whether normalised

#KNN!
#number of k: usually start from sqrt of data points of
#the training set. So sqrt(210): 14
#then trail and error
library(class)
dfAll7WdFeqDfLabledRandm_norm_train <- dfAll7WdFeqDfLabledRandm_norm[1:210,]
dfAll7WdFeqDfLabledRandm_norm_test <- dfAll7WdFeqDfLabledRandm_norm[211:230,]
whthrAllEliot_pred <- knn(dfAll7WdFeqDfLabledRandm_norm_train, dfAll7WdFeqDfLabledRandm_norm_test, dfAll7WdFeqDfLabledRandm[1:210,1], k= 14)
table(whthrAllEliot_pred, dfAll7WdFeqDfLabledRandm[211:230,1])#mistake rate 8/20 very high!

#SVM!
library("e1071")
# simple: no tunning
whthrAllEliot_svm_model <- svm(dfAll7WdFeqDfLabledRandm_norm_train, dfAll7WdFeqDfLabledRandm[1:210,1], type = 'C')
pred <- predict(whthrAllEliot_svm_model, dfAll7WdFeqDfLabledRandm_norm_test)
table(pred, dfAll7WdFeqDfLabledRandm[211:230,1]) #mistake rate 4/20

#use tunning to find costs
dfAll7WdFeqDfLabledRandmLabel1To210AsFactors = as.factor(dfAll7WdFeqDfLabledRandm[1:210,1])
set.seed(12345)
svm_tune <- tune(svm, train.x = dfAll7WdFeqDfLabledRandm_norm_train,
						train.y = dfAll7WdFeqDfLabledRandmLabel1To210AsFactors,
						kernel = 'linear',
						#type = 'C',
						ranges = list(cost = c(.001,.01,.1,1,5,10,100)))
print(svm_tune) #no sufficient information. Just provide the best cost 
# use svm$best.model
#besides best cost, also best number of support vectors, etc.
pred_svm_after_tune <- predict(svm_tune$best.model, dfAll7WdFeqDfLabledRandm_norm_test)
table(pred_svm_after_tune, dfAll7WdFeqDfLabledRandm[211:230,1]) #mistake rate 3/20

#time code:
proc.time() - ptm #41 seconds in my pc
