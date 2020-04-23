#uel 19/20 DS7003
#WARNING! because there is a non-ascii character â in the code, after importing this code to RStudio, one should select File -> Reopen with encoding... then select UTF-8
#Besides the three popular female English novelists I used in the last attempt (17 Helen Mathers 1853-1920 67 (18010- 18669 in csv file), 32 Lucas Malet 1852-1931 79 (33861-34563), 33 Marie Corelli 1855-1924 69 (34564-36305)), I added four novelists, a propular male novelist born in 1850s': 12 Fergue Hume 1859- 1932 73 (12599- 13185), 8 Charles Dickens 1812- 1870 58 (3399- 10312), 14 George Eliot, Real name Mary Ann Evans, author of Middlemarch 1819- 1880 71 (13671- 16366), 45 Thomas Hardy 1840- 1928 88 (48024- 50335)
#I reduced the number of words extracted from each of the seven authors' texts to: 25 texts of 3000 words each (20 for training and 5 for testing). (Previously, in Jane Austen vs John Muir: 100 texts of 4000 words each; in studying the three popular female English novelists: 50 texts of 4000 words each)

#set working directory and load package tm
setwd(dirname(file.choose()))
getwd()
#time code start:
ptm <- proc.time()
library(tm)

#input data and form seven dataframes
#please pay attention to file names. e.g. hyphen or underscore?
dfVictorianEraAA <- read.table('Gungor_2018_VictorianAuthorAttribution_data_train.csv', header = TRUE, sep = (','))
dfCharles_Dickens3398_3472 <- dfVictorianEraAA[3398:3472,]
dfFergue_Hume12558_12632 <- dfVictorianEraAA[12558:12632,]
dfGeorge_Eliot13670_13744 <- dfVictorianEraAA[13670:13744,]
dfHelen_Mathers18009_18083 <- dfVictorianEraAA[18009:18083,]
dfLucas_Malet33860_33934 <- dfVictorianEraAA[33860:33934,]
dfMarie_Corelli34563_34637 <- dfVictorianEraAA[34563:34637,]
dfThomas_Hardy48023_48097 <- dfVictorianEraAA[48023:48097,]

#form corpa with dataframes. Texts already cleaned
#package tm is required here
dfCharles_Dickens3398_3472_corpus <- VCorpus(VectorSource(dfCharles_Dickens3398_3472$text))
dfCharles_Dickens3398_3472_corpus <- tm_map(dfCharles_Dickens3398_3472_corpus, stripWhitespace)
dfFergue_Hume12558_12632_corpus <- VCorpus(VectorSource(dfFergue_Hume12558_12632$text))
dfFergue_Hume12558_12632_corpus <- tm_map(dfFergue_Hume12558_12632_corpus, stripWhitespace)
dfGeorge_Eliot13670_13744_corpus <- VCorpus(VectorSource(dfGeorge_Eliot13670_13744$text))
dfGeorge_Eliot13670_13744_corpus <- tm_map(dfGeorge_Eliot13670_13744_corpus, stripWhitespace)
dfHelen_Mathers18009_18083_corpus <- VCorpus(VectorSource(dfHelen_Mathers18009_18083$text))
dfHelen_Mathers18009_18083_corpus <- tm_map(dfHelen_Mathers18009_18083_corpus, stripWhitespace)
dfLucas_Malet33860_33934_corpus <- VCorpus(VectorSource(dfLucas_Malet33860_33934$text))
dfLucas_Malet33860_33934_corpus <- tm_map(dfLucas_Malet33860_33934_corpus, stripWhitespace)
dfMarie_Corelli34563_34637_corpus <- VCorpus(VectorSource(dfMarie_Corelli34563_34637$text))
dfMarie_Corelli34563_34637_corpus <- tm_map(dfMarie_Corelli34563_34637_corpus, stripWhitespace)
dfThomas_Hardy48023_48097_corpus <- VCorpus(VectorSource(dfThomas_Hardy48023_48097$text))
dfThomas_Hardy48023_48097_corpus <- tm_map(dfThomas_Hardy48023_48097_corpus, stripWhitespace)

#form dtm. Each line(1000 words) a document
#change minimum word length to 1 from 3
dfCharles_Dickens3398_3472_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfCharles_Dickens3398_3472_corpus, control=list(wordLengths = c(1, Inf)))))
dfFergue_Hume12558_12632_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfFergue_Hume12558_12632_corpus, control=list(wordLengths = c(1, Inf)))))
dfGeorge_Eliot13670_13744_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfGeorge_Eliot13670_13744_corpus, control=list(wordLengths = c(1, Inf)))))
dfHelen_Mathers18009_18083_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfHelen_Mathers18009_18083_corpus, control=list(wordLengths = c(1, Inf)))))
dfLucas_Malet33860_33934_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfLucas_Malet33860_33934_corpus, control=list(wordLengths = c(1, Inf)))))
dfMarie_Corelli34563_34637_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfMarie_Corelli34563_34637_corpus, control=list(wordLengths = c(1, Inf)))))
dfThomas_Hardy48023_48097_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfThomas_Hardy48023_48097_corpus, control=list(wordLengths = c(1, Inf)))))

#retain columns of words which can found in every of the seven authors' texts
common_cols <- Reduce(intersect, list(colnames(dfCharles_Dickens3398_3472_dtDf), colnames(dfFergue_Hume12558_12632_dtDf), colnames(dfGeorge_Eliot13670_13744_dtDf), colnames(dfHelen_Mathers18009_18083_dtDf), colnames(dfLucas_Malet33860_33934_dtDf), colnames(dfMarie_Corelli34563_34637_dtDf), colnames(dfThomas_Hardy48023_48097_dtDf)))
BindAll7NoOfWdsInAll7UniqWdLst <- rbind(dfCharles_Dickens3398_3472_dtDf[common_cols], dfFergue_Hume12558_12632_dtDf[common_cols], dfGeorge_Eliot13670_13744_dtDf[common_cols], dfHelen_Mathers18009_18083_dtDf[common_cols], dfLucas_Malet33860_33934_dtDf[common_cols], dfMarie_Corelli34563_34637_dtDf[common_cols],
dfThomas_Hardy48023_48097_dtDf[common_cols])

#further retain columns of words each of which appearing in the texts at least
#263 times(25 x 3000 x 7 x 0.05% = 263)
SevenAuthsTtl263OrMore <- BindAll7NoOfWdsInAll7UniqWdLst[, colSums(BindAll7NoOfWdsInAll7UniqWdLst) >=263] #224

#after inspection of the dataframe,
#remove columns â, d, o (but not t(can't), m(i'm), etc.)
#be careful! â is not an ascii character
SevenAuthsTtl263OrMore[,c('â', 'd', 'o')] <- list(NULL)

#add textNo column
#aggreate and sum every three lines (reduce to 175 lines from 525 lines)
#delete column textNO
SevenAuthsTtl263OrMore$textNo <- rep(1:175, each = 3)
dfAll7WdFeqDf <- aggregate(. ~ textNo, SevenAuthsTtl263OrMore, sum)
dfAll7WdFeqDf$textNo <- NULL

#add labels and move the label column to the first column
dfAll7WdFeqDf$Label = c(rep('CD', 25), rep('FH', 25), rep('GE', 25), rep('HM', 25), rep('LM', 25), rep('MC', 25), rep('TH', 25)) # 221+1 = 222
dfAll7WdFeqDfLabled = dfAll7WdFeqDf[,c(222,1:221)]

# shuffling rows:
set.seed(12345)
rrowNos <- sample(nrow(dfAll7WdFeqDfLabled))
dfAll7WdFeqDfLabledRandm <- dfAll7WdFeqDfLabled[rrowNos,]
#normalisation:
data_norm <- function(x) {(x- min(x))/ (max(x)- min(x))}
dfAll7WdFeqDfLabledRandm_norm <- as.data.frame(lapply(dfAll7WdFeqDfLabledRandm[,-1], data_norm))
summary(dfAll7WdFeqDfLabledRandm_norm[,1:4]) #see whether normalised

#KNN!
#number of k: usually start from sqrt of data points of
#the training set. So sqrt(140): 12
#then trail and error
library(class)
dfAll7WdFeqDfLabledRandm_norm_train <- dfAll7WdFeqDfLabledRandm_norm[1:140,]
dfAll7WdFeqDfLabledRandm_norm_test <- dfAll7WdFeqDfLabledRandm_norm[141:175,]
whichOfThe7_pred <- knn(dfAll7WdFeqDfLabledRandm_norm_train, dfAll7WdFeqDfLabledRandm_norm_test, dfAll7WdFeqDfLabledRandm[1:140,1], k= 12)
table(whichOfThe7_pred, dfAll7WdFeqDfLabledRandm[141:175,1])#mistake rate 3/35

#SVM!
library("e1071")
# simple: no tunning
whichOfThe7_svm_model <- svm(dfAll7WdFeqDfLabledRandm_norm_train, dfAll7WdFeqDfLabledRandm[1:140,1], type = 'C')
pred <- predict(whichOfThe7_svm_model, dfAll7WdFeqDfLabledRandm_norm_test)
table(pred, dfAll7WdFeqDfLabledRandm[141:175,1]) #one error only

#use tunning to find costs
dfAll7WdFeqDfLabledRandmLabel1To140AsFactors = as.factor(dfAll7WdFeqDfLabledRandm[1:140,1])
set.seed(12345)
svm_tune <- tune(svm, train.x = dfAll7WdFeqDfLabledRandm_norm_train,
						train.y = dfAll7WdFeqDfLabledRandmLabel1To140AsFactors,
						kernel = 'linear',
						#type = 'C',
						ranges = list(cost = c(.001,.01,.1,1,5,10,100)))
print(svm_tune) #no sufficient information. Just provide the best cost 
# use svm$best.model
#besides best cost, also best number of support vectors, etc.
pred_svm_after_tune <- predict(svm_tune$best.model, dfAll7WdFeqDfLabledRandm_norm_test)
table(pred_svm_after_tune, dfAll7WdFeqDfLabledRandm[141:175,1]) #all correct

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
#Check whether the knn and svm models developed above (using the first 75 lines of each of the seven authors) can recognise that the 20 documents (each x 3000 words) complied with George Eliot's lines extracted from immediate below the first 75 lines were written by her. 
#need to use package tm
#(no comment lines below. For explanations, see the above part)

dfGeorge_Eliot13745_13804ImmBlw <- dfVictorianEraAA[13745:13804,]
dfGeorge_Eliot13745_13804ImmBlw_corpus <- VCorpus(VectorSource(dfGeorge_Eliot13745_13804ImmBlw$text))
dfGeorge_Eliot13745_13804ImmBlw_dtDf <- as.data.frame(as.matrix(DocumentTermMatrix(dfGeorge_Eliot13745_13804ImmBlw_corpus, control=list(wordLengths = c(1, Inf)))))
dfGeorge_Eliot13745_13804ImmBlw_dtDf$textNo <- rep(1:20, each = 3)

dfGeImmBlwWdFeqAll3000WrdsDf <- aggregate(. ~ textNo, dfGeorge_Eliot13745_13804ImmBlw_dtDf, sum)
dfGeImmBlwWdFeqAll3000WrdsDf$textNo <- NULL
dfGeImmBlwWdFeqAll3000WrdsDf[setdiff(colnames(dfAll7WdFeqDfLabled), colnames(dfGeImmBlwWdFeqAll3000WrdsDf))] <- 0
dfGeImmBlwWdFeqDf <- dfGeImmBlwWdFeqAll3000WrdsDf[colnames(dfAll7WdFeqDfLabled)]
dfGeImmBlwWdFeqDf$Label <- NULL #delete Label col

dfGeImmBlwWdFeqDf_addMaxMin = rbind(dfGeImmBlwWdFeqDf, apply(dfAll7WdFeqDfLabledRandm[,-1], 2, max), apply(dfAll7WdFeqDfLabledRandm[,-1], 2, min))
normGeEtc = function(x, y) {
for (i in 2: (nrow(y)-2)) {
x = rbind(x, (y[i,] - y[nrow(y),]) / (y[(nrow(y)-1),] - y[nrow(y),]))
}
return(x)
}
dfGeImmBlwWdFeqDf_normNotReal = (dfGeImmBlwWdFeqDf_addMaxMin[1,] - dfGeImmBlwWdFeqDf_addMaxMin[22,]) / (dfGeImmBlwWdFeqDf_addMaxMin[21,] - dfGeImmBlwWdFeqDf_addMaxMin[22,])
dfGeImmBlwWdFeqDf_normNotReal = normGeEtc(dfGeImmBlwWdFeqDf_normNotReal, dfGeImmBlwWdFeqDf_addMaxMin)

all20GeEnd_knn_pred <- knn(dfAll7WdFeqDfLabledRandm_norm_train, dfGeImmBlwWdFeqDf_normNotReal, dfAll7WdFeqDfLabledRandm[1:140,1], k= 12)
table(all20GeEnd_knn_pred, rep('GE', 20)) #mistake rate 1/20: better  

#svm_no_tune
pred_svm_GeEnd <- predict(whichOfThe7_svm_model, dfGeImmBlwWdFeqDf_normNotReal)
table(pred_svm_GeEnd, rep('GE', 20)) #mistake rate 3/20: slightly better
#svm_tuned
pred_svm_after_tune_GeEnd <- predict(svm_tune$best.model, dfGeImmBlwWdFeqDf_normNotReal)
table(pred_svm_after_tune_GeEnd, rep('GE', 20)) #mistake rate 2/20: slightly better
