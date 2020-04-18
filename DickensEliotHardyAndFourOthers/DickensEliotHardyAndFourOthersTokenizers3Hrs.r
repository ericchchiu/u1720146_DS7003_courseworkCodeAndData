#Besides the three popular female English novelists I used in the last attempt (17 Helen Mathers 1853-1920 67 (18010- 18669 in csv file), 32 Lucas Malet 1852-1931 79 (33861-34563), 33 Marie Corelli 1855-1924 69 (34564-36305)), I added four novelists, a propular male novelist born in 1850s' 12 Fergue Hume 1859- 1932 73 (12599- 13185), 8 Charles Dickens 1812- 1870 58 (3399- 10312), 14 George Eliot Real name Mary Ann Evans, author of Middlemarch 1819- 1880 71 (13671- 16366), 45 Thomas Hardy 1840- 1928 88 (48024- 50335)
#I reduced the number of words extracted from each of the seven authors' texts and used to: 25 texts of 3000 words each. (Jane Austen and John Muir: 100 texts of 4000 words each; The three popular female English novelists: 50 texts of 4000 words each)

#input data
#please pay attention to file names. e.g. hyphen or en dash
dfVictorianEraAA <- read.table('Gungor_2018_VictorianAuthorAttribution_data_train.csv', header = TRUE, sep = (','))

dfCharles_Dickens3398_3472 <- dfVictorianEraAA[3398:3472,]
dfCharles_Dickens3398_3472$textNo <- rep(1:25, each = 3)
dfCharles_Dickens3398_3472 <- dfCharles_Dickens3398_3472[c('textNo', 'text')]

dfFergue_Hume12558_12632 <- dfVictorianEraAA[12558:12632,]
dfFergue_Hume12558_12632$textNo <- rep(1:25, each = 3)
dfFergue_Hume12558_12632 <- dfFergue_Hume12558_12632[c('textNo', 'text')]

dfGeorge_Eliot13670_13744 <- dfVictorianEraAA[13670:13744,]
dfGeorge_Eliot13670_13744$textNo <- rep(1:25, each = 3)
dfGeorge_Eliot13670_13744 <- dfGeorge_Eliot13670_13744[c('textNo', 'text')]

dfHelen_Mathers18009_18083 <- dfVictorianEraAA[18009:18083,]
dfHelen_Mathers18009_18083$textNo <- rep(1:25, each = 3)
dfHelen_Mathers18009_18083 <- dfHelen_Mathers18009_18083[c('textNo', 'text')]

dfLucas_Malet33860_33934 <- dfVictorianEraAA[33860:33934,]
dfLucas_Malet33860_33934$textNo <- rep(1:25, each = 3)
dfLucas_Malet33860_33934 <- dfLucas_Malet33860_33934[c('textNo', 'text')]

dfMarie_Corelli34563_34637 <- dfVictorianEraAA[34563:34637,]
dfMarie_Corelli34563_34637$textNo <- rep(1:25, each = 3)
dfMarie_Corelli34563_34637 <- dfMarie_Corelli34563_34637[c('textNo', 'text')]

dfThomas_Hardy48023_48097 <- dfVictorianEraAA[48023:48097,]
dfThomas_Hardy48023_48097$textNo <- rep(1:25, each = 3)
dfThomas_Hardy48023_48097 <- dfThomas_Hardy48023_48097[c('textNo', 'text')]

listOfWordsCharles_Dickens3398_3472 <- tokenize_words(paste0(dfCharles_Dickens3398_3472[1,2]))
for (i in 2:75) {
listOfWordsCharles_Dickens3398_3472 <- combineTwoListsAsOne (listOfWordsCharles_Dickens3398_3472, tokenize_words(paste0(dfCharles_Dickens3398_3472[i,2])))
listOfWordsCharles_Dickens3398_3472 <- unique(listOfWordsCharles_Dickens3398_3472)
}
listOfWordsFergue_Hume12558_12632 <- tokenize_words(paste0(dfFergue_Hume12558_12632[1,2]))
for (i in 2:75) {
listOfWordsFergue_Hume12558_12632 <- combineTwoListsAsOne (listOfWordsFergue_Hume12558_12632, tokenize_words(paste0(dfFergue_Hume12558_12632[i,2])))
listOfWordsFergue_Hume12558_12632 <- unique(listOfWordsFergue_Hume12558_12632)
}
listOfWordsGeorge_Eliot13670_13744 <- tokenize_words(paste0(dfGeorge_Eliot13670_13744[1,2]))
for (i in 2:75) {
listOfWordsGeorge_Eliot13670_13744 <- combineTwoListsAsOne (listOfWordsGeorge_Eliot13670_13744, tokenize_words(paste0(dfGeorge_Eliot13670_13744[i,2])))
listOfWordsGeorge_Eliot13670_13744 <- unique(listOfWordsGeorge_Eliot13670_13744)
}
listOfWordsHelen_Mathers18009_18083 <- tokenize_words(paste0(dfHelen_Mathers18009_18083[1,2]))
for (i in 2:75) {
listOfWordsHelen_Mathers18009_18083 <- combineTwoListsAsOne (listOfWordsHelen_Mathers18009_18083, tokenize_words(paste0(dfHelen_Mathers18009_18083[i,2])))
listOfWordsHelen_Mathers18009_18083 <- unique(listOfWordsHelen_Mathers18009_18083)
}
listOfWordsLucas_Malet33860_33934 <- tokenize_words(paste0(dfLucas_Malet33860_33934[1,2]))
for (i in 2:75) {
listOfWordsLucas_Malet33860_33934 <- combineTwoListsAsOne (listOfWordsLucas_Malet33860_33934, tokenize_words(paste0(dfLucas_Malet33860_33934[i,2])))
listOfWordsLucas_Malet33860_33934 <- unique(listOfWordsLucas_Malet33860_33934)
}
listOfWordsMarie_Corelli34563_34637 <- tokenize_words(paste0(dfMarie_Corelli34563_34637[1,2]))
for (i in 2:75) {
listOfWordsMarie_Corelli34563_34637 <- combineTwoListsAsOne (listOfWordsMarie_Corelli34563_34637, tokenize_words(paste0(dfMarie_Corelli34563_34637[i,2])))
listOfWordsMarie_Corelli34563_34637 <- unique(listOfWordsMarie_Corelli34563_34637)
}
listOfWordsThomas_Hardy48023_48097 <- tokenize_words(paste0(dfThomas_Hardy48023_48097[1,2]))
for (i in 2:75) {
listOfWordsThomas_Hardy48023_48097 <- combineTwoListsAsOne (listOfWordsThomas_Hardy48023_48097, tokenize_words(paste0(dfThomas_Hardy48023_48097[i,2])))
listOfWordsThomas_Hardy48023_48097 <- unique(listOfWordsThomas_Hardy48023_48097)
}

#Unique words: Charles Dickens 6202/ Fergue Hume 4806/ Georgr Eliot 5784/ Helen Mathers 5202/ Lucas Malet 5304/ Marie Cerelli 5526/ Thomas Hardy 5599
#Appearing in all 1725
#Appearing in all and >=0.05% (25 x 3000 x 7 x 0.05%) 263

listOfWordsAppearingInAll7Authors <- Reduce(intersect, list(listOfWordsCharles_Dickens3398_3472, listOfWordsFergue_Hume12558_12632, listOfWordsGeorge_Eliot13670_13744, listOfWordsHelen_Mathers18009_18083, listOfWordsLucas_Malet33860_33934, listOfWordsMarie_Corelli34563_34637, listOfWordsThomas_Hardy48023_48097))
A = function(x, y) {       #it works!
df <- data.frame('a' = 0)
for(i in x) {
num = 0
for(val in 1: nrow(y)) {
tokenized = tokenize_words(paste0(y[val,2]))
tokenized = unlist(tokenized, use.names=FALSE)
num = num + length(grep(paste('\\<',i,'\\>', sep =''), tokenized))
}
df[paste(i)] <- c(num)
} 
return(df)
}
CD_NoOfWdsInAll7UniqWdLst = A(listOfWordsAppearingInAll7Authors, dfCharles_Dickens3398_3472)  #5 minutes
FH_NoOfWdsInAll7UniqWdLst = A(listOfWordsAppearingInAll7Authors, dfFergue_Hume12558_12632)
GE_NoOfWdsInAll7UniqWdLst = A(listOfWordsAppearingInAll7Authors, dfGeorge_Eliot13670_13744)
HM_NoOfWdsInAll7UniqWdLst = A(listOfWordsAppearingInAll7Authors, dfHelen_Mathers18009_18083)
LM_NoOfWdsInAll7UniqWdLst = A(listOfWordsAppearingInAll7Authors, dfLucas_Malet33860_33934)
MC_NoOfWdsInAll7UniqWdLst = A(listOfWordsAppearingInAll7Authors, dfMarie_Corelli34563_34637)
TH_NoOfWdsInAll7UniqWdLst = A(listOfWordsAppearingInAll7Authors, dfThomas_Hardy48023_48097)
BindAll7NoOfWdsInAll7UniqWdLst = rbind(CD_NoOfWdsInAll7UniqWdLst, FH_NoOfWdsInAll7UniqWdLst, GE_NoOfWdsInAll7UniqWdLst, HM_NoOfWdsInAll7UniqWdLst, LM_NoOfWdsInAll7UniqWdLst, MC_NoOfWdsInAll7UniqWdLst, TH_NoOfWdsInAll7UniqWdLst)
SevenAuthsTtl263OrMore = BindAll7NoOfWdsInAll7UniqWdLst[, colSums(BindAll7NoOfWdsInAll7UniqWdLst) >=263]  #224
SevenAuthsTtl263OrMoreByAlpha = SevenAuthsTtl263OrMore[,order(names(SevenAuthsTtl263OrMore))]
sink('SevenAuthsTtl263OrMoreByAlpha.txt')  #these three lines do not work
print(SevenAuthsTtl263OrMoreByAlpha, topn = 1000)
sink()  
SevenAuthsTtl263OrMoreByAlphaHeader = colnames(SevenAuthsTtl263OrMoreByAlpha)
remove <- c('â', 'd', 'o') 
SevenAuthsTtl263OrMoreByAlphaHeader = setdiff(SevenAuthsTtl263OrMoreByAlphaHeader, remove) #224 - 3 = 221
# but number of â too high: DC: 154, FH: 256, GE: 530, HM: 853, LM: 717, MC: 2311, TH: 663

dfEach3RWdNoOfOccu = function (x, y, z) {

    for (i in 4: nrow(y)) {
        if (i%%3 == 1) {
			df <- y[i,]
        }
        else if (i%%3 != 0) {
			df <- rbind(df, y[i,])
        }
        else {
            df <- rbind(df, y[i,])
			z = rbind(z, A(x, df))
        }}
    return(z)
}

dfCdWdFeqDf = A(SevenAuthsTtl263OrMoreByAlphaHeader, dfCharles_Dickens3398_3472[1:3,])
dfCdWdFeqDf = dfEach3RWdNoOfOccu(SevenAuthsTtl263OrMoreByAlphaHeader, dfCharles_Dickens3398_3472, dfCdWdFeqDf)
dfFhWdFeqDf = A(SevenAuthsTtl263OrMoreByAlphaHeader, dfFergue_Hume12558_12632[1:3,])
dfFhWdFeqDf = dfEach3RWdNoOfOccu(SevenAuthsTtl263OrMoreByAlphaHeader, dfFergue_Hume12558_12632, dfFhWdFeqDf)
dfGeWdFeqDf = A(SevenAuthsTtl263OrMoreByAlphaHeader, dfGeorge_Eliot13670_13744[1:3,])
dfGeWdFeqDf = dfEach3RWdNoOfOccu(SevenAuthsTtl263OrMoreByAlphaHeader, dfGeorge_Eliot13670_13744, dfGeWdFeqDf)
dfHmWdFeqDf = A(SevenAuthsTtl263OrMoreByAlphaHeader, dfHelen_Mathers18009_18083[1:3,])
dfHmWdFeqDf = dfEach3RWdNoOfOccu(SevenAuthsTtl263OrMoreByAlphaHeader, dfHelen_Mathers18009_18083, dfHmWdFeqDf)
dfLmWdFeqDf = A(SevenAuthsTtl263OrMoreByAlphaHeader, dfLucas_Malet33860_33934[1:3,])
dfLmWdFeqDf = dfEach3RWdNoOfOccu(SevenAuthsTtl263OrMoreByAlphaHeader, dfLucas_Malet33860_33934, dfLmWdFeqDf)
dfMcWdFeqDf = A(SevenAuthsTtl263OrMoreByAlphaHeader, dfMarie_Corelli34563_34637[1:3,])
dfMcWdFeqDf = dfEach3RWdNoOfOccu(SevenAuthsTtl263OrMoreByAlphaHeader, dfMarie_Corelli34563_34637, dfMcWdFeqDf)
dfThWdFeqDf = A(SevenAuthsTtl263OrMoreByAlphaHeader, dfThomas_Hardy48023_48097[1:3,])
dfThWdFeqDf = dfEach3RWdNoOfOccu(SevenAuthsTtl263OrMoreByAlphaHeader, dfThomas_Hardy48023_48097, dfThWdFeqDf)
dfAll7WdFeqDf = rbind(dfCdWdFeqDf, dfFhWdFeqDf, dfGeWdFeqDf, dfHmWdFeqDf, dfLmWdFeqDf, dfMcWdFeqDf, dfThWdFeqDf)
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
library(class)
dfAll7WdFeqDfLabledRandm_norm_train <- dfAll7WdFeqDfLabledRandm_norm[1:140,]
dfAll7WdFeqDfLabledRandm_norm_test <- dfAll7WdFeqDfLabledRandm_norm[141:175,]
whichOfThe7_pred <- knn(dfAll7WdFeqDfLabledRandm_norm_train, dfAll7WdFeqDfLabledRandm_norm_test, dfAll7WdFeqDfLabledRandm[1:140,1], k= 12)
table(whichOfThe7_pred, dfAll7WdFeqDfLabledRandm[141:175,1])
#SVM!
library("e1071")
# simple: no tunning
whichOfThe7_svm_model <- svm(dfAll7WdFeqDfLabledRandm_norm_train, dfAll7WdFeqDfLabledRandm[1:140,1], type = 'C')
pred <- predict(whichOfThe7_svm_model, dfAll7WdFeqDfLabledRandm_norm_test)
table(pred, dfAll7WdFeqDfLabledRandm[141:175,1]) #its good. one error only

#Use tunning to find costs
dfAll7WdFeqDfLabledRandmLabel1To140AsFactors = as.factor(dfAll7WdFeqDfLabledRandm[1:140,1])
set.seed(12345)
svm_tune <- tune(svm, train.x = dfAll7WdFeqDfLabledRandm_norm_train,
						train.y = dfAll7WdFeqDfLabledRandmLabel1To140AsFactors,
						kernel = 'linear',
						#type = 'C',
						ranges = list(cost = c(.001,.01,.1,1,5,10,100)))
print(svm_tune) #no sufficient information. Just provide the best cost 
# use svm$best.model (besides best cost, also best support vectors, etc.
pred_svm_after_tune <- predict(svm_tune$best.model, dfAll7WdFeqDfLabledRandm_norm_test)
table(pred_svm_after_tune, dfAll7WdFeqDfLabledRandm[141:175,1]) #all correct

#--------------------------------------------------------------------

#George Elilot immBlw and end:
dfGeorge_Eliot13745_13804ImmBlw <- dfVictorianEraAA[13745:13804,]
dfGeorge_Eliot13745_13804ImmBlw$textNo <- rep(1:20, each = 3)
dfGeorge_Eliot13745_13804ImmBlw <- dfGeorge_Eliot13745_13804ImmBlw[c('textNo', 'text')]

dfGeorge_Eliot16306_16365End <- dfVictorianEraAA[16306:16365,]
dfGeorge_Eliot16306_16365End$textNo <- rep(1:20, each = 3)
dfGeorge_Eliot16306_16365End <- dfGeorge_Eliot16306_16365End[c('textNo', 'text')]

dfGeImmBlwWdFeqDf = A(SevenAuthsTtl263OrMoreByAlphaHeader, dfGeorge_Eliot13745_13804ImmBlw[1:3,])
dfGeImmBlwWdFeqDf = dfEach3RWdNoOfOccu(SevenAuthsTtl263OrMoreByAlphaHeader, dfGeorge_Eliot13745_13804ImmBlw, dfGeImmBlwWdFeqDf)

dfGeEndWdFeqDf = A(SevenAuthsTtl263OrMoreByAlphaHeader, dfGeorge_Eliot16306_16365End[1:3,])
dfGeEndWdFeqDf = dfEach3RWdNoOfOccu(SevenAuthsTtl263OrMoreByAlphaHeader, dfGeorge_Eliot16306_16365End, dfGeEndWdFeqDf)

dfGeImmBlwWdFeqDf_addMaxMin = rbind(dfGeImmBlwWdFeqDf, apply(dfAll7WdFeqDfLabledRandm[,-1], 2, max), apply(dfAll7WdFeqDfLabledRandm[,-1], 2, min))

dfGeEndWdFeqDf_addMaxMin = rbind(dfGeEndWdFeqDf, apply(dfAll7WdFeqDfLabledRandm[,-1], 2, max), apply(dfAll7WdFeqDfLabledRandm[,-1], 2, min))
#knn!
#immBlw:
dfGeImmBlwWdFeqDf_normNotReal = (dfGeImmBlwWdFeqDf_addMaxMin[1,] - dfGeImmBlwWdFeqDf_addMaxMin[22,]) / (dfGeImmBlwWdFeqDf_addMaxMin[21,] - dfGeImmBlwWdFeqDf_addMaxMin[22,])
normGeEtc = function(x, y) {
for (i in 2: (nrow(y)-2)) {
x = rbind(x, (y[i,] - y[nrow(y),]) / (y[(nrow(y)-1),] - y[nrow(y),]))
}
return(x)
}
dfGeImmBlwWdFeqDf_normNotReal = normGeEtc(dfGeImmBlwWdFeqDf_normNotReal, dfGeImmBlwWdFeqDf_addMaxMin)
all20GeImmBlw_knn_pred <- knn(dfAll7WdFeqDfLabledRandm_norm_train, dfGeImmBlwWdFeqDf_normNotReal, dfAll7WdFeqDfLabledRandm[1:140,1], k= 12)
table(all20GeImmBlw_knn_pred, rep('GE', 20))
#end:
dfGeEndWdFeqDf_normNotReal = (dfGeEndWdFeqDf_addMaxMin[1,] - dfGeEndWdFeqDf_addMaxMin[22,]) / (dfGeEndWdFeqDf_addMaxMin[21,] - dfGeEndWdFeqDf_addMaxMin[22,])
dfGeEndWdFeqDf_normNotReal = normGeEtc(dfGeEndWdFeqDf_normNotReal, dfGeEndWdFeqDf_addMaxMin)
all20GeEnd_knn_pred <- knn(dfAll7WdFeqDfLabledRandm_norm_train, dfGeEndWdFeqDf_normNotReal, dfAll7WdFeqDfLabledRandm[1:140,1], k= 12)
table(all20GeEnd_knn_pred, rep('GE', 20))
#svm_no_tune
#immBlw:
pred_svm_GeImmBlw <- predict(whichOfThe7_svm_model, dfGeImmBlwWdFeqDf_normNotReal)
table(pred_svm_GeImmBlw, rep('GE', 20))
#end
pred_svm_GeEnd <- predict(whichOfThe7_svm_model, dfGeEndWdFeqDf_normNotReal)
table(pred_svm_GeEnd, rep('GE', 20))
#svm_tuned
#immBlw:
pred_svm_after_tune_GeImmBlw <- predict(svm_tune$best.model, dfGeImmBlwWdFeqDf_normNotReal)
table(pred_svm_after_tune_GeImmBlw, rep('GE', 20))
#end
pred_svm_after_tune_GeEnd <- predict(svm_tune$best.model, dfGeEndWdFeqDf_normNotReal)
table(pred_svm_after_tune_GeEnd, rep('GE', 20))


#Marie Corelli immBlw and end
dfMarie_Corelli34638_34697ImmBlw <- dfVictorianEraAA[34638:34697,]
dfMarie_Corelli34638_34697ImmBlw$textNo <- rep(1:20, each = 3)
dfMarie_Corelli34638_34697ImmBlw <- dfMarie_Corelli34638_34697ImmBlw[c('textNo', 'text')]

dfMarie_Corelli36245_36304End <- dfVictorianEraAA[36245:36304,]
dfMarie_Corelli36245_36304End$textNo <- rep(1:20, each = 3)
dfMarie_Corelli36245_36304End <- dfMarie_Corelli36245_36304End[c('textNo', 'text')]

dfMcImmBlwWdFeqDf = A(SevenAuthsTtl263OrMoreByAlphaHeader, dfMarie_Corelli34638_34697ImmBlw[1:3,])
dfMcImmBlwWdFeqDf = dfEach3RWdNoOfOccu(SevenAuthsTtl263OrMoreByAlphaHeader, dfMarie_Corelli34638_34697ImmBlw, dfMcImmBlwWdFeqDf)

dfMcEndWdFeqDf = A(SevenAuthsTtl263OrMoreByAlphaHeader, dfMarie_Corelli36245_36304End[1:3,])
dfMcEndWdFeqDf = dfEach3RWdNoOfOccu(SevenAuthsTtl263OrMoreByAlphaHeader, dfMarie_Corelli36245_36304End, dfMcEndWdFeqDf)

dfMcImmBlwWdFeqDf_addMaxMin = rbind(dfMcImmBlwWdFeqDf, apply(dfAll7WdFeqDfLabledRandm[,-1], 2, max), apply(dfAll7WdFeqDfLabledRandm[,-1], 2, min))

dfMcEndWdFeqDf_addMaxMin = rbind(dfMcEndWdFeqDf, apply(dfAll7WdFeqDfLabledRandm[,-1], 2, max), apply(dfAll7WdFeqDfLabledRandm[,-1], 2, min))
#knn!
#immBlw:
dfMcImmBlwWdFeqDf_normNotReal = (dfMcImmBlwWdFeqDf_addMaxMin[1,] - dfMcImmBlwWdFeqDf_addMaxMin[22,]) / (dfMcImmBlwWdFeqDf_addMaxMin[21,] - dfMcImmBlwWdFeqDf_addMaxMin[22,])
normGeEtc = function(x, y) {
for (i in 2: (nrow(y)-2)) {
x = rbind(x, (y[i,] - y[nrow(y),]) / (y[(nrow(y)-1),] - y[nrow(y),]))
}
return(x)
}
dfMcImmBlwWdFeqDf_normNotReal = normGeEtc(dfMcImmBlwWdFeqDf_normNotReal, dfMcImmBlwWdFeqDf_addMaxMin)
all20McImmBlw_knn_pred <- knn(dfAll7WdFeqDfLabledRandm_norm_train, dfMcImmBlwWdFeqDf_normNotReal, dfAll7WdFeqDfLabledRandm[1:140,1], k= 12)
table(all20McImmBlw_knn_pred, rep('MC', 20))
#end:
dfMcEndWdFeqDf_normNotReal = (dfMcEndWdFeqDf_addMaxMin[1,] - dfMcEndWdFeqDf_addMaxMin[22,]) / (dfMcEndWdFeqDf_addMaxMin[21,] - dfMcEndWdFeqDf_addMaxMin[22,])
dfMcEndWdFeqDf_normNotReal = normGeEtc(dfMcEndWdFeqDf_normNotReal, dfMcEndWdFeqDf_addMaxMin)
all20McEnd_knn_pred <- knn(dfAll7WdFeqDfLabledRandm_norm_train, dfMcEndWdFeqDf_normNotReal, dfAll7WdFeqDfLabledRandm[1:140,1], k= 12)
table(all20McEnd_knn_pred, rep('MC', 20)) #12:14 13:17 11:15 10:18 14:16
#svm_no_tune
#immBlw:
pred_svm_McImmBlw <- predict(whichOfThe7_svm_model, dfMcImmBlwWdFeqDf_normNotReal)
table(pred_svm_McImmBlw, rep('MC', 20)) 
#end
pred_svm_McEnd <- predict(whichOfThe7_svm_model, dfMcEndWdFeqDf_normNotReal)
table(pred_svm_McEnd, rep('MC', 20))  #11
#svm_tuned
#immBlw:
pred_svm_after_tune_McImmBlw <- predict(svm_tune$best.model, dfMcImmBlwWdFeqDf_normNotReal)
table(pred_svm_after_tune_McImmBlw, rep('MC', 20))
#end
pred_svm_after_tune_McEnd <- predict(svm_tune$best.model, dfMcEndWdFeqDf_normNotReal)
table(pred_svm_after_tune_McEnd, rep('MC', 20))  #13