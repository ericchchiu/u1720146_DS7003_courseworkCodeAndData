#Using three popular female novelists all born in the 1850s: 17 Helen Mathers 1853-1920 (18010- 18669 in csv file), 32 Lucas Malet 1852-1931 (33861-34563), 33 Marie Corelli 1855-1924 (34564-36305)

#unique words HM 7165, LM 7587, MC 7773 -> appear on both 5228 -> appear on both total 0.05% 237

dfVictorianEraAA <- read.table('..\\dataset\\Gungor_2018_VictorianAuthorAttribution_data_train.csv', header = TRUE, sep = (','))


dfHelen_Mathers18009_18208 <- dfVictorianEraAA[18009:18208,]
dfHelen_Mathers18009_18208$textNo <- rep(1:50, each = 4)
dfHelen_Mathers18009_18208 <- dfHelen_Mathers18009_18208[c('textNo', 'text')]

dfLucas_Malet33860_34059 <- dfVictorianEraAA[33860:34059,]
dfLucas_Malet33860_34059$textNo <- rep(1:50, each = 4)
dfLucas_Malet33860_34059 <- dfLucas_Malet33860_34059[c('textNo', 'text')]

dfMarie_Corelli34563_34762 <- dfVictorianEraAA[34563:34762,]
dfMarie_Corelli34563_34762$textNo <- rep(1:50, each = 4)
dfMarie_Corelli34563_34762 <- dfMarie_Corelli34563_34762[c('textNo', 'text')]

combineTwoListsAsOne <- function (list1, list2) {
n <- c()
for(x in list1){n<-c(n,x)}
for(x in list2){n<-c(n,x)}
return(n)
}
listOfWordsHelen_Mathers18009_18208 <- tokenize_words(paste0(dfHelen_Mathers18009_18208[1,2]))
for (i in 2:200) {
listOfWordsHelen_Mathers18009_18208 <- combineTwoListsAsOne (listOfWordsHelen_Mathers18009_18208, tokenize_words(paste0(dfHelen_Mathers18009_18208[i,2])))
listOfWordsHelen_Mathers18009_18208 <- unique(listOfWordsHelen_Mathers18009_18208)
}
listOfWordsLucas_Malet33860_34059 <- tokenize_words(paste0(dfLucas_Malet33860_34059[1,2]))
for (i in 2:200) {
listOfWordsLucas_Malet33860_34059 <- combineTwoListsAsOne (listOfWordsLucas_Malet33860_34059, tokenize_words(paste0(dfLucas_Malet33860_34059[i,2])))
listOfWordsLucas_Malet33860_34059 <- unique(listOfWordsLucas_Malet33860_34059)
}
listOfWordsMarie_Corelli34563_34762 <- tokenize_words(paste0(dfMarie_Corelli34563_34762[1,2]))
for (i in 2:200) {
listOfWordsMarie_Corelli34563_34762 <- combineTwoListsAsOne (listOfWordsMarie_Corelli34563_34762, tokenize_words(paste0(dfMarie_Corelli34563_34762[i,2])))
listOfWordsMarie_Corelli34563_34762 <- unique(listOfWordsMarie_Corelli34563_34762)
}
listOfWordsAppearingInBothHMAndLMAndMC <- Reduce(intersect, list(listOfWordsHelen_Mathers18009_18208, listOfWordsLucas_Malet33860_34059, listOfWordsMarie_Corelli34563_34762))
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
HM_NoOfWdsInHMnLMnMCUniqWdLst = A(listOfWordsAppearingInBothHMAndLMAndMC, dfHelen_Mathers18009_18208)   #each 35 minutes
---------------------------------------
LM_NoOfWdsInHMnLMnMCUniqWdLst = A(listOfWordsAppearingInBothHMAndLMAndMC, dfLucas_Malet33860_34059)
MC_NoOfWdsInHMnLMnMCUniqWdLst = A(listOfWordsAppearingInBothHMAndLMAndMC, dfMarie_Corelli34563_34762)
HMnLMnMCJoin_NoOfWdsInHMnLMnMCUniqWdLst = rbind(HM_NoOfWdsInHMnLMnMCUniqWdLst, LM_NoOfWdsInHMnLMnMCUniqWdLst, MC_NoOfWdsInHMnLMnMCUniqWdLst)
HmLmMcTtl300OrMore = HMnLMnMCJoin_NoOfWdsInHMnLMnMCUniqWdLst[, colSums(HMnLMnMCJoin_NoOfWdsInHMnLMnMCUniqWdLst) >=300] #237 words
HmLmMcTtl300OrMoreByAlpha = HmLmMcTtl300OrMore[,order(names(HmLmMcTtl300OrMore))]
sink('HmLmMcTtl300OrMoreByAlpha.txt')
print(HmLmMcTtl300OrMoreByAlpha)
sink()
HmLmMcTtl300OrMoreByAlphaHeader = colnames(HmLmMcTtl300OrMoreByAlpha)
remove <- c('â') #but number of occurrence of â too high: HM 2077, LM 1743, MC 6280
HmLmMcTtl300OrMoreByAlphaHeader = setdiff(HmLmMcTtl300OrMoreByAlphaHeader, remove) #236

dfEach4RWdNoOfOccu = function (x, y, z) {

    for (i in 5: nrow(y)) {
        if (i%%4 == 1) {
			df <- y[i,]
        }
        else if (i%%4 != 0) {
			df <- rbind(df, y[i,])
        }
        else {
            df <- rbind(df, y[i,])
			z = rbind(z, A(x, df))
        }}
    return(z)
}

dfHmWdFeqDf = A(HmLmMcTtl300OrMoreByAlphaHeader, dfHelen_Mathers18009_18208[1:4,])
dfHmWdFeqDf = dfEach4RWdNoOfOccu(HmLmMcTtl300OrMoreByAlphaHeader, dfHelen_Mathers18009_18208, dfHmWdFeqDf)
dfLmWdFeqDf = A(HmLmMcTtl300OrMoreByAlphaHeader, dfLucas_Malet33860_34059[1:4,])
dfLmWdFeqDf = dfEach4RWdNoOfOccu(HmLmMcTtl300OrMoreByAlphaHeader, dfLucas_Malet33860_34059, dfLmWdFeqDf)
dfMcWdFeqDf = A(HmLmMcTtl300OrMoreByAlphaHeader, dfMarie_Corelli34563_34762[1:4,])
dfMcWdFeqDf = dfEach4RWdNoOfOccu(HmLmMcTtl300OrMoreByAlphaHeader, dfMarie_Corelli34563_34762, dfMcWdFeqDf)

dfHmLmMcWdFeqDf = rbind(dfHmWdFeqDf, dfLmWdFeqDf, dfMcWdFeqDf)

dfHmLmMcWdFeqDf$HmOrLmOrMc = c(rep('HM', 50), rep('LM', 50), rep('MC', 50)) #236+1
dfHmLmMcWdFeqDfLabled = dfHmLmMcWdFeqDf[,c(237,1:236)]

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
table(HmOrLmOrMc_pred, dfHmLmMcWdFeqDfLabledRandm[121:150,1])
#confusion tables see photos sqrt(120) = 10.954 . Therefore use k =11 first. only one error 1 MC was misjudged as LM / went worse when k = 8 2 MC as LM / all correct k = 33

#SVM!
#library("e1071")
#old code
#y <- dfHmLmMcWdFeqDfLabledRandm_norm[,1, drop= FALSE]
#dfHmLmMcWdFeqDfLabledRandm_norm_WthLabl <- cbind(dfHmLmMcWdFeqDfLabledRandm[,1, drop= FALSE], dfHmLmMcWdFeqDfLabledRandm_norm)
#svm_model <- svm(HmOrLmOrMc ~ ., data = dfHmLmMcWdFeqDfLabledRandm_norm_WthLabl)
#pred <- predict(svm_model,dfHmLmMcWdFeqDfLabledRandm_norm)
#table(pred,y$HmOrLmOrMc)

#library("e1071")
#dfHmLmMcWdFeqDfLabledRandm_norm_WthLabl <- #cbind(dfHmLmMcWdFeqDfLabledRandm[,1, drop= FALSE], #dfHmLmMcWdFeqDfLabledRandm_norm)
#old code

library("e1071")
HmOrLmOrMc_svm_model <- svm(dfHmLmMcWdFeqDfLabledRandm_norm_train, dfHmLmMcWdFeqDfLabledRandm[1:120,1], type = 'C')
pred <- predict(HmOrLmOrMc_svm_model, dfHmLmMcWdFeqDfLabledRandm_norm_test)
table(pred, dfHmLmMcWdFeqDfLabledRandm[121:150,1])
#all correct





































