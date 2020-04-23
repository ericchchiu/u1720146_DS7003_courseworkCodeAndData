#3 hours to run! 
#if open this code to RStudio. After opened it,
#select File -> Reopen with Encoding -> UTF-8 (if the default encoding is not UTF-8 (Windows: usually ISO 8859-1)
#Reason: need to recognised the non-ascii symbol â

#set working file
setwd(dirname(file.choose()))
getwd()

#input data
dfVictorianEraAA <- read.table('Gungor_2018_VictorianAuthorAttribution_data_train.csv', header = TRUE, sep = (','))

dfJaneAusten26674_27073 <- dfVictorianEraAA[26674:27073,]
dfJaneAusten26674_27073$textNo <- rep(1:100, each = 4)
dfJaneAusten26674_27073 <- dfJaneAusten26674_27073[c('textNo', 'text')]
#write.table(dfJaneAusten26674_27073, 'dfJaneAusten26674_27073.csv', quote = FALSE, sep = ',', row.names = FALSE)

dfJohnMuir31420_31819 <- dfVictorianEraAA[31420:31819,]
dfJohnMuir31420_31819$textNo <- rep(1:100, each = 4)
dfJohnMuir31420_31819 <- dfJohnMuir31420_31819[c('textNo', 'text')]
#write.table(dfJohnMuir31420_31819, 'dfJohnMuir31420_31819.csv', quote = FALSE, sep = ',', row.names = FALSE)

combineTwoListsAsOne <- function (list1, list2) {
n <- c()
for(x in list1){n<-c(n,x)}
for(x in list2){n<-c(n,x)}
return(n)
}

library(tokenizers)
listOfWordsJaneAusten26674_27073 <- tokenize_words(paste0(dfJaneAusten26674_27073[1,2]))
for (i in 2:400) {
listOfWordsJaneAusten26674_27073 <- combineTwoListsAsOne (listOfWordsJaneAusten26674_27073, tokenize_words(paste0(dfJaneAusten26674_27073[i,2])))
listOfWordsJaneAusten26674_27073 <- unique(listOfWordsJaneAusten26674_27073)
}

listOfWordsJohnMuir31420_31819 <- tokenize_words(paste0(dfJohnMuir31420_31819[1,2]))
for (i in 2:400) {
listOfWordsJohnMuir31420_31819 <- combineTwoListsAsOne (listOfWordsJohnMuir31420_31819, tokenize_words(paste0(dfJohnMuir31420_31819[i,2])))
listOfWordsJohnMuir31420_31819 <- unique(listOfWordsJohnMuir31420_31819)
}

listOfWordsAppearingInBothJAAndJM <- Reduce(intersect, list(listOfWordsJaneAusten26674_27073,listOfWordsJohnMuir31420_31819))

#A = function(x, y) {       #it works!
#df <- data.frame('word' = 0)
#for(i in x) {
#num = 0
#for(val in 1: nrow(y)) {
#tokenized = tokenize_words(paste0(y[val,2]))
#tokenized = unlist(tokenized, use.names=FALSE)
#num = num + length(grep(paste('\\<',i,'\\>', sep =''), tokenized))
#}
#df[paste(i)] <- c(num)
#
#return(df)
#}
A = function(x, y) {
for(k in y)
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
JA_NoOfWdsInJAnJMUniqWdLst = A(listOfWordsAppearingInBothJAAndJM, dfJaneAusten26674_27073) #more than 1.5 hour to run this line
JM_NoOfWdsInJAnJMUniqWdLst = A(listOfWordsAppearingInBothJAAndJM, dfJohnMuir31420_31819) #more than 1.5 hour to run this line
JAnJMJoin_NoOfWdsInJAnJMUniqWdLst = rbind(JA_NoOfWdsInJAnJMUniqWdLst, JM_NoOfWdsInJAnJMUniqWdLst)
JaJmTtl400OrMore = JAnJMJoin_NoOfWdsInJAnJMUniqWdLst[, colSums(JAnJMJoin_NoOfWdsInJAnJMUniqWdLst) >=400]
JaJmTtl400OrMoreByAlpha = JaJmTtl400OrMore[,order(names(JaJmTtl400OrMore))]#ok
JaJmTtl400OrMoreByAlpha = JaJmTtl400OrMoreByAlpha[-c(2)] # deleting a head
JaJmTtl400OrMoreByAlphaHeader = colnames(JaJmTtl400OrMoreByAlpha) # a list of 230 words

#remove <- c('e', 'f', 'h', 'j', 'l', 'n', 'o', 'r', 'u', 'v')
#JaJmTtl400OrMoreByAlphaHeader = setdiff(JaJmTtl400OrMoreByAlphaHeader, remove)
######################################################
#Draft: (produce df of word list for each text)

###############################################
#Formed the JA and JM wordDf (220 x 200)
JaJmTtl400OrMoreByAlphaHeader = colnames(JaJmTtl400OrMoreByAlpha)#ok
remove <- c('â', 'e', 'f', 'h', 'j', 'l', 'n', 'o', 'r', 'u', 'v')#ok
JaJmTtl400OrMoreByAlphaHeader = setdiff(JaJmTtl400OrMoreByAlphaHeader, remove)#ok
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
}#ok

dfJaWdFeqDf = A(JaJmTtl400OrMoreByAlphaHeader, dfJaneAusten26674_27073[1:4,])
dfJaWdFeqDf = dfEach4RWdNoOfOccu(JaJmTtl400OrMoreByAlphaHeader, dfJaneAusten26674_27073, dfJaWdFeqDf)
dfJmWdFeqDf = A(JaJmTtl400OrMoreByAlphaHeader, dfJohnMuir31420_31819[1:4,])
dfJmWdFeqDf = dfEach4RWdNoOfOccu(JaJmTtl400OrMoreByAlphaHeader, dfJohnMuir31420_31819, dfJmWdFeqDf)#ok
dfJaAndJmWdFeqDf <- rbind(dfJaWdFeqDf, dfJmWdFeqDf)#ok

#code for preparing the above code by using the small snnt18.csv file
#snnt18WLst = c('a', 'summer', 'the', 'i', 'and')
#dfSnnt18WdFeqDf = A(snnt18WLst, snnt18Ed[1:4,])
#haha = dfEach4RWdNoOfOccu(snnt18WLst, snnt18Ed, dfSnnt18WdFeqDf)

#####################################################
#Add label column to the beginning:
dfJaAndJmWdFeqDf$JAOrJM = c(rep('JA', 100), rep('JM', 100))
dfJaAndJmWdFeqDfLabled = dfJaAndJmWdFeqDf[,c(221,1:220)]#ok
#####################################################
#shuffling rows:
set.seed(12345)
rrowNos <- sample(nrow(dfJaAndJmWdFeqDfLabled))
dfJaAndJmWdFeqDfLabledRandm <- dfJaAndJmWdFeqDfLabled[rrowNos,]#ok
##################################################
#normalisation of columns
data_norm <- function(x) {(x- min(x))/ (max(x)- min(x))}
dfJaAndJmWdFeqDfLabledRandm_norm <- as.data.frame(lapply(dfJaAndJmWdFeqDfLabledRandm[,-1], data_norm))#ok
summary(dfJaAndJmWdFeqDfLabledRandm_norm[,1:4])
####################################################
#KNN!
library(class)
dfJaAndJmWdFeqDfLabledRandm_norm_train <- dfJaAndJmWdFeqDfLabledRandm_norm[1:160,]
dfJaAndJmWdFeqDfLabledRandm_norm_test <- dfJaAndJmWdFeqDfLabledRandm_norm[161:200,]
JaOrJm_pred <- knn(dfJaAndJmWdFeqDfLabledRandm_norm_train, dfJaAndJmWdFeqDfLabledRandm_norm_test, dfJaAndJmWdFeqDfLabledRandm[1:160,1], k= 13)
table(JaOrJm_pred, dfJaAndJmWdFeqDfLabledRandm[161:200,1])#ok

