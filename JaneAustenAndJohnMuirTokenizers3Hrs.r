#3 hours to run! 
#if open this code to RStudio. After opened it,
#select File -> Reopen with Encoding -> UTF-8 (if the default encoding is not UTF-8 (Windows: usually ISO 8859-1)
#Reason: need to recognised the non-ascii symbol â (it should be an a with a caret!)

#set working file
setwd(dirname(file.choose()))
getwd()

#input data
dfVictorianEraAA <- read.table('Gungor_2018_VictorianAuthorAttribution_data-train.csv', header = TRUE, sep = (','))

#produce two dataframes.
#one for Jane Austen's texts (JA) and one for John Muir's texts (JM)
dfJaneAusten26674_27073 <- dfVictorianEraAA[26674:27073,]
dfJaneAusten26674_27073$textNo <- rep(1:100, each = 4)
dfJaneAusten26674_27073 <- dfJaneAusten26674_27073[c('textNo', 'text')]
dfJohnMuir31420_31819 <- dfVictorianEraAA[31420:31819,]
dfJohnMuir31420_31819$textNo <- rep(1:100, each = 4)
dfJohnMuir31420_31819 <- dfJohnMuir31420_31819[c('textNo', 'text')]

#Form a unique word list of the words in texts of JA and JM
library(tokenizers)
combineTwoLists <- function (list1, list2) {
    n <- c()
    for(x in list1){n<-c(n,x)}
    for(x in list2){n<-c(n,x)}
    return(n)
}
listOfWordsJaneAusten26674_27073 <- tokenize_words(paste0(dfJaneAusten26674_27073[1,2]))
for (i in 2:400) {
listOfWordsJaneAusten26674_27073 <- combineTwoLists (listOfWordsJaneAusten26674_27073, tokenize_words(paste0(dfJaneAusten26674_27073[i,2])))
listOfWordsJaneAusten26674_27073 <- unique(listOfWordsJaneAusten26674_27073)
}

listOfWordsJohnMuir31420_31819 <- tokenize_words(paste0(dfJohnMuir31420_31819[1,2]))
for (i in 2:400) {
listOfWordsJohnMuir31420_31819 <- combineTwoLists (listOfWordsJohnMuir31420_31819, tokenize_words(paste0(dfJohnMuir31420_31819[i,2])))
listOfWordsJohnMuir31420_31819 <- unique(listOfWordsJohnMuir31420_31819)
}
listOfWordsAppearingInBothJAAndJM <- Reduce(intersect, list(listOfWordsJaneAusten26674_27073,listOfWordsJohnMuir31420_31819))

#a function for forming a dataframe with each text a row and a list of unique words the columns 
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
#apply the A function to form a dataframe for JA's texts
#the below line take more than an hour to run! 
JA_NoOfWdsInJAnJMUniqWdLst = A(listOfWordsAppearingInBothJAAndJM, dfJaneAusten26674_27073)
#apply the A function to form a dataframe for JA's texts
#the below line take more than an hour to run!
JM_NoOfWdsInJAnJMUniqWdLst = A(listOfWordsAppearingInBothJAAndJM, dfJohnMuir31420_31819)

#combine two dataframes and 
#retain those columns occurrence of the word equal or larger than 400 times
JAnJMJoin_NoOfWdsInJAnJMUniqWdLst = rbind(JA_NoOfWdsInJAnJMUniqWdLst, JM_NoOfWdsInJAnJMUniqWdLst)
JaJmTtl400OrMore = JAnJMJoin_NoOfWdsInJAnJMUniqWdLst[, colSums(JAnJMJoin_NoOfWdsInJAnJMUniqWdLst) >=400]
JaJmTtl400OrMoreByAlpha = JaJmTtl400OrMore[,order(names(JaJmTtl400OrMore))]
#?JaJmTtl400OrMoreByAlpha = JaJmTtl400OrMoreByAlpha[-c(2)] # deleting a head
JaJmTtl400OrMoreByAlphaHeader = colnames(JaJmTtl400OrMoreByAlpha) # a list of 231 words

#formed JA and JM word dataframe (200 x 220)
JaJmTtl400OrMoreByAlphaHeader = colnames(JaJmTtl400OrMoreByAlpha)
remove <- c('â', 'e', 'f', 'h', 'j', 'l', 'n', 'o', 'r', 'u', 'v')
JaJmTtl400OrMoreByAlphaHeader = setdiff(JaJmTtl400OrMoreByAlphaHeader, remove)
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
dfJaWdFeqDf = A(JaJmTtl400OrMoreByAlphaHeader, dfJaneAusten26674_27073[1:4,])
dfJaWdFeqDf = dfEach4RWdNoOfOccu(JaJmTtl400OrMoreByAlphaHeader, dfJaneAusten26674_27073, dfJaWdFeqDf)
dfJmWdFeqDf = A(JaJmTtl400OrMoreByAlphaHeader, dfJohnMuir31420_31819[1:4,])
dfJmWdFeqDf = dfEach4RWdNoOfOccu(JaJmTtl400OrMoreByAlphaHeader, dfJohnMuir31420_31819, dfJmWdFeqDf)
dfJaAndJmWdFeqDf <- rbind(dfJaWdFeqDf, dfJmWdFeqDf)

#Add a column of labels and move it to the front
dfJaAndJmWdFeqDf$JAOrJM = c(rep('JA', 100), rep('JM', 100))
dfJaAndJmWdFeqDfLabled = dfJaAndJmWdFeqDf[,c(221,1:220)]

#shuffling rows:
set.seed(12345)
rrowNos <- sample(nrow(dfJaAndJmWdFeqDfLabled))
dfJaAndJmWdFeqDfLabledRandm <- dfJaAndJmWdFeqDfLabled[rrowNos,]

#normalisation of columns
data_norm <- function(x) {(x- min(x))/ (max(x)- min(x))}
dfJaAndJmWdFeqDfLabledRandm_norm <- as.data.frame(lapply(dfJaAndJmWdFeqDfLabledRandm[,-1], data_norm))

#view normalisation summaryy of the first four columns
summary(dfJaAndJmWdFeqDfLabledRandm_norm[,1:4])

#KNN!
library(class)
dfJaAndJmWdFeqDfLabledRandm_norm_train <- dfJaAndJmWdFeqDfLabledRandm_norm[1:160,]
dfJaAndJmWdFeqDfLabledRandm_norm_test <- dfJaAndJmWdFeqDfLabledRandm_norm[161:200,]
JaOrJm_pred <- knn(dfJaAndJmWdFeqDfLabledRandm_norm_train, dfJaAndJmWdFeqDfLabledRandm_norm_test, dfJaAndJmWdFeqDfLabledRandm[1:160,1], k= 13)
table(pred_3Hrs = JaOrJm_pred, true_JaneAusten_JohnMuir_KNN = dfJaAndJmWdFeqDfLabledRandm[161:200,1])

