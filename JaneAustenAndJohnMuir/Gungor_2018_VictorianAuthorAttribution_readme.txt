=========== HEADER ===========
Readme.txt for "Fifty Victorian Era Novelists Authorship Attribution Data"
Item Handle: https://dataworks.iupui.edu/handle/11243/23
DOI: http://dx.doi.org/10.7912/D2N65J
Documentation written on 20180517, 20180601
By Heather Coates, DataWorks Repository Manager
Revised by Abdulmecit Gungor, investigator

=========== SUGGESTED DATA CITATION ===========

Please cite this data in the references for any publication which uses it. 
Gungor, A. (2018). Fifty Victorian Era Novelists Authorship Attribution Data. IUPUI University Library. http://dx.doi.org/10.7912/D2N65J


=========== PRIMARY STUDY INFORMATION ===========
ACKNOWLEDGEMENTS
Project title: Fifty Victorian Era Novelists Authorship Attribution Data
Investigator Name: Abdulmecit Gungor
Investigator Institution: Indiana University Purdue University Indianapolis (IUPUI) School of Science, Department of Computer & Information Science
Investigator Email: mgungor@iu.edu
Investigator Role (related to this dataset): graduate student investigator

DATA SOURCE
Google Big Query https://cloud.google.com/bigquery/public-data/gdelt-books
The data was extracted through https://blog.gdeltproject.org/ using Google Big Query. This dataset is publicly available for anyone to use under the following terms provided by the Dataset Source —http://gdeltproject.org/about.html. The GDELT Project is an open platform for research and analysis of global society and thus all datasets released by the GDELT Project are available for unlimited and unrestricted use for any academic, commercial, or governmental use of any kind without fee.

DATA PREPARATION
Workflow at https://github.com/agungor2/Authorship_Attribution
Data were prepared for processing using the procedures described in the thesis "Benchmarking Authorship Attribution Over a Thousand Books By Victorian Era Authors", Section 3.1 

DATE(S) of DATA COLLECTION
Data were retrieved November, 2016

FILE DESCRIPTION: DATA DICTIONARY
Name, Size, Bytes, Class
WW, 50x3500, 1400000, double
aid, 93600x1, 748800, double
bid, 93600x1, 748800, double
ind, 93600x1, 748800, double
shortened_vocab, 1x10000, 1254644, cell
test_ind, 93600x1, 93600, logical
tfidf, 1113x50920, 453391680, double
train_ind, 93600x1, 93600, logical
txt_pieces, 93600x1000, 748800000, double
vocab, 1x50920, 6387934, cell

VARIABLE NAMING CONVENTIONS
Name, Description
WW, Author Word list
aid, Author Id
bid, Book Id
ind, Index numbers
shortened_vocab, 10000 Vocabulary list
test_ind, Testing Indexes
tfidf, Tfidf Scores
train_ind, Training Indexes
txt_pieces, All one hot encoded data
vocab, All vocabulary list


=========== THESIS PROJECT ===========
ABSTRACT
Authorship attribution is the process of identifying the author of a given text and from the machine learning perspective, it can be seen as a classification problem. To create the largest publicly available authorship attribution dataset we've extracted the works of 50 well-known Victorian-era authors. All of these extracted works are novels. In order to create non-exhaustive learning problem, we've provided 45 authors in training and 50 authors in the testing data. 5 missing authors in testing consist of %34 of all testing set. Each instance then represented with a 1000 word pieces for each author. There are 93600 text piece instance in total each which consist of 1000 words. To make the problem a bit more challenging, we've separated different books for both training and testing.  We have performed 5 main feature extraction technique on this data and compared the performance of such features within different classifiers and deep learning structures. The usage of Word2Vec in authorship attribution problem is also introduced with two main approaches: author based Word2Vec training and treating each author's text pieces individually. Support vector machine classifiers with nu-SVC type is observed to give best success rates on the stacked useful feature set. 

RESEARCH AIMS
The main purpose of this work is to lay the foundations of feature extraction techniques in Authorship Attribution problems. These are lexical, character-level, syntactic, semantic, application specific features. In order to showcase each of these feature extraction techniques  we have aimed to offer a new data resource for the author attribution research community and demonstrated them with examples. These examples can be found through https://github.com/agungor2/Authorship_Attribution. The dataset we have introduced consists of works of Victorian era authors and the main feature extraction techniques.
 
METHODS
To decrease the bias and create a reliable authorship attribution dataset the following criteria have been chosen to filter out authors in Gdelt database: English language writing authors, authors that have enough books available (at least 5), 19th century authors. With these criteria 50 authors have been selected and their books were queried through Big Query Gdelt database. The next task has been cleaning the dataset due to OCR reading problems in the original raw form. To achieve that, firstly all books have been scanned through to get the overall number of unique words and each words frequencies. While scanning the texts, the first 500 words and the last 500 words have been removed to take out specific features such as the name of the author, the name of the book and other word specific features that could make the classification task easier. After this step, we have chosen top 10,000 words that occurred in the whole 50 authors text data corpus. The words that are not in top 10,000 words were removed while keeping the rest of the sentence structure intact. Afterwards, the words are represented with numbers from 1 to 10,000 reverse ordered according to their frequencies. The entire book is split into text fragments with 1000 words each. We separately maintained author and book identification number for each one of them in different arrays. Text segments with less than 1000 words were filled with zeros to keep them in the dataset as well. 1000 words make approximately 2 pages of writing, which is long enough to extract a variety of features from the document. The reason why we have represented top 10,000 words with numbers is to keep the anonymity of texts and allow researchers to run feature extraction techniques faster. Dealing with large amounts of text data can be more challenging than numerical data for some feature extraction techniques.


=========== CREDITS ===========
Template provided by Indiana University UITS Research Storage, Indiana University Bloomington Libraries, IUPUI University Library

