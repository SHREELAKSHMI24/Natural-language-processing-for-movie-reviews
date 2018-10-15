rm(list=ls())
library(stringr)
install.packages("tm")
library(tm)
library(slam)
#loading the train data 
df = read.table(file = 'file:///C:/Users/madara/Desktop/labeledTrainData.tsv', sep = '\t', header = TRUE)
df$review = str_trim(df$review)
df = data.frame(df[1:10,3])

#renaming the variable which contains the comment and 
#converting it to character data type
names(df) = "review"
str(df$review)
df$review = as.character(df$review)
str(df$review)


# converting the text format to corpus  
dfcorpus = Corpus(VectorSource(df$review))
dfcorpus
writeLines(as.character(dfcorpus[[4]]))

#converting all upper cases to lower cases
dfcorpus = tm_map(dfcorpus,tolower)
writeLines(as.character(dfcorpus[[4]]))

#removing stopowrds 
dfcorpus = tm_map(dfcorpus , removeWords , stopwords('english'))
writeLines(as.character(dfcorpus[[4]]))

#removing punctuations
dfcorpus = tm_map(dfcorpus,removePunctuation)
writeLines(as.character(dfcorpus[[4]]))

#removing numbers
dfcorpus = tm_map(dfcorpus,removeNumbers)

#removing white spaces
dfcorpus = tm_map(dfcorpus,stripWhitespace)
writeLines(as.character(dfcorpus[[4]]))

#removing plain texts 
dfcorpus = tm_map(dfcorpus,PlainTextDocument)

#converting it to document matrix format
dfcorpus = Corpus(VectorSource(df$review))
tdm = TermDocumentMatrix(dfcorpus)

#again converting to text format for renaming
inspect(tdm[155:160,1:5])
wordfrequ = rollup(tdm, 2 , na.rm=TRUE , fun = sum)
wordfrequ = as.matrix(wordfrequ)
wordfrequ = data.frame(wordfrequ)
wordfrequ$words = row.names(wordfrequ)
row.names(wordfrequ) = NULL 
wordfrequ = wordfrequ[,c(2,1)]
names(wordfrequ) = c("words", "frequency")

#finding the frequent stop words
findFreqTerms(tdm,100)

#removing the most frequent stop words
dfcorpus = tm_map(dfcorpus,removeWords, c("about","and", "are","but","can","for","etc","had","have","him","his","just","may","maybe","most",stopwords("english")))

df_tdm = data.frame(tdm)
summary(tdm)
tdm

#feeding the data for sentimental analysis 
install.packages("RSentiment")
library(RSentiment)
dfsenti = calculate_sentiment(df$review) 

#STORING THE OUTPUT IN CSV FORMAT
write.csv(dfsenti,"output.csv", row.names = T)
