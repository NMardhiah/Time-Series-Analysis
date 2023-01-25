library(tidyverse)
# data manipulation & plotting
# Attaching packages: ggplot2,tibble,tidyr,readr,purrr,dplyr,stringr,forcats
# Conflicts: dplyr::filter(),dplyr::lag(),masks stats::filter(),masks stats::lag()

library(stringr)
# text cleaning and regular expressions

library(tidytext)
# provides additional text mining functions
# Attaching packages: dplyr, broom, tidyr and ggplot2

library("ggplot2")

# text mining packages
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# for sentiment analysis
library("syuzhet") # to extract sentiments / plot arc from text
library("sentimentr")

# for word association use
library("qdap")
library("ggraph")
library("tidygraph")
library("igraph")


# hpcs refers to the text data imported
#  - it is used for sentiment analysis and emoticon lexicon 
# hp2_ori refers to the original text data
# hp2_s1 refers text data for cleanings done in stage 1 till delete page footer
# hp2_s2 refers text data for cleanings done till delete extra white spaces
#  - it is used for the parts to identify the occurrence of characters
#  - used for the words association
# hp2_s3 refers text data done with characters removed until text stemming
#  - it is used for the parts to identify the most frequent words in the story



# Import text data
hpcs<-readLines("Book2.txt")

# Define a corpus text
hp2_ori<-Corpus(VectorSource(hpcs))
inspect(hp2_ori)
# the range of story is 16-16752 rows
# remain the story part
hp2_ori<-hp2_ori[16:16752]

##################################################### Data cleaning on text data

# Remove numbers
hp2_s1<-tm_map(hp2_ori,removeNumbers)

# Remove punctuation
hp2_s1<-tm_map(hp2_s1,removePunctuation)

# Convert the text to lower case
hp2_s1<-tm_map(hp2_s1,content_transformer(tolower))

# Remove special characters from the text
# define a white space
toSpace<-content_transformer(function(x,pattern) gsub(pattern,"",x))
# convert all special symbols or patterns into white space
# to remove the footer
# copy from the results shown in console
hp2_s1<-tm_map(hp2_s1,toSpace,"page   harry potter and the chamber of secrets  jk rowling")
# just in case space happens between "page"
hp2_s1<-tm_map(hp2_s1,toSpace,"p a g e   harry potter and the chamber of secrets  jk rowling")
inspect(hp2_s1)

# to remove the contractions that may exist in text
hp2_s2<-tm_map(hp2_s1,toSpace,"’m")
hp2_s2<-tm_map(hp2_s2,toSpace,"’s")
hp2_s2<-tm_map(hp2_s2,toSpace,"’ve")
hp2_s2<-tm_map(hp2_s2,toSpace,"’re")
hp2_s2<-tm_map(hp2_s2,toSpace,"’ll")
hp2_s2<-tm_map(hp2_s2,toSpace,"’d")
hp2_s2<-tm_map(hp2_s2,toSpace,"’t")
hp2_s2<-tm_map(hp2_s2,toSpace,"’all")
hp2_s2<-tm_map(hp2_s2,toSpace,"ma’am")
hp2_s2<-tm_map(hp2_s2,toSpace,"mr ")
hp2_s2<-tm_map(hp2_s2,toSpace,"mrs ")
hp2_s2<-tm_map(hp2_s2,toSpace,"ms ")
# to remove other symbols seen after removePunctuation
hp2_s2<-tm_map(hp2_s2,toSpace,"—")
hp2_s2<-tm_map(hp2_s2,toSpace,"”")
hp2_s2<-tm_map(hp2_s2,toSpace,"“")
hp2_s2<-tm_map(hp2_s2,toSpace,"‘")
hp2_s2<-tm_map(hp2_s2,toSpace,"’")

# Remove the stopwords
hp2_s2<-tm_map(hp2_s2,removeWords,stopwords("en"))

# Eliminate extra unnecessary spaces
hp2_s2<-tm_map(hp2_s2,stripWhitespace)

inspect(hp2_s2)



####################################### To compare the occurrence of characters

# Document-term matrix for character use
tdm1<-TermDocumentMatrix(hp2_s2) ;tdm1
# frequency table of words
freq<-rowSums(as.matrix(tdm1)) ;freq

# import the characters name
hpcharacter<-read.csv("characters_list.csv")
# separate the full name into individual words
out<-stack(setNames(strsplit(hpcharacter$name, " "),hpcharacter$id)) ;out
# remove duplicates and lowercase
hpcharacter<-tolower(unique(out$values))
# add in the names of a professor and a cat
hpcharacter<-c(hpcharacter,"gilderoy","lockhart","norris")

# extract the characters from the frequency table
char_f<-freq[hpcharacter] ;char_f
char_f<-as.data.frame(char_f,row.names = hpcharacter)
colnames(char_f)<-"freq"
char_f<-drop_na(char_f) ;char_f
# make a data frame with two variables for easy bar plot use
char_f<-data.frame(word=row.names(char_f),freq=char_f$freq)
char_f<-char_f %>% arrange(desc(freq)) ;char_f
# to study the value of 3rd quartile
summary(char_f)
# filter out the names with frequency at least 91
main_char<-char_f %>% filter(freq>=91)

# plot the graph of frequency to visualize the characters occurrence
charfreq_plot<-barplot(main_char$freq,
                       names.arg = str_to_title(main_char$word),
                       horiz = TRUE,col = "#B6FCD5",border = "black",
                       main = "Top 25% occurence of characters and family",
                       xlab = "Frequency",xlim = c(0,2000),
                       cex.names = 0.5,las = 1)
mtext(text = "Character name or family name",side = 2,line = 3.35,cex = 0.8)
text(x=main_char$freq+50,y=charfreq_plot,main_char$freq,line=4.5,cex = 0.8)



############################### To identify the most frequent words in the story

# Remove the stopwords as of characters
hp2_s3<-tm_map(hp2_s2,removeWords,hpcharacter)

# Text stemming - reduce the word to its root form
hp2_s3<-tm_map(hp2_s3,stemDocument)

# Document-term matrix
tdm2<-TermDocumentMatrix(hp2_s3)
m<-as.matrix(tdm2)

# frequency table for words w/o character names
word_f<-sort(rowSums(m),decreasing = T) ;word_f
# arrange Document-term matrix
word_f<-data.frame(word=names(word_f),freq=word_f) ;word_f
# when check for first 10 words
word_f[1:10,]
# the word "said" occurs too often and it only means say
# so "said" (in first row) is removed from the data
word_f<-word_f[-1,] ;word_f

# to find the percentiles of the words
quantile(word_f$freq,probs = c(.5,.75,.8,.85,.9,.95,1))
# will go for the top 10% of frequent words in the story
# at 90th percentile, the frequency is 18.4

# Word cloud
set.seed(123) #for same structure
par(mar = rep(0,4)) # to make the results full in the area
wordcloud(words = word_f$word,freq = word_f$freq,
          min.freq = 19,max.words = 500,
          random.order = F,
          colors = brewer.pal(9,"YlGn"))
# max number of different colors in the palette is 9
dev.off() # to reset the plot



############################################# To study the polarity of the text

# Sentiment analysis
sentiment.scor<-get_sentiment(hpcs,method = "syuzhet") ;sentiment.scor
hist(sentiment.scor,
     main ="Histogram of Sentiment Score for the Story",
     xlab = "Sentiment Score",col = "#00BFC4")

#summary statistics
summary(sentiment.scor)
# the polarity of words are with normal distribution
# where most of them are neutral
# the mode of sentiment score is [-0.5 to 0]
# the mean is a small negative value
# the median is 0



################################# To study the words emotions exist in the text

# Emotion classification
# to calculate the presence of 8 different emotions and corresponding valence
d<-get_nrc_sentiment(hpcs) #running around 5 mins
# transpose to create data frame
td<-data.frame(t(d))
td_new<-data.frame(rowSums(td))

names(td_new)[1]<-"count"
td_combine<-cbind("sentiment"=rownames(td_new),td_new)
rownames(td_combine)<-NULL
quickplot(sentiment,data = td_combine,weight = count,
          geom = "bar",fill = sentiment,
          main="Sentiment Analysis of the Harry PotterThe Chamber of Secrets",
          xlab="Sentiment",ylab = "count")



########################### To explore the relationship of certain words in text

# Word association
# analyze the association of words from main_char
# take the words from main_char with freq >= 100
assoc_word<-main_char[main_char$freq>=100,]

tdm3<-TermDocumentMatrix(hp2_s2)
association<-findAssocs(tdm3,terms = assoc_word$word,corlimit = 0.07)
association
# list_vect2df : to convert a list of named vectors to a hierarchical dataframe
association_df<-list_vect2df(association,col1 = "main_words",
                             col2 = "words",col3 = "score",rev = TRUE)

# Data set for word association
association_df<-as_tibble(association_df)

# Create nodes list
# get distinct main_words
main_words<-association_df %>%
  distinct(main_words) %>%
  rename(label=main_words)
main_words
# get distinct words
words<-association_df %>%
  distinct(words) %>%
  rename(label=words)
words
# join the two data to create node
# add unique ID for each word
nodes<-full_join(main_words, words,by = "label") 
nodes <- nodes %>%
  mutate(id = 1:nrow(nodes)) %>%
  select(id, everything())
# everything() selects all variable

# Create edges list
# join nodes id for main_words column
edges<-association_df %>%
  left_join(nodes,by=c("main_words"="label")) %>%
  rename(from=id)
# join nodes id for words column
edges<-edges %>%
  left_join(nodes,by=c("words"="label")) %>%
  rename(to=id)
# select the columns from and to
edges<-select(edges,c(from,to,score))

# Create a network object using tidygraph
net.tidy<-tbl_graph(nodes = nodes,edges = edges,directed = TRUE)

# Visualize network using ggraph
ggraph(net.tidy,layout = "graphopt") +
  geom_node_point() +
  geom_edge_link(aes(width = score),alpha = 0.8) +
  scale_edge_width(range = c(0.2, 2)) +#c(1, 6)
  geom_node_text(aes(label = label),repel = TRUE) +
  labs(edge_width = "association_df") +
  theme_graph()
