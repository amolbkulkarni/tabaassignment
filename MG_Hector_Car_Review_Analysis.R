library(ggplot2) # for plotting
library(tidytext) # for analyzing text in tidy manner
library(dplyr) # data munging
library(wordcloud)

reviews_df <- read.csv('C:\\Users\\Prashant\\Desktop\\Car_Reviews\\Car_Reviews3.csv',stringsAsFactors = FALSE)
reviews_df$doc_id <- paste0('doc ',seq(1,nrow(reviews_df))) # we need this later on
text_df <- reviews_df %>% select(c('doc_id','brand_Name','review')) #lets focus on reviews only

head(text_df)

#---Exploratory Analysis at word level#-----#
#------------filter only jeep compass reviews-----------#
text_df <- text_df %>% filter(brand_Name=="MG Hector")
dim(text_df); head(text_df);

tidy_reviews <- text_df %>% unnest_tokens(word,review)
head(tidy_reviews);tail(tidy_reviews)

#ggplot to see frequent words used in the reviews
tidy_reviews %>%
  count(word, sort = TRUE)%>%
  filter(n >30)%>%   # n is wordcount colname.
  mutate(word = reorder(word, n)) %>%  # mutate() reorders columns & renames too
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", col = "red", fill = "red") +
  xlab(NULL) +
  coord_flip()

#remove stop words and then finding out what are the top words.
tidy_reviews %>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)%>%
  filter(n >15)%>%   # n is wordcount colname.
  mutate(word = reorder(word, n)) %>%  # mutate() reorders columns & renames too
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", col = "red", fill = "red") +
  xlab(NULL) +
  coord_flip()


#Remove Custom Stop Words
custom_stop_words <- bind_rows(stop_words, 
                               tibble(word = c("features","read","mg", "hector", "jeep", "compass", "kia", "seltos","car"), 
                                      lexicon = rep("custom", 9)))

head(custom_stop_words)

tidy_reviews %>% 
  anti_join(custom_stop_words)%>%
  count(word, sort = TRUE) %>%
  filter(n >15) %>%   # n is wordcount colname. 
  mutate(word = reorder(word, n)) %>%  # mutate() reorders columns & renames too
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", col = "red", fill = "red") +
  xlab(NULL) +
  coord_flip()

# define a nice color palette
pal <- brewer.pal(8,"Dark2")

tidy_reviews %>% 
  anti_join(custom_stop_words)%>%
  count(word, sort = TRUE) %>%
  filter(n >10)%>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))

# bigram
review_bigrams <- text_df %>%
  unnest_tokens(bigram, review, token = "ngrams", n = 2)

review_bigrams %>%
  count(bigram, sort = TRUE)

#Removing stopword at bigram level is different. first seperate both the words in bigram
# remove stopwords
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

head(bigram_counts)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

head(bigrams_united)
# Analysing Bigrams most common experience

bigrams_filtered %>%
  filter(word2 == "experience") %>%
  count(brand_Name, word1, sort = TRUE)

head(bigrams_filtered)

# find the words most distinctive to each document

bigram_tf_idf <- bigrams_united %>%
  count(doc_id, bigram) %>%
  bind_tf_idf(bigram, doc_id, n) %>%
  arrange(desc(tf_idf))

head(bigram_tf_idf)

bigrams_united%>%
  count(bigram)%>%
  with(wordcloud(bigram_tf_idf$bigram,bigram_tf_idf$n,max.words=10,min.freq = 1,random.order = F, colors=pal))
#--------------------------------------------------------------------------#
#Q2.2,2.3 & 2.4.which is mainly about consumer perception(strong/weak) towards the product feature.
library('udpipe')
library(lattice)
library(wordcloud)

library(RColorBrewer)
library(sentimentr)
library(dplyr)

# first read the data
reviews_df <- read.csv('C:\\Users\\Prashant\\Desktop\\Car_Reviews\\Car_Reviews3.csv',stringsAsFactors = FALSE)
reviews_df<- reviews_df %>% filter(brand_Name=="MG Hector")

english_model = udpipe_load_model("C:/Users/Prashant/Desktop/Car_Reviews/english-ewt-ud-2.4-190531.udpipe")  # file_model only needed
x <- udpipe_annotate(english_model, x = reviews_df$review,parser = "none",trace = FALSE) #%>% as.data.frame() %>% head()
x <- as.data.frame(x)
head(x)

#Method:1 lets check top noun words in corpus
all_nouns = x %>% subset(., xpos %in% c("NNS") ) # subset all the proper noun in corpus
top_nouns = txt_freq(all_nouns$lemma)  # txt_freq() calcs noun freqs in desc order
head(top_nouns, 5)

pal <- brewer.pal(8,"Dark2")
wordcloud(top_nouns$key,top_nouns$freq,min.freq = 2,max.words = 50,colors=pal)

x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")# recode upos to 1-letter tag for better regex pattern

stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)

stats <- subset(stats, ngram > 1 & freq >3)

stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

barchart(key ~ freq, data = head(stats, 50), col = "cadetblue", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")

#we can see some good features like: value for money, seat comfort, attractive look, longevity of the vehicle, infotainment system, good mileage,price range#

#Method:3 RAKE 
stats <- keywords_rake(x = x, term = "lemma", group = c("doc_id"), 
                       relevant = x$upos %in% c("NOUN", "ADJ"))

stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

barchart(key ~ rake, data = head(subset(stats, freq > 4), 40), col = "cadetblue", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")  
# based on above exploratory analysis, I have selected few product features/attributes
feature_list <- c('airbag', 'interiors','headlamps','exterior','headlamp','voice command', 'boot space', 'panoramic sunroof', 'infotainment system', 'price range','music system', 'front grill', 'price','voice command', 'touch screen','value for money','fog lamps')

#feature_list <- c('price range','value for money','build quality','infotainment system','driving experience')

#########Sentiment analysis##########

df <- x[,1:4] # select doc_id, par_id, sentence_id, sentence
df <- df[!duplicated(df),] # remove duplicate sentences Why? check dataframe x
head(df)

#computation of sentiment score for each sentence
sentiment<-sentiment_by(df$sentence)
df$sent_sentiment <- sentiment$ave_sentiment

#select all the sentences which contains any word from the feature list
#filter sentences based on feature list
df$feature<-NA

# extracting sentiment of features
df$sentence <- tolower(df$sentence) #to get maximum sentences

for (feature in feature_list){
  #print(i)
  df$feature <- ifelse(grepl(feature,df$sentence),feature,df$feature)
}

head(df[!is.na(df$feature),])

#Aggregating score for each feature.
df %>% select(doc_id,sent_sentiment,feature)%>%group_by(feature)%>%summarise(mean_sentiment = mean(sent_sentiment))

df%>%filter(feature=="touch screen")%>%select(sentence,sent_sentiment)
