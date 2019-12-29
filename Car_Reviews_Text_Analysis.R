library(ggplot2) # for plotting
library(tidytext) # for analyzing text in tidy manner
library(dplyr) # data munging
library(wordcloud)
library(udpipe)
library(textrank)

## First step: Take the English udpipe model and annotate the text. Note: this takes about 3 minute
reviews_df <- read.csv('C:\\Users\\Prashant\\Desktop\\Car_Reviews\\New_car_reviews.csv',stringsAsFactors = FALSE)
reviews_df$doc_id <- paste0('doc ',seq(1,nrow(reviews_df))) # we need this later on
reviews_data <- reviews_df %>% select(c('doc_id','brand_Name','review','Review_rating')) #lets focus on reviews only
head(text_df)

#---Exploratory Analysis at word level#-----#
#------------filter only Kia Seltos reviews-----------#
text_df <- text_df %>% filter(brand_Name=="Kia Seltos")
dim(text_df); head(text_df);

tidy_reviews <- text_df %>% unnest_tokens(word,review)
head(tidy_reviews);tail(tidy_reviews)

#ggplot to see frequent words used in the reviews
tidy_reviews %>%
  count(word, sort = TRUE)%>%
  filter(n >350)%>%   # n is wordcount colname
  mutate(word = reorder(word, n)) %>%  # mutate() reorders columns & renames too
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", col = "Blue", fill = "blue") +
  xlab(NULL) +
  coord_flip()

#remove stop words and then finding out what are the top words.
tidy_reviews %>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)%>%
  filter(n >150)%>%   # n is wordcount colname.
  mutate(word = reorder(word, n)) %>%  # mutate() reorders columns & renames too
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", col = "purple", fill = "purple") +
  xlab(NULL) +
  coord_flip()


#Remove Custom Stop Words
custom_stop_words <- bind_rows(stop_words, 
                      tibble(word = c("features","read more","read","mg", "hector", "jeep", "compass", "kia", "seltos","car"), 
                      lexicon = rep("custom", 10)))

head(custom_stop_words)

tidy_reviews %>% 
  anti_join(custom_stop_words)%>%
  count(word, sort = TRUE) %>%
  filter(n >150) %>%   # n is wordcount colname. 
  mutate(word = reorder(word, n)) %>%  # mutate() reorders columns & renames too
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", col = "orange", fill = "orange") +
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

# remove stopwords
library(tidyr)
bigrams_separated <- review_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

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
head(bigrams_united,20)

# Analysing Bigrams most common experience

bigrams_filtered %>%
  filter(word2 == "experience") %>%
  count(brand_Name, word1,word2, sort = TRUE)
head(bigrams_filtered)

# find the words most distinctive to each document

bigram_tf_idf <- bigrams_united %>%
  count(doc_id, bigram) %>%
  bind_tf_idf(bigram, doc_id, n) %>%
  arrange(desc(tf_idf))
head(bigram_tf_idf)

bigrams_united%>%
  count(bigram)%>%
  with(wordcloud(bigram_tf_idf$bigram,bigram_tf_idf$n,max.words=30,min.freq = 10,random.order = F, colors=pal))

# Trigram
review_trigrams <- text_df %>%
  unnest_tokens(trigram, review, token = "ngrams", n = 3)

review_trigrams %>%
  count(trigram, sort = TRUE)

# remove stopwords
library(tidyr)
trigrams_separated <- review_trigrams %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ")

#Removing stopword at trigram level is different. first seperate both the words in bigram
# remove stopwords
trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word) %>%
  filter(!word3 %in% custom_stop_words$word)

# new trigram counts:
trigram_counts <- trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE)

head(trigram_counts,20)

trigrams_united <- trigrams_filtered %>%
  unite(trigram, word1,word2,word3, sep = " ")
head(trigrams_united,20)

# Analysing Trigrams most common experience

trigrams_filtered %>%
  filter(word3 == "experience") %>%
  count(brand_Name, word1,word2,word3, sort = TRUE)
head(trgrams_filtered)

# find the words most distinctive to each document

trigram_tf_idf <- trigrams_united %>%
  count(doc_id, trigram) %>%
  bind_tf_idf(trigram, doc_id, n) %>%
  arrange(desc(tf_idf))

head(trigram_tf_idf,20)

trigrams_united%>%
  count(trigram)%>%
  with(wordcloud(trigram_tf_idf$trigram,trigram_tf_idf$n,max.words=20,min.freq = 3,random.order = F, colors=pal))
count(trigrams_united)


#--------------------------------------------------------------------------#
#Q2.2,2.3 & 2.4.which is mainly about consumer perception(strong/weak) towards the product feature.
library('udpipe')
library(lattice)
library(wordcloud)
library(RColorBrewer)
library('sentimentr')
library(dplyr)
                                      
# first reading the data
reviews_df <- read.csv('C:\\Users\\Prashant\\Desktop\\Car_Reviews\\New_car_reviews.csv',stringsAsFactors = FALSE)
reviews_df<- reviews_df %>% filter(brand_Name=="Kia Seltos")
text_df <- reviews_df %>% select(c('doc_id','brand_Name','review','Review_rating')) #lets focus on reviews only
head(text_df)
text_df <- text_df %>% filter(brand_Name=="Kia Seltos")
dim(text_df); head(text_df);
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = text_df$review)
x <- as.data.frame(x)
                                      
############Extracting nouns only############
stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)
library(lattice)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 30), col = "cadetblue", main = "Most occurring nouns", xlab = "Freq")
                                      
#######Collocation & co-occurrences########
stats <- keywords_collocation(x = x, 
term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),ngram_max = 5)
## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ","ADV")), 
                    term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = x$lemma, relevant = x$upos %in% c("NOUN", "ADJ","ADV"))
## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
stats <- cooccurrence(x = x$lemma, relevant = x$upos %in% c("NOUN", "ADJ","ADV"), skipgram = 2)
head(stats,20)
                                      
#Visualization
library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(stats, 40)
wordnetwork <- graph_from_data_frame(wordnetwork)
wordnetwork
ggraph(wordnetwork, layout = "fr") +
        geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "royalblue") +
        geom_node_text(aes(label = name), col = "darkgreen", size = 9) +
        theme_graph(base_family = "Arial Narrow") +
        theme(legend.position = "none") +
        labs(title = "Cooccurrences within 3 words distance for Kia Seltos", subtitle = "Nouns & Adjective")
                                      
#Option 3: Textrank (word network ordered by Google Pagerank) 
                                      
stats <- textrank_keywords(x$lemma, 
relevant = x$upos %in% c("NOUN", "ADJ"), ngram_max = 8, sep = " ")
stats <- subset(stats$keywords, ngram > 1 & freq >= 10)
library(wordcloud)
wordcloud(words = stats$keyword, freq = stats$freq)
                                      
#Option 4: Rapid Automatic Keyword Extraction: RAKE
                                      
stats <- keywords_rake(x = x, term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                        relevant = x$upos %in% c("NOUN", "ADJ"),ngram_max = 4)
head(subset(stats, freq > 3),21)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
                                      
barchart(key ~ rake, data = head(subset(stats, freq > 3), 21), col = "cadetblue",size=20,
             main = "Keywords identified by RAKE for Kia Seltos",
             xlab = "Rake Score")
                                      
#Option 5: Phrases
## Simple noun phrases (a adjective+noun, pre/postposition, optional determiner and another adjective+noun)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = x$token, 
                    pattern = "(A|N)+N(P+D*(A|N)*N)*", 
                    is_regex = TRUE, ngram_max = 4, detailed = FALSE)
                    head(subset(stats, ngram > 2),20)
                                      
#Option 6: Use dependency parsing output to get the nominal subject and the adjective of it
                                      
stats <- merge(x, x, by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
                     by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
                     all.x = TRUE, all.y = FALSE, 
                     suffixes = c("", "_parent"), sort = FALSE)
                     stats <- subset(stats, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))
                     stats$term <- paste(stats$lemma_parent, stats$lemma, sep = " ")
                     stats <- txt_freq(stats$term)
                     library(wordcloud)
                     wordcloud(words = stats$key, freq = stats$freq, min.freq = 2, max.words = 50,
                     random.order = FALSE, colors = brewer.pal(6, "Dark2"))
                                      
# based on above exploratory analysis, I have selected few product features/attributes
#feature_list <- c('airbag', 'interiors','headlamps','exterior','headlamp','voice command', 'boot space', 'panoramic sunroof', 'infotainment system', 'price range','music system', 'front grill', 'price','voice command', 'touch screen','value for money','fog lamps')
feature_list <- c('build quality','engine performance','airbags','driving experience','price range','overall comfort','maintenance','looks','music system','air purifier','infotainment system','boot space','touch screen','comfortable seats','value for money','brakes','mileage','smooth steering','ground clearance')

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
                                      
head(df[!is.na(df$feature),],20)
                                      
#Aggregating score for each feature.
df %>% select(doc_id,sent_sentiment,feature)%>%group_by(feature)%>%summarise(mean_sentiment = mean(sent_sentiment))%>%
arrange(desc(mean_sentiment))
                                      
                          
df%>%filter(feature=="driving experience")%>%select(sentence,sent_sentiment)
                                      
#--------------------------------------------------------------------------#
# For Q2.4.which is about Attributes/ features which are best evoking customer's emotional response 

#getting the reviews from data collected in csv earlier
df_car_review <- select(reviews_data, brand_Name, review)

#tokenizing the reviews with filtering words which has length less than3 and grouping by 3 different brands we have. At last removing all the stopwords from corpus.
tidy_car_reviews <- df_car_review %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, review)%>%
  filter(nchar(word) > 3)%>%
  group_by(brand_Name) %>%
  anti_join(custom_stop_words)%>%
  ungroup()


#Option 1: Extracting only nouns
stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)
library(lattice)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 30), col = "cadetblue", main = "Most occurring nouns", xlab = "Freq")

#Option 2: Collocation & co-occurrences
## Collocation (words following one another)
stats <- keywords_collocation(x = x,
                              term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)
## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")),
                      term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))

#creating the dataset by applying sentiments from tidyr package and nrc keyset
car_review_bing <- tidy_car_reviews %>%
  inner_join(get_sentiments("nrc"))

#filtering the dataset we got by applying get_sentiments to remove stopwords and adding count of each occurrance and leaving all other sentiments other than positives and negatives.
review_polarity_bing <- car_review_bing %>%
  anti_join(custom_stop_words)%>%
  group_by(brand_Name,word, sentiment) %>%
  count(word, sentiment) %>%
  filter(sentiment %in% c("positive",
                          "negative"))

#finally plotting the data with ggplot to segregate positive and negative sentiments by different colors and applying other filters to get clean meaningfull insights.
stats%>%left_join(review_polarity_bing, by=c(term1='word'))%>%
  group_by(brand_Name)%>%
  filter(!term1 %in% custom_stop_words$word)%>%
  filter(!term2 %in% custom_stop_words$word)%>%
  filter(!term2 %in% review_polarity_bing$word)%>%
  #filter(term2 %in% c('airbag', 'interiors','headlamps','exterior','headlamp','voice', 'command', 'boot', 'space', 'panoramic', 'sunroof', 'infotainment', 'system', 'price range','music', 'grill', 'price','touchscreen','money','fog'))%>%
  filter(!term2 %in% c('comparison','cars','nice','Suv','road','angle','highway','time','owner','cruiser','initial','multiple','Nice','product','similar','round','tag','bad','initial','pleasure','licence','dream'))%>%
  filter(cooc>80)%>%
  filter(!nchar(sentiment)<4)%>%
  ggplot(aes(term2,n,fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() + theme(legend.position = "none") +
  xlab(NULL) +
  ggtitle("Sentiment by features") +
  coord_flip()+
  facet_wrap(~brand_Name, ncol = 1, scales = "free_x")     
