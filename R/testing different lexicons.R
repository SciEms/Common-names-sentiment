####################################################################################################################################
##### Testing words driving sentiment across five different lexicons
############################################################################################################################

# most common positive and negative words

bing_word_counts <- tidy_names %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

bing_word_counts

length(unique(bing_word_counts$word))

# get top 20 most frequent words (words driving sentiment)
top_word_counts <- bing_word_counts[1:20,]


# overall contribution to sentiment
bing_plot <- top_word_counts %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to overall sentiment (frequency)")+
  ggtitle("(c) 'bing'")

#same but with nrc

nrc_word_counts <- tidy_names %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE)

nrc_word_counts

length(unique(nrc_word_counts$word))

# get top 20 most frequent words (words driving sentiment)
top_word_counts2 <- subset(nrc_word_counts, sentiment %in% c("negative", "positive"))
top_word_counts3 <- top_word_counts2[1:20,]

# overall contribution to sentiment
nrc_plot <- top_word_counts3 %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to overall sentiment (frequency)")+
  ggtitle("(a) 'nrc'")

#same but with afinn

afinn_word_counts <- tidy_names %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = TRUE)

afinn_word_counts

length(unique(afinn_word_counts$word))

# get top 20 most frequent words (words driving sentiment)
afinn_word_counts$sentiment <- ifelse(afinn_word_counts$value < 0, "negative",
                                      ifelse(afinn_word_counts$value > 0, "positive", NA))
afinn_word_counts2 <- afinn_word_counts[1:20,]

# overall contribution to sentiment
afinn_plot <- afinn_word_counts2 %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to overall sentiment (frequency)")+
  ggtitle("(d) 'afinn'")

#loughran

loughran_word_counts <- tidy_names %>%
  inner_join(get_sentiments("loughran")) %>%
  count(word, sentiment, sort = TRUE)

loughran_word_counts

length(unique(get_sentiments("loughran")$word))
length(unique(loughran_word_counts$word))

# get top 20 most frequent words (words driving sentiment)
loughran_word_counts2 <- subset(loughran_word_counts, sentiment %in% c("positive", "negative"))
loughran_word_counts3 <- loughran_word_counts2[1:20,]

# overall contribution to sentiment
loughran_plot <- loughran_word_counts3 %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to overall sentiment (frequency)")+
  ggtitle("(e) 'loughran'")


#dodds
dodds_word_counts <- tidy_names %>%
  inner_join(dodds_sentiment) %>%
  count(word, happiness_average, sort = TRUE)

subset(dodds_sentiment, word == "common")

dodds_word_counts

length(unique(dodds_word_counts$word)) #1,595
length(unique(tidy_names$word)) #16,739
1595/16739*100

# get top 20 most frequent words (words driving sentiment)
dodds_word_counts$sentiment <- ifelse(dodds_word_counts$happiness_average > 5, "positive",
                                      ifelse(dodds_word_counts$happiness_average < 5, "negative", NA))
dodds_word_counts2 <- dodds_word_counts[1:20,]

# overall contribution to sentiment
dodds_plot <- dodds_word_counts2 %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to overall sentiment (frequency)")+
  ggtitle("(b) 'dodds'")


#### Figure
multiplot(nrc_plot, dodds_plot, bing_plot, afinn_plot, loughran_plot, cols = 5)
