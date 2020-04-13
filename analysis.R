#####################################################################################
#   Analysis script for common names sentiment paper
###################################################################################

# code used for accepted paper (Gregg et al. 2020), DOI: http://dx.doi.org/10.1080/10871209.2020.1753132

# resources used for coding
# https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html

# set working directory to project directory

# load packages
library(devtools)
library(readxl)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(ochRe)
library(gridExtra)
library(textdata)
library(grid)
library(ggplotify)

install_version("lexicon", version = "0.7.4", repos = "http://cran.us.r-project.org")
library(lexicon)

# read functions
source("R/multiplot.R") # allows for pasting together multiple different plots

# read and process raw data
source("R/01_load_and_clean.R")

# load clean data
cn3 <- read.csv("data/names_clean.csv")

# process data
cn3$common_name_eng <- as.character(cn3$common_name_eng) # make names class character

tidy_names <- cn3 %>%
  unnest_tokens(word, common_name_eng) # unnest words from names

# count of names with 'common' in
com <- subset(tidy_names, word == "common")
summary(com$red_list_status)

# test different lexicons (see Table 1 in paper)
source("R/testing different lexicons.R") # select 1 to download dataset

# determining sample with 'dodds' and 'nrc' lexicons (Table 1)

# IUCN English common names
total_unique_words <- length(unique(tidy_names$word)) #16,739
total_species <- length(unique(tidy_names$species_ID)) #38,694

# dodds
dodds_word_counts <- tidy_names %>%
  inner_join(dodds_sentiment)
dodds_word_counts

dodds_unique_words <- length(unique(dodds_word_counts$word)) #1,595 unique words
dodds_prop_w <- (dodds_unique_words/total_unique_words)*100 #9.53% of unique words in common names
dodds_species <- length(unique(dodds_word_counts$species_ID)) #25,681
dodds_prop_sp <- (dodds_species/total_species)*100 #66.37% of species with english common names on IUCN Red List

# nrc
nrc_word_counts <- tidy_names %>%
  inner_join(get_sentiments('nrc'))

#subset to exclude 'positive' and 'negative' categories
nrc2_word_counts <- subset(nrc_word_counts, !(sentiment %in% c("negative", "positive")))

nrc_unique_words <- length(unique(nrc2_word_counts$word)) #545 unique words
nrc_prop_w <- (nrc_unique_words/total_unique_words)*100 #3.26% of unique words in common names
nrc_species <- length(unique(nrc2_word_counts$species_ID)) #12,745
nrc_prop_sp <- (nrc_species/total_species)*100 #32.94% of species with english common names on IUCN Red List

# together
dodds <- subset(dodds_word_counts, select = c("word", "happiness_average", "species_ID", "red_list_status", "class"))
nrc <- subset(nrc2_word_counts, select = c("word", "sentiment", "species_ID", "red_list_status", "class"))
both_word_counts <- merge(dodds, nrc, by = c("word", "species_ID", "red_list_status", "class"), all = TRUE)

both_unique_words <- length(unique(both_word_counts$word)) #1855 unique words
both_prop_w <- (both_unique_words/total_unique_words)*100 #11.08% of unique words in common names
both_species <- length(unique(both_word_counts$species_ID)) #26,794
both_prop_sp <- (both_species/total_species)*100 #69.25% of species with english common names on IUCN Red List

# distribution of word sample (Table 1)
# across threat status
exp_prop_TS <- summary(tidy_names$red_list_status)/length(tidy_names$word)

dodds_prop_TS <- summary(dodds_word_counts$red_list_status)/length(dodds_word_counts$word)
chisq.test(dodds_prop_TS, exp_prop_TS)

nrc_prop_TS <- summary(nrc2_word_counts$red_list_status)/length(nrc2_word_counts$word)
chisq.test(nrc_prop_TS, exp_prop_TS)

both_prop_TS <- summary(both_word_counts$red_list_status)/length(both_word_counts$word)
chisq.test(both_prop_TS, exp_prop_TS)

# across taxonomic class
exp_prop_class <- summary(tidy_names$class)/length(tidy_names$word)

dodds_prop_class <- summary(dodds_word_counts$class)/length(dodds_word_counts$word)
chisq.test(dodds_prop_class, exp_prop_class)

nrc_prop_class <- summary(nrc2_word_counts$class)/length(nrc2_word_counts$word)
chisq.test(nrc_prop_class, exp_prop_class)

both_prop_class <- summary(both_word_counts$class)/length(both_word_counts$word)
chisq.test(both_prop_class, exp_prop_class)

# overall word frequency
top_words <- tidy_names %>%
  count(word, sort = TRUE)

top_words2 <- head(top_words, 100)

top_analysed <- subset(both_word_counts, word %in% top_words2$word)

length(unique(top_analysed$word))

not_analysed <- subset(top_words2, !(word %in% both_word_counts$word))
not_analysed$type <- as.factor(c("morphology", "morphology", "taxa", "taxa", "taxa", "taxa", "taxa",
                       "morphology", "taxa", "taxa", "morphology", "morphology", "taxa", "morphology",
                       "morphology", "taxa", "morphology", "morphology", "taxa", "morphology", "morphology",
                       "taxa", "morphology", "taxa", "morphology", "taxa", "taxa", "colour",
                       "morphology", "taxa", "morphology", "taxa", "taxa", "morphology", "taxa",
                       "taxa", "taxa", "morphology", "taxa", "taxa", "taxa", "taxa",
                       "taxa", "taxa"))
head(not_analysed, 44)

(summary(not_analysed$type)/44)*100

# get stat for % taxa words in top 100 frequency
top_words3 <- merge(not_analysed, top_words2, by = c("word", "n"), all = TRUE)
to_do <- subset(top_words3, is.na(top_words3$type))
to_do$type <- as.character(c("place", "morphology", "taxa", "colour", "colour", "colour", "habitat", "misc", "taxa", "taxa",
                "place", "taxa", "morphology", "morphology", "morphology", "taxa", "behaviour", "habitat",
                "taxa", "misc", "morphology", "colour", "colour", "colour", "habitat", "morphology", "habitat",
                "morphology", "misc", "misc", "morphology", "morphology", "taxa", "habitat", "taxa", "place",
                "colour", "taxa", "taxa", "taxa", "colour", "habitat", "habitat", "habitat", "taxa", "morphology",
                "morphology", "taxa", "place", "taxa", "habitat", "misc", "habitat", "place", "colour", "colour"))
top_words4 <- rbind(to_do, top_words3)
top_words5 <- subset(top_words4, !is.na(top_words4$type))
summary(as.factor(top_words5$type))

#######################################################################################################################
# top and bottom  HS words with high frequency
#######################################################################################################################
dodds_word_counts <- tidy_names %>%
  inner_join(dodds_sentiment) %>%
  count(word, happiness_average, sort = TRUE)

#get top 10 pos and neg words, freq > 155

bot10 <- head(subset(dodds_word_counts[order(dodds_word_counts$happiness_average, decreasing = FALSE),], n>155 & happiness_average < 4.5), 10)
top10 <- head(subset(dodds_word_counts[order(dodds_word_counts$happiness_average, decreasing = TRUE),], n>155 & happiness_average > 5.5), 10)

ove20 <- rbind(bot10, top10)
ove20$n2 <- ifelse(ove20$happiness_average < 5, -ove20$n, ove20$n)

overallplot <- ove20 %>%
  mutate(n = ifelse(happiness_average < 5, -n, n)) %>%
  mutate(word = reorder(word, n2)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = ochre_palettes[["winmar"]][c(rep(3, 10), rep(2,10))]) +
  coord_flip(ylim = c(-ove20$n, ove20$n), xlim = c(0.5, 20.5), expand = TRUE, clip = "off") +
  labs(title = "(a) Overall", y = "Frequency")+
  annotate("text", x = 1, y = 1600, label = "freq > 155", size = 4)+
  theme_bw()+
  annotate(geom = "text", x = -2, y = c(-750, 750), label = c("Low Happiness", "High Happiness"), size = 4.5)+
  annotate(geom = "text", x = -0.5, y = c(-1500, -1000, -500, 0, 500, 1000, 1500), label = c("1500", "1000", "500","0", "500", "1000", "1500"), size = 3.5)+
  theme(plot.margin = unit(c(1, 1, 1.5, 0.5), "cm"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(vjust=-4),
        axis.text.y = element_text(size = 16),
        plot.title = element_text(size = 22))

###################################################
#by class
###################################################

summary(tidy_names$class) #plots for ones with >10,000 words

### Actinopterygii

actino_names <- subset(tidy_names, class == "ACTINOPTERYGII")

actino_dodds <- subset(actino_names) %>%
  inner_join(dodds_sentiment) %>%
  count(word, happiness_average, sort = TRUE)

#get top 10 pos and neg words, freq > 100
bot10 <- head(subset(actino_dodds[order(actino_dodds$happiness_average, decreasing = FALSE),], n>15 & happiness_average < 4.5), 10)
top10 <- head(subset(actino_dodds[order(actino_dodds$happiness_average, decreasing = TRUE),], n>15 & happiness_average > 5.5), 10)

act20 <- rbind(bot10, top10)
act20$n2 <- ifelse(act20$happiness_average < 5, -act20$n, act20$n)

actino_plot <- act20 %>%
  mutate(n = ifelse(happiness_average < 5, -n, n)) %>%
  mutate(word = reorder(word, n2)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = ochre_palettes[["winmar"]][c(rep(3, 10), rep(2,10))]) +
  coord_flip(ylim = c(-act20$n, act20$n), xlim = c(0.5, 20.5), expand = TRUE, clip = "off") +
  annotate("text", x = 1, y = 190, label = "freq > 15", size = 4)+
  annotate("text", x = 22.5, y = -260, label = "(b)", size = 10)+
  theme_bw()+
  annotate(geom = "text", x = -2, y = c(-100, 100), label = c("Low Happiness", "High Happiness"), size = 4.5)+
  annotate(geom = "text", x = -0.5, y = c(-200, -100, 0, 100, 200), label = c("-200", "-100", "0", "100", "200"), size = 3.5)+
  theme(plot.margin = unit(c(1, 1, 1.5, 0.5), "cm"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(vjust=-4),
        axis.text.y = element_text(size = 16),
        plot.title = element_text(size = 20))+
  labs(title = "(i) Actinopterygii (ray-finned fishes)", y = "Frequency")

### Amphibia

amphi_names <- subset(tidy_names, class == "AMPHIBIA")

amphi_dodds <- subset(amphi_names) %>%
  inner_join(dodds_sentiment) %>%
  count(word, happiness_average, sort = TRUE)

#get top 10 pos and neg words, freq > 100
bot10 <- head(subset(amphi_dodds[order(amphi_dodds$happiness_average, decreasing = FALSE),], n>5 & happiness_average < 4.5), 10)
top10 <- head(subset(amphi_dodds[order(amphi_dodds$happiness_average, decreasing = TRUE),], n>5 & happiness_average > 5.5), 10)

amp20 <- rbind(bot10, top10)
amp20$n2 <- ifelse(amp20$happiness_average < 5, -amp20$n, amp20$n)

amphi_plot <- amp20 %>%
  mutate(n = ifelse(happiness_average < 5, -n, n)) %>%
  mutate(word = reorder(word, n2)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = ochre_palettes[["winmar"]][c(rep(3, 10), rep(2,10))]) +
  coord_flip(ylim = c(-amp20$n, amp20$n), xlim = c(0.5, 20.5), expand = TRUE, clip = "off") +
  annotate("text", x = 1, y = 135, label = "freq > 5", size = 4)+
  theme_bw()+
  annotate(geom = "text", x = -2, y = c(-100, 100), label = c("Low Happiness", "High Happiness"), size = 4.5)+
  annotate(geom = "text", x = -0.5, y = c(-200, -100, 0, 100, 200), label = c("-200", "-100", "0", "100", "200"), size = 3.5)+
  theme(plot.margin = unit(c(1, 1, 1.5, 0.5), "cm"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(vjust=-4),
        axis.text.y = element_text(size = 16))+
  labs(title = "(ii) Amphibia (amphibians)", y = "Frequency", size = 20)

### Aves

aves_names <- subset(tidy_names, class == "AVES")

aves_dodds <- subset(aves_names) %>%
  inner_join(dodds_sentiment) %>%
  count(word, happiness_average, sort = TRUE)

#get top 10 pos and neg words, freq > 100
bot10 <- head(subset(aves_dodds[order(aves_dodds$happiness_average, decreasing = FALSE),], n>15 & happiness_average < 4.5), 10)
top10 <- head(subset(aves_dodds[order(aves_dodds$happiness_average, decreasing = TRUE),], n>15 & happiness_average > 5.5), 10)

ave20 <- rbind(bot10, top10)
ave20$n2 <- ifelse(ave20$happiness_average < 5, -ave20$n, ave20$n)

aves_plot <- ave20 %>%
  mutate(n = ifelse(happiness_average < 5, -n, n)) %>%
  mutate(word = reorder(word, n2)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = ochre_palettes[["winmar"]][c(rep(3, 10), rep(2,10))]) +
  coord_flip(ylim = c(-ave20$n, ave20$n), xlim = c(0.5, 20.5), expand = TRUE, clip = "off") +
  annotate("text", x = 1, y = 430, label = "freq > 15", size = 4)+
  theme_bw()+
  annotate(geom = "text", x = -2, y = c(-250, 250), label = c("Low Happiness", "High Happiness"), size = 4.5)+
  annotate(geom = "text", x = -0.5, y = c(-250, 0, 250), label = c("-250", "0", "250"), size = 3.5)+
  theme(plot.margin = unit(c(1, 1, 1.5, 0.5), "cm"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(vjust=-4),
        axis.text.y = element_text(size = 16))+
  labs(title = "(iii) Aves (birds)", y = "Frequency", size = 20)

### Mammalia

mam_names <- subset(tidy_names, class == "MAMMALIA")

mam_dodds <- subset(mam_names) %>%
  inner_join(dodds_sentiment) %>%
  count(word, happiness_average, sort = TRUE)

#get top 10 pos and neg words, freq > 100
bot10 <- head(subset(mam_dodds[order(mam_dodds$happiness_average, decreasing = FALSE),], n>20 & happiness_average < 4.5), 10)
top10 <- head(subset(mam_dodds[order(mam_dodds$happiness_average, decreasing = TRUE),], n>20 & happiness_average > 5.5), 10)

mam20 <- rbind(bot10, top10)
mam20$n2 <- ifelse(mam20$happiness_average < 5, -mam20$n, mam20$n)

mam_plot <- mam20 %>%
  mutate(n = ifelse(happiness_average < 5, -n, n)) %>%
  mutate(word = reorder(word, n2)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = ochre_palettes[["winmar"]][c(rep(3, 10), rep(2,10))]) +
  coord_flip(ylim = c(-mam20$n, mam20$n), xlim = c(0.5, 20.5), expand = TRUE, clip = "off") +
  annotate("text", x = 1, y = 850, label = "freq > 20", size = 4)+
  theme_bw()+
  annotate(geom = "text", x = -2, y = c(-500, 500), label = c("Low Happiness", "High Happiness"), size = 4.5)+
  annotate(geom = "text", x = -0.5, y = c(-500, 0, 500), label = c("-500", "0", "500"), size = 3.5)+
  theme(plot.margin = unit(c(1, 1, 1.5, 0.5), "cm"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(vjust=-4),
        axis.text.y = element_text(size = 16))+
  labs(title = "(iv) Mammalia (mammals)", y = "Frequency", size = 20)

### Reptilia

reptilia_names <- subset(tidy_names, class == "REPTILIA")

rep_dodds <- subset(reptilia_names) %>%
  inner_join(dodds_sentiment) %>%
  count(word, happiness_average, sort = TRUE)

#get top 10 pos and neg words, freq > 100
bot10 <- head(subset(rep_dodds[order(rep_dodds$happiness_average, decreasing = FALSE),], n>25 & happiness_average < 4.5), 10)
top10 <- head(subset(rep_dodds[order(rep_dodds$happiness_average, decreasing = TRUE),], n>25 & happiness_average > 5.5), 10)

rep20 <- rbind(bot10, top10)
rep20$n2 <- ifelse(rep20$happiness_average < 5, -rep20$n, rep20$n)

rep_plot <- rep20 %>%
  mutate(n = ifelse(happiness_average < 5, -n, n)) %>%
  mutate(word = reorder(word, n2)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = ochre_palettes[["winmar"]][c(rep(3, 10), rep(2,10))]) +
  coord_flip(ylim = c(-rep20$n, rep20$n), xlim = c(0.5, 20.5), expand = TRUE, clip = "off") +
  annotate("text", x = 1, y = 1550, label = "freq > 25", size = 4)+
  theme_bw()+
  annotate(geom = "text", x = -2, y = c(-750, 750), label = c("Low Happiness", "High Happiness"), size = 4.5)+
  annotate(geom = "text", x = -0.5, y = c(-1000, 0, 1000), label = c("-1000", "0", "1000"), size = 3.5)+
  theme(plot.margin = unit(c(1, 1, 1.5, 0.5), "cm"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(vjust=-4),
        axis.text.y = element_text(size = 16))+
  labs(title = "(v) Reptilia (reptiles)", y = "Frequency", size = 20)

##################################
# by threat status
##################################

# Extinct

EX_names <- subset(tidy_names, red_list_status == "EX")

EX_dodds <- subset(EX_names) %>%
  inner_join(dodds_sentiment) %>%
  count(word, happiness_average, sort = TRUE)

#get top 10 pos and neg words, freq > 100
bot10 <- head(subset(EX_dodds[order(EX_dodds$happiness_average, decreasing = FALSE),], n>0 & happiness_average < 4.5), 10)
top10 <- head(subset(EX_dodds[order(EX_dodds$happiness_average, decreasing = TRUE),], n>0 & happiness_average > 5.5), 10)

EX20 <- rbind(bot10, top10)
EX20$n2 <- ifelse(EX20$happiness_average < 5, -EX20$n, EX20$n)

EX_plot <- EX20 %>%
  mutate(n = ifelse(happiness_average < 5, -n, n)) %>%
  mutate(word = reorder(word, n2)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = ochre_palettes[["winmar"]][c(rep(3, 10), rep(2,10))]) +
  coord_flip(ylim = c(-EX20$n, EX20$n), xlim = c(0.5, 20.5), expand = TRUE, clip = "off") +
  annotate("text", x = 1, y = 40, label = "freq > 0", size = 4)+
  theme_bw()+
  annotate(geom = "text", x = -1, y = c(-20, 20), label = c("Low Happiness", "High Happiness"), size = 4.5)+
  annotate(geom = "text", x = -0.5, y = c(-20, 0, 20), label = c("-20", "0", "20"), size = 3.5)+
  theme(plot.margin = unit(c(1, 1, 1.5, 0.5), "cm"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(vjust=-3),
        axis.text.y = element_text(size = 16))+
  annotate("text", x = 22.5, y = -50, label = "(c)", size = 10)+
  labs(title = "(i) Extinct", y = "Frequency")

# Extinct in the Wild

EW_names <- subset(tidy_names, red_list_status == "EW")

EW_count <- subset(EW_names) %>%
  inner_join(dodds_sentiment)

length(unique(EW_count$species_ID))
summary(EW_count$happiness_average)

EW_dodds <- subset(EW_names) %>%
  inner_join(dodds_sentiment) %>%
  count(word, happiness_average, sort = TRUE)

#get top 10 pos and neg words, freq > 100
bot10 <- head(subset(EW_dodds[order(EW_dodds$happiness_average, decreasing = FALSE),], n>0 & happiness_average < 4.5), 10)
top10 <- head(subset(EW_dodds[order(EW_dodds$happiness_average, decreasing = TRUE),], n>0 & happiness_average > 5.5), 10)

EW20 <- top10
EW20$n2 <- ifelse(EW20$happiness_average < 5, -EW20$n, EW20$n)

EW_plot <- EW20 %>%
  mutate(n = ifelse(happiness_average < 5, -n, n)) %>%
  mutate(word = reorder(word, n2)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = ochre_palettes[["winmar"]][c(rep(2, 10))]) +
  coord_flip(ylim = c(-EW20$n, EW20$n), xlim = c(0.5, 10), expand = TRUE, clip = "off") +
  annotate("text", x = 1, y = 15, label = "freq > 0", size = 4)+
  theme_bw()+
  annotate(geom = "text", x = -1, y = c(-10, 10), label = c("Low Happiness", "High Happiness"), size = 4.5)+
  annotate(geom = "text", x = -0.5, y = c(-10, 0, 10), label = c("-10", "0", "10"), size = 3.5)+
  theme(plot.margin = unit(c(1, 1, 1.5, 0.5), "cm"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(vjust=-3),
        axis.text.y = element_text(size = 16))+
  labs(title = "(ii) Extinct in the Wild", y = "Frequency", size = 20)

# Critically Endangered

CR_names <- subset(tidy_names, red_list_status == "CR")

CR_dodds <- subset(CR_names) %>%
  inner_join(dodds_sentiment) %>%
  count(word, happiness_average, sort = TRUE)

#get top 10 pos and neg words, freq > 100
bot10 <- head(subset(CR_dodds[order(CR_dodds$happiness_average, decreasing = FALSE),], n>1 & happiness_average < 4.5), 10)
top10 <- head(subset(CR_dodds[order(CR_dodds$happiness_average, decreasing = TRUE),], n>1 & happiness_average > 5.5), 10)

CR20 <- rbind(bot10, top10)
CR20$n2 <- ifelse(CR20$happiness_average < 5, -CR20$n, CR20$n)

CR_plot <- CR20 %>%
  mutate(n = ifelse(happiness_average < 5, -n, n)) %>%
  mutate(word = reorder(word, n2)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = ochre_palettes[["winmar"]][c(rep(3, 10), rep(2,10))]) +
  coord_flip(ylim = c(-CR20$n, CR20$n), xlim = c(0.5, 20.5), expand = TRUE, clip = "off") +
  annotate("text", x = 1, y = 25, label = "freq > 1", size = 4)+
  theme_bw()+
  annotate(geom = "text", x = -1.5,y = c(-20, 20), label = c("Low Happiness", "High Happiness"), size = 4.5)+
  annotate(geom = "text", x = -0.5, y = c(-20, 0, 20), label = c("-20", "0", "20"), size = 3.5)+
  theme(plot.margin = unit(c(1, 1, 1.5, 0.5), "cm"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(vjust=-3),
        axis.text.y = element_text(size = 16))+
  labs(title = "(iii) Critically Endangered", y = "Frequency", size = 20)

# Endangered

EN_names <- subset(tidy_names, red_list_status == "EN")

EN_dodds <- subset(EN_names) %>%
  inner_join(dodds_sentiment) %>%
  count(word, happiness_average, sort = TRUE)

#get top 10 pos and neg words, freq > 100
bot10 <- head(subset(EN_dodds[order(EN_dodds$happiness_average, decreasing = FALSE),], n>5 & happiness_average < 4.5), 10)
top10 <- head(subset(EN_dodds[order(EN_dodds$happiness_average, decreasing = TRUE),], n>5 & happiness_average > 5.5), 10)

EN20 <- rbind(bot10, top10)
EN20$n2 <- ifelse(EN20$happiness_average < 5, -EN20$n, EN20$n)

EN_plot <- EN20 %>%
  mutate(n = ifelse(happiness_average < 5, -n, n)) %>%
  mutate(word = reorder(word, n2)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = ochre_palettes[["winmar"]][c(rep(3, 10), rep(2,10))]) +
  coord_flip(ylim = c(-EN20$n, EN20$n), xlim = c(0.5, 20.5), expand = TRUE, clip = "off") +
annotate("text", x = 1, y = 70, label = "freq > 5", size = 4)+
  theme_bw()+
  annotate(geom = "text", x = -1.5,y = c(-45, 45), label = c("Low Happiness", "High Happiness"), size = 4.5)+
  annotate(geom = "text", x = -0.5, y = c(-50, 0, 50), label = c("-50", "0", "50"), size = 3.5)+
  theme(plot.margin = unit(c(1, 1, 1.5, 0.5), "cm"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(vjust=-3),
         axis.text.y = element_text(size = 16))+
  labs(title = "(iv) Endangered", y = "Frequency", size = 20)

# Vulnerable

VU_names <- subset(tidy_names, red_list_status == "VU")

VU_dodds <- subset(VU_names) %>%
  inner_join(dodds_sentiment) %>%
  count(word, happiness_average, sort = TRUE)

#get top 10 pos and neg words, freq > 100
bot10 <- head(subset(VU_dodds[order(VU_dodds$happiness_average, decreasing = FALSE),], n>10 & happiness_average < 4.5), 10)
top10 <- head(subset(VU_dodds[order(VU_dodds$happiness_average, decreasing = TRUE),], n>10 & happiness_average > 5.5), 10)

VU20 <- rbind(bot10, top10)
VU20$n2 <- ifelse(VU20$happiness_average < 5, -VU20$n, VU20$n)

VU_plot <- VU20 %>%
  mutate(n = ifelse(happiness_average < 5, -n, n)) %>%
  mutate(word = reorder(word, n2)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = ochre_palettes[["winmar"]][c(rep(3, 10), rep(2,10))]) +
  coord_flip(ylim = c(-VU20$n, VU20$n), xlim = c(0.5, 20.5), expand = TRUE, clip = "off") +
  annotate("text", x = 1, y = 70, label = "freq > 10", size = 4)+
  theme_bw()+
  annotate(geom = "text", x = -1.5,y = c(-40, 40), label = c("Low Happiness", "High Happiness"), size = 4.5)+
  annotate(geom = "text", x = -0.5, y = c(-40, 0, 40), label = c("-40", "0", "40"), size = 3.5)+
  theme(plot.margin = unit(c(1, 1, 1.5, 0.5), "cm"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(vjust=-3),
        axis.text.y = element_text(size = 16))+
  labs(title = "(v) Vulnerable", y = "Frequency", size = 20)

# Near Threatened

NT_names <- subset(tidy_names, red_list_status == "NT")

NT_dodds <- subset(NT_names) %>%
  inner_join(dodds_sentiment) %>%
  count(word, happiness_average, sort = TRUE)

#get top 10 pos and neg words, freq > 100
bot10 <- head(subset(NT_dodds[order(NT_dodds$happiness_average, decreasing = FALSE),], n>5 & happiness_average < 4.5), 10)
top10 <- head(subset(NT_dodds[order(NT_dodds$happiness_average, decreasing = TRUE),], n>5 & happiness_average > 5.5), 10)

NT20 <- rbind(bot10, top10)
NT20$n2 <- ifelse(NT20$happiness_average < 5, -NT20$n, NT20$n)

NT_plot <- NT20 %>%
  mutate(n = ifelse(happiness_average < 5, -n, n)) %>%
  mutate(word = reorder(word, n2)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = ochre_palettes[["winmar"]][c(rep(3, 10), rep(2,10))]) +
  coord_flip(ylim = c(-NT20$n, NT20$n), xlim = c(0.5, 20.5), expand = TRUE, clip = "off") +
  annotate("text", x = 1, y = 60, label = "freq > 5", size = 4)+
  theme_bw()+
  annotate(geom = "text", x = -1.5,y = c(-40, 40), label = c("Low Happiness", "High Happiness"), size = 4.5)+
  annotate(geom = "text", x = -0.5, y = c(-40, 0, 40), label = c("-40", "0", "40"), size = 3.5)+
  theme(plot.margin = unit(c(1, 1, 1.5, 0.5), "cm"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(vjust=-3),
        axis.text.y = element_text(size = 16))+
  labs(title = "(vi) Near Threatened", y = "Frequency", size = 20)

# Least Concern

LC_names <- subset(tidy_names, red_list_status == "LC")

LC_dodds <- subset(LC_names) %>%
  inner_join(dodds_sentiment) %>%
  count(word, happiness_average, sort = TRUE)

#get top 10 pos and neg words, freq > 100
bot10 <- head(subset(LC_dodds[order(LC_dodds$happiness_average, decreasing = FALSE),], n>75 & happiness_average < 4.5), 10)
top10 <- head(subset(LC_dodds[order(LC_dodds$happiness_average, decreasing = TRUE),], n>75 & happiness_average > 5.5), 10)

LC20 <- rbind(bot10, top10)
LC20$n2 <- ifelse(LC20$happiness_average < 5, -LC20$n, LC20$n)

LC_plot <- LC20 %>%
  mutate(n = ifelse(happiness_average < 5, -n, n)) %>%
  mutate(word = reorder(word, n2)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = ochre_palettes[["winmar"]][c(rep(3, 10), rep(2,10))]) +
  coord_flip(ylim = c(-LC20$n, LC20$n), xlim = c(0.5, 20.5), expand = TRUE, clip = "off") +
  annotate("text", x = 1, y = 1000, label = "freq > 75", size = 4)+
  theme_bw()+
  annotate(geom = "text", x = -1.5,y = c(-500, 500), label = c("Low Happiness", "High Happiness"), size = 4.5)+
  annotate(geom = "text", x = -0.5, y = c(-1000, -500, 0, 500, 1000), label = c("-1000", "-500", "0", "500", "1000"), size = 3.5)+
  theme(plot.margin = unit(c(1, 1, 1.5, 0.5), "cm"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(vjust=-3),
        axis.text.y = element_text(size = 16))+
  labs(title = "(vii) Least Concern", y = "Frequency", size = 20)

# Data Deficient

DD_names <- subset(tidy_names, red_list_status == "DD")

DD_dodds <- subset(DD_names) %>%
  inner_join(dodds_sentiment) %>%
  count(word, happiness_average, sort = TRUE)

#get top 10 pos and neg words, freq > 100
bot10 <- head(subset(DD_dodds[order(DD_dodds$happiness_average, decreasing = FALSE),], n>10 & happiness_average < 4.5), 10)
top10 <- head(subset(DD_dodds[order(DD_dodds$happiness_average, decreasing = TRUE),], n>10 & happiness_average > 5.5), 10)

DD20 <- rbind(bot10, top10)
DD20$n2 <- ifelse(DD20$happiness_average < 5, -DD20$n, DD20$n)

DD_plot <- DD20 %>%
  mutate(n = ifelse(happiness_average < 5, -n, n)) %>%
  mutate(word = reorder(word, n2)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = ochre_palettes[["winmar"]][c(rep(3, 10), rep(2,10))]) +
  coord_flip(ylim = c(-DD20$n, DD20$n), xlim = c(0.5, 20.5), expand = TRUE, clip = "off") +
  annotate("text", x = 1, y = 380, label = "freq > 10", size = 4)+
  theme_bw()+
  annotate(geom = "text", x = -1.5,y = c(-200, 200), label = c("Low Happiness", "High Happiness"), size = 4.5)+
  annotate(geom = "text", x = -0.5, y = c(-200, 0, 200), label = c("200", "0", "200"), size = 3.5)+
  theme(plot.margin = unit(c(1, 1, 1.5, 0.5), "cm"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(vjust=-3),
        axis.text.y = element_text(size = 16))+
  labs(title = "(viii) Data Deficient", y = "Frequency", size = 20)


####################################################################################################################
# Which words in IUCN REd List species common names do we associate with certain emotions?
######################################################################################################################

nrc_word_counts <- tidy_names %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE)

nrc_word_counts

summary(as.factor(nrc_word_counts$sentiment))
ang <- head(subset(nrc_word_counts, sentiment == "anger"), 10)
ant <- head(subset(nrc_word_counts, sentiment == "anticipation"), 10)
dis <- head(subset(nrc_word_counts, sentiment == "disgust"), 10)
fea <- head(subset(nrc_word_counts, sentiment == "fear"), 10)
joy <- head(subset(nrc_word_counts, sentiment == "joy"), 10)
sad <- head(subset(nrc_word_counts, sentiment == "sadness"), 10)
surprise <- head(subset(nrc_word_counts, sentiment == "surprise"), 10)
trust <- head(subset(nrc_word_counts, sentiment == "trust"), 10)

a.plot <- ang %>%
  mutate(word = reorder(word, -n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = ochre_palettes[["healthy_reef"]][1]) +
  labs(y = "frequency")+
  ggtitle("(a) anger")+
  theme_bw()+
  ylim(0, 1800)
b.plot <- ant %>%
  mutate(word = reorder(word, -n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = ochre_palettes[["healthy_reef"]][2]) +
  labs(y = "frequency")+
  ggtitle("(d) anticipation")+
  theme_bw()+
  ylim(0, 1800)
c.plot <- dis %>%
  mutate(word = reorder(word, -n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = ochre_palettes[["healthy_reef"]][9]) +
  labs(y = "frequency")+
  ggtitle("(e) disgust")+
  theme_bw()+
  ylim(0, 1800)
d.plot <- fea %>%
  mutate(word = reorder(word, -n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = ochre_palettes[["healthy_reef"]][8]) +
  labs(y = "frequency")+
  ggtitle("(c) fear")+
  theme_bw()+
  ylim(0, 1800)
e.plot <- joy %>%
  mutate(word = reorder(word, -n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(y = "frequency")+
  ggtitle("(b) joy")+
  theme_bw()+
  ylim(0, 1800)
f.plot <- sad %>%
  mutate(word = reorder(word, -n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = ochre_palettes[["healthy_reef"]][4]) +
  labs(y = "frequency")+
  ggtitle("(g) sadness")+
  theme_bw()+
  ylim(0, 1800)
g.plot <- surprise %>%
  mutate(word = reorder(word, -n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = ochre_palettes[["healthy_reef"]][5]) +
  labs(y = "frequency")+
  ggtitle("(h) surprise")+
  theme_bw()+
  ylim(0, 1800)
h.plot <- trust %>%
  mutate(word = reorder(word, -n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", fill = ochre_palettes[["healthy_reef"]][6]) +
  labs(y = "frequency")+
  ggtitle("(f) trust")+
  theme_bw()+
  ylim(0, 1800)

##################################################################################
# Figure production
##################################################################################

# Figure 1

png('figs/Figure 1.png', height = 11000, width = 8000, res =300) #opens up plotting device (in file area)
grid.arrange(overallplot,
             actino_plot, amphi_plot,aves_plot,mam_plot,rep_plot,
             EX_plot,EW_plot, CR_plot,EN_plot,VU_plot,NT_plot, LC_plot, DD_plot, nrow = 5)
dev.off()

png('figs/Figure 2.png', height = 1250, width = 1500, res =140) #opens up plotting device (in file area)
multiplot(a.plot, d.plot, c.plot,f.plot, e.plot, b.plot,  h.plot, g.plot,cols = 2)
dev.off() #device off (closes, resets plot device to plots pane)