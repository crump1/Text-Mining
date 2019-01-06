# project start

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "A Princess of Mars")

books <- gutenberg_works(title %in% titles) %>%
    gutenberg_download(meta_fields = "title")


my_books<- books %>%
    group_by(title) %>%
    mutate(chapter= cumsum(str_detect(text, regex("^chapter",
                                                  ignore_case= TRUE)))) %>%
    ungroup() %>%
    filter(chapter > 0) %>%
    unite(document, title, chapter)

word_counts<- my_books %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(document, word, sort= TRUE)


chapters_dtm<- word_counts %>%
    cast_dtm(document, word, n)

chapters_lda<- LDA(chapters_dtm, k= 3, control= list(seed= 1234))

chapters_topics<- tidy(chapters_lda, matrix= "beta")

chapters_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(term= reorder(term, beta)) %>%
    ggplot(aes(x= term, y= beta, fill= factor(topic))) +
    geom_col(show.legend= FALSE) +
    facet_wrap(~topic, ncol= 2, scales= "free") +
    coord_flip()

ct<- chapters_topics

beta_spread<- ct %>%
    mutate(topic= paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1 > .001 | topic2 > .001 | topic3 > .001) %>%
    mutate(l_r1= log2(topic2/topic1)) %>%
    mutate(l_r2= log2(topic3/topic2)) %>%
    mutate(l_r3= log2(topic3/topic1))

beta_spread %>%
    arrange(-l_r1)

# < -267 & > 92


beta_spread %>%
    filter(l_r1 > 92 | l_r1 < -267) %>%
    mutate(term= reorder(term, l_r1)) %>%
    ggplot(aes(x= term, y= l_r1)) +
    geom_col(show.legend= FALSE) +
    coord_flip()

# in this case l_r1 is between war and mars, as ulla is the martian 
# war cry, or death noise, and John Carter is the protagonist of mars

# Word frequency and more
my_books<- gutenberg_download(c(36, 164, 62))



fix.books<- function(doc) {
    doc<- gsub(164, "leagues", doc)
    doc<- gsub(36, "war", doc)
    doc<- gsub(62, "mars", doc)
    return(doc)
}

mb<- my_books

mb$gutenberg_id<- sapply(mb$gutenberg_id, fix.books)
mb<- mb %>% rename(book= gutenberg_id)

tidy_mb<- mb %>%
    group_by(book) %>%
    mutate(linenumber= row_number(),
           chapter= cumsum(str_detect(text, regex("^chapter",
                                                  ignore_case= TRUE)))) %>%
    ungroup() %>%
    filter(chapter > 0) %>%
    unnest_tokens(word, text)

#remove stop words as needed

tidy_mb %>%
    anti_join(stop_words) %>%
    group_by(book) %>%
    count(book, word, sort= TRUE) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word= reorder(word, n)) %>%
    ggplot(aes(x= word, y= n, fill= book)) +
    geom_col(fill= "grey", colour="blue") +
    facet_wrap(~book, ncol= 2, scales= "free") +
    coord_flip()


wcbb<- tidy_mb %>%
    anti_join(stop_words) %>%
    count(book, word, sort= TRUE) %>%
    group_by(book) %>%
    top_n(10) %>%
    ungroup()

wcbb %>%
    filter(book== "mars") %>%
    mutate(word= reorder(word, n)) %>%
    ggplot(aes(x= word, y= n)) +
    geom_col(fill= "red", colour="black") +
    coord_flip()

wcbb %>%
    mutate(word= factor(word, levels= rev(unique(word)))) %>%
    ggplot(aes(x= word, y= n, fill= book)) +
    geom_col(fill= "blue", colour="black") +
    facet_wrap(~book, ncol= 2, scales= "free") +
    coord_flip()

# not sure why, but when I facet wrap by book category, 
# the bars are not sequential, but if I filter by one book - say mars - it's good  

# Gutenberg IDs: 164- leagues, 36- war, 62- mars

leagues<- gutenberg_download(164)

tidy_leagues<- leagues %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(word, sort= TRUE)
war<- gutenberg_download(36)
mars<- gutenberg_download(62)

tidy_war %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(word, sort= TRUE)

tidy_mars %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(word, sort= TRUE)

tidy_leagues<- leagues %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
tidy_war<- war %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
tidy_mars<- mars %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)


frequency <- bind_rows(mutate(tidy_leagues, author = "Jules Verne"),
                       mutate(tidy_war, author = "H.G. Wells"), 
                       mutate(tidy_mars, author = "Edgar Burroughs")) %>% 
    mutate(word = str_extract(word, "[a-z']+")) %>%
    count(author, word) %>%
    group_by(author) %>%
    mutate(proportion = n / sum(n)) %>% 
    select(-n) %>% 
    spread(author, proportion) %>% 
    gather(author, proportion, `H.G. Wells`:`Jules Verne`)

cor.test(data = frequency[frequency$author == "Jules Verne",],
         ~ proportion + `Edgar Burroughs`)

# result -  cor = 0.3544887 
# not enough observations for H.G. Wells versus Burroughs
# however, Verne and Burroughs word frequencies are not highly correlated
# they use different language

#TF-IDF
# no stop words

book_words<- tidy_mb %>%
    count(book, word, sort= TRUE) %>%
    ungroup()

total_words<- book_words %>%
    group_by(book) %>%
    summarise(total= sum(n))

book_words<- left_join(book_words, total_words)

#frequency by rank
freq_by_rank<- book_words %>%
    group_by(book) %>%
    mutate(rank= row_number(), 'term frequency'= n/total) 

#create histogram with n/total as variable/frequency    
freq_by_rank %>% 
    ggplot(aes(x= n/total, fill= book)) +
    geom_histogram(show.legend= F) +
    xlim(NA, 0.0009) +
    facet_wrap(~book, ncol= 2, scales= "free_y")

#Zipf's Law
freq_by_rank %>% 
    ggplot(aes(rank, `term frequency`, color = book)) + 
    geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
    geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) + 
    scale_x_log10() +
    scale_y_log10()

#deviations at high rank mean none of these authors use common words 
#compared to typical language, at low ranks they deviate too, just not as much

book_words<- book_words %>% bind_tf_idf(word, book, n)

book_words %>%
    arrange(desc(tf_idf)) %>%
    mutate(word= factor(word, levels= rev(unique(word)))) %>%
    group_by(book) %>%
    top_n(10, tf_idf) %>%
    ungroup() %>%
    ggplot(aes(x= word, y= tf_idf, fill= book)) +
    geom_col(show.legend= F) +
    labs(x= NULL, y= "tf_idf") +
    ggtitle("Book Words by TF-IDF") +
    facet_wrap(~book, ncol= 2, scales= "free") +
    coord_flip()

#Sentiment Analysis

#Begin with converting a TDM to a "tidy" data frame
#Visualize contribution to sentiment

my_books<- books %>%
    group_by(title) %>%
    mutate(chapter= cumsum(str_detect(text, regex("^chapter",
                                                  ignore_case= TRUE)))) %>%
    ungroup() %>%
    filter(chapter > 0) %>%
    unite(document, title, chapter)

word_counts<- my_books %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(document, word, sort= TRUE) 

chapter_dtm<- word_counts %>%
    cast_dtm(document, word, n)

ch_td<- tidy(chapter_dtm)


ch_sentiments<- ch_td %>%
    inner_join(get_sentiments("bing"), by= c(term= "word"))

ch_sentiments %>%
    count(sentiment, term, wt= count) %>%
    ungroup() %>%
    filter(n >= 50) %>%
    mutate(n= ifelse(sentiment== "negative", -n, n)) %>%
    mutate(term= reorder(term, n)) %>%
    ggplot(aes(x= term, y= n, fill= sentiment)) +
    geom_bar(stat= "identity") +
    ylab("Contribution to Sentiment") +
    coord_flip()

#For more sentiment analysis, let's get back to the data frame with 
#with linenumber and chapter for columns - tidy_mb

#Let's start with NRC, a sentiment lexicon with specific categories

get_sentiments("nrc") %>% distinct(sentiment)

#Ok, the dark side first - "fear"
nrc.fear<- get_sentiments("nrc") %>% filter(sentiment== "fear")

tidy_mb %>%
    filter(book== "war") %>%
    inner_join(nrc.fear) %>%
    count(word, sort= TRUE) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word= reorder(word, n)) %>%
    ggplot(aes(x= word, y= n)) +
    geom_col(fill= "red", colour= "black") +
    coord_flip()

#For the same book, let's see the antonym of fear...

nrc.joy<- get_sentiments("nrc") %>% filter(sentiment== "joy")
# and apply it to the same book

tidy_mb %>%
    filter(book== "war") %>%
    inner_join(nrc.joy) %>%
    count(word, sort= TRUE) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word= reorder(word, n)) %>%
    ggplot(aes(x= word, y= n)) +
    geom_col(fill= "skyblue", colour= "navyblue") +
    coord_flip()

#Now compare to Verne

tidy_mb %>%
    filter(book== "leagues") %>%
    inner_join(nrc.joy) %>%
    count(word, sort= TRUE) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word= reorder(word, n)) %>%
    ggplot(aes(x= word, y= n)) +
    geom_col(fill= "snow", colour= "dodgerblue") +
    labs(x= "Joy Words", y= "Total Words") +
    ggtitle("Verne Joy Sentiment") +
    coord_flip()

scifi_sent<- tidy_mb %>%
    inner_join(get_sentiments("bing")) %>%
    count(book, index= linenumber %/% 80, sentiment) %>%
    spread(sentiment, fill= 0, n) %>%
    mutate(sentiment= positive- negative) %>%
    ggplot(aes(x= index, y= sentiment, fill= book)) +
    geom_col(show.legend= FALSE) +
    facet_wrap(~book, ncol= 2, scales= "free_x")

#Seems H.G. Wells wrote a pretty "negative" book
#Let's dig deeper

tidy_mb %>%
    inner_join(get_sentiments("bing")) %>%
    group_by(sentiment) %>%
    count(book)

#Yes, turns out Wells used negative words at a much higher rate 
#Verne is pretty even, as reflected througout his entire book

tidy_mb %>%
    filter(book== "war") %>%
    inner_join(get_sentiments("bing")) %>%
    count(book, index= linenumber %/% 80, sentiment) %>%
    spread(sentiment, fill= 0, n) %>%
    mutate(sentiment= positive- negative) %>%
    arrange(sentiment) %>%
    filter(sentiment> 0)

#All but three of War's indices - 80 word blocks - are negative

tidy_mb %>%
    filter(book== "war") %>%
    inner_join(get_sentiments("bing")) %>%
    count(book, index= linenumber %/% 80, sentiment) %>%
    spread(sentiment, fill= 0, n) %>%
    mutate(sentiment= positive- negative) %>%
    ggplot(aes(x= index, y= sentiment)) +
    geom_col() +
    coord_flip()

#Not exactly pretty, but the three positive indices are now visible
#So The War of the Worlds is mostly doom and gloom. 
#Leagues, however, has a nice beat, a rhythm that readers can feel. 
    









    



