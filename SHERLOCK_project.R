
sherlock_raw <- gutenberg_download(1661)

sherlock <- sherlock_raw %>%
    mutate(story = ifelse(str_detect(text, "ADVENTURE"),
                          text,
                          NA)) %>%
    fill(story) %>%
    filter(story != "THE ADVENTURES OF SHERLOCK HOLMES") %>%
    mutate(story = factor(story, levels = unique(story)))


tidy_sherlock<- sherlock %>%
    mutate(line= row_number()) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    filter(word != "holmes")

sherlock_tf_idf<- tidy_sherlock %>%
    count(story, word, sort= TRUE) %>%
    bind_tf_idf(word, story, n) %>%
    arrange(-tf_idf) %>%
    group_by(story) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word= factor(word, level= rev(unique(word)))) %>%
    ggplot(aes(x= word, y= tf_idf, fill= story)) +
    geom_col(show.legend = F) +
    labs(x= NULL, y= "tf_idf") +
    facet_wrap(~story, ncol= 2, scales= "free") +
    coord_flip()

sherlock_dfm<- tidy_sherlock %>%
    count(story, word, sort= TRUE) %>%
    cast_dfm(story, word, n)


topic_model <- stm(sherlock_sparse, K = 6, 
                   verbose = FALSE, init.type = "Spectral")

td_beta %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(term= reorder(term, beta)) %>%
    ggplot(aes(x= term, y= beta, fill= factor(topic))) +
    geom_col(show.legend = F) +
    facet_wrap(~topic, ncol= 2, scales="free") +
    coord_flip()

td_gamma <- tidy(topic_model, matrix = "gamma",                    
                 document_names = rownames(sherlock_sparse))

ggplot(td_gamma, aes(x= gamma, fill= as.factor(topic))) +
    geom_histogram(alpha= 0.8, show.legend = F) +
    facet_wrap(~ topic, ncol= 3)

tidy_sherlock %>%
    count(word, sort= TRUE) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word= reorder(word, n)) %>%
    ggplot(aes(x= word, y= n)) +
    geom_col(fill= "orange", colour= "brown") +
    coord_flip()

#Holmes is all about mystery, sort of a precursor to whodunit stories
#Let's look at two germane emotions: surprise and anticipation

nrc.surprise<- get_sentiments("nrc") %>% filter(sentiment== "surprise")
nrc.anticipation<- get_sentiments("nrc") %>% filter(sentiment== "anticipation")

tidy_sherlock %>%
    inner_join(nrc.surprise) %>%
    count(word, sort= TRUE) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word= reorder(word, n)) %>%
    ggplot(aes(x= word, y= n)) +
    geom_col(fill= "pink", colour= "navyblue") +
    coord_flip()

# Top Ten "surprise" words - leave, suddenly, money, pray, hope, death, chance, mystery, 
# remarkable, angel

tidy_sherlock %>%
    inner_join(nrc.anticipation) %>%
    count(word, sort= TRUE) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word= reorder(word, n)) %>%
    ggplot(aes(x= word, y= n)) +
    geom_col(fill= "pink", colour= "navyblue") +
    coord_flip()

#Top Ten "anticipation" words - time, money, pray, hope, letter, death, 
# white, coming, marriage, visitor, god
#Some crossover, yes, but also considerable representation of both categories

tidy_sherlock %>%
    inner_join(get_sentiments("bing")) %>%
    count(story, index= line %/% 80, sentiment) %>%
    spread(sentiment, fill= 0, n) %>%
    mutate(sentiment= positive- negative) %>%
    ggplot(aes(x= index, y= sentiment, fill= story)) +
    geom_col(show.legend = F) +
    facet_wrap(~story, ncol= 3, scales= "free_x")

tidy_sherlock %>%
    filter(story== "VIII. THE ADVENTURE OF THE SPECKLED BAND") %>%
    inner_join(get_sentiments("bing")) %>%
    count(sentiment) 

#Speckled Band is heavy on the negative words

tidy_sherlock %>%
    filter(story== "VIII. THE ADVENTURE OF THE SPECKLED BAND") %>%
    inner_join(get_sentiments("bing")) %>%
    summarise(proportion= sum(sentiment== "negative")/ n())

# ~72% worth, huh..
#Noble Bachelor appears more even, let's check   

tidy_sherlock %>%
    filter(story== "X. THE ADVENTURE OF THE NOBLE BACHELOR") %>%
    inner_join(get_sentiments("bing")) %>%
    count(sentiment) 

#Yes, much more even than Speckled, as reflected in the VIS - almost 50/50
#In fact, most of Doyle's Sherlock work leans to the "negative" side of sentiment

