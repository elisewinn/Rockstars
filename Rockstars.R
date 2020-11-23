## Load libraries ####

library(rvest)
library(httr)
library(tidyverse)
library(data.table)
library(XML)
library(tidytext)
library(furrr)
library(progressr)

########## ELTON JOHN ###########

## Paste URLS for artist page ####

url <- 'https://www.lyrics.com/artist/Elton-John/4617'
baseurl <- 'https://www.lyrics.com'

## Get artist landing page ####

get_artist_landing_page <- function(url) {
  x <-tryCatch({
    httr::GET(url)}, 
    error = function(e) e)

  headers <- x %>%
    read_html() %>%
    html_nodes(., xpath = '//*[@class="clearfix"]')
  
  return(headers)
}

headers <- get_artist_landing_page('https://www.lyrics.com/artist/Elton-John/4617')
headers[[1]] <- NULL

## Get tables with albums ####

get_one_table <- function(header) {
  
  test <- header %>%
    html_nodes("table") 
  
  if (length(test) > 0) {
  
  web <- header %>%
    html_nodes("table") %>% 
    html_nodes("tr") %>% 
    html_nodes("a") %>%
    html_attr("href") %>% 
    enframe(name = NULL, value = 'url')
  
  album <- header %>%
    html_nodes("h3") %>% 
    html_text() %>% 
    enframe(name = NULL, value = 'Album') 
  
  year <- header %>%
    html_nodes("h3") %>% 
    html_nodes("span") %>%
    html_text() %>% 
    enframe(name = NULL, value = 'Year') %>% 
    mutate(Year = gsub('\\[|\\]', replacement = '', x = Year))
  
  if (nrow(year) == 0) {
    year <- data.frame(Year = NA)
  }
  
  doc <- header %>%
    html_nodes(., 'table') %>%
    html_table(fill = T) %>% 
    bind_cols(., web) %>% 
    bind_cols(., album) %>% 
    bind_cols(., year)
  
  return(doc)
  
  } else {
    
    return(NULL)
  }
  
}

get_all_titles <- function(headers) {
  
  titles <- purrr::map(headers, ~{
    length(headers)
    get_one_table(.x)})
  
  return(titles)
  
}

titles <- get_all_titles(headers) %>% 
  bind_rows() %>% 
  distinct() %>% 
  mutate(finalurl = paste0(baseurl, url))

## Get lyrics from albums ####

get_lyrics <- function(url) {
  
  ex <-tryCatch({
    httr::GET(url)}, 
    error = function(e) e)
  
  lyrics <- ex %>%
    read_html() %>%
    html_nodes(., xpath = '//*[@id="lyric-body-text"]') %>% 
    html_text() %>% 
    enframe(value = 'text', name = NULL) %>% 
    mutate(finalurl = url)
  
  return(lyrics)
  
}

## Get lyrics using future maps ####

urls <- c(titles$finalurl)

options(future.globals.maxSize = 1700 * 1020^2)

# Set a "plan" for how the code should run.
plan(multisession, workers = 5)

fmap_lyrics <- function(urls) {
  p <- progressor(steps = length(urls))
  
  furrr::future_map(urls, ~{
    p()
    get_lyrics(.x)
  })
  
}

with_progress({
  lyrics <- fmap_lyrics(urls)
})

lyrics <- lyrics %>% 
  bind_rows()

#### Remove Greatest Hits, Live, Covers, Very Best of, Best of, DVD, awards, compilations,
# songs by year, bonus tracks, special editions

remove <- c('greatest hits', 'live in', 'live at', 'live', 
            'bonus track', 'deluxe', 'exclusive', 'double disc', '\\blp\\b', 'vynil', 'edition',
            'live 8', 'live aid', 'live from',
            'in concert', 'concert',
            'classic albums', 'collection', 'classics', 'classic tracks', 'legends',
            'love songs', 'awards', 'nominees','classic', 'mixtape',
            'compilation', '70s', '80s', 'throwback', 
            'remixed', 'remix', 'remastered', 'remaster',
            'unknown album',
            'covers', 'hits', 'very best of', 'best of','number ones', 'box set',
            '\\bdvd\\b', 'video', 'blu\\-ray',
            'superior sound of')

remove <- paste(remove, collapse = '|')

df_analysis <- titles %>% 
  mutate_all(., tolower) %>% 
  filter(!(Album %like% remove) & !is.na(Year))  %>% 
  group_by(Album) %>% 
  mutate(n_songs = n_distinct(Song)) %>% 
  filter(n_songs > 2) %>% 
  left_join(., lyrics %>% 
  mutate_all(., tolower) )%>% 
  distinct()

## Analyze lyrics text ####

get_sentiment <- function(song) {
  
  df_text <- df_analysis %>% 
    filter(Song == song)
  
  if (dim(df_text)[1] > 0) {
    
    df_text <- df_text %>% 
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("bing")) %>%
      count(Song, Album, Year, sentiment)
    
    return(df_text)
    
  } else {
    return(NULL) 
  }
  
}

pb <- progress_estimated(length(df_analysis$Song))
lyrics_sentiment <- purrr::map(df_analysis$Song, ~{
  pb$tick()$print()
  get_sentiment(.x)
})

lyrics_sentiment.df <- lyrics_sentiment %>% 
  bind_rows() 

colnames(lyrics_sentiment.df)

lyrics_sentiment_by_song <- lyrics_sentiment.df %>% 
  distinct(Song, Year, sentiment, n) %>% 
  group_by(Song, sentiment) %>% 
  summarise(n = sum(n)) %>% 
  distinct()

lyrics_sentiment_by_song <- lyrics_sentiment_by_song %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(net_sentiment = positive - negative)


####### Plot sentiment #########

library(viridis)

lyrics_sentiment_by_song_and_year <- df_analysis %>% 
  left_join(., lyrics_sentiment_by_song)

df_analysis %>% 
  left_join(., lyrics_sentiment_by_song) %>% 
  group_by(Year) %>% 
  mutate(song_index = row_number())


lyrics_sentiment_by_song_and_year %>% 
  group_by(Year) %>% 
  summarise(positive = sum(positive), 
            negative = sum(negative), 
            net_sentiment = positive-negative) %>% 
  ggplot(aes(Year, net_sentiment)) +
  geom_col(show.legend = FALSE) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  labs(y = 'Net Sentiment per Year', x = NULL)

lyrics_sentiment_by_decade <- lyrics_sentiment_by_song_and_year %>% 
  group_by(Year, Album) %>% 
  summarise(positive = sum(positive), negative = sum(negative), net_sentiment = positive-negative) %>% 
  ungroup %>% 
  mutate(Decade = case_when(
    Year >= 1960 & Year < 1970 ~ '60s',
    Year >= 1970 & Year < 1980 ~ '70s',
    Year >= 1980 & Year < 1990 ~ '80s',
    Year >= 1990 & Year < 2000 ~ '90s',
    Year >= 2000 & Year < 2010 ~ '00s',
    Year >= 2010 & Year < 2020 ~ '10s'
  ),
  Decade = factor(Decade, levels = c('60s','70s','80s','90s','00s','10s'),
                  labels = c('60s','70s','80s','90s','00s','10s'))) %>% 
  distinct() %>% 
  group_by(Decade) %>% 
  mutate(Album = fct_reorder(Album, Year))

lyrics_sentiment_by_decade %>% 
  filter(Decade == '70s') %>% 
  ggplot(aes(Year, net_sentiment, color = reorder(Album,Year), fill = reorder(Album,Year))) +
  geom_col(position = 'dodge') +
  facet_wrap(.~Decade, scales = 'free_x') +
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T) +
  theme_bw()

lyrics_sentiment_by_decade %>% 
  filter(Decade == '80s') %>% 
  ggplot(aes(Year, net_sentiment, color = reorder(Album,Year), fill = reorder(Album,Year))) +
  geom_col(position = 'dodge') +
  facet_wrap(.~Decade, scales = 'free_x') +
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T) +
  theme_bw()

lyrics_sentiment_by_decade %>% 
  filter(Decade == '90s') %>% 
  ggplot(aes(Year, net_sentiment, color = reorder(Album,Year), fill = reorder(Album,Year))) +
  geom_col(position = 'dodge') +
  facet_wrap(.~Decade, scales = 'free_x') +
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T) +
  theme_bw()

lyrics_sentiment_by_decade %>% 
  filter(Decade == '00s') %>% 
  ggplot(aes(Year, net_sentiment, color = reorder(Album,Year), fill = reorder(Album,Year))) +
  geom_col(position = 'dodge') +
  facet_wrap(.~Decade, scales = 'free_x') +
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T) +
  theme_bw()

lyrics_sentiment_by_decade %>% 
  filter(Decade == '10s') %>% 
  ggplot(aes(Year, net_sentiment, color = reorder(Album,Year), fill = reorder(Album,Year))) +
  geom_col(position = 'dodge') +
  facet_wrap(.~Decade, scales = 'free_x') +
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T) +
  theme_bw()

lyrics_sentiment_by_decade %>% 
  group_by(Year, Decade) %>% 
  summarise(positive = sum(positive), negative = sum(negative), net_sentiment = positive-negative) %>% 
  ggplot(aes(Year, net_sentiment)) +
  geom_col(position = 'dodge') +
  facet_wrap(.~Decade, scales = 'free_x') +
  theme_bw() +
  labs(y = 'Net Sentiment per Year', x = NULL)

###### the 70's ########

lyrics_sentiment_70s <- lyrics_sentiment_by_song_and_year %>% 
  mutate(Decade = case_when(
    Year >= 1960 & Year < 1970 ~ '60s',
    Year >= 1970 & Year < 1980 ~ '70s',
    Year >= 1980 & Year < 1990 ~ '80s',
    Year >= 1990 & Year < 2000 ~ '90s',
    Year >= 2000 & Year < 2010 ~ '00s',
    Year >= 2010 & Year < 2020 ~ '10s'
  ),
  Decade = factor(Decade, levels = c('60s','70s','80s','90s','00s','10s'),
                  labels = c('60s','70s','80s','90s','00s','10s'))) %>% 
  distinct() %>% 
  group_by(Decade) %>% 
  mutate(Album = fct_reorder(Album, Year)) %>% 
  filter(Decade == '70s') %>% 
  group_by(Album) %>% 
  mutate(song_index = row_number()) 

lyrics_sentiment_70s %>% 
  ggplot(aes(song_index, net_sentiment)) +
  facet_wrap(.~Album, scales = c('free')) +
  geom_col(show.legend = F) +
  theme_bw() +
  # theme(axis.text.x = element_blank()) +
  labs(x = 'Album Song Nr.')

remove <- c('all the young girls love alice',
            'the ballad of danny bailey (1909-1934)', 
            'the ballad of danny bailey (1909-34)', 
            'the ballad of danny bailey [1909-1934]', 'the wide-eyed and laughing', 
            'bennie & the jets', 'bill bones and the white bird', 
            'funeral for a friend / love lies bleeding', 
            'funeral for a friend/ love lies bleeding', 
            'funeral for a friend/love lies bleeding', 
            'i feel like a bullit (in the gun of robert ford)', 
            "i'm gonna be a teenage idol", 'jamaica jerk-off',
            'jamaica jerkoff', 'medley: yell help/wednesday night/ugly',  
            'one-horse town', "rocket man (i think it's going to be a long, long time)",  
            "saturday night's alright (for fighting)",  
            'we all fall in love sometime',  
            'whatever gets you thru the night', 'where to now, st. peter?', 
            "your sister can't twist (but she can rock 'n' roll)",
            "your sister can't twist (but she can rock'n' roll)", 
            "your sister can't twist (but she can rock'n'roll)" ,
            'your so static',
            '(gotta get a) meal ticket')

ggsave(lyrics_sentiment_70s %>% 
  filter(!Song %like% 'unreleased|demo|version|\\#|\\*|release|live|radio' & !Song %in% remove) %>% 
  ungroup %>% 
  distinct(Song, Year, net_sentiment) %>% 
  arrange(Song, Year) %>% 
  distinct() %>% 
  group_by(Song) %>% 
  filter(Year == min(Year) & net_sentiment != 0) %>% 
  ggplot(aes(Song, net_sentiment)) +
  facet_wrap(.~Year, scales = 'free') +
  geom_col(show.legend = F) +
    labs(y = 'Net Sentiment') +
  coord_flip(), filename = 'seventies.png', path = 'Documents/Rockstars/',
  width = 15, height = 12, device = 'png', units = 'in') 



### Soundracks ####

lyrics_sentiment_by_decade %>% 
  filter(Album %like% 'soundtrack') %>% 
  group_by(Year, Decade) %>% 
  ggplot(aes(x  = Year, y = net_sentiment, color = Album, fill = Album)) +
  geom_col(position = 'dodge') +
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T) +
  theme_bw()

#### David Bowie

#### Joy Division #####

url <- 'https://www.lyrics.com/artist/Joy-Division/71273'
baseurl <- 'https://www.lyrics.com'

headers <- get_artist_landing_page('https://www.lyrics.com/artist/Joy-Division/71273')
headers[[1]] <- NULL

titles <- get_all_titles(headers) %>% 
  bind_rows() %>% 
  distinct() %>% 
  mutate(finalurl = paste0(baseurl, url))

titles <- titles %>% 
  filter(Year == 1979 | Year == 1980)

urls <- c(titles$finalurl)

options(future.globals.maxSize = 1700 * 1020^2)
plan(multisession, workers = 5)

with_progress({
  lyrics <- fmap_lyrics(urls)
})

lyrics <- lyrics %>% 
  bind_rows()

remove <- c('edition|Edition|\\bLP\\b')

df_analysis <- titles %>% 
  group_by(Album) %>% 
  mutate(n_songs = n_distinct(Song)) %>% 
  filter(n_songs > 3) %>% 
  left_join(., lyrics)%>% 
  distinct() %>% 
  filter(!Album %like% remove)

## Analyze lyrics text ####

pb <- progress_estimated(length(df_analysis$Song))
lyrics_sentiment <- purrr::map(df_analysis$Song, ~{
  pb$tick()$print()
  get_sentiment(.x)
})

lyrics_sentiment.df <- lyrics_sentiment %>% 
  bind_rows() 

colnames(lyrics_sentiment.df)

lyrics_sentiment_by_song <- lyrics_sentiment.df %>% 
  distinct(Song, Year, sentiment, n) %>% 
  group_by(Song, sentiment) %>% 
  summarise(n = sum(n)) %>% 
  distinct()

####### Plot sentiment #########

library(viridis)

lyrics_sentiment_by_song_and_year <- df_analysis %>% 
  left_join(., lyrics_sentiment_by_song) %>% 
  group_by(Year, Album) %>% 
  spread(sentiment, n, fill = 0)  %>%
  mutate(net_sentiment = positive - negative) %>% 
  group_by(Year) %>% 
  mutate(song_index = row_number())

JD1 <- lyrics_sentiment_by_song_and_year %>% 
  filter(Year == 1979) %>% 
  ggplot(aes(Song, net_sentiment, fill = Song)) +
  facet_wrap(.~Album) +
  geom_col(show.legend = F) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  labs(y = 'Net Sentiment', x = 'Song', title = 'Joy Division')

JD2 <- lyrics_sentiment_by_song_and_year %>% 
  filter(Year == 1980) %>% 
  ggplot(aes(Song, net_sentiment, fill = Song)) +
  facet_wrap(.~Album) +
  geom_col(show.legend = F) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  labs(y = 'Net Sentiment', x = 'Song', title = ' ')

gridExtra::grid.arrange(JD1, JD2, nrow = 1)

#### NIN

url <- 'https://www.lyrics.com/artist/Nine-Inch-Nails/5033'
baseurl <- 'https://www.lyrics.com'

headers <- get_artist_landing_page('https://www.lyrics.com/artist/Nine-Inch-Nails/5033')
headers[[1]] <- NULL

titles <- get_all_titles(headers) %>% 
  bind_rows() %>% 
  distinct() %>% 
  mutate(finalurl = paste0(baseurl, url))

remove <- c('greatest hits', 'live in', 'live at', 'live', 
            'bonus track', 'deluxe', 'exclusive', 'double disc', '\\blp\\b', 
            '\\b2lp\\b', 'vynil', 'edition', '\\bep\\b',
            'live 8', 'live aid', 'live from',
            'in concert', 'concert',
            'classic albums', 'collection', 'classics', 'classic tracks', 'legends','hottest',
            'love songs', 'awards', 'nominees','classic', 'mixtape',
            'compilation', '70s', '80s', 'throwback', 
            'remixed', 'remix', 'remastered', 'remaster', '\\br3m1x3d\\b', 'version',
            'unknown album',
            'covers', 'hits', 'very best of', 'best of','number ones', 'box set', 'dualdisc',
            '\\bdvd\\b', 'video', 'blu\\-ray', 'broadcasts', 'tour', '\\#', '\\buk\\b',
            'superior sound of', 'fixed')

remove <- paste(remove, collapse = '|')

titles <- titles %>% 
  mutate_all(., tolower) %>% 
  filter(Year >= 1989 & Year <= 2018 & !Album %like% remove)%>% 
  group_by(Album) %>% 
  mutate(n_songs = n_distinct(Song)) %>% 
  filter(n_songs > 3)

urls <- c(titles$finalurl)

options(future.globals.maxSize = 1700 * 1020^2)
plan(multisession, workers = 5)

with_progress({
  lyrics <- fmap_lyrics(urls)
})

lyrics <- lyrics %>% 
  bind_rows()

df_analysis <- titles  %>% 
  left_join(., lyrics)%>% 
  distinct() %>% 
  filter(!Album %like% remove)

## Analyze lyrics text ####

pb <- progress_estimated(length(df_analysis$Song))
lyrics_sentiment <- purrr::map(df_analysis$Song, ~{
  pb$tick()$print()
  get_sentiment(.x)
})

lyrics_sentiment.df <- lyrics_sentiment %>% 
  bind_rows() 

colnames(lyrics_sentiment.df)

lyrics_sentiment_by_song <- lyrics_sentiment.df %>% 
  distinct(Song, Year, sentiment, n) %>% 
  group_by(Song, sentiment) %>% 
  summarise(n = sum(n)) %>% 
  distinct()

####### Plot sentiment #########

library(viridis)

lyrics_sentiment_by_song_and_year <- df_analysis %>% 
  left_join(., lyrics_sentiment_by_song) %>% 
  group_by(Year, Album) %>% 
  spread(sentiment, n, fill = 0)  %>%
  mutate(net_sentiment = positive - negative) %>% 
  group_by(Year) %>% 
  mutate(song_index = row_number())

remove <- c("\\bcloser [precursor]\\b", "\\bhurt [quiet]\\b", "mr self destruct",
            "no you don't",
            "physical (you're so)", "she’s gone away", "that’s what i get", "zero sum")

lyrics_sentiment_by_song_and_year %>% 
  filter(!Song %like% 'unreleased|demo|version|\\#|\\*|release|live|radio' & !Song %in% remove & Year < 2018) %>% 
  ungroup %>% 
  distinct(Song, Year, net_sentiment) %>% 
  arrange(Song, Year) %>% 
  distinct() %>% 
  group_by(Song) %>% 
  filter(Year == min(Year) & net_sentiment == min(net_sentiment) & net_sentiment != 0) %>% 
  ggplot(aes(Song, net_sentiment)) +
  facet_wrap(.~Year, scales = 'free') +
  geom_col(show.legend = F) +
  labs(y = 'Net Sentiment') +
  coord_flip()

ggsave(lyrics_sentiment_by_song_and_year %>% 
         filter(!Song %like% 'unreleased|demo|version|\\#|\\*|release|live|radio' & !Song %in% remove & Year < 2018) %>% 
         ungroup %>% 
         distinct(Song, Year, net_sentiment) %>% 
         arrange(Song, Year) %>% 
         distinct() %>% 
         group_by(Song) %>% 
         filter(Year == min(Year) & net_sentiment == min(net_sentiment) & net_sentiment != 0) %>% 
         ggplot(aes(Song, net_sentiment)) +
         facet_wrap(.~Year, scales = 'free') +
         geom_col(show.legend = F) +
         labs(y = 'Net Sentiment', title = 'Nine Inch Nails') +
         coord_flip(), filename = 'NIN.png', path = 'Documents/Rockstars/',
       width = 16, height = 12, device = 'png', units = 'in') 




