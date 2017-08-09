###
# importing steam game stats and making some basic data exploration
###

library(data.world)
steamGames <- data.world::query(
  data.world::qry_sql('SELECT * FROM `games-features`'),
  dataset = 'https://data.world/craigkelly/steam-game-data'
)

###
# some basic preprocessing
###
library(magrittr)
library(tidyverse)
# removing strange symbols from game names; they're now lowercase (does it matter?)
# it's possible later to search through them with str_detect
steamGames$queryname %<>% iconv(from = 'UTF8', to = 'ASCII')

## how much NA's there's in every column?
sapply(steamGames, function(x) sum(is.na(x)))
## there's a strange tendency when price is zero for pricecurrency to be NA 
###
## isFree doesn't really signify that the game's final price is zero
###
## SteamSpyOwnersVariance - (steamspy.com) total owners,
## which includes free weekend trials and other possibly spurious numbers.
## Note that this is not technically variance: according to steamspy.com,
## "the real number... lies somewhere on... [value +/- variance]"

## recoding variables to logicals
steamGames %<>% 
  mutate_at(vars(contains('genreis')), funs(as.logical)) %>%
  mutate_at(vars(contains('category')), funs(as.logical)) %>%
  replace_na(list(releasedate = ' ', pricecurrency = 'USD'))

## treating genres separately
genres <- 
  steamGames
  select(contains('genreis')) %>%
  summarise_all(funs(sum)) %>%
  unlist()

# genres are non-mutally exclusive
library(stringr)
library(forcats)
print(str_c('Total games: ', nrow(steamGames), '; total by genres: ', sum(genres)))
genres.df <- 
  data_frame(n = genres, genre = names(genres)) %>%
  mutate(genre =
           str_replace_all(genre, 'genreis', '') %>%
           str_replace('massivelymultiplayer', 'mmp') %>%
           factor()) %>%
  arrange(desc(n)) %>%
  mutate(genre = fct_inorder(genre), 
         popularity = 
           ntile(n, 3) %>%
           factor() %>%
           fct_recode(Popular = '3', Common = '2', Unpopular = '1'))

library(ggplot2)
# it looks like more than half of Steam games can be considered indie :D
ggplot(data = genres.df, aes(weight = n, x = genre, fill = popularity)) + 
  geom_bar(alpha = 0.7) + 
  geom_rect(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 8000, alpha = 0, linetype = 1, color = 'darkred') + 
  geom_rect(xmin = 4.52, xmax = 8.5, ymin = 0, ymax = 2800, alpha = 0, linetype = 1, color = 'goldenrod4') + 
  geom_rect(xmin = 8.52, xmax = 13.5, ymin = 0, ymax = 800, alpha = 0, linetype = 1, color = 'darkgreen') + 
  scale_y_continuous(breaks = seq(0, 8000, by = 1000)) + 
  scale_fill_brewer(palette = 'Set1') +
  guides(fill = guide_legend(title = 'Popularity')) +
  theme(legend.position = c(0.85, 0.85)) + 
  ggsave('genre freq naive.png', width = 8.5, height = 6)

# investigationg how much multi-tags there are

multitags <- 
  steamGames %>% 
  select(contains('GenreIs')) %>%
  mutate_all(funs(as.logical)) %>%
  rowSums()

qplot(multitags, breaks = 0:10 - 1/2, alpha = I(0.8), fill = I('orangered2'), color = I('orangered4'), binwidth = I(0.4)) + 
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(name = NULL) + 
  labs(title = 'Distribution of number of tags for all Steam games',
       x = 'Total number of tags game has') + 
  geom_text(stat = 'bin', bins = 11, aes(label = ..count..), vjust = -0.75) +
  ggsave('multitags.png', width = 8, height = 6)

## takes a long time, shows a periodic pattern
## replacing cases like 'Nov 1 2000' with 'Nov 01 2000'
library(anytime)
steamGames$releasedate %<>%
  inset(str_count(., ' ') != 2, NA) %>%
  str_split(' ') %>%
  lapply(
    function(datevec) {
      if(anyNA(datevec)) return(datevec)
      if(str_length(datevec[2]) == 1) {
        datevec[2] %<>% str_c('0', .)
      }
      return(datevec)}) %>%
  sapply(str_c, collapse = ' ') %>%
  anydate()
# more na's appear after anydate for some reson
# perhaps two-spaced stuff involves descriptions sometimes (like 'will release soon')

rd.clean <-
  steamGames %$%
  releasedate %>%
  na.omit()

## totally obvious that steam boom started at around mid-2013
## although Valve might've added those games later and release date was just logged, I still thing it's a 'BOOM' date
library(lubridate)
qplot(rd.clean, breaks = as.numeric(dmy('01 January 2004') + months(seq(0, 6*28, by = 6)))) +
  scale_x_date(name = 'Game release date',
               breaks = dmy('01 January 2004') + years(0:13),
               labels = 2004:2017,
               limits = c(dmy('01 January 2004'), dmy('01 January 2018'))) + 
  scale_y_continuous(breaks = seq(0, 3000, by = 500)) + 
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 7)) + 
  ggsave('release date distibution.png', width = 6, height = 4)

## want to know how popularity decreases as the game ages
library(forcats)
gameAging <- 
  steamGames %>%
  select(responsename,
         steamspyplayersestimate, steamspyplayersvariance,
         steamspyowners, steamspyownersvariance,
         releasedate) %>%
  ## filtering out outliers like Dota2, TF2, CS, L4D and Skyrim
  filter(steamspyplayersestimate < 1000000 & steamspyplayersestimate != 0) %>%
  na.omit() %>%
  mutate(peak = ifelse(releasedate < dmy('01 June 2013'), 
                              'Before Jun 2013', 'After Jun 2013') %>%
           factor() %>%
           fct_inorder(),
         percent_owners_played = steamspyplayersestimate / steamspyowners)


## "flameplot" with which I initially wanted to compare how old games are unpopular, but found out something else instead
## it seems with the rapid growth of the number of games 

## SteamSpyPlayersEstimate - (steamspy.com) best estimate of total number of people who have played the game since March 2009
## so this is a cumulative value afterall
ggplot(gameAging, aes(x = releasedate, y = steamspyplayersestimate)) + 
  scale_x_date(name = 'Game release date',
               breaks = dmy('01 January 2005') + years(0:12),
               labels = 2005:2017,
               limits = c(dmy('01 January 2005'), dmy('01 January 2017'))) +
  geom_hex(bins = 40) +
  scale_fill_gradientn(name = 'counts,\nlog10',
                       colors = brewer.pal(6, 'YlOrRd'), trans = 'log10',
                       breaks = 10^seq(0, log10(700), by = 0.45),
                       labels = round(10^seq(0, log10(700), by = 0.45), 0)) +
  scale_y_continuous(name = 'Number of players, 100 000',
                     breaks = seq(0, 10^6, by = 10^5),
                     labels = 0:10) + 
  theme(axis.text.x = element_text(angle = 35, hjust = 0.9),
        legend.position = c(0.05, 0.811)) + 
  labs(caption = 'Number of players is a cumulative quantity starting at March 2009')
  ggsave('game aging.png', width = 6.5, height = 6)

## comparing popularity distributions before and after the peak
ggplot(gameAging, aes(x = peak, y = steamspyplayersestimate, fill = peak)) + 
  geom_violin(alpha = 0.75) + 
  scale_y_continuous(name = 'Estimated number of players',
                     limits = c(0, 100)*1000,
                     breaks = seq(0, 100, by = 10)*1000,
                     labels = c(seq(0, 90000, by = 10000), '100000')) + 
  scale_fill_brewer(palette = 'Set1') + 
  labs(title = 'Distribuition of number of players before and after mid-2013 surge',
       subtitle = 'Zoomed in at [0; 100k], where the difference is most significant') +
  theme(legend.position = 'none', axis.title.x = element_blank(), 
        axis.text.x = element_text(face = 'bold')) + 
  ggsave('player proportions.png', width = 7, height = 6)

## interestingly enough, steamspy accounts for various variability factors
## in a way that implies steamspyplayersestimate = -c*100 + 0.001*steamspyplayersvariance^2
## meaning steamspyplayersvariance = sqrt(c*10^5 + 1000*steamspyplayersestimate)

# ggplot(gameAging, aes(x = steamspyplayersestimate, y = steamspyplayersvariance^2)) + 
#   geom_point() + 
#   stat_smooth(method = 'lm')
# 
# ggplot(gameAging, aes(x = steamspyowners, y = steamspyownersvariance^2)) + 
#   geom_point() + 
#   stat_smooth(method = 'lm')

## this is really odd, as number of players surpasses the number of owners sometimes
## percent > 1 is true for unpopular games, where owner number's estimate isn't reliable
gameAging %>%
  filter(percent_owners_played > 1) %>%
  select(responsename, steamspyowners, steamspyplayersestimate) %>%
  arrange(desc(steamspyowners))
## as it's the cumulative quantity, this means that 60% of the games that are bought are actually played. heh.
ggplot(gameAging, aes(x = percent_owners_played, fill = percent_owners_played > 1)) + 
  geom_histogram(breaks = seq(0, 1.5, by = 0.1), color = 'darkseagreen4', alpha = 0.65) + 
  scale_x_continuous(name = 'Percent of game owners who played it',
                     breaks = seq(0, 1.5, by = 0.1)) + 
  scale_y_continuous(breaks = seq(0, 1500, by = 250)) + 
  scale_fill_brewer(palette = 'Set1', direction = -1) + 
  theme(legend.position = 'none') + 
  ggsave('owners to players relationship.png', width = 7, height = 4)

## comparing differences in final and initial prices
## turns out they're not so different AND initial price is *always* greater than a final price
## in most cases they're completely equal so final price will be used for predictions
sum(steamGames$pricefinal == steamGames$priceinitial)
qplot(steamGames$pricefinal - steamGames$priceinitial, geom = 'histogram')

