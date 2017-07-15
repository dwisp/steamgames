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

library(tidyverse)
genres <- 
  steamGames %>% 
  select(contains('GenreIs')) %>%
  mutate_all(funs(as.logical)) %>%
  summarise_all(funs(sum)) %>%
  unlist()

# genres are non-mutally exclusive
library(stringr)
print(str_c('Total games: ', nrow(steamGames), '; total by genres: ', sum(genres)))

library(ggplot2)
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

library(forcats)
# it looks like more than half of Steam games can be considered indie :D
ggplot(data = genres.df, aes(weight = n, x = genre, fill = popularity)) + 
  geom_bar(alpha = 0.7) + 
  geom_rect(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 8000, alpha = 0, linetype = 1, color = 'darkred') + 
  geom_rect(xmin = 4.52, xmax = 8.5, ymin = 0, ymax = 2800, alpha = 0, linetype = 1, color = 'goldenrod4') + 
  geom_rect(xmin = 8.52, xmax = 13.5, ymin = 0, ymax = 800, alpha = 0, linetype = 1, color = 'darkgreen') + 
  scale_y_continuous(breaks = seq(0, 8000, by = 1000)) + 
  scale_fill_brewer(palette = 'Set1') +
  guides(fill = guide_legend(title = 'Popularity'))
  ggsave('genre freq naive.png', width = 8.5, height = 6)









