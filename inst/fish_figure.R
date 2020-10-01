
library(tidyverse)

make_groups <- function(tag, val) {
  
  r <- rle(val) # where 'val' is the 0/1 column
  
  # for each contiguous group:
  #   apply flatten_chr() to the letter corresponding to the ith value of the 
  #   lengths column in r
  
  purrr::flatten_chr(purrr::map(1:length(r$lengths), function(i) { 
    rep(LETTERS[i], r$lengths[i])
  })) -> grps # save as new object
  
  sprintf("%s.%s", tag, grps) # concatenate the tag and the letter values 
  # into a single string.
  
}


encounters <- read_csv("fishdata.csv") %>%
  mutate(
    TagID = factor(TagID),
    Station = factor(Station, levels = unique(d$Station))) %>% 
  group_by(TagID) %>% 
  mutate(grp = make_groups(TagID, value)) %>%
  ungroup()


encounters2 <- group_by(encounters, TagID) %>% 
  filter(value != 0) %>%  
  ungroup() 

encounters2 %>%
  ggplot(aes(x = Station, y = TagID)) +
  geom_path(aes(group = grp), size = 0.25) +
  geom_point(shape = 18, size = 2) +
  labs(title = "Encounter histories of tagged Chinook salmon smolts",
       subtitle = "Upstream to downstream") +
  ggsave("fish_encounters.png")


