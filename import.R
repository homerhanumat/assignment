##########################################
## import the data from excel
##########################################

sentences <- readr::read_csv("data/Experiment3.csv")

library(dplyr)

sentences2 <-
  sentences %>% 
  select(Participant, Pair, Sentence, Accent, Condition, Rating) %>% 
  rename(participant_id = Participant,
         pair_id = Pair,
         sentence_id = Sentence,
         speaker_accent = Accent,
         difficulty = Condition,
         rating = Rating) %>% 
  mutate(rating = rating * 100)



sentences3 <-
  sentences2 %>% 
  arrange(sentence_id) %>% 
  mutate(participant = rep(1:10, times = 960)) %>% 
  select(-participant_id)

sentences_real <-
  sentences3 %>% 
  tidyr::spread(key = participant, value = rating, sep = "_rating_") 

names(sentences_real)[5:14] <- paste("participant", 1:10, "rating", sep = "_")

sentences <-
  sentences_real %>% 
  arrange(pair_id)


save(sentences, file = "data/sentences.Rda")
