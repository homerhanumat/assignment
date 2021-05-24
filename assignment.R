########################################################
## code for the matching algorithm, and to produce
## materials for Dr. Incera's research assistants
########################################################

## setup ----

library(tidyverse)
load("data/sentences.Rda")

## Utility ----

## Utility function to measure the difference in difficulty ratings.
## Use the Yuen t-test that allows for working with trimmed means.
## Returns the test-statistic and the P-value.
profile_diff <- function(x, y, trim) {
  test <- DescTools::YuenTTest(x, y, trim = trim, paired = TRUE)
  list(
    statistic = abs(test$statistic),
    p_value = test$p.value
  )
}

## Utility function
ratings_from_pair <- function(pair) {
  foreign <- pair %>% 
    filter(speaker_accent == "Foreign") %>% 
    select(ends_with("rating")) %>% 
    t() %>% 
    as.matrix()
  native <- pair %>% 
    filter(speaker_accent == "Native") %>% 
    select(ends_with("rating")) %>% 
    t() %>% 
    as.matrix()
  list(foreign = foreign, native = native)
}

## Utility function to make matrices of statistics and P-values
## for all pairs of sentences:  one from Speaker A (Foreign), the other
## from speaker B (Native).
make_diff_matrix <- function(pair, trim) {
  ratings <- ratings_from_pair(pair)
  a <- ratings$foreign
  b <- ratings$native
  mat_statistics <- matrix(0, nrow = ncol(a), ncol = ncol(b))
  mat_pvals <- matrix(0, nrow = ncol(a), ncol = ncol(b))
  for (i in 1:ncol(a)) {
    for (j in 1:ncol(b)) {
      result <- profile_diff(x = a[, i], y = b[, j], trim = trim)
      mat_statistics[i, j] <- result$statistic
      mat_pvals[i, j] <- result$p_value
    }
  }
  rownames(mat_statistics) <- pair %>% 
    filter(speaker_accent == "Foreign") %>%
    pull(sentence_id)
  colnames(mat_statistics) <- pair %>% 
    filter(speaker_accent == "Native") %>%
    pull(sentence_id)
  rownames(mat_pvals) <- pair %>% 
    filter(speaker_accent == "Foreign") %>%
    pull(sentence_id)
  colnames(mat_pvals) <- pair %>% 
    filter(speaker_accent == "Native") %>%
    pull(sentence_id)
  list(
    mat_statistics = mat_statistics,
    mat_pvals = mat_pvals
  )
}

## Utility function.  Given a pair of speakers, find the best
## matching of sentences.  Uses brute force to go through
## all possible pairs of subsets of cardinality "select" from speaker A,
## with each A-set using the Hungarian Algorithm
## to find the best matching set of sentences from Speaker B.
## Keeps track of the best matching.
## Returns a data frame of results.
get_best <- function(pair, select, trim) {
  
  ## get sentence ids for each speaker:
  a_sentence_ids <- pair %>% 
    filter(speaker_accent == "Foreign") %>% 
    pull(sentence_id)
  b_sentence_ids <- pair %>% 
    filter(speaker_accent == "Native") %>% 
    pull(sentence_id)
  
  ## compute matrices of difference-statisitics and p-values,
  ## for each pair of sentences
  diff_mats <- make_diff_matrix(pair, trim = trim)
  
  ## make all possible subsets of cardinality select from
  ## the sentences for A-speker
  size <- nrow(diff_mats[[1]])
  a_subsets_numeric <- utils::combn(x = size, m = select)
  
  ## prepare to loop through all A-speaker subsets
  n <- ncol(a_subsets_numeric)
  a_best <- ""
  b_best <- ""
  statistic <- 0
  pval <- 0
  diff_best <- Inf
  
  ## begin looping
  for (i in 1:n) {
    
    ## extract sentence ids from the numbers:
    subset_a <- a_sentence_ids[a_subsets_numeric[, i]]
    
    ## extract the relvant portion of the difference matrices
    dms <- diff_mats$mat_statistics[subset_a, ]
    
    ## Now comes the Hungarian Algorithm ...
    solution <- clue::solve_LSAP(x = dms, maximum = FALSE)
    
    ## extract B-speaker sentence-ids from the solution
    subset_b <- b_sentence_ids[solution]
    
    ## get staitics and p-values for each pair in the
    ## best matching
    sentence_pair_statistics <- numeric(select)
    sentence_pair_pvals <- numeric(select)
    for (i in 1:select) {
      a_location <- subset_a[i]
      b_location <- subset_b[i]
      sentence_pair_statistics[i] <- diff_mats$mat_statistics[a_location, b_location]
      sentence_pair_pvals[i] <- diff_mats$mat_pvals[a_location, b_location]
    }
    
    ## check to see if this best match is better than a best-match
    ## froma previously-analysed A-subsets:
    sum_statistics <- sum(sentence_pair_statistics)
    if (sum_statistics < diff_best) {
      diff_best <- sum_statistics
      a_best <- subset_a
      b_best <- subset_b
      statistic <- sentence_pair_statistics
      pval <- sentence_pair_pvals
    }
  }
  
  ## return data frame of results for the best possible matching;
  data.frame(
    foreign = a_best,
    native = b_best,
    statistic = statistic,
    pval = pval)
}

## match_sentences ----

## This is the function you'll actually use.
## data is the original data frame
## select = desired number of sentence-pairs
##
## trim is there in case researchers desire to omit
## very high or low ratings.  (Setting trim = 0.1 would knock out
## the highest and the lowest of the ten differences in ratings.)
##
## Setting trace to TRUE results in a progress report to the console
## (not needed for the current small study)
##
## Result is a list of data frames, one for each speaker-pair,
## saying which sentence goes to which, and reporting the P-values
## of the Yuen T-Test.  This allows the user to flag pairs where the
## ratings are "too different".
match_sentences <- function(data, size, select, trim = 0, trace = FALSE) {
  pair_ids <- sort(unique(data$pair_id))
  pairs <- length(unique(data$pair_id))
  lst <- vector(mode = "list", length = pairs)
  for (i in 1:pairs) {
    if (trace) {
      cat("Working on speaker pair with id", pair_ids[i], "...\n")
    }
    pair <- data %>% 
      filter(pair_id == pair_ids[i])
    results <-  get_best(pair, select, trim)
    lst[[i]] <- results
  }
  names(lst) <- pair_ids
  lst
}

## Data Frame for Research Assistants ---------

results <- match_sentences(
  data = sentences,
  select = 8,
  trim = 0,  ## the default, actually,
  trace = FALSE
)

results_df_1 <-
  results %>% 
  map_dfr(.f = function(df) {
    data.frame(
      first = as.character(df$foreign),
      second = as.character(df$native),
      stringsAsFactors = FALSE
    )
  })

results_df_2 <-
  data.frame(
    first = results_df_1$second,
    second = results_df_1$first
  )

results_df <-
  rbind(results_df_1, results_df_2) %>% 
  rename(sentence_id = first, paired_with = second)

s2 <- 
  sentences %>% 
  mutate(SentenceRating = rowMeans(select(sentences, ends_with("rating")))) %>% 
  select(sentence_id, pair_id, speaker_accent, SentenceRating) %>% 
  inner_join(results_df, by = "sentence_id")

match_rating <-
  s2$sentence_id %>% 
  map_dbl(.f = function(id) {
    s2 %>% filter(paired_with == id) %>% pull(SentenceRating)
  })

sentence_info <-
  s2 %>% 
  mutate(other_rating = match_rating) %>% 
  mutate(MatchingRating = rowMeans(select(., SentenceRating, other_rating))) %>% 
  select(-other_rating) %>% 
  arrange(pair_id, MatchingRating) %>% 
  mutate(Difficulty = rep(rep(c("Easy", "Difficult"), each = 8), times = 40)) %>% 
  select(-paired_with) %>% 
  rename(Sentence = sentence_id, Pair = pair_id, Accent = speaker_accent)

readr::write_csv(sentence_info, path = "data/sentence_info.csv")

## Corpus Sentence IDs (for Research Assistants) -------------

selected_sentences <-
  results %>% 
  map(
    .f = function(df) {
      c(as.character(df$foreign), as.character(df$native))
    }
  ) %>% 
  unlist()

## save selected sentence ids:
selected_sentences_frame <-
  data.frame(
    sentence_id = selected_sentences
  )
readr::write_csv(
  selected_sentences_frame, 
  path = "data/selected_sentences_frame.csv"
)

sentence_info_correct3 <-
  sentence_info %>% 
  filter(Pair %in% c(14, 33, 45))
readr::write_csv(
  sentence_info_correct3, 
  path = "data/sentence_info_correct3.csv"
)

sentence_info_correct4 <-
  sentence_info %>% 
  filter(Pair %in% c(33))
readr::write_csv(
  sentence_info_correct4, 
  path = "data/sentence_info_correct4.csv"
)


