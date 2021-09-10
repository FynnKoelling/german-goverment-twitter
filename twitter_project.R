# A short script that outputs for a given twitter account for the latest 3200 tweets:
# - a barchart with the 15 most used words as .png
# - a wordcloud with word occurence over 100 as .png
# - a sentiment barchart as .png.
# In this example we used the six (seven since we split CDU/CSU) current parties in the german Bundestag.

# Libraries
library(stopwords)   # Stop Words Datasets
library(wordcloud2)  # Wordcloud
library(syuzhet)     # Sentiment Analysis
library(htmlwidgets) # Create html from Wordcloud
library(webshot)     # Save html to png
library(rtweet)      # Collect, organize and visualize Twitter Data
library(tidytext)    # Text Mining
library(tidyverse)   # Package Collection for Data Science

# API Twitter Token
twitter_token <- create_token(
  app = "INSERT NAME",
  consumer_key = "INSERT KEY",
  consumer_secret = "INSERT SECRET",
  access_token = "INSERT TOKEN",
  access_secret = "INSERT SECRET",
  set_renv = TRUE
)

# Get Twitter Data by API
cdu <- get_timeline("@CDU", n = 3200)
csu <- get_timeline("@CSU", n = 3200)
afd <- get_timeline("@AfD", n = 3200)
fdp <- get_timeline("@fdp", n = 3200)
spd <- get_timeline("@spdde", n = 3200)
gruene <- get_timeline("@Die_Gruenen", n = 3200)
linke <- get_timeline("@dieLinke", n = 3200)

# Wrangle Twitter Data and Generate Plots
# @param dataframe: Twitter Data
# @param name: Name of current party
generate_plots <- function(dataframe, name) {
  
  # Save as .csv
  save_as_csv(dataframe, paste(name, "_data.csv"), prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
  
  # We found no satisfying solution to filter unicode emojis (ex. \ufe0f, displayed as <U+FE0F>) with regex:
  # "\\W+|\\d|_" doesn't filter all emojis and something like "[^\u0001-\u007F]" filters "ä", "ü", "ö" as well.
  # Replacing "ä", "ö", "ü" by "ae", "oe", "ue" before conflicts with stopword removal and sentiment analysis,
  # changing it back to "ä", "ö", "ü" afterwards conflicts with words like "Baerbock".
  # Trying to filter with a package that removes non-ASCII doesn't work either as "ä", "ö", "ü" are not in the 7-Bit ASCII-Code table.
  
  # Clean text
  dataframe_clean <- dataframe %>% 
    mutate(text = str_replace_all(text, "https\\S*", "")) %>%   # remove links
    mutate(text = str_replace_all(text, "@\\S*", "")) %>%       # remove mentions
    mutate(text = str_replace_all(text, "\\W+|\\d|_", " ")) %>% # remove non-word characters
    mutate(text = str_replace_all(text, "[\r\n]", "")) %>%      # remove newline
    mutate(text = str_replace_all(text, "[[:punct:]]", "")) %>% # remove punctuation
    mutate(text = str_replace_all(text, "dass", " ")) %>%       # remove dass (not covered by stopwords)
    mutate(text = str_replace_all(text, " amp ", " "))          # remove amp (possible leftover link element)
    
  ### Remove stopwords
  
  # Get stopword datasets
  stop_german <- tibble(word = stopwords::stopwords("de"))
  stop_english <- tibble(word = stopwords::stopwords("en"))
  
  # Unnest tokens
  dataframe_tokens <- dataframe_clean %>%
    select(text) %>%
    unnest_tokens(word, text)
  
  # Remove stopwords
  dataframe_tokens <- dataframe_tokens %>%
    anti_join(stop_german) %>%
    anti_join(stop_english)
  
  ### Generate column chart of the 15 most used words
  
  # Most used Words (15 in number)
  dataframe_count <- dataframe_tokens %>%
    count(word) %>%                       # count words
    slice_max(n, n = 15) %>%              # select top entries
    mutate(word = fct_reorder(word, n))   # create factor ordered for plot
  
  # Plot most frequent Words
  plt_most_words <- ggplot(dataframe_count, aes(x = word, y = n)) +
    geom_col() +                                    # column chart
    ylab("Words") + xlab("Count") +                 # axis labels
    ggtitle(paste(name, "- 15 most used words")) +  # title
    coord_flip()                                    # words on y-axis
  
  # Save plot
  ggsave(paste(name, "_bar.png"), plt_most_words, width = 10, height = 7)
  
  ### Generate Wordcloud with word count >= 100
  
  #  Get most used Words (100 in number)
  dataframe_count <- dataframe_tokens %>%
    count(word) %>%                       # count words
    slice_max(n, n = 100) %>%             # select top entries
    mutate(word = fct_reorder(word, n))   # create factor ordered for plot
  
  # control randomness of wordcloud
  set.seed(12)
  
  # generate cloud
  wc <- wordcloud2(data = dataframe_count, minRotation = 0, maxRotation = 0)
  
  ## Save Wordcloud 
  
  # convert to .html
  saveWidget(wc, "tmp.html", selfcontained = F)
  
  # save .html as .png
 webshot("tmp.html", paste(name, "_cloud.png"), vwidth = 1000, vheight = 1000, delay = 10) 
  
  ### Generate sentiment bar chart
  
  # Get dummy encodings per sentiment
  dataframe_sentiment <- get_nrc_sentiment((dataframe_tokens$word), language = "german") 
  
  # Sum encodings per sentiment
  dataframe_sentimentscores <- dataframe_sentiment %>% 
    mutate(across(.cols = everything(), .fns = sum)) %>% # Sum each column
    slice(1)                                             # Select only first row, since all rows consist now of the sum
  
  # Get long format
  dataframe_sentimentscores_long <- dataframe_sentimentscores %>% pivot_longer(cols = everything()) 
  
  # Plot Sentiment
  plt_sentiment_bar <- ggplot(data = dataframe_sentimentscores_long, aes(x = name, y = value)) +
    geom_bar(aes(fill = name), stat = "identity", show.legend = FALSE) + # make bar chart
    ylim(0, 1600) +                                                      # make y-range = 0 - 1600
    xlab("Sentiments") + ylab("Scores") +                                # axis labels
    ggtitle(paste(name, "- total sentiments based on scores")) +         # title
    theme_minimal()                                                      # white background
  
  # Save plot
  ggsave(paste(name, "_senitment.png"), plt_sentiment_bar, width = 10, height = 7)
}

#Function calls
generate_plots(gruene, "gruene")
generate_plots(cdu, "cdu")
generate_plots(csu, "csu")
generate_plots(afd, "afd")
generate_plots(fdp, "fdp")
generate_plots(spd, "spd")
generate_plots(linke, "linke")
