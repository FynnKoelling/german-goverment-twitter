# Tweet Analysis - German Goverment

created by Kea Fronzek and Fynn Linus Kölling

This repository contains a short R script that outputs for any given Twitter account for the latest 3200 tweets:
- A bar chart with the 15 most used words as .png
- A word cloud with word occurrence over 100 as .png
- A sentiment bar chart as .png.

The following techniques were used in the project:
- Use of the Twitter API
- Cleaning up the text data including removing of stopwords, punctuation, mentions and links
- Use of tidyverse for data operations
- Use of ggplot for data visualisations

In this example, we used the six (seven since we split CDU/CSU) current parties in the German Bundestag. Example images can be found in the pics folder.

Note that you need a Twitter developer account (https://developer.twitter.com/en) to run this script.
