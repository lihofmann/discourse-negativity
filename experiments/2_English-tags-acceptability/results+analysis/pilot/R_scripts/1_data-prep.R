# discourse negativity acceptabiltiy ratings experiments
# data prep

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages for pre-processing data
library(tidyverse)
library(readr)

# read in the raw data
d = read_csv("../data/combined.csv")

# select and rename needed columns
d = d %>% mutate(response = response, item = itemID, condition = condition,
                 tag = tag, trial_index = trial_index, participant = participant_id, .keep = "none")

# replace participant by running number
length(unique(d$participant)) #
d$participant <- as.factor(as.character(d$participant))
levels(d$participant) <- 1:length(unique(d$participant))
# d$participant <- match(d$participant, unique(sort(d$participant)))
table(d$participant)

# get demographics info
dg <- d %>% ##### 
  filter(trial_index == "54") %>%
  select(c(participant,response)) %>%
  mutate(age = str_extract(response, "[:digit:]{2}"),
         gender = str_extract(response, "(?<=gender[:punct:]{3})[:alpha:]"),
         lang = str_detect(response, "(?<=language[:punct:]{3})y"),
         amE = str_detect(response, "(?<=amE[:punct:]{3})y"),
         comments = str_extract(response, "(?<=comments[:punct:]{3}).*(?=\")"),
         .keep = "unused"
         )
view(dg)

write_csv(dg, file="../data/demographic.csv")

# add demographics data back to data #####
d = left_join(d, dg, by = "participant")

# keep only rows w acceptability rating observations
d <- d %>% filter(!is.na(condition)) %>% select(!comments)

# make response a numeric column, with values between 0 and 1
summary(d$response)
d$response <- as.numeric(d$response)
d$response <- d$response/100
table(d$response)

table(d$condition) # 15 conditions
table(d$item) # 15 items
table(d$tag) # 3 tags
table(d$condition, d$item)

write_csv(d, file="../data/data_all.csv")
