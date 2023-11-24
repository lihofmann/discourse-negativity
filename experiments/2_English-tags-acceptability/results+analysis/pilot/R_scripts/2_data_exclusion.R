# discourse negativity acceptabiltiy ratings experiments
# demographic data + data exclusion

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages for pre-processing data ####
library(tidyverse)
library(readr)

# read in the raw data
d = read_csv("../data/data_all.csv")
# read demographic data
dg = read_csv("../data/demographic.csv")


# DEMOGRAPHIC INFO #####

# participant info -- average age
table(dg$age) #xx-xx
length(which(is.na(dg$age))) # 0 missing values
str(boxplot(dg$age)) # 0 outliers (possibly remove for calculation of mean?)
mean(dg$age,na.rm=TRUE) # 30.8

# participant info -- gender
dg$gender <- as.factor(dg$gender)
levels(dg$gender)
# levels(dg$gender) <- c("f", "m")#, "nb", N/A)
table(dg$gender)

# only include data from native speakers of american english
# exclude non-English speakers
length(which(is.na(d$lang))) #no missing responses
table(d$lang)
length(which(is.na(d$amE))) #0 (everybody responded)
table(d$amE) 
d <- d %>% filter(lang==TRUE & amE==TRUE) %>% droplevels()
length(unique(d$participant)) # 4

#####
##### data exclusion: we exclude data from participants, which satisfy at least TWO of the following FOUR criteria ####
# low (more than 2sd away from mean) overall variance - clicked same region of slider each time
# rating for pos controls is high (more than 2sd away from mean)
# rating for neg controls is low (more than 2sd away from mean)
# difference in rating between pos+neg controls is low (more than 2sd away from mean difference)

d$response <- as.numeric(d$response)
summary(d$response)

# plot responses by condition
table(d$condition, d$response)
d %>%  mutate(condition = fct_reorder(condition, response, 
                                 .fun = 'mean')) %>%
  ggplot(aes(x = response, group = condition)) +
  geom_density() +
  facet_grid(rows = vars(condition))

# a closer look at the controls
# plot responses by good controls, to see if they worked as expected
cond_means = d %>% #filter(condition = "think")
  group_by(condition) %>%
  summarize(Mean = mean(response),CI.Low = ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)
cond_means

cond_means %>% filter(condition == "think" | condition == "thinknot" )  %>%
  ggplot(aes(x=condition,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  ylab("Mean response")

# plot control responses by participant
d %>% filter(condition == "think" | condition == "thinknot" )  %>%
  mutate(condition = fct_reorder(condition, response, .fun = 'mean')) %>%
  ggplot(aes(x = response, group = condition)) + 
  geom_density() +
  facet_grid(rows = vars(condition))


# for each participant: get mean overall rating, CIs, and variance
participant_summaries <- d %>% 
  group_by(participant) %>% 
  summarize(mean = mean(response), CIlow = ci.low(response), 
            CIhigh = ci.high(response), variance = var(response))
# check if overall variance is low: clicked on same region of scale most of the time
participant_summaries <- participant_summaries %>% 
  mutate(varTooLow = variance < mean(variance) - 2*sd(variance))

# get high/low control ratings and info about whether outlier
pos_controls <- d %>% filter(condition == "think") %>% 
  select (participant, posControl = response) %>% 
  mutate(posTooHigh = posControl < mean(posControl) - 2*sd(posControl))
neg_controls <- d %>% filter(condition == "thinknot") %>% 
  select (participant, negControl = response) %>% 
  mutate(negTooLow = negControl < mean(negControl) - 2*sd(negControl))
participant_summaries <- participant_summaries %>% 
  left_join(pos_controls, by = "participant") %>% 
  left_join(neg_controls, by = "participant")

participant_summaries <- participant_summaries %>% 
  mutate(control_difference = negControl - posControl, differenceTooSmall = control_difference < mean(control_difference) - 2*sd(control_difference))

# check if participants meet at least TWO of the FOUR exclusion criteria
participant_summaries <- participant_summaries %>% 
  mutate(exclude = 1 < (as.numeric(varTooLow) + as.numeric(posTooHigh) + 
                          as.numeric(negTooLow)  + as.numeric(differenceTooSmall)))
View(participant_summaries)
length(participant_summaries$participant[participant_summaries$exclude])
# x participants excluded

# exclude all participants identified above
excludeParts <- participant_summaries$participant[participant_summaries$exclude]  
d <- d %>%
  filter(!(participant %in% excludeParts)) %>%
  droplevels()
length(unique(d$participant)) # x, so x participants excluded


# DEMOGRAPHIC INFO ON REMAINING PARTICIPANTS #####
# participant info -- average age
table(d$age) #19-53
length(which(is.na(d$age))) # 0 missing values
str(boxplot(dg$age)) # 0 outliers (possibly remove?)
mean(dg$age,na.rm=TRUE) # 30.8

# participant info -- gender
dg$gender <- as.factor(dg$gender)
levels(dg$gender)
# levels(dg$gender) <- c("f", "m")#, "nb", N/A)
table(dg$gender)

write_csv(d, file="../data/excluded.csv")

