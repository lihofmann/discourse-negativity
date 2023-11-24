# discourse negativity acceptabiltiy ratings experiments
# graphing the data

# 1 libraries, helpers, settings
# 2 acceptability by tag
# 3 acceptability by condition
# 4 acceptability by condition and tag
## 4b acceptability means with distributions for observations
## 4c acceptability means with distributions for paritipant means


# 1 Libraries,  helpers, settings -------------------------------------------
  # set working directory to directory of script
  this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(this.dir)
  
  source('../../helpers.R')
  
  # load required packages for pre-processing data
  library(tidyverse)
  library(readr)
  library(ggplot2)
  
  # read data
  d = read_csv("../data/excluded.csv")
  d$response <- as.numeric(d$response)
  
  # Color blind friendly palette (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)):
  cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#F0E442", "#D55E00", "#CC79A7", "#000000")
  graphcolor <- "#0072B2"

# 2 acceptability by tag ------------------------------------------------
  accept_means_tag = d %>% group_by(tag) %>%
    summarize(Mean = mean(response), CILow = ci.low(response),
              CIHigh = ci.high(response)) %>%
    mutate(YMin = Mean - CILow, YMax = Mean + CIHigh,
           tag = fct_reorder(as.factor(tag), Mean), response = Mean) %>% ungroup()
  
  accept_means_tag %>% 
    ggplot(aes(x = reorder(tag, response), y = response)) +
    geom_violin(data=d, color=graphcolor) +
    geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.3,color="black") +
    geom_point(size=0.5,color="black") +
    scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                       labels = c("0","0.2","0.4","0.6","0.8","1"),
                       name = "Acceptability rating") +
    labs(title = "")+
    xlab("Tag") + 
    theme_bw()

# 3 acceptability by condition ------------------------------------------------
  
  # 3.1 aggregate for all tags
    
  accept_means_cond = d %>% group_by(condition) %>%
    summarize(Mean = mean(response), CILow = ci.low(response),
              CIHigh = ci.high(response)) %>%
    mutate(YMin = Mean - CILow, YMax = Mean + CIHigh,
           condition = fct_reorder(as.factor(condition), Mean), response = Mean) %>% 
    ungroup()
  
  accept_means_cond %>% mutate(condition = fct_reorder(condition, response)) %>%
    ggplot(aes(x = reorder(condition, response), y = response)) +
    geom_violin(data=d,scale = "width", color=graphcolor) +
    geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.3,color="black") +
    geom_point(size=0.5,color="black") +
    scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                       labels = c("0","0.2","0.4","0.6","0.8","1"),
                       name = "Acceptability ratings") +
    labs(title = "") +
    xlab("") + 
    theme_bw() +
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  
  # 3.2 by tag
    # separate three data sets for the tags
    d_wn <- d %>% filter(tag == "whynot")
    d_neither <- d %>% filter(tag == "neither")
    d_even <- d %>% filter(tag == "noteven")
    
    accept_means_cond_wn = d_wn %>% group_by(condition) %>%
      summarize(Mean = mean(response), CILow = ci.low(response),
                CIHigh = ci.high(response)) %>%
      mutate(YMin = Mean - CILow, YMax = Mean + CIHigh,
             condition = fct_reorder(as.factor(condition), Mean), response = Mean) %>% 
      ungroup()
    
    accept_means_cond_wn %>% mutate(condition = fct_reorder(condition, response)) %>%
      ggplot(aes(x = reorder(condition, response), y = response)) +
      geom_violin(data=d,scale = "width", color=graphcolor) +
      geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.3,color="black") +
      geom_point(size=0.5,color="black") +
      scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                         labels = c("0","0.2","0.4","0.6","0.8","1"),
                         name = "Acceptability ratings") +
      labs(title = "") +
      xlab("") + 
      theme_bw() +
      theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
    
    accept_means_cond_neither = d_neither %>% group_by(condition) %>%
      summarize(Mean = mean(response), CILow = ci.low(response),
                CIHigh = ci.high(response)) %>%
      mutate(YMin = Mean - CILow, YMax = Mean + CIHigh,
             condition = fct_reorder(as.factor(condition), Mean), response = Mean) %>% 
      ungroup()
    
    accept_means_cond_neither %>% mutate(condition = fct_reorder(condition, response)) %>%
      ggplot(aes(x = reorder(condition, response), y = response)) +
      geom_violin(data=d,scale = "width", color=graphcolor) +
      geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.3,color="black") +
      geom_point(size=0.5,color="black") +
      scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                         labels = c("0","0.2","0.4","0.6","0.8","1"),
                         name = "Acceptability ratings") +
      labs(title = "") +
      xlab("") + 
      theme_bw() +
      theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
    
    accept_means_cond_even = d_even %>% group_by(condition) %>%
      summarize(Mean = mean(response), CILow = ci.low(response),
                CIHigh = ci.high(response)) %>%
      mutate(YMin = Mean - CILow, YMax = Mean + CIHigh,
             condition = fct_reorder(as.factor(condition), Mean), response = Mean) %>% 
      ungroup()
    
    accept_means_cond_even %>% mutate(condition = fct_reorder(condition, response)) %>%
      ggplot(aes(x = reorder(condition, response), y = response)) +
      geom_violin(data=d,scale = "width", color=graphcolor) +
      geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.3,color="black") +
      geom_point(size=0.5,color="black") +
      scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                         labels = c("0","0.2","0.4","0.6","0.8","1"),
                         name = "Acceptability ratings") +
      labs(title = "") +
      xlab("") + 
      theme_bw() +
      theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
    
# 4 acceptability by condition and tag -----------------------------------------
  tcmeans = d %>% group_by(condition, tag) %>%
    summarize(Mean = mean(response), CILow = ci.low(response), 
              CIHigh = ci.high(response)) %>%
    mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, 
           condition = fct_reorder(as.factor(condition),Mean)) %>%
    mutate(condition = fct_reorder(condition, Mean, .fun = mean)) %>%
    mutate(tag = fct_reorder(tag, Mean, .fun = mean)) %>% ungroup()
  
  # plot for comparing all tags ----
  tcmeans %>% 
    ggplot(aes(x=fct_reorder(condition, Mean), y=Mean, group = tag, color = tag)) +
    geom_point(aes(shape = tag), size = 4) + 
    geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1) +
    geom_line() +
    xlab("") +
    scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), 
                       labels = c("0","0.2","0.4","0.6","0.8","1"),
                       name = "Mean acceptabiltiy rating") +
    scale_shape_manual(values = c("W", "N", "E")) +
    scale_colour_manual(values=cbbPalette) +
    theme_bw() +
    # theme(legend.position = "none") +
    theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
  
  tcmeans <- tcmeans %>% mutate(response = Mean)
  
  # plot with distributions ----
  tcmeans %>% 
    ggplot(aes(x=fct_reorder(condition, response), y = response)) +
    geom_violin(data=d, scale="width", color=graphcolor) +
    geom_errorbar(aes(ymin=YMin,ymax=YMax),width = 0.5, color="black") +
    geom_point(size=0.5,color="black") +
    xlab("") +
    scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                       labels = c("0","0.2","0.4","0.6","0.8","1"),
                       name = "Mean acceptability rating") +
    facet_grid(rows = vars(tag)) +
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  