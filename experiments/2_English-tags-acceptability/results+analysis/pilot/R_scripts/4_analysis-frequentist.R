# discourse negativity acceptability ratings experiments
# data analysis

# 0 -- load data and settings

# 1 -- testing the hypothesis: pos baseline vs other conditions (each tag in its own data set)
## 1.1 - linear models
## 1.2 - beta regression

# 2 -- testing for differences between conditions (exploratory) -- (a kind of) difference coding (+ reference level)
## 2.1 linear
## 2.2 beta regression

# 3 -- testing for interactions of tag / condition (exploratory) -- do I have enough data for this
## 3.1 linear
## 3.2 beta regression

# 4 -- testing for differences between tags
# 4.1 linear
# 4.2 beta regression

# -----------------------------------------------------------------------------
# the packages for frequentist models have some drawbacks: betareg has no mixed effects
# lme4 only linear, but our data is not in normal distribution
# we will do bayesian mixed effects beta regression, but explore some models here as well
#
# 0 ---------------------------------------------------------------------------
# load data and settings
  # set working directory to directory of script
  this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(this.dir)
  
  # load packages
  library(readr)
  library(tidyverse)
  library(lme4)
  library(lmerTest)
  library(betareg)
  library(hypr)
    
  # read data
  d = read_csv("../data/excluded.csv")
  
  # make sure data types makes sense
  d$response <- as.numeric(d$response)
  summary(d$response)
  
  d$condition <- as.factor(d$condition)
  levels(d$condition)
  d <- d %>% mutate(condition = fct_reorder(condition, response, .fun = mean))
  contrasts(d$condition)
  
  d$tag <- as.factor(d$tag)
  levels(d$tag)
  
  d$participant <- as.factor(d$participant)
  levels(d$participant)
  d$item <- as.factor(d$item)
  levels(d$item)
  
  # scaling transform from closed unit interval [0,1], to  open unit interval (0,1), excluding boundaries
  # using method used in Degen \& Tonhauser (2022), from Smithson \& Verkuilen (2006)
  #  y' = (y · (n − 1) + 0.5)/n
  d$betaresponse <- (d$response*(nrow(d)-1) + .5)/nrow(d)
  d$betaresponse <- as.numeric(d$betaresponse)
  summary(d$betaresponse)
  
  # separate three data sets for the tags
  d_wn <- d %>% filter(tag == "whynot")
  d_neither <- d %>% filter(tag == "neither")
  d_even <- d %>% filter(tag == "noteven")

# 1 - testing the hypothesis: pos baseline vs other conditions ---- 
# for each of the three tags
  
  ## 1.1 linear models
  d$condition <- relevel(d$condition, ref="think")
  contrasts(d$condition)
  
  # ## look at aggregate for all tags
  # lm1 <- lmer(response ~ condition + (condition | participant) + (condition | item), data = d)
  # lm2 <- lmer(response ~ condition + (1 | participant) + (condition | item), data = d)
  # lm3 <- lmer(response ~ condition + (condition | participant) + (1 | item), data = d)
  lm4 <- lmer(response ~ condition + (1 | participant) + (1 | item), data = d)
  
  save.image("linear-models.RData")
  load("linear-models.RData")
  
  print(summary(lm4), cor=F, dig=3)
  
  # ## look at "why not"
  # # check: what is the largest model structure that converges?
  wn_lm1 <- lmer(response ~ condition + (condition | participant) + (condition | item), data = d_wn)
  wn_lm2 <- lmer(response ~ condition + (1 | participant) + (condition | item), data = d_wn)
  wn_lm3 <- lmer(response ~ condition + (condition | participant) + (1 | item), data = d_wn)
  wn_lm4 <- lmer(response ~ condition + (1 | participant) + (1 | item), data = d_wn)
  #

  save.image("linear-models.RData")
  load("linear-models.RData")

  print(summary(wn_lm4), cor=F, dig=3)
  
  ## look at "neither" tags
  # check: what is the largest model structure that converges?
  # neither_lm1 <- lmer(response ~ condition + (condition | participant) + (condition | item), data = d_neither)
  # neither_lm2 <- lmer(response ~ condition + (1 | participant) + (condition | item), data = d_neither)
  # neither_lm3 <- lmer(response ~ condition + (condition | participant) + (1 | item), data = d_neither)
  # neither_lm4 <- lmer(response ~ condition + (1 | participant) + (1 | item), data = d_neither)
  
  save.image("linear-models.RData")
  load("linear-models.RData")
  
  print(summary(neither_lm4), cor=F, dig=3)
  
  ## look at "not even" tags
  # check: what is the largest model structure that converges?
  even_lm1 <- lmer(response ~ condition + (condition | participant) + (condition | item), data = d_even)
  even_lm2 <- lmer(response ~ condition + (1 | participant) + (condition | item), data = d_even)
  even_lm3 <- lmer(response ~ condition + (condition | participant) + (1 | item), data = d_even)
  even_lm4 <- lmer(response ~ condition + (1 | participant) + (1 | item), data = d_even)
  
  save.image("linear-models.RData")
  load("linear-models.RData")
  
  print(summary(even_lm4), cor=F, dig=3)
  
  ## 1.2 beta regression models
  # betareg package for frequentist beta regression supports only fixed effects models :(
  
  bm1 <- betareg(betaresponse ~ condition, data = d)
  summary(bm1)
  bm2 <- betareg(betaresponse ~ condition, data = d_wn)
  summary(bm2)
  bm3 <- betareg(betaresponse ~ condition, data = d_neither)
  summary(bm3)
  bm4 <- betareg(betaresponse ~ condition, data = d_even)
  summary(bm4)

  
  
  
  

  

# 2 - testing for differences between conditions ----
  # difference coding with pos baseline
  
  d <- d %>% mutate(condition = fct_reorder(condition, response, .fun = mean))
  contrasts(d$condition)
  
  # we want a sliding contrast but still keep a reference level
  # i.e. the intercept should be the pos baseline
  # then we want to test differences between "adjacent" conditions
  # the order between condtions will be based on the means in the data, so this part of the analysis will have to be somewhat post hoc
  
  # we can get the right contrast coding from the hypr package, which specifies a contrast matrix from a set of hypotheses
  # here, the comparisons between "adjacent" conditions are based on the pilot data and the order will likely change
  
  ref_slide <- hypr(~think, notview~think, rumor~notview, asif~rumor, notopinion~asif, 
       wrong~notopinion, like~wrong, notimpression~like, notthink~notimpression, 
       notfeeling~notthink, disagree~notfeeling, lie~disagree, thinknot~lie, 
       doubt~thinknot, notbelieve~doubt,  levels = levels(d$condition))
  
  # hypr object containing 15 null hypotheses:
  #   H0.1: 0 = think                     (Intercept)
  # H0.2: 0 = notview - think
  # H0.3: 0 = rumor - notview
  # H0.4: 0 = asif - rumor
  # H0.5: 0 = notopinion - asif
  # H0.6: 0 = wrong - notopinion
  # H0.7: 0 = like - wrong
  # H0.8: 0 = notimpression - like
  # H0.9: 0 = notthink - notimpression
  # H0.10: 0 = notfeeling - notthink
  # H0.11: 0 = disagree - notfeeling
  # H0.12: 0 = lie - disagree
  # H0.13: 0 = thinknot - lie
  # H0.14: 0 = doubt - thinknot
  # H0.15: 0 = notbelieve - doubt
  # 
  # Call:
  #   hypr(~think, ~notview - think, ~rumor - notview, ~asif - rumor, 
  #        ~notopinion - asif, ~wrong - notopinion, ~like - wrong, ~notimpression - 
  #          like, ~notthink - notimpression, ~notfeeling - notthink, 
  #        ~disagree - notfeeling, ~lie - disagree, ~thinknot - lie, 
  #        ~doubt - thinknot, ~notbelieve - doubt, levels = c("think", 
  #                                                           "notview", "rumor", "asif", "notopinion", "wrong", "like", 
  #                                                           "notimpression", "notthink", "notfeeling", "disagree", "lie", 
  #                                                           "thinknot", "doubt", "notbelieve"))
  # 
  # Hypothesis matrix (transposed):
  #   [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15]
  # think          1   -1    0    0    0    0    0    0    0    0     0     0     0     0     0   
  # notview        0    1   -1    0    0    0    0    0    0    0     0     0     0     0     0   
  # rumor          0    0    1   -1    0    0    0    0    0    0     0     0     0     0     0   
  # asif           0    0    0    1   -1    0    0    0    0    0     0     0     0     0     0   
  # notopinion     0    0    0    0    1   -1    0    0    0    0     0     0     0     0     0   
  # wrong          0    0    0    0    0    1   -1    0    0    0     0     0     0     0     0   
  # like           0    0    0    0    0    0    1   -1    0    0     0     0     0     0     0   
  # notimpression  0    0    0    0    0    0    0    1   -1    0     0     0     0     0     0   
  # notthink       0    0    0    0    0    0    0    0    1   -1     0     0     0     0     0   
  # notfeeling     0    0    0    0    0    0    0    0    0    1    -1     0     0     0     0   
  # disagree       0    0    0    0    0    0    0    0    0    0     1    -1     0     0     0   
  # lie            0    0    0    0    0    0    0    0    0    0     0     1    -1     0     0   
  # thinknot       0    0    0    0    0    0    0    0    0    0     0     0     1    -1     0   
  # doubt          0    0    0    0    0    0    0    0    0    0     0     0     0     1    -1   
  # notbelieve     0    0    0    0    0    0    0    0    0    0     0     0     0     0     1   
  # 
  # Contrast matrix:
  #   [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15]
  # think         1    0    0    0    0    0    0    0    0    0     0     0     0     0     0    
  # notview       1    1    0    0    0    0    0    0    0    0     0     0     0     0     0    
  # rumor         1    1    1    0    0    0    0    0    0    0     0     0     0     0     0    
  # asif          1    1    1    1    0    0    0    0    0    0     0     0     0     0     0    
  # notopinion    1    1    1    1    1    0    0    0    0    0     0     0     0     0     0    
  # wrong         1    1    1    1    1    1    0    0    0    0     0     0     0     0     0    
  # like          1    1    1    1    1    1    1    0    0    0     0     0     0     0     0    
  # notimpression 1    1    1    1    1    1    1    1    0    0     0     0     0     0     0    
  # notthink      1    1    1    1    1    1    1    1    1    0     0     0     0     0     0    
  # notfeeling    1    1    1    1    1    1    1    1    1    1     0     0     0     0     0    
  # disagree      1    1    1    1    1    1    1    1    1    1     1     0     0     0     0    
  # lie           1    1    1    1    1    1    1    1    1    1     1     1     0     0     0    
  # thinknot      1    1    1    1    1    1    1    1    1    1     1     1     1     0     0    
  # doubt         1    1    1    1    1    1    1    1    1    1     1     1     1     1     0    
  # notbelieve    1    1    1    1    1    1    1    1    1    1     1     1     1     1     1
  
  contrasts(d$condition) <- cmat(ref_slide, remove_intercept = FALSE)
  contrasts(d$condition)
  
  # get new data sets for the tags with new contrast coding
  d_wn <- d %>% filter(tag == "whynot")
  d_neither <- d %>% filter(tag == "neither")
  d_even <- d %>% filter(tag == "noteven")
  
  ## 3.1. linear models
  
  # ## look at aggregate for all tags
  dif_lm1 <- lmer(response ~ condition + (condition | participant) + (condition | item), data = d)
  dif_lm2 <- lmer(response ~ condition + (1 | participant) + (condition | item), data = d)
  dif_lm3 <- lmer(response ~ condition + (condition | participant) + (1 | item), data = d)
  dif_lm4 <- lmer(response ~ condition + (1 | participant) + (1 | item), data = d)
  
  save.image("linear-models.RData")
  load("linear-models.RData")
  
  print(summary(dif_lm4), cor=F, dig=3)
  
  # ## look at "why not"
  # # check: what is the largest model structure that converges?
  dif_wn_lm1 <- lmer(response ~ condition + (condition | participant) + (condition | item), data = d_wn)
  dif_wn_lm2 <- lmer(response ~ condition + (1 | participant) + (condition | item), data = d_wn)
  dif_wn_lm3 <- lmer(response ~ condition + (condition | participant) + (1 | item), data = d_wn)
  dif_wn_lm4 <- lmer(response ~ condition + (1 | participant) + (1 | item), data = d_wn)
  #
  
  save.image("linear-models.RData")
  load("linear-models.RData")
  
  print(summary(dif_wn_lm4), cor=F, dig=3)
  
  ## look at "neither" tags
  # check: what is the largest model structure that converges?
  # dif_neither_lm1 <- lmer(response ~ condition + (condition | participant) + (condition | item), data = d_neither)
  # dif_neither_lm2 <- lmer(response ~ condition + (1 | participant) + (condition | item), data = d_neither)
  # dif_neither_lm3 <- lmer(response ~ condition + (condition | participant) + (1 | item), data = d_neither)
  # dif_neither_lm4 <- lmer(response ~ condition + (1 | participant) + (1 | item), data = d_neither)
  
  save.image("linear-models.RData")
  load("linear-models.RData")
  
  print(summary(dif_neither_lm4), cor=F, dig=3)
  
  ## look at "not even" tags
  # check: what is the largest model structure that converges?
  dif_even_lm1 <- lmer(response ~ condition + (condition | participant) + (condition | item), data = d_even)
  dif_even_lm2 <- lmer(response ~ condition + (1 | participant) + (condition | item), data = d_even)
  dif_even_lm3 <- lmer(response ~ condition + (condition | participant) + (1 | item), data = d_even)
  dif_even_lm4 <- lmer(response ~ condition + (1 | participant) + (1 | item), data = d_even)
  
  save.image("linear-models.RData")
  load("linear-models.RData")
  
  print(summary(dif_even_lm4), cor=F, dig=3)
  
  ## 3.2. beta regression models
  
  dif_bm1 <- betareg(betaresponse ~ condition, data = d)
  summary(dif_bm1)
  dif_bm2 <- betareg(betaresponse ~ condition, data = d_wn)
  summary(dif_bm2)
  dif_bm3 <- betareg(betaresponse ~ condition, data = d_neither)
  summary(dif_bm3)
  dif_bm4 <- betareg(betaresponse ~ condition, data = d_even)
  summary(dif_bm4)
  
# 3 - testing for interactions of tag / condition (exploratory) ----
  
  d <- d %>% mutate(condition = fct_reorder(condition, response, .fun = mean))
  trt_contrasts <- contr.treatment(15)
  colnames(trt_contrasts) <- levels(d$condition)[-1]
  contrasts(d$condition) <- trt_contrasts
  contrasts(d$condition)
  
  d <- d %>% mutate(tag = fct_reorder(tag, response, .fun = mean))
  contrasts(d$tag)
  
  # 4.1. linear models
  int_lm1 <- lmer(response ~ condition * tag + (condition * tag | participant) + (condition * tag | item), data = d)
  int_lm2 <- lmer(response ~ condition * tag + (condition | participant) + (condition | item), data = d)
  int_lm3 <- lmer(response ~ condition * tag + (condition | participant) + (1 | item), data = d)
  int_lm4 <- lmer(response ~ condition * tag + (tag | participant) + (tag | item), data = d)
  int_lm5 <- lmer(response ~ condition * tag + (1 | participant) + (1 | item), data = d)

  summary(int_lm5)
  
  # 4.2 beta regression
  int_bm <- betareg(betaresponse ~ condition * tag, data = d)
  summary(int_bm)

# 4 - testing for differences between tags ----
  
  # 5.1 linear
  
  t_lm1 <- lmer(response ~ tag + (tag | participant) + (tag | item), data = d)
  summary(t_lm1)
  
  # 5.2 beta regression
  t_bm <- betareg(betaresponse ~ tag, data = d)
  summary(t_bm)
  