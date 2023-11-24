# discourse negativity acceptability ratings experiments
# data analysis
# bayesian mixed effects regression

# 0 -- load data and settings

# 1 -- testing the hypothesis: pos baseline vs other conditions (all, then each tag in its own data set)
## 1.1 - condition as fixed effect, full ranef structure
## 1.2 - beta-regression w condition as random effect

# 2 -- testing for differences between conditions (exploratory)
# 3 -- testing for interactions of tag / condition (exploratory)
# 4 -- testing for differences between tags


# -----------------------------------------------------------------------------
# 0 ---------------------------------------------------------------------------
# load data and settings

  # set working directory to directory of script
  this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(this.dir)
  
  # load packages / options
  library(readr)
  library(tidyverse)
  library(rstan)
  options(mc.cores=parallel::detectCores())
  rstan_options(auto_write=TRUE)
  library(brms)
  library(bayesplot)
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
  d$tag <- relevel(d$tag, ref="whynot")
  contrasts(d$tag)
  
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
  # 1.1 condition as fixed effect, full ranef structure
  
  # comparing conditions to pos baseline, all data
  bm <- brm(formula = bf(betaresponse ~ condition + (1 + condition | participant) + (1 + condition | item),
                             phi ~ condition + (1 + condition | participant) + (1 + condition | item),
                             family = Beta()),
                family = Beta(),
                data = d,
                iter=10000, warmup=2000,chains=6,
                control = list(adapt_delta = .95,max_treedepth=15))

  print(bm$family)
  print(bm$formula)
  summary(bm, waic = TRUE)
  prior_summary(bm)
  plot(bm, ask = TRUE)
  pp_check(bm, type = 'dens_overlay_grouped', ndraws = 300, group = "condition")
  
  # look at each tag separately
  wn_bm <- brm(formula = bf(betaresponse ~ condition + (1 + condition  | participant) + (1 + condition | item),
                         phi ~ condition + (1 + condition | participant) + (1 + condition | item),
                         family = Beta()),
            family = Beta(),
            data = d_wn,
            iter=10000, warmup=2000,chains=6,
            control = list(adapt_delta = .95,max_treedepth=15))
  
  neither_bm <- brm(formula = bf(betaresponse ~ condition + (1 + condition | participant) + (1 + condition | item),
                            phi ~ condition + (1 + condition | participant) + (1 + condition | item),
                            family = Beta()),
               family = Beta(),
               data = d_neither,
               iter=10000, warmup=2000,chains=6,
               control = list(adapt_delta = .95,max_treedepth=15))
  
  even_bm <- brm(formula = bf(betaresponse ~ condition + (1 + condition | participant) + (1 + condition | item),
                                 phi ~ condition + (1 + condition | participant) + (1 + condition | item),
                                 family = Beta()),
                    family = Beta(),
                    data = d_even,
                    iter=10000, warmup=2000,chains=6,
                    control = list(adapt_delta = .95,max_treedepth=15))

  # 1.2 try w condition as random effect
  bm_2 <- brm(formula = bf(betaresponse ~ 1 + (1 | condition) + (1 | participant) + (1| item),
                         phi ~ 1 + (1 | condition) + (1 | participant) + (1| item),
                         family = Beta()),
            family = Beta(),
            data = d,
            iter=10000, warmup=2000,chains=6,
            control = list(adapt_delta = .95,max_treedepth=15))
  
  # look at each tag separately
  wn_bm_2 <- brm(formula = bf(betaresponse ~ 1 + (1 | condition) + (1 | participant) + (1| item),
                              phi ~ 1 + (1 | condition) + (1 | participant) + (1| item),
                              family = Beta()),
               family = Beta(),
               data = d_wn,
               iter=10000, warmup=2000,chains=6,
               control = list(adapt_delta = .95,max_treedepth=15))
  
  neither_bm_2 <- brm(formula = bf(betaresponse ~ 1 + (1 | condition) + (1 | participant) + (1| item),
                                   phi ~ 1 + (1 | condition) + (1 | participant) + (1| item),
                                   family = Beta()),
                    family = Beta(),
                    data = d_neither,
                    iter=10000, warmup=2000,chains=6,
                    control = list(adapt_delta = .95,max_treedepth=15))
  
  even_bm_2 <- brm(formula = bf(betaresponse ~ 1 + (1 | condition) + (1 | participant) + (1| item),
                                phi ~ 1 + (1 | condition) + (1 | participant) + (1| item),
                                family = Beta()),
                 family = Beta(),
                 data = d_even,
                 iter=10000, warmup=2000,chains=6,
                 control = list(adapt_delta = .95,max_treedepth=15))
  
# 2 - testing for differences between conditions (exploratory) ---- 

  ## sliding contrast w lowest as reference level
  
  d <- d %>% mutate(condition = fct_reorder(condition, response, .fun = mean))
  contrasts(d$condition)
  ref_slide <- hypr(~think, notview~think, rumor~notview, asif~rumor, notopinion~asif, 
                    wrong~notopinion, like~wrong, notimpression~like, notthink~notimpression, 
                    notfeeling~notthink, disagree~notfeeling, lie~disagree, thinknot~lie, 
                    doubt~thinknot, notbelieve~doubt,  levels = levels(d$condition))
  
  contrasts(d$condition) <- cmat(ref_slide, remove_intercept = FALSE)
  contrasts(d$condition)
  
  # get new data sets for the tags with new contrast coding
  d_wn <- d %>% filter(tag == "whynot")
  d_neither <- d %>% filter(tag == "neither")
  d_even <- d %>% filter(tag == "noteven")
  
  ##  beta regression
  bm_3 <- brm(formula = bf(betaresponse ~ condition + (1 | participant) + (1 + condition | item),
                         phi ~ condition + (1 | participant) + (1 + condition | item),
                         family = Beta()),
            family = Beta(),
            data = d,
            iter=10000, warmup=2000,chains=6,
            control = list(adapt_delta = .95,max_treedepth=15))
  
  print(bm$family)
  print(bm$formula)
  summary(bm, waic = TRUE)
  prior_summary(bm)
  plot(bm, ask = TRUE)
  pp_check(bm, type = 'dens_overlay_grouped', ndraws = 300, group = "condition")
  
  bm_3_wn <- brm(formula = bf(betaresponse ~ condition + (1 | participant) + (1 + condition | item),
                           phi ~ condition + (1 | participant) + (1 + condition | item),
                           family = Beta()),
              family = Beta(),
              data = d_wn,
              iter=10000, warmup=2000,chains=6,
              control = list(adapt_delta = .95,max_treedepth=15))
  
  bm_3_neither <- brm(formula = bf(betaresponse ~ condition + (1 | participant) + (1 + condition | item),
                              phi ~ condition + (1 | participant) + (1 + condition | item),
                              family = Beta()),
                 family = Beta(),
                 data = d_neither,
                 iter=10000, warmup=2000,chains=6,
                 control = list(adapt_delta = .95,max_treedepth=15))
  
  bm_3_even <- brm(formula = bf(betaresponse ~ condition + (1 | participant) + (1 + condition | item),
                              phi ~ condition + (1 | participant) + (1 + condition | item),
                              family = Beta()),
                 family = Beta(),
                 data = d_even,
                 iter=10000, warmup=2000,chains=6,
                 control = list(adapt_delta = .95,max_treedepth=15))
  
# 3 - testing for interactions of tag / condition (exploratory) ----
  
  # go back to treatment contrast
  d <- d %>% mutate(condition = fct_reorder(condition, response, .fun = mean))
  trt_contrasts <- contr.treatment(15)
  colnames(trt_contrasts) <- levels(d$condition)[-1]
  contrasts(d$condition) <- trt_contrasts
  contrasts(d$condition)
  
  d <- d %>% mutate(tag = fct_reorder(tag, response, .fun = mean))
  contrasts(d$tag)
  
  ## beta regression
  bm_4 <- brm(formula = bf(betaresponse ~ condition * tag + (1 + condition * tag | participant) 
                           + (1 * condition * tag + condition | item),
                           phi ~ condition * tag + (1 + condition * tag | participant) 
                           + (1 * condition * tag + condition | item),
                           family = Beta()),
              family = Beta(),
              data = d,
              iter=10000, warmup=2000,chains=6,
              control = list(adapt_delta = .95,max_treedepth=15))
  
# 4 -- testing for differences between tags ----
  bm_5 <- brm(formula = bf(betaresponse ~ tag + (1 + tag | participant) + (1 + tag | item),
                           phi ~ tag + (1 + tag | participant) + (1 + tag | item),
                           family = Beta()),
              family = Beta(),
              data = d,
              iter=10000, warmup=2000,chains=6,
              control = list(adapt_delta = .95,max_treedepth=15))
  
  print(bm_5$family)
  print(bm_5$formula)
  summary(bm_5, waic = TRUE)
  prior_summary(bm_5)
  plot(bm_5, ask = TRUE)
  pp_check(bm_5, type = 'dens_overlay_grouped', ndraws = 300, group = "condition")
  
  