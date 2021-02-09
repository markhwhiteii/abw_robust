# prep -------------------------------------------------------------------------
library(betareg)
library(quantreg)
library(emmeans)
library(tidyverse)

dat <- read_csv("../Data/results4.csv")

# eda --------------------------------------------------------------------------
# 8 failures of rsq
table(is.na(dat$rsq))
dat[is.na(dat$rsq), ]
dat <- dat[!is.na(dat$rsq), ]

hist(dat$rsq)
hist(dat$rank_avg)
table(dat$rank_avg)
table(dat$d)

# rsq --------------------------------------------------------------------------
m1 <- betareg(rsq ~ d | d, dat)
summary(m1)

plot_dat <- emmeans(m1, ~ d, at = list(d = 1:20)) %>% 
  as_tibble() %>% 
  mutate(outcome = "italic(R)^2")

# full rank --------------------------------------------------------------------
m2 <- glm(rank_all ~ d, binomial, dat)
summary(m2)

plot_dat <- plot_dat %>% 
  bind_rows({
    emmeans(m2, ~ d, at = list(d = 1:20), type = "response") %>% 
      as_tibble() %>% 
      mutate(outcome = "Full~Rank") %>% 
      rename("emmean" = "prob")
  })

# average rank -----------------------------------------------------------------
m3a <- lm(rank_avg ~ d, data = dat)
summary(m3a)

emmeans(m3a, ~ d, at = list(d = 1:20))

m3b <- rq(rank_avg ~ d, data = dat)
summary(m3b)

plot_dat <- plot_dat %>% 
  bind_rows({
    predict(m3b, data.frame(d = 1:20)) %>% 
      as_tibble() %>% 
      transmute(d = 1:20, emmean = value, outcome = "Average~Rank")
  })

# top 3 ------------------------------------------------------------------------
m5 <- glm(rank_top3 ~ d, binomial, dat)
summary(m5)

plot_dat <- plot_dat %>% 
  bind_rows({
    emmeans(m5, ~ d, at = list(d = 1:20), type = "response") %>% 
      as_tibble() %>% 
      mutate(outcome = "Top~3~Rank") %>% 
      rename("emmean" = "prob")
  }) %>% 
  mutate(outcome = factor(outcome, unique(outcome)))

# plot -------------------------------------------------------------------------
ggplot(plot_dat, aes(x = d, y = emmean)) +
  geom_line() +
  facet_wrap(~ outcome, labeller = label_parsed)
