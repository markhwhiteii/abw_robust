# prep -------------------------------------------------------------------------
library(betareg)
library(quantreg)
library(emmeans)
library(tidyverse)

dat <- read_csv("../Data/results4.csv")

# eda --------------------------------------------------------------------------
# 12 failures of rsq
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

emmeans(m1, ~ d, at = list(d = c(1, 2, 5, 10, 20)))

# full rank --------------------------------------------------------------------
m2 <- glm(rank_all ~ d, binomial, dat)
summary(m2)

emmeans(m2, ~ d, at = list(d = c(1, 2, 5, 10, 20)), type = "response")

# average rank -----------------------------------------------------------------
m3a <- lm(rank_avg ~ d, data = dat)
summary(m3a)

emmeans(m3a, ~ d, at = list(d = c(1, 2, 5, 10, 20)))

m3b <- rq(rank_avg ~ d, data = dat)
summary(m3b)

predict(m3b, data.frame(d = c(1, 2, 5, 10, 20)))

# bot 3 ------------------------------------------------------------------------
m4 <- glm(rank_top3 ~ d, binomial, dat)
summary(m4)

emmeans(m4, ~ d, at = list(d = c(1, 2, 5, 10, 20)), type = "response")

# top 3 ------------------------------------------------------------------------
m5 <- glm(rank_bot3 ~ d, binomial, dat)
summary(m5)

emmeans(m5, ~ d, at = list(d = c(1, 2, 5, 10, 20)), type = "response")
