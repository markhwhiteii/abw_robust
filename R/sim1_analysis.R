# prep -------------------------------------------------------------------------
library(betareg)
library(emmeans)
library(tidyverse)

dat <- read_csv("../Data/results1.csv")

# efa --------------------------------------------------------------------------
# 11 failures of rsq
table(is.na(dat$rsq))
dat[is.na(dat$rsq), ]
dat <- dat[!is.na(dat$rsq), ]

hist(dat$rsq)
mean(dat$rank)
table(dat$rank)

range(dat$rsq)

# treat b and k as factors -----------------------------------------------------
m1 <- dat %>% 
  mutate(b = factor(b), k = factor(k)) %>% 
  betareg(rsq ~ b * k | b * k, .)

emmeans(m1, ~ b + k) %>% 
  as_tibble() %>%
  ggplot(aes(x = b, y = emmean, group = k, color = k)) +
  geom_point(size = 3) +
  geom_line() +
  labs(y = expression(R^2~Between~ABW~Estimate~and~Latent~Score), 
       x = "# of Blocks") +
  scale_color_discrete(name = "# of Items per Block") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 18)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0)

dat %>% 
  mutate(b = factor(b), k = factor(k)) %>% 
  group_by(b, k) %>% 
  summarise(retain_rank = mean(rank), .groups = "drop") %>% 
  ggplot(aes(x = b, y = retain_rank, group = k, color = k)) +
  geom_point(size = 3) +
  geom_line() +
  labs(y = "Proportion of Simulations With\nFull Rank Order Retained", 
       x = "# of Blocks") +
  scale_color_discrete(name = "# of Items per Block") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 18))

# correlate characteristics ----------------------------------------------------
# among non-bibds
dat_nonbibd <- dat %>% 
  filter(!(b == 13 & k == 4))

dat_nonbibd %>% 
  select(rsq, rank, pairs_avg, pairs_var, items_var) %>% 
  cor() %>% 
  round(2)

m2 <- betareg(
  rsq ~ pairs_avg * pairs_var + items_var | 
    pairs_avg * pairs_var + items_var,
  dat_nonbibd
)
summary(m2)

m2_preds <- emmeans(
  m2, 
  ~ pairs_avg + pairs_var,
  at = list(
    pairs_avg = seq(
      min(dat_nonbibd$pairs_avg), 
      max(dat_nonbibd$pairs_avg), 
      length.out = 6
    ),
    pairs_var = seq(
      min(dat_nonbibd$pairs_var), 
      max(dat_nonbibd$pairs_var), 
      length.out = 6
    )
  )
) %>% 
  as_tibble()

ggplot(
  m2_preds, 
  aes(
    x = pairs_avg, 
    y = emmean, 
    group = factor(round(pairs_var, 2)),
    color = factor(round(pairs_var, 2))
  )
) +
  geom_point() +
  geom_line()

ggplot(
  m2_preds, 
  aes(
    x = pairs_var, 
    y = emmean, 
    group = factor(round(pairs_avg, 2)),
    color = factor(round(pairs_avg, 2))
  )
) +
  geom_point() +
  geom_line()

# I don't think this is helpful because a lot of these combinations represent
#     situations that are impossible given how the designs are structured
