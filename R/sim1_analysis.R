# prep -------------------------------------------------------------------------
library(betareg)
library(emmeans)
library(tidyverse)

dat <- read_csv("../Data/results1.csv") %>% 
  mutate(
    is_bibd = k == 4 & b == 13,
    reps = (k * b) / 13,
    lambda = reps * (k - 1) / (13 - 1),
    reps_int = abs(round(reps) - reps),
    lambda_int = abs(round(lambda) - lambda)
  ) %>% 
  select(-pairs_avg, -pairs_var, -items_var)

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
  left_join({
    dat %>% 
      transmute(b = factor(b), k = factor(k), is_bibd, reps, lambda) %>% 
      unique()
  }) %>% 
  ggplot(aes(x = b, y = emmean, group = k)) +
  geom_point(aes(shape = is_bibd), size = 3) +
  geom_line() +
  labs(y = expression(R^2~Between~ABW~Estimate~and~Latent~Score), 
       x = "# of Blocks") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 18)) +
  facet_wrap(~ k, labeller = as_labeller(function(x) paste0("r = ", x))) +
  scale_shape_manual(
    name = "Is BIBD?",
    labels = c("False", "True"),
    values = c(1, 19)
  ) +
  scale_y_continuous(expand = expansion(c(.3, .1))) +
  geom_text(
    aes(label = paste0("lambda==", round(lambda, 1))),
    parse = TRUE,
    vjust = 3.25
  ) +
  geom_text(
    aes(label = paste0("italic(r)==", round(reps, 1))),
    parse = TRUE,
    vjust = 4.75
  )

dat %>% 
  mutate(b = factor(b), k = factor(k)) %>% 
  group_by(b, k, is_bibd, reps, lambda) %>% 
  summarise(retain_rank = mean(rank), .groups = "drop") %>% 
  ggplot(aes(x = b, y = retain_rank, group = k)) +
  geom_point(aes(shape = is_bibd), size = 3) +
  geom_line() +
  labs(y = "Proportion of Simulations With\nFull Rank Order Retained", 
       x = "# of Blocks") +
  scale_color_discrete(name = "# of Items per Block") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 18)) +
  facet_wrap(~ k, labeller = as_labeller(function(x) paste0("r = ", x))) +
  scale_shape_manual(
    name = "Is BIBD?",
    labels = c("False", "True"),
    values = c(1, 19)
  ) +
  scale_y_continuous(expand = expansion(c(.1, .3))) +
  geom_text(
    aes(label = paste0("lambda==", round(lambda, 1))),
    parse = TRUE,
    vjust = -4.75
  ) +
  geom_text(
    aes(label = paste0("italic(r)==", round(reps, 1))),
    parse = TRUE,
    vjust = -3.25
  )

# lamba and reps ---------------------------------------------------------------
# rsq
m2 <- betareg(rsq ~ b + k + lambda_int + reps_int | 
                b + k + lambda_int + reps_int, dat)
summary(m2)

emmeans(m2, ~ lambda_int, at = list(lambda_int = seq(.0, .5, .25)))

emmeans(m2, ~ reps_int, at = list(reps_int = seq(.0, .5, .25)))

# full rank
m3 <- glm(rank ~ b + k + lambda_int + reps_int, binomial, dat)
summary(m3)

emmeans(
  m3,
  ~ lambda_int,
  at = list(lambda_int = seq(.0, .5, .25)),
  type = "response"
)

emmeans(
  m3,
  ~ reps_int,
  at = list(reps_int = seq(.0, .5, .25)),
  type = "response"
)
