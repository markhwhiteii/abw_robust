# prep -------------------------------------------------------------------------
library(betareg)
library(emmeans)
library(tidyverse)

dat <- read_csv("../Data/results3.csv") %>% 
  mutate(
    reps = (4 * b) / items,
    lambda = reps * (4 - 1) / (items - 1),
    reps_int = abs(round(reps) - reps),
    lambda_int = abs(round(lambda) - lambda)
  ) %>% 
  select(-pairs_avg, -pairs_var, -items_var)

# efa --------------------------------------------------------------------------
# 12 failures of rsq
table(is.na(dat$rsq))
dat[is.na(dat$rsq), ]
dat <- dat[!is.na(dat$rsq), ]

hist(dat$rsq)
mean(dat$rank_all)
mean(dat$rank_avg)
mean(dat$rank_avg == 1L)
mean(dat$rank_avg == 0L)

mean(dat$rank_top3)
mean(dat$rank_bot3)
table(dat$is_bibd)
range(dat$rsq)

# every time a bibd was available, did it find one?
dat %>% 
  select(b, items) %>% 
  unique() %>% 
  nrow()

dat %>% 
  select(b, items, is_bibd) %>% 
  unique() %>% 
  nrow()

# no, there were two instances where one was possible but we didn't converge
# what were these?
bad_designs <- dat %>% 
  count(b, items, is_bibd) %>% 
  group_by(b, items) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  filter(!is_bibd) %>% 
  select(-n)

# lets drop these
dat <- dat %>% 
  anti_join(bad_designs)

# check again
dat %>% 
  select(b, items) %>% 
  unique() %>% 
  nrow()

dat %>% 
  select(b, items, is_bibd) %>% 
  unique() %>% 
  nrow()

# cor mat
round(cor(dat), 2)

# bibd vs non bibd -------------------------------------------------------------
m0 <- betareg(rsq ~ is_bibd | is_bibd, dat)
summary(m0)

emmeans(m0, ~ is_bibd)

ggplot(dat, aes(x = rsq, fill = is_bibd)) +
  geom_density(alpha = .7) +
  labs(x = expression(R^2~Between~ABW~Estimate~and~Latent~Score), 
       y = "Density") +
  scale_fill_discrete(name = "Is BIBD?", labels = c("False", "True")) +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 18))

glm(rank_all ~ is_bibd, binomial, dat) %>% 
  emmeans(~ is_bibd, type = "response")

ggplot(dat, aes(x = rank_avg, fill = is_bibd)) +
  geom_density(alpha = .7) +
  labs(x = "Proportion of Ranks Preserved", 
       y = "Density") +
  scale_fill_discrete(name = "Is BIBD?", labels = c("False", "True")) +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 18))

betareg(rank_avg ~ is_bibd | is_bibd, dat[!dat$rank_avg %in% 0:1, ]) %>% 
  emmeans(~ is_bibd, type = "response")

glm(rank_top3 ~ is_bibd, binomial, dat) %>% 
  emmeans(~ is_bibd, type = "response")

glm(rank_bot3 ~ is_bibd, binomial, dat) %>% 
  emmeans(~ is_bibd, type = "response")

# rsq --------------------------------------------------------------------------
m1 <- dat %>% 
  mutate(b = factor(b), items = factor(items)) %>% 
  betareg(rsq ~ b * items | b * items, .)

key <- dat %>% 
  transmute(b = factor(b), items = factor(items), is_bibd) %>% 
  unique()

emmeans(m1, ~ b + items) %>% 
  as_tibble() %>% 
  left_join(key) %>% 
  ggplot(aes(x = b, y = emmean, group = items)) +
  geom_line() +
  labs(y = expression(R^2~Between~ABW~Estimate~and~Latent~Score), 
       x = "# of Blocks",
       caption = "Each panel follows a different y-axis range for clarity") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 16)) +
  geom_point(aes(shape = is_bibd), size = 2) +
  scale_shape_manual(
    name = "Is BIBD?", 
    labels = c("False", "True"),
    values = c(1, 19)
  ) +
  facet_wrap(
    ~ items, 
    scales = "free",
    labeller = as_labeller(function(x) paste0("t = ", x))
  )

# rank all ---------------------------------------------------------------------
dat %>% 
  filter(items < 19) %>% 
  mutate(b = factor(b), items = factor(items), is_bibd) %>% 
  group_by(b, items, is_bibd) %>% 
  summarise(retain_rank = mean(rank_all), .groups = "drop") %>% 
  ggplot(aes(x = b, y = retain_rank, group = items)) +
  geom_line() +
  labs(y = "Proportion of Simulations With\nFull Rank Order Retained", 
       x = "# of Blocks",
       caption = "Each panel follows a different y-axis range for clarity\nPanels for 19 and 20 not shown because they are constant at zero") +
  scale_color_discrete(name = "# of Items in Total Set") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 16)) +
  facet_wrap(
    ~ items, 
    scales = "free",
    labeller = as_labeller(function(x) paste0("t = ", x))
  ) +
  geom_point(aes(shape = is_bibd), size = 3) +
  scale_shape_manual(
    name = "Is BIBD?", 
    labels = c("False", "True"),
    values = c(1, 19)
  )

# rank avg ---------------------------------------------------------------------
dat %>% 
  mutate(b = factor(b), items = factor(items), is_bibd) %>% 
  group_by(b, items, is_bibd, lambda, reps) %>% 
  summarise(retain_rank = mean(rank_avg), .groups = "drop") %>% 
  # filter(items != "20") %>% 
  ggplot(aes(x = b, y = retain_rank, group = items)) +
  geom_line() +
  labs(
    y = "Median Proportion of Items with Rank Order Retained",
    x = "# of Blocks",
    caption = "Each panel follows a different y-axis range for clarity"
  ) +
  scale_color_discrete(name = "# of Items in Total Set") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 16)) +
  facet_wrap(
    ~ items, 
    scales = "free",
    labeller = as_labeller(function(x) paste0("t = ", x))
  ) +
  geom_point(aes(shape = is_bibd), size = 2) +
  scale_shape_manual(
    name = "Is BIBD?",
    labels = c("False", "True"),
    values = c(1, 19)
  )# +
  # scale_y_continuous(expand = expansion(.5)) +
  # geom_text(
  #   aes(label = paste0("lambda==", round(lambda, 1))),
  #   parse = TRUE,
  #   vjust = 2
  # ) +
  # geom_text(
  #   aes(label = paste0("italic(r)==", round(reps, 1))),
  #   parse = TRUE,
  #   vjust = 3.5
  # )

# top 3 bot 3 ------------------------------------------------------------------
with(dat, table(rank_top3, rank_bot3))
sum(diag(with(dat, prop.table(table(rank_top3, rank_bot3)))))
# they only agree 59 percent of the time

dat %>% 
  mutate(b = factor(b), items = factor(items), is_bibd) %>% 
  group_by(b, items, is_bibd) %>% 
  summarise(retain_rank = mean(rank_top3), .groups = "drop") %>% 
  ggplot(aes(x = b, y = retain_rank, group = items)) +
  geom_line() +
  labs(y = "Proportion of Simulations With\nBest 3 Rank Order Retained", 
       x = "# of Blocks",
       caption = "Each panel follows a different y-axis range for clarity") +
  scale_color_discrete(name = "# of Items in Total Set") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 16)) +
  facet_wrap(
    ~ items, 
    scales = "free",
    labeller = as_labeller(function(x) paste0("t = ", x))
  ) +
  geom_point(aes(shape = is_bibd), size = 2) +
  scale_shape_manual(
    name = "Is BIBD?", 
    labels = c("False", "True"),
    values = c(1, 19)
  )

dat %>% 
  mutate(b = factor(b), items = factor(items), is_bibd) %>% 
  group_by(b, items, is_bibd) %>% 
  summarise(retain_rank = mean(rank_bot3), .groups = "drop") %>% 
  ggplot(aes(x = b, y = retain_rank, group = items)) +
  geom_line() +
  labs(y = "Proportion of Simulations With\nWorst 3 Rank Order Retained", 
       x = "# of Blocks",
       caption = "Each panel follows a different y-axis range for clarity") +
  scale_color_discrete(name = "# of Items in Total Set") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 16)) +
  facet_wrap(
    ~ items, 
    scales = "free",
    labeller = as_labeller(function(x) paste0("t = ", x))
  ) +
  geom_point(aes(shape = is_bibd), size = 2) +
  scale_shape_manual(
    name = "Is BIBD?", 
    labels = c("False", "True"),
    values = c(1, 19)
  )

# lamba and reps ---------------------------------------------------------------
# rsq
m2 <- betareg(rsq ~ b + items + lambda_int + reps_int | 
                b + items + lambda_int + reps_int, dat)
summary(m2)

emmeans(m2, ~ lambda_int, at = list(lambda_int = seq(.0, .5, .25)))

# full rank
m3 <- glm(rank_all ~ b + items + lambda_int + reps_int, binomial, dat)
summary(m3)

emmeans(
  m3,
  ~ lambda_int,
  at = list(lambda_int = seq(.0, .5, .25)),
  type = "response"
)

# avg rank
table(dat$rank_avg == 0)
table(dat$rank_avg == 1)

dat_ <- dat %>% 
  filter(!rank_avg %in% 0:1)

range(dat_$rank_avg)

m4 <- betareg(rank_avg ~ b + items + lambda_int + reps_int | 
                b + items + lambda_int + reps_int, dat_)
summary(m4)

emmeans(m4, ~ lambda_int, at = list(lambda_int = seq(.0, .5, .25)))

# top 3
m5 <- glm(rank_top3 ~ b + items + lambda_int + reps_int, binomial, dat)
summary(m5)

emmeans(
  m5,
  ~ lambda_int,
  at = list(lambda_int = seq(.0, .5, .25)),
  type = "response"
)

# bot 3
m6 <- glm(rank_top3 ~ b + items + lambda_int + reps_int, binomial, dat)
summary(m6)

emmeans(
  m6,
  ~ lambda_int,
  at = list(lambda_int = seq(.0, .5, .25)),
  type = "response"
)
