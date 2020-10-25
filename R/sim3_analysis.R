# prep -------------------------------------------------------------------------
library(betareg)
library(emmeans)
library(tidyverse)

dat <- read_csv("../Data/results3.csv") %>% 
  mutate(
    reps = (4 * b) / items,
    lambda = reps * (4 - 1) / (items - 1)
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

chisq.test(xtabs(~ rank_all + is_bibd, dat))
prop.table(xtabs(~ rank_all + is_bibd, dat), 2)

ggplot(dat, aes(x = rank_avg, fill = is_bibd)) +
  geom_density(alpha = .7) +
  labs(x = "Proportion of Ranks Preserved", 
       y = "Density") +
  scale_fill_discrete(name = "Is BIBD?", labels = c("False", "True")) +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 18))

chisq.test(xtabs(~ rank_top3 + is_bibd, dat))
prop.table(xtabs(~ rank_top3 + is_bibd, dat), 2)

chisq.test(xtabs(~ rank_bot3 + is_bibd, dat))
prop.table(xtabs(~ rank_bot3 + is_bibd, dat), 2)

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
  ggplot(aes(x = b, y = emmean, group = items, color = items)) +
  geom_line() +
  labs(y = expression(R^2~Between~ABW~Estimate~and~Latent~Score), 
       x = "# of Blocks") +
  scale_color_discrete(name = "# of Items in Total Set") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 18)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0) +
  geom_point(aes(shape = is_bibd), size = 3) +
  scale_shape_manual(
    name = "Is BIBD?", 
    labels = c("False", "True"),
    values = c(1, 19)
  )

emmeans(m1, ~ b + items) %>% 
  as_tibble() %>% 
  left_join(key) %>% 
  ggplot(aes(x = b, y = emmean, group = items)) +
  geom_line() +
  labs(y = expression(R^2~Between~ABW~Estimate~and~Latent~Score), 
       x = "# of Blocks",
       caption = "Each panel represents number of total items in set\nand follows a different y-axis range for clarity") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 18)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0) +
  geom_point(aes(shape = is_bibd), size = 3) +
  scale_shape_manual(
    name = "Is BIBD?", 
    labels = c("False", "True"),
    values = c(1, 19)
  ) +
  facet_wrap(~ items, scales = "free")

# guidance for when you can't get all pairs
dat %>% 
  filter(pairs_avg < 1 & !is_bibd) %>% 
  betareg(rsq ~ pairs_avg + items_var | pairs_avg + items_var, .) %>% 
  summary()

dat %>% 
  filter(pairs_avg < 1 & !is_bibd) %>% 
  ggplot(aes(x = pairs_avg, y = rsq)) +
  geom_point(shape = 1, alpha = .2) +
  geom_smooth(se = FALSE, method = "betareg", color = "black") +
  theme_minimal()

dat %>% 
  filter(pairs_avg < 1 & !is_bibd) %>% 
  ggplot(aes(x = items_var, y = rsq)) +
  geom_point(shape = 1, alpha = .2) +
  geom_smooth(se = FALSE, method = "betareg", color = "black") +
  theme_minimal()

# what about when you can put all pairs in
dat %>% 
  filter(pairs_avg == 1) %>% 
  betareg(rsq ~ pairs_var | pairs_var, .) %>% 
  summary()

dat %>% 
  filter(pairs_avg == 1) %>% 
  ggplot(aes(x = pairs_var, y = rsq)) +
  geom_point(shape = 1, alpha = .2) +
  geom_smooth(se = FALSE, method = "betareg", color = "black") +
  theme_minimal()

dat %>% 
  filter(pairs_avg == 1 & !is_bibd) %>% 
  betareg(rsq ~ pairs_var | pairs_var, .) %>% 
  summary()

dat %>% 
  filter(pairs_avg == 1 & !is_bibd) %>% 
  ggplot(aes(x = pairs_var, y = rsq)) +
  geom_point(shape = 1, alpha = .2) +
  geom_smooth(se = FALSE, method = "betareg", color = "black") +
  theme_minimal()

# rank all ---------------------------------------------------------------------
dat %>% 
  mutate(b = factor(b), items = factor(items), is_bibd) %>% 
  group_by(b, items, is_bibd) %>% 
  summarise(retain_rank = mean(rank_all), .groups = "drop") %>% 
  ggplot(aes(x = b, y = retain_rank, group = items, color = items)) +
  geom_line() +
  labs(y = "Proportion of Simulations With\nFull Rank Order Retained", 
       x = "# of Blocks") +
  scale_color_discrete(name = "# of Items in Total Set") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 18)) +
  geom_point(aes(shape = is_bibd), size = 3) +
  scale_shape_manual(
    name = "Is BIBD?", 
    labels = c("False", "True"),
    values = c(1, 19)
  )

dat %>% 
  filter(items < 19) %>% 
  mutate(b = factor(b), items = factor(items), is_bibd) %>% 
  group_by(b, items, is_bibd) %>% 
  summarise(retain_rank = mean(rank_all), .groups = "drop") %>% 
  ggplot(aes(x = b, y = retain_rank, group = items)) +
  geom_line() +
  labs(y = "Proportion of Simulations With\nFull Rank Order Retained", 
       x = "# of Blocks",
       caption = "Each panel represents number of total items in set\nand follows a different y-axis range for clarity\nPanels for 19 and 20 not shown because they are constant at zero") +
  scale_color_discrete(name = "# of Items in Total Set") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 18)) +
  facet_wrap(~ items, scales = "free") +
  geom_point(aes(shape = is_bibd), size = 3) +
  scale_shape_manual(
    name = "Is BIBD?", 
    labels = c("False", "True"),
    values = c(1, 19)
  )

dat %>% 
  filter(items %in% {dat %>% filter(is_bibd) %>% pull(items) %>% unique()}) %>% 
  mutate(b = factor(b), items = factor(items), is_bibd) %>% 
  group_by(b, items, is_bibd) %>% 
  summarise(retain_rank = mean(rank_all), .groups = "drop") %>% 
  ggplot(aes(x = b, y = retain_rank, group = items, color = items)) +
  geom_line() +
  labs(y = "Proportion of Simulations With\nFull Rank Order Retained", 
       x = "# of Blocks",
       caption = "Only displaying total items where a BIBD is possible") +
  scale_color_discrete(name = "# of Items in Total Set") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 18)) +
  geom_point(aes(shape = is_bibd), size = 3) +
  scale_shape_manual(
    name = "Is BIBD?", 
    labels = c("False", "True"),
    values = c(1, 19)
  )

dat %>% 
  filter(!items %in% {dat %>% filter(is_bibd) %>% pull(items) %>% unique()}) %>% 
  mutate(b = factor(b), items = factor(items), is_bibd) %>% 
  group_by(b, items, is_bibd) %>% 
  summarise(retain_rank = mean(rank_all), .groups = "drop") %>% 
  ggplot(aes(x = b, y = retain_rank, group = items, color = items)) +
  geom_line() +
  labs(y = "Proportion of Simulations With\nFull Rank Order Retained", 
       x = "# of Blocks",
       caption = "Only displaying total items where a BIBD is not possible") +
  scale_color_discrete(name = "# of Items in Total Set") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 18)) +
  geom_point(size = 3, shape = 1)

# rank avg ---------------------------------------------------------------------
dat %>% 
  mutate(b = factor(b), items = factor(items), is_bibd) %>% 
  group_by(b, items, is_bibd, lambda, reps) %>% 
  summarise(retain_rank = mean(rank_avg), .groups = "drop") %>% 
  ggplot(aes(x = b, y = retain_rank, group = items)) +
  geom_line() +
  labs(
    y = "Average Proportion of Items with Rank Order Retained", 
    x = "# of Blocks"
  ) +
  scale_color_discrete(name = "# of Items in Total Set") +
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 18)) +
  facet_wrap(~ items, scales = "free") +
  geom_point(aes(shape = is_bibd), size = 3) +
  scale_shape_manual(
    name = "Is BIBD?", 
    labels = c("False", "True"),
    values = c(1, 19)
  ) +
  geom_text(aes(label = round(lambda, 1)), vjust = 2) +
  geom_text(aes(label = round(reps, 1)), vjust = 3)

# misc -------------------------------------------------------------------------
# annotate the b by items points with lambda and reps
ggplot(dat[!dat$is_bibd, ], aes(x = lambda, y = rank_avg)) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(dat[!dat$is_bibd, ], aes(x = reps, y = rank_avg)) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(dat[!dat$is_bibd, ], aes(x = lambda, y = rsq)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "betareg")

ggplot(dat[!dat$is_bibd, ], aes(x = reps, y = rsq)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "betareg")

ggplot(dat[!dat$is_bibd, ], aes(x = abs(round(lambda) - lambda), y = rsq)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "betareg")

ggplot(dat[!dat$is_bibd, ], aes(x = abs(round(reps) - reps), y = rsq)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "betareg")
