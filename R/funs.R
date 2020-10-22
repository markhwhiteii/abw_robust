# not best practices necessarily but loading libraries
library(bwsTools)
library(crossdes)
library(tidyverse)

# given a named vector of true latent values, get best and worst choice
# where max latent is best and min latent is worst
# eps_sd is noise standard deviation applied to latent scale
get_choices <- function(x, eps_sd = 1) {
  eps <- rnorm(length(x), 0, eps_sd)
  x_eps <- x + eps
  out <- c(
    b = names(x_eps)[which.max(x_eps)], 
    w = names(x_eps)[which.min(x_eps)]
  )
  return(out)
}

# given a design and true latent values, get counts for one person
get_oneperson <- function(d, x, eps_sd) {
  res <- apply(d, 1, function(i) get_choices(x[i], eps_sd))
  items <- sort(names(x))
  out <- as_tibble(
    t(sapply(items, function(z) rowSums(res == z))), rownames = "item"
  )
  out$t <- c(table(unlist(d))[items])
  return(out)
}

# run it for an entire sample, return abw coefs
sim_abw <- function(N, d, x, eps_sd) {
  dat <- map_dfr(seq_len(N), ~get_oneperson(d, x, eps_sd)) %>% 
    group_by(item) %>% 
    summarise(t = sum(t), b = sum(b), w = sum(w), .groups = "drop")
  out <- ae_mnl(dat, "t", "b", "w")$b
  names(out) <- dat$item
  return(out)
}

# function for making designs
make_design <- function(...) {
  out <- find.BIB(...) %>% 
    as.data.frame() %>% 
    as_tibble(rownames = "Block") %>% 
    mutate_if(is.double, ~letters[.]) %>% 
    select(-Block)
  return(out)
}

# look at pairs: do they all occur? if they do, whats the variance?
get_pairwise0 <- function(x) apply(combn(x, 2), 2, paste, collapse = "")

get_pairwise <- function(d) {
  tab <- table(c(apply(d, 1, get_pairwise0)))
  allpairs <- apply(combn(sort(unique(unlist(d))), 2), 2, paste, collapse = "")
  if (!all(names(tab) %in% allpairs)) stop()
  out <- c(pairs_avg = mean(allpairs %in% names(tab)), pairs_var = var(tab))
  return(out)
}

# get variance in how many times each item appears
get_itemvar <- function(d) {
  out <- var(table(unlist(d)))
  return(out)
}
