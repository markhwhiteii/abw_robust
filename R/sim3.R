source("funs.R")

# initialize output
iter <- 10000
res <- tibble(
  iter = rep(NA, iter),
  rsq = NA,
  rank_all = NA,
  rank_avg = NA,
  rank_top3 = NA,
  rank_bot3 = NA,
  noise = NA,
  N = NA,
  b = NA,
  items = NA,
  is_bibd = NA,
  pairs_avg = NA,
  pairs_var = NA,
  items_var = NA
)

# do the damn thing
set.seed(1839)
for (i in seq_len(iter)) {
  items <- sample(8:20, 1)        # draw number of items
  phi <- rnorm(items)             # make true latent score
  names(phi) <- letters[1:items]  # give them names
  noise <- runif(1, 0.5, 1.5)     # simulate how much noise
  N <- round(runif(1, 300, 2000)) # simulate the sample size
  b <- sample(8:20, 1)            # draw number of blocks in design
  k <- 4                          # constant number of items per block
  Dx <- make_design(items, b, k)  # generate a design from these parameters
  is_bibd <- Dx %>%               # see if its bibd
    mutate_all(                   #   for details, see the while loop
      ~sapply(., function(i) {    #   inside the find.BIB function
        which(i == letters)
      })) %>% 
    as.matrix() %>%
    isGYD(FALSE, FALSE) %>% 
    getElement(1) %>% 
    `[`(1:4) %>% 
    all()
  Dx_pairs <- get_pairwise(Dx)    # get pairwise info on design
  Dx_items <- get_itemvar(Dx)     # get variance of number of times appeared
  r <- sim_abw(N, Dx, phi, noise) # generate result from these parameters
  r <- r[sort(names(phi))]        # make sure r is in same order as phi
  phi <- phi[sort(names(phi))]    # make sure r is in same order as phi
  ord <- order(r) == order(phi)   # see if orders line up
  top3 <- all(names(sort(phi)[1:3]) == names(sort(r)[1:3])) # top 3 the same
  bot3 <- all(names(sort(phi, TRUE)[1:3]) == names(sort(r, TRUE)[1:3])) # min 3

  # write output
  res$iter[i] <- i
  res$rsq[i] <- cor(r, phi) ^ 2
  res$rank_all[i] <- all(ord)
  res$rank_avg[i] <- mean(ord)
  res$rank_top3[i] <- top3
  res$rank_bot3[i] <- bot3
  res$noise[i] <- noise
  res$N[i] <- N
  res$b[i] <- b
  res$items[i] <- items
  res$is_bibd[i] <- is_bibd
  res$pairs_avg[i] <- Dx_pairs[[1]]
  res$pairs_var[i] <- Dx_pairs[[2]]
  res$items_var[i] <- Dx_items
  
  # progress
  if ((i %% 100) == 0L) cat(i, "\n")
}

write_csv(res, "../Data/results3.csv")
