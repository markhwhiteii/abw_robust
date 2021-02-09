source("funs.R")

# initialize output
iter <- 10000
res <- tibble(
  iter = rep(NA, iter),
  rsq = NA,
  rank = NA,
  noise = NA,
  N = NA,
  b = NA,
  k = NA,
  pairs_avg = NA,
  pairs_var = NA,
  items_var = NA,
  is_bibd = NA
)

# do the damn thing
set.seed(1839)
for (i in seq_len(iter)) {
  phi <- rnorm(13)                # make true latent score
  names(phi) <- letters[1:13]     # give them names
  noise <- runif(1, 0.5, 1.5)     # simulate how much noise
  N <- round(runif(1, 300, 2000)) # simulate the sample size
  b <- sample(10:16, 1)           # simulate number of blocks in design
  k <- sample(3:5, 1)             # simulate number of items per block
  Dx <- make_design(13, b, k)     # generate a design from these parameters
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
  
  # write output
  res$iter[i] <- i
  res$rsq[i] <- cor(r, phi) ^ 2
  res$rank[i] <- all(order(r) == order(phi))
  res$noise[i] <- noise
  res$N[i] <- N
  res$b[i] <- b
  res$k[i] <- k
  res$pairs_avg[i] <- Dx_pairs[1]
  res$pairs_var[i] <- Dx_pairs[2]
  res$items_var[i] <- Dx_items
  res$is_bibd[i] <- is_bibd
  
  # progress
  if ((i %% 100) == 0L) cat(i, "\n")
}

write_csv(res, "../Data/results1.csv")
