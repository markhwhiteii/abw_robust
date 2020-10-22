source("funs.R")

# initialize output
iter <- 25000
res <- tibble(
  iter = rep(NA, iter),
  rsq = NA,
  rank = NA,
  noise = NA,
  N = NA,
  b = NA,
  items = NA
)

# do the damn thing
set.seed(1839)
for (i in seq_len(iter)) {
  items <- sample(8:20, 1)        # draw number of items
  phi <- rnorm(items)             # make true latent score
  names(phi) <- letters[1:items]  # give them names
  noise <- runif(1, 0, 1.5)       # simulate how much noise
  N <- round(runif(1, 200, 5000)) # draw the sample size
  b <- sample(8:20, 1)            # draw number of blocks in design
  k <- 4                          # constant number of items per block
  Dx <- make_design(items, b, k)  # generate a design from these parameters
  r <- sim_abw(N, Dx, phi, noise) # generate result from these parameters
  r <- r[sort(names(phi))]        # make sure r is in same order as phi
  phi <- phi[sort(names(phi))]    # make sure r is in same order as phi
  
  # write output
  res$iter[i] = i
  res$rsq[i] = cor(r, phi) ^ 2
  res$rank[i] = all(order(r) == order(phi))
  res$noise[i] = noise
  res$N[i] = N
  res$b[i] = b
  res$items[i] = items
  
  # progress
  if ((i %% 100) == 0L) cat(i, "\n")
}

write_csv(res, "../Data/results2.csv")
