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
  d = NA
)

# do the damn thing
set.seed(1839)
for (i in seq_len(iter)) {
  # draw parameters:
  items <- 10                     # constant number of items
  phi <- rnorm(items)             # make true latent score
  names(phi) <- letters[1:items]  # give them names
  noise <- runif(1, 0.5, 1.5)     # simulate how much noise
  N <- round(runif(1, 300, 2000)) # simulate the sample size
  b <- 8                          # constant number of blocks in design
  k <- 4                          # constant number of items per block
  d <- sample(1:20, 1)            # sample number of designs to make
  Dxs <- map(seq_len(d), ~make_design(items, b, k)) # make d designs
  
  # scoring how well it performed:
  r <- ldx_sim_abw(N, Dxs, phi, noise) # generate result from these parameters
  r <- r[sort(names(phi))]             # make sure r is in same order as phi
  phi <- phi[sort(names(phi))]         # make sure r is in same order as phi
  ord <- order(r) == order(phi)        # see if orders line up
  top3 <- all(names(sort(phi)[1:3]) == names(sort(r)[1:3]))             # top 3
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
  res$d[i] <- d
  
  # progress
  if ((i %% 100) == 0L) cat(i, "\n")
}

write_csv(res, "../Data/results3.csv")
