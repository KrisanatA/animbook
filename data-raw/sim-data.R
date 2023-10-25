# Simulated data with change between groups

# Category

n_org <- 200
n_in_q <- 40
n_q <- 5 # n_q*n_in_q=n_org
n_t1 <- n_in_q/2
cat_change <- tibble::tibble(
  id = factor(c(1:n_org, 1:n_org)),
  time = as.integer(rep(c(2020, 2023), rep(n_org, times = 2))),
  gp = factor(rep(rep(c("X", "Y"), n_q, each = n_t1), 2)),
  qnt = factor(c(rep(LETTERS[1:n_q], rep(n_in_q, n_q)),
                 rep(LETTERS[1:n_q], c(30, 0, 0, 0, 10)),
                 rep(LETTERS[1:n_q], c(0, 40, 0, 0, 0)),
                 rep(LETTERS[1:n_q], c(0, 0, 40, 0, 0)),
                 rep(LETTERS[1:n_q], c(0, 0, 0, 40, 0)),
                 rep(LETTERS[1:n_q], c(10, 0, 0, 0, 30))))
)

usethis::use_data(cat_change, overwrite = TRUE)


# Numerical

n_org <- 200
n_in_q <- 40
n_q <- 5
n_t1 <- n_in_q/2

dbl_change <- tibble::tibble(
  id = factor(c(1:n_org, 1:n_org)),
  time = as.integer(rep(c(2020, 2023), rep(n_org, times = 2))),
  gp = factor(rep(rep(c("X", "Y"), n_q, each = n_t1), 2)),
  values = as.double(runif(400, 1, 10000))
)

usethis::use_data(dbl_change, overwrite = TRUE)
