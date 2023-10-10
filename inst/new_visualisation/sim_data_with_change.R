# Simulated data with change between groups

n_org <- 200
n_in_q <- 40
n_q <- 5 # n_q*n_in_q=n_org
n_t1 <- n_in_q/2
d_w_change <- tibble(
  id = factor(c(1:n_org, 1:n_org)),
  time = as.integer(rep(c(2020, 2023), rep(n_org, times=2))),
  gp = factor(rep(rep(c("X", "Y"), n_q, each=n_t1), 2)),
  qnt = factor(c(rep(LETTERS[1:n_q], rep(n_in_q, n_q)),
         rep(LETTERS[1:n_q], c(30, 0, 0, 0, 10)),
         rep(LETTERS[1:n_q], c(0, 40, 0, 0, 0)),
         rep(LETTERS[1:n_q], c(0, 0, 40, 0, 0)),
         rep(LETTERS[1:n_q], c(0, 0, 0, 40, 0)),
         rep(LETTERS[1:n_q], c(10, 0, 0, 0, 30))))
)
d_w_change_p <- d_w_change %>%
  pivot_wider(names_from = time, values_from=qnt)

library(animbook)
d <- anim_prep_cat(d_w_change, id=id,
                   values=qnt,
                   time=time,
                   color=gp,
                   time_dependent = FALSE)
d_p <- wallaby_plot(d, height=1)
d_p_anim <- anim_animate(d_p)
gganimate::animate(d_p_anim)

d_p <- wallaby_plot(d, height=1, subset = "E")
d_p_anim <- anim_animate(d_p)
gganimate::animate(d_p_anim)

d_p <- kangaroo_plot(d)
d_p_anim <- anim_animate(d_p)
gganimate::animate(d_p_anim)

