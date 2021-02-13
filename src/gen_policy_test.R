library(tidyverse)
library(reticulate)
use_python('/usr/bin/python3')

# load the Python optimization routine
gen_policy <- import('gen_policy')

# check that two objects are equal up to small numerical differences
approx_equal <- function(a, b) {
  return(max(abs(a-b)) < 1e-10)
}

# various unit tests for the assignment solvers

#
# Test 1: in optimal assignment, each person is assigned to their most preferred arm
#
budget <- c(1, 1, 1)
reward <- matrix(c(
  .8, 1, .1,
  1, .6, .2,
  .2, .5, 1),
  nrow = 3, byrow=TRUE
)
ans <- matrix(c(
  0, 1, 0,
  1, 0, 0,
  0, 0, 1),
  nrow = 3, byrow=TRUE
)

sol <- gen_policy$optimize(reward, budget/3)


cat('Test 1')
cat('\nsolution is correct: ', approx_equal(sol, ans))

#
# Test 2: same as test 1, but with a small parity penalty, which shouldn't affect solution
#
budget <- c(1, 1, 1)
reward <- matrix(c(
  .8, 1, .1,
  1, .6, .2,
  .2, .5, 1),
  nrow = 3, byrow=TRUE
)

group_id = c('g1','g1','g2')
target = c(1/3, 1/3, 1/3)
penalty = 0.1

ans <- matrix(c(
  0, 1, 0,
  1, 0, 0,
  0, 0, 1),
  nrow = 3, byrow=TRUE
)

sol <- gen_policy$optimize(reward, budget/3, group_id, penalty)

cat('\n\nTest 2')
cat('\nsolution is correct: ', approx_equal(sol, ans))

#
# Test 3: with a large penalty, solution should satisfy target distribution
#
budget <- c(1, 1, 1)
reward <- matrix(c(
  .8, 1, .1,
  1, .6, .2,
  .2, .5, 1),
  nrow = 3, byrow=TRUE
)

group_id = c('g1','g1','g2')
target = c(1/3, 1/3, 1/3)
penalty = 10

sol <- gen_policy$optimize(reward, budget/3, group_id, penalty)

cat('\n\nTest 3')

colnames(sol) <- c('y0', 'y1', 'y2')
group_dist <- bind_cols(
    group = group_id,
    as_tibble(sol, .name_repair='minimal')
  ) %>%
  group_by(group) %>%
  summarize_all(mean) %>%
  select(-group)

cat('\ntarget distribution satisfied: ', approx_equal(group_dist, target))

#
# test 4: optimal solution forces one person to have zero utility
#
budget <- c(1, 1, 1)
reward <- matrix(c(
  1.1, 0, 0,
  1, 0, 0,
  0, 0, 1),
  nrow = 3, byrow=TRUE
)
ans <- diag(3)

sol <- gen_policy$optimize(reward, budget/3)

cat('\n\nTest 4')
cat('\nsolution is correct: ', approx_equal(sol, ans))

#
# test 5: mulitple assignment to same treatment
#
budget <- c(2, 2, 2)
reward <- matrix(c(
  1.1, 0, 0,
  1, 0, 0,
  0, 0, 1),
  nrow = 3, byrow=TRUE
)

ans <- matrix(c(
  1, 0, 0,
  1, 0, 0,
  0, 0, 1),
  nrow = 3, byrow=TRUE
)

sol <- gen_policy$optimize(reward, budget/3)


cat('\n\nTest 5')
cat('\nsolution is correct: ', approx_equal(sol, ans))

#
# test 6: fractional solution
#
budget <- c(1.5, 1, 1)
reward <- matrix(c(
  1.1, 0, 0,
  1, .1, .1,
  0, 0, 1),
  nrow = 3, byrow=TRUE
)

ans <- matrix(c(
  1, 0, 0,
  0.5, 0.5, 0,
  0, 0, 1),
  nrow = 3, byrow=TRUE
)

sol <- gen_policy$optimize(reward, budget/3)

cat('\n\nTest 6')
cat('\nsolution is correct: ', approx_equal(sol, ans))

#
# test 7: comparison with analytic solution for 2 treatments
#
budget <- c(90, 10)
reward <- matrix(runif(200), nrow=100)

threshold <- sort(reward[,2] - reward[,1], decreasing=TRUE)[10]
treated <- as.integer(reward[,2] - reward[,1] >= threshold)
ans <- matrix(c(1-treated, treated), nrow=100)

sol <- gen_policy$optimize(reward, budget/100)

cat('\n\nTest 7')
cat('\nsolution is correct: ', approx_equal(sol, ans))

#
# test 8: speed test
#
n <- 1e3
k <- 5

reward <- matrix(runif(n*k), nrow=n)
budget <- rep(n/k, k)
group = rep(1:10, n/10)
target = rep(1/k, k)

ts <- system.time(
  sol <- gen_policy$optimize(reward, budget/n)
)

ts_parity <- system.time(
  sol <- gen_policy$optimize(reward, budget/n, group, 1)
)

cat('\n\nTest 8')
cat('\ntime: ', ts['elapsed'])
cat('\ntime with parity: ', ts_parity['elapsed'])


