library(tidyverse)
library(reticulate)
use_python('/usr/bin/python3')

# load the Python optimization routine
gen_policy <- import('gen_policy_with_costs')

# check that two objects are equal up to small numerical differences
approx_equal <- function(a, b) {
  return(max(abs(a-b)) < 1e-10)
}

# various unit tests for the assignment solvers

#
# Test 1: in optimal assignment, with effectively no budget constraint, 
# each person is assigned to their most preferred arm
#
budget = 1

reward = matrix(c(
  .1, 1, .8,
  .2, .6, 1,
  .2, .5, 1),
  nrow = 3, byrow=TRUE
)

cost = matrix(c(
  0, 1, 1,
  0, .8, .8,
  0, .8, .8),
  nrow = 3, byrow=TRUE
)

ans <- matrix(c(
  0, 1, 0,
  0, 0, 1,
  0, 0, 1),
  nrow = 3, byrow=TRUE
)

sol <- gen_policy$optimize(reward, cost, budget)

cat('Test 1')
cat('\nsolution is correct: ', approx_equal(sol, ans))

#
# Test 2: same as Test 1, but with a small parity penalty, which shouldn't affect solution
#

group_id = c('g1','g2','g2')
penalty = 0.1

sol <- gen_policy$optimize(reward, cost, budget, group_id, penalty)

cat('\n\nTest 2')
cat('\nsolution is correct: ', approx_equal(sol, ans))

#
# Test 3: Same as Test 2 but with a large penalty; 
# per capita expenditure should be the same for both groups
#

penalty = 10

sol <- gen_policy$optimize(reward, cost, budget, group_id, penalty)

colnames(sol) <- c('y0', 'y1', 'y2')
parity_diff <- bind_cols(
  group = group_id,
  as_tibble(sol*cost, .name_repair='minimal')
) %>% 
  rowwise() %>%
  mutate(spend=sum(c_across(!group))) %>%
  group_by(group) %>%
  summarize(per_capita_spend = mean(spend), .groups='drop') %>%
  mutate(rel_per_capita_spend = per_capita_spend - mean(per_capita_spend)) %>%
  summarize(sum(abs(rel_per_capita_spend))) %>%
  as.numeric()
  
cat('\n\nTest 3')
cat('\nParity satisfied: ', parity_diff < 1e-10)

#
# Test 4: comparison with analytic solution for 2 treatments
#
budget <- .1

reward <- matrix(runif(200), nrow=100)
reward[,2] <- reward[,1] + reward[,2]

cost <- matrix(runif(200), nrow=100)
cost[,1] <- 0

sol <- gen_policy$optimize(reward, cost, budget)

stats <- cbind(reward, cost[,2], sol[,2])
colnames(stats) <- c('y0','y1','cost','treated')
stats <- tibble(as.data.frame(stats)) %>%
  mutate(
    treated = treated == 1,
    delta = (y1-y0)/cost) %>%
  arrange(desc(delta)) %>%
  mutate(
    per_capita_spend = cumsum(cost)/n(),
    below_threshold = per_capita_spend <= budget)

cat('\n\nTest 4')
cat('\nsolution is correct: ', all(stats$treated == stats$below_threshold))

#
# Test 5: speed test
#
n <- 1e3
k <- 5

reward <- matrix(runif(n*k), nrow=n)
cost <- matrix(runif(n*k), nrow=n)
cost[,1] <- 0
budget <- 0.1

group_id = rep(1:10, n/10)
penalty = 1

ts <- system.time(
  sol <- gen_policy$optimize(reward, cost, budget)
)

ts_parity <- system.time(
  sol <- gen_policy$optimize(reward, cost, budget, group_id, penalty)
)

cat('\n\nTest 5')
cat('\ntime: ', ts['elapsed'])
cat('\ntime with parity: ', ts_parity['elapsed'])
