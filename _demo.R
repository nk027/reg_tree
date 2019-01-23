# Demonstrate usage
#
# Author: NK, LV & DW
# Editor: NK


# Setup -------------------------------------------------------------------

set.seed(123)
library(spdep)
source("tree_core.R")
source("tree_aux.R")

row_std <- function(W) {
  return(W / rowSums(W))
}


# Generated data ---------------------------------------------------------

n <- 100

# weights
W <- matrix(rgamma(n ^ 2, shape = 10, 10),  nrow = n, ncol = n)
W[lower.tri(W)] <- 0
W <- W %*% t(W)
diag(W) <- 0
W <- row_std(W)

# data
X <- cbind(1, rnorm(n), rnorm(n))
Z1 <- rnorm(n, 0, 100)
Z2 <- rnorm(n, 0, 100)
Z3 <- rnorm(n, 0, 100)
RHO = -.5
BETA <- c(5, -2, 2.5)
SIGMA = 0.03
Y <- solve(diag(n) - RHO * W) %*% (X %*%  BETA + ifelse(Z1 <= 3, 5 * X[, 1], 0) +
                                     ifelse(Z2 <= 7, 3 * X[, 3], 0) +
                                     ifelse(Z3 <= 4, 8 * X[, 2], 0) +
                                     rnorm(n, mean = 0, sd = SIGMA))

# spdep
w.list <- mat2listw(W, style = "W")
reg_df <- data.frame(Y = as.matrix(Y), X, Z1, Z2, Z3)
reg_df$X1Z1 <- reg_df$X1 * as.integer(Z1 <= 3)
reg_df$X3Z2 <- reg_df$X3 * as.integer(Z2 <= 7)
reg_df$X2Z3 <- reg_df$X2 * as.integer(Z3 <= 4)
mod_true <- lagsarlm(Y ~ X2 + X3 + X1Z1 + X3Z2 + X2Z3, data = reg_df, listw = w.list)
mod_filter <- lagsarlm(Y ~ X2 + X3, data = reg_df, listw = w.list)
reg_df$Ytilde <- reg_df$Y - coef(mod_filter)[1] * W %*% reg_df$Y
summary(mod_true)


# Regression tree ---------------------------------------------------------

nodes <- get_nodes(reg_df, split_vars = c("Z1", "Z2", "Z3"), 
                   formula = "Ytilde ~ X2 + X3", verbose = TRUE, 
                   max_steps = 10, min_obs = 10, pval = 0.001, cpp = FALSE)

# helpers
df_split <- nodes2dfs(nodes, terminal = TRUE)
sim_nodes <- simplify_nodes(nodes)
un_nodes <- untree(sim_nodes, FALSE)
plant <- make_candidates(un_nodes)
reg_plan <- make_plan(plant)
data_split <- get_data(reg_df, reg_plan$plan)
tree <- plant_tree(nodes, lm, formula = "Ytilde ~ X2 + X3")

summary.tree(tree)


# Processed data --------------------------------------------------------------------

load("data/processed_data.Rdata")

# aggregate
data <- data.frame(
  gdp_gr = depvar_list$gdp_pc_gr$GDP_gr_0015, 
  gdp_init = depvar_list$gdp_pc$GDP_pc_2000,
  pop_den = split_list$pop_den$POP_den_2000,
  inv_agr = split_list$inv_agr$INV_agr_2000,
  inv_con = split_list$inv_con$INV_con_2000,
  inv_ind = split_list$inv_ind$INV_ind_2000,
  inv_mar = split_list$inv_mar$INV_mar_2000,
  inv_nms = split_list$inv_nms$INV_nms_2000,
  emp_agr = split_list$emp_agr$EMP_agr_2000,
  emp_con = split_list$emp_con$EMP_con_2000,
  emp_ind = split_list$emp_ind$EMP_ind_2000,
  emp_mar = split_list$emp_mar$EMP_mar_2000,
  emp_nms = split_list$emp_nms$EMP_nms_2000,
  thw_agr = split_list$thw_agr$THW_agr_2000,
  thw_con = split_list$thw_con$THW_con_2000,
  thw_ind = split_list$thw_ind$THW_ind_2000,
  thw_mar = split_list$thw_mar$THW_mar_2000,
  thw_nms = split_list$thw_nms$THW_nms_2000)

rownames(data) <- depvar_list$gdp_pc_gr$NUTS_ID
split_vars <- names(data)[3:ncol(data)]

# weights
W_negexp <- exp(-((dist / 1000) ^ 2))
diag(W_negexp) <- 0
W_negexp <- row_std(W_negexp)
listw_negexp <- mat2listw(W_negexp, style = "W")

# models
lm_mod <- lm(gdp_gr ~ gdp_init, data = data)
lm_tests <- lm.LMtests(lm_mod, listw = listw_negexp, test = "all")
summary(lm_tests)

sar_mod <- lagsarlm(gdp_gr~gdp_init, data = data, listw = listw_negexp)
data$gdp_gr_sar <- data$gdp_gr - coef(sar_mod)[1] * W_negexp %*% data$gdp_gr
sem_mod <- errorsarlm(gdp_gr~gdp_init, data = data, listw = listw_negexp)
data$gdp_gr_sem <- data$gdp_gr - coef(sem_mod)[1] * W_negexp %*% data$gdp_gr
data$gdp_init_sem <- data$gdp_init - coef(sem_mod)[1] * W_negexp %*% data$gdp_init

# regression trees
max_steps <- 5
min_obs <- 50

tree_lm <- get_nodes(data, split_vars = split_vars, 
                     formula = "gdp_gr ~ gdp_init",
                     max_steps, min_obs, n_splits = 1000, 
                     verbose = TRUE)

tree_sar <- get_nodes(data, split_vars = split_vars, 
                      formula = "gdp_gr_sar ~ gdp_init",
                      max_steps, min_obs, n_splits = 1000, 
                      verbose = TRUE)

tree_sem <- get_nodes(data, split_vars = split_vars, 
                      formula = "gdp_gr_sem ~ gdp_init_sem",
                      max_steps, min_obs, n_splits = 1000, 
                      verbose = TRUE)

sim_nodes_lm <- simplify_nodes(tree_lm)
sim_nodes_lm <- untree(sim_nodes_lm)
cand_lm <- make_candidates(sim_nodes_lm)
plan_lm <- make_plan(cand_lm)
terminal_lm <- get_data(data, plan_lm$terminal)

sim_nodes_sar <- simplify_nodes(tree_sar)
sim_nodes_sar <- untree(sim_nodes_sar)
cand_sar <- make_candidates(sim_nodes_sar)
plan_sar <- make_plan(cand_sar)
terminal_sar <- get_data(data, plan_sar$terminal)

sim_nodes_sem <- simplify_nodes(tree_sem)
sim_nodes_sem <- untree(sim_nodes_sem)
cand_sem <- make_candidates(sim_nodes_sem)
plan_sem <- make_plan(cand_sem)
terminal_sem <- get_data(data, plan_sem$terminal)
