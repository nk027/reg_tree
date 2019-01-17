require(partykit)
require(spdep)

row_stdz <- function(W){
  sums <- rowSums(W)
  return(W / sums)
}

load("data/processed_data.Rdata")

source("reg_tree/8_model-fun.R")

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


W_negexp <- exp(-((dist/1000)^2))
diag(W_negexp) <- 0
W_negexp <- row_stdz(W_negexp)
listw_negexp <- mat2listw(W_negexp, style = "W")

lm_mod <- lm(gdp_gr~gdp_init, data = data)

lm_tests <- lm.LMtests(lm_mod, listw = listw_negexp, test = "all")
summary(lm_tests)



sar_mod <- lagsarlm(gdp_gr~gdp_init, data = data, 
                    listw = listw_negexp)
data$gdp_gr_sar <- data$gdp_gr - coef(sar_mod)[1] * W_negexp %*% data$gdp_gr

sem_mod <- errorsarlm(gdp_gr~gdp_init, data = data, 
                      listw = listw_negexp)
data$gdp_gr_sem <- data$gdp_gr - coef(sem_mod)[1] * W_negexp %*% data$gdp_gr
data$gdp_init_sem <- data$gdp_init - coef(sem_mod)[1] * W_negexp %*% data$gdp_init



tree_lm <- get_nodes(data, 
                     split_vars = split_vars, 
                     formula = "gdp_gr ~ gdp_init",
                     max_steps = 5, n_splits = 1000, min_obs = 50, 
                     verbose = TRUE)

tree_sar <- get_nodes(data, 
                      split_vars = split_vars, 
                      formula = "gdp_gr_sar ~ gdp_init",
                      max_steps = 5, n_splits = 1000, min_obs = 50, 
                      verbose = TRUE)

tree_sem <- get_nodes(data, 
                      split_vars = split_vars, 
                      formula = "gdp_gr_sem ~ gdp_init_sem",
                      max_steps = 5, n_splits = 1000, min_obs = 50, 
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



data$clubs_lm <- NA
for(i in 1:length(terminal_lm)){
  name_temp <- rownames(terminal_lm[[i]])
  data[which(rownames(data) %in% name_temp) , "clubs_lm"] <- paste("Club", i)
}
data$clubs_lm <- as.factor(data$clubs_lm)


data$clubs_sar <- NA
for(i in 1:length(terminal_sar)){
  name_temp <- rownames(terminal_sar[[i]])
  data[which(rownames(data) %in% name_temp) , "clubs_sar"] <- paste("Club", i)
}
data$clubs_sar <- as.factor(data$clubs_sar)


data$clubs_sem <- NA
for(i in 1:length(terminal_sem)){
  name_temp <- rownames(terminal_sem[[i]])
  data[which(rownames(data) %in% name_temp) , "clubs_sem"] <- paste("Club", i)
}
data$clubs_sem <- as.factor(data$clubs_sem)



regs_lm <- lapply(terminal_lm, 
                  function(x) lm(formula = "gdp_gr~gdp_init", data = x))
regs_sar <- lapply(terminal_sar, 
                   function(x) lm(formula = "gdp_gr_sar~gdp_init", data = x))
regs_sem <- lapply(terminal_sem, 
                   function(x) lm(formula = "gdp_gr_sem~gdp_init_sem", data = x))



save(data, tree_lm, tree_sar, tree_sem,
     regs_lm, regs_sar, regs_sem,
     file = "./output/run.Rda")

