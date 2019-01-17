load("data/processed_data.Rdata")

source("reg_tree/9_data-fun.R")

gva <- dfify(depvar_list$gva_tot)

data <- cbind((gva$GVA_tot_2015 - gva$GVA_tot_1996) / 19,
              gva$GVA_tot_1996,
              split_list$emp_agr$EMP_agr_1996,
              split_list$emp_ind$EMP_ind_1996,
              split_list$inv_agr$INV_agr_1996,
              split_list$inv_ind$INV_ind_1996)
data <- as.data.frame(data)
rownames(data) <- gva$NUTS_ID
names(data) <- c("gva", "init", "emp_a", "emp_i", "inv_a", "inv_i")

mdl <- lm(gva ~ init + emp_a + emp_i + inv_a + inv_i, data)
summary(mdl)

source("reg_tree/8_model-fun.R")

tree <- get_nodes(data, 
                  split_vars = c("emp_a", "emp_i", "inv_a", "inv_i"), 
                  formula = "gva ~ init", verbose = TRUE, min_obs = 20, max_steps = 5)

saveRDS(tree, "output/test-run.rds")

test_tree_data <- data.frame(gdp_gr = depvar_list$gdp_pc_gr$GDP_gr_0015,
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

rownames(test_tree_data) <- depvar_list$gdp_pc_gr$NUTS_ID

# for(i in names(test_tree_data[2:length(names(test_tree_data))])){
#   val <- sum(!(split_list[[i]]$NUTS_ID == rownames(test_tree_data)))
#   print(val)
# }

w_tree <- get_nodes(test_tree_data, 
                    split_vars = names(test_tree_data)[3:ncol(test_tree_data)], 
                    formula = "gdp_gr ~ gdp_init", max_steps = 5, n_splits = 1000, cpp = FALSE, 
                    penalty = TRUE, pmult = 1000, distmat = dist,
                    min_obs = 50, verbose = TRUE)

s_nodes <- simplify_nodes(w_tree)

u_nodes <- untree(s_nodes)

cand <- make_candidates(u_nodes)

plan <- make_plan(cand)

dat <- get_data(test_tree_data, plan$terminal)
#node_summary(w_tree[[1]], 'leq')

splt_data <- nodes2dfs(w_tree)

plant_tree(w_tree, formula = "gdp_gr ~ gdp_init", max_steps = 5, n_splits = 50,
                    min_obs = 50, verbose = TRUE)
# plant_tree(w_tree, formula = "gdp_gr ~ gdp_init")






