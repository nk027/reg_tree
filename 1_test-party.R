library(partykit)
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

#lmtree(gva ~  init | emp_a + emp_i + inv_a + inv_i, data = data)
plot(lmtree(gva ~  init | emp_a + emp_i + inv_a + inv_i, data = data))
