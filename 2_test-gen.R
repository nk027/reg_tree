n <- 100
W <- matrix(rgamma(n^2, shape = 10, 10),  nrow = n, ncol = n)
W[lower.tri(W)] <- 0
W <- W %*% t(W)
diag(W) <- 0
W

## Row standardizes a matrix
row_stdz <- function(W){
  sums <- rowSums(W)
  return(W / sums)
}
(W <- row_stdz(W))
set.seed(123)
X <- cbind( 1, rnorm(n), rnorm(n) )
Z1 <- rnorm(n, 0, 100)
Z2 <- rnorm(n, 0, 100)
Z3 <- rnorm(n, 0, 100)
RHO = -.5
BETA <- c(5, -2 , 2.5)
SIGMA = 0.03
Y <- solve(diag(n) - RHO * W) %*% (X %*%  BETA + ifelse(Z1<=3, 5 * X[, 1], 0) + 
                                     ifelse(Z2<=7, 3 * X[,3], 0) + 
                                     ifelse(Z3<=4, 8 * X[,2], 0) +
                                     rnorm(n, mean = 0,sd = SIGMA) )
library(spdep)
w.list <- mat2listw(W, style = 'W')
reg_df <- data.frame(Y = as.matrix(Y), X, Z1, Z2, Z3)
reg_df$X1Z1 <- reg_df$X1 * as.integer(Z1<=3)
reg_df$X3Z2 <- reg_df$X3 * as.integer(Z2<=7)
reg_df$X2Z3 <- reg_df$X2 * as.integer(Z3<=4)
mod_true <- lagsarlm(Y ~ X2 + X3 + X1Z1 + X3Z2 + X2Z3, data = reg_df, listw = w.list)

mod_filter <- lagsarlm(Y ~ X2 + X3, data = reg_df, listw = w.list)

reg_df$Ytilde <- reg_df$Y - coef(mod_filter)[1] * W %*% reg_df$Y

summary(mod_true)

source("reg_tree/8_model-fun.R")
nodes <- get_nodes(reg_df, split_vars = c("Z1", "Z2", "Z3"), 
                   formula = "Ytilde ~ X2 + X3", verbose = TRUE, 
                   max_steps = 10, min_obs = 20, pval = 0.001, cpp = FALSE)
df_split <- nodes2dfs(nodes, terminal = TRUE)
library(microbenchmark)
# Unit: seconds
# expr
# get_nodes(reg_df, split_vars = c("Z1", "Z2", "Z3"), formula = "Ytilde ~ X2 + X3",      verbose = TRUE, n_splits = nrow(reg_df), max_steps = 10,      min_obs = 20, pval = 0.001, cpp = FALSE)
#   min       lq     mean     median       uq      max      neval
# 62.64016 63.13553 63.24686 63.27059 63.46811 64.00353    10
#microbenchmark(get_nodes(reg_df, split_vars = c("Z1", "Z2", "Z3"), 
#          formula = "Ytilde ~ X2 + X3", verbose = TRUE, n_splits = nrow(reg_df),
#          max_steps = 10, min_obs = 20, pval = 0.001, cpp = FALSE), times = 10)
# Unit: seconds
# expr
# get_nodes(reg_df, split_vars = c("Z1", "Z2", "Z3"), formula = "Ytilde ~ X2 + X3",      verbose = TRUE, n_splits = nrow(reg_df), max_steps = 10,      min_obs = 20, pval = 0.001, cpp = TRUE)
# min       lq     mean   median       uq      max neval
# 58.0226 58.20946 59.38295 59.14392 60.23289 61.32172    10
# microbenchmark(get_nodes(reg_df, split_vars = c("Z1", "Z2", "Z3"), 
#                        formula = "Ytilde ~ X2 + X3", verbose = TRUE, n_splits = nrow(reg_df),
#                        max_steps = 10, min_obs = 20, pval = 0.001, cpp = TRUE), times = 10)
# 
microbenchmark(get_nodes(reg_df, split_vars = c("Z1", "Z2", "Z3"), 
                        formula = "Ytilde ~ X2 + X3", verbose = TRUE, n_splits = nrow(reg_df),
                         max_steps = 10, min_obs = 20, pval = 0.001, cpp = TRUE), times = 2)

simnodes <- simplify_nodes(nodes)
unnodes <- untree(simnodes, FALSE)
plantt <- make_candidates(unnodes)
reg_plan <- make_plan(plantt)
dat_split <- get_data(reg_df, reg_plan$plan)

dat_split


tree <- plant_tree(nodes, lm, formula = "Ytilde ~ X2 + X3")



print.node <- function(node, level, grp){
  require(igraph)
  #indent_print(node$coefs$coefficients, .indent = ifelse(level == 1, "", paste0(paste0(rep("-", level), collapse = ''), "  ", collapse = '')))
  nod <- l2df(node$nodes, ncol = 3, byrow = TRUE)
  grp <- l2df(grp, ncol = 1)
  colnames(nod) <- names(node$nodes[[1]])
  rownames(nod) <- paste("level: ", rownames(nod))
  nod <- cbind(nod, grp)
  return(nod)
  #indent_print(nod, .indent = ifelse(level == 1, "", paste0(paste0(rep("-", level), collapse = ''), "  ", collapse='')))
}

get_last <- function(node, level){
  if(is.null(names(node))){
    return(Recall(node[[1]]))
  }
  if(is.null(names(node$nodes))){
    ret <- node$nodes[[2]]
    ret <- ret$value
  }else{
    ret <- node$nodes$value
  }
  return(ret)
}

summary.tree <- function(tree, level = 1, grp = NULL){
  cat("\n")
  cat(ifelse(level == 1, "Root", paste("Split", level)))
  cat("\n\n")
  leq <- tree[[1]]
  gre <- tree[[2]]
  
  if(is.null(grp)){
    grpx <- "gre"
  }else{
    grpx <- list(grp, "gre")
  }
  if(is.null(grp)){
    grpy <- "leq"
  }else{
    grpy <- list(grp, "leq")
  }
  
  term_leq <- !is.null(names(leq))
  term_gre <- !is.null(names(gre))
  if(term_leq & term_gre){
    cat("<= ", get_last(leq, level),'\n' )
    print.node(leq, level, grp = grpy)
    cat("\n")
    cat("> ", get_last(gre, level),'\n' )
    print.node(gre, level, grp = grpx)
  }else if(term_leq){
    cat("<= ", get_last(leq, level),'\n' )
    print.node(leq, level, grp = grpy)
    cat('\n', "> ", get_last(leq, level),":")
    levelx <- level + 1
    Recall(gre, level = levelx, grp = grpx)
  }else if(term_gre){
    cat("> ", get_last(gre, level),'\n' )
    print.node(gre, level = level, grp = grpx)
    levelx <- level + 1
    Recall(leq, level = levelx, grp = grpy)
  }else{
    levelx <- level + 1
    return(list(Recall(leq, level = levelx, grp = grpy), 
                Recall(gre, level = levelx, grp = grpx)))
  }
}

summary.tree(tree)

library(partykit)
lmtree(Ytilde ~ X2 + X3 | Z1 + Z2 + Z3, data = reg_df, minsize = 30)

