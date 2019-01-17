# Have we reached a terminal node
enough_obs <- function(df_list, min_obs) {
  leq_works <- nrow(df_list$leq) > min_obs
  gre_works <- nrow(df_list$gre) > min_obs
  return(leq_works && gre_works)
}

# Split data
splitter <- function(df, split_var, split_val) {
  split_data <- df[[split_var]]
  sel_low <- which(split_data <= split_val)
  sel_high <- which(split_data > split_val)
  df_low  <- df[sel_low, ]
  df_high <- df[sel_high, ]
  return(list(leq = df_low, gre = df_high))
}

# Return model's coefficients
coefs <- function(df_list, mod, fun, ...) {
  mod_leq <- fun(mod, data = df_list$leq, ...)
  mod_gre <- fun(mod, data = df_list$gre, ...)
  cov_leq <- vcov(mod_leq)
  cov_gre <- vcov(mod_gre)
  return(list(coef_leq = coef(mod_leq), 
              coef_gre = coef(mod_gre), 
              cov_leq = cov_leq, 
              cov_gre = cov_gre))
}

# What's the p-value
obj_fun <- function(model_list) {
  leq <- as.matrix(model_list$coef_leq)
  gre <- as.matrix(model_list$coef_gre)
  covs <- as.matrix(model_list$cov_leq) + as.matrix(model_list$cov_gre)
  stat <- t((leq - gre)) %*% solve(covs) %*% (leq - gre)
  pval <- dchisq(stat, df = length(leq))
  return(list(stat = stat, p_val = pval))
}

avg_dist <- function(regions, distmat){
  dist_regions <- distmat[regions, regions]
  # not including diagonal (see ?lower.tri)
  mean_dist <- mean(dist_regions[lower.tri(dist_regions)])
  return(mean_dist)
}

# Source Cpp version
Rcpp::sourceCpp("reg_tree/cpp_version.cpp")

# Takes in df & looks for nice splits, returns the best
find_split <- function(
  df, split_vars, formula, 
  fun = lm, predictors = 3, 
  min_obs = 3, n_splits = 10, distmat, penalty, pmult, cpp_sp, ...) {
  if(penalty & cpp_sp){
    stop("Penalty not yet implemented in C++ version!")
  }
  best_vals <- matrix(NA, nrow = length(split_vars), ncol = 3)
  rownames(best_vals) <- split_vars
  colnames(best_vals) <- c("var_value", "chisq_value", "penalized")
  if(cpp_sp){
    yvar <- all.vars(as.formula(formula), df)[1]
    ycpp <- as.matrix(df[yvar], ncol = 1)
    Xcpp <- model.matrix(as.formula(formula), df)
  }
  j <- 1
  for(var in split_vars) {
    if(cpp_sp){
      
      Zcpp <- as.matrix(df[var], ncol = 1)

      split_vals <- as.vector(Zcpp)
      var_stat <- get_var_stat(y = ycpp, X = Xcpp, Z = Zcpp, min_obs = min_obs)
    }else{
    var_stat <- vector("double", length = length(split_vars))
    if(penalty)varp_stat <- vector("double", length = length(split_vars))
    i <- 1
    split_vals <- seq(min(df[[var]]), max(df[[var]]), length.out = n_splits)
    for(z in split_vals) {
      df_list <- splitter(df, var, z)
      
      if(enough_obs(df_list, min_obs = min_obs)) {
        res <- obj_fun(coefs(df_list, formula, fun, ...))
        var_stat[i] <- as.numeric(res$stat)
        if(penalty){
          # TODO: This is potentially dangerous but works for now
          regions_leq <- rownames(df_list$leq)
          regions_gre <- rownames(df_list$gre)
          w_leq <- length(regions_leq)
          w_gre <- length(regions_gre)
          wghts <- c(w_leq, w_gre) / (w_leq + w_gre)
          adist_leq <- avg_dist(regions_leq, distmat)
          adist_gre <- avg_dist(regions_gre, distmat)
          # TODO: Penalty is extremely small -> multiplier
          penalty <- ifelse(is.null(pmult), 1, pmult) / weighted.mean(c(adist_leq, adist_gre), wghts)
          varp_stat[i] <- var_stat[i] + penalty
        }
      } else {
        var_stat[i] <- NA
        if(penalty)varp_stat[i] <- NA
      }
      i <- i + 1
    } # for(z in split_vals)
    } # else of cpp
    if(all(is.na(var_stat))) {
      best_vals[j, 1] <- NA
      best_vals[j, 2] <- NA
    } else {
      if(penalty){
        best_vals[j, 1] <- split_vals[which(varp_stat == max(varp_stat, na.rm = TRUE))][1]
        # TODO: here penalized stat?
        # p-vals should be correct like here!
        best_vals[j, 2] <- var_stat[which(varp_stat == max(varp_stat, na.rm = TRUE))][1]
        best_vals[j, 3] <- max(varp_stat, na.rm = TRUE)
      }else{
      best_vals[j, 1] <- split_vals[which(var_stat == max(var_stat, na.rm = TRUE))][1]
      best_vals[j, 2] <- max(var_stat, na.rm = TRUE)
      }
    }
    j <- j + 1
  } # for(var in split_vars)
  
  split <- list()
  if(all(is.na(best_vals[, 2]))) {
    split$pval <- Inf
    return(split)
  }
  if(penalty){
    best_split <- which(best_vals[, 3] == max(best_vals[, 3], na.rm = TRUE))[1]
  }else{
    best_split <- which(best_vals[, 2] == max(best_vals[, 2], na.rm = TRUE))[1]
  }
  split$pval <- dchisq(best_vals[best_split, 2], df = predictors)
  split$name <- rownames(best_vals)[best_split]
  split$value <- best_vals[best_split, 1]

  return(split)
}

# Finds splits until min_obs, max_steps or pval is breached
get_nodes <- function(
  df, split_vars, formula,predictors = 3,
  n_splits = 10, min_obs = 3, max_steps = 4, pval = 0.05,
  step = 0, verbose = FALSE, state = NULL,
  distmat = NULL, penalty = FALSE, pmult = NULL, cpp = FALSE,  ...) {
  
  split <- find_split(df, split_vars, formula, fun = lm, 
                      predictors, min_obs, n_splits, distmat = distmat, penalty = penalty, pmult = pmult, cpp_sp = cpp, ...)
  
  if(split$pval < pval && step < max_steps) {
    if(is.null(state)){
      state <- list(split)
    }else{
      state[[step + 1]] <- split
    }
    nodes <- splitter(df, split_var = split$name, split_val = split$value)
    stepx <- step + 1
    return(list(Recall(df = nodes$leq, split_vars, formula, predictors, 
                          n_splits, min_obs, max_steps, step = stepx, verbose, pval = pval, state = state), 
                Recall(df = nodes$gre, split_vars, formula, predictors, 
                          n_splits, min_obs, max_steps, step = stepx, verbose, pval = pval, state = state)))
  } else {
    if(verbose) {
      # Prints reason and adds the split output
      if(split$pval == Inf) {
        cat("Encountered node below minimum size\n")
      } else if(split$pval >= pval) {
        cat("Encountered p-value of", split$pval, "\n")
      }
      if(step >= max_steps) cat("Maximum steps performed\n")
      return(list(df = df, node = state))
    } else {
      # Returns a plain df
      return(df)
    }
  }
}

do_reg <- function(node, fun, formula, ...){
  out <- fun(formula, data = node$df, ...)
  return(list(coefs = out))
}

plant_tree <- function(nodes, fun = lm, formula, ...){
  leq <- nodes[[1]]
  gre <- nodes[[2]]
  
  term_leq <- !is.null(names(leq))
  term_gre <- !is.null(names(gre))
  
  if(term_leq & term_gre){
    out_leq <- do_reg(leq, fun, formula, ...)
    out_leq$nodes <- leq$node
    out_gre <- do_reg(gre, fun, formula, ...)
    out_gre$nodes <- gre$node
    return(list(out_leq, out_gre))
  }else if(term_leq){
    out_leq <- do_reg(leq, fun, formula, ...)
    out_leq$nodes <- leq$node
    return(list(out_leq, Recall(gre, fun, formula, ...)))
  }else if(term_gre){
    out_gre <- do_reg(gre, fun, formula)
    out_gre$nodes <- gre$node
    return(list(out_gre, Recall(leq, fun, formula, ...)))
  }else{
    return(list(Recall(leq, fun, formula, ...), 
                Recall(gre, fun, formula, ...)))
  }
}


l2df <- function(l, ...){
  return(data.frame(matrix(unlist(l), ...), stringsAsFactors = FALSE))
}

node_summary <- function(node, grp){
  nod <- l2df(node$node, ncol = 3, byrow=TRUE)
  #print(nod)
  grp <- l2df(grp, ncol = 1)
  colnames(grp) <- 'direction'
  #grp[nrow(grp),] <- 'terminal'
  colnames(nod) <- names(node$node[[1]])
  rownames(nod) <- paste("level: ", rownames(nod))
  nod <- data.frame(cbind(nod, grp))
  return(nod)
}

simplify_nodes <- function(nodes, level = 1, grp = NULL){
  leq <- nodes[[1]]
  gre <- nodes[[2]]
  
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
    return(list(
    node_summary(leq, grp = grpy),
    node_summary(gre, grp = grpx)))
  }else if(term_leq){
    levelx <- level + 1
    return(list(
    node_summary(leq, grp = grpy),
    Recall(gre, level = levelx, grp = grpx)))
  }else if(term_gre){
    levelx <- level + 1
    return(list(
    node_summary(gre, grp = grpx),
    Recall(leq, level = levelx, grp = grpy)))
  }else{
    levelx <- level + 1
    return(list(Recall(leq, level = levelx, grp = grpy), 
                Recall(gre, level = levelx, grp = grpx)))
  }
}

untree <- function(nodes, simplify = FALSE){
  out <- list()
  lumberjack <- function(nodes){
    leq <- nodes[[1]]
    gre <- nodes[[2]]
    term_leq <- !is.null(names(leq))
    term_gre <- !is.null(names(gre))
    parent <- parent.frame()
    pos <- length(parent$out) + 1
    if(term_leq & term_gre){
      parent$out[[pos]] <- leq
      parent$out[[pos + 1]] <- gre
      #print('both')
    }else if(term_leq){
      parent$out[[pos]] <- leq
      Recall(gre)
      #print('leq')
    }else if(term_gre){
      parent$out[[pos]] <- gre
      Recall(leq)
      #print("gre")
    }else{
      #print('none')
      Recall(leq)
      Recall(gre)
    }
  }
  lumberjack(nodes)
  if(simplify){
    out <- do.call("rbind", out)
  }
  return(out)
}

make_candidates <- function(nodes){
  plan_node <- function(node){
    combine_node <- function(node_row){
      dir <- ifelse(node_row[4] == 'leq', '<=', '>')
      condition <- paste(node_row[2], dir, node_row[3], collapse = '')
      return(condition)
    }
    apply(node, 1, combine_node)
  }
  plan <- lapply(nodes, plan_node)
  return(plan)
}

# Helper
cumpaste <- function(vec, collps = NULL){
  return(sapply(vec, function(x)paste(vec[1:which(vec == x)], collapse = collps)))
}

fix_plan <- function(plan){
  he_fx <- function(pp){
    require(stringr)
    str <- pp[[length(pp)]]
    spstr <- stringr::str_split(str, "&", simplify = TRUE)
    #print(spstr)
    final <- spstr[ncol(spstr)]
    if(stringr::str_detect(final, ">")){
      final <- stringr::str_replace(final, ">", "<=")
    }else if(stringr::str_detect(final, "<=")){
      final <- stringr::str_replace(final, "<=", ">")
    }
    spstr[ncol(spstr)] <- final
    pp_new <- c(pp, paste0(spstr, collapse = '&'))
    return(pp_new)
  }
  return(lapply(plan, function(x) he_fx(x)))
}

make_plan <- function(candidates){
  plan <- lapply(candidates, cumpaste, collps = " & ")
  terminals <- lapply(plan, function(pp) pp[[length(pp)]])
  #print(terminals)
  plan <- fix_plan(plan)
  #terminals <- fix_plan(terminals)
  terminals <- do.call('c', terminals)
  plan <- do.call('c', plan)
  plan <- plan[!duplicated(plan)]
  terminals <- terminals[!duplicated(terminals)]
  names(plan) <- NULL
  return(list('plan' = plan, 'terminal' = terminals))
}

get_data <- function(data, plan){
  split_data  <- lapply(plan, function(cond) subset(data, eval(parse(text = cond))))
  names(split_data) <- paste0("df", 1:length(plan))
  for(pp in seq_along(plan)){
    attr(split_data[[pp]], which = "split") <- plan[[pp]]
  }
  return(split_data)
}

nodes2dfs <- function(nodes, dat, terminal = TRUE){
  simp <- simplify_nodes(nodes)
  untr <- untree(simp)
  cand <- make_candidates(untr)
  plan <- make_plan(cand)
  if(terminal){
    plan <- plan$terminal
  }else{
    plan <- plan$plan
  }
  splt_data <- get_data(data = dat, plan)
  return(splt_data)
}

lm_list <- function(dfs, formula){
  regs <- lapply(dfs, function(x) lm(formula = formula, data = x))
  splits <- lapply(dfs, function(x) attr(x, which = 'split'))
  names(regs) <- splits
  return(regs)
}

# E.g. run:
# tree <- get_nodes(df, split_vars, formula, verbose = TRUE)
# reg_data <- nodes2dfs(tree)