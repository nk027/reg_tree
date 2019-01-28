# Generate regression tree
#
# Author: DW & NK
# Editor: NK


# Helpers -----------------------------------------------------------------

# Have we reached a terminal node
enough_obs <- function(df_list, min_obs) {
  leq_works <- nrow(df_list$leq) >= min_obs
  gre_works <- nrow(df_list$gre) >= min_obs
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

# What's the average distance between the regions
# avg_dist <- function(regions, dist_mat) {
#   dist_regions <- dist_mat[regions, regions]
#   mean_dist <- mean(dist_regions[lower.tri(dist_regions)]) # strict lower triangular
#   return(mean_dist)
# }


# Meat --------------------------------------------------------------------

# Source the Rcpp implementation for the option cpp
cat("Sourcing RCpp...")
Rcpp::sourceCpp("cpp_implementation.cpp")

#' @title Find Split
#' @description Takes in a df & looks for nice splits.
#' @author DW & NK
#' 
#' @note Distance penalisation is currently canned due to instability and 
#' little theoretical base. Issues include: handling dist_mat and accessing
#' the appropriate distances & a sensible method of choosing and applying this
#' penalty.
#' Rcpp 
#'
#' @param df A dataframe with named columns and rows.
#' @param split_vars Vector of names of the variables used for splitting.
#' @param formula Formula to feed to fun().
#' @param fun Function to apply.
#' @param predictors Integer indicating the dof.
#' @param min_obs Integer with minimum number of observations in a split.
#' @param n_splits Integer determining how many splits should be attempted 
#' per variable (done via quantiles).
#' @param cpp Logical indicating whether to use the C++ implementation.
#' @param ... 
#'
#' @return Returns a list with a split's pval, name of the splitting variable 
#' and value at which to split or a pval of Inf if no split was found.
#'
find_split <- function(
  df, split_vars,
  formula, fun, predictors, 
  min_obs, n_splits,
  # dist_penalty = 0, dist_mat = NULL,
  cpp = FALSE,
  ...) {
  
  # if(dist_penalty && cpp) stop("dist_penalty not implemented in Rcpp")
  # if(dist_penalty) warning("dist_penalty is an experimental feature")
  if(cpp) warning("cpp is an experimental feature")
  
  best_vals <- matrix(NA, nrow = length(split_vars), ncol = 3)
  
  rownames(best_vals) <- split_vars
  colnames(best_vals) <- c("var_value", "chisq_value", "dist_pen")
  
  # if(cpp) {
  #   yvar <- all.vars(as.formula(formula), df)[1]
  #   ycpp <- as.matrix(df[yvar], ncol = 1)
  #   Xcpp <- model.matrix(as.formula(formula), df)
  # }
  
  j <- 1
  for(var in split_vars) {
    if(cpp) {
      Zcpp <- as.matrix(df[var], ncol = 1)
      split_vals <- as.vector(Zcpp)
      # Run Rcpp implementation
      var_stat <- get_var_stat(y = ycpp, X = Xcpp, Z = Zcpp, min_obs = min_obs)
    } else {
      var_stat <- vector("double", length = length(split_vars))

      i <- 1
      # split_vals <- seq(min(df[[var]]), max(df[[var]]), length.out = n_splits)
      split_vals <- quantile(df[[var]], seq(0, 1, length.out = n_splits))
      
      for(z in split_vals) {
        df_list <- splitter(df, var, z)
        if(enough_obs(df_list, min_obs = min_obs)) {
          res <- obj_fun(coefs(df_list, formula, fun))
          var_stat[i] <- as.numeric(res$stat)
          # if(dist_penalty) {
          #   wghts <- c(length(rownames(df_list$leq)), length(rownames(df_list$gre))) / 
          #     (length(rownames(df_list$leq)) + length(rownames(df_list$gre)))
          #   adist_leq <- avg_dist(rownames(df_list$leq), dist_mat)
          #   adist_gre <- avg_dist(rownames(df_list$gre), dist_mat)
          #   penalty <- dist_penalty * weighted.mean(c(adist_leq, adist_gre), wghts)
          #   var_stat[i] <- var_stat[i] + penalty
          # }
        } else { # if(!enough_obs)
          var_stat[i] <- NA
        }
        i <- i + 1
      } # for(z in split_vals)
    } # if(!cpp)
    
    if(all(is.na(var_stat))) {
      best_vals[j, 1] <- NA
      best_vals[j, 2] <- NA
    } else {
      best_vals[j, 1] <- split_vals[which(var_stat == max(var_stat, na.rm = TRUE))][1]
      best_vals[j, 2] <- max(var_stat, na.rm = TRUE)
      # if(dist_penalty) {
      #   best_vals[j, 3] <- var_stat[which(var_stat == max(var_stat, na.rm = TRUE))][1]
      # }
    }
    j <- j + 1
  } # for(var in split_vars)
  
  split <- list()
  if(all(is.na(best_vals[, 2]))) {
    split$pval <- Inf
    return(split)
  }
  
  best_split <- which(best_vals[, 2] == max(best_vals[, 2], na.rm = TRUE))[1]

  split$pval <- dchisq(best_vals[best_split, 2], df = predictors)
  split$name <- rownames(best_vals)[best_split]
  split$value <- best_vals[best_split, 1]

  return(split)
}


# Wrapper -----------------------------------------------------------------

#' @title Get Nodes
#' @description Wraps find_split() and looks for splits until a termination 
#' condition is reached.
#' @author DW & NK
#'
#' @param df A dataframe with named columns and rows.
#' @param split_vars Vector of names of the variables used for splitting.
#' @param formula Formula to feed to fun().
#' @param fun Function to apply. Defaults to lm().
#' @param predictors Integer indicating the dof. Defaults to 1.
#' @param min_obs Integer with minimum number of observations in a split. Defaults to 5.
#' @param max_steps Integer with the maximum number of recursions to perform. Defaults to 5.
#' @param pval Double of the pval to consider significant. Defaults to 0.05.
#' @param n_splits Integer determining how many splits should be attempted 
#' per variable (done via quantiles). Default value is 100.
#' @param verbose Logical determining whether to include and print a reason when a 
#' terminal node is reached. Defaults to TRUE.
#' @param cpp Logical indicating whether to use the C++ implementation. Defaults to
#' FALSE, as it is an experimental feature.
#' @param step Used when recursing - do not supply manually.
#' @param state Used when recursing - do not supply manually.
#' @param ... 
#'
#' @return Returns a list of lists, where terminal nodes contain the final dataframe,
#' and the path to it.
#'
get_nodes <- function(
  df, split_vars, 
  formula, fun = lm, predictors = 1,
  min_obs = 5, max_steps = 5, pval = 0.05,
  n_splits = 100, 
  verbose = TRUE, 
  # dist_mat = NULL, dist_penalty = FALSE, 
  cpp = FALSE, 
  step = 0, state = NULL, 
  ...) {
  
  split <- find_split(df, split_vars, 
                      formula, fun, predictors, 
                      min_obs = min_obs, n_splits = n_splits, 
                      # dist_mat, dist_penalty, 
                      cpp = cpp,
                      ...)
  
  if(split$pval < pval && step < max_steps) {
    if(is.null(state)) {
      state <- list(split)
    } else {
      state[[step + 1]] <- split
    }
    nodes <- splitter(df, split_var = split$name, split_val = split$value)
    step <- step + 1
    return(list(Recall(df = nodes$leq, split_vars, formula, fun, predictors, 
                       min_obs, max_steps, pval, n_splits, verbose, cpp, step, state), 
                Recall(df = nodes$gre, split_vars, formula, fun, predictors, 
                       min_obs, max_steps, pval, n_splits, verbose, cpp, step, state)))
  } else {
    if(verbose) {
      if(split$pval == Inf) {
        cat("Encountered node below minimum size.\n")
      } else if(split$pval >= pval) {
        cat("Encountered p-value of", split$pval, ".\n")
      }
      if(step >= max_steps) cat("Maximum steps performed.\n")
      return(list(df = df, node = state))
    } else {
      # Returns a plain df
      return(df)
    }
  }
}
