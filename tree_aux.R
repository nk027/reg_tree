# Regression tree handling
#
# Author: DW
# Editor: NK


# Stuff -------------------------------------------------------------------

do_reg <- function(node, fun, formula, ...) {
  out <- fun(formula, data = node$df, ...)
  return(list(coefs = out))
}

plant_tree <- function(nodes, fun = lm, formula, ...) {
  leq <- nodes[[1]]
  gre <- nodes[[2]]
  
  term_leq <- !is.null(names(leq))
  term_gre <- !is.null(names(gre))
  
  if(term_leq & term_gre) {
    out_leq <- do_reg(leq, fun, formula, ...)
    out_leq$nodes <- leq$node
    out_gre <- do_reg(gre, fun, formula, ...)
    out_gre$nodes <- gre$node
    return(list(out_leq, out_gre))
  } else if(term_leq) {
    out_leq <- do_reg(leq, fun, formula, ...)
    out_leq$nodes <- leq$node
    return(list(out_leq, Recall(gre, fun, formula, ...)))
  } else if(term_gre) {
    out_gre <- do_reg(gre, fun, formula)
    out_gre$nodes <- gre$node
    return(list(out_gre, Recall(leq, fun, formula, ...)))
  } else {
    return(list(Recall(leq, fun, formula, ...), 
                Recall(gre, fun, formula, ...)))
  }
}

l2df <- function(l, ...) {
  return(data.frame(matrix(unlist(l), ...), stringsAsFactors = FALSE))
}

node_summary <- function(node, grp) {
  nod <- l2df(node$node, ncol = 3, byrow = TRUE)
  grp <- l2df(grp, ncol = 1)
  colnames(grp) <- "direction"
  colnames(nod) <- names(node$node[[1]])
  rownames(nod) <- paste("level:", rownames(nod))
  nod <- data.frame(cbind(nod, grp))
  return(nod)
}

simplify_nodes <- function(nodes, level = 1, grp = NULL) {
  leq <- nodes[[1]]
  gre <- nodes[[2]]
  
  if(is.null(grp)) {
    grpx <- "gre"
  } else {
    grpx <- list(grp, "gre")
  }
  if(is.null(grp)) {
    grpy <- "leq"
  } else {
    grpy <- list(grp, "leq")
  }
  
  term_leq <- !is.null(names(leq))
  term_gre <- !is.null(names(gre))
  
  if(term_leq & term_gre) {
    return(list(node_summary(leq, grp = grpy),
                node_summary(gre, grp = grpx)))
  } else if(term_leq) {
    level <- level + 1
    return(list(node_summary(leq, grp = grpy),
                Recall(gre, level = level, grp = grpx)))
  } else if(term_gre) {
    level <- level + 1
    return(list(node_summary(gre, grp = grpx),
                Recall(leq, level = level, grp = grpy)))
  } else {
    level <- level + 1
    return(list(Recall(leq, level = level, grp = grpy), 
                Recall(gre, level = level, grp = grpx)))
  }
}

untree <- function(nodes, simplify = FALSE) {
  out <- list()
  lumberjack <- function(nodes) {
    leq <- nodes[[1]]
    gre <- nodes[[2]]
    term_leq <- !is.null(names(leq))
    term_gre <- !is.null(names(gre))
    parent <- parent.frame()
    pos <- length(parent$out) + 1
    if(term_leq & term_gre) {
      parent$out[[pos]] <- leq
      parent$out[[pos + 1]] <- gre
    } else if(term_leq) {
      parent$out[[pos]] <- leq
      Recall(gre)
    } else if(term_gre) {
      parent$out[[pos]] <- gre
      Recall(leq)
    } else {
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

make_candidates <- function(nodes) {
  plan_node <- function(node) {
    combine_node <- function(node_row) {
      dir <- ifelse(node_row[4] == "leq", "<=", ">")
      condition <- paste(node_row[2], dir, node_row[3], collapse = "")
      return(condition)
    }
    apply(node, 1, combine_node)
  }
  plan <- lapply(nodes, plan_node)
  return(plan)
}

cumPaste <- function(vec, collapse = NULL) {
  return(sapply(vec, function(x)paste(vec[1:which(vec == x)], collapse)))
}

fix_plan <- function(plan) {
  he_fx <- function(pp) {
    require(stringr)
    str <- pp[[length(pp)]]
    spstr <- stringr::str_split(str, "&", simplify = TRUE)
    final <- spstr[ncol(spstr)]
    final <- if(stringr::str_detect(final, ">")) {
      stringr::str_replace(final, ">", "<=")
    } else if(stringr::str_detect(final, "<=")) {
      stringr::str_replace(final, "<=", ">")
    }
    spstr[ncol(spstr)] <- final
    pp_new <- c(pp, paste0(spstr, collapse = "&"))
    return(pp_new)
  }
  return(lapply(plan, function(x) he_fx(x)))
}

make_plan <- function(candidates) {
  plan <- lapply(candidates, cumPaste, collapse = " & ")
  terminals <- lapply(plan, function(pp) pp[[length(pp)]])
  plan <- fix_plan(plan)
  terminals <- do.call("c", terminals)
  plan <- do.call("c", plan)
  plan <- plan[!duplicated(plan)]
  terminals <- terminals[!duplicated(terminals)]
  names(plan) <- NULL
  return(list("plan" = plan, "terminal" = terminals))
}

get_data <- function(data, plan) {
  split_data  <- lapply(plan, function(cond) subset(data, eval(parse(text = cond))))
  names(split_data) <- paste0("df", 1:length(plan))
  for(pp in seq_along(plan)) {
    attr(split_data[[pp]], which = "split") <- plan[[pp]]
  }
  return(split_data)
}

# Get the terminal nodes' dataframes
nodes2dfs <- function(nodes, dat, terminal = TRUE) {
  simp <- simplify_nodes(nodes)
  untr <- untree(simp)
  cand <- make_candidates(untr)
  plan <- make_plan(cand)
  if(terminal) {
    plan <- plan$terminal
  } else {
    plan <- plan$plan
  }
  splt_data <- get_data(data = dat, plan)
  return(splt_data)
}

lm_list <- function(dfs, formula) {
  regs <- lapply(dfs, function(x) lm(formula = formula, data = x))
  splits <- lapply(dfs, function(x) attr(x, which = "split"))
  names(regs) <- splits
  return(regs)
}

print.node <- function(node, level, grp) {
  require(igraph)
  nod <- l2df(node$nodes, ncol = 3, byrow = TRUE)
  grp <- l2df(grp, ncol = 1)
  colnames(nod) <- names(node$nodes[[1]])
  rownames(nod) <- paste("level:", rownames(nod))
  nod <- cbind(nod, grp)
  return(nod)
}

get_last <- function(node, level) {
  if(is.null(names(node))) {
    return(Recall(node[[1]]))
  }
  if(is.null(names(node$nodes))) {
    ret <- node$nodes[[2]]
    ret <- ret$value
  } else {
    ret <- node$nodes$value
  }
  return(ret)
}

summary.tree <- function(tree, level = 1, grp = NULL) {
  cat("\n")
  cat(ifelse(level == 1, "Root", paste("Split", level)))
  cat("\n\n")
  leq <- tree[[1]]
  gre <- tree[[2]]
  
  if(is.null(grp)) {
    grpx <- "gre"
  } else {
    grpx <- list(grp, "gre")
  }
  if(is.null(grp)) {
    grpy <- "leq"
  } else {
    grpy <- list(grp, "leq")
  }
  
  term_leq <- !is.null(names(leq))
  term_gre <- !is.null(names(gre))
  if(term_leq & term_gre) {
    cat("<= ", get_last(leq, level),'\n' )
    print.node(leq, level, grp = grpy)
    cat("\n")
    cat("> ", get_last(gre, level),'\n' )
    print.node(gre, level, grp = grpx)
  } else if(term_leq) {
    cat("<= ", get_last(leq, level),'\n' )
    print.node(leq, level, grp = grpy)
    cat('\n', "> ", get_last(leq, level),":")
    level <- level + 1
    Recall(gre, level = level, grp = grpx)
  } else if(term_gre) {
    cat("> ", get_last(gre, level),'\n' )
    print.node(gre, level = level, grp = grpx)
    level <- level + 1
    Recall(leq, level = level, grp = grpy)
  } else {
    level <- level + 1
    return(list(Recall(leq, level = level, grp = grpy), 
                Recall(gre, level = level, grp = grpx)))
  }
}