## load required packages
library(knockofftools)
library(dplyr)
library(tidyverse)
library(bnlearn)
library(caret)
library(recipes)
library(tictoc)
library(gtools)

source("R/utls_plotting.R")




#' Generate variables from a randomly generated (sparse) Bayesian network, using the \code{bnlearn} package.
#'
#' @param p integer, number of variables 
#' @param n integer, number of observations
#' @param max.degree integer, maximum number of degrees of graphical model
#' @param parent.pred flaot, predictive strength of parent nodes for their children nodes. This is assumed to be the same for all parent nodes.
#'
#' @return list containing the Bayesian network (as entry \code{bn}), its graph (as entry \code{graph}), and the generated data (as entry \code{X})
#' @export
#'
#' @examples
#' X <- generate_X_MGM(p=50, n=100, max.degree=5, parent.pred=0.1)$X
generate_X_MGM <- function(p, n=100, max.degree=5, parent.pred=0.1){
  require(bnlearn)

  names <- sapply(c(1:p), function(n) paste0("X", n))

  ## generate graph structure (nodes and edges)
  graph <- random.graph(names, num=1, max.degree=max.degree ,method="melancon")
  plot(graph)

  ## generate conditional distributions
  cds <- lapply(names, function(x) "x" = list(coef=c("(Intercept)"=0,
                                                     setNames(rep(parent.pred,length(graph$nodes[[x]]$parents)), graph$nodes[[x]]$parents)
                                                     ),
                                              sd=1))
  names(cds) <- names

  ## combine graph and conditional distributions into network
  bn <- custom.fit(graph, cds)

  ## generate data from network
  X <- rbn(bn, n=n)

  return (list("bn"=bn,
               "graph"=graph,
               "X"=X))
}



#' Categorise (=dichotomise, trichotomise, ...) a subset of variables in X.
#'
#' @param X data.frame or tibble of data
#' @param k.cat list indicating the number of (balanced) categories to be generated. Example k.cat=c(3,4,3) means that 2 variables will be trichotomised and 1 variable will be split into 4 balanced classes. Note that the identity of the categorised variables is random. 
#'
#' @return X with a subset of its variables (columns) categorised
#' @export
#'
#' @examples
#' X.continuous <- generate_X_MGM(p=50, n=100)$X
#' X <- categorise_subset_of_features(X.continuous, c(3,3,3,4,4))
categorise_subset_of_features <- function(X, k.cat){
  p = ncol(X)

  if (length(k.cat) > 0){
    i.cat <- sample(1:p, length(k.cat), replace=FALSE) # indices of covariates that shall be categorised
    for (i in c(1:length(i.cat))){
      X[,i.cat[i]] = as.factor(quantcut(X[,i.cat[i]], q=k.cat[i])) # create factor variable using the quantiles of a continuous variable
      levels(X[,i.cat[i]]) = paste0(sample(letters, 1), c(1:k.cat[i])) # add a letter to the names of the categories to ensure that they are not by mistake interpreted as numeric features
    }
  }

  return(X)
}


#' Create (and execute) recipe for pre-processing the data.
#'
#' @param X data.frame or tibble
#' @param freq.threshold passed to step_other. Default is 0.1
#' @param freq_cut passed to step_nzv. Default is 9/1.
#'
#' @return list of pre-processed data (as entry \code{X.prepped}) and recipe for pre-processing (as entry \code{recipe.prepped}).
#' @export
#'
#' @examples
#' X.continuous <- generate_X_MGM(p=50, n=100)$X
#' X <- categorise_subset_of_features(X.continuous, rep(10,30))
#' X.prepped <- preprocess_data(X)$X.prepped
preprocess_data <- function(X, freq.threshold=0.1, freq_cut=9/1){
  rec <- recipe(~., data=X) %>%
    step_other(all_nominal(), threshold=freq.threshold) %>% # combine rare categories to 'other'
    step_nzv(all_nominal(), freq_cut=freq_cut)  # remove highly unbalanced categorical variables

  rec_prepped <-  prep(rec, training = X)
  X.prepped <- bake(rec_prepped, X)

  X.prepped <- as.data.frame(X.prepped)

  ## drop factor levels that were not realised
  X.prepped <- droplevels(X.prepped)

  ## drop factor features that have only 1 level (= are non-informative)
  X.prepped <- X.prepped[,sapply(X.prepped, function(x) length(table(x))>1)]

  return(list("X.prepped" = X.prepped,
              "recipe.prepped"=rec_prepped))
}


# Normal score transform samples.
ns.transform <- function(y) {

  require("tidyverse")

  # Normal Q-Q plot:
  ns <- qqnorm(y, plot.it=FALSE)
  ns <- tibble::as_tibble(data.frame(x=ns$x, y=ns$y)) %>% dplyr::group_by(factor(y)) %>% dplyr::summarise(x=mean(x), y=mean(y))

  # Find the functional relationship between normal and empirical quantiles:
  ns.transform.fun <- approxfun(ns$y,ns$x, rule=2)
  yt <- ns.transform.fun(y)

  return(yt)
}


#' Generate response y as in the \code{knockofftools} package, but with the option to have the response non-linear (used for Figure 4 in the paper)
#'
#' @param X data.frame or tibble
#' @param p_nn integer, number of non-null features
#' @param a float, signal strength
#' @param bool_linear boolean, indicating whether the response should depend linearly on the p_nn predictors (y = a(X1 + X2 + ...)) or non-linearly, in which case y = a(X1^2 + X2^2 + ...)
#'
#' @return response
#' @export
#'
#' @examples
#' X <- generate_X_MGM(p=50, n=100)$X
#' y <- generate_y(X, p_nn=20, a=3)
generate_y <- function(X, p_nn, a, bool_linear=TRUE) {


  mm <- model.matrix(~., data=X)
  x <- mm[,-1]

  indices_vars <- attributes(mm)$assign[-1]


  n <- nrow(x)
  p <- ncol(x)

  # Standardize design matrix to zero mean, "variance" one
  x_centered <- apply(x, 2, function(x) x - mean(x))
  x <- apply(x_centered, 2, function(x) x / sqrt(mean(x^2)))

  first_occurence <-  c(0,indices_vars)[-1] - c(0,indices_vars)[-length(indices_vars)] # prepend indices_vars with a zero such that we can compare each entry with its predecessor to evaluate whether it's the first occurance of that value in the list
  beta <- a * (indices_vars <= p_nn) * first_occurence

  if (bool_linear){ # if we want y to be of the following form: y = a(X1 + X2 + ...); where a is a scalar
    mu <- as.numeric(x %*% beta)
  } else { # if we want y to be of the following form: y = a(X1^2 + X2^2 + ...); where a is a scalar
    mu <- as.numeric(x^2 %*% beta)
  }

  y <- mu + rnorm(n)

  return(y)

}


#' Run a single experiment. Intended to be passed to clustermq for parallel computation, but can also be used as standalone.
#'
#' @param n integer, number of observations
#' @param p integer, number of variables in the candidate set
#' @param p_f integer, number of factor (categorical) variables in the canddiate set
#' @param n.cat integer, number of (balanced) classes of categorical variables in candidate set
#' @param p_nn integer, number of non-null features (= cardinality of S)
#' @param a float, signal strength
#' @param bool_linear boolean indicating whether the response should be a linear function of the variables in S. Default is TRUE.
#' @param level numeric, nominal level of type-I errors to be controlled
#' @param error.type string, kind of type-I errror control
#' @param knockoff.method string, name of knockoff generation method. This is passed to \code{knockofftools::knockoff.statistic}
#' @param statistic string, name of knockoff statistic. This is passed to \code{knockofftools::knockoff.statistic}
#' @param M integer, number of derandomisations
#' @param seed integer, seed that will be passed to worker (one seed = one independent data set)
#' @param encoding string, only if encoding=="dummy", data in X are dummy encoded before feeding them into the knockof framework. Default is "None".
#' @param cov_type="MGM" string, indicates how data are generated, in analogy to the cov_type of \code{knockofftools}. The default is "MGM".
#' @param MGM.max.degree, integer, maximum number of connections in graphical model (relevant only if cov_type=="MGM"). The default is 10.
#' @param parent.pred, flaot, ~ predictive strength of parent nodes (relevant only if cov_type=="MGM"). The default is 0.1.
#' @param rho float, for cov_type %in% c(cov_ar1, cov_equi), see \code{knockofftools} package. The default is 0.5

#' @return
#' @export
#'
worker_fct <- function(n, # number of observations
                       p, # number of variables in the candidate set
                       p_f, # number of factor (categorical) variables in the canddiate set
                       n.cat=3, # number of (balanced) classes of categorical variables in candidate set
                       p_nn, # number of non-null features (= cardinality of S)
                       a, # signal strength
                       bool_linear=TRUE, # boolean of whether the response should be a linear function of the variables in S
                       level, # nominal level of type-I errors to be controlled
                       error.type, # kind of type-I errror control
                       knockoff.method, # knockoff generation method. This is passed to \code{knockofftools::knockoff.statistic}
                       statistic, # knockoff statistic. This is passed to \code{knockofftools::knockoff.statistic}
                       M, # number of derandomisations
                       seed, # seed that will be passed to worker
                       encoding="None", # only if encoding=="dummy", data in X are dummy encoded before feeding them into the knockof framework
                       cov_type="MGM", # indicates how data are generated, in analogy to the cov_type of \code{knockofftools}
                       MGM.max.degree=10, # maximum number of connections in graphical model (relevant only if cov_type=="MGM")
                       parent.pred=0.1, # ~ predictive strength of parent nodes (for cov_type=="MGM")
                       rho=0.5 # for cov_type %in% c(cov_ar1, cov_equi)
                       ){

  ## load required packages to worker
  if (exists("current_lib_paths")) {
    .libPaths(current_lib_paths)
  }
  library(knockofftools)
  library(dplyr)
  library(tidyverse)
  library(bnlearn)
  library(recipes)
  library(gtools)

  ## set seed to make result reproducible
  set.seed(seed, kind="L'Ecuyer-CMRG", normal.kind = "Inversion", sample.kind = "Rejection")


  if (cov_type=="MGM"){
    ## generate design matrix
    graph <- generate_X_MGM(p=p, # number of features
                            n=n, # number of observations
                            max.degree=MGM.max.degree, #maximal degree of each node
                            parent.pred=parent.pred # ~ predictive strength of parent nodes (for cov_type=="MGM")
    )
    X <- as.data.frame(graph$X)
    narcs = narcs(graph$bn) # number of arcs in graph
  } else{
    X <- as.data.frame(generate_X(n=n, p=p, p_b=0, cov_type = cov_type, rho = rho)) # note on p_b=0: categorisation will be done separately later (see below)
    narcs = NULL
  }

  ## simulate response and (possibly) categorise (some) feature

  ## categorise (some) features
  X <- categorise_subset_of_features(X, k.cat=rep(n.cat, p_f)) # (p_f: number of factor variables)

  ## generate response
  Y <- generate_y(X, p_nn=p_nn, a=a, bool_linear=bool_linear)


  ## preprocess data
  X <- preprocess_data(X, freq.threshold=0.1)$X.prepped
  p=ncol(X)
  var.names <- colnames(X)


  ## dummy encode if indicated
  if (encoding=="dummy"){
    mm <- model.matrix(~., data=X)
    X.dummy <- as.data.frame(mm[,-1])

    indices_vars <- attributes(mm)$assign[-1]

    # logical matrix that maps columns (of model.matrix) to variable indices:
    cols_to_vars <- outer(indices_vars, 1:max(indices_vars), function(x,y) x==y)


    t1 <- Sys.time() # time stamp to evaluate performance

    ## estimate knockoff statistic
    W <- knockoff.statistics(y=Y, X=X.dummy, knockoff.method=knockoff.method, M=M, statistic=statistic)


  } else{ # not dummy-encoded

    t1 <- Sys.time() # time stamp to evaluate performance

    ## estimate knockoff statistic
    W <- knockoff.statistics(y=Y, X=X, knockoff.method=knockoff.method, M=M, statistic=statistic)
  }


  ## select variables with guarantee on type-I error control
  S <- variable.selections(W, level=level, error.type=error.type)
  t2 <- Sys.time() # time stamp to evaluate performance

  if (encoding=="dummy"){
    ## combine selection for dummy encoded features by selecting a categorical feature if at least one of its categories was identified as important
    selected <- apply(S$selected,2, function(x) apply(x*cols_to_vars, 2, max))
    rownames(selected) = var.names


    # Perform the final selection
    thres=0.5
    if (error.type == 'pfer'| error.type == 'kfwer') {
      stable.variables = which(rowMeans(selected)>thres)
    } else if (error.type == 'fdr'){
      stable.variables = multi_select(S = selected, trim = thres)
    }

    S <- list(selected=selected, stable.variables=var.names[stable.variables])
  }



  ## summarise results
  result = c("n" = n,
             "encoding" = encoding,
             "TPP" = eval_tpp(S$stable.variables, var.names[1:(p_nn)]),
             "TP" = sum(S$stable.variables %in% var.names[1:(p_nn)]),
             "FDP" = eval_fdp(S$stable.variables, var.names[(p_nn+1):ncol(X)]),
             "FP" = sum(S$stable.variables %in% var.names[(p_nn+1):ncol(X)]),
             "p"=ncol(X),
             "parent.pred"=parent.pred,
             "MGM.max.degree"=MGM.max.degree,
             "narcs"= narcs,
             "cov_type"=cov_type,
             "rho"=rho,
             "p_nn"=p_nn,
             "p_f"=p_f,
             "n.cat"=n.cat,
             "a"=a,
             "bool_linear"=bool_linear,
             "knockoff.method"=knockoff.method,
             "statistic"=statistic,
             "error.type"=error.type,
             "level"=level,
             "time"=difftime(t2, t1, units = "s"),
             "seed" = seed
  )

  return (result)

}




