source("R/utls.R") # load all helper functions


##############################
## Define experiments ====
##############################

## defining simulation setup 
params <- list(n=c(3:10)*50, # number of observations
               p = 100, # number of features
               p_f=0, # number of factor (categorical) features
               p_nn=20, # number of non-null (i.e. important) features
               a=0.3, # signal amplitude (of each non-null feature)
               bool_linear=c(FALSE, TRUE), # indicating whether the response should be a linear function of the variables in S, otherwise y = a(X1^2 + X2^2 + ...)
               encoding=c("None"), # (dummy) encoding?
               M=1, # number of experiments in derandomising step
               error.type=c("fdr"), # type of error control
               level=c(0.2), # level at which error is controlled
               knockoff.method=c("mx", "sparseseq", "seq"), # type of knockoffs
               statistic=c("stat_glmnet", "stat_random_forest"), # knockoff statistic
               seed=c(1:100) # seeds (one seed = one independent data set)
)

## generate all possible combinations of elements in params
experiments <- expand.grid(params, stringsAsFactors=FALSE)

## preview experiments to be carried out
head(experiments)

##############################
## Run simulations ====
##############################

## setup parallel computations
results.global <- clustermq::Q_rows(df=experiments, # df of all experiments/simulations to be carried out
                                    fun=worker_fct, # function that runs a single experiment; defined in R/utls.R
                                    const = list(), # no further constant information passed to all workers
                                    n_jobs=min(dim(experiments)[1], 200), # max n_jobs = 200 to restrict ourselves to a fair use of the HPC environment
                                    export = list(current_lib_paths = .libPaths(),
                                                  generate_X_MGM = generate_X_MGM, # export selected function to all workers (function is defined in R/utls.R)
                                                  preprocess_data = preprocess_data, # export selected function to all workers (function is defined in R/utls.R)
                                                  categorise_subset_of_features = categorise_subset_of_features, # export selected function to all workers (function is defined in R/utls.R)
                                                  generate_y = generate_y # export selected function to all workers (function is defined in R/utls.R)
                                    ), # exporting functions to all workers
                                    log_worker = FALSE,
                                    fail_on_error=TRUE,
                                    template=list(walltime = 600), # in minutes
                                    timeout = 36000 # in seconds
)

## combine results
results.global <- bind_rows(results.global)

## save results to file
if (!file.exists("output/")){ # if output subfolder doesn't exist yet ...
  dir.create(file.path("output/")) # ... create it
}
if (!file.exists("output/Figure3/")){ # if intended output subfolder doesn't exist yet ...
  dir.create(file.path("output/Figure3/")) # ... create it
}
saveRDS(results.global, file="output/Figure3/resultsglobal.RDS") # save results to file


##############################
## Visualise output ====
##############################

## helpers for plotting
colour.values <- c('cornflowerblue', 'turquoise2') # custom colours


facet.titles <- c("stat_glmnet"= "|LASSO coefficient| difference",
                  "stat_random_forest"="RF importance score difference",
                  "TRUE" = "Y ~ linear function of variables in S",
                  "FALSE" = "Y ~ U-shaped function of variables in S",
                  "TPP"="TDP",
                  "FDP"="FDP") # custom lables / titles


## prepare data for plotting
gg.dat <- results.global %>%
  mutate(method=factor(ifelse(knockoff.method=="seq",
                              "vanilla sequential",
                              ifelse(knockoff.method=="sparseseq",
                                     "sparse sequential",
                                     ifelse(knockoff.method=="mx",
                                            "second-order MX",
                                            knockoff.method)
                              )
  ),
  levels=c( "second-order MX", "vanilla sequential", "sparse sequential"))
  ) # make knockoff method labels easier to read and order them as desired

## bring data to format needed for plotting
gg.dat <- gg.dat %>%
  select(c(n, method, statistic, FDP, TPP, bool_linear)) %>%
  gather(key="kpi", value="value", -c(n, method, statistic, bool_linear)) %>%
  mutate(value = as.numeric(value))



## create plot
pp <- ggplot(data=gg.dat, aes(x=as.numeric(n), y=as.numeric(value), group=interaction(n,statistic), color=statistic, shape=method)) +
  facet_wrap(factor(kpi, levels=c("TPP", "FDP"))~ factor(bool_linear, levels=c(TRUE,FALSE)), scales="free", labeller = as_labeller(facet.titles), ncol=2 ) +
  xlab("sample size") +
  ylab("") +
  stat_summary(fun.data = "mean_se", aes(colour=interaction(statistic), group=interaction(method,statistic), shape=method), size=0.5) +
  stat_summary(fun.data = "mean_se", aes(colour=interaction(statistic), group=interaction(method,statistic), linetype=method), geom="line", size=0.8) +
  geom_hline(data=gg.dat %>%filter(kpi=="FDP"), aes(yintercept=0.2), linetype="dashed", color = "black", size=0.8) +
  theme(legend.position="bottom", legend.text=element_text(size=12), legend.title=element_text(size=12)) +
  scale_colour_manual(values=colour.values, name='knockoff statistic', labels=c(stat_glmnet="|LASSO coefficient| difference", stat_random_forest="RF importance score difference")) +
  scale_linetype_manual(values=c(4, 1,3 ), name='knockoff generation') +
  scale_shape_manual(values=c(18, 16,17), name='knockoff generation') +
  scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), minor_breaks = NULL, limits=c(0, 1)) +
  mytheme +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin())
ggsave(file="output/Figure3/Figure3.pdf", width=8, height=8) ## save to file (pdf)
ggsave(file="output/Figure3/Figure3.png", width=8, height=8) ## save to file (png)
print(pp)




