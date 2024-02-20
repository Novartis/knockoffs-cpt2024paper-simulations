source("R/utls.R") # load all helper functions


##############################
## Define experiments ====
##############################

## defining simulation setup 
params <- list(n=c(250, 500), # number of observations
               p = 100, # number of features
               p_f=c(0:5)*20, # number of factor (categorical) features
               p_nn=20, # number of non-null (i.e. important) features
               a=0.3, # signal amplitude (of each non-null feature)
               encoding=c("None", "dummy"), # (dummy) encoding
               M=1, # number of experiments in derandomising step
               error.type=c("fdr"), # type of error control
               level=c(0.2), # level at which error is controlled
               knockoff.method=c("sparseseq", "seq", "mx"), # type of knockoffs
               statistic=c("stat_glmnet"), # knockoff statistic
               seed=c(1:100) # seeds (one seed = one independent data set)
)

## generate all possible combinations of elements in params
experiments <- expand.grid(params, stringsAsFactors=FALSE)

## remove unwanted combinations from simulation setups
experiments <- experiments %>%
  filter(!(knockoff.method=="mx" & encoding=="None"))  %>%
  filter(!(knockoff.method=="seq" & encoding=="dummy")) %>%
  filter(!(knockoff.method=="sparseseq" & encoding=="dummy")) %>%
  filter(!(error.type=="fdr" & level >= 1)) %>%
  filter(!(error.type=="pfer" & level<1))

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


## save results to 
if (!file.exists("output/")){ # if  output subfolder doesn't exist yet ...
  dir.create(file.path("output/")) # ... create it
}
if (!file.exists("output/Figure2")){ # if intended output subfolder doesn't exist yet ...
  dir.create(file.path("output/Figure2/")) # ... create it
}
saveRDS(results.global, file="output/Figure2/resultsglobal.RDS") # save results to file


##############################
## Visualise output ====
##############################

## helpers for plotting
colour.values <- c('darkslategray', 'steelblue2', 'chartreuse3', 'navyblue') # custom colours

facet.titles <- c("250" = "n = 250",
                  "500" = "n = 500",
                  "FDP" = "FDP",
                  "TPP" = "TPP") # custom labels / titles


## prepare data for plotting
gg.dat <- results.global %>%
  mutate(method=factor(ifelse(knockoff.method=="seq",
                              "vanilla sequential",
                              ifelse(knockoff.method=="sparseseq",
                                     "sparse sequential",
                                     ifelse(knockoff.method=="mx",
                                            "dummy MX",
                                            knockoff.method)
                              )
  ),
  levels=c( "dummy MX", "vanilla sequential", "sparse sequential"))
  ) # make knockoff method labels easier to read and order them as desired

## bring data to format needed for plotting
gg.dat <- gg.dat%>%
  dplyr::select(FDP, TPP, method, n, p_f, time, statistic) %>%
  gather(key="kpi", value="value", -c(method, n, p_f, time, statistic))  %>%
  mutate(value = as.numeric(value))

## create plot
pp <- ggplot(data=gg.dat, aes(x=as.numeric(p_f), y=as.numeric(value), group=method)) +
  facet_wrap(factor(kpi, levels=c("TPP", "FDP"))~as.numeric(n),  labeller = as_labeller(facet.titles), scales="free", ncol=2) +
  xlab("# categorical variables out of 100 variables") +
  ylab(NULL) +
  stat_summary(fun.data = "mean_se", aes(colour=method), size=0.2) +
  stat_summary(fun.data = "mean_se", aes(colour=method, group=method), geom="line", size=0.2) +
  theme(legend.position="bottom", legend.text=element_text(size=12), legend.title=element_text(size=12)) +
  guides(color=guide_legend(nrow=1, title.position = "left", title.vjust = 0.7)) +
  scale_colour_manual(values=colour.values,  name='knockoff generation algorithm',) +
  geom_hline(data=gg.dat %>%filter(kpi=="FDP"), aes(yintercept=0.2), linetype="dashed", color = "black") +
  scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), minor_breaks = NULL, limits=c(0, 1)) +
  mytheme
ggsave(file="output/Figure2/Figure2.pdf", width=8, height=8) ## save to file (pdf)
ggsave(file="output/Figure2/Figure2.png", width=8, height=8) ## save to file (png)
print(pp)



