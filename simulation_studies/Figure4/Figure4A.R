source("R/utls.R") # load all helper functions

##############################
## Define experiments ====
##############################

## defining simulation setup 
params <- list(n=c(150, 200, 250, 300, 350, 400, 450, 500), # number of observations
               p=100, # number of features
               p_f=50, # number of factor (categorical) features
               n.cat=3, # number of balanced classes for each categorical variable
               p_nn=20, # number of non-null (i.e. important) features
               a=0.3, # signal amplitude (of each non-null feature)
               encoding="None", # (dummy) encoding?
               M=1, # number of experiments in derandomising step
               error.type="fdr", # type of error control
               level=0.2, # level at which error is controlled
               knockoff.method=c("sparseseq", "seq"), # type of knockoffs
               statistic=c("stat_glmnet"), # knockoff statistic
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
if (!file.exists("output/Figure4/")){ # if intended output subfolder doesn't exist yet ...
  dir.create(file.path("output/Figure4/")) # ... create it
}
saveRDS(results.global, file="output/Figure4/resultsglobal_4A.RDS") # save results to file


##############################
## Visualise output ====
##############################

## helpers for plotting
colour.values <- c( 'steelblue2', 'chartreuse3', 'navyblue') # custom colours


## prepare data for plotting
gg.dat <- results.global %>%
  mutate(method=factor(ifelse(knockoff.method=="seq",
                              "vanilla sequential",
                              ifelse(knockoff.method=="sparseseq",
                                     "sparse sequential",
                                     knockoff.method)
                              ),
         levels=c( "second-order MX", "vanilla sequential", "sparse sequential"))
  ) # make knockoff method labels easier to read and order them as desired




## create first subplot
pp1 <- ggplot(data=gg.dat, aes(x=as.numeric(n), y=as.numeric(TPP), group=method, colour=method)) +
  xlab("") +
  ylab("TDP") +
  stat_summary(fun.data = "mean_se", aes(colour=method), size=0.2) +
  stat_summary(fun.data = "mean_se", aes(colour=method, group=method), geom="line", size=0.2) +
  theme(legend.position="bottom", legend.text=element_text(size=12), legend.title=element_text(size=12)) +
  guides(color=guide_legend(nrow=1, title.position = "left", title.vjust = 0.7)) +
  scale_colour_manual(values=colour.values, name='knockoff generation algorithm') +
  scale_fill_manual(values=colour.values)+
  scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), minor_breaks = NULL, limits=c(0, 1)) +
  mytheme

## create second subplot
pp2 <- ggplot(data=gg.dat, aes(x=as.numeric(n), y=as.numeric(FDP), group=method, colour=method)) +
  xlab("sample size") +
  ylab("FDP") +
  stat_summary(fun.data = "mean_se", aes(colour=method), size=0.2) +
  stat_summary(fun.data = "mean_se", aes(colour=method, group=method), geom="line", size=0.2) +
  theme(legend.position="bottom", legend.text=element_text(size=12), legend.title=element_text(size=12)) +
  guides(color=guide_legend(nrow=1, title.position = "left", title.vjust = 0.7)) +
  scale_colour_manual(values=colour.values) +
  scale_fill_manual(values=colour.values)+
  scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), minor_breaks = NULL, limits=c(0, 1)) +
  geom_hline(yintercept=0.2, linetype="dashed", color = "black") +
  mytheme

## create third subplot
pp3 <- ggplot(data=gg.dat, aes(x=as.numeric(n), y=as.numeric(time), group=method, colour=method)) +
  xlab("") +
  ylab("computational time / s") +
  stat_summary(fun.data = "mean_se", aes(colour=method), size=0.2) +
  stat_summary(fun.data = "mean_se", aes(colour=method, group=method), geom="line", size=0.2) +
  theme(legend.position="bottom", legend.text=element_text(size=12), legend.title=element_text(size=12)) +
  guides(color=guide_legend(nrow=1, title.position = "left", title.vjust = 0.7)) +
  scale_colour_manual(values=colour.values) +
  scale_fill_manual(values=colour.values)+
  scale_y_continuous(trans='log10') +
  annotation_logticks(sides='l') +
  mytheme

# combine the three subplots into one plot
fig4A <- grid_arrange_shared_legend(pp1, pp2, pp3) 
ggsave(file="output/Figure4/Figure4A.pdf", plot=fig4A, width=12, height=4) ## save to file (pdf)
ggsave(file="output/Figure4/Figure4A.png", plot=fig4A, width=12, height=4) ## save to file (png)


