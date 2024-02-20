source("R/utls.R") # load all helper functions


##############################
## Define experiments ====
##############################

## defining simulation setup 
params <- list(n=250, # number of observations
               p = 100, # number of features
               p_f=0, # number of factor (categorical) features
               p_nn=c(1:20), # number of non-null (i.e. important) features
               a=0.3, # signal amplitude (of each non-null feature)
               encoding="None", # (dummy) encoding
               M=1, # number of experiments in derandomising step
               error.type=c("fdr", "pfer"), # type of error control
               level=c(0.2, 1), # level at which error is controlled
               knockoff.method=c("sparseseq"), # type of knockoffs
               statistic=c("stat_glmnet"), # knockoff statistic
               seed=c(1:100) # seeds (one seed = one independent data set)
)

## generate all possible combinations of elements in params
experiments <- expand.grid(params, stringsAsFactors=FALSE)

## remove unwanted combinations from simulation setups
experiments <- experiments %>%
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

## save results to file
if (!file.exists("output/")){ # if output subfolder doesn't exist yet ...
  dir.create(file.path("output/")) # ... create it
}
if (!file.exists("output/Figure5/")){ # if intended output subfolder doesn't exist yet ...
  dir.create(file.path("output/Figure5/")) # ... create it
}
saveRDS(results.global, file="output/Figure5/resultsglobal.RDS") # save results to file

##############################
## Visualise output ====
##############################

## helpers for plotting
colour.values <- c('darkslategray', 'aquamarine3','steelblue2', 'navyblue') # custom colours

plotfct <- function(x) x # define helper function for plotting

## prepare data for plotting
gg.dat <- results.global %>%
  mutate("countP" = as.numeric(TP) + as.numeric(FP)) %>% # number (count) of positives (= true positives + false positives)
  select(countP, p_nn, error.type) %>%
  mutate(labelling.dummy = "1") # hack to set manual colour later on

gg.dat.summary <- gg.dat %>%
  group_by(across(everything())) %>%
  mutate(frequency= n() / 100) # add frequency column

## create plot
pp <- ggplot(data=gg.dat.summary, aes(x=as.numeric(p_nn), y=countP)) +
  facet_wrap(~error.type)+
  geom_count(aes(size=frequency, colour=frequency, alpha=0.5), alpha=0.75) +
  scale_size(range=c(0,5), name="Relative frequency",  breaks = c(0.0,  0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_colour_gradientn(colours=colour.values, name="Relative frequency", guide="legend",  breaks = c(0.0,  0.2, 0.4, 0.6, 0.8, 1.0)) +
  geom_ribbon(data = subset(gg.dat.summary, error.type == "fdr"),
              stat = 'function', fun = plotfct,
              mapping = aes(ymin = 0, ymax = 5, xmin=-1, xmax=21),
              fill = 'gray', alpha = 0.5) +
  scale_x_continuous(expand=c(0,0), limits = c(0,21)) +
  xlab("# truly relevant variables") +
  ylab("# selected variables") +
  mytheme
ggsave(file="output/Figure5/Figure5.pdf", width=6, height=4) ## save to file (pdf)
ggsave(file="output/Figure5/Figure5.png", width=6, height=4) ## save to file (png)
print(pp)


