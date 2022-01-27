
# Jan 2022 Revision Requested to use same n for 
# univariate and multivariable analyses. 
# Decided to use n of 14987 (filthosp). So we need to rerun
# univariate analyses with this data frame

#get dimensions
dim(filthosp)

#re-run table 1 

## due to glmer algo, that ran the multiv analses, ~500 observations were dropped. 

## There isn't a way to use built-in model functionality do find these.
## So write code to identify these rows, and subset to get the 14,397 n.

revision <- filthosp[!(Reduce("|", lapply(filthosp[all.vars(fit2@call$formula)], is.na))), ]
dim(revision)


## Rerun table1 code on revision dataframe that has the 14397 n

