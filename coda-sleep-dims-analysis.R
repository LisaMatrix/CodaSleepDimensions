#### Packages #### 

library(compositions)
library(zCompositions)

library(foreign)
library(haven)

library(codaredistlm)

library(dplyr)
library(ggplot2)

library(report)

# citation("codaredistlm")


#### Datasets ####

chkpnt_loc <- "location/of/data"
chkpnt_file <- "chkpnt_data.sav"
(chkpnt_loc_and_file <- paste0(chkpnt_loc, "/", chkpnt_file))
# path.expand(normalizePath(chkpnt_loc, winslash  = "/"))
chkpnt <- read_sav(chkpnt_loc_and_file)
head(chkpnt)


lsac_loc <- "location/of/data"
lsac_file <- "lsac_data.sav"
(lsac_loc_and_file <- paste0(lsac_loc, "/", lsac_file))
lsac <- read_sav(lsac_loc_and_file)


#### working dataset ###

data <- merge(chkpnt, lsac, by = "hicid")

### Data management ###

# exclude those without valid accelerometry
dataset1<- subset.data.frame(data, fcvalidwkwe == 1)

# create season variable
dataset1$datetime <- paste(dataset1$fstudy)
dataset1$datetime <- strptime(dataset1$datetime, format = "%Y-%m-%d")
dataset1$month <- as.numeric(format(dataset1$datetime, "%m"))
dataset1$season <- "summer"
dataset1$season[dataset1$month > 2 & dataset1$month <= 5] <- "autumn"
dataset1$season[dataset1$month > 5 & dataset1$month <= 8] <- "winter"
dataset1$season[dataset1$month > 8 & dataset1$month <= 11] <- "spring"
dataset1$season <- factor(dataset1$season, levels = c("summer", "spring", "winter", "autumn"))
summary(dataset1$season)



dataset2 <- dataset1 %>% dplyr::select(hicid, zf02m1cp, season, fsep2, fcpds,
                                       fpcodenw, fch23c04d, fca52sdtm,
                                       fca52ligtm,fca52mvtm, fca52slmp,
                                       fca52bedtm, fca52guptm,fca52slcon,
                                       fca52sltm, fca52sleff, fch23c07d)


##Rename variables
dataset2$MVPA <- dataset2$fca52mvtm
dataset2$Sleep <- dataset2$fca52sltm
dataset2$Sedentary <- dataset2$fca52sdtm
dataset2$LPA <- dataset2$fca52ligtm

nrow(dataset)
dataset <- na.omit(dataset2)
nrow(dataset) # check the change in rows after NA deletions


## check zero values in dataset  
dataset$comp4 <- dataset %>% dplyr::select(fca52sltm, fca52sdtm, fca52mvtm, fca52ligtm)
missingSummary(dataset$comp4) # no zeros

#### Analysis #####

### set variables to use in codaredistlm package
dataset$TroubledSleep <- as.numeric(dataset$fch23c04d)
delta_mins <- seq(-30, 30, by = 15)
comp_names <- c("MVPA", "Sleep", "Sedentary", "LPA")
cov_names <- c("fsep2", "zf02m1cp", "fcpds", "season")
outc_name <- "TroubledSleep"

### unadjusted regression analysis with ilrs ###

deltacomp_udf_TS <-
  predict_delta_comps(
    dataf = dataset,
    y = outc_name,
    comps = comp_names,
    deltas = delta_mins / (24 * 60), # changes as proportion of day
    comparisons = "prop-realloc",
    alpha = 0.05 / 4 # bonferonni adjusted alpha
  )
deltacomp_udf_TS


### adjusted regression analysis with ilrs ###

deltacomp_adf_TS <-
  predict_delta_comps(
    dataf = dataset,
    y = outc_name,
    comps = comp_names,
    covars = cov_names, # include covariates
    deltas = delta_mins / (24 * 60), # changes as proportion of day
    comparisons = "prop-realloc",
    alpha = 0.05 / 4 # bonferonni adjusted alpha
  )
deltacomp_adf_TS

deltacomp_adf_TS <- as_tibble(deltacomp_adf_TS) %>% mutate(outcome = "Troubled Sleep")

### Figure: 1:1 reallocations of adjusted predictions
gp <- 
  ggplot(deltacomp_adf_TS) +
  geom_vline(xintercept = 0, col = "grey60") + 
  geom_hline(yintercept = 0, col = "grey60") + 
  geom_line(aes(x = delta * 1440, y = delta_pred, col = `comp+`)) + 
  geom_point(aes(x = delta * 1440, y = delta_pred, col = `comp+`), size = 1) + 
  geom_ribbon(aes(x = delta * 1440, ymin = ci_lo, ymax = ci_up, fill = `comp+`), alpha = 0.3) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(
    x = paste0("Change/delta in composition"), 
    y = paste0("Predicted change in outcome")
  )  + 
  facet_grid(outcome ~ `comp+`, scales = "free_y", labeller = label_parsed) +
  scale_color_colorblind() +
  scale_fill_colorblind() 
gp


### compare overall model and test for variable significance ###

str(deltacomp_adf_TS) # look at structure of codaredistlm returned object
X <- attr(deltacomp_adf_TS, "dataf") # extract data.frame of model

# fit intercept only model
(int_only_form <- paste(outc_name, "~ 1"))
lm_int_only <- lm(as.formula(int_only_form), data = X)
summary(lm_int_only)

# fit intercept + covariates model
(no_ilr_form <- paste(int_only_form, "+", paste(cov_names, collapse = " + ")))
lm_no_comp <- lm(as.formula(no_ilr_form), data = X)
summary(lm_no_comp)

# fit intercept + ilrs model
(ilr_only_form <- paste(int_only_form, "+",  paste(paste0("ilr", 1:3), collapse = " + ")))
lm_ilr_only <- lm(as.formula(ilr_only_form), data = X)
summary(lm_ilr_only)

# fit intercept + covariates + ilrs model
(ilr_form <- paste(no_ilr_form, "+", paste(paste0("ilr", 1:3), collapse = " + ") )) 
lm_comp <- lm(as.formula(ilr_form), data = X)
summary(lm_comp)

# full model summary(should be same as above)
summary(attr(deltacomp_adf_TS, "lm"))

# are the compositions/ilrs significant? (adjusted: all covariates in both models)
anova(lm_no_comp, lm_comp)

# are the compositions/ilrs significant? (unadjusted: NO covariates in both models)
anova(lm_int_only, lm_ilr_only)

# print summary of results
report(lm_comp)

