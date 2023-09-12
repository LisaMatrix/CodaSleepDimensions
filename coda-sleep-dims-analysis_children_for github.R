#### Packages #### 

library(compositions)
library(zCompositions)

library(foreign)
library(haven)

library(codaredistlm)

library(dplyr)
library(ggplot2)
library(ggthemes)
library(forcats)

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


#### working dataset ####

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
table(dataset1$season, dataset1$month)


dataset2 <- dataset1 %>% dplyr::select(hicid, zf02m1cp, season, fsep2, fcpds,
                                       fpcodenw, fch23c04d, fca52sdtm,
                                       fca52ligtm,fca52mvtm, fca52slmp,
                                       fca52bedtm, fca52guptm,fca52slcon,
                                       fca52sltm, fca52sleff, fch23c07d)


##Check correct variables for children
str(dataset1$fch23c07d)
str(dataset1$fch23c04d)
str(dataset1$zf02m1cp)

##Rename variables
dataset2$MVPA <- dataset2$fca52mvtm
dataset2$Sleep <- dataset2$fca52sltm
dataset2$Sedentary <- dataset2$fca52sdtm
dataset2$LPA <- dataset2$fca52ligtm

nrow(dataset2)
dataset <- na.omit(dataset2)
nrow(dataset) # check the change in rows after NA deletions


## check zero values in dataset  
dataset$comp4 <- dataset %>% dplyr::select(fca52sltm, fca52sdtm, fca52mvtm, fca52ligtm)
missingSummary(dataset$comp4) # no zeros


##wear time
mean(dataset$fca52sltm+ dataset$fca52sdtm+ dataset$fca52mvtm+ dataset$fca52ligtm)
sd(dataset$fca52sltm+ dataset$fca52sdtm+ dataset$fca52mvtm+ dataset$fca52ligtm)

#### Descriptive table####

##need to get BMI
dataset2b<-dataset1 %>% dplyr::select (hicid, fcbmizc, fcpdscat)

##need to remove comp4
dataset2c<-dataset%>% dplyr::select(!comp4)


###merge datasets with variables of interest
Table_dd<-merge(dataset2b, dataset2c, by= "hicid")

###assign variable types
Table_child <- Table_dd %>% dplyr::select (!hicid)
Table_child$zf02m1cp <-as.factor(Table_child$zf02m1cp)
Table_child$fch23c04d <-as.factor(Table_child$fch23c04d)
Table_child$fch23c07d <-as.factor(Table_child$fch23c07d)
Table_child$fcpds  <-as.numeric(Table_child$fcpds)
Table_child$fcpdscat  <-as.factor(Table_child$fcpdscat)


##Create table
Table_c<-tableone::CreateTableOne(data=Table_child)
Table_c


#### Analysis #####
is.numeric(dataset$fcpds)
is.numeric(dataset$zf02m1cp)
dataset$zf02m1cp<-as.factor(dataset$zf02m1cp)
is.numeric(dataset$fsep2)
is.numeric(dataset$season)

####Troubled Sleep####

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

ts_adf <- as_tibble(deltacomp_adf_TS) %>% mutate(outcome = "Troubled Sleep")
ts_adf_fig1 <- as_tibble(deltacomp_adf_TS) %>% mutate(outcome = "TroubledSleep")

### Figure: 1:1 reallocations of adjusted predictions
gp <- 
  ggplot(ts_adf_fig1) +
  geom_vline(xintercept = 0, col = "grey60") + 
  geom_hline(yintercept = 0, col = "grey60") + 
  geom_line(aes(x = delta * 1440, y = delta_pred, col = `comp+`)) + 
  geom_point(aes(x = delta * 1440, y = delta_pred, col = `comp+`), size = 1) + 
  geom_ribbon(aes(x = delta * 1440, ymin = ci_lo, ymax = ci_up, fill = `comp+`), alpha = 0.3) + 
  theme_bw() + 
  theme(
    legend.position = "none",
    text = element_text(size = 20),
    axis.title.x =element_text(size = 4)
  ) + 
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

### Model 1: ilr significance
# are the compositions/ilrs significant? (unadjusted: NO covariates in both models)
anova(lm_int_only, lm_ilr_only)

### Model 2: ilr significance
# are the compositions/ilrs significant? (adjusted: all covariates in both models)
anova(lm_no_comp, lm_comp)

# print summary of results
report(lm_comp)

####Tired####

### set variables to use in codaredistlm package
dataset$Tired <- as.numeric(dataset$fch23c07d)
delta_mins <- seq(-30, 30, by = 15)
comp_names <- c("MVPA", "Sleep", "Sedentary", "LPA")
cov_names <- c("fsep2", "zf02m1cp", "fcpds", "season")
outc_name <- "Tired"

### unadjusted regression analysis with ilrs ###

deltacomp_udf_Tired <-
  predict_delta_comps(
    dataf = dataset,
    y = outc_name,
    comps = comp_names,
    deltas = delta_mins / (24 * 60), # changes as proportion of day
    comparisons = "prop-realloc",
    alpha = 0.05 / 4 # bonferonni adjusted alpha
  )
deltacomp_udf_Tired


### adjusted regression analysis with ilrs ###

deltacomp_adf_Tired <-
  predict_delta_comps(
    dataf = dataset,
    y = outc_name,
    comps = comp_names,
    covars = cov_names, # include covariates
    deltas = delta_mins / (24 * 60), # changes as proportion of day
    comparisons = "prop-realloc",
    alpha = 0.05 / 4 # bonferonni adjusted alpha
  )
deltacomp_adf_Tired

tired_adf <- as_tibble(deltacomp_adf_Tired) %>% mutate(outcome = "Tired")

### Figure: 1:1 reallocations of adjusted predictions
gp <- 
  ggplot(tired_adf) +
  geom_vline(xintercept = 0, col = "grey60") + 
  geom_hline(yintercept = 0, col = "grey60") + 
  geom_line(aes(x = delta * 1440, y = delta_pred, col = `comp+`)) + 
  geom_point(aes(x = delta * 1440, y = delta_pred, col = `comp+`), size = 1) + 
  geom_ribbon(aes(x = delta * 1440, ymin = ci_lo, ymax = ci_up, fill = `comp+`), alpha = 0.3) + 
  theme_bw() + 
  theme(
    legend.position = "none",
    text = element_text(size = 20),
    axis.title.x =element_text(size = 4)
  ) + 
  labs(
    x = paste0("Change/delta in composition"), 
    y = paste0("Predicted change in outcome")
  )  + 
  facet_grid(outcome ~ `comp+`, scales = "free_y", labeller = label_parsed) +
  scale_color_colorblind() +
  scale_fill_colorblind() 
gp
### compare overall model and test for variable significance ###

str(deltacomp_adf_Tired) # look at structure of codaredistlm returned object
X <- attr(deltacomp_adf_Tired, "dataf") # extract data.frame of model

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

### Model 1: ilr significance
# are the compositions/ilrs significant? (unadjusted: NO covariates in both models)
anova(lm_int_only, lm_ilr_only)

### Model 2: ilr significance
# are the compositions/ilrs significant? (adjusted: all covariates in both models)
anova(lm_no_comp, lm_comp)

# print summary of results
report(lm_comp)


####Sleep Efficiency####

### set variables to use in codaredistlm package
delta_mins <- seq(-30, 30, by = 15)
comp_names <- c("MVPA", "Sleep", "Sedentary", "LPA")
cov_names <- c("fsep2", "zf02m1cp", "fcpds", "season")
outc_name <- "fca52sleff"

### unadjusted regression analysis with ilrs ###

deltacomp_udf_SE <-
  predict_delta_comps(
    dataf = dataset,
    y = outc_name,
    comps = comp_names,
    deltas = delta_mins / (24 * 60), # changes as proportion of day
    comparisons = "prop-realloc",
    alpha = 0.05 / 4 # bonferonni adjusted alpha
  )
deltacomp_udf_SE


### adjusted regression analysis with ilrs ###

deltacomp_adf_SE <-
  predict_delta_comps(
    dataf = dataset,
    y = outc_name,
    comps = comp_names,
    covars = cov_names, # include covariates
    deltas = delta_mins / (24 * 60), # changes as proportion of day
    comparisons = "prop-realloc",
    alpha = 0.05 / 4 # bonferonni adjusted alpha
  )
deltacomp_adf_SE

Sleff_adf <- as_tibble(deltacomp_adf_SE) %>% mutate(outcome = "Sleep Efficiency")
Sleff_adf_fig1 <- as_tibble(deltacomp_adf_SE) %>% mutate(outcome = "SleepEfficiency")

### Figure: 1:1 reallocations of adjusted predictions
gp <- 
  ggplot(Sleff_adf_fig1) +
  geom_vline(xintercept = 0, col = "grey60") + 
  geom_hline(yintercept = 0, col = "grey60") + 
  geom_line(aes(x = delta * 1440, y = delta_pred, col = `comp+`)) + 
  geom_point(aes(x = delta * 1440, y = delta_pred, col = `comp+`), size = 1) + 
  geom_ribbon(aes(x = delta * 1440, ymin = ci_lo, ymax = ci_up, fill = `comp+`), alpha = 0.3) + 
  theme_bw() + 
  theme(
    legend.position = "none",
    text = element_text(size = 20),
    axis.title.x =element_text(size = 4)
  ) + 
  labs(
    x = paste0("Change/delta in composition"), 
    y = paste0("Predicted change in outcome")
  )  + 
  facet_grid(outcome ~ `comp+`, scales = "free_y", labeller = label_parsed) +
  scale_color_colorblind() +
  scale_fill_colorblind() 
gp


### compare overall model and test for variable significance ###

str(deltacomp_adf_SE) # look at structure of codaredistlm returned object
X <- attr(deltacomp_adf_SE, "dataf") # extract data.frame of model

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


### Model 1: ilr significance
# are the compositions/ilrs significant? (unadjusted: NO covariates in both models)
anova(lm_int_only, lm_ilr_only)

### Model 2: ilr significance
# are the compositions/ilrs significant? (adjusted: all covariates in both models)
anova(lm_no_comp, lm_comp)

# print summary of results
report(lm_comp)



####Sleep Onset####

### set variables to use in codaredistlm package
delta_mins <- seq(-30, 30, by = 15)
comp_names <- c("MVPA", "Sleep", "Sedentary", "LPA")
cov_names <- c("fsep2", "zf02m1cp", "fcpds", "season")
outc_name <- "fca52bedtm"

### unadjusted regression analysis with ilrs ###

deltacomp_udf_bed <-
  predict_delta_comps(
    dataf = dataset,
    y = outc_name,
    comps = comp_names,
    deltas = delta_mins / (24 * 60), # changes as proportion of day
    comparisons = "prop-realloc",
    alpha = 0.05 / 4 # bonferonni adjusted alpha
  )
deltacomp_udf_bed


### adjusted regression analysis with ilrs ###

deltacomp_adf_bed <-
  predict_delta_comps(
    dataf = dataset,
    y = outc_name,
    comps = comp_names,
    covars = cov_names, # include covariates
    deltas = delta_mins / (24 * 60), # changes as proportion of day
    comparisons = "prop-realloc",
    alpha = 0.05 / 4 # bonferonni adjusted alpha
  )
deltacomp_adf_bed

bed_adf <- as_tibble(deltacomp_adf_bed) %>% mutate(outcome = "Sleep Onset")
bed_adf_fig1 <- as_tibble(deltacomp_adf_bed) %>% mutate(outcome = "SleepOnset")

### Figure: 1:1 reallocations of adjusted predictions
gp <- 
  ggplot(bed_adf_fig1) +
  geom_vline(xintercept = 0, col = "grey60") + 
  geom_hline(yintercept = 0, col = "grey60") + 
  geom_line(aes(x = delta * 1440, y = delta_pred, col = `comp+`)) + 
  geom_point(aes(x = delta * 1440, y = delta_pred, col = `comp+`), size = 1) + 
  geom_ribbon(aes(x = delta * 1440, ymin = ci_lo, ymax = ci_up, fill = `comp+`), alpha = 0.3) + 
  theme_bw() + 
  theme(
    legend.position = "none",
    text = element_text(size = 20),
    axis.title.x =element_text(size = 4)
  ) + 
  labs(
    x = paste0("Change/delta in composition"), 
    y = paste0("Predicted change in outcome")
  )  + 
  facet_grid(outcome ~ `comp+`, scales = "free_y", labeller = label_parsed) +
  scale_color_colorblind() +
  scale_fill_colorblind() 
gp

### compare overall model and test for variable significance ###

str(deltacomp_adf_bed) # look at structure of codaredistlm returned object
X <- attr(deltacomp_adf_bed, "dataf") # extract data.frame of model

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



### Model 1: ilr significance
# are the compositions/ilrs significant? (unadjusted: NO covariates in both models)
anova(lm_int_only, lm_ilr_only)

### Model 2: ilr significance
# are the compositions/ilrs significant? (adjusted: all covariates in both models)
anova(lm_no_comp, lm_comp)

# print summary of results
report(lm_comp)


####Sleep Offset ####

### set variables to use in codaredistlm package
delta_mins <- seq(-30, 30, by = 15)
comp_names <- c("MVPA", "Sleep", "Sedentary", "LPA")
cov_names <- c("fsep2", "zf02m1cp", "fcpds", "season")
outc_name <- "fca52guptm"

### unadjusted regression analysis with ilrs ###

deltacomp_udf_wake <-
  predict_delta_comps(
    dataf = dataset,
    y = outc_name,
    comps = comp_names,
    deltas = delta_mins / (24 * 60), # changes as proportion of day
    comparisons = "prop-realloc",
    alpha = 0.05 / 4 # bonferonni adjusted alpha
  )
deltacomp_udf_wake


### adjusted regression analysis with ilrs ###

deltacomp_adf_wake <-
  predict_delta_comps(
    dataf = dataset,
    y = outc_name,
    comps = comp_names,
    covars = cov_names, # include covariates
    deltas = delta_mins / (24 * 60), # changes as proportion of day
    comparisons = "prop-realloc",
    alpha = 0.05 / 4 # bonferonni adjusted alpha
  )
deltacomp_adf_wake

wake_adf <- as_tibble(deltacomp_adf_wake) %>% mutate(outcome = "Sleep Offset")
wake_adf_fig1 <- as_tibble(deltacomp_adf_wake) %>% mutate(outcome = "SleepOffset")

### Figure: 1:1 reallocations of adjusted predictions
gp <- 
  ggplot(wake_adf_fig1) +
  geom_vline(xintercept = 0, col = "grey60") + 
  geom_hline(yintercept = 0, col = "grey60") + 
  geom_line(aes(x = delta * 1440, y = delta_pred, col = `comp+`)) + 
  geom_point(aes(x = delta * 1440, y = delta_pred, col = `comp+`), size = 1) + 
  geom_ribbon(aes(x = delta * 1440, ymin = ci_lo, ymax = ci_up, fill = `comp+`), alpha = 0.3) + 
  theme_bw() + 
  theme(
    legend.position = "none",
    text = element_text(size = 20),
    axis.title.x =element_text(size = 4)
  ) + 
  labs(
    x = paste0("Change/delta in composition"), 
    y = paste0("Predicted change in outcome")
  )  + 
  facet_grid(outcome ~ `comp+`, scales = "free_y", labeller = label_parsed) +
  scale_color_colorblind() +
  scale_fill_colorblind() 
gp


### compare overall model and test for variable significance ###

str(deltacomp_adf_wake) # look at structure of codaredistlm returned object
X <- attr(deltacomp_adf_wake, "dataf") # extract data.frame of model

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



### Model 1: ilr significance
# are the compositions/ilrs significant? (unadjusted: NO covariates in both models)
anova(lm_int_only, lm_ilr_only)

### Model 2: ilr significance
# are the compositions/ilrs significant? (adjusted: all covariates in both models)
anova(lm_no_comp, lm_comp)

# print summary of results
report(lm_comp)


####Sleep variability ####

### set variables to use in codaredistlm package
delta_mins <- seq(-30, 30, by = 15)
comp_names <- c("MVPA", "Sleep", "Sedentary", "LPA")
cov_names <- c("fsep2", "zf02m1cp", "fcpds", "season")
outc_name <- "fca52slcon"

### unadjusted regression analysis with ilrs ###

deltacomp_udf_variability <-
  predict_delta_comps(
    dataf = dataset,
    y = outc_name,
    comps = comp_names,
    deltas = delta_mins / (24 * 60), # changes as proportion of day
    comparisons = "prop-realloc",
    alpha = 0.05 / 4 # bonferonni adjusted alpha
  )
deltacomp_udf_variability


### adjusted regression analysis with ilrs ###

deltacomp_adf_variability <-
  predict_delta_comps(
    dataf = dataset,
    y = outc_name,
    comps = comp_names,
    covars = cov_names, # include covariates
    deltas = delta_mins / (24 * 60), # changes as proportion of day
    comparisons = "prop-realloc",
    alpha = 0.05 / 4 # bonferonni adjusted alpha
  )
deltacomp_adf_variability

var_adf <- as_tibble(deltacomp_adf_variability) %>% mutate(outcome = "Sleep Variability")
var_adf_fig1 <- as_tibble(deltacomp_adf_variability) %>% mutate(outcome = "SleepVariability")

### Figure: 1:1 reallocations of adjusted predictions
gp <- 
  ggplot(var_adf_fig1) +
  geom_vline(xintercept = 0, col = "grey60") + 
  geom_hline(yintercept = 0, col = "grey60") + 
  geom_line(aes(x = delta * 1440, y = delta_pred, col = `comp+`)) + 
  geom_point(aes(x = delta * 1440, y = delta_pred, col = `comp+`), size = 1) + 
  geom_ribbon(aes(x = delta * 1440, ymin = ci_lo, ymax = ci_up, fill = `comp+`), alpha = 0.3) + 
  theme_bw() + 
  theme(
    legend.position = "none",
    text = element_text(size = 20),
    axis.title.x =element_text(size = 4)
  ) + 
  labs(
    x = paste0("Change/delta in composition"), 
    y = paste0("Predicted change in outcome")
  )  + 
  facet_grid(outcome ~ `comp+`, scales = "free_y", labeller = label_parsed) +
  scale_color_colorblind() +
  scale_fill_colorblind() 
gp

### compare overall model and test for variable significance ###

str(deltacomp_adf_variability) # look at structure of codaredistlm returned object
X <- attr(deltacomp_adf_variability, "dataf") # extract data.frame of model

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



### Model 1: ilr significance
# are the compositions/ilrs significant? (unadjusted: NO covariates in both models)
anova(lm_int_only, lm_ilr_only)

### Model 2: ilr significance
# are the compositions/ilrs significant? (adjusted: all covariates in both models)
anova(lm_no_comp, lm_comp)

# print summary of results
report(lm_comp)



####wrap up####


plot_data <-
  bind_rows(
    ts_adf,
    tired_adf,
    Sleff_adf,
    bed_adf,
    wake_adf,
    var_adf
  )



plot_data$`comp+` <- paste0(plot_data$`comp+`, "+Delta")

plot_data$outcome <- as.character(plot_data$outcome)
plot_data$outcome <- gsub(" ", "~", plot_data$outcome)
plot_data$outcome
plot_data$outcome <- fct_inorder(plot_data$outcome)


gp <- 
  ggplot(plot_data) +
  geom_vline(xintercept = 0, col = "grey60") + 
  geom_hline(yintercept = 0, col = "grey60") + 
  geom_line(aes(x = delta * 1440, y = delta_pred, col = `comp+`)) + 
  geom_point(aes(x = delta * 1440, y = delta_pred, col = `comp+`), size = 1) + 
  geom_ribbon(aes(x = delta * 1440, ymin = ci_lo, ymax = ci_up, fill = `comp+`), alpha = 0.3) + 
  theme_bw() + 
  theme(
    legend.position = "none",
    text = element_text(size = 16)
    ) + 
  labs(
    x = paste0("Change/delta in composition"), 
    y = paste0("Predicted change in outcome")
  )  + 
  facet_grid(outcome ~ `comp+`, scales = "free_y", labeller = label_parsed) +
  scale_color_colorblind() +
  scale_fill_colorblind() 
gp

ggsave(filename = "location/of/data/name.png", width = 8, height = 12, dpi = 600)
