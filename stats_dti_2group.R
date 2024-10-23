
# NOT SHOWN IN THE PAPER!!!

# FIT MIXED EFFECT MODEL (segment as categorical variable)
# mixed effect model to investigate group differences over segments
#   - group: fixed effect (2 levels)
#   - segment: fixed effect (12 levels)
#   - group-segment interaction: fixed effect (48 levels)
#   - subject over segments: random effect
#   - area: dependent variable
#
# Model: dti_metric ~ group + segment + segment*subject(group_bin) + segment|

ctrl <- lmeControl(maxIter = 1000, msMaxIter = 1000, optimMethod = "optim", msMaxEval = 1000, niterEM = 1000)

summary(lme_fa_gm     <- lme(data = dfl_fa_gm,     value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_dor <- lme(data = dfl_fa_gm_dor, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_ven <- lme(data = dfl_fa_gm_ven, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm     <- lme(data = dfl_fa_wm,     value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_dor <- lme(data = dfl_fa_wm_dor, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_lat <- lme(data = dfl_fa_wm_lat, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_ven <- lme(data = dfl_fa_wm_ven, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm     <- lme(data = dfl_md_gm,     value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_dor <- lme(data = dfl_md_gm_dor, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_ven <- lme(data = dfl_md_gm_ven, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm     <- lme(data = dfl_md_wm,     value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_dor <- lme(data = dfl_md_wm_dor, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_lat <- lme(data = dfl_md_wm_lat, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_ven <- lme(data = dfl_md_wm_ven, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm     <- lme(data = dfl_ad_gm,     value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_dor <- lme(data = dfl_ad_gm_dor, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_ven <- lme(data = dfl_ad_gm_ven, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm     <- lme(data = dfl_ad_wm,     value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_dor <- lme(data = dfl_ad_wm_dor, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_lat <- lme(data = dfl_ad_wm_lat, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_ven <- lme(data = dfl_ad_wm_ven, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm     <- lme(data = dfl_rd_gm,     value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_dor <- lme(data = dfl_rd_gm_dor, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_ven <- lme(data = dfl_rd_gm_ven, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm     <- lme(data = dfl_rd_wm,     value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_dor <- lme(data = dfl_rd_wm_dor, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_lat <- lme(data = dfl_rd_wm_lat, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_ven <- lme(data = dfl_rd_wm_ven, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))


# ========== post-hoc test 1: SHAM vs SCI (main effect model) ==========
# We start with the simplest question possible: is the SCI group (the 3 SCI groups combined) differ from the sham group?
# For this, we combine the mild, moderate, and severe groups into a single SCI group.

# get marginal means
emm1_fa_gm     <- emmeans(lme_fa_gm,     specs = ~ group_bin)
emm1_fa_gm_dor <- emmeans(lme_fa_gm_dor, specs = ~ group_bin)
emm1_fa_gm_ven <- emmeans(lme_fa_gm_ven, specs = ~ group_bin)
emm1_fa_wm     <- emmeans(lme_fa_wm,     specs = ~ group_bin)
emm1_fa_wm_dor <- emmeans(lme_fa_wm_dor, specs = ~ group_bin)
emm1_fa_wm_lat <- emmeans(lme_fa_wm_lat, specs = ~ group_bin)
emm1_fa_wm_ven <- emmeans(lme_fa_wm_ven, specs = ~ group_bin)
emm1_md_gm     <- emmeans(lme_md_gm,     specs = ~ group_bin)
emm1_md_gm_dor <- emmeans(lme_md_gm_dor, specs = ~ group_bin)
emm1_md_gm_ven <- emmeans(lme_md_gm_ven, specs = ~ group_bin)
emm1_md_wm     <- emmeans(lme_md_wm,     specs = ~ group_bin)
emm1_md_wm_dor <- emmeans(lme_md_wm_dor, specs = ~ group_bin)
emm1_md_wm_lat <- emmeans(lme_md_wm_lat, specs = ~ group_bin)
emm1_md_wm_ven <- emmeans(lme_md_wm_ven, specs = ~ group_bin)
emm1_ad_gm     <- emmeans(lme_ad_gm,     specs = ~ group_bin)
emm1_ad_gm_dor <- emmeans(lme_ad_gm_dor, specs = ~ group_bin)
emm1_ad_gm_ven <- emmeans(lme_ad_gm_ven, specs = ~ group_bin)
emm1_ad_wm     <- emmeans(lme_ad_wm,     specs = ~ group_bin)
emm1_ad_wm_dor <- emmeans(lme_ad_wm_dor, specs = ~ group_bin)
emm1_ad_wm_lat <- emmeans(lme_ad_wm_lat, specs = ~ group_bin)
emm1_ad_wm_ven <- emmeans(lme_ad_wm_ven, specs = ~ group_bin)
emm1_rd_gm     <- emmeans(lme_rd_gm,     specs = ~ group_bin)
emm1_rd_gm_dor <- emmeans(lme_rd_gm_dor, specs = ~ group_bin)
emm1_rd_gm_ven <- emmeans(lme_rd_gm_ven, specs = ~ group_bin)
emm1_rd_wm     <- emmeans(lme_rd_wm,     specs = ~ group_bin)
emm1_rd_wm_dor <- emmeans(lme_rd_wm_dor, specs = ~ group_bin)
emm1_rd_wm_lat <- emmeans(lme_rd_wm_lat, specs = ~ group_bin)
emm1_rd_wm_ven <- emmeans(lme_rd_wm_ven, specs = ~ group_bin)

# define contrasts
shm <- c(1,0)
sci <- c(0,1)

list <- list("sci-shm" = sci - shm)

# estimate contrasts
c1_fa_gm     <- contrast(emm1_fa_gm,     method = list); c1_fa_gm;
c1_fa_gm_dor <- contrast(emm1_fa_gm_dor, method = list); c1_fa_gm_dor;
c1_fa_gm_ven <- contrast(emm1_fa_gm_ven, method = list); c1_fa_gm_ven;
c1_fa_wm     <- contrast(emm1_fa_wm,     method = list); c1_fa_wm;
c1_fa_wm_dor <- contrast(emm1_fa_wm_dor, method = list); c1_fa_wm_dor;
c1_fa_wm_lat <- contrast(emm1_fa_wm_lat, method = list); c1_fa_wm_lat;
c1_fa_wm_ven <- contrast(emm1_fa_wm_ven, method = list); c1_fa_wm_ven;
c1_md_gm     <- contrast(emm1_md_gm,     method = list); c1_md_gm;
c1_md_gm_dor <- contrast(emm1_md_gm_dor, method = list); c1_md_gm_dor;
c1_md_gm_ven <- contrast(emm1_md_gm_ven, method = list); c1_md_gm_ven;
c1_md_wm     <- contrast(emm1_md_wm,     method = list); c1_md_wm;
c1_md_wm_dor <- contrast(emm1_md_wm_dor, method = list); c1_md_wm_dor;
c1_md_wm_lat <- contrast(emm1_md_wm_lat, method = list); c1_md_wm_lat;
c1_md_wm_ven <- contrast(emm1_md_wm_ven, method = list); c1_md_wm_ven;
c1_ad_gm     <- contrast(emm1_ad_gm,     method = list); c1_ad_gm;
c1_ad_gm_dor <- contrast(emm1_ad_gm_dor, method = list); c1_ad_gm_dor;
c1_ad_gm_ven <- contrast(emm1_ad_gm_ven, method = list); c1_ad_gm_ven;
c1_ad_wm     <- contrast(emm1_ad_wm,     method = list); c1_ad_wm;
c1_ad_wm_dor <- contrast(emm1_ad_wm_dor, method = list); c1_ad_wm_dor;
c1_ad_wm_lat <- contrast(emm1_ad_wm_lat, method = list); c1_ad_wm_lat;
c1_ad_wm_ven <- contrast(emm1_ad_wm_ven, method = list); c1_ad_wm_ven;
c1_rd_gm     <- contrast(emm1_rd_gm,     method = list); c1_rd_gm;
c1_rd_gm_dor <- contrast(emm1_rd_gm_dor, method = list); c1_rd_gm_dor;
c1_rd_gm_ven <- contrast(emm1_rd_gm_ven, method = list); c1_rd_gm_ven;
c1_rd_wm     <- contrast(emm1_rd_wm,     method = list); c1_rd_wm;
c1_rd_wm_dor <- contrast(emm1_rd_wm_dor, method = list); c1_rd_wm_dor;
c1_rd_wm_lat <- contrast(emm1_rd_wm_lat, method = list); c1_rd_wm_lat;
c1_rd_wm_ven <- contrast(emm1_rd_wm_ven, method = list); c1_rd_wm_ven;


# RESULTS: Atlas-based DTI analysis: Post-hoc tests revealed significant differences between SHM and SCI in
# fa_wm_dor (p=0.0288)



# ========== post-hoc test 2: group-wise analysis per each spinal level  ==========
# We've previously detected significant group differences between the SCI and sham groups.
# Now, we want to test at which levels these differences exist.

# get marginal means
emm2_fa_gm     <- emmeans(lme_fa_gm,     specs = ~ group_bin*level)
emm2_fa_gm_dor <- emmeans(lme_fa_gm_dor, specs = ~ group_bin*level)
emm2_fa_gm_ven <- emmeans(lme_fa_gm_ven, specs = ~ group_bin*level)
emm2_fa_wm     <- emmeans(lme_fa_wm,     specs = ~ group_bin*level)
emm2_fa_wm_dor <- emmeans(lme_fa_wm_dor, specs = ~ group_bin*level)
emm2_fa_wm_lat <- emmeans(lme_fa_wm_lat, specs = ~ group_bin*level)
emm2_fa_wm_ven <- emmeans(lme_fa_wm_ven, specs = ~ group_bin*level)
emm2_md_gm     <- emmeans(lme_md_gm,     specs = ~ group_bin*level)
emm2_md_gm_dor <- emmeans(lme_md_gm_dor, specs = ~ group_bin*level)
emm2_md_gm_ven <- emmeans(lme_md_gm_ven, specs = ~ group_bin*level)
emm2_md_wm     <- emmeans(lme_md_wm,     specs = ~ group_bin*level)
emm2_md_wm_dor <- emmeans(lme_md_wm_dor, specs = ~ group_bin*level)
emm2_md_wm_lat <- emmeans(lme_md_wm_lat, specs = ~ group_bin*level)
emm2_md_wm_ven <- emmeans(lme_md_wm_ven, specs = ~ group_bin*level)
emm2_ad_gm     <- emmeans(lme_ad_gm,     specs = ~ group_bin*level)
emm2_ad_gm_dor <- emmeans(lme_ad_gm_dor, specs = ~ group_bin*level)
emm2_ad_gm_ven <- emmeans(lme_ad_gm_ven, specs = ~ group_bin*level)
emm2_ad_wm     <- emmeans(lme_ad_wm,     specs = ~ group_bin*level)
emm2_ad_wm_dor <- emmeans(lme_ad_wm_dor, specs = ~ group_bin*level)
emm2_ad_wm_lat <- emmeans(lme_ad_wm_lat, specs = ~ group_bin*level)
emm2_ad_wm_ven <- emmeans(lme_ad_wm_ven, specs = ~ group_bin*level)
emm2_rd_gm     <- emmeans(lme_rd_gm,     specs = ~ group_bin*level)
emm2_rd_gm_dor <- emmeans(lme_rd_gm_dor, specs = ~ group_bin*level)
emm2_rd_gm_ven <- emmeans(lme_rd_gm_ven, specs = ~ group_bin*level)
emm2_rd_wm     <- emmeans(lme_rd_wm,     specs = ~ group_bin*level)
emm2_rd_wm_dor <- emmeans(lme_rd_wm_dor, specs = ~ group_bin*level)
emm2_rd_wm_lat <- emmeans(lme_rd_wm_lat, specs = ~ group_bin*level)
emm2_rd_wm_ven <- emmeans(lme_rd_wm_ven, specs = ~ group_bin*level)

# define contrasts
shm_C3 <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
sci_C3 <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
shm_C4 <- c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
sci_C4 <- c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
shm_C5 <- c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
sci_C5 <- c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
shm_C6 <- c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
sci_C6 <- c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
shm_C7 <- c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
sci_C7 <- c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
shm_C8 <- c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
sci_C8 <- c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
shm_T1 <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0)
sci_T1 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
shm_T2 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0)
sci_T2 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
shm_T3 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
sci_T3 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
shm_T4 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0)
sci_T4 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
shm_T5 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0)
sci_T5 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)
shm_T6 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0)
sci_T6 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)

list <- list(
  "sci_C3-shm_C3" = sci_C3-shm_C3,
  "sci_C4-shm_C4" = sci_C4-shm_C4,
  "sci_C5-shm_C5" = sci_C5-shm_C5,
  "sci_C6-shm_C6" = sci_C6-shm_C6,
  "sci_C7-shm_C7" = sci_C7-shm_C7,
  "sci_C8-shm_C8" = sci_C8-shm_C8,
  "sci_T1-shm_T1" = sci_T1-shm_T1,
  "sci_T2-shm_T2" = sci_T2-shm_T2,
  "sci_T3-shm_T3" = sci_T3-shm_T3,
  "sci_T4-shm_T4" = sci_T4-shm_T4,
  "sci_T5-shm_T5" = sci_T5-shm_T5,
  "sci_T6-shm_T6" = sci_T6-shm_T6
)

# estimate contrasts
c2_fa_gm     <- contrast(emm2_fa_gm,     method = list); c2_fa_gm;
c2_fa_gm_dor <- contrast(emm2_fa_gm_dor, method = list); c2_fa_gm_dor;
c2_fa_gm_ven <- contrast(emm2_fa_gm_ven, method = list); c2_fa_gm_ven;
c2_fa_wm     <- contrast(emm2_fa_wm,     method = list); c2_fa_wm;
c2_fa_wm_dor <- contrast(emm2_fa_wm_dor, method = list); c2_fa_wm_dor;
c2_fa_wm_lat <- contrast(emm2_fa_wm_lat, method = list); c2_fa_wm_lat;
c2_fa_wm_ven <- contrast(emm2_fa_wm_ven, method = list); c2_fa_wm_ven;
c2_md_gm     <- contrast(emm2_md_gm,     method = list); c2_md_gm;
c2_md_gm_dor <- contrast(emm2_md_gm_dor, method = list); c2_md_gm_dor;
c2_md_gm_ven <- contrast(emm2_md_gm_ven, method = list); c2_md_gm_ven;
c2_md_wm     <- contrast(emm2_md_wm,     method = list); c2_md_wm;
c2_md_wm_dor <- contrast(emm2_md_wm_dor, method = list); c2_md_wm_dor;
c2_md_wm_lat <- contrast(emm2_md_wm_lat, method = list); c2_md_wm_lat;
c2_md_wm_ven <- contrast(emm2_md_wm_ven, method = list); c2_md_wm_ven;
c2_ad_gm     <- contrast(emm2_ad_gm,     method = list); c2_ad_gm;
c2_ad_gm_dor <- contrast(emm2_ad_gm_dor, method = list); c2_ad_gm_dor;
c2_ad_gm_ven <- contrast(emm2_ad_gm_ven, method = list); c2_ad_gm_ven;
c2_ad_wm     <- contrast(emm2_ad_wm,     method = list); c2_ad_wm;
c2_ad_wm_dor <- contrast(emm2_ad_wm_dor, method = list); c2_ad_wm_dor;
c2_ad_wm_lat <- contrast(emm2_ad_wm_lat, method = list); c2_ad_wm_lat;
c2_ad_wm_ven <- contrast(emm2_ad_wm_ven, method = list); c2_ad_wm_ven;
c2_rd_gm     <- contrast(emm2_rd_gm,     method = list); c2_rd_gm;
c2_rd_gm_dor <- contrast(emm2_rd_gm_dor, method = list); c2_rd_gm_dor;
c2_rd_gm_ven <- contrast(emm2_rd_gm_ven, method = list); c2_rd_gm_ven;
c2_rd_wm     <- contrast(emm2_rd_wm,     method = list); c2_rd_wm;
c2_rd_wm_dor <- contrast(emm2_rd_wm_dor, method = list); c2_rd_wm_dor;
c2_rd_wm_lat <- contrast(emm2_rd_wm_lat, method = list); c2_rd_wm_lat;
c2_rd_wm_ven <- contrast(emm2_rd_wm_ven, method = list); c2_rd_wm_ven;


# RESULTS: Atlas-based DTI analysis: Post-hoc tests revealed differences between SHM and SCI in
# fa_wm_dor at T4 (p=0.0232), T2 (p=0.0183)







# ========== post-hoc test 3: group differences within combined segments ==========
# NOT SHOWN IN THE PAPER!!!
# To reduce the number of levels, we combine the 16 vertebral levels into 4 regions of 3 vertebral level each:
#   upper cervical cord (uc): C3, C4, C5
#   lower cervical cord (lc): C6, C7, C8
#   upper thoracic cord (ut): T1, T2, T3
#   mid   thoracic cord (mt): T4, T5, T6

# define contrasts
shm_uc <- c(1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
sci_uc <- c(0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
shm_lc <- c(0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
sci_lc <- c(0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0)/3
shm_ut <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0)/3
sci_ut <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0)/3
shm_mt <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0)/3
sci_mt <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1)/3

list <- list("sci_uc-shm_uc" = sci_uc - shm_uc,
             "sci_lc-shm_lc" = sci_lc - shm_lc,
             "sci_ut-shm_ut" = sci_ut - shm_ut,
             "sci_mt-shm_mt" = sci_mt - shm_mt)

# estimate contrasts
c3_fa_gm     <- contrast(emm2_fa_gm,     method = list); c3_fa_gm;
c3_fa_gm_dor <- contrast(emm2_fa_gm_dor, method = list); c3_fa_gm_dor;
c3_fa_gm_ven <- contrast(emm2_fa_gm_ven, method = list); c3_fa_gm_ven;
c3_fa_wm     <- contrast(emm2_fa_wm,     method = list); c3_fa_wm;
c3_fa_wm_dor <- contrast(emm2_fa_wm_dor, method = list); c3_fa_wm_dor;
c3_fa_wm_lat <- contrast(emm2_fa_wm_lat, method = list); c3_fa_wm_lat;
c3_fa_wm_ven <- contrast(emm2_fa_wm_ven, method = list); c3_fa_wm_ven;
c3_md_gm     <- contrast(emm2_md_gm,     method = list); c3_md_gm;
c3_md_gm_dor <- contrast(emm2_md_gm_dor, method = list); c3_md_gm_dor;
c3_md_gm_ven <- contrast(emm2_md_gm_ven, method = list); c3_md_gm_ven;
c3_md_wm     <- contrast(emm2_md_wm,     method = list); c3_md_wm;
c3_md_wm_dor <- contrast(emm2_md_wm_dor, method = list); c3_md_wm_dor;
c3_md_wm_lat <- contrast(emm2_md_wm_lat, method = list); c3_md_wm_lat;
c3_md_wm_ven <- contrast(emm2_md_wm_ven, method = list); c3_md_wm_ven;
c3_ad_gm     <- contrast(emm2_ad_gm,     method = list); c3_ad_gm;
c3_ad_gm_dor <- contrast(emm2_ad_gm_dor, method = list); c3_ad_gm_dor;
c3_ad_gm_ven <- contrast(emm2_ad_gm_ven, method = list); c3_ad_gm_ven;
c3_ad_wm     <- contrast(emm2_ad_wm,     method = list); c3_ad_wm;
c3_ad_wm_dor <- contrast(emm2_ad_wm_dor, method = list); c3_ad_wm_dor;
c3_ad_wm_lat <- contrast(emm2_ad_wm_lat, method = list); c3_ad_wm_lat;
c3_ad_wm_ven <- contrast(emm2_ad_wm_ven, method = list); c3_ad_wm_ven;
c3_rd_gm     <- contrast(emm2_rd_gm,     method = list); c3_rd_gm;
c3_rd_gm_dor <- contrast(emm2_rd_gm_dor, method = list); c3_rd_gm_dor;
c3_rd_gm_ven <- contrast(emm2_rd_gm_ven, method = list); c3_rd_gm_ven;
c3_rd_wm     <- contrast(emm2_rd_wm,     method = list); c3_rd_wm;
c3_rd_wm_dor <- contrast(emm2_rd_wm_dor, method = list); c3_rd_wm_dor;
c3_rd_wm_lat <- contrast(emm2_rd_wm_lat, method = list); c3_rd_wm_lat;
c3_rd_wm_ven <- contrast(emm2_rd_wm_ven, method = list); c3_rd_wm_ven;


# RESULTS: Atlas-based DTI analysis
# fma_dor     is different at   mid-th (p=0.0269), upper-th (p=0.0146)
