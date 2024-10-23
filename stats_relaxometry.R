# =======================================================================================
# ============================== PART IV. ANALYSIS OF RELAXOMETRY =======================
# =======================================================================================
# run script stats_relaxometry.R

# GENERATE PLOTS HERE; use the script
# stats_plot_figs.R


# FIT MIXED EFFECT MODEL (segment as categorical variable)
# mixed effect model to investigate group differences over segments
#   - group: fixed effect
#   - segment: fixed effect
#   - group-segment interaction: fixed effect
#   - subject over segments: random effect
#   - area: dependent variable
#
# Model: area ~ group + segment + segment*subject(group_bin) + segment|subject
ctrl <- lmeControl(maxIter = 1000, msMaxIter = 1000, optimMethod = "optim", msMaxEval = 1000, niterEM = 1000)

summary(lme41_T1_gm      <- lme(data = dfl_T1_gm,      value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme41_T1_gm_dor  <- lme(data = dfl_T1_gm_dor,  value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme41_T1_gm_ven  <- lme(data = dfl_T1_gm_ven,  value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme41_T1_wm      <- lme(data = dfl_T1_wm,      value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme41_T1_wm_dor  <- lme(data = dfl_T1_wm_dor,  value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme41_T1_wm_lat  <- lme(data = dfl_T1_wm_lat,  value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme41_T1_wm_ven  <- lme(data = dfl_T1_wm_ven,  value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
#summary(lme41_r2s_gm     <- lme(data = dfl_r2s_gm,     value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
#summary(lme41_r2s_gm_dor <- lme(data = dfl_r2s_gm_dor, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
#summary(lme41_r2s_gm_ven <- lme(data = dfl_r2s_gm_ven, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
#summary(lme41_r2s_wm     <- lme(data = dfl_r2s_wm,     value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
#summary(lme41_r2s_wm_dor <- lme(data = dfl_r2s_wm_dor, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
#summary(lme41_r2s_wm_lat <- lme(data = dfl_r2s_wm_lat, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
#summary(lme41_r2s_wm_ven <- lme(data = dfl_f2s_wm_ven, value ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))



# ========== post-hoc test 1: SHAM vs SCI (main effect model) ==========
# We start with the simplest question possible: is the SCI group (the 3 SCI groups combined) differ from the sham group?
# For this, we combine the mild, moderate, and severe groups into a single SCI group.
emm41_T1_gm      <- emmeans(lme41_T1_gm,      specs = ~ group_bin, at = list(level=level_names))
emm41_T1_gm_dor  <- emmeans(lme41_T1_gm_dor,  specs = ~ group_bin, at = list(level=level_names))
emm41_T1_gm_ven  <- emmeans(lme41_T1_gm_ven,  specs = ~ group_bin, at = list(level=level_names))
emm41_T1_wm      <- emmeans(lme41_T1_wm,      specs = ~ group_bin, at = list(level=level_names))
emm41_T1_wm_dor  <- emmeans(lme41_T1_wm_dor,  specs = ~ group_bin, at = list(level=level_names))
emm41_T1_wm_lat  <- emmeans(lme41_T1_wm_lat,  specs = ~ group_bin, at = list(level=level_names))
emm41_T1_wm_ven  <- emmeans(lme41_T1_wm_ven,  specs = ~ group_bin, at = list(level=level_names))
#emm41_r2s_gm     <- emmeans(lme41_r2s_gm,     specs = ~ group_bin, at = list(level=level_names))
#emm41_r2s_gm_dor <- emmeans(lme41_r2s_gm_dor, specs = ~ group_bin, at = list(level=level_names))
#emm41_r2s_gm_ven <- emmeans(lme41_r2s_gm_ven, specs = ~ group_bin, at = list(level=level_names))
#emm41_r2s_wm     <- emmeans(lme41_r2s_wm,     specs = ~ group_bin, at = list(level=level_names))
#emm41_r2s_wm_dor <- emmeans(lme41_r2s_wm_dor, specs = ~ group_bin, at = list(level=level_names))
#emm41_r2s_wm_lat <- emmeans(lme41_r2s_wm_lat, specs = ~ group_bin, at = list(level=level_names))
#emm41_r2s_wm_ven <- emmeans(lme41_r2s_wm_ven, specs = ~ group_bin, at = list(level=level_names))

sham <- c(1,0)
sci  <- c(0,1)

c41_T1_gm      <- contrast(emm41_T1_gm,      method = list("sca_sci-sham" = sci-sham)); c41_T1_gm;
c41_T1_gm_dor  <- contrast(emm41_T1_gm_dor,  method = list("sca_sci-sham" = sci-sham)); c41_T1_gm_dor;
c41_T1_gm_ven  <- contrast(emm41_T1_gm_ven,  method = list("sca_sci-sham" = sci-sham)); c41_T1_gm_ven;
c41_T1_wm      <- contrast(emm41_T1_wm,      method = list("wma_sci-sham" = sci-sham)); c41_T1_wm;
c41_T1_wm_dor  <- contrast(emm41_T1_wm_dor,  method = list("gma_sci-sham" = sci-sham)); c41_T1_wm_dor;
c41_T1_wm_lat  <- contrast(emm41_T1_wm_lat,  method = list("gma_sci-sham" = sci-sham)); c41_T1_wm_lat;
c41_T1_wm_ven  <- contrast(emm41_T1_wm_ven,  method = list("gma_sci-sham" = sci-sham)); c41_T1_wm_ven;
#c41_r2s_gm     <- contrast(emm41_r2s_gm,     method = list("sca_sci-sham" = sci-sham)); c41_r2s_gm;
#c41_r2s_gm_dor <- contrast(emm41_r2s_gm_dor, method = list("sca_sci-sham" = sci-sham)); c41_r2s_gm_dor;
#c41_r2s_gm_ven <- contrast(emm41_r2s_gm_ven, method = list("sca_sci-sham" = sci-sham)); c41_r2s_gm_ven;
#c41_r2s_wm     <- contrast(emm41_r2s_wm,     method = list("wma_sci-sham" = sci-sham)); c41_r2s_wm;
#c41_r2s_wm_dor <- contrast(emm41_r2s_wm_dor, method = list("gma_sci-sham" = sci-sham)); c41_r2s_wm_dor;
#c41_r2s_wm_lat <- contrast(emm41_r2s_wm_lat, method = list("gma_sci-sham" = sci-sham)); c41_r2s_wm_lat;
#c41_r2s_wm_ven <- contrast(emm41_r2s_wm_ven, method = list("gma_sci-sham" = sci-sham)); c41_r2s_wm_ven;


# RESULTS: Atlas-based DTI analysis: Post-hoc tests revealed that no metric is significantly different between SCI and sham.



# ========== post-hoc test 2: SHAM vs SCI within combined segments ==========
# We've detected significant differences between SCI and sham. Now, we want to look at the effect of the region. Where are these differences the strongest?
# To reduce the number of levels, we combine the 16 vertebral levels into 4 regions of 3 vertebral level each:
#   upper cervical cord: C3, C4, C5
#   lower cervical cord: C6, C7, C8
#   upper thoracic cord: T1, T2, T3
#   mid   thoracic cord: T4, T5, T6

shm_s1 <- c(1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
sci_s1 <- c(0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
shm_s2 <- c(0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
sci_s2 <- c(0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
shm_s3 <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0)
sci_s3 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0)
shm_s4 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0)
sci_s4 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1)

list <- list("sca_sci-sham_s1" = sci_s1-shm_s1,
             "sca_sci-sham_s2" = sci_s2-shm_s2,
             "sca_sci-sham_s3" = sci_s3-shm_s3,
             "sca_sci-sham_s4" = sci_s4-shm_s4)

emm42_T1_gm      <- emmeans(lme41_T1_gm,     specs = ~ group_bin*level, at = list(level=level_names))
emm42_T1_gm_dor  <- emmeans(lme41_T1_gm_dor, specs = ~ group_bin*level, at = list(level=level_names))
emm42_T1_gm_ven  <- emmeans(lme41_T1_gm_ven, specs = ~ group_bin*level, at = list(level=level_names))
emm42_T1_wm      <- emmeans(lme41_T1_wm,     specs = ~ group_bin*level, at = list(level=level_names))
emm42_T1_wm_dor  <- emmeans(lme41_T1_wm_dor, specs = ~ group_bin*level, at = list(level=level_names))
emm42_T1_wm_lat  <- emmeans(lme41_T1_wm_lat, specs = ~ group_bin*level, at = list(level=level_names))
emm42_T1_wm_ven  <- emmeans(lme41_T1_wm_ven, specs = ~ group_bin*level, at = list(level=level_names))
#emm42_r2s_gm     <- emmeans(lme41_r2s_gm,     specs = ~ group_bin*level, at = list(level=level_names))
#emm42_r2s_gm_dor <- emmeans(lme41_r2s_gm_dor, specs = ~ group_bin*level, at = list(level=level_names))
#emm42_r2s_gm_ven <- emmeans(lme41_r2s_gm_ven, specs = ~ group_bin*level, at = list(level=level_names))
#emm42_r2s_wm     <- emmeans(lme41_r2s_wm,     specs = ~ group_bin*level, at = list(level=level_names))
#emm42_r2s_wm_dor <- emmeans(lme41_r2s_wm_dor, specs = ~ group_bin*level, at = list(level=level_names))
#emm42_r2s_wm_lat <- emmeans(lme41_r2s_wm_lat, specs = ~ group_bin*level, at = list(level=level_names))
#emm42_r2s_wm_ven <- emmeans(lme41_r2s_wm_ven, specs = ~ group_bin*level, at = list(level=level_names))

c42_T1_gm      <- contrast(emm42_T1_gm,      method = list); c42_T1_gm;
c42_T1_gm_dor  <- contrast(emm42_T1_gm_dor,  method = list); c42_T1_gm_dor;
c42_T1_gm_ven  <- contrast(emm42_T1_gm_ven,  method = list); c42_T1_gm_ven;
c42_T1_wm      <- contrast(emm42_T1_wm,      method = list); c42_T1_wm;
c42_T1_wm_dor  <- contrast(emm42_T1_wm_dor,  method = list); c42_T1_wm_dor;
c42_T1_wm_lat  <- contrast(emm42_T1_wm_lat,  method = list); c42_T1_wm_lat;
c42_T1_wm_ven  <- contrast(emm42_T1_wm_ven,  method = list); c42_T1_wm_ven;
#c42_r2s_gm     <- contrast(emm42_r2s_gm,     method = list); c42_r2s_gm;
#c42_r2s_gm_dor <- contrast(emm42_r2s_gm_dor, method = list); c42_r2s_gm_dor;
#c42_r2s_gm_ven <- contrast(emm42_r2s_gm_ven, method = list); c42_r2s_gm_ven;
#c42_r2s_wm     <- contrast(emm42_r2s_wm,     method = list); c42_r2s_wm;
#c42_r2s_wm_dor <- contrast(emm42_r2s_wm_dor, method = list); c42_r2s_wm_dor;
#c42_r2s_wm_lat <- contrast(emm42_r2s_wm_lat, method = list); c42_r2s_wm_lat;
#c42_r2s_wm_ven <- contrast(emm42_r2s_wm_ven, method = list); c42_r2s_wm_ven;



# RESULTS: Atlas-based DTI analysis: Post-hoc tests revealed that
# T1_wm_dor shows a trend to lower values between T4-T6 (p=0.099)




















# TEST: Wilcoxon's test:
wilcox.test(data = dfw_T1_wm_dor, T6~group_bin, exact = FALSE, correct = FALSE, conf.int = FALSE)
wilcox.test(data = dfw_T1_wm_dor, T5~group_bin, exact = FALSE, correct = FALSE, conf.int = FALSE)
wilcox.test(data = dfw_T1_wm_dor, T4~group_bin, exact = FALSE, correct = FALSE, conf.int = FALSE)

t.test(data = dfw_T1_wm_dor, T6~group_bin, alternative = c("two.sided"), paired = FALSE, var.equal = FALSE)
t.test(data = dfw_T1_wm_dor, T5~group_bin, alternative = c("two.sided"), paired = FALSE, var.equal = FALSE)
t.test(data = dfw_T1_wm_dor, T4~group_bin, alternative = c("two.sided"), paired = FALSE, var.equal = FALSE)