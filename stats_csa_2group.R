
# NOT SHOWN IN THE PAPER!!!


# FIT MIXED EFFECT MODEL (segment as categorical variable)
# mixed effect model to investigate group differences over segments
#   - group: fixed effect (2 levels)
#   - segment: fixed effect (12 levels)
#   - group-segment interaction: fixed effect (48 levels)
#   - subject over segments: random effect
#   - area: dependent variable
#
# Model1: area ~ group + segment + segment*subject(group) + segment|subject

ctrl <- lmeControl(maxIter = 1000, msMaxIter = 1000, optimMethod = "optim", msMaxEval = 1000, niterEM = 1000)

summary(lme_sca       <- lme(data = dfl_sca,       area ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_wma       <- lme(data = dfl_wma,       area ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_gma       <- lme(data = dfl_gma,       area ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_gma_dor   <- lme(data = dfl_gma_dor,   area ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_gma_ven   <- lme(data = dfl_gma_ven,   area ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_wma_dor   <- lme(data = dfl_wma_dor,   area ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_wma_lat   <- lme(data = dfl_wma_lat,   area ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_wma_ven   <- lme(data = dfl_wma_ven,   area ~ group_bin*level, random = ~ 1|id, na.action = na.omit, control = ctrl))


# ========== post-hoc test 1: SHAM vs SCI (main effect model) ==========
# We start with the simplest question possible: is the SCI group (the 3 SCI groups combined) differ from the sham group?
# For this, we combine the mild, moderate, and severe groups into a single SCI group.

# get marginal means
emm1_sca       <- emmeans(lme_sca,       specs = ~ group_bin)
emm1_gma       <- emmeans(lme_gma,       specs = ~ group_bin)
emm1_gma_dor   <- emmeans(lme_gma_dor,   specs = ~ group_bin)
emm1_gma_ven   <- emmeans(lme_gma_ven,   specs = ~ group_bin)
emm1_wma       <- emmeans(lme_wma,       specs = ~ group_bin)
emm1_wma_dor   <- emmeans(lme_wma_dor,   specs = ~ group_bin)
emm1_wma_lat   <- emmeans(lme_wma_lat,   specs = ~ group_bin)
emm1_wma_ven   <- emmeans(lme_wma_ven,   specs = ~ group_bin)

# define contrasts
shm <- c(1,0)
sci <- c(0,1)

list <- list("sci-shm" = sci - shm)

# estimate contrasts
c1_sca       <- contrast(emm1_sca,       method = list); c1_sca;
c1_gma       <- contrast(emm1_gma,       method = list); c1_gma;
c1_gma_dor   <- contrast(emm1_gma_dor,   method = list); c1_gma_dor;
c1_gma_ven   <- contrast(emm1_gma_ven,   method = list); c1_gma_ven;
c1_wma       <- contrast(emm1_wma,       method = list); c1_wma;
c1_wma_dor   <- contrast(emm1_wma_dor,   method = list); c1_wma_dor;
c1_wma_lat   <- contrast(emm1_wma_lat,   method = list); c1_wma_lat;
c1_wma_ven   <- contrast(emm1_wma_ven,   method = list); c1_wma_ven;

# RESULTS: Atlas-based segmentation: Post-hoc tests revealed significant differences between SHM and SCI in
# gma     (p=0.0162)
# gma_dor (p=0.0235)
# gma_ven (p=0.0126)
# wma_dor (p=0.0361)


# ========== post-hoc test 2: group-wise analysis per each spinal level  ==========
# We've previously detected significant group differences between the SCI and sham groups.
# Now, we want to test at which levels these differences exist.

# get marginal means
emm2_sca       <- emmeans(lme_sca,       specs = ~ group_bin*level)
emm2_gma       <- emmeans(lme_gma,       specs = ~ group_bin*level)
emm2_gma_dor   <- emmeans(lme_gma_dor,   specs = ~ group_bin*level)
emm2_gma_ven   <- emmeans(lme_gma_ven,   specs = ~ group_bin*level)
emm2_wma       <- emmeans(lme_wma,       specs = ~ group_bin*level)
emm2_wma_dor   <- emmeans(lme_wma_dor,   specs = ~ group_bin*level)
emm2_wma_lat   <- emmeans(lme_wma_lat,   specs = ~ group_bin*level)
emm2_wma_ven   <- emmeans(lme_wma_ven,   specs = ~ group_bin*level)

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
  "sci_C3-shm_C3" = sci_C3 - shm_C3,
  "sci_C4-shm_C4" = sci_C4 - shm_C4,
  "sci_C5-shm_C5" = sci_C5 - shm_C5,
  "sci_C6-shm_C6" = sci_C6 - shm_C6,
  "sci_C7-shm_C7" = sci_C7 - shm_C7,
  "sci_C8-shm_C8" = sci_C8 - shm_C8,
  "sci_T1-shm_T1" = sci_T1 - shm_T1,
  "sci_T2-shm_T2" = sci_T2 - shm_T2,
  "sci_T3-shm_T3" = sci_T3 - shm_T3,
  "sci_T4-shm_T4" = sci_T4 - shm_T4,
  "sci_T5-shm_T5" = sci_T5 - shm_T5,
  "sci_T6-shm_T6" = sci_T6 - shm_T6
)

# estimate contrasts
c2_sca       <- contrast(emm2_sca,       method = list); c2_sca;
c2_gma       <- contrast(emm2_gma,       method = list); c2_gma;
c2_gma_dor   <- contrast(emm2_gma_dor,   method = list); c2_gma_dor;
c2_gma_ven   <- contrast(emm2_gma_ven,   method = list); c2_gma_ven;
c2_wma       <- contrast(emm2_wma,       method = list); c2_wma;
c2_wma_dor   <- contrast(emm2_wma_dor,   method = list); c2_wma_dor;
c2_wma_lat   <- contrast(emm2_wma_lat,   method = list); c2_wma_lat;
c2_wma_ven   <- contrast(emm2_wma_ven,   method = list); c2_wma_ven;

# RESULTS: Atlas-based CSA analysis: Post-hoc tests revealed differences
# gma     is different between SHM and SCI at T2 (p=0.0385), T1 (p=0.0013), C8 (p=0.0098), C5 (p=0.0163), C4 (p=0.0202)
# gma_dor is different between SHM and SCI at T1 (p=0.0205), C5 (p=0.0261), C4 (p=0.0090), C3 (p=0.0102)       
# gma_ven is different between SHM and SCI at T2 (p=0.0345), T1 (p=0.0004), C8 (p=0.0041), C7 (p=0.0376), C5 (p=0.0160), C4 (p= 0.0438)
# wma_dor is different between SHM and SCI at T4 (p=0.0409), T1 (p=0.0210), C5 (p=0.0263)              


# ========== post-hoc test 3: group differences within combined segments ==========
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
c3_sca       <- contrast(emm2_sca,       method = list, adjust = "none"); c3_sca;
c3_gma       <- contrast(emm2_gma,       method = list, adjust = "none"); c3_gma;
c3_gma_dor   <- contrast(emm2_gma_dor,   method = list, adjust = "none"); c3_gma_dor;
c3_gma_ven   <- contrast(emm2_gma_ven,   method = list, adjust = "none"); c3_gma_ven;
c3_wma       <- contrast(emm2_wma,       method = list, adjust = "none"); c3_wma;
c3_wma_dor   <- contrast(emm2_wma_dor,   method = list, adjust = "none"); c3_wma_dor;
c3_wma_lat   <- contrast(emm2_wma_lat,   method = list, adjust = "none"); c3_wma_lat;
c3_wma_ven   <- contrast(emm2_wma_ven,   method = list, adjust = "none"); c3_wma_ven;

# RESULTS (no correction): Atlas-based segmentation
# gma     is different at                    upper-th (p=0.0081), lower-cer (p=0.0255), upper-cer (p=0.0145)  
# gma_dor is different at                    upper-th (p=0.0272)                        upper-cer (p=0.0065)
# gma_ven is different at                    upper-th (p=0.0045), lower-cer (p=0.0143), upper-cer (p=0.0211)
# wma_dor is different at mid-th (p=0.0480), upper-th (p=0.0248),                       upper-cer (p=0.0343)

c3_sca       <- contrast(emm2_sca,       method = list, adjust = "sidak"); c3_sca;
c3_gma       <- contrast(emm2_gma,       method = list, adjust = "sidak"); c3_gma;
c3_gma_dor   <- contrast(emm2_gma_dor,   method = list, adjust = "sidak"); c3_gma_dor;
c3_gma_ven   <- contrast(emm2_gma_ven,   method = list, adjust = "sidak"); c3_gma_ven;
c3_wma       <- contrast(emm2_wma,       method = list, adjust = "sidak"); c3_wma;
c3_wma_dor   <- contrast(emm2_wma_dor,   method = list, adjust = "sidak"); c3_wma_dor;
c3_wma_lat   <- contrast(emm2_wma_lat,   method = list, adjust = "sidak"); c3_wma_lat;
c3_wma_ven   <- contrast(emm2_wma_ven,   method = list, adjust = "sidak"); c3_wma_ven;

# RESULTS (Sidak correction): Atlas-based segmentation
# gma     is different at                    upper-th (p=0.0322) 
# gma_dor is different at                                                               upper-cer (p=0.0256)
# gma_ven is different at                    upper-th (p=0.0179)