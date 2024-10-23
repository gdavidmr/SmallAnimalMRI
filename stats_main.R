library(dplyr)
library(emmeans)
library(ggplot2)
library(lme4)
library(multcomp)
library(nlme)
library(rlang)
library(sciplot)
library(sjmisc)
library(tidyverse)

# set and change working directory
DIR <- 'E:\\projects\\small_animal_mri\\05_results'
setwd(DIR)

# ==============================================================================================
# ===================================== LOADING IN DATAFRAMES ==================================
# ==============================================================================================


# read dataframe containing relaxometry and csa data (long format)
DATAl <- read.csv('relaxometry_groupstats_avg.csv', header = TRUE, sep = ',')

# change column names
colnames(DATAl)[6]  <- "level"
colnames(DATAl)[9]  <- "area"
colnames(DATAl)[12] <- "area_sc"

# convert number of voxels to area (in mm^2)
DATAl$area <- DATAl$area*0.007956

# typesetting
DATAl$group    <- as.factor(DATAl$group)
DATAl$contrast <- as.factor(DATAl$contrast)
DATAl$tissue   <- as.factor(DATAl$tissue)
DATAl$level    <- as.factor(DATAl$level)

# create new variables
DATAl <- DATAl %>% 
  mutate(group_bin = case_when(group == 'SHM' ~ 0,
                            group == 'MLD' ~ 1,
                            group == 'MOD' ~ 1,
                            group == 'SEV' ~ 1,))
#DATAl$segment <- as.integer(DATAl$level)



# reshape dataframe
DATAl0 <- DATAl[(DATAl$contrast=="T1" & DATAl$tissue=="gm" & DATAl$BBB_day==1),]
DATAl0 <- select_(DATAl0, "id", "group", "level","area_sc")
DATAw0 <- reshape(DATAl0, idvar = "id", v.names = c("area_tot"), timevar = "level", direction = "wide")

# count the number of samples per group
sum(DATAw0$group=="SHM")
sum(DATAw0$group=="MLD")
sum(DATAw0$group=="MOD")
sum(DATAw0$group=="SEV")

level_names <- c("C3","C4","C5","C6","C7","C8","T1","T2","T3","T4","T5","T6")
group_names <- c("SHM","MLD","MOD","SEV")

# add columns for quadratic components of time and level
#DATAl$BBB_day2 <- DATAl$BBB_day^2
#DATAl$segment2 <- DATAl$segment^2


# ==============================================================================================
# load in dataframes containing csa from atlas-based analysis

load_dataframe_csa_wide <- function(filename) {
  
  # loading in dataframe
  dfw <- read.csv(paste(DIR, 'experiment3_mri_atlas_based', 'CSA', filename, sep = "\\"), header = TRUE, sep = ',')
  colnames(dfw) <- c('id','group','group_bin','C1','C2','C3','C4','C5','C6','C7','C8','T1','T2','T3','T4','T5','T6','T7')
  
  # typesetting
  dfw$group     <- as.factor(dfw$group);
  dfw$group     <- factor(dfw$group, levels=group_names)
  dfw$group_bin <- as.factor(dfw$group_bin);
  
  # remove spinal levels C1, C3, and T7: too many missing values
  dfw <- subset(dfw, select = -c(C1,C2,T7));
  
  return(dfw)
}

load_dataframe_csa_long <- function(filename) {
  df <- read.csv(paste(DIR, 'experiment3_mri_atlas_based', 'CSA', filename, sep = "\\"), header = TRUE, sep = ',')
  dfl <- reshape(df, direction = "long",
                    varying = colnames(df)[4:ncol(df)],
                    v.names = "area",
                    timevar = "level",
                    times = c('C1','C2','C3','C4','C5','C6','C7','C8','T1','T2','T3','T4','T5','T6','T7'),
                    idvar='id')
  
  # typesetting
  dfl$group     <- as.factor(dfl$group);
  dfl$group     <- factor(dfl$group, levels=group_names)
  dfl$group_bin <- as.factor(dfl$group_bin);
  
  # remove spinal levels C1, C3, and T7: too many missing values
  dfl <- subset(dfl, level != 'C1' & level != 'C2' & level != 'T7');
  
  return(dfl)
}

dfw_sca       <- load_dataframe_csa_wide('table-merged_sca.csv')
dfw_gma       <- load_dataframe_csa_wide('table-merged_gma.csv')
dfw_gma_dor   <- load_dataframe_csa_wide('table-merged_gma_dor.csv')
dfw_gma_ven   <- load_dataframe_csa_wide('table-merged_gma_ven.csv')
dfw_wma       <- load_dataframe_csa_wide('table-merged_wma.csv')
dfw_wma_dor   <- load_dataframe_csa_wide('table-merged_wma_dor.csv')
dfw_wma_lat   <- load_dataframe_csa_wide('table-merged_wma_lat.csv')
dfw_wma_lat_l <- load_dataframe_csa_wide('table-merged_wma_lat_l.csv')
dfw_wma_lat_r <- load_dataframe_csa_wide('table-merged_wma_lat_r.csv')
dfw_wma_ven   <- load_dataframe_csa_wide('table-merged_wma_ven.csv')
dfl_sca       <- load_dataframe_csa_long('table-merged_sca.csv')
dfl_gma       <- load_dataframe_csa_long('table-merged_gma.csv')
dfl_gma_dor   <- load_dataframe_csa_long('table-merged_gma_dor.csv')
dfl_gma_ven   <- load_dataframe_csa_long('table-merged_gma_ven.csv')
dfl_wma       <- load_dataframe_csa_long('table-merged_wma.csv')
dfl_wma_dor   <- load_dataframe_csa_long('table-merged_wma_dor.csv')
dfl_wma_lat   <- load_dataframe_csa_long('table-merged_wma_lat.csv')
dfl_wma_lat_l <- load_dataframe_csa_long('table-merged_wma_lat_l.csv')
dfl_wma_lat_r <- load_dataframe_csa_long('table-merged_wma_lat_r.csv')
dfl_wma_ven   <- load_dataframe_csa_long('table-merged_wma_ven.csv')


# exclude single subject (id=191) from the CSA analysis. Note that this subject does not have DTI data, so no exclusion is needed there.
#dfw2_sca       <- subset(dfw_sca,       id != 191);
#dfw2_gma       <- subset(dfw_gma,       id != 191);
#dfw2_gma_dor   <- subset(dfw_gma_dor,   id != 191);
#dfw2_gma_ven   <- subset(dfw_gma_ven,   id != 191);
#dfw2_wma       <- subset(dfw_wma,       id != 191);
#dfw2_wma_dor   <- subset(dfw_wma_dor,   id != 191);
#dfw2_wma_lat   <- subset(dfw_wma_lat,   id != 191);
#dfw2_wma_lat_l <- subset(dfw_wma_lat_l, id != 191);
#dfw2_wma_lat_r <- subset(dfw_wma_lat_r, id != 191);
#dfw2_wma_ven   <- subset(dfw_wma_ven,   id != 191);
#dfl2_sca       <- subset(dfl_sca,       id != 191);
#dfl2_gma       <- subset(dfl_gma,       id != 191);
#dfl2_gma_dor   <- subset(dfl_gma_dor,   id != 191);
#dfl2_gma_ven   <- subset(dfl_gma_ven,   id != 191);
#dfl2_wma       <- subset(dfl_wma,       id != 191);
#dfl2_wma_dor   <- subset(dfl_wma_dor,   id != 191);
#dfl2_wma_lat   <- subset(dfl_wma_lat,   id != 191);
#dfl2_wma_lat_l <- subset(dfl_wma_lat_l, id != 191);
#dfl2_wma_lat_r <- subset(dfl_wma_lat_r, id != 191);
#dfl2_wma_ven   <- subset(dfl_wma_ven,   id != 191);




# ==============================================================================================
# load in dataframes containing dti metrics from atlas-based analysis

load_dataframe_dti_wide <- function(filename) {
  
  # loading in dataframe
  dfw <- read.csv(paste(DIR, 'experiment3_mri_atlas_based', 'DTI', 'eroded_1x', filename, sep = "\\"), header = TRUE, sep = ',')
  colnames(dfw) <- c('id','group','group_bin','C1','C2','C3','C4','C5','C6','C7','C8','T1','T2','T3','T4','T5','T6','T7')
  
  # typesetting
  dfw$group     <- as.factor(dfw$group);
  dfw$group     <- factor(dfw$group, levels=group_names)
  dfw$group_bin <- as.factor(dfw$group_bin); 
  
  # remove spinal levels C1, C3, and T7: too many missing values
  dfw <- subset(dfw, select = -c(C1,C2,T7));
  
  return(dfw)
}

load_dataframe_dti_long <- function(filename) {
  df  <- read.csv(paste(DIR, 'experiment3_mri_atlas_based', 'DTI', 'eroded_1x', filename, sep = "\\"), header = TRUE, sep = ',')
  dfl <- reshape(df, direction = "long",
                 varying = colnames(df)[4:ncol(df)],
                 v.names = "value",
                 timevar = "level",
                 times = c('C1','C2','C3','C4','C5','C6','C7','C8','T1','T2','T3','T4','T5','T6','T7'),
                 idvar='id')
  
  # typesetting
  dfl$group     <- as.factor(dfl$group);
  dfl$group     <- factor(dfl$group, levels=group_names)
  dfl$group_bin <- as.factor(dfl$group_bin);
  
  # remove spinal levels C1, C3, and T7: too many missing values
  dfl <- subset(dfl, level != 'C1' & level != 'C2' & level != 'T7');
  
  return(dfl)
}

dfw_fa_gm     <- load_dataframe_dti_wide('table-merged_fa_gm.csv')
dfw_fa_gm_dor <- load_dataframe_dti_wide('table-merged_fa_gm_dor.csv')
dfw_fa_gm_ven <- load_dataframe_dti_wide('table-merged_fa_gm_ven.csv')
dfw_fa_wm     <- load_dataframe_dti_wide('table-merged_fa_wm.csv')
dfw_fa_wm_dor <- load_dataframe_dti_wide('table-merged_fa_wm_dor.csv')
dfw_fa_wm_lat <- load_dataframe_dti_wide('table-merged_fa_wm_lat.csv')
dfw_fa_wm_ven <- load_dataframe_dti_wide('table-merged_fa_wm_ven.csv')
dfw_md_gm     <- load_dataframe_dti_wide('table-merged_md_gm.csv')
dfw_md_gm_dor <- load_dataframe_dti_wide('table-merged_md_gm_dor.csv')
dfw_md_gm_ven <- load_dataframe_dti_wide('table-merged_md_gm_ven.csv')
dfw_md_wm     <- load_dataframe_dti_wide('table-merged_md_wm.csv')
dfw_md_wm_dor <- load_dataframe_dti_wide('table-merged_md_wm_dor.csv')
dfw_md_wm_lat <- load_dataframe_dti_wide('table-merged_md_wm_lat.csv')
dfw_md_wm_ven <- load_dataframe_dti_wide('table-merged_md_wm_ven.csv')
dfw_ad_gm     <- load_dataframe_dti_wide('table-merged_ad_gm.csv')
dfw_ad_gm_dor <- load_dataframe_dti_wide('table-merged_ad_gm_dor.csv')
dfw_ad_gm_ven <- load_dataframe_dti_wide('table-merged_ad_gm_ven.csv')
dfw_ad_wm     <- load_dataframe_dti_wide('table-merged_ad_wm.csv')
dfw_ad_wm_dor <- load_dataframe_dti_wide('table-merged_ad_wm_dor.csv')
dfw_ad_wm_lat <- load_dataframe_dti_wide('table-merged_ad_wm_lat.csv')
dfw_ad_wm_ven <- load_dataframe_dti_wide('table-merged_ad_wm_ven.csv')
dfw_rd_gm     <- load_dataframe_dti_wide('table-merged_rd_gm.csv')
dfw_rd_gm_dor <- load_dataframe_dti_wide('table-merged_rd_gm_dor.csv')
dfw_rd_gm_ven <- load_dataframe_dti_wide('table-merged_rd_gm_ven.csv')
dfw_rd_wm     <- load_dataframe_dti_wide('table-merged_rd_wm.csv')
dfw_rd_wm_dor <- load_dataframe_dti_wide('table-merged_rd_wm_dor.csv')
dfw_rd_wm_lat <- load_dataframe_dti_wide('table-merged_rd_wm_lat.csv')
dfw_rd_wm_ven <- load_dataframe_dti_wide('table-merged_rd_wm_ven.csv')
dfl_fa_gm     <- load_dataframe_dti_long('table-merged_fa_gm.csv')
dfl_fa_gm_dor <- load_dataframe_dti_long('table-merged_fa_gm_dor.csv')
dfl_fa_gm_ven <- load_dataframe_dti_long('table-merged_fa_gm_ven.csv')
dfl_fa_wm     <- load_dataframe_dti_long('table-merged_fa_wm.csv')
dfl_fa_wm_dor <- load_dataframe_dti_long('table-merged_fa_wm_dor.csv')
dfl_fa_wm_lat <- load_dataframe_dti_long('table-merged_fa_wm_lat.csv')
dfl_fa_wm_ven <- load_dataframe_dti_long('table-merged_fa_wm_ven.csv')
dfl_md_gm     <- load_dataframe_dti_long('table-merged_md_gm.csv')
dfl_md_gm_dor <- load_dataframe_dti_long('table-merged_md_gm_dor.csv')
dfl_md_gm_ven <- load_dataframe_dti_long('table-merged_md_gm_ven.csv')
dfl_md_wm     <- load_dataframe_dti_long('table-merged_md_wm.csv')
dfl_md_wm_dor <- load_dataframe_dti_long('table-merged_md_wm_dor.csv')
dfl_md_wm_lat <- load_dataframe_dti_long('table-merged_md_wm_lat.csv')
dfl_md_wm_ven <- load_dataframe_dti_long('table-merged_md_wm_ven.csv')
dfl_ad_gm     <- load_dataframe_dti_long('table-merged_ad_gm.csv')
dfl_ad_gm_dor <- load_dataframe_dti_long('table-merged_ad_gm_dor.csv')
dfl_ad_gm_ven <- load_dataframe_dti_long('table-merged_ad_gm_ven.csv')
dfl_ad_wm     <- load_dataframe_dti_long('table-merged_ad_wm.csv')
dfl_ad_wm_dor <- load_dataframe_dti_long('table-merged_ad_wm_dor.csv')
dfl_ad_wm_lat <- load_dataframe_dti_long('table-merged_ad_wm_lat.csv')
dfl_ad_wm_ven <- load_dataframe_dti_long('table-merged_ad_wm_ven.csv')
dfl_rd_gm     <- load_dataframe_dti_long('table-merged_rd_gm.csv')
dfl_rd_gm_dor <- load_dataframe_dti_long('table-merged_rd_gm_dor.csv')
dfl_rd_gm_ven <- load_dataframe_dti_long('table-merged_rd_gm_ven.csv')
dfl_rd_wm     <- load_dataframe_dti_long('table-merged_rd_wm.csv')
dfl_rd_wm_dor <- load_dataframe_dti_long('table-merged_rd_wm_dor.csv')
dfl_rd_wm_lat <- load_dataframe_dti_long('table-merged_rd_wm_lat.csv')
dfl_rd_wm_ven <- load_dataframe_dti_long('table-merged_rd_wm_ven.csv')




# ==============================================================================================
# load in dataframes containing T1 from atlas-based analysis

load_dataframe_T1_wide <- function(filename) {
  
  # loading in dataframe
  dfw <- read.csv(paste(DIR, '01_atlas_based', 'T1', 'eroded_1x', filename, sep = "\\"), header = TRUE, sep = ',')
  colnames(dfw) <- c('id','group','group_bin','C1','C2','C3','C4','C5','C6','C7','C8','T1','T2','T3','T4','T5','T6','T7')
  
  # typesetting
  dfw$group     <- as.factor(dfw$group);
  dfw$group_bin <- as.factor(dfw$group_bin); 
  
  # remove spinal levels C1, C3, and T7: too many missing values
  dfw <- subset(dfw, select = -c(C1,C2,T7));
  
  # set entries with T1>2500 to NaN
  dfw[dfw > 2500] <- NaN
  
  return(dfw)
}

load_dataframe_T1_long <- function(filename) {
  df  <- read.csv(paste(DIR, '01_atlas_based', 'T1', 'eroded_1x', filename, sep = "\\"), header = TRUE, sep = ',')
  
  # set entries with T1>2500 to NaN
  df[df > 2500] <- NaN

  dfl <- reshape(df, direction = "long",
                 varying = colnames(df)[4:ncol(df)],
                 v.names = "value",
                 timevar = "level",
                 times = c('C1','C2','C3','C4','C5','C6','C7','C8','T1','T2','T3','T4','T5','T6','T7'),
                 idvar='id')
  
  # typesetting
  dfl$group     <- as.factor(dfl$group);
  dfl$group_bin <- as.factor(dfl$group_bin);
  
  # remove spinal levels C1, C3, and T7: too many missing values
  dfl <- subset(dfl, level != 'C1' & level != 'C2' & level != 'T7');
  
  return(dfl)
}

dfw_T1_gm     <- load_dataframe_T1_wide('table-merged_T1_gm.csv')
dfw_T1_gm_dor <- load_dataframe_T1_wide('table-merged_T1_gm_dor.csv')
dfw_T1_gm_ven <- load_dataframe_T1_wide('table-merged_T1_gm_ven.csv')
dfw_T1_wm     <- load_dataframe_T1_wide('table-merged_T1_wm.csv')
dfw_T1_wm_dor <- load_dataframe_T1_wide('table-merged_T1_wm_dor.csv')
dfw_T1_wm_lat <- load_dataframe_T1_wide('table-merged_T1_wm_lat.csv')
dfw_T1_wm_ven <- load_dataframe_T1_wide('table-merged_T1_wm_ven.csv')
dfl_T1_gm     <- load_dataframe_T1_long('table-merged_T1_gm.csv')
dfl_T1_gm_dor <- load_dataframe_T1_long('table-merged_T1_gm_dor.csv')
dfl_T1_gm_ven <- load_dataframe_T1_long('table-merged_T1_gm_ven.csv')
dfl_T1_wm     <- load_dataframe_T1_long('table-merged_T1_wm.csv')
dfl_T1_wm_dor <- load_dataframe_T1_long('table-merged_T1_wm_dor.csv')
dfl_T1_wm_lat <- load_dataframe_T1_long('table-merged_T1_wm_lat.csv')
dfl_T1_wm_ven <- load_dataframe_T1_long('table-merged_T1_wm_ven.csv')







# ==============================================================================================
# ============================== PART I. ANALYSIS OF BBB SCORES ================================
# ==============================================================================================
# run script stats_bbb.R



# ======================================================================================
# ==================== PART II. ANALYSIS OF CROSS-SECTIONAL AREAS ======================
# ======================================================================================
# run script stats_csa.R


# ======================================================================================
# ==================== PART III. ANALYSIS OF DTI METRICS ===============================
# ======================================================================================
# run script stats_dti.R



# =======================================================================================
# ============================== PART IV. ANALYSIS OF RELAXOMETRY =======================
# =======================================================================================
# run script stats_relaxometry.R




# ================================================================================
# ======================== PART V. CORRELATION ANALYSIS =========================
# correlations - improvement in BBB score vs. endpoint cross-sectional areas
# ================================================================================

# extract BBB scores from dataframe DATAl, EXCLUDING sham
DATAl5 <- DATAl[(!DATAl$group=="SHAM" &
                    !DATAl$level=="C1" &
                    !DATAl$level=="C2" &
                    !DATAl$level=="T7" &
                    DATAl$tissue=="wm" &
                    DATAl$level=="T2" &
                    DATAl$contrast=="T1"),]

# extract BBB scores from dataframe DATAl, INCLUDING sham
DATAl5f <- DATAl[(!DATAl$level=="C1" &
                   !DATAl$level=="C2" &
                   !DATAl$level=="T7" &
                   DATAl$tissue=="wm" &
                   DATAl$level=="T2" &
                   DATAl$contrast=="T1"),]

BBB_score_day01     <- DATAl5[DATAl5$BBB_day==1,]$BBB_score
BBB_score_day84     <- DATAl5[DATAl5$BBB_day==84,]$BBB_score
BBB_score_delta     <- BBB_score_day84  - BBB_score_day01
BBB_score_delta_rel <- (BBB_score_day84 - BBB_score_day01)/(21 - BBB_score_day01)

BBB_score_day01_f     <- DATAl5f[DATAl5f$BBB_day==1,]$BBB_score
BBB_score_day84_f     <- DATAl5f[DATAl5f$BBB_day==84,]$BBB_score
BBB_score_delta_f     <- BBB_score_day84_f  - BBB_score_day01_f
BBB_score_delta_rel_f <- (BBB_score_day84_f - BBB_score_day01_f)/(21 - BBB_score_day01_f)


# compute correlations between BBB scores and the MRI metrics in each of the 4 segments
compute_correlation_persegment <- function(df, BBB_score, BBB_score_id, type, name_BBB_score, name_MRI_score, dummy_sham=0) {
  
  # dealing with sham
  if (dummy_sham==0) {df <- df[df$group_bin==1,]}
  
  # get mean value between segment1 (C3-C5), segment2 (C6-C8), segment3 (T1-T3), and segment4 (T4-T6)
  df$Mean1 <- rowMeans(subset(df, select=c("C3","C4","C5")), na.rm = TRUE);
  df$Mean2 <- rowMeans(subset(df, select=c("C6","C7","C8")), na.rm = TRUE);
  df$Mean3 <- rowMeans(subset(df, select=c("T1","T2","T3")), na.rm = TRUE);
  df$Mean4 <- rowMeans(subset(df, select=c("T4","T5","T6")), na.rm = TRUE);
  
  # create dataframe for BBB score
  df_BBB <- data.frame(BBB_score_id, BBB_score)
  colnames(df_BBB) <- c("id", "BBB_score")

  # inner join of the dataframes
  df2 <- merge(df, df_BBB, by = "id")
  
  # Pearson's correlation
  if (type=="pearson") {
    
    print(cor.test(df2$Mean1, df2$BBB_score, method = "pearson", exact=FALSE))
    print(cor.test(df2$Mean2, df2$BBB_score, method = "pearson", exact=FALSE))
    print(cor.test(df2$Mean3, df2$BBB_score, method = "pearson", exact=FALSE))
    print(cor.test(df2$Mean4, df2$BBB_score, method = "pearson", exact=FALSE))
  }
  
  # Spearman rank sum correlation
  if (type=="spearman") {
    print(cor.test(df2$Mean1, df2$BBB_score, method = "spearman", exact=FALSE))
    print(cor.test(df2$Mean2, df2$BBB_score, method = "spearman", exact=FALSE))
    print(cor.test(df2$Mean3, df2$BBB_score, method = "spearman", exact=FALSE))
    print(cor.test(df2$Mean4, df2$BBB_score, method = "spearman", exact=FALSE))
  }
}



# ==============================================================================================================
# Pearson's correlation with BBB score at day84 - INCLUDING SHAM
compute_correlation_persegment(dfw_sca,       BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "sca", 1)
compute_correlation_persegment(dfw_gma,       BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "gma", 1)
compute_correlation_persegment(dfw_gma_dor,   BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "gma_dor", 1)
compute_correlation_persegment(dfw_gma_ven,   BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "gma_ven", 1)
compute_correlation_persegment(dfw_wma,       BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "wma", 1)
compute_correlation_persegment(dfw_wma_dor,   BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "wma_dor", 1)
compute_correlation_persegment(dfw_wma_lat,   BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "wma_lat", 1)
compute_correlation_persegment(dfw_wma_ven,   BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "wma_ven", 1)

compute_correlation_persegment(dfw_fa_gm,     BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "fa_gm", 1)
compute_correlation_persegment(dfw_fa_gm_dor, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "fa_gm_dor", 1)
compute_correlation_persegment(dfw_fa_gm_ven, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "fa_gm_ven", 1)
compute_correlation_persegment(dfw_fa_wm,     BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "fa_wm", 1)
compute_correlation_persegment(dfw_fa_wm_dor, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "fa_wm_dor", 1)
compute_correlation_persegment(dfw_fa_wm_lat, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "fa_wm_lat", 1)
compute_correlation_persegment(dfw_fa_wm_ven, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "fa_wm_ven", 1)
compute_correlation_persegment(dfw_md_gm,     BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "md_gm", 1)
compute_correlation_persegment(dfw_md_gm_dor, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "md_gm_dor", 1)
compute_correlation_persegment(dfw_md_gm_ven, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "md_gm_ven", 1)
compute_correlation_persegment(dfw_md_wm,     BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "md_wm", 1)
compute_correlation_persegment(dfw_md_wm_dor, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "md_wm_dor", 1)
compute_correlation_persegment(dfw_md_wm_lat, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "md_wm_lat", 1)
compute_correlation_persegment(dfw_md_wm_ven, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "md_wm_ven", 1)
compute_correlation_persegment(dfw_ad_gm,     BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "ad_gm", 1)
compute_correlation_persegment(dfw_ad_gm_dor, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "ad_gm_dor", 1)
compute_correlation_persegment(dfw_ad_gm_ven, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "ad_gm_ven", 1)
compute_correlation_persegment(dfw_ad_wm,     BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "ad_wm", 1)
compute_correlation_persegment(dfw_ad_wm_dor, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "ad_wm_dor", 1)
compute_correlation_persegment(dfw_ad_wm_lat, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "ad_wm_lat", 1)
compute_correlation_persegment(dfw_ad_wm_ven, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "ad_wm_ven", 1)
compute_correlation_persegment(dfw_rd_gm,     BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "rd_gm", 1)
compute_correlation_persegment(dfw_rd_gm_dor, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "rd_gm_dor", 1)
compute_correlation_persegment(dfw_rd_gm_ven, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "rd_gm_ven", 1)
compute_correlation_persegment(dfw_rd_wm,     BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "rd_wm", 1)
compute_correlation_persegment(dfw_rd_wm_dor, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "rd_wm_dor", 1)
compute_correlation_persegment(dfw_rd_wm_lat, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "rd_wm_lat", 1)
compute_correlation_persegment(dfw_rd_wm_ven, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "rd_wm_ven", 1)

compute_correlation_persegment(dfw_T1_gm,     BBB_score_day84, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "T1_gm", 1)
compute_correlation_persegment(dfw_T1_gm_dor, BBB_score_day84, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "T1_gm_dor", 1)
compute_correlation_persegment(dfw_T1_gm_ven, BBB_score_day84, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "T1_gm_ven", 1)
compute_correlation_persegment(dfw_T1_wm,     BBB_score_day84, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "T1_wm", 1)
compute_correlation_persegment(dfw_T1_wm_dor, BBB_score_day84, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "T1_wm_dor", 1)
compute_correlation_persegment(dfw_T1_wm_lat, BBB_score_day84, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "T1_wm_lat", 1)
compute_correlation_persegment(dfw_T1_wm_ven, BBB_score_day84, DATAl5f[DATAl5f$BBB_day==84,]$id, "pearson", "BBB_day84", "T1_wm_ven", 1)

# RESULTS for BBB_score_day84:
# correlations in the expected direction:
#   gma     between T4-T6 (p=0.050)
#   gma_dor between T4-T6 (p=0.035),                  C6-C8 (p=0.043)
#   wma_dor between T4-T6 (p=0.044), T1-T3 (p=0.049)

#   fa_wm     between T4-T6 (p=0.022)
#   fa_wm_lat between T4-T6 (p=0.005)

#   md_gm_dor between                               C6-C8 (p=0.023)
#   md_wm     between                               C6-C8 (p=0.040), C3-C5 (p=0.004)

#   ad_gm     between                                                C3-C5 (p=0.033)
#   ad_gm_dor between                               C6-C8 (p=0.013), C3-C5 (p=0.034)
#   ad_wm     between T4-T6 (p=0.046),              C6-C8 (p=0.012), C3-C5 (p=0.001)
#   ad_wm_dor between T4-T6 (p=0.009),              C6-C8 (p=0.011), C3-C5 (p=0.013)


# ==============================================================================================================
# Spearman correlation with BBB score at day84 INCLUDING sham
compute_correlation(dfw_sca,       BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "sca")
compute_correlation(dfw_gma,       BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "gma")
compute_correlation(dfw_gma_dor,   BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "gma_dor")
compute_correlation(dfw_gma_ven,   BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "gma_ven")
compute_correlation(dfw_wma,       BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "wma")
compute_correlation(dfw_wma_dor,   BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "wma_dor")
compute_correlation(dfw_wma_lat,   BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "wma_lat")
compute_correlation(dfw_wma_ven,   BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "wma_ven")

compute_correlation(dfw_fa_gm,     BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "fa_gm")
compute_correlation(dfw_fa_gm_dor, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "fa_gm_dor")
compute_correlation(dfw_fa_gm_ven, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "fa_gm_ven")
compute_correlation(dfw_fa_wm,     BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "fa_wm")
compute_correlation(dfw_fa_wm_dor, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "fa_wm_dor")
compute_correlation(dfw_fa_wm_lat, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "fa_wm_lat")
compute_correlation(dfw_fa_wm_ven, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "fa_wm_ven")
compute_correlation(dfw_md_gm,     BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "md_gm")
compute_correlation(dfw_md_gm_dor, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "md_gm_dor")
compute_correlation(dfw_md_gm_ven, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "md_gm_ven")
compute_correlation(dfw_md_wm,     BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "md_wm")
compute_correlation(dfw_md_wm_dor, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "md_wm_dor")
compute_correlation(dfw_md_wm_lat, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "md_wm_lat")
compute_correlation(dfw_md_wm_ven, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "md_wm_ven")
compute_correlation(dfw_ad_gm,     BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "ad_gm")
compute_correlation(dfw_ad_gm_dor, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "ad_gm_dor")
compute_correlation(dfw_ad_gm_ven, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "ad_gm_ven")
compute_correlation(dfw_ad_wm    , BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "ad_wm")
compute_correlation(dfw_ad_wm_dor, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "ad_wm_dor")
compute_correlation(dfw_ad_wm_lat, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "ad_wm_lat")
compute_correlation(dfw_ad_wm_ven, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "ad_wm_ven")
compute_correlation(dfw_rd_gm,     BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "rd_gm")
compute_correlation(dfw_rd_gm_dor, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "rd_gm_dor")
compute_correlation(dfw_rd_gm_ven, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "rd_gm_ven")
compute_correlation(dfw_rd_wm,     BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "rd_wm")
compute_correlation(dfw_rd_wm_dor, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "rd_wm_dor")
compute_correlation(dfw_rd_wm_lat, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "rd_wm_lat")
compute_correlation(dfw_rd_wm_ven, BBB_score_day84_f, DATAl5[DATAl5f$BBB_day==84,]$id, "spearman", "BBB_day84", "rd_wm_ven")

# RESULTS for BBB_score_day84:

#   gma        between T4-T6 (p=0.022), T1-T3 (p=0.038)
#   gma_dor    between T4-T6 (p=0.010),                   C6-C8 (p=0.028)
#   gma_ven    between T4-T6 (p=0.038), T1-T3 (p=0.027)
#   wma_dor    between T4-T6 (p=0.029), T1-T3 (p=0.034)

#   fa_wm_dor between T1-T3 (p=0.028), C6-C8 (p=0.024)

#   md_gm     between T4-T6 (p=0.048)
#   md_gm_dor between                  T1-T3 (p=0.020)
#   md_gm_ven between T4-T6 (p=0.048)
#   md_wm     between T4-T6 (p=0.005), T1-T3 (p=0.026)
#   md_wm_dor between T4-T6 (p=0.004), T1-T3 (p=0.008), C6-C8 (p=0.053), C3-C5 (p=0.020)
#   md_wm_lat between T4-T6 (p=0.014)

#   ad_gm     between T4-T6 (p=0.035),                  C6-C8 (0.050)
#   ad_gm_dor between T4-T6 (p=0.043), T1-T3 (p=0.033), C6-C8 (p=0.023)
#   ad_wm     between T4-T6 (p=0.002), T1-T3 (p=0.010)
#   ad_wm_dor between T4-T6 (p=0.008), T1-T3 (p=0.006)
#   ad_wm_lat between T4-T6 (p=0.006), T1-T3 (p=0.042)

#   rd_gm_dor between                  T1-T3 (p=0.038)
#   rd_wm     between T4-T6 (p=0.002), T1-T3 (p=0.027)
#   rd_wm_dor between T4-T6 (p=0.016), T1-T3 (p=0.002), , C6-C8 (p=0.006), C3-C5 (p=0.050)
#   rd_wm_lat between T4-T6 (p=0.027)








# ==============================================================================================================
# scatterplots

plot_scatter <- function(df, region, BBB_score, BBB_score_id, xlabel="MRI metric", ylabel="BBB score", fout, dummy_sham=0, dummy_rank=0) {
  
  # dealing with sham
  if (dummy_sham==0) {df <- df[!df$group=="SHM",]}
  
  # get mean values between C3-C5, C6-C8, T1-T3, and T4-T6
  df$Mean1 <- rowMeans(subset(df, select=c("C3","C4","C5")), na.rm = TRUE);
  df$Mean2 <- rowMeans(subset(df, select=c("C6","C7","C8")), na.rm = TRUE);
  df$Mean3 <- rowMeans(subset(df, select=c("T1","T2","T3")), na.rm = TRUE);
  df$Mean4 <- rowMeans(subset(df, select=c("T4","T5","T6")), na.rm = TRUE);
  
  # create dataframe for BBB score
  df_BBB <- data.frame(BBB_score_id, BBB_score)
  colnames(df_BBB) <- c("id", "BBB_score")
  
  # inner join of the dataframes
  df2 <- merge(df, df_BBB, by = "id")
  
  # if dummy_rank==1, take the ranks of the BBB scores and the mean values (Mean1 ... Mean4)
  if (dummy_rank==1) {
    df2$BBB_score <- rank(df2$BBB_score)
    df2$Mean1 <- rank(df2$Mean1)
    df2$Mean2 <- rank(df2$Mean2)
    df2$Mean3 <- rank(df2$Mean3)
    df2$Mean4 <- rank(df2$Mean4)
  }
  
  # what to plot
  y <- df2$BBB_score
  if (region==1) {x <- df2$Mean1}
  if (region==2) {x <- df2$Mean2}
  if (region==3) {x <- df2$Mean3}
  if (region==4) {x <- df2$Mean4}
  
  # get regression line
  if (region==1) {m <- lm(data=df2, BBB_score ~ Mean1)}
  if (region==2) {m <- lm(data=df2, BBB_score ~ Mean2)}
  if (region==3) {m <- lm(data=df2, BBB_score ~ Mean3)}
  if (region==4) {m <- lm(data=df2, BBB_score ~ Mean4)}
  
  ggplot() +
    geom_point(aes(x=x, y=y, shape=df2$group, fill=df2$group), size=5, color="black") +
    scale_shape_manual(values=c(22,21,24,23)) +
    scale_fill_manual(values=c("grey30","grey53","grey76","grey100")) +
    geom_abline(slope=m$coefficients[2], intercept=m$coefficients[1], color="black") +
    ylim(c(0,21)) +
    labs(x=xlabel, y=ylabel) +
    theme_classic() +
    theme(axis.text.x = element_text(size=22)) +
    theme(axis.text.y = element_text(size=22)) +
    theme(axis.title = element_text(size=24))
  ggsave(paste(DIR, 'figures', 'scatterplots', fout, sep='\\'), height=6.7, width=8)
  
}


# significant correlations INCLUDING sham
# those that showed significant group difference

# SHOWN IN THE PAPER:
plot_scatter(dfw_wma_dor,   4, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "CSA of Dorsal White Matter Column [mm2]", "BBB Score", "wma_dor_T4T6-vs-bbb_dpi84.png",   1)
plot_scatter(dfw_gma,       4, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "CSA of Gray Matter [mm2]",                "BBB Score", "gma_T4T6-vs-bbb_dpi84.png",       1)
plot_scatter(dfw_fa_wm_lat, 4, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "FA in Lateral White Matter Column",       "BBB Score", "fa_wm_lat_T4T6-vs-bbb_dpi84.png", 1)



# other correlations:
plot_scatter(dfw_wma_dor, 3, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "CSA of dorsal WM column at T1-T3 [mm2]", "84-dpi BBB score", "wma_dor_T1T3-vs-bbb_dpi84.png", 1)
plot_scatter(dfw_gma_dor, 2, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "CSA of dorsal GM horns at C6-C8 [mm2]", "84-dpi BBB score", "gma_dor_C6C8-vs-bbb_dpi84.png", 1)
plot_scatter(dfw_gma_dor, 4, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "CSA of dorsal GM horns at T4-T6 [mm2]", "84-dpi BBB score", "gma_dor_T4T6-vs-bbb_dpi84.png", 1)

plot_scatter(dfw_ad_wm_dor, 4, BBB_score_day84_f, DATAl5f[DATAl5f$BBB_day==84,]$id, "AD in Dorsal White Matter Column",       "BBB Score", "ad_wm_dor_T4T6-vs-bbb_dpi84.png", 1)