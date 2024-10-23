# ==============================================================================================================
# linear correlation with absolute delta BBB score (BBB_day84 - BBB_day01)
compute_correlation(dfw_sca,       BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "sca")
compute_correlation(dfw_gma,       BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "gma")
compute_correlation(dfw_gma_dor,   BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "gma_dor")
compute_correlation(dfw_gma_ven,   BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "gma_ven")
compute_correlation(dfw_wma,       BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "wma")
compute_correlation(dfw_wma_dor,   BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "wma_dor")
compute_correlation(dfw_wma_lat,   BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "wma_lat")
compute_correlation(dfw_wma_ven,   BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "wma_ven")

compute_correlation(dfw_fa_gm,     BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "fa_gm")
compute_correlation(dfw_fa_wm,     BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "fa_wm")
compute_correlation(dfw_fa_wm_dor, BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "fa_wm_dor")
compute_correlation(dfw_fa_wm_lat, BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "fa_wm_lat")
compute_correlation(dfw_fa_wm_ven, BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "fa_wm_ven")
compute_correlation(dfw_md_gm,     BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "md_gm")
compute_correlation(dfw_md_wm,     BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "md_wm")
compute_correlation(dfw_md_wm_dor, BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "md_wm_dor")
compute_correlation(dfw_md_wm_lat, BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "md_wm_lat")
compute_correlation(dfw_md_wm_ven, BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "md_wm_ven")
compute_correlation(dfw_ad_gm,     BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "ad_gm")
compute_correlation(dfw_ad_wm,     BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "ad_wm")
compute_correlation(dfw_ad_wm_dor, BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "ad_wm_dor")
compute_correlation(dfw_ad_wm_lat, BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "ad_wm_lat")
compute_correlation(dfw_ad_wm_ven, BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "ad_wm_ven")
compute_correlation(dfw_rd_gm,     BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "rd_gm")
compute_correlation(dfw_rd_wm,     BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "rd_wm")
compute_correlation(dfw_rd_wm_dor, BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "rd_wm_dor")
compute_correlation(dfw_rd_wm_lat, BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "rd_wm_lat")
compute_correlation(dfw_rd_wm_ven, BBB_score_delta, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta", "rd_wm_ven")

compute_correlation(dfw_T1_gm,     BBB_score_delta, DATAl5[DATAl5$BBB_day==84,]$id, "linear", "BBB_day84", "T1_gm")
compute_correlation(dfw_T1_gm_dor, BBB_score_delta, DATAl5[DATAl5$BBB_day==84,]$id, "linear", "BBB_day84", "T1_gm_dor")
compute_correlation(dfw_T1_gm_ven, BBB_score_delta, DATAl5[DATAl5$BBB_day==84,]$id, "linear", "BBB_day84", "T1_gm_ven")
compute_correlation(dfw_T1_wm,     BBB_score_delta, DATAl5[DATAl5$BBB_day==84,]$id, "linear", "BBB_day84", "T1_wm")
compute_correlation(dfw_T1_wm_dor, BBB_score_delta, DATAl5[DATAl5$BBB_day==84,]$id, "linear", "BBB_day84", "T1_wm_dor")
compute_correlation(dfw_T1_wm_lat, BBB_score_delta, DATAl5[DATAl5$BBB_day==84,]$id, "linear", "BBB_day84", "T1_wm_lat")
compute_correlation(dfw_T1_wm_ven, BBB_score_delta, DATAl5[DATAl5$BBB_day==84,]$id, "linear", "BBB_day84", "T1_wm_ven")

# RESULTS for BBB_score_delta:
#   -



# ==============================================================================================================
# linear correlation with relative delta BBB score (BBB_day84 - BBB_day01)/(21 - BBB_day01)*100
compute_correlation(dfw_sca,       BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "sca")
compute_correlation(dfw_gma,       BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "gma")
compute_correlation(dfw_gma_dor,   BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "gma_dor")
compute_correlation(dfw_gma_ven,   BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "gma_ven")
compute_correlation(dfw_wma,       BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "wma")
compute_correlation(dfw_wma_dor,   BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "wma_dor")
compute_correlation(dfw_wma_lat,   BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "wma_lat")
compute_correlation(dfw_wma_ven,   BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "wma_ven")

compute_correlation(dfw_fa_gm,     BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "fa_gm")
compute_correlation(dfw_fa_wm,     BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "fa_wm")
compute_correlation(dfw_fa_wm_dor, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "fa_wm_dor")
compute_correlation(dfw_fa_wm_lat, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "fa_wm_lat")
compute_correlation(dfw_fa_wm_ven, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "fa_wm_ven")
compute_correlation(dfw_md_gm,     BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "md_gm")
compute_correlation(dfw_md_wm,     BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "md_wm")
compute_correlation(dfw_md_wm_dor, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "md_wm_dor")
compute_correlation(dfw_md_wm_lat, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "md_wm_lat")
compute_correlation(dfw_md_wm_ven, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "md_wm_ven")
compute_correlation(dfw_ad_gm,     BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "ad_gm")
compute_correlation(dfw_ad_wm,     BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "ad_wm")
compute_correlation(dfw_ad_wm_dor, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "ad_wm_dor")
compute_correlation(dfw_ad_wm_lat, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "ad_wm_lat")
compute_correlation(dfw_ad_wm_ven, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "ad_wm_ven")
compute_correlation(dfw_rd_gm,     BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "rd_gm")
compute_correlation(dfw_rd_wm,     BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "rd_wm")
compute_correlation(dfw_rd_wm_dor, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "rd_wm_dor")
compute_correlation(dfw_rd_wm_lat, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "rd_wm_lat")
compute_correlation(dfw_rd_wm_ven, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_delta_rel", "rd_wm_ven")

compute_correlation(dfw_T1_gm,     BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==84,]$id, "linear", "BBB_day84", "T1_gm")
compute_correlation(dfw_T1_gm_dor, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==84,]$id, "linear", "BBB_day84", "T1_gm_dor")
compute_correlation(dfw_T1_gm_ven, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==84,]$id, "linear", "BBB_day84", "T1_gm_ven")
compute_correlation(dfw_T1_wm,     BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==84,]$id, "linear", "BBB_day84", "T1_wm")
compute_correlation(dfw_T1_wm_dor, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==84,]$id, "linear", "BBB_day84", "T1_wm_dor")
compute_correlation(dfw_T1_wm_lat, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==84,]$id, "linear", "BBB_day84", "T1_wm_lat")
compute_correlation(dfw_T1_wm_ven, BBB_score_delta_rel, DATAl5[DATAl5$BBB_day==84,]$id, "linear", "BBB_day84", "T1_wm_ven")

# RESULTS for BBB_score_delta_rel:
# correlations in the expected direction
# fa_wm_lat at C5-C8

# correlation NOT in the expected direction

#   T1_gm_dor between T4-T6 (p=0.008)
#   T1_gm_ven between T4-T6 (p=0.007)
#   T1_wm_dor between T4-T6 (p=0.006)
#   T1_wm_lat between T4-T6 (p=0.037)
#   T1_wm_ven between T4-T6 (p=0.007)





# ==============================================================================================================
# linear correlation with BBB score at day01
compute_correlation(dfw_sca,       BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "sca")
compute_correlation(dfw_gma,       BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "gma")
compute_correlation(dfw_gma_dor,   BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "gma_dor")
compute_correlation(dfw_gma_ven,   BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "gma_ven")
compute_correlation(dfw_wma,       BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "wma")
compute_correlation(dfw_wma_dor,   BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "wma_dor")
compute_correlation(dfw_wma_lat,   BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "wma_lat")
compute_correlation(dfw_wma_ven,   BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "wma_ven")

compute_correlation(dfw_fa_gm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "fa_gm")
compute_correlation(dfw_fa_wm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "fa_wm")
compute_correlation(dfw_fa_wm_dor, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "fa_wm_dor")
compute_correlation(dfw_fa_wm_lat, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "fa_wm_lat")
compute_correlation(dfw_fa_wm_ven, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "fa_wm_ven")
compute_correlation(dfw_md_gm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "md_gm")
compute_correlation(dfw_md_wm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "md_wm")
compute_correlation(dfw_md_wm_dor, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "md_wm_dor")
compute_correlation(dfw_md_wm_lat, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "md_wm_lat")
compute_correlation(dfw_md_wm_ven, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "md_wm_ven")
compute_correlation(dfw_ad_gm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "ad_gm")
compute_correlation(dfw_ad_wm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "ad_wm")
compute_correlation(dfw_ad_wm_dor, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "ad_wm_dor")
compute_correlation(dfw_ad_wm_lat, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "ad_wm_lat")
compute_correlation(dfw_ad_wm_ven, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "ad_wm_ven")
compute_correlation(dfw_rd_gm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "rd_gm")
compute_correlation(dfw_rd_wm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "rd_wm")
compute_correlation(dfw_rd_wm_dor, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "rd_wm_dor")
compute_correlation(dfw_rd_wm_lat, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "rd_wm_lat")
compute_correlation(dfw_rd_wm_ven, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "linear", "BBB_day01", "rd_wm_ven")


# RESULTS for BBB_score_day01:
# 
#   ad_wm_dor between T4-T6 (p=0.046)






















# ==============================================================================================================
# Spearman correlation with BBB score at day01
compute_correlation(dfw_sca,       BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "sca")
compute_correlation(dfw_gma,       BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "gma")
compute_correlation(dfw_gma_dor,   BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "gma_dor")
compute_correlation(dfw_gma_ven,   BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "gma_ven")
compute_correlation(dfw_wma,       BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "wma")
compute_correlation(dfw_wma_dor,   BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "wma_dor")
compute_correlation(dfw_wma_lat,   BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "wma_lat")
compute_correlation(dfw_wma_ven,   BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "wma_ven")

compute_correlation(dfw_fa_gm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "fa_gm")
compute_correlation(dfw_fa_wm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "fa_wm")
compute_correlation(dfw_fa_wm_dor, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "fa_wm_dor")
compute_correlation(dfw_fa_wm_lat, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "fa_wm_lat")
compute_correlation(dfw_fa_wm_ven, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "fa_wm_ven")
compute_correlation(dfw_md_gm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "md_gm")
compute_correlation(dfw_md_wm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "md_wm")
compute_correlation(dfw_md_wm_dor, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "md_wm_dor")
compute_correlation(dfw_md_wm_lat, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "md_wm_lat")
compute_correlation(dfw_md_wm_ven, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "md_wm_ven")
compute_correlation(dfw_ad_gm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "ad_gm")
compute_correlation(dfw_ad_wm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "ad_wm")
compute_correlation(dfw_ad_wm_dor, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "ad_wm_dor")
compute_correlation(dfw_ad_wm_lat, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "ad_wm_lat")
compute_correlation(dfw_ad_wm_ven, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "ad_wm_ven")
compute_correlation(dfw_rd_gm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "rd_gm")
compute_correlation(dfw_rd_wm,     BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "rd_wm")
compute_correlation(dfw_rd_wm_dor, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "rd_wm_dor")
compute_correlation(dfw_rd_wm_lat, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "rd_wm_lat")
compute_correlation(dfw_rd_wm_ven, BBB_score_day01, DATAl5[DATAl5$BBB_day==1,]$id, "spearman", "BBB_day01", "rd_wm_ven")


# RESULTS for BBB_score_day01:
#   ad_wm_lat between                 T1-T3 (p=0.015)


































# select T1 contrast only and exclude group SHAM and segments C1,C2,T7
DATAl5 <- DATAl[DATAl$contrast=="T1",]
DATAl5 <- DATAl5[(!DATAl5$group=="SHAM" & !DATAl5$level=="C1" & !DATAl5$level=="C2" & !DATAl5$level=="T7"),]

# select BBB score at first and last timepoint, then create a difference
DATAl5$BBB_score_day01 <- DATAl5[DATAl5$BBB_day==1,]$BBB_score
DATAl5$BBB_score_day84 <- DATAl5[DATAl5$BBB_day==84,]$BBB_score
DATAl5$BBB_score_diff <- DATAl5[DATAl5$BBB_day==84,]$BBB_score - DATAl5[DATAl5$BBB_day==1,]$BBB_score
DATAl5$BBB_score_diff_rel <- (DATAl5[DATAl5$BBB_day==84,]$BBB_score - DATAl5[DATAl5$BBB_day==1,]$BBB_score)/(21 - DATAl5[DATAl5$BBB_day==1,]$BBB_score)
DATAl5$BBB_score <- NULL
DATAl5$BBB_day   <- NULL

DATAl5 <- DATAl5[(DATAl5$tissue=="wm" & DATAl5$level=="T1"),]


# average cross-sectional area across certain segments
DATAl41 <- DATAl4[(DATAl4$segment_name=="C3" | DATAl4$segment_name=="C4" | DATAl4$segment_name=="C5"),]
DATAl42 <- DATAl4[(DATAl4$segment_name=="C6" | DATAl4$segment_name=="C7" | DATAl4$segment_name=="C8"),]
DATAl43 <- DATAl4[(DATAl4$segment_name=="T1" | DATAl4$segment_name=="T2" | DATAl4$segment_name=="T3"),]
DATAl44 <- DATAl4[(DATAl4$segment_name=="T4" | DATAl4$segment_name=="T5" | DATAl4$segment_name=="T6"),]

df_sca41 <- aggregate(DATAl41[DATAl41$tissue=="wm","area_sc"], list(DATAl41[DATAl41$tissue=="wm","id"]), mean); colnames(df_sca41) <- c("id","sca");
df_sca42 <- aggregate(DATAl42[DATAl42$tissue=="wm","area_sc"], list(DATAl42[DATAl42$tissue=="wm","id"]), mean); colnames(df_sca42) <- c("id","sca");
df_sca43 <- aggregate(DATAl43[DATAl43$tissue=="wm","area_sc"], list(DATAl43[DATAl43$tissue=="wm","id"]), mean); colnames(df_sca43) <- c("id","sca")
df_sca44 <- aggregate(DATAl44[DATAl44$tissue=="wm","area_sc"], list(DATAl44[DATAl44$tissue=="wm","id"]), mean); colnames(df_sca44) <- c("id","sca")

df_wma41 <- aggregate(DATAl41[DATAl41$tissue=="wm","area"], list(DATAl41[DATAl41$tissue=="wm","id"]), mean); colnames(df_wma41) <- c("id","wma");
df_wma42 <- aggregate(DATAl42[DATAl42$tissue=="wm","area"], list(DATAl42[DATAl42$tissue=="wm","id"]), mean); colnames(df_wma42) <- c("id","wma");
df_wma43 <- aggregate(DATAl43[DATAl43$tissue=="wm","area"], list(DATAl43[DATAl43$tissue=="wm","id"]), mean); colnames(df_wma43) <- c("id","wma");
df_wma44 <- aggregate(DATAl44[DATAl44$tissue=="wm","area"], list(DATAl44[DATAl44$tissue=="wm","id"]), mean); colnames(df_wma44) <- c("id","wma");

df_gma41 <- aggregate(DATAl41[DATAl41$tissue=="gm","area"], list(DATAl41[DATAl41$tissue=="gm","id"]), mean); colnames(df_gma41) <- c("id","gma");
df_gma42 <- aggregate(DATAl42[DATAl42$tissue=="gm","area"], list(DATAl42[DATAl42$tissue=="gm","id"]), mean); colnames(df_gma42) <- c("id","gma");
df_gma43 <- aggregate(DATAl43[DATAl43$tissue=="gm","area"], list(DATAl43[DATAl43$tissue=="gm","id"]), mean); colnames(df_gma43) <- c("id","gma");
df_gma44 <- aggregate(DATAl44[DATAl44$tissue=="gm","area"], list(DATAl44[DATAl44$tissue=="gm","id"]), mean); colnames(df_gma44) <- c("id","gma");

# merge dataframes
df1_list <- list(df_sca41, df_wma41, df_gma41, DATAl41[(DATAl41$tissue=="wm" & DATAl41$segment_name=="C5"),c("id","BBB_score_day84","BBB_score_diff_rel")])
df2_list <- list(df_sca42, df_wma42, df_gma42, DATAl42[(DATAl42$tissue=="wm" & DATAl42$segment_name=="C8"),c("id","BBB_score_day84","BBB_score_diff_rel")])
df3_list <- list(df_sca43, df_wma43, df_gma43, DATAl43[(DATAl43$tissue=="wm" & DATAl43$segment_name=="T1"),c("id","BBB_score_day84","BBB_score_diff_rel")])
df4_list <- list(df_sca44, df_wma44, df_gma44, DATAl44[(DATAl44$tissue=="wm" & DATAl44$segment_name=="T4"),c("id","BBB_score_day84","BBB_score_diff_rel")])

df_csa41 <- df1_list %>% reduce(full_join, by="id")
df_csa42 <- df2_list %>% reduce(full_join, by="id")
df_csa43 <- df3_list %>% reduce(full_join, by="id")
df_csa44 <- df4_list %>% reduce(full_join, by="id")























# =======================================================================================
# ============================== PART IV. ANALYSIS OF RELAXOMETRY ======================
# =======================================================================================


# select first timepoint only
# split into gm and wm
# extract separate relaxation metrics
DATAl3_R1_wm <- DATAl[(DATAl$BBB_day==1 & DATAl$tissue=="wm" & DATAl$contrast=="T1"),]
DATAl3_R1_gm <- DATAl[(DATAl$BBB_day==1 & DATAl$tissue=="gm" & DATAl$contrast=="T1"),]
DATAl3_R2all_wm <- DATAl[(DATAl$BBB_day==1 & DATAl$tissue=="wm" & DATAl$contrast=="T2all"),]
DATAl3_R2all_gm <- DATAl[(DATAl$BBB_day==1 & DATAl$tissue=="gm" & DATAl$contrast=="T2all"),]
DATAl3_R2sel_wm <- DATAl[(DATAl$BBB_day==1 & DATAl$tissue=="wm" & DATAl$contrast=="T2sel"),]
DATAl3_R2sel_gm <- DATAl[(DATAl$BBB_day==1 & DATAl$tissue=="gm" & DATAl$contrast=="T2sel"),]

# convert to wide format for plotting and extract needed columns
DATAw3_R1_wm <- reshape(DATAl3_R1_wm, idvar = "id", timevar = "level", direction = "wide")
DATAw3_R1_gm <- reshape(DATAl3_R1_gm, idvar = "id", timevar = "level", direction = "wide")
DATAw3_R2all_wm <- reshape(DATAl3_R2all_wm, idvar = "id", timevar = "level", direction = "wide")
DATAw3_R2all_gm <- reshape(DATAl3_R2all_gm, idvar = "id", timevar = "level", direction = "wide")
DATAw3_R2sel_wm <- reshape(DATAl3_R2sel_wm, idvar = "id", timevar = "level", direction = "wide")
DATAw3_R2sel_gm <- reshape(DATAl3_R2sel_gm, idvar = "id", timevar = "level", direction = "wide")
groups <- DATAw3_R1_wm$group.T4

DATAw3_R1_wm <- select_(DATAw3_R1_wm, "relaxation_rate.C3", "relaxation_rate.C4", "relaxation_rate.C5", "relaxation_rate.C6", "relaxation_rate.C7", "relaxation_rate.C8",
                        "relaxation_rate.T1", "relaxation_rate.T2", "relaxation_rate.T3", "relaxation_rate.T4", "relaxation_rate.T5", "relaxation_rate.T6")
DATAw3_R1_gm <- select_(DATAw3_R1_gm, "relaxation_rate.C3", "relaxation_rate.C4", "relaxation_rate.C5", "relaxation_rate.C6", "relaxation_rate.C7", "relaxation_rate.C8",
                        "relaxation_rate.T1", "relaxation_rate.T2", "relaxation_rate.T3", "relaxation_rate.T4", "relaxation_rate.T5", "relaxation_rate.T6")
DATAw3_R2all_wm <- select_(DATAw3_R2all_wm, "relaxation_rate.C3", "relaxation_rate.C4", "relaxation_rate.C5", "relaxation_rate.C6", "relaxation_rate.C7", "relaxation_rate.C8",
                           "relaxation_rate.T1", "relaxation_rate.T2", "relaxation_rate.T3", "relaxation_rate.T4", "relaxation_rate.T5", "relaxation_rate.T6")
DATAw3_R2all_gm <- select_(DATAw3_R2all_gm, "relaxation_rate.C3", "relaxation_rate.C4", "relaxation_rate.C5", "relaxation_rate.C6", "relaxation_rate.C7", "relaxation_rate.C8",
                           "relaxation_rate.T1", "relaxation_rate.T2", "relaxation_rate.T3", "relaxation_rate.T4", "relaxation_rate.T5", "relaxation_rate.T6")
DATAw3_R2sel_wm <- select_(DATAw3_R2sel_wm, "relaxation_rate.C3", "relaxation_rate.C4", "relaxation_rate.C5", "relaxation_rate.C6", "relaxation_rate.C7", "relaxation_rate.C8",
                           "relaxation_rate.T1", "relaxation_rate.T2", "relaxation_rate.T3", "relaxation_rate.T4", "relaxation_rate.T5", "relaxation_rate.T6")
DATAw3_R2sel_gm <- select_(DATAw3_R2sel_gm, "relaxation_rate.C3", "relaxation_rate.C4", "relaxation_rate.C5", "relaxation_rate.C6", "relaxation_rate.C7", "relaxation_rate.C8",
                           "relaxation_rate.T1", "relaxation_rate.T2", "relaxation_rate.T3", "relaxation_rate.T4", "relaxation_rate.T5", "relaxation_rate.T6")


# visualization - R1 in white matter
plot.new()
x <- 1:length(levels)
y1 <- unlist(colMeans(DATAw3_R1_wm[groups=='SHAM',],na.rm = TRUE))
y2 <- unlist(colMeans(DATAw3_R1_wm[groups=='MILD',],na.rm = TRUE))
y3 <- unlist(colMeans(DATAw3_R1_wm[groups=='MODERATE',],na.rm = TRUE))
y4 <- unlist(colMeans(DATAw3_R1_wm[groups=='SEVERE',],na.rm = TRUE))
xlab <- "Vertebral level"
ylab <- "R1 relaxation rate in the white matter (1/s)"
ylim <- c(0.0005,0.0007)
legendpos <- c(1,4)
labels <- c("C3","C4","C5","C6","C7","C8","T1","T2","T3","T4","T5","T6")
plot_over_segments(x,y1,y2,y3,y4,xlab,xaxt,ylab,ylim,labels,legendpos)

# visualization - R1 in gray matter
plot.new()
x <- 1:length(levels)
y1 <- unlist(colMeans(DATAw3_R1_gm[groups=='SHAM',],na.rm = TRUE))
y2 <- unlist(colMeans(DATAw3_R1_gm[groups=='MILD',],na.rm = TRUE))
y3 <- unlist(colMeans(DATAw3_R1_gm[groups=='MODERATE',],na.rm = TRUE))
y4 <- unlist(colMeans(DATAw3_R1_gm[groups=='SEVERE',],na.rm = TRUE))
xlab <- "Vertebral level"
ylab <- "R1 relaxation rate in the gray matter (1/s)"
ylim <- c(0.0005,0.0007)
legendpos <- c(1,4)
labels <- c("C3","C4","C5","C6","C7","C8","T1","T2","T3","T4","T5","T6")
plot_over_segments(x,y1,y2,y3,y4,xlab,xaxt,ylab,ylim,labels,legendpos)

# visualization - R2all in white matter
plot.new()
x <- 1:length(levels)
y1 <- unlist(colMeans(DATAw3_R2all_wm[groups=='SHAM',],na.rm = TRUE))
y2 <- unlist(colMeans(DATAw3_R2all_wm[groups=='MILD',],na.rm = TRUE))
y3 <- unlist(colMeans(DATAw3_R2all_wm[groups=='MODERATE',],na.rm = TRUE))
y4 <- unlist(colMeans(DATAw3_R2all_wm[groups=='SEVERE',],na.rm = TRUE))
xlab <- "Vertebral level"
ylab <- "R2 relaxation rate in the white matter (1/s)"
ylim <- c(0.012,0.022)
legendpos <- c(1,4)
labels <- c("C3","C4","C5","C6","C7","C8","T1","T2","T3","T4","T5","T6")
plot_over_segments(x,y1,y2,y3,y4,xlab,xaxt,ylab,ylim,labels,legendpos)

# visualization - R2all in gray matter
plot.new()
x <- 1:length(levels)
y1 <- unlist(colMeans(DATAw3_R2all_gm[groups=='SHAM',],na.rm = TRUE))
y2 <- unlist(colMeans(DATAw3_R2all_gm[groups=='MILD',],na.rm = TRUE))
y3 <- unlist(colMeans(DATAw3_R2all_gm[groups=='MODERATE',],na.rm = TRUE))
y4 <- unlist(colMeans(DATAw3_R2all_gm[groups=='SEVERE',],na.rm = TRUE))
xlab <- "Vertebral level"
ylab <- "R2 relaxation rate in the gray matter (1/s)"
ylim <- c(0.012,0.022)
legendpos <- c(1,4)
labels <- c("C3","C4","C5","C6","C7","C8","T1","T2","T3","T4","T5","T6")
plot_over_segments(x,y1,y2,y3,y4,xlab,xaxt,ylab,ylim,labels,legendpos)

# visualization - R2sel in white matter
plot.new()
x <- 1:length(levels)
y1 <- unlist(colMeans(DATAw3_R2sel_wm[groups=='SHAM',],na.rm = TRUE))
y2 <- unlist(colMeans(DATAw3_R2sel_wm[groups=='MILD',],na.rm = TRUE))
y3 <- unlist(colMeans(DATAw3_R2sel_wm[groups=='MODERATE',],na.rm = TRUE))
y4 <- unlist(colMeans(DATAw3_R2sel_wm[groups=='SEVERE',],na.rm = TRUE))
xlab <- "Vertebral level"
ylab <- "R2 relaxation rate in the white matter (1/s)"
ylim <- c(0.012,0.022)
legendpos <- c(1,4)
labels <- c("C3","C4","C5","C6","C7","C8","T1","T2","T3","T4","T5","T6")
plot_over_segments(x,y1,y2,y3,y4,xlab,xaxt,ylab,ylim,labels,legendpos)

# visualization - R2sel in gray matter
plot.new()
x <- 1:length(levels)
y1 <- unlist(colMeans(DATAw3_R2sel_gm[groups=='SHAM',],na.rm = TRUE))
y2 <- unlist(colMeans(DATAw3_R2sel_gm[groups=='MILD',],na.rm = TRUE))
y3 <- unlist(colMeans(DATAw3_R2sel_gm[groups=='MODERATE',],na.rm = TRUE))
y4 <- unlist(colMeans(DATAw3_R2sel_gm[groups=='SEVERE',],na.rm = TRUE))
xlab <- "Vertebral level"
ylab <- "R2 relaxation rate in the gray matter (1/s)"
ylim <- c(0.012,0.022)
legendpos <- c(1,4)
labels <- c("C3","C4","C5","C6","C7","C8","T1","T2","T3","T4","T5","T6")
plot_over_segments(x,y1,y2,y3,y4,xlab,xaxt,ylab,ylim,labels,legendpos)



# FIT MIXED EFFECT MODEL (segment as categorical variable)
# mixed effect model to investigate group differences over segments
#   - group: fixed effect
#   - segment: fixed effect
#   - group-segment interaction: fixed effect
#   - subject over segments: random effect
#   - relaxation rate: dependent variable
#
# Model: relaxation rate ~ group + segment + segment*group + 1|subject
ctrl <- lmeControl(maxIter = 1000, msMaxIter = 1000, optimMethod = "optim", msMaxEval = 1000, niterEM = 1000)
summary(lme3_R1_wm <- lme(relaxation_rate ~ group*level, random = ~ 1|id, data = DATAl3_R1_wm, na.action = na.omit, control = ctrl))
summary(lme3_R1_gm <- lme(relaxation_rate ~ group*level, random = ~ 1|id, data = DATAl3_R1_gm, na.action = na.omit, control = ctrl))
summary(lme3_R2all_wm <- lme(relaxation_rate ~ group*level, random = ~ 1|id, data = DATAl3_R2all_wm, na.action = na.omit, control = ctrl))
summary(lme3_R2all_gm <- lme(relaxation_rate ~ group*level, random = ~ 1|id, data = DATAl3_R2all_gm, na.action = na.omit, control = ctrl))
summary(lme3_R2sel_wm <- lme(relaxation_rate ~ group*level, random = ~ 1|id, data = DATAl3_R2sel_wm, na.action = na.omit, control = ctrl))
summary(lme3_R2sel_gm <- lme(relaxation_rate ~ group*level, random = ~ 1|id, data = DATAl3_R2sel_gm, na.action = na.omit, control = ctrl))


# ========== post-hoc test 1: SCI vs SHAM (main effect model) ==========
# We start with the simplest question possible: is the SCI group (the 3 SCI groups combined) differ from the sham group?
# For this, we combine the mild, moderate, and severe groups into a single SCI group.
emm31_R1_wm <- emmeans(lme3_R1_wm, specs = ~ group, at = list(level=levels))
emm31_R1_gm <- emmeans(lme3_R1_gm, specs = ~ group, at = list(level=levels))
emm31_R2all_wm <- emmeans(lme3_R2all_wm, specs = ~ group, at = list(level=levels))
emm31_R2all_gm <- emmeans(lme3_R2all_gm, specs = ~ group, at = list(level=levels))
emm31_R2sel_wm <- emmeans(lme3_R2sel_wm, specs = ~ group, at = list(level=levels))
emm31_R2sel_gm <- emmeans(lme3_R2sel_gm, specs = ~ group, at = list(level=levels))

sham <- c(0,0,0,1)
sci  <- c(1,1,1,0)/3

c31_R1_wm <- contrast(emm31_R1_wm, method = list("wma_sci-sham" = sci-sham)); c31_R1_wm;
c31_R1_gm <- contrast(emm31_R1_gm, method = list("gma_sci-sham" = sci-sham)); c31_R1_gm;
c31_R2all_wm <- contrast(emm31_R2all_wm, method = list("wma_sci-sham" = sci-sham)); c31_R2all_wm;
c31_R2all_gm <- contrast(emm31_R2all_gm, method = list("gma_sci-sham" = sci-sham)); c31_R2all_gm;
c31_R2sel_wm <- contrast(emm31_R2sel_wm, method = list("wma_sci-sham" = sci-sham)); c31_R2sel_wm;
c31_R2sel_gm <- contrast(emm31_R2sel_gm, method = list("gma_sci-sham" = sci-sham)); c31_R2sel_gm;

# NO differences in relaxation rate (WM nor GM) between SCI and sham


# ========== post-hoc test 2: pairwise group differences  ==========
# Now, we want to compare the sub-groups as well.

emm32_R1_wm <- emmeans(lme3_R1_wm, specs = ~ group, at = list(level=levels))
emm32_R1_gm <- emmeans(lme3_R1_gm, specs = ~ group, at = list(level=levels))
emm32_R2all_wm <- emmeans(lme3_R2all_wm, specs = ~ group, at = list(level=levels))
emm32_R2all_gm <- emmeans(lme3_R2all_gm, specs = ~ group, at = list(level=levels))
emm32_R2sel_wm <- emmeans(lme3_R2sel_wm, specs = ~ group, at = list(level=levels))
emm32_R2sel_gm <- emmeans(lme3_R2sel_gm, specs = ~ group, at = list(level=levels))

sham <- c(0,0,0,1)
mild <- c(1,0,0,0)
moderate <- c(0,1,0,0)
severe <- c(0,0,1,0)

list = list("sca_mild-sham" = mild-sham,
            "sca_moderate-sham" = moderate-sham,
            "sca_severe-sham" = severe-sham,
            "sca_severe-mild" = severe-mild,
            "sca_severe-moderate" = severe-moderate,
            "sca_moderate-mild" = moderate-mild)

c32_R1_wm <- contrast(emm32_R1_wm, method = list); c32_R1_wm;
c32_R1_gm <- contrast(emm32_R1_gm, method = list); c32_R1_gm;
c32_R2all_wm <- contrast(emm32_R2all_wm, method = list); c32_R2all_wm;
c32_R2all_gm <- contrast(emm32_R2all_gm, method = list); c32_R2all_gm;
c32_R2sel_wm <- contrast(emm32_R2sel_wm, method = list); c32_R2sel_wm;
c32_R2sel_gm <- contrast(emm32_R2sel_gm, method = list); c32_R2sel_gm;


# Differences found in:
# R1 in WM: - severe vs. sham
#           - severe vs. mild
#           - severe vs. moderate (p>0.05)
#
# R1 in GM: - severe vs. sham (p>0.05)
#           - severe vs. mild (p>0.05)



# ========== post-hoc test 3: combined SCI vs SHAM within combined segments (C3-T6)==========
# We found differences between severe SCI and sham (but NOT between SCI and sham).
# Now, we want to look at the effect of the region. Where are these differences the strongest?
# To reduce the number of levels, we combine the 16 vertebral levels into 4 regions of 3 vertebral level each:
#   upper cervical cord: C3, C4, C5
#   lower cervical cord: C6, C7, C8
#   upper thoracic cord: T1, T2, T3
#   mid   thoracic cord: T4, T5, T6

emm33_R1_wm <- emmeans(lme3_R1_wm, specs = ~ group*level, at = list(level=levels))
emm33_R1_gm <- emmeans(lme3_R1_gm, specs = ~ group*level, at = list(level=levels))
emm33_R2all_wm <- emmeans(lme3_R2all_wm, specs = ~ group*level, at = list(level=levels))
emm33_R2all_gm <- emmeans(lme3_R2all_gm, specs = ~ group*level, at = list(level=levels))
emm33_R2sel_wm <- emmeans(lme3_R2sel_wm, specs = ~ group*level, at = list(level=levels))
emm33_R2sel_gm <- emmeans(lme3_R2sel_gm, specs = ~ group*level, at = list(level=levels))

sham_s1 <- c(0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
sham_s2 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
sham_s3 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)/3
sham_s4 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1)/3
severe_s1  <- c(0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
severe_s2  <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
severe_s3  <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
severe_s4  <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0)/3
moderate_s1  <- c(0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
moderate_s2  <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
moderate_s3  <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
moderate_s4  <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0)/3

list <- list("sca_severe-sham_s1" = severe_s1-sham_s1,
             "sca_severe-sham_s2" = severe_s2-sham_s2,
             "sca_severe-sham_s3" = severe_s3-sham_s3,
             "sca_severe-sham_s4" = severe_s4-sham_s4)

c33_R1_wm <- contrast(emm33_R1_wm, method = list); c33_R1_wm;
c33_R1_gm <- contrast(emm33_R1_gm, method = list); c33_R1_gm;
c33_R2all_wm <- contrast(emm33_R2all_wm, method = list); c33_R2all_wm;
c33_R2all_gm <- contrast(emm33_R2all_gm, method = list); c33_R2all_gm;
c33_R2sel_wm <- contrast(emm33_R2sel_wm, method = list); c33_R2sel_wm;
c33_R2sel_gm <- contrast(emm33_R2sel_gm, method = list); c33_R2sel_gm;

