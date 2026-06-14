#######################################################################
###--------------------------------------------------------------------
### TABLE 5-6 SYM+MRI FOR LCZ2 ###
is_outlier <- function(x, na.rm = T) {
  qs = quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  lowerq <- qs[1]
  upperq <- qs[2]
  iqr = upperq - lowerq 
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  x > extreme.threshold.upper | x < extreme.threshold.lower}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {df <- df[!is_outlier(df[[col]]), ]} 
  return(df)}

gm_vars <- c("acc_vol", "amyg_vol", "cau_vol", "hip_vol", "pal_vol", "put_vol", "tha_vol", "bsts_vol", "cac_vol", "caumf_vol", "cun_vol", "entor_vol", "fpo_vol", "ff_vol", "ipl_vol", "itp_vol", "ins_vol", "iscg_vol", "laoc_vol", "laorf_vol", "ling_vol", 
             "morf_vol", "mtp_vol", "pcl_vol", "phip_vol", "poper_vol", "porb_vol", "ptriang_vol", "pcal_vol", "poc_vol", "pcg_vol", "prec_vol", "pcun_vol", "rac_vol", "rmf_vol", "sf_vol", "spl_vol", "stp_vol", "smg_vol", "ttp_vol")

wm_vars <- c("wm_fa_tfma", "wm_fa_tfmi", "wm_fa_tmcp", "wm_fa_tar", "wm_fa_tatr", "wm_fa_tcgpc", "wm_fa_tct", "wm_fa_tifof", "wm_fa_tilf", "wm_fa_tml", "wm_fa_tppc", "wm_fa_tptr", "wm_fa_tslf", "wm_fa_tstr", "wm_fa_tuf", 
             "wm_md_tfma", "wm_md_tfmi", "wm_md_tmcp", "wm_md_tar", "wm_md_tatr", "wm_md_tcgpc", "wm_md_tct", "wm_md_tifof", "wm_md_tilf", "wm_md_tml", "wm_md_tppc", "wm_md_tptr", "wm_md_tslf", "wm_md_tstr", "wm_md_tuf", 
             "wm_mo_tfma", "wm_mo_tfmi", "wm_mo_tmcp", "wm_mo_tar", "wm_mo_tatr", "wm_mo_tcgpc", "wm_mo_tct", "wm_mo_tifof", "wm_mo_tilf", "wm_mo_tml", "wm_mo_tppc", "wm_mo_tptr", "wm_mo_tslf", "wm_mo_tstr", "wm_mo_tuf", 
             "wm_od_tfma", "wm_od_tfmi", "wm_od_tmcp", "wm_od_tar", "wm_od_tatr", "wm_od_tcgpc", "wm_od_tct", "wm_od_tifof", "wm_od_tilf", "wm_od_tml", "wm_od_tppc", "wm_od_tptr", "wm_od_tslf", "wm_od_tstr", "wm_od_tuf", 
             "wm_icvf_tfma", "wm_icvf_tfmi", "wm_icvf_tmcp", "wm_icvf_tar", "wm_icvf_tatr", "wm_icvf_tcgpc", "wm_icvf_tct", "wm_icvf_tifof", "wm_icvf_tilf", "wm_icvf_tml", "wm_icvf_tppc", "wm_icvf_tptr", "wm_icvf_tslf", "wm_icvf_tstr", "wm_icvf_tuf", 
             "wm_isovf_tfma", "wm_isovf_tfmi", "wm_isovf_tmcp", "wm_isovf_tar", "wm_isovf_tatr", "wm_isovf_tcgpc", "wm_isovf_tct", "wm_isovf_tifof", "wm_isovf_tilf", "wm_isovf_tml", "wm_isovf_tppc", "wm_isovf_tptr", "wm_isovf_tslf", "wm_isovf_tstr", "wm_isovf_tuf")

all_mri_vars <- c(gm_vars, wm_vars)

symptom_vars <- c("fhapp", "fhh", "flife", 
                  "fsuic", "fslp", "fmove", "finad", "ftired", "fdepr", "fconc", "fappe", "finte", 
                  "fwor", "frestl", "frelax", "fann", "fworr", "ffore", "fnerv")

covs_all <- "age + sex + ethnic + ses + bmi + whr + nosmoking + drinkingM + sleeph + regular + dieth + dmp + htnp + opioid + NSAID"
covs_no_sex <- "age + ethnic + ses + bmi + whr + nosmoking + drinkingM + sleeph + regular + dieth + dmp + htnp + opioid + NSAID"
covs_no_age <- "sex + ethnic + ses + bmi + whr + nosmoking + drinkingM + sleeph + regular + dieth + dmp + htnp + opioid + NSAID"

data_image_clean <- data0 %>% drop_na(all_of(c(all_mri_vars, "lcz_2", "age", "sex")))
data_image_clean <- remove_outliers(data_image_clean, all_mri_vars) ### 26834 ###
data_image_clean[all_mri_vars] <- scale(data_image_clean[all_mri_vars], center=TRUE, scale=TRUE)
data_image_clean$age_cat <- as.factor(ifelse(data_image_clean$age >= 60, ">=60", "<60"))

data_symptom <- data0 %>% drop_na(all_of(c(symptom_vars, "lcz_2", "age", "sex"))) ### 103482 ###
data_symptom$age_cat <- as.factor(ifelse(data_symptom$age >= 60, ">=60", "<60"))

FUN_sym_main <- function(x) {
  data_symptom[[x]] <- as.numeric(as.character(data_symptom[[x]]))
  eval(parse(text=paste("model <- glm(", x, " ~ relevel(factor(lcz_2), ref='nature') + ", covs_all, ", family='binomial', data=data_symptom)", sep="")))
  res_coef <- summary(model)$coefficients
  res_ci   <- confint.default(model)
  target_name <- "relevel(factor(lcz_2), ref = \"nature\")built"
  est <- round(exp(res_coef[target_name, 1]), 3)
  lci <- round(exp(res_ci[target_name, 1]), 3)
  uci <- round(exp(res_ci[target_name, 2]), 3)
  p   <- round(res_coef[target_name, 4], 6)
  re1 <- paste0(est, " (", lci, ", ", uci, ")")
  return(cbind(re1, p))}

FUN_sym_interact <- function(x, interact_var, ref1, ref2, covs_str) {
  data_symptom[[x]] <- as.numeric(as.character(data_symptom[[x]]))
  eval(parse(text=paste("model1 <- glm(", x, " ~ relevel(factor(lcz_2), ref='nature') * relevel(factor(", interact_var, "), ref='", ref1, "') + ", covs_str, ", family='binomial', data=data_symptom)", sep="")))
  eval(parse(text=paste("model2 <- glm(", x, " ~ relevel(factor(lcz_2), ref='nature') * relevel(factor(", interact_var, "), ref='", ref2, "') + ", covs_str, ", family='binomial', data=data_symptom)", sep="")))
  c1 <- summary(model1)$coefficients; ci1 <- confint.default(model1)
  c2 <- summary(model2)$coefficients; ci2 <- confint.default(model2)
  target_name <- "relevel(factor(lcz_2), ref = \"nature\")built"
  res1 <- paste0(round(exp(c1[target_name, 1]), 3), " (", round(exp(ci1[target_name, 1]), 3), ", ", round(exp(ci1[target_name, 2]), 3), ")")
  p1   <- round(c1[target_name, 4], 6)
  res2 <- paste0(round(exp(c2[target_name, 1]), 3), " (", round(exp(ci2[target_name, 1]), 3), ", ", round(exp(ci2[target_name, 2]), 3), ")")
  p2   <- round(c2[target_name, 4], 6)
  all_rows <- rownames(c1)
  inter_idx <- grep(":", all_rows)
  if(length(inter_idx) > 0) {
    p_int <- round(c1[inter_idx[1], 4], 3)} else {p_int <- NA}
  return(cbind(res1, p1, res2, p2, p_int))}

record_sym_main <- NULL
for(var in symptom_vars) {
  res <- data.frame(SYMPTOM = var, FUN_sym_main(var))
  record_sym_main <- rbind(record_sym_main, res)}
colnames(record_sym_main) <- c("SYMPTOM", "OR (95% CI)", "P")
record_sym_main$P_FDR <- round(p.adjust(record_sym_main$P, "fdr"), 6)
write.xlsx(record_sym_main, "/Users/guuuuu/Desktop/LCZ_mh/results0405/table5a_sym_main.xlsx")

record_sym_sex <- NULL
for(var in symptom_vars) {
  res_raw <- FUN_sym_interact(var, "sex", "male", "female", covs_no_sex)
  res_df  <- data.frame(SYMPTOM = var, res_raw, stringsAsFactors = FALSE)
  record_sym_sex <- rbind(record_sym_sex, res_df)}
colnames(record_sym_sex) <- c("SYMPTOM", "MALE", "P_male", "FEMALE", "P_female", "P_for_interaction")
write.xlsx(record_sym_sex, "/Users/guuuuu/Desktop/LCZ_mh/results0405/table5b_sym_sex.xlsx")

record_sym_age <- NULL
for(var in symptom_vars) {
  res <- data.frame(SYMPTOM = var, FUN_sym_interact(var, "age_cat", "<60", ">=60", covs_no_age))
  record_sym_age <- rbind(record_sym_age, res)}
colnames(record_sym_age) <- c("SYMPTOM", "Age<60", "P_Age<60", "Age>=60", "P_Age>=60", "P_for_interaction")
write.xlsx(record_sym_age, "/Users/guuuuu/Desktop/LCZ_mh/results0405/table5c_sym_age.xlsx")

FUN_mri_main <- function(x) {
  eval(parse(text=paste("model <- glm(", x, " ~ relevel(factor(lcz_2), ref='nature') + ", covs_all, ", family='gaussian', data=data_image_clean)", sep="")))
  res_coef <- summary(model)$coefficients
  res_ci   <- confint.default(model) 
  target_name <- "relevel(factor(lcz_2), ref = \"nature\")built"
  est <- round(res_coef[target_name, 1], 3)
  lci <- round(res_ci[target_name, 1], 3)
  uci <- round(res_ci[target_name, 2], 3)
  p   <- round(res_coef[target_name, 4], 6)
  re1 <- paste0(est, " (", lci, ", ", uci, ")")
  return(cbind(re1, p))}

FUN_mri_interact <- function(x, interact_var, ref1, ref2, covs_str) {
  eval(parse(text=paste("model1 <- glm(", x, " ~ relevel(factor(lcz_2), ref='nature') * relevel(factor(", interact_var, "), ref='", ref1, "') + ", covs_str, ", family='gaussian', data=data_image_clean)", sep="")))
  eval(parse(text=paste("model2 <- glm(", x, " ~ relevel(factor(lcz_2), ref='nature') * relevel(factor(", interact_var, "), ref='", ref2, "') + ", covs_str, ", family='gaussian', data=data_image_clean)", sep="")))
  c1 <- summary(model1)$coefficients; ci1 <- confint.default(model1)
  c2 <- summary(model2)$coefficients; ci2 <- confint.default(model2)
  target_name <- "relevel(factor(lcz_2), ref = \"nature\")built"
  res1 <- paste0(round(c1[target_name, 1], 3), " (", round(ci1[target_name, 1], 3), ", ", round(ci1[target_name, 2], 3), ")")
  p1   <- round(c1[target_name, 4], 6)
  res2 <- paste0(round(c2[target_name, 1], 3), " (", round(ci2[target_name, 1], 3), ", ", round(ci2[target_name, 2], 3), ")")
  p2   <- round(c2[target_name, 4], 6)
  all_rows <- rownames(c1)
  inter_idx <- grep(":", all_rows) 
  if(length(inter_idx) > 0) {
    p_int <- round(c1[inter_idx[1], 4], 3)} else {
      p_int <- NA}
  return(cbind(res1, p1, res2, p2, p_int))}

record_mri_main <- NULL
for(var in all_mri_vars) {
  res <- data.frame(PHENOTYPE = var, FUN_mri_main(var))
  record_mri_main <- rbind(record_mri_main, res)}
colnames(record_mri_main) <- c("PHENOTYPE", "BETA (95% CI)", "P")
record_mri_main$P_FDR <- round(p.adjust(record_mri_main$P, "fdr"), 6)
write.xlsx(record_mri_main, "/Users/guuuuu/Desktop/LCZ_mh/results0405/table6a_mri_main.xlsx")

record_mri_sex <- NULL
for(var in all_mri_vars) {
  res_raw <- FUN_mri_interact(var, "sex", "male", "female", covs_no_sex)
  res_df  <- data.frame(PHENOTYPE = var, res_raw, stringsAsFactors = FALSE)
  record_mri_sex <- rbind(record_mri_sex, res_df)}
colnames(record_mri_sex) <- c("PHENOTYPE", "MALE", "P_male", "FEMALE", "P_female", "P_for_interaction")
write.xlsx(record_mri_sex, "/Users/guuuuu/Desktop/LCZ_mh/results0405/table6b_mri_sex.xlsx")

record_mri_age <- NULL
for(var in all_mri_vars) {
  res <- data.frame(PHENOTYPE = var, FUN_mri_interact(var, "age_cat", "<60", ">=60", covs_no_age))
  record_mri_age <- rbind(record_mri_age, res)}
colnames(record_mri_age) <- c("PHENOTYPE", "Age<60", "P_Age<60", "Age>=60", "P_Age>=60", "P_for_interaction")
write.xlsx(record_mri_age, "/Users/guuuuu/Desktop/LCZ_mh/results0405/table6c_mri_age.xlsx")
