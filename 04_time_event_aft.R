#######################################################################
###--------------------------------------------------------------------
### TABLE 4a AFT LCZ2 ###
extract_aft_results <- function(model) {
  sum_mod <- summary(model)$table
  est <- sum_mod[2, 1]
  se  <- sum_mod[2, 2]
  p_val <- round(sum_mod[2, 4], 3)
  pct_est <- (exp(est) - 1) * 100
  pct_lower <- (exp(est - 1.96 * se) - 1) * 100
  pct_upper <- (exp(est + 1.96 * se) - 1) * 100
  res_str <- sprintf("%.1f%% (%.1f%%, %.1f%%)", pct_est, pct_lower, pct_upper)
  result_vec <- c(res_str, p_val)
  return(matrix(result_vec, ncol=2, nrow=1, byrow=TRUE))}

data0$md_time  <- ifelse(data0$md_time <= 0, 0.001, data0$md_time)
data0$mdd_time <- ifelse(data0$mdd_time <= 0, 0.001, data0$mdd_time)
data0$gad_time <- ifelse(data0$gad_time <= 0, 0.001, data0$gad_time)
data0$sud_time <- ifelse(data0$sud_time <= 0, 0.001, data0$sud_time)

FUN_aft_model1 <- function(x, y, dataset){
  form <- as.formula(paste0("Surv(", y, ",", x, ") ~ lcz_2 + age + sex + ethnic"))
  model <- survreg(form, data = eval(parse(text=dataset)), dist = "weibull")
  extract_aft_results(model)}

FUN_aft_model2 <- function(x, y, dataset){
  form <- as.formula(paste0("Surv(", y, ",", x, ") ~ lcz_2 + age + sex + ethnic + ses + nosmoking + drinkingM + sleeph + regular + dieth"))
  model <- survreg(form, data = eval(parse(text=dataset)), dist = "weibull")
  extract_aft_results(model)}

FUN_aft_model3 <- function(x, y, dataset){
  form <- as.formula(paste0("Surv(", y, ",", x, ") ~ lcz_2 + age + sex + ethnic + ses + nosmoking + drinkingM + sleeph + regular + dieth + dmp + htnp + opioid + NSAID"))
  model <- survreg(form, data = eval(parse(text=dataset)), dist = "weibull")
  extract_aft_results(model)}

table_results <- data.frame(matrix(NA, nrow=12, ncol=3))
colnames(table_results) <- c("Disease", "built", "P_built")
table_results$Disease <- c("MD_Model1", "MD_Model2", "MD_Model3", 
                           "MDD_Model1", "MDD_Model2", "MDD_Model3",
                           "GAD_Model1", "GAD_Model2", "GAD_Model3", 
                           "SUD_Model1", "SUD_Model2", "SUD_Model3")

table_results[1, 2:3] <- FUN_aft_model1("md", "md_time", "data0")
table_results[2, 2:3] <- FUN_aft_model2("md", "md_time", "data0")
table_results[3, 2:3] <- FUN_aft_model3("md", "md_time", "data0")

table_results[4, 2:3] <- FUN_aft_model1("mdd", "mdd_time", "data0")
table_results[5, 2:3] <- FUN_aft_model2("mdd", "mdd_time", "data0")
table_results[6, 2:3] <- FUN_aft_model3("mdd", "mdd_time", "data0")

table_results[7, 2:3] <- FUN_aft_model1("gad", "gad_time", "data0")
table_results[8, 2:3] <- FUN_aft_model2("gad", "gad_time", "data0")
table_results[9, 2:3] <- FUN_aft_model3("gad", "gad_time", "data0")

table_results[10, 2:3] <- FUN_aft_model1("sud", "sud_time", "data0")
table_results[11, 2:3] <- FUN_aft_model2("sud", "sud_time", "data0")
table_results[12, 2:3] <- FUN_aft_model3("sud", "sud_time", "data0")

print(table_results)
write.xlsx(table_results, "/Users/guuuuu/Desktop/LCZ_mh/results0405/table4a_aft_lcz2.xlsx")


#######################################################################
###--------------------------------------------------------------------
### TABLE 4b MAIN RESULTS LCZ4 ###
extract_aft_results_cat4 <- function(model) {
  sum_mod <- summary(model)$table
  result_vec <- c()
  for(i in 2:4){
    est <- sum_mod[i, 1]
    se  <- sum_mod[i, 2]
    p_val <- round(sum_mod[i, 4], 3)
    pct_est   <- (exp(est) - 1) * 100
    pct_lower <- (exp(est - 1.96 * se) - 1) * 100
    pct_upper <- (exp(est + 1.96 * se) - 1) * 100
    res_str <- sprintf("%.1f%% (%.1f%%, %.1f%%)", pct_est, pct_lower, pct_upper)
    result_vec <- c(result_vec, res_str, p_val)}
  return(matrix(result_vec, ncol=6, nrow=1, byrow=TRUE))}

data0$md_time  <- ifelse(data0$md_time <= 0, 0.001, data0$md_time)
data0$mdd_time <- ifelse(data0$mdd_time <= 0, 0.001, data0$mdd_time)
data0$gad_time <- ifelse(data0$gad_time <= 0, 0.001, data0$gad_time)
data0$sud_time <- ifelse(data0$sud_time <= 0, 0.001, data0$sud_time)

FUN_aft_model1_cat4 <- function(x, y, dataset){
  form <- as.formula(paste0("Surv(", y, ",", x, ") ~ lcz_4 + age + sex + ethnic"))
  model <- survreg(form, data = eval(parse(text=dataset)), dist = "weibull")
  extract_aft_results_cat4(model)}

FUN_aft_model2_cat4 <- function(x, y, dataset){
  form <- as.formula(paste0("Surv(", y, ",", x, ") ~ lcz_4 + age + sex + ethnic + ses + nosmoking + drinkingM + sleeph + regular + dieth"))
  model <- survreg(form, data = eval(parse(text=dataset)), dist = "weibull")
  extract_aft_results_cat4(model)}

FUN_aft_model3_cat4 <- function(x, y, dataset){
  form <- as.formula(paste0("Surv(", y, ",", x, ") ~ lcz_4 + age + sex + ethnic + ses + nosmoking + drinkingM + sleeph + regular + dieth + dmp + htnp + opioid + NSAID"))
  model <- survreg(form, data = eval(parse(text=dataset)), dist = "weibull")
  extract_aft_results_cat4(model)}

table_results_4cat_aft <- data.frame(matrix(NA, nrow=12, ncol=7))
colnames(table_results_4cat_aft) <- c("Disease", "compact","P_compact","open","P_open","industry","P_industry")

table_results_4cat_aft$Disease <- c("MD_Model1", "MD_Model2", "MD_Model3", 
                                    "MDD_Model1", "MDD_Model2", "MDD_Model3",
                                    "GAD_Model1", "GAD_Model2", "GAD_Model3", 
                                    "SUD_Model1", "SUD_Model2", "SUD_Model3")

table_results_4cat_aft[1, 2:7] <- FUN_aft_model1_cat4("md", "md_time", "data0")
table_results_4cat_aft[2, 2:7] <- FUN_aft_model2_cat4("md", "md_time", "data0")
table_results_4cat_aft[3, 2:7] <- FUN_aft_model3_cat4("md", "md_time", "data0")

table_results_4cat_aft[4, 2:7] <- FUN_aft_model1_cat4("mdd", "mdd_time", "data0")
table_results_4cat_aft[5, 2:7] <- FUN_aft_model2_cat4("mdd", "mdd_time", "data0")
table_results_4cat_aft[6, 2:7] <- FUN_aft_model3_cat4("mdd", "mdd_time", "data0")

table_results_4cat_aft[7, 2:7] <- FUN_aft_model1_cat4("gad", "gad_time", "data0")
table_results_4cat_aft[8, 2:7] <- FUN_aft_model2_cat4("gad", "gad_time", "data0")
table_results_4cat_aft[9, 2:7] <- FUN_aft_model3_cat4("gad", "gad_time", "data0")

table_results_4cat_aft[10, 2:7] <- FUN_aft_model1_cat4("sud", "sud_time", "data0")
table_results_4cat_aft[11, 2:7] <- FUN_aft_model2_cat4("sud", "sud_time", "data0")
table_results_4cat_aft[12, 2:7] <- FUN_aft_model3_cat4("sud", "sud_time", "data0")

print(table_results_4cat_aft)
write.xlsx(table_results_4cat_aft, "/Users/guuuuu/Desktop/LCZ_mh/results0405/table4b_aft_lcz4.xlsx")


#######################################################################
###--------------------------------------------------------------------
### TABLE 4c AFT LCZ11 ###
extract_aft_results <- function(model) {
  sum_mod <- summary(model)$table
  result_vec <- c()
  for(i in 2:11){
    est <- sum_mod[i, 1]
    se  <- sum_mod[i, 2]
    p_val <- round(sum_mod[i, 4], 3)
    pct_est <- (exp(est) - 1) * 100
    pct_lower <- (exp(est - 1.96 * se) - 1) * 100
    pct_upper <- (exp(est + 1.96 * se) - 1) * 100
    res_str <- sprintf("%.1f%% (%.1f%%, %.1f%%)", pct_est, pct_lower, pct_upper)
    result_vec <- c(result_vec, res_str, p_val)}
  return(matrix(result_vec, ncol=20, nrow=1, byrow=TRUE))}

data0$md_time  <- ifelse(data0$md_time <= 0, 0.001, data0$md_time)
data0$mdd_time <- ifelse(data0$mdd_time <= 0, 0.001, data0$mdd_time)
data0$gad_time <- ifelse(data0$gad_time <= 0, 0.001, data0$gad_time)
data0$sud_time <- ifelse(data0$sud_time <= 0, 0.001, data0$sud_time)

FUN_aft_model1 <- function(x, y, dataset){
  form <- as.formula(paste0("Surv(", y, ",", x, ") ~ lcz_11 + age + sex + ethnic"))
  model <- survreg(form, data = eval(parse(text=dataset)), dist = "weibull")
  extract_aft_results(model)}

FUN_aft_model2 <- function(x, y, dataset){
  form <- as.formula(paste0("Surv(", y, ",", x, ") ~ lcz_11 + age + sex + ethnic + ses + nosmoking + drinkingM + sleeph + regular + dieth"))
  model <- survreg(form, data = eval(parse(text=dataset)), dist = "weibull")
  extract_aft_results(model)}

FUN_aft_model3 <- function(x, y, dataset){
  form <- as.formula(paste0("Surv(", y, ",", x, ") ~ lcz_11 + age + sex + ethnic + ses + nosmoking + drinkingM + sleeph + regular + dieth + dmp + htnp + opioid + NSAID"))
  model <- survreg(form, data = eval(parse(text=dataset)), dist = "weibull")
  extract_aft_results(model)}

table_results_11cat <- data.frame(matrix(NA, nrow=12, ncol=21))
colnames(table_results_11cat) <- c("Disease", 
                                   "LCZ1","P1","LCZ2","P2","LCZ3","P3","LCZ4","P4","LCZ5","P5",
                                   "LCZ6","P6","LCZ7","P7","LCZ8","P8","LCZ9","P9","LCZ10","P10")

table_results_11cat$Disease <- c("MD_Model1", "MD_Model2", "MD_Model3", "MDD_Model1", "MDD_Model2", "MDD_Model3",
                                 "GAD_Model1", "GAD_Model2", "GAD_Model3", "SUD_Model1", "SUD_Model2", "SUD_Model3")

table_results_11cat[1, 2:21] <- FUN_aft_model1("md", "md_time", "data0")
table_results_11cat[2, 2:21] <- FUN_aft_model2("md", "md_time", "data0")
table_results_11cat[3, 2:21] <- FUN_aft_model3("md", "md_time", "data0")

table_results_11cat[4, 2:21] <- FUN_aft_model1("mdd", "mdd_time", "data0")
table_results_11cat[5, 2:21] <- FUN_aft_model2("mdd", "mdd_time", "data0")
table_results_11cat[6, 2:21] <- FUN_aft_model3("mdd", "mdd_time", "data0")

table_results_11cat[7, 2:21] <- FUN_aft_model1("gad", "gad_time", "data0")
table_results_11cat[8, 2:21] <- FUN_aft_model2("gad", "gad_time", "data0")
table_results_11cat[9, 2:21] <- FUN_aft_model3("gad", "gad_time", "data0")

table_results_11cat[10, 2:21] <- FUN_aft_model1("sud", "sud_time", "data0")
table_results_11cat[11, 2:21] <- FUN_aft_model2("sud", "sud_time", "data0")
table_results_11cat[12, 2:21] <- FUN_aft_model3("sud", "sud_time", "data0")

print(table_results_11cat)
write.xlsx(table_results_11cat, "/Users/guuuuu/Desktop/LCZ_mh/results0405/table4c_aft_lcz11.xlsx")
