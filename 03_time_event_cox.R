#######################################################################
###--------------------------------------------------------------------
### TABLE 2 - INCIDENCE OF MENTAL DISORDERS BY SEX ###
data_male <- data0 %>% filter(sex=="male")
data_female <- data0 %>% filter(sex=="female")
FUN_incidence <- function(x, y, data){
  event <- as.numeric(as.character(data[[x]]))
  time  <- as.numeric(as.character(data[[y]]))
  inci <- sum(event, na.rm = TRUE)
  py   <- round(sum(time, na.rm = TRUE), 0)
  ir   <- ifelse(py > 0, round(inci / py * 100000, 1), NA)
  cbind(inci, py, ir)}

table2 <- matrix(NA,12,3)
colnames(table2) <- c("NO.incident case","Total person-year","Incidence rate")
rownames(table2) <- c("ALL","md","mdd","gad", "MALE","md", "mdd","gad", "FEMALE","md","mdd","gad")

table2[2, ] <- FUN_incidence("md","md_time",data0)
table2[3, ] <- FUN_incidence("mdd","mdd_time",data0)
table2[4, ] <- FUN_incidence("gad","gad_time",data0)

table2[6, ] <- FUN_incidence("md","md_time",data_male)
table2[7, ] <- FUN_incidence("mdd","mdd_time",data_male)
table2[8, ] <- FUN_incidence("gad","gad_time",data_male)

table2[10, ] <- FUN_incidence("md","md_time",data_female)
table2[11, ] <- FUN_incidence("mdd","mdd_time",data_female)
table2[12, ] <- FUN_incidence("gad","gad_time",data_female)

table2 <- data.frame(table2)
table2$var <- row.names(table2)
table2 <- table2 %>% select(4,1:3)
write.xlsx(table2, "/Users/guuuuu/Desktop/LCZ_mh/results0405/table2_incidence_description.xlsx")


#######################################################################
###--------------------------------------------------------------------
### TABLE 3a MAIN RESULTS LCZ22 ###
library(survival)
FUN_model1 <- function(x, y, dataset){
  eval(parse(text=paste("model <- coxph(Surv(", y, ",", x, ") ~ lcz_2 + age + sex + ethnic, data=", dataset, ")", sep='')))
  re = summary(model)$conf.int
  re_p = round(summary(model)$coef[1, 5], 3)
  result_built = paste0(round(re[1, 1], 2), " (", round(re[1, 3], 2), ", ", round(re[1, 4], 2), ")")
  result = matrix(c(result_built, re_p), ncol=2, nrow=1, byrow=T)
  return(result)}

FUN_model2 <- function(x, y, dataset){
  eval(parse(text=paste("model <- coxph(Surv(", y, ",", x, ") ~ lcz_2 + age + sex + ethnic + ses + nosmoking + drinkingM + sleeph + regular + dieth, data=", dataset, ")", sep='')))
  re = summary(model)$conf.int
  re_p = round(summary(model)$coef[1, 5], 3)
  result_built = paste0(round(re[1, 1], 2), " (", round(re[1, 3], 2), ", ", round(re[1, 4], 2), ")")
  result = matrix(c(result_built, re_p), ncol=2, nrow=1, byrow=T)
  return(result)}

FUN_model3 <- function(x, y, dataset){
  eval(parse(text=paste("model <- coxph(Surv(", y, ",", x, ") ~ lcz_2 + age + sex + ethnic + ses + nosmoking + drinkingM + sleeph + regular + dieth + dmp + htnp + opioid + NSAID, data=", dataset, ")", sep='')))
  re = summary(model)$conf.int
  re_p = round(summary(model)$coef[1, 5], 3)
  result_built = paste0(round(re[1, 1], 2), " (", round(re[1, 3], 2), ", ", round(re[1, 4], 2), ")")
  result = matrix(c(result_built, re_p), ncol=2, nrow=1, byrow=T)
  return(result)}

event_vars <- c("md", "mdd", "gad", "sud")
data0[event_vars] <- lapply(data0[event_vars], function(x) as.numeric(as.character(x)))
table_results <- data.frame( Disease = c("MD_Model1", "MD_Model2", "MD_Model3","MDD_Model1", "MDD_Model2", "MDD_Model3",
                                         "GAD_Model1", "GAD_Model2", "GAD_Model3","SUD_Model1", "SUD_Model2", "SUD_Model3"),built = NA,P_built = NA)

table_results[1, 2:3] <- FUN_model1("md", "md_time", "data0")
table_results[2, 2:3] <- FUN_model2("md", "md_time", "data0")
table_results[3, 2:3] <- FUN_model3("md", "md_time", "data0")

table_results[4, 2:3] <- FUN_model1("mdd", "mdd_time", "data0")
table_results[5, 2:3] <- FUN_model2("mdd", "mdd_time", "data0")
table_results[6, 2:3] <- FUN_model3("mdd", "mdd_time", "data0")

table_results[7, 2:3] <- FUN_model1("gad", "gad_time", "data0")
table_results[8, 2:3] <- FUN_model2("gad", "gad_time", "data0")
table_results[9, 2:3] <- FUN_model3("gad", "gad_time", "data0")

table_results[10, 2:3] <- FUN_model1("sud", "sud_time", "data0")
table_results[11, 2:3] <- FUN_model2("sud", "sud_time", "data0")
table_results[12, 2:3] <- FUN_model3("sud", "sud_time", "data0")

print(table_results)
write.xlsx(table_results, "/Users/guuuuu/Desktop/LCZ_mh/results0405/table3a_cox_lcz2.xlsx")


#######################################################################
###--------------------------------------------------------------------
### TABLE 3b MAIN RESULTS LCZ4 ###
FUN_model1_cat4 <- function(x, y, dataset){
  eval(parse(text=paste("model <- coxph(Surv(", y, ",", x, ") ~ lcz_4 + age + sex + ethnic, data=", dataset, ")", sep='')))
  re = summary(model)$conf.int
  re_coef = summary(model)$coef
  res_compact   = paste0(round(re[1, 1], 2), " (", round(re[1, 3], 2), ", ", round(re[1, 4], 2), ")"); p_compact  = round(re_coef[1, 5], 3)
  res_open      = paste0(round(re[2, 1], 2), " (", round(re[2, 3], 2), ", ", round(re[2, 4], 2), ")"); p_open     = round(re_coef[2, 5], 3)
  res_industry  = paste0(round(re[3, 1], 2), " (", round(re[3, 3], 2), ", ", round(re[3, 4], 2), ")"); p_industry = round(re_coef[3, 5], 3)
  result = matrix(c(res_compact, p_compact, res_open, p_open, res_industry, p_industry), ncol=6, nrow=1, byrow=T)
  return(result)}

FUN_model2_cat4 <- function(x, y, dataset){
  eval(parse(text=paste("model <- coxph(Surv(", y, ",", x, ") ~ lcz_4 + age + sex + ethnic + ses + nosmoking + drinkingM + sleeph + regular + dieth, data=", dataset, ")", sep='')))
  re = summary(model)$conf.int
  re_coef = summary(model)$coef
  res_compact   = paste0(round(re[1, 1], 2), " (", round(re[1, 3], 2), ", ", round(re[1, 4], 2), ")"); p_compact  = round(re_coef[1, 5], 3)
  res_open      = paste0(round(re[2, 1], 2), " (", round(re[2, 3], 2), ", ", round(re[2, 4], 2), ")"); p_open     = round(re_coef[2, 5], 3)
  res_industry  = paste0(round(re[3, 1], 2), " (", round(re[3, 3], 2), ", ", round(re[3, 4], 2), ")"); p_industry = round(re_coef[3, 5], 3)
  result = matrix(c(res_compact, p_compact, res_open, p_open, res_industry, p_industry), ncol=6, nrow=1, byrow=T)
  return(result)}

FUN_model3_cat4 <- function(x, y, dataset){
  eval(parse(text=paste("model <- coxph(Surv(", y, ",", x, ") ~ lcz_4 + age + sex + ethnic + ses + nosmoking + drinkingM + sleeph + regular + dieth + dmp + htnp + opioid + NSAID, data=", dataset, ")", sep='')))
  re = summary(model)$conf.int
  re_coef = summary(model)$coef
  res_compact   = paste0(round(re[1, 1], 2), " (", round(re[1, 3], 2), ", ", round(re[1, 4], 2), ")"); p_compact  = round(re_coef[1, 5], 3)
  res_open      = paste0(round(re[2, 1], 2), " (", round(re[2, 3], 2), ", ", round(re[2, 4], 2), ")"); p_open     = round(re_coef[2, 5], 3)
  res_industry  = paste0(round(re[3, 1], 2), " (", round(re[3, 3], 2), ", ", round(re[3, 4], 2), ")"); p_industry = round(re_coef[3, 5], 3)
  result = matrix(c(res_compact, p_compact, res_open, p_open, res_industry, p_industry), ncol=6, nrow=1, byrow=T)
  return(result)}

table_results_4cat <- data.frame(matrix(NA, nrow=12, ncol=7))
colnames(table_results_4cat) <- c("Disease", "compact","P_compact","open","P_open","industry","P_industry")

table_results_4cat$Disease <- c("MD_Model1", "MD_Model2", "MD_Model3", "MDD_Model1", "MDD_Model2", "MDD_Model3",
                                "GAD_Model1", "GAD_Model2", "GAD_Model3", "SUD_Model1", "SUD_Model2", "SUD_Model3")

table_results_4cat[1, 2:7] <- FUN_model1_cat4("md", "md_time", "data0")
table_results_4cat[2, 2:7] <- FUN_model2_cat4("md", "md_time", "data0")
table_results_4cat[3, 2:7] <- FUN_model3_cat4("md", "md_time", "data0")

table_results_4cat[4, 2:7] <- FUN_model1_cat4("mdd", "mdd_time", "data0")
table_results_4cat[5, 2:7] <- FUN_model2_cat4("mdd", "mdd_time", "data0")
table_results_4cat[6, 2:7] <- FUN_model3_cat4("mdd", "mdd_time", "data0")

table_results_4cat[7, 2:7] <- FUN_model1_cat4("gad", "gad_time", "data0")
table_results_4cat[8, 2:7] <- FUN_model2_cat4("gad", "gad_time", "data0")
table_results_4cat[9, 2:7] <- FUN_model3_cat4("gad", "gad_time", "data0")

table_results_4cat[10, 2:7] <- FUN_model1_cat4("sud", "sud_time", "data0")
table_results_4cat[11, 2:7] <- FUN_model2_cat4("sud", "sud_time", "data0")
table_results_4cat[12, 2:7] <- FUN_model3_cat4("sud", "sud_time", "data0")

print(table_results_4cat)
write.xlsx(table_results_4cat, "/Users/guuuuu/Desktop/LCZ_mh/results0405/table3b_cox_lcz4.xlsx")


#######################################################################
###--------------------------------------------------------------------
### TABLE 3c MAIN RESULTS LCZ11 ###
library(survival)
FUN_model1 <- function(x, y, dataset){
  eval(parse(text=paste("model <- coxph(Surv(", y, ",", x, ") ~ lcz_11 + age + sex + ethnic, data=", dataset, ")", sep='')))
  re = summary(model)$conf.int
  re_p = summary(model)$coef
  res_1 = paste0(round(re[1,1],2)," (",round(re[1,3],2),", ",round(re[1,4],2),")"); p_1 = round(re_p[1,5],3)
  res_2 = paste0(round(re[2,1],2)," (",round(re[2,3],2),", ",round(re[2,4],2),")"); p_2 = round(re_p[2,5],3)
  res_3 = paste0(round(re[3,1],2)," (",round(re[3,3],2),", ",round(re[3,4],2),")"); p_3 = round(re_p[3,5],3)
  res_4 = paste0(round(re[4,1],2)," (",round(re[4,3],2),", ",round(re[4,4],2),")"); p_4 = round(re_p[4,5],3)
  res_5 = paste0(round(re[5,1],2)," (",round(re[5,3],2),", ",round(re[5,4],2),")"); p_5 = round(re_p[5,5],3)
  res_6 = paste0(round(re[6,1],2)," (",round(re[6,3],2),", ",round(re[6,4],2),")"); p_6 = round(re_p[6,5],3)
  res_7 = paste0(round(re[7,1],2)," (",round(re[7,3],2),", ",round(re[7,4],2),")"); p_7 = round(re_p[7,5],3)
  res_8 = paste0(round(re[8,1],2)," (",round(re[8,3],2),", ",round(re[8,4],2),")"); p_8 = round(re_p[8,5],3)
  res_9 = paste0(round(re[9,1],2)," (",round(re[9,3],2),", ",round(re[9,4],2),")"); p_9 = round(re_p[9,5],3)
  res_10 = paste0(round(re[10,1],2)," (",round(re[10,3],2),", ",round(re[10,4],2),")"); p_10 = round(re_p[10,5],3)
  result = matrix(c(res_1,p_1,res_2,p_2,res_3,p_3,res_4,p_4,res_5,p_5,res_6,p_6,res_7,p_7,res_8,p_8,res_9,p_9,res_10,p_10), ncol=20, nrow=1, byrow=T)
  result}


FUN_model2 <- function(x, y, dataset){
  eval(parse(text=paste("model <- coxph(Surv(", y, ",", x, ") ~ lcz_11 + age + sex + ethnic + ses + nosmoking + drinkingM + sleeph + regular + dieth, data=", dataset, ")", sep='')))
  re = summary(model)$conf.int
  re_p = summary(model)$coef
  res_1 = paste0(round(re[1,1],2)," (",round(re[1,3],2),", ",round(re[1,4],2),")"); p_1 = round(re_p[1,5],3)
  res_2 = paste0(round(re[2,1],2)," (",round(re[2,3],2),", ",round(re[2,4],2),")"); p_2 = round(re_p[2,5],3)
  res_3 = paste0(round(re[3,1],2)," (",round(re[3,3],2),", ",round(re[3,4],2),")"); p_3 = round(re_p[3,5],3)
  res_4 = paste0(round(re[4,1],2)," (",round(re[4,3],2),", ",round(re[4,4],2),")"); p_4 = round(re_p[4,5],3)
  res_5 = paste0(round(re[5,1],2)," (",round(re[5,3],2),", ",round(re[5,4],2),")"); p_5 = round(re_p[5,5],3)
  res_6 = paste0(round(re[6,1],2)," (",round(re[6,3],2),", ",round(re[6,4],2),")"); p_6 = round(re_p[6,5],3)
  res_7 = paste0(round(re[7,1],2)," (",round(re[7,3],2),", ",round(re[7,4],2),")"); p_7 = round(re_p[7,5],3)
  res_8 = paste0(round(re[8,1],2)," (",round(re[8,3],2),", ",round(re[8,4],2),")"); p_8 = round(re_p[8,5],3)
  res_9 = paste0(round(re[9,1],2)," (",round(re[9,3],2),", ",round(re[9,4],2),")"); p_9 = round(re_p[9,5],3)
  res_10 = paste0(round(re[10,1],2)," (",round(re[10,3],2),", ",round(re[10,4],2),")"); p_10 = round(re_p[10,5],3)
  result = matrix(c(res_1,p_1,res_2,p_2,res_3,p_3,res_4,p_4,res_5,p_5,res_6,p_6,res_7,p_7,res_8,p_8,res_9,p_9,res_10,p_10), ncol=20, nrow=1, byrow=T)
  result}

FUN_model3 <- function(x, y, dataset){
  eval(parse(text=paste("model <- coxph(Surv(", y, ",", x, ") ~ lcz_11 + age + sex + ethnic + ses + nosmoking + drinkingM + sleeph + regular + dieth + dmp + htnp + opioid + NSAID, data=", dataset, ")", sep='')))
  re = summary(model)$conf.int
  re_p = summary(model)$coef
  res_1 = paste0(round(re[1,1],2)," (",round(re[1,3],2),", ",round(re[1,4],2),")"); p_1 = round(re_p[1,5],3)
  res_2 = paste0(round(re[2,1],2)," (",round(re[2,3],2),", ",round(re[2,4],2),")"); p_2 = round(re_p[2,5],3)
  res_3 = paste0(round(re[3,1],2)," (",round(re[3,3],2),", ",round(re[3,4],2),")"); p_3 = round(re_p[3,5],3)
  res_4 = paste0(round(re[4,1],2)," (",round(re[4,3],2),", ",round(re[4,4],2),")"); p_4 = round(re_p[4,5],3)
  res_5 = paste0(round(re[5,1],2)," (",round(re[5,3],2),", ",round(re[5,4],2),")"); p_5 = round(re_p[5,5],3)
  res_6 = paste0(round(re[6,1],2)," (",round(re[6,3],2),", ",round(re[6,4],2),")"); p_6 = round(re_p[6,5],3)
  res_7 = paste0(round(re[7,1],2)," (",round(re[7,3],2),", ",round(re[7,4],2),")"); p_7 = round(re_p[7,5],3)
  res_8 = paste0(round(re[8,1],2)," (",round(re[8,3],2),", ",round(re[8,4],2),")"); p_8 = round(re_p[8,5],3)
  res_9 = paste0(round(re[9,1],2)," (",round(re[9,3],2),", ",round(re[9,4],2),")"); p_9 = round(re_p[9,5],3)
  res_10 = paste0(round(re[10,1],2)," (",round(re[10,3],2),", ",round(re[10,4],2),")"); p_10 = round(re_p[10,5],3)
  result = matrix(c(res_1,p_1,res_2,p_2,res_3,p_3,res_4,p_4,res_5,p_5,res_6,p_6,res_7,p_7,res_8,p_8,res_9,p_9,res_10,p_10), ncol=20, nrow=1, byrow=T)
  result}

table_results_11cat <- data.frame(matrix(NA, nrow=12, ncol=21))
colnames(table_results_11cat) <- c("Disease", 
                                   "LCZ1","P1","LCZ2","P2","LCZ3","P3","LCZ4","P4","LCZ5","P5",
                                   "LCZ6","P6","LCZ7","P7","LCZ8","P8","LCZ9","P9","LCZ10","P10")

table_results_11cat$Disease <- c("MD_Model1", "MD_Model2", "MD_Model3", "MDD_Model1", "MDD_Model2", "MDD_Model3",
                                 "GAD_Model1", "GAD_Model2", "GAD_Model3", "SUD_Model1", "SUD_Model2", "SUD_Model3")

table_results_11cat[1, 2:21] <- FUN_model1("md", "md_time", "data0")
table_results_11cat[2, 2:21] <- FUN_model2("md", "md_time", "data0")
table_results_11cat[3, 2:21] <- FUN_model3("md", "md_time", "data0")

table_results_11cat[4, 2:21] <- FUN_model1("mdd", "mdd_time", "data0")
table_results_11cat[5, 2:21] <- FUN_model2("mdd", "mdd_time", "data0")
table_results_11cat[6, 2:21] <- FUN_model3("mdd", "mdd_time", "data0")

table_results_11cat[7, 2:21] <- FUN_model1("gad", "gad_time", "data0")
table_results_11cat[8, 2:21] <- FUN_model2("gad", "gad_time", "data0")
table_results_11cat[9, 2:21] <- FUN_model3("gad", "gad_time", "data0")

table_results_11cat[10, 2:21] <- FUN_model1("sud", "sud_time", "data0")
table_results_11cat[11, 2:21] <- FUN_model2("sud", "sud_time", "data0")
table_results_11cat[12, 2:21] <- FUN_model3("sud", "sud_time", "data0")

print(table_results_11cat)
write.xlsx(table_results_11cat, "/Users/guuuuu/Desktop/LCZ_mh/results0405/table3c_cox_lcz11.xlsx")
