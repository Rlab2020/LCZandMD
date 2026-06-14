#######################################################################
###--------------------------------------------------------------------
### TABLE 0 LCZ CLASSIFICATION ###
data0 <- data0 %>%
  mutate(
    lcz0 = as.character(lcz0),
    lcz_2 = case_when(
      lcz0 %in% c("LCZ1","LCZ2","LCZ3","LCZ4","LCZ5","LCZ6","LCZ7","LCZ8","LCZ9","LCZ10") ~ "built",
      lcz0 %in% c("LCZA","LCZB","LCZC","LCZD","LCZE","LCZF","LCZG") ~ "nature",
      TRUE ~ NA_character_),
    lcz_4 = case_when(
      lcz0 %in% c("LCZ1","LCZ2","LCZ3") ~ "compact",
      lcz0 %in% c("LCZ4","LCZ5","LCZ6") ~ "open",
      lcz0 %in% c("LCZ7","LCZ8","LCZ9","LCZ10") ~ "industry",
      lcz0 %in% c("LCZA","LCZB","LCZC","LCZD","LCZE","LCZF","LCZG") ~ "nature",
      TRUE ~ NA_character_),
    lcz_11 = case_when(
      lcz0 %in% c("LCZA","LCZB","LCZC","LCZD","LCZE","LCZF","LCZG") ~ "nature",
      lcz0 %in% c("LCZ1","LCZ2","LCZ3","LCZ4","LCZ5","LCZ6","LCZ7","LCZ8","LCZ9","LCZ10") ~ lcz0,
      TRUE ~ NA_character_),
    lcz_2 = factor(lcz_2, levels = c("nature","built")),
    lcz_4 = factor(lcz_4, levels = c("nature","compact","open","industry")),
    lcz_11 = factor(lcz_11, levels = c("nature","LCZ1","LCZ2","LCZ3","LCZ4","LCZ5","LCZ6","LCZ7","LCZ8","LCZ9","LCZ10")))

tabs <- list(
  LCZ0 = data.frame(Category=c(paste0("LCZ",1:10), paste0("LCZ",LETTERS[1:7])), N=c(sapply(paste0("LCZ",1:10),function(x)sum(data0$lcz0==x,na.rm=TRUE)), sapply(paste0("LCZ",LETTERS[1:7]),function(x)sum(data0$lcz0==x,na.rm=TRUE)))),
  LCZ_2 = data.frame(Category=c("nature","built"), N=c(sum(data0$lcz_2=="nature",na.rm=TRUE), sum(data0$lcz_2=="built",na.rm=TRUE))),
  LCZ_4 = data.frame(Category=c("nature","compact","open","industry"), N=c(sum(data0$lcz_4=="nature",na.rm=TRUE),sum(data0$lcz_4=="compact",na.rm=TRUE),sum(data0$lcz_4=="open",na.rm=TRUE),sum(data0$lcz_4=="industry",na.rm=TRUE))),
  LCZ_11 = data.frame(Category=c("nature",paste0("LCZ",1:10)), N=c(sum(data0$lcz_11=="nature",na.rm=TRUE), sapply(paste0("LCZ",1:10),function(x)sum(data0$lcz_11==x,na.rm=TRUE)))))

for(n in names(tabs)) {
  tabs[[n]]$Percent <- round(tabs[[n]]$N/sum(tabs[[n]]$N)*100,1)
  tabs[[n]]$`N (%)` <- paste0(tabs[[n]]$N," (",tabs[[n]]$Percent,"%)")}

dir.create("/Users/guuuuu/Desktop/LCZ_mh/results0405", recursive=TRUE, showWarnings=FALSE)
wb <- createWorkbook()
headerStyle <- createStyle(textDecoration="bold", halign="center")

for(n in names(tabs)) {addWorksheet(wb,n)
  writeData(wb,n,tabs[[n]])
  setColWidths(wb,n,"auto")
  addStyle(wb,n,headerStyle,rows=1,cols=1:4,gridExpand=TRUE)}
saveWorkbook(wb,"/Users/guuuuu/Desktop/LCZ_mh/results0405/table0_lcz_class_description.xlsx",overwrite=TRUE)


#######################################################################
###--------------------------------------------------------------------
### TABLE 1 BASELINE POPULATION ###
load("/Users/guuuuu/Desktop/LCZ_mh/data/data_lcz_mh.rda")
library(compareGroups)
data_lcz_mh$lcz_2 <- dplyr::case_when(
  data_lcz_mh$lcz0 %in% c("LCZ1","LCZ2","LCZ3","LCZ4","LCZ5","LCZ6","LCZ7","LCZ8","LCZ9","LCZ10") ~ "built",
  data_lcz_mh$lcz0 %in% c("LCZA","LCZB","LCZC","LCZD","LCZE","LCZF","LCZG") ~ "nature", TRUE ~ NA_character_)
data_lcz_mh$lcz_2 <- factor(data_lcz_mh$lcz_2, levels = c("nature", "built"))
data_table1 <- data0 %>% dplyr::select(lcz_2, age, sex, ethnic, ses, bmi, whr, nosmoking, drinkingM, sleeph, regular, dieth, dmp, htnp, opioid, NSAID, fsp0610, ndvi0610, lst0610)
table1 <- compareGroups(lcz_2 ~ ., data = data_table1)
table1 <- createTable(table1, show.all = TRUE, hide.no = "no", show.p.overall = TRUE)
export2xls(table1, file = "/Users/guuuuu/Desktop/LCZ_mh/results0405/table1_baseline_description.xlsx")

