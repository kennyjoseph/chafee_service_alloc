library(data.table)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(fixest)
library(scales)
library(tidytext)
library(plyr); library(dplyr)
library(Hmisc)
library(grid)

get_preds <- function(data, outcome_col, max_val){
  mlt <- melt(data, c("age","St","fold","outcome","RaceEthn",outcome_col), 
              grep("^pred_",names(d),value=T))
  setnames(mlt,outcome_col,"true")
  mlt[is.na(value)]$value <- 0
  mlt[value > max_val]$value <- max_val
  mlt[value < 0]$value <- 0
  setnames(mlt, "value","predicted")
  return(mlt)
}

clean_varnames <- function(sm){
  sm[, variable := sub("pred_","",variable)]
  sm[, includes_previous_year := !grepl("no_previous_year",variable)]
  sm[, is_baseline := ifelse(variable %in% c("constant","age","age_race","age_race_ru13","previous_year","age_state"), T,F)]
  sm[, modelname := ifelse(is_baseline, paste("Baseline:", capitalize(gsub("_",", ",variable))),
                           ifelse(grepl("xgb",variable),"GBT",
                                  ifelse(grepl("linreg",variable),
                                         "Linear Regression",
                                         ifelse(grepl("rf_",variable),
                                                "Random Forest",
                                                "Negative Binomial Regression"))))]
  sm[, outcome := sub("Services_Binary"," Services",outcome)]
  sm[, outcome := sub("Younger","Academic and\nEmployment Support\n",outcome)]
  sm[, modelname := sub("Previous, year", "Previous Year's\nService Count",modelname)]
  return(sm)
}

d <- fread("preds_2018_g.csv")

outcomes_map <- 
  c(
    "AllServices_Binary"=14,
    "YoungerServices_Binary" = 9,
    "FinancialServices_Binary"=4
  )
mlt <- data.table()

for(o in names(outcomes_map)){
  mlt <- rbind(mlt,get_preds(d[outcome == o],o , outcomes_map[o]) )
}

sm <- mlt[,sqrt(sum((true-predicted)**2)/.N), by=.(outcome,variable,fold)]
sm <- sm[, as.list(smean.cl.normal(V1)), by=.(outcome,variable)]
sm <- clean_varnames(sm)
theme_set(theme_bw(15))
pf <- ggplot(sm, aes(reorder(modelname,Mean),
               Mean,
               ymin=Lower,
               ymax=Upper,
               color = includes_previous_year,
               shape= includes_previous_year)) + 
  geom_pointrange(size=.8,position=position_dodge(width=.4))  + 
  facet_wrap(~outcome, scales="free_x")+
  coord_flip() +
  scale_color_discrete("Features\nInclude\nPrevious Year's\nServices",
                       labels=c("No","Yes")) +
  scale_shape_discrete("Features\nInclude\nPrevious Year's\nServices",
                       labels=c("No","Yes")) +
  xlab("Model") +
  ylab("RMSE (Mean+95%CI, 10-fold Cross-Validation)\nLower is Better")+
 theme(panel.spacing = unit(2, "lines"))
ggsave("img/predictions.pdf",pf,w=12,h=5)


p <- clean_varnames(mlt)

p


pm <- p[,sum(true-predicted)/.N, by=.(outcome,variable,fold,RaceEthn)]
pm <- pm[, as.list(smean.cl.normal(V1)), by=.(outcome,variable,RaceEthn)]
pm <- clean_varnames(pm)
pm[, race_ethn := mapvalues(RaceEthn,
                            c(1,2,3,4,5,6,7,99),
                            c("NH White",
                              "NH Black",
                              "NH Am Ind",
                              "NH Asian",
                              "NH Pac Islander",
                              "NH Multiple",
                              "Hispanic",
                              "Unknown"))]
pf <- ggplot(pm[RaceEthn %in% c(1,2,7) &
                  variable %in% c("age_race_ru13",#"age_state",
                                  "constant",
                                  #"previous_year",
                                  "xgb_all_features",
                                  "negbin_all_features")], 
             aes(race_ethn,
                 Mean,
                 ymin=Lower,
                 ymax=Upper,
                 color = outcome,
                 shape=outcome)) + 
  geom_pointrange(size=.5,position=position_dodge(width=.4))  + 
  facet_wrap(~modelname,scales="free",nrow=1) +
  geom_hline(yintercept = 0, color='red') +
  ylab("Difference Between True\nAnd Predicted Services Allocated")+
  xlab("Race/Ethnicity (Coded in NDACAN)") +
  theme(legend.position = "top",axis.text.x=element_text(angle=45,hjust=1))
ggsave("img/race_diff.pdf", pf, w=12,h=4)

