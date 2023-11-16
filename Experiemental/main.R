rm(list=ls())
# setting the working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####### load libraries ####### 
library(dplyr)
library(lubridate)
library(plotrix)
library(ggplot2)
library(grid)
library(gridExtra)
library(data.table)
library(tidyverse)
library(ggthemes)
library(extrafont)
library(ggtext)
library(reshape2)
library(ggpubr)
library(rstatix)


####### helper functions ####### 
pvalue_round = function(pvalue){
  # input: p-value as a float
  # output: if it's less than 0.001, return a string "< 0.001"
  #         otherwise round to 3rd decimal
  cat = ""
  if (pvalue < 0.001){
    cat = "p < 0.001"
  } else {
    cat = paste0("p = ", round(pvalue, 3))
  }
  return(cat)
}

parse_date_manual = function(data){
  # each study's date has a different format
  # this function harmonizes them
  p1 = subset(data, study %in% c(1, 2, 3))
  p1$date = parse_date_time(sapply(strsplit(p1$date," "), `[`, 1), orders = c("mdy"))
  p2 = subset(data, study==4)
  p2$date = parse_date_time(sapply(strsplit(p2$date," "), `[`, 1), orders = c("dmy"))
  p3 = subset(data, study==5)
  p3$date = parse_date_time(sapply(strsplit(p3$date," "), `[`, 1), orders = c("ymd"))
  return(rbind(p1, p2, p3))
}

figure2 = function(data){
  # input: subset of the data corresponding to Study 1
  # output: barplot
  # prints: caption to be then used in the paper
  d = data %>% 
    select(p1_human_wants, p1_bot_wants, p1_bot_asif,
           twitter_human_wants, twitter_bot_wants, twitter_bot_asif,
           wiki_human_wants, wiki_bot_wants, wiki_bots_asif) %>% 
    mutate(id = paste0("subj", 1:299)) %>% 
    gather(key = transit, value = score, p1_human_wants:wiki_bots_asif) %>% 
    separate(col = transit, into = c("coin", "agent", "preference"), sep = "_") %>% 
    mutate(coin = case_when(coin == "p1" ~ "Win money\nin the game",
                            coin == "twitter" ~ "Get likes\non Twitter",
                            coin == "wiki" ~ "Avoid ban\non Wikipedia")) %>% 
    mutate(agent = ifelse(agent == "human", "human", "bot")) %>% 
    unite(question, agent:preference, remove = T) 
  
  fig =  d %>% 
    dplyr::group_by(coin, question) %>% 
    dplyr::summarize(Score = mean(score, na.rm = T), se = sciplot::se(score)) %>% 
    ggplot(., aes(coin, Score, color = question)) +
    geom_segment(aes(x = coin, xend = coin, y = Score - 1.96*se, yend = Score + 1.96*se), size = 5, alpha = .30, lineend = "round")+
    geom_hline(yintercept = 50, linetype = "dashed", color = "grey20")+
    geom_point(size = 5)+
    scale_color_manual(values = c("#EE7733", "#CC3311", "#009988"))+
    scale_y_continuous(limits = c(0,100), 
                       breaks = c(0, 25, 50, 75, 100), 
                       labels = c("Definitely\ndisagree", "25", "50", "75", "Definitely\nagree"),
    )+
    labs(x = "", y = "")+
    coord_flip()+
    theme_minimal()+
    theme(legend.position = "none",
          axis.line.x = element_line(color = "grey20"),
          axis.title.x = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          aspect.ratio = 1/3)
  caption = "Results of Study 1. Data are presented as mean values and 95% confidence intervals."
  n = dim(data)[1]
  caption = paste(caption, "Number of participants in this study is", n,".")
  print(caption)
  return(fig)
}


ttest_extract = function(test_res, eff_res){
  # given the t-test output, print the relevant information
  tstat = round(test_res$statistic,3)
  lower_bound = round(test_res$conf.int[[1]],3)
  upper_bound = round(test_res$conf.int[[2]], 3)
  df = round(test_res$parameter, 1)
  pval = pvalue_round(test_res$p.value)
  eff_size = round(eff_res$effsize, 2)
  return(paste0("degrees of freedom = $", df, "$, ",
                "$",pval, "$, ",
                "t-statistic = $", tstat ,"$, ",
                "Cohen's $d = ", eff_size, "$, ",
                "95\\% Confidence Intervals = (", lower_bound, ", " , upper_bound  ,")."))
}

create_helper_vars = function(){
  # input: none
  # output: useful variables for using while
  #         creating the figure, e.g. order of
  #         bars, columns to keep for each treatment,
  #         what colors to use for each treatment
  order_lookup <- vector(mode="list", length=4)
  names(order_lookup) = c("1", "2", "4(H)", "4(P)")
  order_lookup[["1"]] = c("p2bot","no_bot")
  order_lookup[["3"]] = c("p1bot","p2bot","no_bot")
  order_lookup[["4(H)"]] = c("p2bot","p1bot","no_bot")
  order_lookup[["4(P)"]] = c("p1bot","p2bot", "p3bot","no_bot")
  
  keep_cols <- vector(mode="list", length=6)
  names(keep_cols) = c("1", "2", "4(H)", "4(P)", "4H_trust", "4P_trust")
  keep_cols[["1"]] = c("scenario", "P1S1")
  keep_cols[["3"]] = c("scenario","P3S1")
  keep_cols[["4(H)"]] = c("scenario","P4HS1_Yes","P4HS1_No")
  keep_cols[["4(P)"]] = c("scenario","P4PS1_Yes", "P4PS1_No")
  keep_cols[["4H_trust"]] = c("scenario","P4H_trust_gain")
  keep_cols[["4P_trust"]] = c("scenario","P4P_trust_gain")
  
  colors <- vector(mode="list", length=6)
  names(colors) = c("1", "2", "4(H)", "4(P)", "4H_trust","4P_trust")
  colors[["1"]] = "#2968a9"
  colors[["3"]] = "#3da23a"
  colors[["4(H)"]] = c("#83328F", "#BF91C6")
  colors[["4(P)"]] = c("#83328F", "#BF91C6")
  colors[["4H_trust"]] = "#2968a9"
  colors[["4P_trust"]] = "#3da23a"
  
  return(list(order_lookup, keep_cols, colors))
}


create_stat_test = function(df, study_id="55", player="5", keep_cols_name="5", is_fig4 = FALSE, panel=""){
  # input: df = dataset
  #        study_id = study number for subsetting
  #        player = which player to use
  #        keep_cols_name = either same as player or ends with _trust
  #                         depending on this, will get specific list of columns
  # output: subset dataframe, tibble with t-tests and p-values. these
  #         are for adding the pvalue text on the figure
  if (is_fig4==FALSE){
    if ((player %in%  c("1", "3") | grepl("_trust", keep_cols_name))){
      # for players 1, 3, and trust_gain, create stat.test to add to the plot
      subdf = subset(df, is_player==player & study==study_id, select=keep_cols[[keep_cols_name]])
      names(subdf)[2] = "value"
      if (grepl("_trust", keep_cols_name)){
        subdf["trust"] = "none"
      }
      subdf$scenario = factor(subdf$scenario, levels=order_lookup[[player]])
      stat.test <- subdf %>%
        t_test(value ~ scenario, ref.group = "no_bot") %>%
        add_xy_position(x = "scenario", dodge = 0.8) %>%
        rowwise %>%
        mutate(p_sig =  pvalue_round(p))
      return(list(subdf, data.frame(stat.test)))        
    } else {
      subdf = subset(data, is_player==player & study==study, select=keep_cols[[player]])
      subdf <- melt(setDT(subdf), id.vars = c("scenario"), variable.name = "trust")
      subdf = data.frame(subdf)
      subdf$scenario = factor(subdf$scenario, levels=order_lookup[[player]])
      if (player=="4(H)"){
        comparison1 = "P4HS1_Yes"
        comparison2 = "P4HS1_No"
        nudge1 = c(20, 12)
        nudge2 = c(-112, -119)
      } else {
        comparison1 = "P4PS1_Yes"
        comparison2 = "P4PS1_No"
        nudge1 = c(10, 20, 30)
        nudge2 = c(-115, -125, -135)
      }
      stat.test = subset(subdf, trust==comparison1) %>%
        t_test(value ~ scenario, ref.group = "no_bot") %>%
        add_xy_position(x = "scenario", dodge = 0.8) %>%
        rowwise %>%
        dplyr::mutate(p_sig =  pvalue_round(p))
      
      stat.test_no = subset(subdf, trust==comparison2) %>%
        t_test(value ~ scenario, ref.group = "no_bot") %>%
        add_xy_position(x = "scenario", dodge = 0.8) %>%
        rowwise %>%
        dplyr::mutate(p_sig =  pvalue_round(p))
      
      
      stat.test = data.frame(stat.test)
      stat.test$xmax = stat.test$xmax + 0.1
      stat.test$xmin = stat.test$xmin + 0.1
      stat.test_no = data.frame(stat.test_no)
      stat.test_no$xmax = stat.test_no$xmax - 0.3
      stat.test_no$xmin = stat.test_no$xmin - 0.3
      stat.test["trust"] = "yes"
      stat.test_no["trust"] = "no"
      stat.test = rbind(stat.test, stat.test_no)
      
      return(list(subdf, stat.test))
      
    }
  }
  else {
    if(panel=="a"){
      ppl = df$uniqueId[which(df$study == 4 & df$is_player=="4(H)")]
      
      keep_cols = c("P4H_trust_gain", "scenario", "study", "uniqueId")
      df = subset(df, uniqueId %in% ppl & study %in% c(2, 4) , select = keep_cols)
      df$with_norm = ifelse(df$study==2, "no", "yes")
      df2 = subset(df, study==2)
      df4 = subset(df, study==4)
      df = rbind(df2,
                 df4[match(df2$uniqueId, df4$uniqueId),])
      names(df)[1] = "value"
      df$scenario = factor(df$scenario, levels=order_lookup[["4(H)"]])
      df$trust = df$with_norm
      
      stat.test <- subset(df, with_norm=="yes") %>%
        t_test(value ~ scenario, ref.group = "no_bot") %>%
        add_xy_position(x = "scenario", dodge = 0.8) %>%
        rowwise %>%
        dplyr::mutate(p_sig =  pvalue_round(p))
      
      stat.test_within <- subset(df, scenario !="no_bot") %>%
        group_by(scenario) %>%
        t_test(value ~ with_norm,paired = T) %>%
        add_xy_position(x = "scenario", dodge = 0.8) %>%
        rowwise %>%
        dplyr::mutate(p_sig =  pvalue_round(p))
      
      stat.test = data.frame(stat.test)
      stat.test$xmax = stat.test$xmax + 0.15
      stat.test$xmin = stat.test$xmin + 0.15
      stat.test_within = data.frame(stat.test_within)
      stat.test["trust"] = "between"
      stat.test_within["trust"] = "within"
    } else {
      df = subset(df, (study==5 & is_player=="4(H)" & with_norm==1 &  info_true=="Yes") | (study==5 & is_player=="4(H)" & with_norm==0), select = c("P4H_trust_gain", "scenario", "with_norm"))
      df$with_norm = factor(df$with_norm, labels=c("no", "yes"))
      names(df)[1] = "value"
      df$scenario = factor(df$scenario, levels=order_lookup[["4(H)"]])
      df$trust = df$with_norm
      stat.test <- subset(df, with_norm=="yes") %>%
        t_test(value ~ scenario, ref.group = "no_bot") %>%
        add_xy_position(x = "scenario", dodge = 0.8) %>%
        rowwise %>%
        dplyr::mutate(p_sig =  pvalue_round(p))
      
      stat.test_within <- subset(df, scenario !="no_bot") %>%
        group_by(scenario) %>%
        t_test(value ~ with_norm) %>%
        add_xy_position(x = "scenario", dodge = 0.8) %>%
        rowwise %>%
        dplyr::mutate(p_sig =  pvalue_round(p))
      
      stat.test = data.frame(stat.test)
      stat.test$xmax = stat.test$xmax + 0.15
      stat.test$xmin = stat.test$xmin + 0.15
      stat.test_within = data.frame(stat.test_within)
      stat.test["trust"] = "between"
      stat.test_within["trust"] = "within"
    }
    return(list(df, stat.test, stat.test_within))
  }
  
}

barplot_manual = function(subdf, stat_tbl=NULL, player, keep_cols_name, nudge=NULL){
  p <- ggbarplot(
    subdf, x = "scenario", y = "value", add = "mean_se", 
    add.params = list(width=0, color="black"),  color = colors[[keep_cols_name]], fill=colors[[keep_cols_name]],
    position = position_dodge(0.8), width = 0.25)  + 
    stat_pvalue_manual(stat_tbl,  tip.length = 0.01,
                       label = "p_sig", coord.flip = TRUE, 
                       bracket.nudge.y = nudge) + 
    theme_minimal() + 
    coord_flip() + 
    xlab("") + 
    ylab("")
  
  return(p)
}


boxjitter_creator = function(df, stat.test, player, is_trust, colors="None", paired="None", is_norm=FALSE, extra_stats=""){
  if (is_norm==FALSE){
    if (player=="4(H)" & is_trust==FALSE){
      comparison1 = "P4HS1_Yes"
      comparison2 = "P4HS1_No"
      nudge1 = c(20, 12)
      nudge2 = c(-112, -119)
      
    } 
    if (player=="4(P)" & is_trust==FALSE){
      comparison1 = "P4PS1_Yes"
      comparison2 = "P4PS1_No"
      nudge1 = c(10, 20, 30)
      nudge2 = c(-115, -125, -135)
      
    }
    if (player=="4P_trust"){
      nudgey_val = c(13, 23, 33)
    }
    
    if (player=="4H_trust" & is_trust==TRUE){
      nudgey_val = c(13, 20)
    }
  }
  
  output = ggplot(df) + 
    geom_boxjitter(aes(x = scenario, y = value, fill=trust), 
                   fatten=0, 
                   jitter.shape = 21, jitter.color = NA,
                   outlier.color = NA, errorbar.draw = TRUE,
                   position=position_dodge(0.8)) +
    scale_fill_manual(values = colors) +
    theme_minimal()+
    theme(legend.position="none",
          panel.grid.minor = element_blank())+
    coord_flip() 
  if (is_norm==TRUE){
    colors = c( "#307db8ff", "#a1c7e3ff")
    nudge1 = c(25, 35)
    nudge2 = c(-190, -205)
    output = output + 
      scale_fill_manual(values = colors) +
      scale_y_continuous(breaks = round(seq(-120, 140, by = 20),1),
                         labels = round(seq(-120, 140, by = 20),1),
                         limits=c(-120, 140)) +
      stat_pvalue_manual(stat.test, label="p_sig", 
                         bracket.nudge.y = nudge1,
                         coord.flip = TRUE,
                         tip.length = 0.01) +
      
      stat_pvalue_manual(extra_stats, label="p_sig", 
                         bracket.nudge.y = nudge2,
                         coord.flip = TRUE,
                         tip.length = 0.01) +
      stat_summary(geom = "crossbar", fatten=2, width=0.325,  color="black", 
                   fun.data = function(x){c(y=mean(x), ymin=mean(x), ymax=mean(x))},
                   aes(x = scenario, y=value, group=trust),
                   position=position_dodge(c(0.435, 1.167)))
  } else if (is_trust==FALSE){
    output = output + 
      stat_summary(geom = "crossbar", fatten=2, width=0.325,  color="black", 
                   fun.data = function(x){c(y=mean(x), ymin=mean(x), ymax=mean(x))},
                   aes(x = scenario, y=value, group=trust),
                   position=position_dodge(c(0.435, 1.167)))+
      scale_y_continuous(breaks = round(seq(-40, 140, by = 20),1),
                         labels = round(seq(-40, 140, by = 20),1),
                         limits=c(-40, 140)) +
      stat_pvalue_manual(stat.test[which(stat.test$trust=="yes"),], label="p_sig", 
                         bracket.nudge.y = nudge1,
                         coord.flip = TRUE,
                         tip.length = 0.01) +
      
      stat_pvalue_manual(stat.test[which(stat.test$trust=="no"),], label="p_sig", 
                         bracket.nudge.y = nudge2,
                         coord.flip = TRUE,
                         tip.length = 0.01)
  } else {
    
    output = output + 
      stat_summary(geom = "crossbar", fatten=2, width=0.375,  color="black", 
                   fun.data = function(x){c(y=mean(x), ymin=mean(x), ymax=mean(x))},
                   aes(x = scenario, y=value, group=trust),
                   position=position_nudge(x=-0.18))+
      scale_y_continuous(breaks = round(seq(-140, 140, by = 20),1),
                         labels = round(seq(-140, 140, by = 20),1),
                         limits=c(-140, 140)) +
      stat_pvalue_manual(stat.test, label="p_sig", 
                         bracket.nudge.y = nudgey_val,
                         coord.flip = TRUE,
                         tip.length = 0.01)
  }
  
  return(output)
}
####### reading the data ####### 
data = read.csv("HB_data.csv", stringsAsFactors = F)
# keep only people who finished the studies. note that we will bring them back later
# for analyzing attrition rates

data = subset(data, finished==1)
# harmonize the date variable
data = parse_date_manual(data)

####### Tables ####### 
# Study 3 Results

# Running the necessary regressions for constructing the tables (Tables 1 and 2)
# Helping main effect
model_cont_self_help <- lm(trust_gain ~ guess_norm_all, 
                           data = data[which(data$is_player == "4(H)"  & data$study==3),])

# Helping with controls
model_cont_self_help_all <- lm(trust_gain ~ guess_norm_all  + guess_emp_all + factor(treatment)
                               + factor(age_categories) + factor(race_categories)
                               + factor(income_categories) + factor(edu_categories) 
                               + factor(gender_categories) + factor(region) + factor(norm_self), 
                               data = data[which(data$is_player == "4(H)"  & data$study==3),])

main_eff_model = summary(model_cont_self_help)
main_effect = main_eff_model$coef["guess_norm_all", c("Estimate", "Pr(>|t|)")]
main_effect_ci = round(confint(model_cont_self_help)["guess_norm_all",], 3)
n_obs_main = dim(data[which(data$is_player == "4(H)" & data$study==3),])[1]

controls_eff_model = summary(model_cont_self_help_all)
effect_controls = controls_eff_model$coef["guess_norm_all", c("Estimate", "Pr(>|t|)")]
effect_controls_ci = round(confint(model_cont_self_help_all)["guess_norm_all",], 3)
n_obs_controls = dim(data[which(data$is_player == "4(H)"  & data$study==3),])[1]

Table1 =  paste0("Table 1 \n",
                 "-------------------", "\n",
                 "Main effect", "\n",
                 "Coefficient: ", round(main_effect["Estimate"],3), "\n",
                 "p-value: p ", pvalue_round(main_effect["Pr(>|t|)"]), "\n",
                 "95% CI: (", round(main_effect_ci["2.5 %"], 3), ", ", round(main_effect_ci["97.5 %"], 3), ") \n",
                 "N: ", n_obs_main, "\n",
                 "Adj. R squared: ", round(main_eff_model$adj.r.squared, 3) ,"\n", 
                 "-------------------", "\n",
                 "With controls", "\n",
                 "Coefficient: ", round(effect_controls["Estimate"],3), "\n",
                 "p-value: p ", pvalue_round(effect_controls["Pr(>|t|)"]), "\n",
                 "95% CI: (", round(effect_controls_ci["2.5 %"], 3), ", ", round(effect_controls_ci["97.5 %"], 3), ") \n",
                 "N: ", n_obs_controls, "\n",
                 "Adj. R squared: ", round(controls_eff_model$adj.r.squared, 3)
)

# Punishing main effect
model_cont_self_punish <- lm(trust_gain ~ guess_norm_all, 
                             data = data[which(data$is_player == "4(P)" & data$study==3),])

#### Punishment with controls
model_cont_self_punish_all <- lm(trust_gain ~ guess_norm_all + guess_emp_all  + factor(treatment)
                                 + factor(age_categories) + factor(race_categories)
                                 + factor(income_categories) + factor(edu_categories) 
                                 + factor(gender_categories) + factor(region) + factor(norm_self), 
                                 data = data[which(data$is_player == "4(P)" & data$study==3),])

main_eff_model = summary(model_cont_self_punish)
main_effect = main_eff_model$coef["guess_norm_all", c("Estimate", "Pr(>|t|)")]
main_effect_ci = round(confint(model_cont_self_punish)["guess_norm_all",], 3)
n_obs_main = dim(data[which(data$is_player == "4(P)" & data$study==3),])[1]

controls_eff_model = summary(model_cont_self_punish_all)
effect_controls = controls_eff_model$coef["guess_norm_all", c("Estimate", "Pr(>|t|)")]
effect_controls_ci = round(confint(model_cont_self_punish_all)["guess_norm_all",], 3)
n_obs_controls = dim(data[which(data$is_player == "4(P)"  & data$study==3),])[1]
Table2 =  paste0("Table 2 \n",
                 "-------------------", "\n",
                 "Main effect \n",
                 "Coefficient: ", round(main_effect["Estimate"],3), "\n",
                 "p-value: p ", pvalue_round(main_effect["Pr(>|t|)"]), "\n",
                 "95% CI: (", round(main_effect_ci["2.5 %"], 3), ", ", round(main_effect_ci["97.5 %"], 3), ") \n",
                 "N: ", n_obs_main, "\n",
                 "Adj. R squared: ", round(main_eff_model$adj.r.squared, 3) ,"\n", 
                 "-------------------", "\n",
                 "With controls", "\n",
                 "Coefficient: ", round(effect_controls["Estimate"],3), "\n",
                 "p-value: p ", pvalue_round(effect_controls["Pr(>|t|)"]), "\n",
                 "95% CI: (", round(effect_controls_ci["2.5 %"], 3), ", ", round(effect_controls_ci["97.5 %"], 3), ") \n",
                 "N: ", n_obs_controls, "\n",
                 "Adj. R squared: ", round(controls_eff_model$adj.r.squared, 3)
)

cat(Table1)
cat(Table2)


####### Figures ####### 

# Figure 2: no sample restrictions are applied
figure2(subset(data, study==1))

# Figure 3: no sample restrictions are applied
order_lookup = create_helper_vars()[[1]]
keep_cols = create_helper_vars()[[2]]
colors = create_helper_vars()[[3]]


p1 = create_stat_test(data, 2, "1", "1")
p1 = barplot_manual(p1[[1]], p1[[2]], player="1", keep_cols_name = "1")

p3 = create_stat_test(data, 2, "3", "3")
p3 = barplot_manual(p3[[1]], p3[[2]], player="3", keep_cols_name = "3", nudge = c(-0.45, -0.4))



p4h = create_stat_test(data, 2, "4(H)", "4(H)")
p4h = boxjitter_creator(p4h[[1]], p4h[[2]], is_trust=FALSE, player="4(H)",colors=colors[["4(H)"]])

p4p = create_stat_test(data, 2, "4(P)", "4(P)")
p4p = boxjitter_creator(p4p[[1]], p4p[[2]], is_trust=FALSE, player="4(P)",colors=colors[["4(P)"]])


p4h_trust = create_stat_test(data, 2, "4(H)", "4H_trust")
p4h_trust = boxjitter_creator(p4h_trust[[1]], p4h_trust[[2]], is_trust=TRUE, player="4H_trust",colors=colors[["4H_trust"]])



p4p_trust = create_stat_test(data, 2, "4(P)", "4P_trust")
p4p_trust = boxjitter_creator(p4p_trust[[1]], p4p_trust[[2]], is_trust=TRUE, player="4P_trust",colors=colors[["4P_trust"]])


grid.arrange(p1,p3,p4h,p4h_trust,p4p, p4p_trust, 
             layout_matrix = rbind(c(1,NA),c(2,NA), c(3,4), c(5,6)),
             top=textGrob("Figure 3",gp=gpar(fontsize=12,font=3)))

p = subset(data, study==2, select=c("scenario", "is_player", "uniqueId")) %>%
  dplyr::group_by(scenario, is_player) %>%
  dplyr::summarise(n = length(uniqueId)) 



# Part of the caption for the figure
cat(
  paste0(
    "In subsection *a* the number of participants is, ", p$n[which(p$scenario=="no_bot" & p$is_player==1)], 
    ", ", p$n[which(p$scenario=="p2bot" & p$is_player==1)] ," corresponding to the plotted order of bars. \n",
    "In subsection *b* the number of participants is ", p$n[which(p$scenario=="no_bot" & p$is_player==3)], ", ",
    p$n[which(p$scenario=="p2bot" & p$is_player==3)], " and ", 
    p$n[which(p$scenario=="p1bot" & p$is_player==3)] ," corresponding to the plotted order of bars. \n",
    "In subsection *c* the number of participants is, ", p$n[which(p$scenario=="no_bot" & p$is_player=="4(H)")], ", ",
    p$n[which(p$scenario=="p2bot" & p$is_player=="4(H)")], " and ", 
    p$n[which(p$scenario=="p1bot" & p$is_player=="4(H)")] ," corresponding to the plotted order of box plots. \n",
    "In subsection *d* the number of participants is, ", p$n[which(p$scenario=="no_bot" & p$is_player=="4(P)")], ", ",
    p$n[which(p$scenario=="p3bot" & p$is_player=="4(P)")], ", ",
    p$n[which(p$scenario=="p2bot" & p$is_player=="4(P)")], " and ", 
    p$n[which(p$scenario=="p1bot" & p$is_player=="4(P)")] ," corresponding to the plotted order of box plots. \n"
  )
)

# Figure 4:

order_lookup = create_helper_vars()[[1]]
keep_cols = create_helper_vars()[[2]]
colors = create_helper_vars()[[3]]

p4_s4 = create_stat_test(data, is_fig4 = TRUE, panel="a")
s4_df = p4_s4[[1]]
p4_s4 = boxjitter_creator(p4_s4[[1]], p4_s4[[2]],is_norm=TRUE, extra_stats = p4_s4[[3]])

p4_s5 = create_stat_test(data, is_fig4 = TRUE, panel="b") 
s5_df = p4_s5[[1]]
p4_s5 = boxjitter_creator(p4_s5[[1]], p4_s5[[2]],is_norm=TRUE, extra_stats = p4_s5[[3]])

grid.arrange(p4_s4, p4_s5, 
             layout_matrix = rbind(c(1,NA),c(2,NA)),
             top=textGrob("Figure 4",gp=gpar(fontsize=12,font=3)))

s4_ppl = s4_df %>%
  group_by(with_norm, scenario) %>%
  dplyr::summarise(n = length(value)) %>%
  arrange(scenario)

s5_ppl = s5_df %>%
  group_by(with_norm, scenario) %>%
  dplyr::summarise(n = length(value)) %>%
  arrange(scenario)

s4_ppl$panel = "a"
s5_ppl$panel = "b"
p = plyr::rbind.fill(s4_ppl,s5_ppl)

cat(
  paste0(
    "In panel (a) the number of participants is ", 
    p$n[which(p$scenario=="no_bot" & p$panel=="a")], ", ", 
    p$n[which(p$scenario=="p2bot" & p$panel=="a")], ", ",
    p$n[which(p$scenario=="p1bot" & p$panel=="a")],
    " corresponding to the plotted order of pairs of box plots. \n",
    "In panel (b) the number of participants is ", 
    p$n[which(p$scenario=="no_bot" & p$panel=="b" & p$with_norm=="no")], ", ",
    p$n[which(p$scenario=="no_bot" & p$panel=="b" & p$with_norm=="yes")], ", ",
    p$n[which(p$scenario=="p2bot" & p$panel=="b" & p$with_norm=="no")], ", ",
    p$n[which(p$scenario=="p2bot" & p$panel=="b" & p$with_norm=="yes")],   ", ",
    p$n[which(p$scenario=="p1bot" & p$panel=="b" & p$with_norm=="no")] , " and ",
    p$n[which(p$scenario=="p1bot" & p$panel=="b" & p$with_norm=="yes")] ," corresponding to the plotted order of pairs of box plots \n"
  ))
