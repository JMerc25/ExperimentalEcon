rm(list=ls())
# setting the working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####### load libraries ####### 
library(dplyr)
library(lubridate)
library(plotrix)
library(ggplot2)
library(ggpol)
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
library(fastDummies)



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

stats = function(vecs){
  avg = mean(vecs)
  se = sciplot::se(vecs)
  ci.lo = avg-1.96*se
  ci.hi = avg+1.96*se 
  
  return(paste0("   mean: ", round(avg, 1), "\n",
                "    CI: [", round(ci.lo, 3), ", ", round(ci.hi, 3), "] \n"))
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


###### Abstract ###### 

# number of unique people 
n_people = length(unique(data$uniqueId))
print(paste0("Number of unique participants: ", n_people))


###### Results ###### 

# Money in the game is comparable to other online currencies

# number of participants in Study 1
n_people_s1 = sum(data$study==1)
print(paste0("Number of unique participants in Study 1: ", n_people_s1))

# Figure 2 

figure2(subset(data, study==1))

# Statistical comparisons

p1_human_wants = t.test(data$p1_human_wants[which(data$study==1)], mu = 50, alternative = "greater")
twitter_human_wants = t.test(data$twitter_human_wants[which(data$study==1)], mu = 50, alternative = "greater")
wiki_human_wants = t.test(data$wiki_human_wants[which(data$study==1)], mu = 50, alternative = "greater")


effp1 = data[which(data$study==1),] %>% cohens_d(p1_human_wants ~ 1, mu = 50)
efftwitter = data[which(data$study==1),] %>% cohens_d(twitter_human_wants ~ 1, mu = 50)
effwiki = data[which(data$study==1),] %>% cohens_d(wiki_human_wants ~ 1, mu = 50)

cat(paste0("Study 1: t-tests \n",
           "In our paradigm: ", ttest_extract(p1_human_wants, effp1), "\n",
           "In Twitter: ", ttest_extract(twitter_human_wants, efftwitter), "\n",
           "In Wikipedia: ",ttest_extract(wiki_human_wants, effwiki), "\n"))


# Humans' preferences for different currencies
cat("Study 1: humans' preference for currencies \n", 
    "Money: \n",
    stats(data$p1_human_wants[which(data$study==1)]),
    "Likes: \n",
    stats(data$twitter_human_wants[which(data$study==1)]),
    "Bans: \n",
    stats(data$wiki_human_wants[which(data$study==1)]))

p1_bot_asif = t.test(data$p1_bot_asif[which(data$study==1)], mu = 50, alternative = "greater")
twitter_bot_asif = t.test(data$twitter_bot_asif[which(data$study==1)], mu = 50, alternative = "greater")
wiki_bots_asif = t.test(data$wiki_bots_asif[which(data$study==1)], mu = 50, alternative = "greater")

effp1 = data[which(data$study==1),] %>% cohens_d(p1_bot_asif ~ 1, mu = 50)
efftwitter = data[which(data$study==1),] %>% cohens_d(twitter_bot_asif ~ 1, mu = 50)
effwiki = data[which(data$study==1),] %>% cohens_d(wiki_bots_asif ~ 1, mu = 50)

cat(paste0("Study 1: t-tests \n",
           "In our paradigm: ", ttest_extract(p1_bot_asif, effp1), "\n",
           "In Twitter: ", ttest_extract(twitter_bot_asif, efftwitter), "\n",
           "In Wikipedia: ",ttest_extract(wiki_bots_asif, effwiki), "\n"))

# Bots' preferences for different currencies
cat("Study 1: bots' behave as if they have preference for currencies \n", 
    "Money: \n",
    stats(data$p1_bot_asif[which(data$study==1)]),
    "Likes: \n",
    stats(data$twitter_bot_asif[which(data$study==1)]),
    "Bans: \n",
    stats(data$wiki_bots_asif[which(data$study==1)]))

p1_bot_wants = t.test(data$p1_bot_wants[which(data$study==1)], mu = 50, alternative = "greater")
twitter_bot_wants = t.test(data$twitter_bot_wants[which(data$study==1)], mu = 50, alternative = "greater")
wiki_bot_wants = t.test(data$wiki_bot_wants[which(data$study==1)], mu = 50, alternative = "greater")

effp1 = data[which(data$study==1),] %>% cohens_d(p1_bot_wants ~ 1, mu = 50)
efftwitter = data[which(data$study==1),] %>% cohens_d(twitter_bot_wants ~ 1, mu = 50)
effwiki = data[which(data$study==1),] %>% cohens_d(wiki_bot_wants ~ 1, mu = 50)

cat(paste0("Study 1: t-tests \n",
           "In our paradigm: ", ttest_extract(p1_bot_wants, effp1), "\n",
           "In Twitter: ", ttest_extract(twitter_bot_wants, efftwitter), "\n",
           "In Wikipedia: ",ttest_extract(wiki_bot_wants, effwiki), "\n"))

# Bots' preferences for different currencies
cat("Study 1: bots' behave as if they have preference for currencies \n", 
    "Money: \n",
    stats(data$p1_bot_wants[which(data$study==1)]),
    "Likes: \n",
    stats(data$twitter_bot_wants[which(data$study==1)]),
    "Bans: \n",
    stats(data$wiki_bot_wants[which(data$study==1)]))



# Bots gain less trust than people by helping and punishing

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

# Hypotheses

# B1: Do people share with bot Beneficiaries as much as they share with humans?

nobot = data$P1S1[which(data$study==2 & data$scenario=="no_bot" & data$is_player=="1")]
p2bot = data$P1S1[which(data$study==2 & data$scenario=="p2bot" & data$is_player=="1")]

test_res = t.test(nobot,
                  p2bot, alternative = "two.sided") 


vals = c(nobot, p2bot)
keys = c(rep("nobot", length(nobot)), 
         rep("p2bot", length(p2bot)))
df = data.frame(vals, keys)
effsize = cohens_d(df, vals~keys, var.equal = FALSE)

cat(paste0("Hypothesis B1 \n",
           "No Bot average: ",round(mean(nobot)*100), "%", "\n",
           "P2 Bot average: ",round(mean(p2bot)*100), "%", "\n",
           "test details: ",ttest_extract(test_res, effsize), "\n",
           "Number of people: ", length(nobot) + length(p2bot)))

# B2: When people share with bot Beneficiaries, do they receive the same 
#     trust-gain as when they share with humans?

nobot = data$P4H_trust_gain[which(data$study==2 & data$is_player=="4(H)" & data$scenario=="no_bot")]
p2bot = data$P4H_trust_gain[which(data$study==2 & data$is_player=="4(H)" & data$scenario=="p2bot")]

test_res = t.test(nobot,
                  p2bot, alternative = "two.sided",paired = FALSE) 
vals = c(nobot, p2bot)
keys = c(rep("nobot", length(nobot)), 
         rep("p2bot", length(p2bot)))
df = data.frame(vals, keys)
effsize = cohens_d(df, vals~keys, var.equal = FALSE)

cat(paste0("Hypothesis B2 \n",
           "No Bot average: ",round(mean(nobot)), "%", "\n",
           "P2 Bot average: ",round(mean(p2bot)), "%", "\n",
           "test details: ",ttest_extract(test_res, effsize), "\n",
           "Number of people: ", length(nobot) + length(p2bot)))

nobot = data$P4HS1_No[which(data$study==2 & data$is_player=="4(H)" & data$scenario=="no_bot")]
p2bot = data$P4HS1_No[which(data$study==2 & data$is_player=="4(H)" & data$scenario=="p2bot")]
test_res = t.test(nobot,
                  p2bot, alternative = "two.sided") 

vals = c(nobot, p2bot)
keys = c(rep("nobot", length(nobot)), 
         rep("p2bot", length(p2bot)))
df = data.frame(vals, keys)
effsize = cohens_d(df, vals~keys, var.equal = FALSE)

cat(paste0("Leniency towards people who didn't share with bots vs. didn't share with people \n",
           "No Bot average: ",round(mean(nobot)), "%", "\n",
           "P2 Bot average: ",round(mean(p2bot)), "%", "\n",
           "test details: ",ttest_extract(test_res, effsize), "\n"))

nobot = data$P4HS1_Yes[which(data$study==2 & data$is_player=="4(H)" & data$scenario=="no_bot")]
p2bot = data$P4HS1_Yes[which(data$study==2 & data$is_player=="4(H)" & data$scenario=="p2bot")]
test_res = t.test(nobot,
                  p2bot, alternative = "two.sided") 
vals = c(nobot, p2bot)
keys = c(rep("nobot", length(nobot)), 
         rep("p2bot", length(p2bot)))
df = data.frame(vals, keys)
effsize = cohens_d(df, vals~keys, var.equal = FALSE)

cat(paste0("trust when people who share with bots vs. share with people \n",
           "No Bot average: ",round(mean(nobot)), "%", "\n",
           "P2 Bot average: ",round(mean(p2bot)), "%", "\n",
           "test details: ",ttest_extract(test_res, effsize), "\n",
           "Number of people: ", length(nobot) + length(p2bot)))

# B3: When people do not share with bot Beneficiaries, are they punished 
#     to the same extent as when they do not share with humans?

nobot = data$P3S1[which(data$study==2 & data$scenario=="no_bot" & data$is_player=="3")]
p2bot = data$P3S1[which(data$study==2 & data$scenario=="p2bot" & data$is_player=="3")]

test_res = t.test(nobot,
                  p2bot, alternative = "two.sided") 

vals = c(nobot, p2bot)
keys = c(rep("nobot", length(nobot)), 
         rep("p2bot", length(p2bot)))
df = data.frame(vals, keys)
effsize = cohens_d(df, vals~keys, var.equal = FALSE)

cat(paste0("Hypothesis B3 \n",
           "No Bot average: ",round(mean(nobot)*100), "%", "\n",
           "P2 Bot average: ",round(mean(p2bot)*100), "%", "\n",
           "test details: ",ttest_extract(test_res, effsize), "\n",
           "Number of people: ", length(nobot) + length(p2bot)))


# B4: When people punish human Helpers who do not share with bot Beneficiaries, 
#     do they receive the same trust-gain as when they punish human Helpers who 
#     do not share with human Beneficiaries?


nobot = data$P4P_trust_gain[which(data$study==2 & data$is_player=="4(P)" & data$scenario=="no_bot")]
p2bot = data$P4P_trust_gain[which(data$study==2 & data$is_player=="4(P)" & data$scenario=="p2bot")]

test_res = t.test(nobot,
                  p2bot, alternative = "two.sided") 

vals = c(nobot, p2bot)
keys = c(rep("nobot", length(nobot)), 
         rep("p2bot", length(p2bot)))
df = data.frame(vals, keys)
effsize = cohens_d(df, vals~keys, var.equal = FALSE)


cat(paste0("Hypothesis B4 \n",
           "No Bot average: ",round(mean(nobot)), "%", "\n",
           "P2 Bot average: ",round(mean(p2bot)), "%", "\n",
           "test details: ",ttest_extract(test_res, effsize), "\n",
           "Number of people: ", length(nobot) + length(p2bot)))



# qualitative responses
code_vars = names(data)[grepl("P1_f", names(data))]

df = data[which(data$study==2 & data$is_player=="1"),c("uniqueId", code_vars, "scenario")]

df[code_vars] <- sapply(df[code_vars],as.character)

answers <- melt(setDT(subset(df, select=-c(scenario))), id.vars = c("uniqueId") , value.name = "answer", na.rm=T)
answers = subset(answers, select=c("uniqueId", "answer"))
answers = dummy_cols(answers, select_columns = 'answer')
answers = answers %>% 
  dplyr::group_by(uniqueId) %>%
  summarise_at(vars(answer_1:answer_12), sum) %>%
  ungroup

answers_helper = dplyr::left_join(answers, df[c("uniqueId", "scenario")])

trustor_impress = answers_helper$answer_3 + answers_helper$answer_3a + answers_helper$answer_3b 
trustor_impress = ifelse(trustor_impress==0, 0, 1)

mcnemar.test(table(trustor_impress,answers_helper$scenario))

as.numeric(round(prop.table(table(trustor_impress,answers_helper$scenario),2)[2,]*100))


as.numeric(round(prop.table(table(answers_helper$answer_5, answers_helper$scenario),2),2)[2,]*100,1)
mcnemar.test(table(answers_helper$answer_5,answers_helper$scenario))

# percentage of participants that mentioned higher-level principles (morality/ethics)
as.numeric(round(prop.table(table(answers_helper$answer_4, answers_helper$scenario),2),2)[2,]*100,1)


# H1: When bot Helpers share with human Beneficiaries, do they receive the same 
#     trust-gain as humans who share with human Beneficiaries?

nobot = data$P4H_trust_gain[which(data$study==2 & data$is_player=="4(H)" & data$scenario=="no_bot")]
p1bot = data$P4H_trust_gain[which(data$study==2 & data$is_player=="4(H)" & data$scenario=="p1bot")]

test_res = t.test(nobot,
                  p1bot, alternative = "two.sided") 
vals = c(nobot, p1bot)
keys = c(rep("nobot", length(nobot)), 
         rep("p1bot", length(p1bot)))
df = data.frame(vals, keys)
effsize = cohens_d(df, vals~keys, var.equal = FALSE)

cat(paste0("Hypothesis H1 \n",
           "No Bot average: ",round(mean(nobot)), "%", "\n",
           "P1 Bot average: ",round(mean(p1bot)), "%", "\n",
           "test details: ", ttest_extract(test_res, effsize), "\n",
           "Number of people: ", length(nobot) + length(p1bot)))


# H2: When bot Helpers do not share with human Beneficiaries, are they punished 
#     to the same extent as humans in the same situation?

nobot = data$P3S1[which(data$study==2 & data$is_player=="3" & data$scenario=="no_bot")]
p1bot = data$P3S1[which(data$study==2 & data$is_player=="3" & data$scenario=="p1bot")]

test_res = t.test(nobot,
                  p1bot, alternative = "two.sided") 
vals = c(nobot, p1bot)
keys = c(rep("nobot", length(nobot)), 
         rep("p1bot", length(p1bot)))
df = data.frame(vals, keys)
effsize = cohens_d(df, vals~keys, var.equal = FALSE)

cat(paste0("Hypothesis H2 \n",
           "No Bot average: ",round(mean(nobot)*100), "%", "\n",
           "P2 Bot average: ",round(mean(p1bot)*100), "%", "\n",
           "test details: ",ttest_extract(test_res, effsize), "\n",
           "Number of people: ", length(nobot) + length(p1bot)))

# H3: When people punish bot Helpers who do not share, do they receive the same 
#     trust-gain as when they punish human Helpers who do not share?

nobot = data$P4P_trust_gain[which(data$study==2 & data$is_player=="4(P)" & data$scenario=="no_bot")]
p1bot = data$P4P_trust_gain[which(data$study==2 & data$is_player=="4(P)" & data$scenario=="p1bot")]

test_res = t.test(nobot,
                  p1bot, alternative = "two.sided") 

vals = c(nobot, p1bot)
keys = c(rep("nobot", length(nobot)), 
         rep("p1bot", length(p1bot)))
df = data.frame(vals, keys)
effsize = cohens_d(df, vals~keys, var.equal = FALSE)

cat(paste0("Hypothesis H3 \n",
           "No Bot average: ",round(mean(nobot)), "%", "\n",
           "P1 Bot average: ",round(mean(p1bot)), "%", "\n",
           "test details: ",ttest_extract(test_res, effsize), "\n",
           "Number of people: ", length(nobot) + length(p1bot)))


# P1: When bot Punishers punish human Helpers who do not share, do they receive
#     the same trust-gain as humans who punish humans who do not share?

nobot = data$P4P_trust_gain[which(data$study==2 & data$is_player=="4(P)" & data$scenario=="no_bot")]
p3bot = data$P4P_trust_gain[which(data$study==2 & data$is_player=="4(P)" & data$scenario=="p3bot")]

test_res = t.test(nobot,
                  p3bot, alternative = "two.sided") 

vals = c(nobot, p3bot)
keys = c(rep("nobot", length(nobot)), 
         rep("p3bot", length(p3bot)))
df = data.frame(vals, keys)
effsize = cohens_d(df, vals~keys, var.equal = FALSE)

cat(paste0("Hypothesis P1 \n",
           "No Bot average: ",round(mean(nobot)), "%", "\n",
           "P2 Bot average: ",round(mean(p3bot)), "%", "\n",
           "test details: ",ttest_extract(test_res, effsize), "\n",
           "Number of people: ", length(nobot) + length(p3bot)))



# qualitative analysis of Trustor justifications
code_vars = names(data)[grepl("P4Hfollowup_code\\d", names(data))]

df = data[which(data$study==2 & data$is_player=="4(H)"),c("uniqueId", code_vars, "scenario")]
df[code_vars] <- sapply(df[code_vars],as.character)

answers <- melt(setDT(subset(df, select=-c(scenario))), id.vars = c("uniqueId") , value.name = "answer", na.rm=T)
answers = subset(answers, select=c("uniqueId", "answer"))
answers = dummy_cols(answers, select_columns = 'answer')
answers = answers %>% 
  dplyr::group_by(uniqueId) %>%
  summarise_at(vars(answer_1:answer_12), sum) %>%
  ungroup

answers = dplyr::left_join(answers, df[c("uniqueId", "scenario")])
answers_p4 = answers
mcnemar.test(table(answers$answer_4,answers$scenario)[,1:2])
round(prop.table(table(answers$answer_4,answers$scenario)[,1:2], 2), 2)
round(prop.table(table(answers$answer_1,answers$scenario)[,1:2], 2), 2)
mcnemar.test(table(answers$answer_1,answers$scenario)[,1:2])



# Trust gains depend on perceived helping and punishing norms

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

controls_eff_model = summary(model_cont_self_help_all)
effect_controls = controls_eff_model$coef["guess_norm_all", c("Estimate", "Pr(>|t|)")]
effect_controls_ci = round(confint(model_cont_self_punish_all)["guess_norm_all",], 3)
n_obs_controls = dim(data[which(data$is_player == "4(P)"  & data$study==3),])[1]

Table2 =  paste0("Table 2 \n",
                 "-------------------", "\n",
                 "Main effect \n",
                 "Coefficient: ", round(main_effect["Estimate"],3), "\n",
                 "p-value: ", pvalue_round(main_effect["Pr(>|t|)"]), "\n",
                 "95% CI: (", round(main_effect_ci["2.5 %"], 3), ", ", round(main_effect_ci["97.5 %"], 3), ") \n",
                 "N: ", n_obs_main, "\n",
                 "Adj. R squared: ", round(main_eff_model$adj.r.squared, 3) ,"\n", 
                 "-------------------", "\n",
                 "With controls", "\n",
                 "Coefficient: ", round(effect_controls["Estimate"],3), "\n",
                 "p-value: ", pvalue_round(effect_controls["Pr(>|t|)"]), "\n",
                 "95% CI: (", round(effect_controls_ci["2.5 %"], 3), ", ", round(effect_controls_ci["97.5 %"], 3), ") \n",
                 "N: ", n_obs_controls, "\n",
                 "Adj. R squared: ", round(controls_eff_model$adj.r.squared, 3)
)

cat(Table1)
cat(Table2)


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

# Hypothesis and comparisons

hb4 = data[which(data$study==4 & data$info_true=="Yes"),c("uniqueId", "P4H_trust_gain", "scenario")]
hb2 = data[which(data$study==2),c("uniqueId", "P4H_trust_gain")]
colnames(hb4)[2] = "P4H_trust_gain_s4"
colnames(hb2)[2] = "P4H_trust_gain_s2"

hb = left_join(hb4, hb2)
test_res = t.test(hb$P4H_trust_gain_s4[which(hb$scenario=='p1bot')], 
                  hb$P4H_trust_gain_s2[which(hb$scenario=='p1bot')], alternative = 'two.sided', paired = T)

s4 = hb$P4H_trust_gain_s4[which(hb$scenario=='p1bot')]
s2 = hb$P4H_trust_gain_s2[which(hb$scenario=='p1bot')]
vals = c(s4, s2)
keys = c(rep("s4", length(s4)), 
         rep("s2", length(s2)))
df = data.frame(vals, keys)
effsize = cohens_d(df, vals~keys, var.equal = FALSE)

cat(paste0("With norm manipulation: ",round(mean(hb$P4H_trust_gain_s4[which(hb$scenario=='p1bot')])), "%", "\n",
           "Without norm manipulation: ",round(mean(hb$P4H_trust_gain_s2[which(hb$scenario=='p1bot')])), "%", "\n",
           "test details: ",ttest_extract(test_res, effsize)))

# Manipulating norm-consensus of helping may boost trust-gain

# Study 5
with_norm = data$P4H_trust_gain[which(data$study==5 & data$is_player=="4(H)" & data$info_true =="Yes" & data$scenario=="p2bot" & data$with_norm==1)]
without_norm = data$P4H_trust_gain[which(data$study==5 & data$is_player=="4(H)" & data$scenario=="p2bot" & data$with_norm==0)]
test_res = t.test(with_norm, 
                  without_norm, alternative = 'two.sided')

vals = c(with_norm, without_norm)
keys = c(rep("with_norm", length(with_norm)), 
         rep("without_norm", length(without_norm)))
df = data.frame(vals, keys)
effsize = cohens_d(df, vals~keys, var.equal = FALSE)

cat(paste0("Study 4: ",round(mean(with_norm)), "%", "\n",
           "Study 2: ",round(mean(without_norm)), "%", "\n",
           "test details: ",ttest_extract(test_res, effsize)))




# some qualitative data stats

p1 = sum(data$P1_followup_code1!="", na.rm=T) 
p4 = sum(data$P4Hfollowup_codes1!="", na.rm=T) 
n_responses = p1 + p4 
# only 1 person was confused
sum(answers_helper$answer_8)
# percentage of people that appeared confused
perc_confused = ((sum(answers_helper$answer_9==1) + sum(answers_p4$answer_11==1))/n_responses) 


###### Methods ###### 

# convert date to date object

s4 = data[which(data$study==4), c("uniqueId", "date")]
s2 = data[which(data$study==2), c("uniqueId", "date")]

# number of days passed between Study 2 and Study 4
colnames(s4)[2] = "date_s4"
colnames(s2)[2] = "date_s2"
hb = left_join(s4, s2)
hb$days_passed = hb$date_s4 - hb$date_s2
avg_days = round(mean(hb$days_passed),1)
min_days = min(hb$days_passed)

cat(
  paste0("Days passed between Study 2 and Study 4 \n",
         "Average: ", avg_days, "\n",
         "Minimum: ", min_days)
)

# manipulation belief in Study 5 vs Study 4

round(prop.table(table(data$info_true[which(data$study==4)])),2)
round(prop.table(table(data$info_true[which(data$study==5 & data$with_norm==1)])),2)

# Study 5 new participants who have never seen any version of the study
sum(data$uniqueId[which(data$study==5)] %in% data$uniqueId[which(data$study!=5)])

prevIds = unique(data$uniqueId[which(data$study!=5)])


s5 = data[which(data$study==5 & data$uniqueId %in% prevIds),c("uniqueId", "date")]
s14 = data[which((data$uniqueId %in% s5$uniqueId) & (data$study!=5)), c("uniqueId", "date", "study")]


colnames(s5)[2] = "date_s5"
colnames(s14)[2] = "date_s1_4"
hb = dplyr::right_join(s5,s14, by="uniqueId")
hb$days_passed = hb$date_s5 - hb$date_s1_4
avg_days = round(mean(hb$days_passed),1)
min_days = min(hb$days_passed)

cat(
  paste0("Days passed between Study 2 and Study 4 \n",
         "Average: ", avg_days, "\n",
         "Minimum: ", min_days)
)

###### Recruitment ###### 

# breakdown of participants in each study

table(data$study)

# Study 4 returned

round(sum(data$study==4)/sum(data$study==2 & data$is_player=="4(H)")*100, 2)

# Study 5 returned
s5_returned = ifelse(data$uniqueId[which(data$study==5)] %in% data$uniqueId[which(data$study!=5)], 1, 0)

table(s5_returned)

##### Inclusion criteria and data quality ##### 

# Study 4 norm-consensus acceptance across treatments

round(prop.table(table(data$scenario[which(data$study==4)], data$info_true[which(data$study==4)]),1)*100)

belief = data$P4H_trust_gain[which(data$study==4 & data$info_true=="Yes" & data$scenario=="p2bot")]
non_belief = data$P4H_trust_gain[which(data$study==4 & data$info_true=="No" & data$scenario=="p2bot")]

test_res = t.test(belief, 
                  non_belief)
vals = c(belief, non_belief)
keys = c(rep("belief", length(belief)), 
         rep("non_belief", length(non_belief)))
df = data.frame(vals, keys)
effsize = cohens_d(df, vals~keys, var.equal = FALSE)


cat(paste0(" \n",
           "No Bot average: ",round(mean(belief)), "%", "\n",
           "P2 Bot average: ",round(mean(non_belief)), "%", "\n",
           "test details: ",ttest_extract(test_res, effsize)))

# Study 5 norm-consensus acceptance across treatments
round(prop.table(table(data$info_true[which(data$study==5 & data$with_norm==1)])),2)
round(prop.table(table(data$scenario[which(data$study==5 & data$with_norm==1)], data$info_true[which(data$study==5 & data$with_norm==1)]),1)*100)


###### Sample composition and compensation ###### 
data$gender = ifelse(data$gender=="Male", 1, 0)
data$non_Hispanic_White = ifelse(data$ethnics=="White", 1, 0)
data %>%
  group_by(study) %>%
  dplyr::summarize(avg_age = mean(age),
                   sd_age = sd(age),
                   gender = round(mean(gender)*100),
                   non_Hispanic_White = round(mean(non_Hispanic_White)*100))


# in Study 3, number of people with the same level of comprehension as in Study 2
sum(data$comprehension_num_correct[which(data$study==3)]>=6)

# Study 5 people who returned, saw the following studies
s5_ppl = data$uniqueId[which(data$study==5)]
others = data[which(data$study!=5), c("uniqueId", "study")]

returned = others[others$uniqueId %in% s5_ppl, ]
dim(returned)
table(returned$study) # 484 people in Study 2 includes people from study 4 as well

# time of completion & compensation

avg_time = data %>%
  dplyr::group_by(study) %>%
  dplyr::summarise(avg_mins = round(mean(duration)/60,1))
avg_time

avg_earn = data %>%
  dplyr::group_by(study) %>%
  dplyr::summarise(avg_earn = round(mean(earned, na.rm=T),2))

avg_time$earn = avg_earn$avg_earn
avg_time$avg_hourly = (60 * avg_time$earn)/avg_time$avg_mins 
avg_time

###### Robustness checks ###### 

# Study 2
hb = subset(data, study==2)
# didn't answer all comprehension questions correctly
sum(hb$comprehension_num_correct!=8)
# failed manipulation check
table(hb$treatment_takeup)

# Study 3 
hb = subset(data, study==3)
# didn't answer all comprehension questions correctly
sum(hb$comprehension_num_correct!=8)
# failed manipulation check
table(hb$treatment_takeup)


# Study 4
hb = subset(data, study==4)
hb2 = subset(data, study==2)
# failed manipulation check in either Study 2 or Study 4
table(hb$treatment_takeup)
hb = hb[c("uniqueId", "treatment_takeup")]
names(hb)[2] = "treatment_takeup_s4"
hb = dplyr::left_join(hb, hb2[c("uniqueId", "treatment_takeup")])
sum((hb$treatment_takeup_s4 + hb$treatment_takeup) < 2)

round((sum((hb$treatment_takeup_s4 + hb$treatment_takeup) < 2)/458)*100,2)
hb = subset(data, study==4)
table(hb$info_true)
round(prop.table(table(hb$info_true))*100, 2)

# Study 5
hb = subset(data, study==5)
# failed manipulation check 
table(hb$treatment_takeup)
round((prop.table(table(hb$treatment_takeup)))*100,2)
# didn't believe the norm consensus manipulation
table(hb$info_true[which(hb$with_norm==1)])

round(prop.table(table(hb$info_true))*100, 2)


# Attrition rates
# reread the data to get partial responses as well
data = read.csv("HB_data.csv", stringsAsFactors = F)

# Study 2
round(prop.table(table(data$comprehension_passed[which(data$study==2)], 
                       data$finished[which(data$study==2)]),1)*100, 2)
# Study 3
round(prop.table(table(data$comprehension_passed[which(data$study==3)], 
                       data$finished[which(data$study==3)]),1)*100, 2)
# Study 4
round(prop.table(table(data$comprehension_passed[which(data$study==4)], 
                       data$finished[which(data$study==4)]),1)*100, 2)
# Study 5
round(prop.table(table(data$comprehension_passed[which(data$study==5)], 
                       data$finished[which(data$study==5)]),1)*100, 2)




### --- OUR STUFF____

agdata<-subset(data, is.na(data$age)=='FALSE')

sum(is.na(agdata$age))

mean(agdata$age)

sd(agdata$age)

