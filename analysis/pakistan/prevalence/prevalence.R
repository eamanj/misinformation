library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(MASS)

MSM_PREV_VARS <- c("msm_prevalence1", "msm_prevalence2", "msm_prevalence3")
MISINFO_PREV_VARS <- c("misinfo_prevalence1", "misinfo_prevalence2", "misinfo_prevalence3", "misinfo_prevalence4", "misinfo_prevalence5")
PLACEBO_PREV_VARS <- c("placebo_prevalence1", "placebo_prevalence2")
PREVALENCE_COLS <- paste0("LOOPCLAIM_PREV_CLAIM_BELIEF_", seq(116), "_QCLAIM_PREV")
BELIEF_COLS <- paste0("LOOPCLAIM_PREV_CLAIM_BELIEF_", seq(116), "_QCLAIM_BELIEF")
PLATFORM_BASE_COLS <- paste0("LOOPCLAIM_PREV_CLAIM_BELIEF_", seq(116), "_QCLAIM_PLAT_")
PLATFORM_COLS <- paste0(rep(PLATFORM_BASE_COLS, each = 17), c(seq(15), 98, 99))
PREVALENCE_BELIEF_COLS <- paste0("claim_prevalence_belief", seq(116))

setwd("~/research/misinformation/")
source("./analysis/pakistan/debunk/clean_data.R")
source("./analysis/debunk_utils.R")
data <- process_data("./data/pakistan/debunk/Pakistan results – wave one - answers displayed as text.xlsx")

# add prevelance-belief-platform columns
for(i in seq(116)) {
  new_col <- PREVALENCE_BELIEF_COLS[i]
  data[,new_col] <- paste0(data[[PREVALENCE_COLS[i]]], "_", data[[BELIEF_COLS[i]]])
}

# which filtering to use?
df <- exclude_nonattentive(data,
                           no_treatment=FALSE,
                           real_tie=TRUE,
                           search_internet=TRUE,
                           attention_check=FALSE,
                           correct_misinfo=FALSE,
                           correct_correction=FALSE,
                           long_duration=FALSE)

print_summary_table <- function(polr_fit) {
  ctable <- coef(summary(polr_fit))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- cbind(ctable, "p value" = p)
  return(round(ctable, 4))
}

# first of all make sure each row has exactly 3 msms, discard rows that don't satisfy this
MSM_VARS <- c(paste0('MRK_EXAMPLE2_', seq(11, 20)), paste0('MRK_EXAMPLE3_', seq(21, 30)),
              paste0('MRK_EXAMPLE7_', seq(31, 40)), paste0('MRK_EXAMPLE10_', seq(41, 50)),
              paste0('MRK_EXAMPLE12_', seq(51, 60)), paste0('MRK_EXAMPLE14_', seq(61, 70)),
              paste0('MRK_EXAMPLE17_', seq(71, 80)), paste0('MRK_EXAMPLE18_', seq(81, 90)),
              paste0('MRK_EXAMPLE20_', seq(91, 100)), paste0('MRK_EXAMPLE26_', seq(101, 110)))
claims <- apply(df[,MSM_VARS] == "Yes", 1, function(x) list(names(x)[which(x == TRUE)]))
df <- df[which(rapply(claims, length) == 3),]
  




# Function returns a dataframe with one row per individual subject. The dataframe has information on which
# claims were shown to the subject and how the subject answered them.
generate_shown_claims_df <- function(df) {
  # MRK_DEBUNKED_X indicates whether debunk claim X was selected and shown to subject
  # MRK_PLACEBOS_X indicates whether placebo claim X was selected and shown to subject
  # QUOTA_FINAL_EXAMPLE_X indicates whether mainstream claim X was selected and shown to subject
  # X for debunk claims range from 1 to 10, for placebos range from 111 to 116, and
  # from 11 to 110 to mainstream claims.
  # MRK_EXAMPLESY_X are similar to QUOTA_FINAL_EXAMPLE_X. The number X in both is exactly
  # the same, if X in MRK_EXPAMPLES is Yes, it will be yes in QUOTA_FINAL_EXAMPLE too.
  # so X in both refers to the same claim number. Y in MRK_EXAMPLESY refer to the number of
  # main stream family. Each debunk claim has a corresponding family of 10 claims. The family
  # corresponding to a single debunk claim is numbered as Y. Only one in each family
  # is randomly selected and marked as "Yes"
  debunk_vars <- paste0('MRK_DEBUNKED_', seq(1, 10))
  placebo_vars <- paste0('MRK_PLACEBOS_', seq(111, 116))
  msm_vars1 <- MSM_VARS
  msm_vars2 <- paste0('QUOTA_FINAL_EXAMPLE_', seq(11, 110))
  
  # get the ID (column name) of claims that were shown (coded as Yes) to each row
  claims <- as.data.frame(t(apply(df[,msm_vars1] == "Yes", 1, function(x) names(x)[which(x == TRUE)])))
  colnames(claims) <- c("MARK_MSM1", "MARK_MSM2", "MARK_MSM3")
  claims[,c("FINAL_MSM1", "FINAL_MSM2", "FINAL_MSM3")] <- t(
    apply(df[,msm_vars2] == "Yes", 1, function(x) names(x)[which(x == TRUE)]))
  claims[,c("MISINFO1", "MISINFO2", "MISINFO3", "MISINFO4", "MISINFO5")] <- t(
    apply(df[,debunk_vars] == "Yes", 1, function(x) names(x)[which(x == TRUE)]))
  claims[,c("PLACEBO1", "PLACEBO2")] <- t(
    apply(df[,placebo_vars] == "Yes", 1, function(x) names(x)[which(x == TRUE)]))
  
  # now extract the claim numbers
  claims$MSM_CODE1 <- gsub("QUOTA_FINAL_EXAMPLE_", "", claims$FINAL_MSM1)
  claims$MSM_CODE2 <- gsub("QUOTA_FINAL_EXAMPLE_", "", claims$FINAL_MSM2)
  claims$MSM_CODE3 <- gsub("QUOTA_FINAL_EXAMPLE_", "", claims$FINAL_MSM3)
  claims$MISINFO_CODE1 <- gsub("MRK_DEBUNKED_", "", claims$MISINFO1)
  claims$MISINFO_CODE2 <- gsub("MRK_DEBUNKED_", "", claims$MISINFO2)
  claims$MISINFO_CODE3 <- gsub("MRK_DEBUNKED_", "", claims$MISINFO3)
  claims$MISINFO_CODE4 <- gsub("MRK_DEBUNKED_", "", claims$MISINFO4)
  claims$MISINFO_CODE5 <- gsub("MRK_DEBUNKED_", "", claims$MISINFO5)
  claims$PLACEBO_CODE1 <- gsub("MRK_PLACEBOS_", "", claims$PLACEBO1)
  claims$PLACEBO_CODE2 <- gsub("MRK_PLACEBOS_", "", claims$PLACEBO2)
  
  # Add demographic variables
  claims$resp_gender <- df$resp_gender
  claims$resp_age <- df$resp_age
  claims$resp_age_group <- ifelse(claims$resp_age < 25, "<25",
                                  ifelse(claims$resp_age < 40, "25-40", ">40"))
  claims$age_group <- factor(claims$resp_age_group, levels=c("<25", "25-40", ">40"), ordered=TRUE)
  claims$income <- df$QHOUSE_INC
  claims$income_numeric <- df$income_numeric
  claims$education <- df$QEDUCATION_CHIEF
  claims$education_numeric <- df$education_numeric
  claims$religion <- df$QRELIGION
  claims$pml_oppose <- df$pml_tweet
  claims$ppp_oppose <- df$ppp_tweet
  claims$pti_oppose <- df$pti_tweet
  claims$mim_discuss_news <- df$QMIM_DISCUSS_NEWS
  claims$mim_discuss_news_binary <- ifelse(df$QMIM_DISCUSS_NEWS > "Sometimes", "High", "Low")
  claims$politics_attention <- df$QPOLI_ATT
  claims$politics_interest <- df$QPOLI_INTEREST
  claims$follow_news <- df$QACC_NEWS
  
  return(claims)
}

# this function converts the claims dataframe generated by generate_claims function above, to a long format
# where column names corresponding to prevalence, belief or platform of each claim become rows. gather_cols
# determine which columns to use for pivoting the dataframe to long format
gather_claims_by_columns <- function(df, claims, gather_cols, gather_col_name) {
  # convert to long format and add columns on claim type and the index of claim
  # shown to subject
  # copy gather column answers to claims df, so we can extract the non-NA answers to the random claims shown
  claims[,gather_cols] <- df[,gather_cols]
  
  claim_code_vars <- c("PLACEBO_CODE1", "PLACEBO_CODE2",
                       "MISINFO_CODE1", "MISINFO_CODE2", "MISINFO_CODE3", "MISINFO_CODE4", "MISINFO_CODE5",
                       "MSM_CODE1", "MSM_CODE2", "MSM_CODE3")
  claims <- claims %>%
    gather_(key="claim", value=gather_col_name, gather_cols=gather_cols)
  claims <- claims[!is.na(claims[,gather_col_name]),]
  claims <- claims[claims[,gather_col_name] != "NA_NA",]
  claims <- claims[claims[,gather_col_name] != "NA_NA_NA",]
  claims$claim_code <- as.numeric(gsub(".*?([0-9]+).*", "\\1", claims$claim))
  claims$claim_type <- ifelse(claims$claim_code < 11, "Misinfo",
                              ifelse(claims$claim_code < 111, "MSM", "Placebo"))
  claims$claim_type_index <-
    apply(claims[,claim_code_vars] == claims$claim_code, 1, function(x) names(x)[which(x == TRUE)])
  
  # remove unnecessary columns
  cols_drop <- c(paste0("MARK_MSM", seq(1,3)),
                 paste0("FINAL_MSM", seq(1,3)),
                 paste0("MSM_CODE", seq(1,3)),
                 paste0("MISINFO", seq(1,5)),
                 paste0("MISINFO_CODE", seq(1,5)),
                 paste0("PLACEBO", seq(1,2)),
                 paste0("PLACEBO_CODE", seq(1,2)))
  claims[,cols_drop] <- NULL
  
  return(claims)
}


####################################################################################
# PREVALENCE
# evalute prevalence of misinfo claims versus mainstream and placebo claims
claims <- generate_shown_claims_df(df)
claims <- gather_claims_by_columns(df, claims, PREVALENCE_COLS, "prevalence")
claims$prevalence <- factor(claims$prevalence, 
                            levels=c("No", "Maybe", "Yes"), ordered=TRUE)

grouping_vars <- c()
summary_stats <- claims %>%
  group_by_at(c(grouping_vars, "claim_type_index")) %>%
  mutate(group_count=n()) %>%
  group_by_at(c(grouping_vars, "claim_type_index", "prevalence")) %>%
  summarise(freq=n()/first(group_count))
if(is.null(grouping_vars)) {
  summary_stats$group <- summary_stats$claim_type_index
} else {
  summary_stats$group <- paste0(summary_stats$claim_type_index, '-', summary_stats[[grouping_vars]])
}
ggplot(summary_stats, aes(x=prevalence, y=freq, fill=group)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Fraction within claim type") +
  xlab("Prevalence (Encountered before)") +
  theme(plot.title = element_text(hjust = 0.5))
  
  

# perform pairwise tests between claim types
msm_misinfo_user_claim_stats <- claims[claims$claim_type %in% c("MSM", "Misinfo"),]
msm_placebo_user_claim_stats <- claims[claims$claim_type %in% c("MSM", "Placebo"),]
misinfo_placebo_user_claim_stats <- claims[claims$claim_type %in% c("Misinfo", "Placebo"),]
test_res <- chisq.test(claims$claim_type, claims$prevalence)
msm_misinfo_test_res <- chisq.test(msm_misinfo_user_claim_stats$claim_type, msm_misinfo_user_claim_stats$prevalence)
msm_placebo_test_res <- chisq.test(msm_placebo_user_claim_stats$claim_type, msm_placebo_user_claim_stats$prevalence)
misinfo_placebo_test_res <- chisq.test(misinfo_placebo_user_claim_stats$claim_type, misinfo_placebo_user_claim_stats$prevalence)

grouping_vars <- c()
summary_stats <- claims %>%
  group_by_at(c(grouping_vars, "claim_type")) %>%
  mutate(group_count=n()) %>%
  group_by_at(c(grouping_vars, "claim_type", "prevalence")) %>%
  summarise(freq=n()/first(group_count))
if(is.null(grouping_vars)) {
  summary_stats$group <- summary_stats$claim_type
} else {
  summary_stats$group <- paste0(summary_stats$claim_type, '-', summary_stats[[grouping_vars]])
}
#summary_stats <- summary_stats[summary_stats$claim_type != "msm",]
summary_stats$percent_freq <- paste0(round(summary_stats$freq*100, 1), '%')
ggplot(summary_stats, aes(x=prevalence, y=freq, fill=group)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Fraction within claim type") +
  xlab("Prevalence (Encountered before)") +
  labs(fill="Claim Type") +
  ggtitle(paste('Pearson Chi-squared p-vals\n',
                'misinfo/msm:', round(msm_misinfo_test_res$p.value, 4),
                'misinfo/placebo:', round(misinfo_placebo_test_res$p.value, 4),
                'msm/placebo:', round(msm_placebo_test_res$p.value, 4))) +
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
ggplot(summary_stats, aes(x=prevalence, y=freq*100, fill=group, label=percent_freq)) +
  geom_bar(stat='identity', position='dodge') +
  geom_text(size=6, position=position_dodge(0.9), hjust=0.5, vjust=-0.25) +
  ylab("% of Respondents") +
  labs(fill="Claim Type") +
  coord_cartesian(ylim=c(0,50.5)) +
  ggtitle('Have you encountered this claim before?') +
  theme(plot.title = element_text(hjust = 0.5, size=27),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size=26),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=26),
        legend.title = element_text(size=26), 
        legend.text = element_text(size=26))
ggsave("./results/pakistan/prevalence/prevalence_by_claim_type.png",  width = 10, height = 6)


grouping_vars <- c()
summary_stats <- claims %>%
  group_by_at(c(grouping_vars, "claim")) %>%
  mutate(group_count=n()) %>%
  group_by_at(c(grouping_vars, "claim", "prevalence")) %>%
  summarise(freq=n()/first(group_count))
summary_stats$claim_num <- readr::parse_number(summary_stats$claim)
summary_stats$claim_type <- ifelse(summary_stats$claim_num < 11, "Misinfo",
                                   ifelse(summary_stats$claim_num < 111, "MSM", "Placebo"))
summary_stats$claim_type <- factor(summary_stats$claim_type, levels=c('Placebo', 'Misinfo', 'MSM'), ordered=TRUE)
if(is.null(grouping_vars)) {
  summary_stats <- summary_stats[order(summary_stats$claim_type, summary_stats$freq),]
} else {
  summary_stats <- summary_stats[order(summary_stats$claim_type, summary_stats[[grouping_vars]], summary_stats$freq),]
}
summary_stats$group <- seq(nrow(summary_stats))
summary_stats$claim_type <- as.character(summary_stats$claim_type)
if(is.null(grouping_vars)) {
  summary_stats$grouping <- summary_stats$claim_type
} else {
  summary_stats$grouping <- paste0(summary_stats$claim_type, '-', summary_stats[[grouping_vars]])
}
ggplot(summary_stats, aes(x=prevalence, y=freq, group=group, fill=grouping)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Fraction within claim type") +
  xlab("Prevalence (Encountered before)") +
  labs(fill="Claim Type") +
  theme(plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18), 
        legend.text = element_text(size=16))
ggsave("./results/pakistan/prevalence/individual_prevalence.png",  width = 10, height = 6)

####################################################################################
# BELIEF
# evalute belief about misinfo claims versus mainstream and placebo claims
claims <- generate_shown_claims_df(df)
claims <- gather_claims_by_columns(df, claims, BELIEF_COLS, "belief")
claims$belief <- factor(claims$belief,
                        levels=c("Definitely accurate", "Probably accurate",
                                 "Not sure if accurate or inaccurate",
                                 "Probably inaccurate", "Definitely inaccurate"),
                        ordered=TRUE)
levels(claims$belief) <- c("Definitely\naccurate", "Probably\naccurate",
                           "Not sure\nif accurate\nor inaccurate",
                           "Probably\ninaccurate", "Definitely\ninaccurate")  
grouping_vars <- c()
summary_stats <- claims %>%
  group_by_at(c(grouping_vars, "claim_type_index")) %>%
  mutate(group_count=n()) %>%
  group_by_at(c(grouping_vars, "claim_type_index", "belief")) %>%
  summarise(freq=n()/first(group_count))
if(is.null(grouping_vars)) {
  summary_stats$group <- summary_stats$claim_type_index
} else {
  summary_stats$group <- paste0(summary_stats$claim_type_index, '-', summary_stats[[grouping_vars]])
}
ggplot(summary_stats, aes(x=belief, y=freq, fill=group)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Fraction within claim type") +
  xlab("Belief (Now or when encountered)") +
  theme(plot.title = element_text(hjust = 0.5))
  
  

# perform pairwise tests between claim types
msm_misinfo_user_claim_stats <- claims[claims$claim_type %in% c("MSM", "Misinfo"),]
msm_placebo_user_claim_stats <- claims[claims$claim_type %in% c("MSM", "Placebo"),]
misinfo_placebo_user_claim_stats <- claims[claims$claim_type %in% c("Misinfo", "Placebo"),]
test_res <- chisq.test(claims$claim_type, claims$belief)
msm_misinfo_test_res <- chisq.test(msm_misinfo_user_claim_stats$claim_type, msm_misinfo_user_claim_stats$belief)
msm_placebo_test_res <- chisq.test(msm_placebo_user_claim_stats$claim_type, msm_placebo_user_claim_stats$belief)
misinfo_placebo_test_res <- chisq.test(misinfo_placebo_user_claim_stats$claim_type, misinfo_placebo_user_claim_stats$belief)

grouping_vars <- c()
summary_stats <- claims %>%
  group_by_at(c(grouping_vars, "claim_type")) %>%
  mutate(group_count=n()) %>%
  group_by_at(c(grouping_vars, "claim_type", "belief")) %>%
  summarise(freq=n()/first(group_count))
if(is.null(grouping_vars)) {
  summary_stats$group <- summary_stats$claim_type
} else {
  summary_stats$group <- paste0(summary_stats$claim_type, '-', summary_stats[[grouping_vars]])
}
ggplot(summary_stats, aes(x=belief, y=freq, fill=group)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Fraction within claim type") +
  xlab("Belief (Now or when encountered)") +
  labs(fill="Claim Type") +
  ggtitle(paste('Pearson Chi-squared p-vals\n',
                'misinfo/msm:', round(msm_misinfo_test_res$p.value, 4),
                'misinfo/placebo:', round(misinfo_placebo_test_res$p.value, 4),
                'msm/placebo:', round(msm_placebo_test_res$p.value, 4))) +
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
ggsave("./results/pakistan/prevalence/belief_by_claim_type.png",  width = 10, height = 6)


belief_df <- summary_stats
belief_df <- belief_df %>%
  mutate(collapsed_belief=case_when(
    belief <= 'Probably\naccurate' ~ "Accurate",
    belief == 'Not sure\nif accurate\nor inaccurate' ~ "Unsure",
    belief <= 'Definitely\ninaccurate' ~ "Inaccurate",
  ))
belief_df$collapsed_belief <- factor(belief_df$collapsed_belief, levels=c('Inaccurate', 'Unsure', 'Accurate'), ordered=T)
belief_df <- belief_df %>% ungroup(claim_type) %>% dplyr::select(-c(claim_type, belief))
belief_df
belief_df <- belief_df %>% 
  group_by(collapsed_belief, group) %>%
  summarise(freq=sum(freq)*100)
belief_df$percent_freq <- paste0(round(belief_df$freq, 1), '%')
ggplot(belief_df, aes(x=collapsed_belief, y=freq, fill=group, label=percent_freq)) +
  geom_bar(stat='identity', position='dodge') +
  geom_text(size=6, position=position_dodge(0.9), hjust=0.5, vjust=-0.25) +
  ylab("% of Respondents") +
  labs(fill="Claim Type") +
  coord_cartesian(ylim=c(0,46)) +
  ggtitle('Did you believe this claim?') +
  theme(plot.title = element_text(hjust = 0.5, size=27),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size=26),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=26),
        legend.title = element_text(size=26), 
        legend.text = element_text(size=26))
ggsave("./results/pakistan/prevalence/collapsed_belief_by_claim_type.png",  width = 10, height = 6)
 
grouping_vars <- c()
summary_stats <- claims %>%
  group_by_at(c(grouping_vars, "claim")) %>%
  mutate(group_count=n()) %>%
  group_by_at(c(grouping_vars, "claim", "belief")) %>%
  summarise(freq=n()/first(group_count))
summary_stats$claim_num <- readr::parse_number(summary_stats$claim)
summary_stats$claim_type <- ifelse(summary_stats$claim_num < 11, "Misinfo",
                                   ifelse(summary_stats$claim_num < 111, "MSM", "Placebo"))
summary_stats$claim_type <- factor(summary_stats$claim_type, levels=c('Placebo', 'Misinfo', 'MSM'), ordered=TRUE)
if(is.null(grouping_vars)) {
  summary_stats <- summary_stats[order(summary_stats$claim_type, summary_stats$freq),]
} else {
  summary_stats <- summary_stats[order(summary_stats$claim_type, summary_stats[[grouping_vars]], summary_stats$freq),]
}
summary_stats$group <- seq(nrow(summary_stats))
summary_stats$claim_type <- as.character(summary_stats$claim_type)
if(is.null(grouping_vars)) {
  summary_stats$grouping <- summary_stats$claim_type
} else {
  summary_stats$grouping <- paste0(summary_stats$claim_type, '-', summary_stats[[grouping_vars]])
}
ggplot(summary_stats, aes(x=belief, y=freq, group=group, fill=grouping)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Fraction within claim type") +
  xlab("Belief (Now or when encountered)") +
  theme(plot.title = element_text(hjust = 0.5, size=22),
        axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
ggsave("./results/pakistan/prevalence/individual_belief.png",  width = 10, height = 6)


####################################################################################
# PLATFORM
# evalute platform about misinfo claims versus mainstream and placebo claims
claims <- generate_shown_claims_df(df)
claims <- gather_claims_by_columns(df, claims, PLATFORM_COLS, "platform")
# remove the platform code from claim var
claims$claim <- gsub("(LOOPCLAIM_PREV_CLAIM_BELIEF_[0-9]+_QCLAIM_PLAT)_[0-9]+", "\\1", claims$claim)

grouping_vars <- c()
summary_stats <- claims %>%
  group_by_at(c(grouping_vars, "claim_type_index")) %>%
  mutate(group_count=n()) %>%
  group_by_at(c(grouping_vars, "claim_type_index", "platform")) %>%
  summarise(freq=n()/first(group_count))
if(is.null(grouping_vars)) {
  summary_stats$group <- summary_stats$claim_type_index
} else {
  summary_stats$group <- paste0(summary_stats$claim_type_index, '-', summary_stats[[grouping_vars]])
}
ggplot(summary_stats, aes(x=platform, y=freq, fill=group)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Fraction within claim type") +
  xlab("Platform encountered") +
  theme(plot.title = element_text(hjust = 0.5))
  
  

# perform pairwise tests between claim types
msm_misinfo_user_claim_stats <- claims[claims$claim_type %in% c("MSM", "Misinfo"),]
msm_placebo_user_claim_stats <- claims[claims$claim_type %in% c("MSM", "Placebo"),]
misinfo_placebo_user_claim_stats <- claims[claims$claim_type %in% c("Misinfo", "Placebo"),]
test_res <- chisq.test(claims$claim_type, claims$platform)
msm_misinfo_test_res <- chisq.test(msm_misinfo_user_claim_stats$claim_type, msm_misinfo_user_claim_stats$platform)
msm_placebo_test_res <- chisq.test(msm_placebo_user_claim_stats$claim_type, msm_placebo_user_claim_stats$platform)
misinfo_placebo_test_res <- chisq.test(misinfo_placebo_user_claim_stats$claim_type, misinfo_placebo_user_claim_stats$platform)

grouping_vars <- c()
summary_stats <- claims %>%
  group_by_at(c(grouping_vars, "claim_type")) %>%
  mutate(group_count=n()) %>%
  group_by_at(c(grouping_vars, "claim_type", "platform")) %>%
  summarise(freq=n()/first(group_count))
if(is.null(grouping_vars)) {
  summary_stats$group <- summary_stats$claim_type
} else {
  summary_stats$group <- paste0(summary_stats$claim_type, '-', summary_stats[[grouping_vars]])
}
# drop platform with very low frequency
summary_stats <- summary_stats %>% group_by(platform, group) %>%
  mutate(platform_group_sum=sum(freq))
summary_stats <- summary_stats[summary_stats$platform_group_sum > 0.05,]
summary_stats$percent_freq <- paste0(round(summary_stats$freq*100, 1), '%')
summary_stats[summary_stats$platform == "Facebook Messenger", "platform"] <- "Facebook\nMessenger"

ggplot(summary_stats, aes(x=platform, y=freq, fill=group, label=percent_freq)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Fraction within claim type") +
  xlab("Platform encountered") +
  labs(fill="Claim Type") +
  ggtitle(paste('Pearson Chi-squared p-vals\n',
                'misinfo/msm:', round(msm_misinfo_test_res$p.value, 4),
                'misinfo/placebo:', round(misinfo_placebo_test_res$p.value, 4),
                'msm/placebo:', round(msm_placebo_test_res$p.value, 4))) +
  geom_text(size=2.8, position=position_dodge(0.9), hjust=0.5, vjust=-0.25) +
  theme(plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
ggsave("./results/pakistan/prevalence/platform_by_claim_type.png",  width = 10, height = 6)


grouping_vars <- c()
summary_stats <- claims %>%
  group_by_at(c(grouping_vars, "claim")) %>%
  mutate(group_count=n()) %>%
  group_by_at(c(grouping_vars, "claim", "platform")) %>%
  summarise(freq=n()/first(group_count))
summary_stats$claim_num <- readr::parse_number(summary_stats$claim)
summary_stats$claim_type <- ifelse(summary_stats$claim_num < 11, "Misinfo",
                                   ifelse(summary_stats$claim_num < 111, "MSM", "Placebo"))
summary_stats$claim_type <- factor(summary_stats$claim_type, levels=c('Placebo', 'Misinfo', 'MSM'), ordered=TRUE)
if(is.null(grouping_vars)) {
  summary_stats <- summary_stats[order(summary_stats$claim_type, summary_stats$freq),]
} else {
  summary_stats <- summary_stats[order(summary_stats$claim_type, summary_stats[[grouping_vars]], summary_stats$freq),]
}
summary_stats$group <- seq(nrow(summary_stats))
summary_stats$claim_type <- as.character(summary_stats$claim_type)
if(is.null(grouping_vars)) {
  summary_stats$grouping <- summary_stats$claim_type
} else {
  summary_stats$grouping <- paste0(summary_stats$claim_type, '-', summary_stats[[grouping_vars]])
}

ggplot(summary_stats, aes(x=platform, y=freq, group=group, fill=grouping)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Fraction within claim type") +
  xlab("Platform encountered") +
  theme(plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.title = element_text(size=20), 
        legend.text = element_text(size=18))
ggsave("./results/pakistan/prevalence/individual_platform.png",  width = 12, height = 6)

####################################################################################
# ANALYSIS OF PREVALENCE AND BELIEF
# evalute prevalence of misinfo claims versus mainstream and placebo claims
claims <- generate_shown_claims_df(df)
claims <- gather_claims_by_columns(df, claims, PREVALENCE_BELIEF_COLS, "prevalence_belief")
claims$prevalence <- gsub("_.*", "", claims$prevalence_belief)
claims$belief <- gsub(".*_", "", claims$prevalence_belief)
claims[claims$belief == "NA", "belief"] <- NA

claims$prevalence <-  factor(claims$prevalence, 
                            levels=c("No", "Maybe", "Yes"), ordered=TRUE)
claims$belief <- factor(claims$belief,
                        levels=c("Definitely accurate", "Probably accurate",
                                 "Not sure if accurate or inaccurate",
                                 "Probably inaccurate", "Definitely inaccurate"),
                        ordered=F)
levels(claims$belief) <- c("Definitely\naccurate", "Probably\naccurate",
                           "Not sure if accurate\nor inaccurate",
                           "Probably\ninaccurate", "Definitely\n inaccurate")  
claims$prevalence_numeric <- sapply(as.character(claims$prevalence), switch,
                                    "No"=0, "Maybe"=1, "Yes"=2, "NA"=NA, NA)
summary(lm(prevalence_numeric ~ claim_type, claims))
summary(lm(prevalence_numeric ~ claim_type + belief, claims))
print_summary_table(polr(prevalence ~ claim_type, claims, Hess=TRUE))
print_summary_table(polr(prevalence ~ claim_type + belief, claims, Hess=TRUE))
print_summary_table(polr(prevalence ~ claim_type + belief + follow_news, claims, Hess=TRUE))

# perform pairwise tests between claim types on prevalence
msm_misinfo_user_claim_stats <- claims[claims$claim_type %in% c("MSM", "Misinfo"),]
msm_placebo_user_claim_stats <- claims[claims$claim_type %in% c("MSM", "Placebo"),]
misinfo_placebo_user_claim_stats <- claims[claims$claim_type %in% c("Misinfo", "Placebo"),]
test_res <- chisq.test(claims$claim_type, claims$prevalence)
msm_misinfo_test_res <- chisq.test(msm_misinfo_user_claim_stats$claim_type, msm_misinfo_user_claim_stats$prevalence)
msm_placebo_test_res <- chisq.test(msm_placebo_user_claim_stats$claim_type, msm_placebo_user_claim_stats$prevalence)
misinfo_placebo_test_res <- chisq.test(misinfo_placebo_user_claim_stats$claim_type, misinfo_placebo_user_claim_stats$prevalence)


grouping_vars <- c()
summary_stats <- claims %>%
  group_by_at(c(grouping_vars, "claim_type")) %>%
  mutate(group_count=n()) %>%
  group_by_at(c(grouping_vars, "claim_type", "prevalence")) %>%
  summarise(freq=n()/first(group_count))
if(is.null(grouping_vars)) {
  summary_stats$group <- summary_stats$claim_type
} else {
  summary_stats$group <- paste0(summary_stats$claim_type, '-', summary_stats[[grouping_vars]])
}
ggplot(summary_stats, aes(x=prevalence, y=freq, fill=group)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Fraction within claim type") +
  xlab("Prevalence (Encountered before)") +
  labs(fill="Claim Type") +
  ggtitle(paste('Pearson Chi-squared p-vals\n',
                'misinfo/msm:', round(msm_misinfo_test_res$p.value, 4),
                'misinfo/placebo:', round(misinfo_placebo_test_res$p.value, 4),
                'msm/placebo:', round(msm_placebo_test_res$p.value, 4))) +
  theme(plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18), 
        legend.text = element_text(size=16))



# perform pairwise tests between claim types on platform
claims <- claims[!is.na(claims$platform),]
msm_misinfo_user_claim_stats <- claims[claims$claim_type %in% c("MSM", "Misinfo"),]
msm_placebo_user_claim_stats <- claims[claims$claim_type %in% c("MSM", "Placebo"),]
misinfo_placebo_user_claim_stats <- claims[claims$claim_type %in% c("Misinfo", "Placebo"),]
test_res <- chisq.test(claims$claim_type, claims$platform)
msm_misinfo_test_res <- chisq.test(msm_misinfo_user_claim_stats$claim_type, msm_misinfo_user_claim_stats$platform)
msm_placebo_test_res <- chisq.test(msm_placebo_user_claim_stats$claim_type, msm_placebo_user_claim_stats$platform)
misinfo_placebo_test_res <- chisq.test(misinfo_placebo_user_claim_stats$claim_type, misinfo_placebo_user_claim_stats$platform)

grouping_vars <- c()
summary_stats <- claims %>%
  group_by_at(c(grouping_vars, "claim_type")) %>%
  mutate(group_count=n()) %>%
  group_by_at(c(grouping_vars, "claim_type", "platform")) %>%
  summarise(freq=n()/first(group_count))
if(is.null(grouping_vars)) {
  summary_stats$group <- summary_stats$claim_type
} else {
  summary_stats$group <- paste0(summary_stats$claim_type, '-', summary_stats[[grouping_vars]])
}
# drop platform with very low frequency
summary_stats <- summary_stats %>% group_by(platform, group) %>%
  mutate(platform_group_sum=sum(freq))
summary_stats <- summary_stats[summary_stats$platform_group_sum > 0.05,]

summary_stats[summary_stats$platform == "Facebook Messenger", "platform"] <- "Facebook\nMessenger"
ggplot(summary_stats, aes(x=platform, y=freq, fill=group)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Fraction within claim type") +
  xlab("Platform encountered") +
  labs(fill="Claim Type") +
  ggtitle(paste('Pearson Chi-squared p-vals\n',
                'misinfo/msm:', round(msm_misinfo_test_res$p.value, 4),
                'misinfo/placebo:', round(misinfo_placebo_test_res$p.value, 4),
                'msm/placebo:', round(msm_placebo_test_res$p.value, 4))) +
  theme(plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18), 
        legend.text = element_text(size=16))
