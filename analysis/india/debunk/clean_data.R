library(readxl)

add_aux_variables <- function(data) {
  # add tie and tie-group treatment variables
  data$tie_treatment <- ifelse(data$CLOSE_QUAL_1 == "Yes", "Strong", "Weak")
  data$group_treatment <- "Outgroup"
  data$group_treatment[data$AGREE_QUAL_1 == "Yes" | data$AGREE_QUAL_3 == "Yes"] <- "Ingroup"
  data$format_treatment <- "None"
  data$format_treatment[data$MRK_STIMULI_1 == "Yes"] <- "Image"
  data$format_treatment[data$MRK_STIMULI_2 == "Yes"] <- "Audio"
  data$format_treatment[data$MRK_STIMULI_3 == "Yes"] <- "Text"
  data$tie_group_treatment <- paste(data$tie_treatment, data$group_treatment, sep='-')
  data$tie_group_format_treatment <- paste(data$tie_treatment, data$group_treatment, data$format_treatment, sep='-')
  
  data$tie_treatment <- as.factor(data$tie_treatment)
  data$group_treatment <- as.factor(data$group_treatment)
  data$format_treatment <- as.factor(data$format_treatment)
  data$tie_group_treatment <- as.factor(data$tie_group_treatment)
  data$tie_group_format_treatment <- as.factor(data$tie_group_format_treatment)
  

  # Add collapsed version of intention to share  
  data$QCORR_MIMSHARE_BINARY <- (data$QCORR_MIMSHARE > "Moderately likely")
  data$QCORR_SNSSHARE_BINARY <- (data$QCORR_SNSSHARE > "Moderately likely")
  
  # binary correction interest variables
  data$QCORR_IN01_BINARY <- factor(ifelse(data$QCORR_IN01 >= "A good amount", "High", "Low"),
                                   levels=c("Low", "High"), ordered=TRUE)
  data$QCORR_IN02_BINARY <- factor(ifelse(data$QCORR_IN02 >= "A good amount", "High", "Low"),
                                   levels=c("Low", "High"), ordered=TRUE)
  data$QCORR_IN03_BINARY <- factor(ifelse(data$QCORR_IN03 >= "Interesting", "High", "Low"),
                                   levels=c("Low", "High"), ordered=TRUE)
  # binary correction credibility variables
  data$QCORR_CRED01_BINARY <- factor(ifelse(data$QCORR_CRED01 >= "Accurate", "High", "Low"),
                                   levels=c("Low", "High"), ordered=TRUE)
  data$QCORR_CRED02_BINARY <- factor(ifelse(data$QCORR_CRED02 >= "Authentic", "High", "Low"),
                                   levels=c("Low", "High"), ordered=TRUE)
  data$QCORR_CRED03_BINARY <- factor(ifelse(data$QCORR_CRED03 >= "Believable", "High", "Low"),
                                     levels=c("Low", "High"), ordered=TRUE)
  data$QCORR_CRED04_BINARY <- factor(ifelse(data$QCORR_CRED04 >= "Professional", "High", "Low"),
                                     levels=c("Low", "High"), ordered=TRUE)
  data$survey_duration_mins <- as.numeric(data$DataCollection_FinishTime - data$DataCollection_StartTime)
  
  # same gender tie
  data$tie_same_gender <- (data$QTIE_GEN == data$resp_gender)
  data$tie_diff_gender <- (data$QTIE_GEN != data$resp_gender)
  
  #########
  # add variables on level of attention
  data$real_tie_name <- (data$QIND_SCREEN == "Yes") 
  data$searched_internet <- (data$QSEARCH_SCREEN == "Yes") 
  # determine whether misinfo and correction manipulation checks were answered correctly 
  data$correct_misinfo_manip <- (data$QMISINFO_MANIP == "Abhinandan Varthaman's wife, Tanvi Marwaha")
  data$correct_corr_manip <- ((data$QCORR_MANIP == "Jahnavi Das, wife of election strategist Prashant Kishore" &
                               data$modi_tweet) |
                              (data$QCORR_MANIP == "Aditi Singh, an MLA from Rae Bareli" &  
                               data$gandhi_tweet))
  # determine whether color attention check was answered correctly
  data$correct_attention_check <- (data$QATT_SCREEN == 'puce')
  # number of possible correct attention combinations
  data$correct_all_three_attention_checks <- (data$correct_attention_check & data$correct_misinfo_manip & data$correct_corr_manip)
  data$correct_atleast_one_attention_check <- (data$correct_attention_check | data$correct_misinfo_manip | data$correct_corr_manip)
  data$correct_atleast_two_attention_checks <- ((data$correct_attention_check & data$correct_misinfo_manip) |
                                                (data$correct_attention_check & data$correct_corr_manip) |
                                                (data$correct_misinfo_manip & data$correct_corr_manip))
  data$correct_only_one_attention_check <- (data$correct_atleast_one_attention_check & !data$correct_atleast_two_attention_checks)
  data$correct_color_misinfo_attention_checks <- (data$correct_attention_check & data$correct_misinfo_manip)
  data$correct_color_corr_attention_checks <- (data$correct_attention_check & data$correct_corr_manip)
  data$correct_misinfo_corr_attention_checks <- (data$correct_misinfo_manip & data$correct_corr_manip)
  # survey took too long (99% quantile)
  data$too_long <- (data$survey_duration_mins > quantile(data$survey_duration_mins, 0.99, na.rm=TRUE))
  
  return(data)
}


add_numeric_variables <- function(data) {
  # convert a few variable to their numeric versions 
  data$correction_mimshare_numeric <- sapply(as.character(data$QCORR_MIMSHARE), switch,
                                             "Not at all likely"=0, "Slightly likely"=1, "Moderately likely"=2,
                                             "Likely"=3, "Very likely"=4, "NA"=NA, NA)
  data$correction_snsshare_numeric <- sapply(as.character(data$QCORR_SNSSHARE), switch,
                                             "Not at all likely"=0, "Slightly likely"=1, "Moderately likely"=2,
                                             "Likely"=3, "Very likely"=4, "NA"=NA, NA)
  data$education_numeric <- sapply(as.character(data$IN01EDU), switch,
                                   "School up to 4 Years"=1, "School 5-9 years"=2, "SSC (completed 10th class)"=3,
                                   "HSC (completed 12th class)"=4, "Some college but not graduate"=5,
                                   "Graduate /Post-Graduate – Professional(B.Tech/M.Tech, MBA, MBBS etc)"=6,
                                   "Graduate/ Post-Graduate – General( B.Sc/M.Sc, B.Com/M.Com, B.A./M.A. etc)"=7, NA)
  data$income_numeric <- sapply(as.character(data$IN01INC), switch,
                                "<Rs. 10,000"=1,
                                "Rs. 10,001 - Rs. 25,000"=2,
                                "Rs. 25,001 - Rs. 50,000"=3,
                                "Rs. 50,001 - Rs. 100,000"=4,
                                "Rs. 1 Lakh – Rs. 1.5 Lakh"=5,
                                "Rs. 1.5 Lakh – Rs. 2 Lakh"=6,
                                "Above 2 Lakh"=7,
                                "Prefer not to answer"=NA, NA)
  data$mim_discuss_news_numeric <- sapply(as.character(data$QMIM_DISCUSS_NEWS), switch,
                                          "Never"=0, "Rarely"=1, "Sometimes"=2,
                                          "Frequently"=3, "Very frequently"=4, "NA"=NA, NA)
  data$sns_discuss_news_numeric <- sapply(as.character(data$QSNS_DISCUSS_NEWS), switch,
                                          "Never"=0, "Rarely"=1, "Sometimes"=2,
                                          "Frequently"=3, "Very frequently"=4, "NA"=NA, NA)
  data$misinfo_belief_numeric <- sapply(as.character(data$QMISINFO_BELIEF), switch,
                                        "Definitely inaccurate"=0, "Probably inaccurate"=1,
                                        "Not sure if accurate or inaccurate"=2,
                                        "Probably accurate"=3, "Definitely accurate"=4,
                                        "NA"=NA, NA)
  data$resp_age <- as.numeric(data$resp_age)
  data$politics_attention_numeric <- sapply(as.character(data$QPOLI_ATT), switch,
                                            "Never"=0, "Rarely"=1, "Sometimes"=2, "Frequently"=3, "Very frequently"=4,
                                            "NA"=NA, NA)
  data$politics_interest_numeric <- sapply(data$QPOLI_INTEREST, switch,
                                           "Not at all interested"=0, "Slightly interested"=1, "Moderately interested"=2,
                                           "Interested"=3, "Very interested"=4,
                                           "NA"=NA, NA)
  
  # numeric representation of interest in message
  data$correction_interest1_numeric <- sapply(as.character(data$QCORR_IN01), switch,
                                              "None at all"=0, "A little"=1, "A moderate amount"=2,
                                              "A good amount"=3, "A great deal"=4,
                                              "NA"=NA, NA)
  data$correction_interest2_numeric <- sapply(as.character(data$QCORR_IN02), switch,
                                              "Not at all"=0, "A little"=1, "A moderate amount"=2,
                                              "A good amount"=3, "A great deal"=4,
                                              "NA"=NA, NA)
  data$correction_interest3_numeric <- sapply(as.character(data$QCORR_IN03), switch,
                                              "Not at all interesting"=0, "Slightly interesting"=1,
                                              "Moderately interesting"=2, "Interesting"=3,
                                              "Very interesting"=4, "NA"=NA, NA)
  
  # numeric representation of message credibility
  data$correction_credibility1_numeric <- sapply(as.character(data$QCORR_CRED01), switch,
                                              "Not at all accurate"=0, "Slightly accurate"=1,
                                              "Moderately accurate"=2, "Accurate"=3,
                                              "Very accurate"=4, "NA"=NA, NA)
  data$correction_credibility2_numeric <- sapply(as.character(data$QCORR_CRED02), switch,
                                              "Not at all authentic"=0, "Slightly authentic"=1,
                                              "Moderately authentic"=2, "Authentic"=3,
                                              "Very authentic"=4, "NA"=NA, NA)
  data$correction_credibility3_numeric <- sapply(as.character(data$QCORR_CRED03), switch,
                                              "Not at all believable"=0, "Slightly believable"=1,
                                              "Moderately believable"=2, "Believable"=3,
                                              "Very believable"=4, "NA"=NA, NA)
  data$correction_credibility4_numeric <- sapply(as.character(data$QCORR_CRED04), switch,
                                                 "Not at all professional"=0, "Slightly professional"=1,
                                                 "Moderately professional"=2, "Professional"=3,
                                                 "Very professional"=4, "NA"=NA, NA)
  
  data$misinfo_belief_numeric <- sapply(as.character(data$QMISINFO_BELIEF), switch,
                                        "Definitely accurate"=0, "Probably accurate"=1,
                                        "Not sure if accurate or inaccurate"=2,
                                        "Probably inaccurate"=3, "Definitely inaccurate"=4,
                                        "NA"=NA, NA)
  data$misinfo_belief_1_numeric <- sapply(as.character(data$QMISINFO_BELIEF_1), switch,
                                        "Definitely accurate"=0, "Probably accurate"=1,
                                        "Not sure if accurate or inaccurate"=2,
                                        "Probably inaccurate"=3, "Definitely inaccurate"=4,
                                        "NA"=NA, NA)
  
  # numeric representation of tie strength/agree measures
  for(i in c(paste0("0", 1:9), as.character(10:12))) {
    data[,paste0("tie_strength", i, "_numeric")] <- sapply(as.character(data[[paste0("QTIE_STR", i)]]), switch,
                                                           "Strongly disagree"=0, "Somewhat disagree"=1,
                                                           "Neither agree nor disagree"=2,
                                                           "Somewhat agree"=3, "Strongly agree"=4,
                                                           "NA"=NA, NA)
  }
  data$average_tie_strength <- rowMeans(data[,c(paste0("tie_strength", c(paste0("0", 1:9), as.character(10:12)), "_numeric"))])
  data$tie_agree_numeric <- sapply(as.character(data$QTIE_AGREE), switch,
                                   "Strongly disagree"=0, "Somewhat disagree"=1,
                                   "Neither agree nor disagree"=2,
                                   "Somewhat agree"=3, "Strongly agree"=4,
                                   "NA"=NA, NA)
 
  # Numeric version of treatment and correction variables 
  data$tie_treatment_numeric <- ifelse(data$tie_treatment == "Strong", 0, 1)
  data$group_treatment_numeric <- ifelse(data$group_treatment == "Ingroup", 0, 1)
  data$correction_interest1_binary_numeric <- ifelse(data$QCORR_IN01_BINARY == "High", 1, 0)
  data$correction_interest2_binary_numeric <- ifelse(data$QCORR_IN02_BINARY == "High", 1, 0)
  data$correction_interest3_binary_numeric <- ifelse(data$QCORR_IN03_BINARY == "High", 1, 0)
  data$correction_credibility1_binary_numeric <- ifelse(data$QCORR_CRED01_BINARY == "High", 1, 0)
  data$correction_credibility2_binary_numeric <- ifelse(data$QCORR_CRED02_BINARY == "High", 1, 0)
  data$correction_credibility3_binary_numeric <- ifelse(data$QCORR_CRED03_BINARY == "High", 1, 0)
  data$correction_credibility4_binary_numeric <- ifelse(data$QCORR_CRED04_BINARY == "High", 1, 0)
  
  # numeric variables on tie description
  data$tie_credibility1_numeric <- sapply(as.character(data$QTIE_CRED01), switch,
                                          "Strongly disagree"=0, "Somewhat disagree"=1,
                                          "Neither agree nor disagree"=2,
                                          "Somewhat agree"=3, "Strongly agree"=4,
                                          "NA"=NA, NA)
  data$tie_credibility2_numeric <- sapply(as.character(data$QTIE_CRED02), switch,
                                          "Strongly disagree"=0, "Somewhat disagree"=1,
                                          "Neither agree nor disagree"=2,
                                          "Somewhat agree"=3, "Strongly agree"=4,
                                          "NA"=NA, NA)
  data$tie_credibility3_numeric <- sapply(as.character(data$QTIE_CRED03), switch,
                                          "Strongly disagree"=0, "Somewhat disagree"=1,
                                          "Neither agree nor disagree"=2,
                                          "Somewhat agree"=3, "Strongly agree"=4,
                                          "NA"=NA, NA)
  data$tie_political_freq_numeric <- sapply(as.character(data$QTIE_POLI_FREQ), switch,
                                            "Never"=0, "Rarely"=1, "Sometimes"=2, "Frequently"=3, "Very frequently"=4,
                                            "NA"=NA, NA)
  data$tie_same_gender_numeric <- ifelse(data$tie_same_gender == TRUE, 1, 0)
  data$tie_diff_gender_numeric <- ifelse(data$tie_diff_gender == TRUE, 1, 0)
  
  return(data)
}


convert_to_factor <- function(data) {
  # Function converts a few variables to factors
  data$resp_gender <- as.factor(data$resp_gender)
  data$IN01INC <- as.factor(data$IN01INC)
  data$QSEARCH_SCREEN <- as.factor(data$QSEARCH_SCREEN)
  data$QIND_SCREEN <- as.factor(data$QIND_SCREEN)
  
  # ordinal factors
  data$QCORR_MIMSHARE <- factor(data$QCORR_MIMSHARE,
                                levels=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"),
                                ordered=TRUE)
  data$QCORR_SNSSHARE <- factor(data$QCORR_SNSSHARE,
                                levels=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"),
                                ordered=TRUE)
  data$QMIM_USE02 <- factor(data$QMIM_USE02,
                            levels=c("Never", "Once", "A few times", "Once a day", "A couple of times a day",
                                     "Once every few hours", "Once an hour", "Multiple times an hour"),
                            ordered=TRUE)
  data$QMIM_DISCUSS_NEWS <- factor(data$QMIM_DISCUSS_NEWS,
                                   levels=c("Never", "Rarely", "Sometimes", "Frequently", "Very frequently"),
                                   ordered=TRUE)
  data$QMIM_SEE_NEWS <- factor(data$QMIM_SEE_NEWS,
                                   levels=c("Never", "Rarely", "Sometimes", "Frequently", "Very frequently"),
                                   ordered=TRUE)
  data$QSNS_USE02 <- factor(data$QSNS_USE02,
                            levels=c("Never", "Once", "A few times", "Once a day", "A couple of times a day",
                                     "Once every few hours", "Once an hour", "Multiple times an hour"),
                            ordered=TRUE)
  data$QSNS_DISCUSS_NEWS <- factor(data$QSNS_DISCUSS_NEWS,
                                   levels=c("Never", "Rarely", "Sometimes", "Frequently", "Very frequently"),
                                   ordered=TRUE)
  data$QSNS_SEE_NEWS <- factor(data$QSNS_SEE_NEWS,
                                   levels=c("Never", "Rarely", "Sometimes", "Frequently", "Very frequently"),
                                   ordered=TRUE)
  data$QPOLI_ATT <- factor(data$QPOLI_ATT,
                           levels=c("Never", "Rarely", "Sometimes", "Frequently", "Very frequently"),
                           ordered=TRUE)
  data$QPOLI_INTEREST <- factor(data$QPOLI_INTEREST,
                                levels=c("Not at all interested", "Slightly interested", "Moderately interested",
                                         "Interested", "Very interested"),
                                ordered=TRUE)
                            
  data$QCORR_IN01 <- factor(data$QCORR_IN01,
                            levels=c("None at all", "A little", "A moderate amount", "A good amount", "A great deal"),
                            ordered=TRUE)
  data$QCORR_IN02 <- factor(data$QCORR_IN02,
                            levels=c("Not at all", "A little", "A moderate amount", "A good amount", "A great deal"),
                            ordered=TRUE)
  data$QCORR_IN03 <- factor(data$QCORR_IN03,
                            levels=c("Not at all interesting", "Slightly interesting", "Moderately interesting",
                                     "Interesting", "Very interesting"),
                            ordered=TRUE)
  
  data$QCORR_CRED01 <- factor(data$QCORR_CRED01,
                            levels=c("Not at all accurate", "Slightly accurate",
                                     "Moderately accurate", "Accurate", "Very accurate"),
                            ordered=TRUE)
  data$QCORR_CRED02 <- factor(data$QCORR_CRED02,
                            levels=c("Not at all authentic", "Slightly authentic",
                                     "Moderately authentic", "Authentic", "Very authentic"),
                            ordered=TRUE)
  data$QCORR_CRED03 <- factor(data$QCORR_CRED03,
                            levels=c("Not at all believable", "Slightly believable",
                                     "Moderately believable", "Believable", "Very believable"),
                            ordered=TRUE)
  data$QCORR_CRED04 <- factor(data$QCORR_CRED04,
                            levels=c("Not at all professional", "Slightly professional",
                                     "Moderately professional", "Professional", "Very professional"),
                            ordered=TRUE)
  
  # convert belief measures pre and post correction to factor
  data$QMISINFO_BELIEF <- factor(data$QMISINFO_BELIEF,
                                 levels=c("Definitely accurate", "Probably accurate",
                                          "Not sure if accurate or inaccurate",
                                          "Probably inaccurate", "Definitely inaccurate"),
                                 ordered = TRUE)
  data$QMISINFO_BELIEF_1 <- factor(data$QMISINFO_BELIEF_1,
                                   levels=c("Definitely accurate", "Probably accurate",
                                            "Not sure if accurate or inaccurate",
                                            "Probably inaccurate", "Definitely inaccurate"),
                                   ordered = TRUE)
  
  # convert tie strength/agree vars to ordered factor
  for(i in c(paste0("0", 1:9), as.character(10:12))) {
    # convert to ordered factor
    data[,paste0("QTIE_STR", i)] <- factor(data[[paste0("QTIE_STR", i)]],
                                           c("Strongly disagree", "Somewhat disagree",
                                             "Neither agree nor disagree",
                                             "Somewhat agree", "Strongly agree"),
                                           ordered=TRUE)
  }
  data$QTIE_AGREE <- factor(data$QTIE_AGREE,
                            c("Strongly disagree", "Somewhat disagree",
                              "Neither agree nor disagree",
                              "Somewhat agree", "Strongly agree"),
                            ordered=TRUE)
  
  # convert tie description variables to ordered factors
  data$QTIE_CRED01 <- factor(data$QTIE_CRED01,
                             c("Strongly disagree", "Somewhat disagree",
                               "Neither agree nor disagree",
                               "Somewhat agree", "Strongly agree"),
                            ordered=TRUE)
  data$QTIE_CRED02 <- factor(data$QTIE_CRED02,
                             c("Strongly disagree", "Somewhat disagree",
                               "Neither agree nor disagree",
                               "Somewhat agree", "Strongly agree"),
                            ordered=TRUE)
  data$QTIE_CRED03 <- factor(data$QTIE_CRED03,
                             c("Strongly disagree", "Somewhat disagree",
                               "Neither agree nor disagree",
                               "Somewhat agree", "Strongly agree"),
                            ordered=TRUE)
  data$QTIE_POLI_FREQ <- factor(data$QTIE_POLI_FREQ,
                             c("Never", "Rarely", "Sometimes", "Frequently", "Very frequently"),
                            ordered=TRUE)
  data$QTIE_GEN <- as.factor(data$QTIE_GEN)
  
  # prevalence, belief vars
  data$QTIE_AGREE <- factor(data$QTIE_AGREE,
                            c("Strongly disagree", "Somewhat disagree",
                              "Neither agree nor disagree",
                              "Somewhat agree", "Strongly agree"),
                            ordered=TRUE)
  for(i in seq(1,116)) {
    # convert to ordered factor
    belief_col <- paste0("LOOPCLAIM_PREV_CLAIM_BELIEF_", i, "_QCLAIM_BELIEF")
    data[,belief_col] <- factor(data[[belief_col]],
                                levels=c("Definitely accurate", "Probably accurate",
                                         "Not sure if accurate or inaccurate",
                                         "Probably inaccurate", "Definitely inaccurate"),
                                ordered=TRUE)
    
    data[,paste0("LOOPCLAIM_PREV_CLAIM_BELIEF_", i, "_QCLAIM_PREV")] <- factor(
      data[[paste0("LOOPCLAIM_PREV_CLAIM_BELIEF_", i, "_QCLAIM_PREV")]],
      levels=c("No", "Maybe", "Yes"), ordered=TRUE)
  }
  
  return(data)
}

process_data <- function(data_file) {
  data <- read_excel(data_file)
  data$QATT_SCREEN <- tolower(data$QATT_SCREEN)
  data$modi_tweet <- (data$TWEET_ASSGNMENT_MARKER_1 == "Yes")
  data$gandhi_tweet <- (data$TWEET_ASSGNMENT_MARKER_2 == "Yes")

  # get tie role
  data$tie_role <- "Friend"
  data$tie_role[data$QTIE_ROLE_2 == "Yes"] <- "Family"
  data$tie_role[data$QTIE_ROLE_3 == "Yes"] <- "Romantic Partner"
  data$tie_role[data$QTIE_ROLE_4 == "Yes"] <- "Work Colleague" 
  data$tie_role[data$QTIE_ROLE_5 == "Yes"] <- "Schoolmate"
  data$tie_role[data$QTIE_ROLE_6 == "Yes"] <- "Neighbor" 
  data$tie_role[data$QTIE_ROLE_7 == "Yes"] <- "Member of peer group"
  data$tie_role[data$QTIE_ROLE_8 == "Yes"] <- "Acquaintance"
  data$tie_role[data$QTIE_ROLE_9 == "Yes"] <- "Stranger"
  data$tie_role[data$QTIE_ROLE_10 == "Yes"] <- "Other"
  data$tie_role[data$QTIE_ROLE_101 == "Yes"] <- NA

  data <- convert_to_factor(data)
  data <- add_aux_variables(data)
  data <- add_numeric_variables(data)
  
  return(data)
}


exclude_nonattentive <- function(data, no_treatment, real_tie, search_internet,
                                 attention_check, correct_misinfo, correct_correction,
                                 long_duration) {
  df <- data
  if (no_treatment) {
    df <- df[df$format_treatment != "None",]
  }
  if (real_tie) {
    df <- df[df$real_tie_name,]
  }
  if (search_internet) {
    df <- df[!df$searched_internet,]
  }
  if (attention_check) {
    df <- df[df$correct_attention_check,]
  }
  if (correct_misinfo) {
    df <- df[df$correct_misinfo_manip & !is.na(df$correct_misinfo_manip),]
  }
  if (correct_correction) {
    df <- df[df$correct_corr_manip & !is.na(df$correct_corr_manip),]
  }
  if (long_duration) {
    df <- df[!df$too_long,]
  }
  return(df)
}

print_attentive_stats <- function(data) {
  percent_no_treament <- round(sum(data$format_treatment == "None") / nrow(data), 4) * 100
  percent_unreal_ind <- round(1 - (sum(data$real_tie_name) / nrow(data)), 4) * 100
  percent_searched_internet <- round(sum(data$searched_internet) / nrow(data), 4) * 100
  percent_failed_color_attention <- round(1 - sum(data$correct_attention_check) / nrow(data), 4) * 100
  percent_failed_misinfo_attention <- round(1 - sum(data$correct_misinfo_manip) / nrow(data), 4) * 100
  percent_failed_color_misinfo_attentions <- round(1 - sum(data$correct_attention_check & data$correct_misinfo_manip) / nrow(data), 4) * 100
  
  tmp_data <- data[!is.na(data$correct_corr_manip),]
  percent_failed_correction_attention <- round(1 - sum(tmp_data$correct_corr_manip) / nrow(tmp_data), 4) * 100
  percent_failed_color_correction_attentions <- round(1 - sum(tmp_data$correct_attention_check & tmp_data$correct_corr_manip) / nrow(tmp_data), 4) * 100
  percent_failed_misinfo_correction_attentions <- round(1 - sum(tmp_data$correct_misinfo_manip & tmp_data$correct_corr_manip) / nrow(tmp_data), 4) * 100
  
  percent_failed_all_three_attentions <- round(1 - sum(tmp_data$correct_atleast_one_attention_check) / nrow(tmp_data), 4) * 100
  percent_failed_atleast_two_attentions <- round(sum(!tmp_data$correct_atleast_one_attention_check | tmp_data$correct_only_one_attention_check) / nrow(tmp_data), 4) * 100
  percent_failed_atleast_one_attention <- round(1 - sum(tmp_data$correct_all_three_attention_checks) / nrow(tmp_data), 4) * 100
  
  cat(paste0(percent_no_treament, '% of subjects did not receive the correction treatment.\n'))
  cat(paste0(percent_unreal_ind, '% of subjects entered an unreal name for individual contact.\n'))
  cat(paste0(percent_searched_internet, '% of subjects searched the internet during survey.\n'))
  cat(paste0(percent_failed_color_attention, '% of subjects failed the color attention check.\n'))
  cat(paste0(percent_failed_misinfo_attention, '% of subjects failed the misinfo manipulation check.\n'))
  cat(paste0(percent_failed_correction_attention, '% of subjects failed the correction manipulation check.\n'))
  cat(paste0(percent_failed_color_misinfo_attentions, '% of subjects failed either the color attention or misinfo manipulation checks.\n'))
  cat(paste0(percent_failed_color_correction_attentions, '% of subjects failed either the color attention or correction manipulation checks.\n'))
  cat(paste0(percent_failed_misinfo_correction_attentions, '% of subjects failed either the misinfo manipulation or correction manipulation checks.\n'))
  cat(paste0(percent_failed_all_three_attentions, '% of subjects failed all three attention checks.\n'))
  cat(paste0(percent_failed_atleast_two_attentions, '% of subjects failed at least two attention checks.\n'))
  cat(paste0(percent_failed_atleast_one_attention, '% of subjects failed at least one attention checks.\n'))
}
  
