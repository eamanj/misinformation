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
  data$real_tie_name <- (grepl("Yes", data$QIND_SCREEN))
  data$searched_internet <- (grepl("Yes", data$QSEARCH_SCREEN))
  # determine whether misinfo and correction manipulation checks were answered correctly 
  data$correct_misinfo_manip <- ((data$QMISINFO_MANIP == "Bilawal Bhutto Zardari" & data$ppp_tweet) |
                                 (data$QMISINFO_MANIP == "Fawad Chaudhry" & data$pti_tweet) |
                                 (data$QMISINFO_MANIP == "Nawaz Sharif" & data$pml_tweet))
  data$correct_corr_manip <- (data$QCORR_MANIP == "Lahore, November 2018")
  # determine whether color attention check was answered correctly
  correct_patterns <- c("arag", "arg", "arq", "aurg", "aurq", "ark", "aro", "azg", "irg", "????", "????????", "??????", "??????","????????" , "??????", "??????")
  data$correct_attention_check <- (grepl(paste(correct_patterns, collapse='|'), data$QATT_SCREEN))
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
  # survey took too long (97% quantile)
  data$too_long <- (data$survey_duration_mins > quantile(data$survey_duration_mins, 0.97, na.rm=TRUE))
  
   
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
  data$education_numeric <- sapply(as.character(data$QEDUCATION_CHIEF), switch,
                                   "Illiterate"=1, "Less than primary"=2, "School 5-9 years"=3,
                                   "Metric / O Levels"=4, "Intermediate / A Levels"=5,
                                   "Graduate ??? B.A"=6, "Post Graduate ??? M.A"=7, "NA"=NA, NA)
  data$income_numeric <- sapply(as.character(data$QHOUSE_INC), switch,
                                "Less than 14,000 PKR"=1,
                                "PKR 14,000 - PKR 30,000"=2,
                                "PKR 30,001 ??? PKR 50,000"=3,
                                "PKR 50,001 ??? PKR 80,000"=4,
                                "PKR 80,001 ??? PKR 100,000"=5,
                                "PKR 100,001 ??? PKR 150,000"=6,
                                "More than PKR 150,000"=7,
                                "Refused / Prefer not to say"=NA, NA)
  data$mim_discuss_news_numeric <- sapply(as.character(data$QMIM_DISCUSS_NEWS), switch,
                                          "Never"=0, "Rarely"=1, "Sometimes"=2,
                                          "Frequently"=3, "Very frequently"=4, "NA"=NA, NA)
  data$sns_discuss_news_numeric <- sapply(as.character(data$QSNS_DISCUSS_NEWS), switch,
                                          "Never"=0, "Rarely"=1, "Sometimes"=2,
                                          "Frequently"=3, "Very frequently"=4, "NA"=NA, NA)
  data$resp_age <- as.numeric(sapply(strsplit(data$resp_age, " / "), '[[', 1))
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
  
  data$misinfo_belief_1_numeric <- sapply(as.character(data$QMISINFO_BELIEF01), switch,
                                         "Definitely accurate"=0, "Probably accurate"=1,
                                         "Not sure if accurate or inaccurate"=2,
                                         "Probably inaccurate"=3, "Definitely inaccurate"=4,
                                         "NA"=NA, NA)
  data$misinfo_belief_2_numeric <- sapply(as.character(data$QMISINFO_BELIEF02), switch,
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
  data$QHOUSE_INC <- as.factor(data$QHOUSE_INC)
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
  data$QMISINFO_BELIEF01 <- factor(data$QMISINFO_BELIEF01,
                                   levels=c("Definitely accurate", "Probably accurate",
                                            "Not sure if accurate or inaccurate",
                                            "Probably inaccurate", "Definitely inaccurate"),
                                   ordered = TRUE)
  data$QMISINFO_BELIEF02 <- factor(data$QMISINFO_BELIEF02,
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
  
  
  return(data)
}
  
fix_urdu_variables <- function(data) {
  gender <- stringr::str_extract(data$resp_gender, "(Male)|(Female)")
  data$resp_gender <- gender
  tie_gender <- stringr::str_extract(data$QTIE_GEN, "(Male)|(Female)")
  data$QTIE_GEN <- tie_gender
  searched <- stringr::str_extract(data$QSEARCH_SCREEN, "(Yes)|(No)")
  data$QSEARCH_SCREEN <- searched
  individual <- stringr::str_extract(data$QIND_SCREEN, "(Yes)|(No)")
  data$QIND_SCREEN <- individual
  
  # demographic vars
  education <- stringr::str_extract(data$QEDUCATION_CHIEF,
                                    "(Illiterate)|(Less than primary)|(School 5-9 years)|(Metric / O Levels)|(Intermediate / A Levels)|(Graduate ??? B.A)|(Post Graduate ??? M.A)")
  data$QEDUCATION_CHIEF <- education
  
  income <- stringr::str_extract(data$QHOUSE_INC,
                                 "(Less than 14,000 PKR)|(PKR 14,000 - PKR 30,000)|(PKR 30,001 ??? PKR 50,000)|(PKR 50,001 ??? PKR 80,000)|(PKR 80,001 ??? PKR 100,000)|(PKR 100,001 ??? PKR 150,000)|(More than PKR 150,000)|(More than PKR 150,000)|(Refused / Prefer not to say)")
  data$QHOUSE_INC <- income
  
  religion <- stringr::str_extract(data$QRELIGION,
                                   "(Christianity)|(Hinduism)|(Islam)|(None)|(Other)|(Sikhism)")
  data$QRELIGION <- religion
  
  # news consumption vars
  poli_interst <- stringr::str_extract(data$QPOLI_INTEREST,
                                      "(Moderately interested)|(Not at all interested)|(Slightly interested)|(Very interested)|(Interested)")
  data$QPOLI_INTEREST <- poli_interst
  
  poli_att <- stringr::str_extract(data$QPOLI_ATT,
                                      "(Never)|(Sometimes)|(Rarely)|(Very frequently)|(Frequently)")
  data$QPOLI_ATT <- poli_att
  
  acc_news <- stringr::str_extract(data$QACC_NEWS,
                                      "(A couple of times a day)|(A few times)|(Multiple times an hour)|(Never)|(Once every few hours)|(Once an hour)|(Once a day)|(Once)")
  data$QACC_NEWS <- acc_news
  
  # tie credibility
  for(i in 1:3) {
    qtie_cred <- stringr::str_extract(data[[paste0("QTIE_CRED0", i)]],
                                      "(Neither agree nor disagree)|(Somewhat agree)|(Somewhat disagree)|(Strongly agree)|(Strongly disagree)")
    data[[paste0("QTIE_CRED0", i)]] <- qtie_cred
  }
  qtie_poli <- stringr::str_extract(data$QTIE_POLI_FREQ,
                                    "(Never)|(Rarely)|(Sometimes)|(Very frequently)|(Frequently)")
  data$QTIE_POLI_FREQ <- qtie_poli
  
  # misinfo and correction vars 
  qmisinfo_manip <- stringr::str_extract(data$QMISINFO_MANIP,
                                         "(Fawad Chaudhry)|(Nawaz Sharif)|(Bilawal Bhutto Zardari)|(Asif Ali Zardari)|(Sheikh Rasheed Ahmad)")
  data$QMISINFO_MANIP <- qmisinfo_manip
  qcorr_manip <- stringr::str_extract(data$QCORR_MANIP,
                                      "(Lahore, November 2018)|(Karachi, January 2019)|(Islamabad, June 2018)|(Peshawar, March 2019)|(Islamabad, September 2016)")
  data$QCORR_MANIP <- qcorr_manip
  qcorr_mimshare <- stringr::str_extract(data$QCORR_MIMSHARE,
                                         "(Not at all likely)|(2)|(3)|(4)|(Very likely)")
  data$QCORR_MIMSHARE <- sapply(qcorr_mimshare, switch,
                                "Not at all likely"="Not at all likely",
                                "2"="Slightly likely", "3"="Moderately likely", "4"="Likely",
                                "Very likely"="Very likely", "NA"=NA, NA)
  qcorr_snsshare <- stringr::str_extract(data$QCORR_SNSSHARE,
                                         "(Not at all likely)|(2)|(3)|(4)|(Very likely)")
  data$QCORR_SNSSHARE <- sapply(qcorr_snsshare, switch,
                                "Not at all likely"="Not at all likely",
                                "2"="Slightly likely", "3"="Moderately likely", "4"="Likely",
                                "Very likely"="Very likely", "NA"=NA, NA)
  
  # news discussion vars
  mim_discuss_news <- stringr::str_extract(data$QMIM_DISCUSS_NEWS,
                                          "(Never)|(Rarely)|(Sometimes)|(Frequently)|(Very frequently)")
  data$QMIM_DISCUSS_NEWS <- mim_discuss_news
  sns_discuss_news <- stringr::str_extract(data$QSNS_DISCUSS_NEWS,
                                           "(Never)|(Rarely)|(Sometimes)|(Frequently)|(Very frequently)")
  data$QSNS_DISCUSS_NEWS <- sns_discuss_news
  mim_see_news <- stringr::str_extract(data$QMIM_SEE_NEWS,
                                       "(Never)|(Rarely)|(Sometimes)|(Frequently)|(Very frequently)")
  data$QMIM_SEE_NEWS <- mim_see_news
  sns_see_news <- stringr::str_extract(data$QSNS_SEE_NEWS,
                                       "(Never)|(Rarely)|(Sometimes)|(Frequently)|(Very frequently)")
  data$QSNS_SEE_NEWS <- sns_see_news
  
  # mim and sns use variables
  mim_use2 <- stringr::str_extract(data$QMIM_USE02,
                                   "(Never)|(A few times)|(Once a day)|(A couple of times a day)|(Once every few hours)|(Once an hour)|(Multiple times an hour)|(Once)")
  data$QMIM_USE02 <- mim_use2
                    
  sns_use2 <- stringr::str_extract(data$QSNS_USE02,
                                   "(Never)|(A few times)|(Once a day)|(A couple of times a day)|(Once every few hours)|(Once an hour)|(Multiple times an hour)|(Once)")
  data$QSNS_USE02 <- sns_use2
  
  # misinfo belief vars
  misinfo_belief1 <- stringr::str_extract(data$QMISINFO_BELIEF01,
                                          "(Definitely inaccurate)|(Probably inaccurate)|(Not sure if accurate or inaccurate)|(Probably accurate)|(Definitely accurate)")
  data$QMISINFO_BELIEF01 <- misinfo_belief1
  misinfo_belief2 <- stringr::str_extract(data$QMISINFO_BELIEF02,
                                          "(Definitely inaccurate)|(Probably inaccurate)|(Not sure if accurate or inaccurate)|(Probably accurate)|(Definitely accurate)")
  data$QMISINFO_BELIEF02 <- misinfo_belief2
  
  # interest vars 
  qcorr_in1 <- stringr::str_extract(data$QCORR_IN01,
                                    "(None at all)|(A little)|(A moderate amount)|(A good amount)|(A great deal)")
  data$QCORR_IN01 <- qcorr_in1
  qcorr_in2 <- stringr::str_extract(data$QCORR_IN02,
                                    "(Not at all)|(A little)|(A moderate amount)|(A good amount)|(A great deal)")
  data$QCORR_IN02 <- qcorr_in2
  qcorr_in3 <- stringr::str_extract(data$QCORR_IN03,
                                    "(Not at all interesting)|(Slightly interesting)|(Moderately interesting)|(Interesting)|(Very interesting)")
  data$QCORR_IN03 <- qcorr_in3
  
  # credibility vars 
  qcorr_cred1 <- stringr::str_extract(data$QCORR_CRED01,
                                      "(Not at all accurate)|(Slightly accurate)|(Moderately accurate)|(Accurate)|(Very accurate)")
  data$QCORR_CRED01 <- qcorr_cred1
  qcorr_cred2 <- stringr::str_extract(data$QCORR_CRED02,
                                      "(Not at all authentic)|(Slightly authentic)|(Moderately authentic)|(Authentic)|(Very authentic)")
  data$QCORR_CRED02 <- qcorr_cred2
  qcorr_cred3 <- stringr::str_extract(data$QCORR_CRED03,
                                      "(Not at all believable)|(Slightly believable)|(Moderately believable)|(Believable)|(Very believable)")
  data$QCORR_CRED03 <- qcorr_cred3
  qcorr_cred4 <- stringr::str_extract(data$QCORR_CRED04,
                                      "(Not at all professional)|(Slightly professional)|(Moderately professional)|(Professional)|(Very professional)")
  data$QCORR_CRED04 <- qcorr_cred4
  
  # tie strength/agree variables
  for(i in c(paste0("0", 1:9), as.character(10:12))) {
    tie_str <- stringr::str_extract(data[[paste0("QTIE_STR", i)]],
                                    "(Strongly disagree)|(Somewhat disagree)|(Neither agree nor disagree)|(Somewhat agree)|(Strongly agree)")
    data[[paste0("QTIE_STR", i)]] <- tie_str
  }
  
  tie_agree <- stringr::str_extract(data$QTIE_AGREE,
                                    "(Strongly disagree)|(Somewhat disagree)|(Neither agree nor disagree)|(Somewhat agree)|(Strongly agree)")
  data$QTIE_AGREE <- tie_agree
  
  # prevalence columns
  PREVALENCE_COLS <- paste0("LOOPCLAIM_PREV_CLAIM_BELIEF_", seq(116), "_QCLAIM_PREV")
  BELIEF_COLS <- paste0("LOOPCLAIM_PREV_CLAIM_BELIEF_", seq(116), "_QCLAIM_BELIEF")
  for(prev_col in PREVALENCE_COLS) {
    prevalence <- stringr::str_extract(data[[prev_col]], "(Yes)|(Maybe)|(No)")
    data[[prev_col]] <- prevalence
  }
  for(belief_col in BELIEF_COLS) {
    belief <- stringr::str_extract(data[[belief_col]],
                                   "(Definitely accurate)|(Probably accurate)|(Not sure if accurate or inaccurate)|(Probably inaccurate)|(Definitely inaccurate)")
    data[[belief_col]] <- belief
  }
  return(data)
}


fix_platform_variables <- function(data) {
  # the order in platforms list corresponds to the order of platform variables in the data
  PLATFORMS <- c("WhatsApp", "Telegram", "Facebook Messenger", "Viber", "Line", "WeChat", "Facebook",
                 "Twitter", "Instagram", "LinkedIn", "Television", "Radio", "Newspaper", "Magazine",
                 "Word-of-mouth", "Other", "Dont know")
  PLATFORM_COLS <- paste0("LOOPCLAIM_PREV_CLAIM_BELIEF_", seq(116), "_QCLAIM_PLAT_")
  for(i in c(seq(15), 98, 99)) {
    cols <- paste0(PLATFORM_COLS, i)
    # replace No  values in the columns of all claims with NA
    data[,cols] <- lapply(data[cols], function(x) replace(x,x == "No", NA))
    # Now replace yes values with corresponding platform
    platform_idx <- ifelse(i < 16, i, ifelse(i==98, 16, 17) )
    platform <- PLATFORMS[platform_idx]
    data[,cols] <- lapply(data[cols], function(x) replace(x,x == "Yes", platform))
  }
  return(data)
}

process_data <- function(data_file) {
  data <- read_excel(data_file)
  data$QATT_SCREEN <- tolower(data$QATT_SCREEN)
  data$pml_tweet <- (data$MRK_DEF_MAIN_1 == "Yes")
  data$ppp_tweet <- (data$MRK_DEF_MAIN_2 == "Yes")
  data$pti_tweet <- (data$MRK_DEF_MAIN_3 == "Yes")
  data <- fix_urdu_variables(data)
  data <- fix_platform_variables(data)
  
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
  
  
