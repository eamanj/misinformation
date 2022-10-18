library(ggplot2)
library(gridExtra)
library(dplyr)

setwd("~/research/misinformation/")
source("./analysis/india/debunk/clean_data.R")
source("./analysis/debunk_utils.R")
data <- process_data("./data/india/debunk/Harvard India data file (August 15) - answers displayed as text.xlsx")

# write the contacts info of participants who are OK with contact to csv
colnames(data)[grepl("QSEG", colnames(data))]
df <- data[,colnames(data)[grepl("QSEG", colnames(data))]]
df <- df[is.na(df$QSEG_PART),]
table(df$QSEG_EMAIL_Codes, useNA = "ifany")
table(df$QSEG_PHONE_Codes, useNA = "ifany")
df$QSEG_EMAIL_Codes <- NULL
df$QSEG_PHONE_Codes <- NULL
df$QSEG_PART <- NULL
df <- df[!is.na(df$QSEG_FNAME),]
df <- df %>% rename(First_Name = QSEG_FNAME, Last_Name = QSEG_LNAME, Email = QSEG_EMAIL, Email_again = QSEG_EMAIL_1,
                    Phone_number = QSEG_PHONE, Spoken_languages = QSEG_LANG)
write.csv(df, file="./results/india/debunk/contacts.csv", row.names = FALSE)


print_attentive_stats(data)
# which filtering to use?
df <- exclude_nonattentive(data,
                           no_treatment=TRUE,
                           real_tie=FALSE,
                           search_internet=FALSE,
                           attention_check=TRUE,
                           correct_misinfo=FALSE,
                           correct_correction=FALSE,
                           long_duration=FALSE)

table(df$QMISINFO_MANIP)
table(df$QCORR_MANIP)

table(df$modi_tweet, useNA="ifany")
table(df$gandhi_tweet, useNA="ifany")

prop.table(table(df$modi_tweet, useNA="ifany"))
prop.table(table(df$gandhi_tweet, useNA="ifany"))

table(df$correct_misinfo_manip, useNA="ifany")                          
table(df$correct_corr_manip, useNA="ifany")
table(df$correct_corr_manip, df$correct_misinfo_manip)

prop.table(table(df$correct_misinfo_manip, useNA="ifany"))
prop.table(table(df$correct_corr_manip, useNA="ifany"))
prop.table(table(df$correct_corr_manip, df$correct_misinfo_manip, useNA="ifany"))

# fraction of those who got the attention check correct
table(df$QATT_SCREEN == 'puce')
prop.table(table(df$QATT_SCREEN == 'puce'))

# interest in politics
table(df$QPOLI_INTEREST)
table(df$QPOLI_ATT)
table(df$QACC_NEWS)
table(df$QMIM_SEE_NEWS)
table(df$QMIM_DISCUSS_NEWS)
table(df$QSNS_SEE_NEWS)
table(df$QSNS_DISCUSS_NEWS)

prop.table(table(df$QPOLI_INTEREST))
prop.table(table(df$QPOLI_ATT))
prop.table(table(df$QACC_NEWS))
prop.table(table(df$QMIM_SEE_NEWS))
prop.table(table(df$QMIM_DISCUSS_NEWS))
prop.table(table(df$QSNS_SEE_NEWS))
prop.table(table(df$QSNS_DISCUSS_NEWS))

# attention to correction
table(df$QCORR_IN01, useNA="ifany")
table(df$QCORR_IN02, useNA="ifany")
table(df$QCORR_IN03, useNA="ifany")
table(df$QCORR_IN01, df$QCORR_IN02, useNA="ifany")

################################################################################################
# How well self-reported tie strength match against tie strength treatment? fraction of compliers
df <- add_inferred_treatment_variables(df, "median")

df$strength_complier <- (df$inferred_tie_treatment == df$tie_treatment)
round(prop.table(table(df$tie_treatment, df$inferred_tie_treatment, dnn=c("Treatment", "Self_reported")), margin=1), 3)
round(prop.table(table(df$strength_complier)), 3)

# How well self-reported tie agreement match against tie agreement treatment? fraction of compliers
df$agree_complier <- (df$inferred_group_treatment == df$group_treatment)
round(prop.table(table(df$group_treatment, df$inferred_group_treatment, dnn=c("Treatment", "Self_reported")), margin=1), 3)
round(prop.table(table(df$agree_complier)), 3)

table(df$tie_treatment, useNA="ifany")
prop.table(table(df$tie_treatment, useNA="ifany"))
table(df$tie_group_treatment, useNA="ifany")
prop.table(table(df$tie_group_treatment, useNA="ifany"))

strong_tie_df <- df[df$tie_treatment == "Strong",]
weak_tie_df <- df[df$tie_treatment == "Weak",]
strong_tie_ingroup_df <- df[df$tie_group_treatment == "Strong-Ingroup",]
strong_tie_outgroup_df <- df[df$tie_group_treatment == "Strong-Outgroup",]
weak_tie_ingroup_df <- df[df$tie_group_treatment == "Weak-Ingroup",]
weak_tie_outgroup_df <- df[df$tie_group_treatment == "Weak-Outgroup",]

# plot the Correction sharing among all ties/groups
ggplot(df, aes(x=QCORR_MIMSHARE)) + geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Correction MIM Share") + ylab("Fraction") +
  ylim(0, 0.27) +
  scale_x_discrete(limits=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"))

# plot sharing correction on MIM by the strength of the tie
p1 <- ggplot(strong_tie_df, aes(x=QCORR_MIMSHARE)) + geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Correction MIM Share") + ylab("Fraction") + ggtitle("MIMShare Strong Ties") +
  ylim(0, 0.33) +
  scale_x_discrete(limits=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"))
p2 <- ggplot(weak_tie_df, aes(x=QCORR_MIMSHARE)) + geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Correction MIM Share") + ylab("Fraction") + ggtitle("MIMShare Weak Ties") +
  ylim(0, 0.33) +
  scale_x_discrete(limits=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"))
p <- grid.arrange(p1, p2, ncol=2)
ggsave("./results/tie_strength_mimshare.png", plot <- p, device = "png",
       width = 12, height = 6, units = "in")


# plot sharing correction on MIM by the strength and group of the tie
p1 <- ggplot(strong_tie_ingroup_df, aes(x=QCORR_MIMSHARE)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Correction MIM Share") + ylab("Fraction") + ggtitle("MIMShare Strong Ingroup Ties") +
  ylim(0, 0.4) +
  scale_x_discrete(limits=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"))
p2 <- ggplot(strong_tie_outgroup_df, aes(x=QCORR_MIMSHARE)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Correction MIM Share") + ylab("Fraction") + ggtitle("MIMShare Strong Outgroup Ties") +
  ylim(0, 0.4) +
  scale_x_discrete(limits=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"))
p3 <- ggplot(weak_tie_ingroup_df, aes(x=QCORR_MIMSHARE)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Correction MIM Share") + ylab("Fraction") + ggtitle("MIMShare Weak Ingroup Ties") +
  ylim(0, 0.4) +
  scale_x_discrete(limits=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"))
p4 <- ggplot(weak_tie_outgroup_df, aes(x=QCORR_MIMSHARE)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Correction MIM Share") + ylab("Fraction") + ggtitle("MIMShare Weak Outgroup Ties") +
  ylim(0, 0.4) +
  scale_x_discrete(limits=c("Not at all likely", "Slightly likely", "Moderately likely", "Likely", "Very likely"))
p <- grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
ggsave("./results/tie_strength_group_mimshare.png", plot <- p, device = "png",
       width = 12, height = 12, units = "in")


# chi-squared test of strong vs weak
table(df$QCORR_MIMSHARE, df$tie_treatment)
chisq.test(df$tie_treatment, df$QCORR_MIMSHARE)
table(df$QCORR_SNSSHARE, df$tie_treatment)
chisq.test(df$tie_treatment, df$QCORR_SNSSHARE)

# chi-squared test of strong-ingroup vs weak-outgroup
df_test <- df[df$tie_group_treatment %in% c("Strong-Ingroup", "Weak-Outgroup"),]
#df_test <- df[df$tie_group_treatment %in% c("Strong-Ingroup", "Strong-Outgroup"),]
#df_test <- df[df$tie_group_treatment %in% c("Strong-Ingroup", "Weak-Ingroup"),]
table(df_test$QCORR_MIMSHARE, df_test$tie_group_treatment)
chisq.test(df_test$tie_group_treatment, df_test$QCORR_MIMSHARE)
table(df_test$QCORR_SNSSHARE, df_test$tie_group_treatment)
chisq.test(df_test$tie_group_treatment, df_test$QCORR_SNSSHARE)

# plot duration to finish survey between those who get attention check right or wrong
max_duration <- 120
correct_attention_check_df <- df[df$correct_attention_check==TRUE,]
wrong_attention_check_df <- df[df$correct_attention_check==FALSE,]
correct_attention_check_df <- correct_attention_check_df[correct_attention_check_df$survey_duration_mins < max_duration,]
wrong_attention_check_df <- wrong_attention_check_df[wrong_attention_check_df$survey_duration_mins < max_duration,]
p1 <- ggplot(correct_attention_check_df, aes(x=survey_duration_mins)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) + 
  ylab("Fraction") + ggtitle("Correct Attention Check") +
  xlim(0,max_duration) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), fill="red") +
  geom_vline(aes(xintercept=median(survey_duration_mins)), linetype="dashed", color = "black") +
  geom_vline(aes(xintercept=mean(survey_duration_mins)), linetype="solid", color = "black")

p2 <- ggplot(wrong_attention_check_df, aes(x=survey_duration_mins)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  ylab("Fraction") + ggtitle("Wrong Attention Check") +
  xlim(0,max_duration) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), fill="blue") +
  geom_vline(aes(xintercept=median(survey_duration_mins)), linetype="dashed", color = "black") +
  geom_vline(aes(xintercept=mean(survey_duration_mins)), linetype="solid", color = "black")
p <- grid.arrange(p1, p2, nrow=1, ncol=2)

t.test(correct_attention_check_df$survey_duration_mins,
       wrong_attention_check_df$survey_duration_mins)

# print list of numerical columns that are highly correlated with self-reported tie strength or agreement
cat(paste('correlation between assigned treatment and actual inferred treatment:',
          round(cor(df$inferred_tie_treatment_numeric, df$tie_treatment_numeric), 3)))
numerical_cols <- which(sapply(df, is.numeric))
threshold <- 0.2
for(col_id in numerical_cols) {
  col_name <- colnames(df)[col_id]
  #res <- cor(df$inferred_tie_treatment_numeric, df[,col_id])
  res <- cor(df$inferred_tie_treatment_numeric, df$tie_treatment_numeric*df[,col_id])
  if(!grepl("tie_strength", col_name) &
     !grepl("tie_agree", col_name) &
     !grepl("inferred", col_name) &
     !is.na(res) & abs(res) > threshold) {
    print(paste0(col_name, ": ", round(res[[1]], 3)))
  }
}