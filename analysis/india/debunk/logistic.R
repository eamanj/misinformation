library(ggplot2)
library(MASS)

setwd("~/research/misinformation/")
source("./analysis/india/debunk/clean_data.R")
source("./analysis/debunk_utils.R")
data <- process_data("./data/india/debunk/Harvard India data file (August 15) - answers displayed as text.xlsx")

# which filtering to use?
df <- data

# those who were actually shown a correction from a tie
df <- data[data$format_treatment != "None",]

df <- data[data$QATT_SCREEN == 'puce',]
df <- data[data$QATT_SCREEN == 'puce' & data$format_treatment != "None",]
df <- data[data$QATT_SCREEN == 'puce' & data$QIND_SCREEN=="Yes",]
df <- data[data$QATT_SCREEN == 'puce' & data$QIND_SCREEN=="Yes" & data$QSEARCH_SCREEN=="No",]
df <- data[data$QATT_SCREEN == 'puce' & data$correct_corr_manip & !is.na(data$correct_corr_manip),]
df <- data[data$QATT_SCREEN == 'puce' & data$correct_misinfo_manip & !is.na(data$correct_misinfo_manip),]
df <- data[data$QATT_SCREEN == 'puce' & data$correct_corr_manip & !is.na(data$correct_corr_manip) &
           data$correct_misinfo_manip & !is.na(data$correct_misinfo_manip),]

df <- data[data$correct_misinfo_manip & !is.na(data$correct_misinfo_manip),]
df <- data[data$correct_misinfo_manip & !is.na(data$correct_misinfo_manip) & data$QIND_SCREEN=="Yes",]
df <- data[data$correct_misinfo_manip & !is.na(data$correct_misinfo_manip) & data$QIND_SCREEN=="Yes" & data$QSEARCH_SCREEN=="No",]

df <- data[data$correct_corr_manip & !is.na(data$correct_corr_manip),] # BEST DATA
df <- data[data$correct_corr_manip & !is.na(data$correct_corr_manip) & data$QIND_SCREEN=="Yes",] # BEST DATA
df <- data[data$correct_corr_manip & !is.na(data$correct_corr_manip) & data$QIND_SCREEN=="Yes" & data$QSEARCH_SCREEN=="No",]

df <- data[data$correct_misinfo_manip & data$correct_corr_manip &
           !is.na(data$correct_misinfo_manip) & !is.na(data$correct_corr_manip),]
df <- data[data$correct_misinfo_manip & data$correct_corr_manip & data$QIND_SCREEN=="Yes" &
           !is.na(data$correct_misinfo_manip) & !is.na(data$correct_corr_manip),]
df <- data[data$correct_misinfo_manip & data$correct_corr_manip & data$QIND_SCREEN=="Yes" & data$QSEARCH_SCREEN=="No" &
           !is.na(data$correct_misinfo_manip) & !is.na(data$correct_corr_manip),]

print_summary_table <- function(polr_fit) {
  ctable <- coef(summary(polr_fit))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- cbind(ctable, "p value" = p)
  return(round(ctable, 4))
}


print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QPOLI_INTEREST, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QPOLI_ATT, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QACC_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_SEE_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMISINFO_BELIEF, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + misinfo_belief_numeric, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_DISCUSS_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + mim_discuss_news_numeric, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_DISCUSS_NEWS + QMISINFO_BELIEF, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_DISCUSS_NEWS + income_numeric, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_gender, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + IN01EDU, df, Hess=TRUE))

print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QPOLI_INTEREST, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QPOLI_ATT, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QACC_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMIM_SEE_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMISINFO_BELIEF, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + misinfo_belief_numeric, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMIM_DISCUSS_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + mim_discuss_news_numeric, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMIM_DISCUSS_NEWS + income_numeric, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_gender, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + IN01EDU, df, Hess=TRUE))


#################################
# Same analysis but with binary DV
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QPOLI_INTEREST, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QPOLI_ATT, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QACC_NEWS, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_SEE_NEWS, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMISINFO_BELIEF, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + misinfo_belief_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + mim_discuss_news_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + QMISINFO_BELIEF, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + income_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_gender, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + IN01EDU, df, family="binomial"))

summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QPOLI_INTEREST, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QPOLI_ATT, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QACC_NEWS, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_SEE_NEWS, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMISINFO_BELIEF, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + misinfo_belief_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + mim_discuss_news_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + QMISINFO_BELIEF, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + income_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_gender, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df, family="binomial"))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + IN01EDU, df, family="binomial"))

#################################

# analysis with inferred treatment variables
df <- add_inferred_treatment_variables(df, "median")
df <- add_inferred_treatment_variables(df, "divisive")
df <- add_inferred_treatment_variables(df, "agglomorative")

# Use inferred tie strength for treatment effects
round(prop.table(table(df$inferred_tie_treatment)), 3)
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_treatment, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_treatment + QPOLI_INTEREST, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_treatment + QPOLI_ATT, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_treatment + QMIM_SEE_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_treatment + QMIM_DISCUSS_NEWS, df, Hess=TRUE))
ggplot(df, aes(inferred_tie_treatment, correction_mimshare_numeric, fill = inferred_tie_treatment)) +
  theme_bw() + theme(legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "blue"))


# Use inferred agreement for treatment effects
round(prop.table(table(df$inferred_group_treatment)), 3)
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_group_treatment, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_group_treatment + QPOLI_INTEREST, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_group_treatment + QPOLI_ATT, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_group_treatment + QMIM_SEE_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_group_treatment + QMIM_DISCUSS_NEWS, df, Hess=TRUE))
ggplot(df, aes(inferred_group_treatment, correction_mimshare_numeric, fill = inferred_group_treatment)) +
  theme_bw() + theme(legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "blue"))

# Use correction Format
round(prop.table(table(df$format_treatment)), 3)
print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment + QPOLI_INTEREST, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment + QPOLI_ATT, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment + QMIM_SEE_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment + QMIM_DISCUSS_NEWS, df, Hess=TRUE))
ggplot(df, aes(format_treatment, correction_mimshare_numeric, fill = format_treatment)) +
  theme_bw() + theme(legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "green", "blue"))


# Use inferred strength and agreement
round(prop.table(table(df$inferred_tie_group_treatment)), 3)
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_treatment, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_treatment + QPOLI_INTEREST, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_treatment + QPOLI_ATT, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_treatment + QMIM_SEE_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_treatment + QMIM_DISCUSS_NEWS, df, Hess=TRUE))
ggplot(df, aes(inferred_tie_group_treatment, correction_mimshare_numeric, fill = inferred_tie_group_treatment)) +
  theme_bw() + theme(legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "orange", "green", "blue"))


# Use inferred strength and agreement and format
round(prop.table(table(df$inferred_tie_group_format_treatment)), 3)
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_format_treatment, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_format_treatment + QPOLI_INTEREST, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_format_treatment + QPOLI_ATT, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_format_treatment + QMIM_SEE_NEWS, df, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ inferred_tie_group_format_treatment + QMIM_DISCUSS_NEWS, df, Hess=TRUE))
ggplot(df, aes(inferred_tie_group_format_treatment, correction_mimshare_numeric, fill = inferred_tie_group_format_treatment)) +
  theme_bw() + theme(legend.position = "none", axis.text=element_text(size=6)) +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95))