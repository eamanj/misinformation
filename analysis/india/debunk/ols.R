library(ggplot2)
library(ivpack)

setwd("~/research/misinformation/")
source("./analysis/india/debunk/clean_data.R")
source("./analysis/debunk_utils.R")
data <- process_data("./data/india/debunk/Harvard India data file (August 15) - answers displayed as text.xlsx")

# which filtering to use?
df <- exclude_nonattentive(data,
                           no_treatment=TRUE,
                           real_tie=FALSE,
                           search_internet=FALSE,
                           attention_check=TRUE,
                           correct_misinfo=FALSE,
                           correct_correction=FALSE,
                           long_duration=FALSE)

summary(lm(correction_mimshare_numeric ~ tie_treatment, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QPOLI_INTEREST, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QPOLI_ATT, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QACC_NEWS, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_SEE_NEWS, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMISINFO_BELIEF, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS + QMISINFO_BELIEF, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS + income_numeric, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_gender, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + IN01EDU, df))


summary(lm(correction_mimshare_numeric ~ tie_group_treatment, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QPOLI_INTEREST, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QPOLI_ATT, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QACC_NEWS, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QMIM_SEE_NEWS, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QMIM_DISCUSS_NEWS, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QMIM_DISCUSS_NEWS + income_numeric, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_gender, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + IN01EDU, df))

summary(lm(correction_mimshare_numeric ~ tie_treatment + QTIE_POLI_FREQ, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QTIE_POLI_FREQ, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QTIE_POLI_FREQ + QMIM_DISCUSS_NEWS, df))
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QTIE_POLI_FREQ + income_numeric, df)) 
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QTIE_POLI_FREQ + resp_gender, df)) 
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QTIE_POLI_FREQ + resp_age, df)) 
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QTIE_POLI_FREQ + resp_age + education_numeric, df)) 
summary(lm(correction_mimshare_numeric ~ tie_group_treatment + QTIE_POLI_FREQ + resp_age + IN01EDU, df))

#################################
# analysis with collapsed dependent variable into 2 categories
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QPOLI_INTEREST, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QPOLI_ATT, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QACC_NEWS, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_SEE_NEWS, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMISINFO_BELIEF, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + QMISINFO_BELIEF, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + income_numeric, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_gender, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS + resp_age + IN01EDU, df))

summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QPOLI_INTEREST, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QPOLI_ATT, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QACC_NEWS, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_SEE_NEWS, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + income_numeric, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_gender, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + education_numeric, df))
summary(lm(QCORR_MIMSHARE_BINARY ~ tie_group_treatment + QMIM_DISCUSS_NEWS + resp_age + IN01EDU, df))

#################################
# analysis with inferred treatment variables
df <- add_inferred_treatment_variables(df, "median")
df <- add_inferred_treatment_variables(df, "divisive")
df <- add_inferred_treatment_variables(df, "agglomorative")

# Use inferred tie strength for treatment effects
round(prop.table(table(df$inferred_tie_treatment)), 3)
summary(lm(correction_mimshare_numeric ~ inferred_tie_treatment, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_treatment + QPOLI_INTEREST, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_treatment + QPOLI_ATT, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_treatment + QMIM_SEE_NEWS, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_treatment + QMIM_DISCUSS_NEWS, df))
ggplot(df, aes(inferred_tie_treatment, correction_mimshare_numeric, fill = inferred_tie_treatment)) +
  theme_bw() + theme(legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "blue"))


# Use inferred agreement for treatment effects
round(prop.table(table(df$inferred_group_treatment)), 3)
summary(lm(correction_mimshare_numeric ~ inferred_group_treatment, df))
summary(lm(correction_mimshare_numeric ~ inferred_group_treatment + QPOLI_INTEREST, df))
summary(lm(correction_mimshare_numeric ~ inferred_group_treatment + QPOLI_ATT, df))
summary(lm(correction_mimshare_numeric ~ inferred_group_treatment + QMIM_SEE_NEWS, df))
summary(lm(correction_mimshare_numeric ~ inferred_group_treatment + QMIM_DISCUSS_NEWS, df))
ggplot(df, aes(inferred_group_treatment, correction_mimshare_numeric, fill = inferred_group_treatment)) +
  theme_bw() + theme(legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "blue"))

# Use correction Format
round(prop.table(table(df$format_treatment)), 3)
summary(lm(correction_mimshare_numeric ~ format_treatment, df))
summary(lm(correction_mimshare_numeric ~ format_treatment + QPOLI_INTEREST, df))
summary(lm(correction_mimshare_numeric ~ format_treatment + QPOLI_ATT, df))
summary(lm(correction_mimshare_numeric ~ format_treatment + QMIM_SEE_NEWS, df))
summary(lm(correction_mimshare_numeric ~ format_treatment + QMIM_DISCUSS_NEWS, df))
ggplot(df, aes(format_treatment, correction_mimshare_numeric, fill = format_treatment)) +
  theme_bw() + theme(legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "green", "blue"))


# Use inferred strength and agreement
round(prop.table(table(df$inferred_tie_group_treatment)), 3)
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_treatment, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_treatment + QPOLI_INTEREST, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_treatment + QPOLI_ATT, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_treatment + QMIM_SEE_NEWS, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_treatment + QMIM_DISCUSS_NEWS, df))
ggplot(df, aes(inferred_tie_group_treatment, correction_mimshare_numeric, fill = inferred_tie_group_treatment)) +
  theme_bw() + theme(legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "orange", "green", "blue"))


# Use inferred strength and agreement and format
round(prop.table(table(df$inferred_tie_group_format_treatment)), 3)
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_format_treatment, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_format_treatment + QPOLI_INTEREST, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_format_treatment + QPOLI_ATT, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_format_treatment + QMIM_SEE_NEWS, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_group_format_treatment + QMIM_DISCUSS_NEWS, df))
ggplot(df, aes(inferred_tie_agree_format, correction_mimshare_numeric, fill = inferred_tie_agree_format)) +
  theme_bw() + theme(legend.position = "none", axis.text=element_text(size=6)) +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95))


###################################################################
# encouragement design analysis 
# Weak or Strong instrument? compute the F-statistics of treatment instrument on actual treatment.
# Is F-statistic greater than 20?
df <- add_inferred_treatment_variables(df, "median")
df <- add_inferred_treatment_variables(df, "divisive")
df <- add_inferred_treatment_variables(df, "agglomorative")
summary(lm(inferred_tie_treatment_numeric ~ tie_treatment, df))
summary(lm(inferred_tie_treatment_numeric ~ tie_treatment, df))$fstatistic

ACE <- mean(df$correction_mimshare_numeric[df$tie_treatment=="Strong"]) - mean(df$correction_mimshare_numeric[df$tie_treatment=="Weak"])
p_complier <- mean(df$inferred_tie_treatment[df$tie_treatment=="Strong"] == "Strong") - mean(df$inferred_tie_treatment[df$tie_treatment=="Weak"] == "Strong")
ACE/p_complier

ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_tie_treatment | tie_treatment,
                 data = df, x = TRUE)
summary(ivmodel)
ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_group_treatment | group_treatment,
                 data = df, x = TRUE)
summary(ivmodel)
ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_tie_group_treatment | tie_group_treatment,
                 data = df, x = TRUE)
summary(ivmodel)
ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_tie_treatment + QMIM_DISCUSS_NEWS | tie_treatment + QMIM_DISCUSS_NEWS,
                 data = df, x = TRUE)
summary(ivmodel)
ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_group_treatment + QMIM_DISCUSS_NEWS | group_treatment + QMIM_DISCUSS_NEWS,
                 data = df, x = TRUE)
summary(ivmodel)
ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_tie_group_treatment + QMIM_DISCUSS_NEWS | tie_group_treatment + QMIM_DISCUSS_NEWS,
                 data = df, x = TRUE)
summary(ivmodel)

########
# IV regression where instrument is the interaction of treatment with another variable that is correlated with inferred treatment
# This leads to a conditional model where the auxiliary variable has to be included
variables = c("correction_interest1_numeric", "correction_interest2_numeric", "correction_interest3_numeric",
              "correction_credibility1_numeric", "correction_credibility2_numeric", "correction_credibility1_numeric", "correction_credibility4_numeric",
              "correction_interest1_binary_numeric", "correction_interest2_binary_numeric", "correction_interest3_binary_numeric",
              "correction_credibility1_binary_numeric", "correction_credibility2_binary_numeric", "correction_credibility1_binary_numeric", "correction_credibility4_binary_numeric",
              "tie_credibility1_numeric", "tie_credibility2_numeric", "tie_credibility3_numeric",
              "tie_political_freq_numeric", "tie_diff_gender_numeric")
              
# Get the f-statistic of the new IV when the interacted variable is included as control              
idx <- 4
df$new_var <- df$tie_treatment_numeric * df[[variables[idx]]]
mod1 <- lm(paste("inferred_tie_treatment_numeric ~ ", variables[[idx]]), df)
summary(mod1)
mod2 <- lm(paste("inferred_tie_treatment_numeric ~ new_var +", variables[[idx]]), df)
summary(mod2)
waldtest(mod2, mod1)
mod0 <- lm("inferred_tie_treatment_numeric ~ tie_treatment", df)
summary(mod0)

formula <- paste("correction_mimshare_numeric ~ inferred_tie_treatment +", variables[idx], "| new_var +", variables[idx])
ivmodel <- ivreg(formula = formula, data = df, x = TRUE)
summary(ivmodel)
formula <- paste("correction_mimshare_numeric ~ inferred_tie_treatment + QMIM_DISCUSS_NEWS +", variables[idx], "| new_var + QMIM_DISCUSS_NEWS +", variables[idx])
ivmodel <- ivreg(formula = formula, data = df, x = TRUE)
summary(ivmodel)

#######
# IV estimate with weak instrument
conf_level = 0.95
ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_tie_treatment + QMIM_DISCUSS_NEWS | tie_treatment + QMIM_DISCUSS_NEWS,
                 data = df, x = TRUE)
ivmodel <- ivreg(formula = correction_mimshare_numeric ~ inferred_tie_treatment + QMIM_DISCUSS_NEWS + QCORR_CRED01 | tie_treatment + QMIM_DISCUSS_NEWS + QCORR_CRED01,
                 data = df, x = TRUE)
summary(ivmodel)
ivmodel_coef <- ivmodel$coefficients[2]
ivmodel_se = sqrt(diag(vcovHC(ivmodel, "HC3")))[2]
coeftest(ivmodel, vcov = vcovHC(ivmodel, "HC3"))
# construct confidence interval based on model 3
ci_low = ivmodel_coef + qnorm((1-conf_level)/2) * ivmodel_se
ci_high = ivmodel_coef - qnorm((1-conf_level)/2) * ivmodel_se
ivmodel_ci = paste0("(", round(ci_low, 3), " , ", round(ci_high, 3), ")")
ivmodel_ci

# first compute the f-statistic in the first stage
first_stage = (lm(inferred_tie_treatment_numeric ~ tie_treatment, df))
first_stage$coefficients
f_stat = (first_stage$coefficients[2])^2 / vcovHC(first_stage, "HC3")[2,2]
f_stat

# create the grid based on the estimate from previous model
alpha_se = ivmodel_se
alphas = ivmodel_coef + seq(-5*alpha_se, 5*alpha_se, by=alpha_se/20)
#alphas

accepted_alphas = c()
for(alpha in alphas) {
  y = df$correction_mimshare_numeric - alpha*(df$inferred_tie_treatment_numeric)
  x1 = df$tie_treatment_numeric
  x2 = df$QMIM_DISCUSS_NEWS
  x3 = df$QCORR_CRED01
  lmfit = lm(y ~ x1 + x2 + x3)
  summary(lmfit)
  # compute the wald statistic of regressing Expropriation on the instrument and control
  wald = (lmfit$coefficients[2])^2 / vcovHC(lmfit, "HC3")[2,2]
  # check if the wald stat passes the test, if so the alpha is in the confidence set
  if(wald <= qchisq(conf_level, 1))  {
    accepted_alphas = c(accepted_alphas, alpha)
  }
}

# generate the confidence band and the estimated alpha as mid point
ci_low = min(accepted_alphas)
ci_high = max(accepted_alphas)
estimated_alpha = (ci_low + ci_high)/2
robust_iv_se = (ci_high - ci_low) / (2 * qnorm((1 - conf_level)/2, lower.tail = FALSE))
# construct the confidence interval for model4
weak_iv_ci = paste0("(", round(ci_low, 3), " , ", round(ci_high, 3), ")")
weak_iv_ci









control_vars <- c("QMIM_DISCUSS_NEWS", #"QSNS_DISCUSS_NEWS",
                  "QCORR_CRED01", "QCORR_CRED02", "QCORR_CRED03", "QCORR_CRED04",
                  "QCORR_IN01", "QCORR_IN02", "QCORR_IN03",
                  "QMISINFO_BELIEF", "QMISINFO_BELIEF_1",
                  "QPOLI_ATT", "QPOLI_INTEREST", "QACC_NEWS",
                  "QMIM_SEE_NEWS", #"QSNS_SEE_NEWS",
                  "QTIE_CRED01", "QTIE_CRED02", "QTIE_CRED03", "QTIE_CRED03",
                  "QTIE_POLI_FREQ", "QTIE_GEN")
control_vars_combs <- combn(control_vars, 2, simplify=F)
indep_var <- "inferred_group_treatment"
instrument_var <- "group_treatment"
for(real_tie in c(FALSE, TRUE)) {
  for(search_internet in c(FALSE, TRUE)) {
    for(attention_check in c(FALSE, TRUE)) {
      for(correct_misinfo in c(FALSE, TRUE)) {
        for(correct_correction in c(FALSE, TRUE)) {
          for(i in seq(1, length(control_vars_combs))) {
            for(quant in seq(0.1, 0.9, 0.05)) {
              controls <- control_vars_combs[[i]]
              df <- exclude_nonattentive(data,
                                         no_treatment=TRUE,
                                         real_tie=real_tie,
                                         search_internet=search_internet,
                                         attention_check=attention_check,
                                         correct_misinfo=correct_misinfo,
                                         correct_correction=correct_correction,
                                         long_duration=F)
              capture.output(df <- add_inferred_treatment_variables(df, quant))
              formula <- paste0("correction_mimshare_numeric ~ ", indep_var, " + ", paste(controls, collapse=" + "),
                                " | ", instrument_var, " + ", paste(controls, collapse=" + "))
              ivmodel <- ivreg(formula = formula, data = df, x = TRUE)
              ivmodel_sum <- summary(ivmodel)
              ivmodel_coef <- ivmodel_sum$coefficients[2,1]
              ivmodel_pval <- ivmodel_sum$coefficients[2,4]
              if(ivmodel_coef < 0 & ivmodel_pval < 0.005) {
                cat(paste("real_tie:", real_tie, "search_internet:", search_internet,
                          "attention_check:", attention_check, "correct_misinfo", correct_misinfo,
                          "correct_correction:", correct_correction, "quantile:", quant,
                          "controls:", paste(controls, collapse=" "), "\n"))
                print(ivmodel_sum$coefficients[2,])
                cat('*********************\n')
              }
            }
          }
        }
      }
    }
  }
}
