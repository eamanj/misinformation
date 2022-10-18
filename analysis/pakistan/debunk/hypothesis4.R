library(ggplot2)
library(MASS)
library(dplyr)
library(ivpack)
library(stargazer)

setwd("~/research/misinformation/")
source("./analysis/pakistan/debunk/clean_data.R")
source("./analysis/debunk_utils.R")
data <- process_data("./data/pakistan/debunk/Pakistan results – wave one - answers displayed as text.xlsx")


# which filtering to use?
df <- exclude_nonattentive(data,
                           no_treatment=TRUE,
                           real_tie=TRUE,
                           search_internet=FALSE,
                           attention_check=FALSE,
                           correct_misinfo=FALSE,
                           correct_correction=FALSE,
                           long_duration=TRUE)

# if belief moves toward inaccuracy of the claim, it is deemed as higher
df$belief_change <- ifelse(df$QMISINFO_BELIEF02 > df$QMISINFO_BELIEF01, "Higher",
                           ifelse(df$QMISINFO_BELIEF02 < df$QMISINFO_BELIEF01, "Lower",
                                  "Unchanged"))
df$belief_change <- factor(df$belief_change, levels=c("Lower", "Unchanged", "Higher"), ordered=TRUE)
# belief change with degrees of change as numeric
df$belief_change_numeric <- (df$misinfo_belief_2_numeric - df$misinfo_belief_1_numeric)
# belief change with  degrees of change as factor
df$belief_change_factor <- factor(df$belief_change_numeric,
                                  levels=c(-4,-3,-2,-1,0,1,2,3,4), ordered=TRUE)

print_summary_table <- function(polr_fit) {
  ctable <- coef(summary(polr_fit))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- cbind(ctable, "p value" = p)
  return(round(ctable, 4))
}



############################################
# Hypothesis 4: belief change vs. strength of tie
test_res <- chisq.test(df$tie_treatment, df$belief_change)
summary_stats <- df[,c("tie_treatment", "belief_change")] %>% group_by(tie_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(tie_treatment, belief_change) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=belief_change, y=freq, fill=tie_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))

df_test <- df
df_test <- droplevels(df_test)
print_summary_table(polr(belief_change ~ tie_treatment, df_test, Hess=TRUE))
#print_summary_table(polr(belief_change ~ tie_treatment + correct_corr_manip + real_tie_name, df_test, Hess=TRUE))
print_summary_table(polr(belief_change ~ tie_treatment + QPOLI_INTEREST, df_test, Hess=TRUE))
print_summary_table(polr(belief_change ~ tie_treatment + QPOLI_ATT, df_test, Hess=TRUE))
print_summary_table(polr(belief_change ~ tie_treatment + QACC_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(belief_change ~ tie_treatment + QMIM_SEE_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(belief_change ~ tie_treatment + QCORR_CRED01, df_test, Hess=TRUE))
print_summary_table(polr(belief_change ~ tie_treatment + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(belief_change ~ tie_treatment + QCORR_CRED01 + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
# interaction terms
print_summary_table(polr(belief_change ~ tie_treatment*QCORR_CRED01, df_test, Hess=TRUE))
print_summary_table(polr(belief_change ~ tie_treatment*QCORR_CRED01 + QPOLI_INTEREST, df_test, Hess=TRUE))
####
test_res <- summary(aov(belief_change_numeric ~ tie_treatment, df))
ggplot(df, aes(tie_treatment, belief_change_numeric, fill = tie_treatment)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_se) +
  ggtitle(paste('ANOVA p-val:', round(test_res[[1]]$`Pr(>F)`[1], 4)))

df_test <- df
df_test <- droplevels(df_test)
summary(lm(belief_change_numeric ~ tie_treatment, df_test))
summary(lm(belief_change_numeric ~ tie_treatment + QPOLI_INTEREST, df_test))
summary(lm(belief_change_numeric ~ tie_treatment + QPOLI_ATT, df_test))
summary(lm(belief_change_numeric ~ tie_treatment + QACC_NEWS, df_test))
summary(lm(belief_change_numeric ~ tie_treatment + QMIM_SEE_NEWS, df_test))
summary(lm(belief_change_numeric ~ tie_treatment + QCORR_CRED01, df_test))
summary(lm(belief_change_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS, df_test))
summary(lm(belief_change_numeric ~ tie_treatment + QCORR_CRED01 + QMIM_DISCUSS_NEWS, df_test))
# interaction terms
summary(lm(belief_change_numeric ~ tie_treatment*QCORR_CRED01, df_test))
summary(lm(belief_change_numeric ~ tie_treatment*QCORR_CRED01 + QPOLI_INTEREST, df_test))

####
test_res <- chisq.test(df$tie_treatment, df$belief_change_factor)
summary_stats <- df[,c("tie_treatment", "belief_change_factor")] %>% group_by(tie_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(tie_treatment, belief_change_factor) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=belief_change_factor, y=freq, fill=tie_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))


df_test <- df
df_test <- droplevels(df_test)
print_summary_table(polr(belief_change_factor ~ tie_treatment, df_test, Hess=TRUE))
print_summary_table(polr(belief_change_factor ~ tie_treatment + QPOLI_INTEREST, df_test, Hess=TRUE))
print_summary_table(polr(belief_change_factor ~ tie_treatment + QPOLI_ATT, df_test, Hess=TRUE))
print_summary_table(polr(belief_change_factor ~ tie_treatment + QACC_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(belief_change_factor ~ tie_treatment + QMIM_SEE_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(belief_change_factor ~ tie_treatment + QCORR_CRED01, df_test, Hess=TRUE))
print_summary_table(polr(belief_change_factor ~ tie_treatment + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(belief_change_factor ~ tie_treatment + QCORR_CRED01 + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
# interaction terms
print_summary_table(polr(belief_change_factor ~ tie_treatment*QCORR_IN01, df_test, Hess=TRUE))
print_summary_table(polr(belief_change_factor ~ tie_treatment*QCORR_IN01 + QPOLI_INTEREST, df_test, Hess=TRUE))


############################################
# Hypothesis 4: intention to share on MIM vs. strength of tie
test_res <- chisq.test(df$tie_treatment, df$QCORR_MIMSHARE)
summary_stats <- df[,c("tie_treatment", "QCORR_MIMSHARE")] %>%
  group_by(tie_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(tie_treatment, QCORR_MIMSHARE) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_MIMSHARE, y=freq, fill=tie_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))
ggplot(df, aes(x=tie_treatment, fill=QCORR_MIMSHARE)) +
  geom_bar(position='fill') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Within group Fraction") +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))

test_res <- chisq.test(df$tie_group_treatment, df$QCORR_MIMSHARE)
summary_stats <- df[,c("tie_group_treatment", "QCORR_MIMSHARE")] %>%
  group_by(tie_group_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(tie_group_treatment, QCORR_MIMSHARE) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_MIMSHARE, y=freq, fill=tie_group_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))
ggplot(df, aes(x=tie_group_treatment, fill=QCORR_MIMSHARE)) +
  geom_bar(position='fill') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Within group Fraction") +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))


# tie strength interacted with first measure of message interest
summary_stats <- df[,c("tie_treatment", "QCORR_IN01", "QCORR_MIMSHARE")]
summary_stats$tie_interest01 <- paste(summary_stats$tie_treatment,
                                      summary_stats$QCORR_IN01, sep='-')
summary_stats$tie_interest01 <- factor(summary_stats$tie_interest01,
                                       levels=c('Strong-None at all', 'Weak-None at all',
                                                'Strong-A little', 'Weak-A little',
                                                'Strong-A moderate amount', 'Weak-A moderate amount',
                                                'Strong-A good amount', 'Weak-A good amount',
                                                'Strong-A great deal', 'Weak-A great deal'),
                                       ordered=TRUE)
ggplot(summary_stats, aes(x=tie_interest01, fill=QCORR_MIMSHARE)) +
  geom_bar(position='fill') +
  theme(axis.text.x = element_text(angle = 90, size=8)) +
  ylab("Within group Fraction")

# tie strength interacted with first measure of message credibility
summary_stats <- df[,c("tie_treatment", "QCORR_CRED01", "QCORR_MIMSHARE")]
summary_stats$tie_credibility01 <- paste(summary_stats$tie_treatment,
                                         summary_stats$QCORR_CRED01, sep='-')
summary_stats$tie_credibility01 <- factor(summary_stats$tie_credibility01,
                                          levels=c("Strong-Not at all accurate", "Weak-Not at all accurate",
                                                   "Strong-Slightly accurate", "Weak-Slightly accurate",
                                                   "Strong-Moderately accurate", "Weak-Moderately accurate",
                                                   "Strong-Accurate", "Weak-Accurate",
                                                   "Strong-Very accurate", "Weak-Very accurate"),
                                          ordered=TRUE)
ggplot(summary_stats, aes(x=tie_credibility01, fill=QCORR_MIMSHARE)) +
  geom_bar(position='fill') +
  theme(axis.text.x = element_text(angle = 90, size=8)) +
  ylab("Within group Fraction")

# tie strength interacted with second measure of message credibility
summary_stats <- df[,c("tie_treatment", "QCORR_CRED02", "QCORR_MIMSHARE")]
summary_stats$tie_credibility02 <- paste(summary_stats$tie_treatment,
                                         summary_stats$QCORR_CRED02, sep='-')
summary_stats$tie_credibility02 <- factor(summary_stats$tie_credibility02,
                                          levels=c("Strong-Not at all authentic", "Weak-Not at all authentic",
                                                   "Strong-Slightly authentic", "Weak-Slightly authentic",
                                                   "Strong-Moderately authentic", "Weak-Moderately authentic",
                                                   "Strong-Authentic", "Weak-Authentic",
                                                   "Strong-Very authentic", "Weak-Very authentic"),
                                          ordered=TRUE)
ggplot(summary_stats, aes(x=tie_credibility02, fill=QCORR_MIMSHARE)) +
  geom_bar(position='fill') +
  theme(axis.text.x = element_text(angle = 90, size=8)) +
  ylab("Within group Fraction")

# tie strength interacted with third measure of message credibility
summary_stats <- df[,c("tie_treatment", "QCORR_CRED03", "QCORR_MIMSHARE")]
summary_stats$tie_credibility03 <- paste(summary_stats$tie_treatment,
                                         summary_stats$QCORR_CRED03, sep='-')
summary_stats$tie_credibility03 <- factor(summary_stats$tie_credibility03,
                                          levels=c("Strong-Not at all believable", "Weak-Not at all believable",
                                                   "Strong-Slightly believable", "Weak-Slightly believable",
                                                   "Strong-Moderately believable", "Weak-Moderately believable",
                                                   "Strong-Believable", "Weak-Believable",
                                                   "Strong-Very believable", "Weak-Very believable"),
                                          ordered=TRUE)
ggplot(summary_stats, aes(x=tie_credibility03, fill=QCORR_MIMSHARE)) +
  geom_bar(position='fill') +
  theme(axis.text.x = element_text(angle = 90, size=8)) +
  ylab("Within group Fraction")


df_test <- df
df_test <- droplevels(df_test)
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QPOLI_INTEREST, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QPOLI_ATT, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QACC_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_SEE_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QCORR_IN01, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QCORR_IN01 + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QCORR_IN02, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QCORR_IN02 + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QCORR_IN03, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QCORR_IN03 + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QCORR_CRED01, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QCORR_CRED01 + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QCORR_CRED02, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QCORR_CRED02 + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QCORR_CRED03, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment + QCORR_CRED03 + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
# interaction terms
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_group_treatment + QCORR_CRED01 + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment*group_treatment, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment*QCORR_IN01, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment*QCORR_IN01 + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment*QCORR_CRED01, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ tie_treatment*QCORR_CRED01 + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))

####
# numeric measure
test_res <- summary(aov(correction_mimshare_numeric ~ tie_treatment, df))
ggplot(df, aes(tie_treatment, correction_mimshare_numeric, fill = tie_treatment)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_se) +
  ggtitle(paste('ANOVA p-val:', round(test_res[[1]]$`Pr(>F)`[1], 4)))
test_res <- summary(lm(correction_mimshare_numeric ~ tie_treatment + QCORR_CRED01, df))
ggplot(df, aes(QCORR_CRED01, correction_mimshare_numeric, fill = tie_treatment)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(geom = "bar", fun.y = mean, position='dodge') +
  stat_summary(geom = "errorbar", fun.data = mean_se, position='dodge') +
  ggtitle(paste('ANCOVA p-val:', round(test_res$coefficients[[2,4]], 4))) +
  labs(x="Perceived Correction Credibility", y="Numeric Sharing Correction", fill="Tie\nTreatment") +
  scale_x_discrete(labels=c("Not at all accurate"="Not at all\naccurate",
                            "Slightly accurate"="Slightly\naccurate",
                            "Moderately accurate"="Moderately\naccurate",
                            "Accurate"="Accurate",
                            "Very accurate"="Very\naccurate")) +
  theme(plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18), 
        legend.text = element_text(size=16))
ggsave("./results/pakistan/debunk/tie_strength_intention_to_share.png",  width = 10, height = 6)

test_res <- summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS, df))
ggplot(df, aes(QMIM_DISCUSS_NEWS, correction_mimshare_numeric, fill = tie_treatment)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(geom = "bar", fun.y = mean, position='dodge') +
  stat_summary(geom = "errorbar", fun.data = mean_se, position='dodge') +
  ggtitle(paste('ANCOVA p-val:', round(test_res$coefficients[[2,4]], 4)))

df_test <- df
df_test <- droplevels(df_test)
summary(lm(correction_mimshare_numeric ~ tie_treatment, df_test))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QPOLI_INTEREST, df_test))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QPOLI_ATT, df_test))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QACC_NEWS, df_test))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_SEE_NEWS, df_test))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QCORR_CRED01, df_test))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QMIM_DISCUSS_NEWS, df_test))
summary(lm(correction_mimshare_numeric ~ tie_treatment + QCORR_CRED01 + QMIM_DISCUSS_NEWS, df_test))
# interaction terms
summary(lm(correction_mimshare_numeric ~ tie_treatment*QCORR_CRED01, df_test))
summary(lm(correction_mimshare_numeric ~ tie_treatment*QCORR_CRED01 + QMIM_DISCUSS_NEWS, df_test))

####
# Binary measure
test_res <- chisq.test(df$tie_treatment, df$QCORR_MIMSHARE_BINARY)
summary_stats <- df[,c("tie_treatment", "QCORR_MIMSHARE_BINARY")] %>% group_by(tie_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(tie_treatment, QCORR_MIMSHARE_BINARY) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_MIMSHARE_BINARY, y=freq, fill=tie_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))
ggplot(df, aes(x=tie_treatment, fill=QCORR_MIMSHARE_BINARY)) +
  geom_bar(position='fill') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Within group Fraction") +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))

test_res <- summary(aov(QCORR_MIMSHARE_BINARY ~ tie_treatment, df))
ggplot(df, aes(tie_treatment, as.integer(QCORR_MIMSHARE_BINARY), fill = tie_treatment)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_se) +
  ggtitle(paste('ANOVA p-val:', round(test_res[[1]]$`Pr(>F)`[1], 4)))
test_res <- summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QCORR_CRED01, df))
ggplot(df, aes(QCORR_CRED01, as.integer(QCORR_MIMSHARE_BINARY), fill = tie_treatment)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(geom = "bar", fun.y = mean, position='dodge') +
  stat_summary(geom = "errorbar", fun.data = mean_se, position='dodge') +
  ggtitle(paste('ANCOVA p-val:', round(test_res$coefficients[[2,4]], 4)))
test_res <- summary(lm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS, df))
ggplot(df, aes(QMIM_DISCUSS_NEWS, as.integer(QCORR_MIMSHARE_BINARY), fill = tie_treatment)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(geom = "bar", fun.y = mean, position='dodge') +
  stat_summary(geom = "errorbar", fun.data = mean_se, position='dodge') +
  ggtitle(paste('ANCOVA p-val:', round(test_res$coefficients[[2,4]], 4)))

df_test <- df
df_test <- droplevels(df_test)
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QPOLI_INTEREST, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QPOLI_ATT, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QACC_NEWS, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_SEE_NEWS, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QCORR_CRED01, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QMIM_DISCUSS_NEWS, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment + QCORR_CRED01 + QMIM_DISCUSS_NEWS, df_test, family='binomial'))
# interaction terms
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment*QCORR_CRED01, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ tie_treatment*QCORR_CRED01 + QMIM_DISCUSS_NEWS, df_test, family='binomial'))


####
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

summary(lm(correction_mimshare_numeric ~ inferred_tie_treatment, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_treatment + QPOLI_INTEREST, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_treatment + QPOLI_ATT, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_treatment + QMIM_SEE_NEWS, df))
summary(lm(correction_mimshare_numeric ~ inferred_tie_treatment + QMIM_DISCUSS_NEWS, df))

summary(glm(QCORR_MIMSHARE_BINARY ~ inferred_tie_treatment, df, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ inferred_tie_treatment + QPOLI_INTEREST, df, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ inferred_tie_treatment + QPOLI_ATT, df, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ inferred_tie_treatment + QMIM_SEE_NEWS, df, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ inferred_tie_treatment + QMIM_DISCUSS_NEWS, df, family='binomial'))

# numeric measure
test_res <- summary(aov(correction_mimshare_numeric ~ inferred_tie_treatment, df))
ggplot(df, aes(inferred_tie_treatment, correction_mimshare_numeric, fill = inferred_tie_treatment)) +
  theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  coord_cartesian(ylim=c(0,2)) +
  scale_y_continuous(breaks=c(0,1,2)) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(x="Self-reported Tie Strength", y="Numeric Correction Sharing") +
  ggtitle(paste('ANOVA p-val:', round(test_res[[1]]$`Pr(>F)`[1], 6))) +
  theme(plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18), 
        legend.text = element_text(size=16))
ggplot(df, aes(inferred_tie_treatment, correction_mimshare_numeric, fill = inferred_tie_treatment)) +
  theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(x="Self-reported Tie Strength", y="Intention to Share Correction") +
  coord_cartesian(ylim=c(0,2.7)) +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size=26),
        axis.title.x = element_text(size=26),
        axis.title.y = element_text(size=26),
        legend.title = element_text(size=26), 
        legend.text = element_text(size=26))
ggsave("./results/pakistan/debunk/inferred_tie_strength_numeric_sharing.png",  width = 6, height = 6)

# binary measure
test_res <- summary(aov(QCORR_MIMSHARE_BINARY ~ inferred_tie_treatment, df))
ggplot(df, aes(inferred_tie_treatment, as.numeric(QCORR_MIMSHARE_BINARY), fill = inferred_tie_treatment)) +
  theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, fun.args=list(conf.int=0.95)) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(x="Self-reported Tie Strength", y="Binary Correction Sharing") +
  ggtitle(paste('ANOVA p-val:', round(test_res[[1]]$`Pr(>F)`[1], 6))) +
  theme(plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18), 
        legend.text = element_text(size=16))

# ordinal measure
test_res <- chisq.test(df$inferred_tie_treatment, df$QCORR_MIMSHARE)
summary_stats <- df[,c("inferred_tie_treatment", "QCORR_MIMSHARE")] %>%
  group_by(inferred_tie_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(inferred_tie_treatment, QCORR_MIMSHARE) %>% 
  summarise(freq=n()/first(treatment_count))
summary_stats$percent_freq <- paste0(round(summary_stats$freq*100, 1), '%')
ggplot(summary_stats, aes(x=QCORR_MIMSHARE, y=freq, fill=inferred_tie_treatment, label=percent_freq)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sharing Correction", y="Within treatment fraction", fill="Self-reported\nTie Strength") +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4))) +
  ylim(0,0.42) +
  geom_text(size=5, position=position_dodge(0.9), hjust=0.5, vjust=-0.25) +
  theme(plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18), 
        legend.text = element_text(size=16))
ggsave("./results/pakistan/debunk/inferred_tie_strength_ordinal_sharing.png",  width = 11, height = 7)

# with tie and group
test_res <- summary(aov(correction_mimshare_numeric ~ inferred_tie_group_treatment, df))
ggplot(df, aes(inferred_tie_group_treatment, correction_mimshare_numeric, fill = inferred_tie_group_treatment)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_se) +
  coord_cartesian(ylim=c(0,2)) +
  scale_y_continuous(breaks=c(0,1,2)) +
  labs(x="Self-reported Tie Strength-Group", y="Numeric Correction Sharing", fill="Self-reported\nTie Strength-Group") +
  ggtitle(paste('ANOVA p-val:', round(test_res[[1]]$`Pr(>F)`[1], 4))) +
  theme(plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18), 
        legend.text = element_text(size=16))
ggsave("./results/pakistan/debunk/inferred_tie_strength_group_numeric_sharing.png",  width = 11, height = 7)

######################
# IV analysis
df <- exclude_nonattentive(data,
                           no_treatment=TRUE,
                           real_tie=TRUE,
                           search_internet=FALSE,
                           attention_check=FALSE,
                           correct_misinfo=FALSE,
                           correct_correction=FALSE,
                           long_duration=FALSE)
df <- add_inferred_treatment_variables(df, 0.5)
#df$QMIM_DISCUSS_NEWS_UNORDERED <- factor(df$QMIM_DISCUSS_NEWS, ordered = FALSE)
#df$QCORR_CRED01_UNORDERED <- factor(df$QCORR_CRED01, ordered = FALSE)
#controls <- c("QMIM_DISCUSS_NEWS", "QCORR_CRED01")
#controls <- c("QMIM_DISCUSS_NEWS_UNORDERED", "QCORR_CRED01_UNORDERED")
formula <- paste0("correction_mimshare_numeric ~ inferred_tie_treatment ",
                  " | tie_treatment")
ivmodel1 <- ivreg(formula = formula, data = df, x = TRUE)
ivmod1_sum <- summary(ivmodel1, diagnostic = TRUE)
ivmod1_sum

controls <- c("mim_discuss_news_numeric")
formula <- paste0("correction_mimshare_numeric ~ inferred_tie_treatment + ",
                  paste(controls, collapse=" + "),
                  " | tie_treatment + ", paste(controls, collapse=" + "))
ivmodel2 <- ivreg(formula = formula, data = df, x = TRUE)
ivmod2_sum <- summary(ivmodel2, diagnostic = TRUE)
ivmod2_sum

controls <- c("mim_discuss_news_numeric", "correction_credibility1_numeric")
formula <- paste0("correction_mimshare_numeric ~ inferred_tie_treatment + ",
                  paste(controls, collapse=" + "),
                  " | tie_treatment + ", paste(controls, collapse=" + "))
ivmodel3 <- ivreg(formula = formula, data = df, x = TRUE)
ivmod3_sum <- summary(ivmodel3, diagnostic = TRUE)
ivmod3_sum

output_file <- "./results/pakistan/debunk/tie_strength_iv_model.html"
dep_var_description <- "Dep Var: Numeric Intention to Share"
indep_var_descriptions <- c("Weak Tie Treatment", "Numeric Discuss News on MIM",
                            "Numeric Debunk Perceived Credibility")
title <- "IV Regression: Randomized tie-strength treatment as instrument for self-reported tie-strength"
stargazer(ivmodel1, ivmodel2, ivmodel3, type = "html",
          out = output_file,
          title = title,
          #report=('vc*sp'),
          report=('vc*s'),
          #omit = c("Constant"),
          covariate.labels = indep_var_descriptions,
          dep.var.labels.include = F,
          dep.var.caption = dep_var_description,
          column.sep.width = "0.03pt",
          add.lines = list(c(rownames(ivmod3_sum$diagnostics)[1], 
                             round(ivmod1_sum$diagnostics[1, "statistic"], 3), 
                             round(ivmod2_sum$diagnostics[1, "statistic"], 3), 
                             round(ivmod3_sum$diagnostics[1, "statistic"], 3))))