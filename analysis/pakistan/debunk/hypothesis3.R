library(ggplot2)
library(MASS)
library(dplyr)

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
                           long_duration=FALSE)

print_summary_table <- function(polr_fit) {
  ctable <- coef(summary(polr_fit))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- cbind(ctable, "p value" = p)
  return(round(ctable, 4))
}



############################################
# MIM sharing intention vs. format without interaction terms
test_res <- chisq.test(df$format_treatment, df$QCORR_MIMSHARE)
summary_stats <- df[,c("format_treatment", "QCORR_MIMSHARE")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_MIMSHARE) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_MIMSHARE, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))


df_test <- df
df_test <- df[df$format_treatment %in% c("Text", "Image"),]
df_test <- df[df$format_treatment %in% c("Text", "Audio"),]
df_test <- df[df$format_treatment %in% c("Image", "Audio"),]

df_test <- droplevels(df_test)
print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment + QPOLI_INTEREST, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment + QPOLI_ATT, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment + QACC_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment + QMIM_SEE_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))



# Binary measure
test_res <- chisq.test(df$format_treatment, df$QCORR_MIMSHARE_BINARY)
summary_stats <- df[,c("format_treatment", "QCORR_MIMSHARE_BINARY")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_MIMSHARE_BINARY) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_MIMSHARE_BINARY, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))

df_test <- df
df_test <- df[df$format_treatment %in% c("Text", "Image"),]
df_test <- df[df$format_treatment %in% c("Text", "Audio"),]

df_test <- droplevels(df_test)
summary(glm(QCORR_MIMSHARE_BINARY ~ format_treatment, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ format_treatment + QPOLI_INTEREST, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ format_treatment + QPOLI_ATT, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ format_treatment + QACC_NEWS, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ format_treatment + QMIM_SEE_NEWS, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, family='binomial'))

####
# MIM sharing intention vs. format with interaction terms
df_test <- df
df_test <- df[df$format_treatment %in% c("Text", "Image"),]
df_test <- df[df$format_treatment %in% c("Text", "Audio"),]

df_test <- droplevels(df_test)
# set contrasts for interest to the linear contrast
contrasts(df_test$QCORR_IN01, 1) <- contr.poly(length(unique(df_test$QCORR_IN01)))[, 1:1]
contrasts(df_test$QCORR_IN01_BINARY, 1) <- contr.poly(length(unique(df_test$QCORR_IN01_BINARY)))[, 1:1]
print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment*QCORR_IN01, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment*QCORR_IN01_BINARY, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment*QCORR_IN01 + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_MIMSHARE ~ format_treatment*QCORR_IN01_BINARY + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))

summary(glm(QCORR_MIMSHARE_BINARY ~ format_treatment*QCORR_IN01, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ format_treatment*QCORR_IN01_BINARY, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ format_treatment*QCORR_IN01 + QMIM_DISCUSS_NEWS, df_test, family='binomial'))
summary(glm(QCORR_MIMSHARE_BINARY ~ format_treatment*QCORR_IN01_BINARY + QMIM_DISCUSS_NEWS, df_test, family='binomial'))



############################################
# SNS sharing intention vs. format without interaction terms
df <- df[!is.na(df$QCORR_SNSSHARE),]
test_res <- chisq.test(df$format_treatment, df$QCORR_SNSSHARE)
summary_stats <- df[,c("format_treatment", "QCORR_SNSSHARE")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_SNSSHARE) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_SNSSHARE, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))


df_test <- df
df_test <- df[df$format_treatment %in% c("Text", "Image"),]
df_test <- df[df$format_treatment %in% c("Text", "Audio"),]

df_test <- droplevels(df_test)
print_summary_table(polr(QCORR_SNSSHARE ~ format_treatment, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_SNSSHARE ~ format_treatment + QPOLI_INTEREST, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_SNSSHARE ~ format_treatment + QPOLI_ATT, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_SNSSHARE ~ format_treatment + QACC_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_SNSSHARE ~ format_treatment + QSNS_SEE_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_SNSSHARE ~ format_treatment + QSNS_DISCUSS_NEWS, df_test, Hess=TRUE))



# Binary measure
test_res <- chisq.test(df$format_treatment, df$QCORR_SNSSHARE_BINARY)
summary_stats <- df[,c("format_treatment", "QCORR_SNSSHARE_BINARY")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_SNSSHARE_BINARY) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_SNSSHARE_BINARY, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))

df_test <- df
df_test <- df[df$format_treatment %in% c("Text", "Image"),]
df_test <- df[df$format_treatment %in% c("Text", "Audio"),]

df_test <- droplevels(df_test)
summary(glm(QCORR_SNSSHARE_BINARY ~ format_treatment, df_test, family='binomial'))
summary(glm(QCORR_SNSSHARE_BINARY ~ format_treatment + QPOLI_INTEREST, df_test, family='binomial'))
summary(glm(QCORR_SNSSHARE_BINARY ~ format_treatment + QPOLI_ATT, df_test, family='binomial'))
summary(glm(QCORR_SNSSHARE_BINARY ~ format_treatment + QACC_NEWS, df_test, family='binomial'))
summary(glm(QCORR_SNSSHARE_BINARY ~ format_treatment + QSNS_SEE_NEWS, df_test, family='binomial'))
summary(glm(QCORR_SNSSHARE_BINARY ~ format_treatment + QSNS_DISCUSS_NEWS, df_test, family='binomial'))

####
# SIMS sharing intention vs. format with interaction terms
df_test <- df
df_test <- df[df$format_treatment %in% c("Text", "Image"),]
df_test <- df[df$format_treatment %in% c("Text", "Audio"),]

df_test <- droplevels(df_test)
print_summary_table(polr(QCORR_SNSSHARE ~ format_treatment*QCORR_IN01, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_SNSSHARE ~ format_treatment*QCORR_IN01_BINARY, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_SNSSHARE ~ format_treatment*QCORR_IN01 + QSNS_DISCUSS_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_SNSSHARE ~ format_treatment*QCORR_IN01 + QSNS_DISCUSS_NEWS, df_test, Hess=TRUE))

summary(glm(QCORR_SNSSHARE_BINARY ~ format_treatment*QCORR_IN01, df_test, family='binomial'))
summary(glm(QCORR_SNSSHARE_BINARY ~ format_treatment*QCORR_IN01_BINARY, df_test, family='binomial'))
summary(glm(QCORR_SNSSHARE_BINARY ~ format_treatment*QCORR_IN01 + QSNS_DISCUSS_NEWS, df_test, family='binomial'))
summary(glm(QCORR_SNSSHARE_BINARY ~ format_treatment*QCORR_IN01_BINARY + QSNS_DISCUSS_NEWS, df_test, family='binomial'))
