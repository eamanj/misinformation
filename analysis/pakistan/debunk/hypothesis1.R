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
# Hypothesis 1: message interest vs. format
# Using interest measure QCORR_IN01
df1 <- df[!is.na(df$QCORR_IN01),]
test_res <- chisq.test(df1$format_treatment, df1$QCORR_IN01)
summary_stats <- df1[,c("format_treatment", "QCORR_IN01")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_IN01) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_IN01, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))


df_test <- df1
df_test <- df1[df1$format_treatment %in% c("Text", "Image"),]
df_test <- df1[df1$format_treatment %in% c("Text", "Audio"),]
df_test <- df1[df1$format_treatment %in% c("Image", "Audio"),]

df_test <- droplevels(df_test)
df_test$is_audio <- (df_test$format_treatment == "Audio")
print_summary_table(polr(QCORR_IN01 ~ format_treatment, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN01 ~ is_audio, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN01 ~ format_treatment + QPOLI_INTEREST, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN01 ~ format_treatment + QPOLI_ATT, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN01 ~ format_treatment + QACC_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN01 ~ format_treatment + QMIM_SEE_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN01 ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))

# Binary measure
test_res <- chisq.test(df1$format_treatment, df1$QCORR_IN01_BINARY)
summary_stats <- df1[,c("format_treatment", "QCORR_IN01_BINARY")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_IN01_BINARY) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_IN01_BINARY, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))

df_test <- df1
df_test <- df1[df1$format_treatment %in% c("Text", "Image"),]
df_test <- df1[df1$format_treatment %in% c("Text", "Audio"),]
df_test <- df1[df1$format_treatment %in% c("Image", "Audio"),]

df_test <- droplevels(df_test)
df_test$is_audio <- (df_test$format_treatment == "Audio")
summary(glm(QCORR_IN01_BINARY ~ format_treatment, df_test, family='binomial'))
summary(glm(QCORR_IN01_BINARY ~ is_audio, df_test, family='binomial'))
summary(glm(QCORR_IN01_BINARY ~ format_treatment + QPOLI_INTEREST, df_test, family='binomial'))
summary(glm(QCORR_IN01_BINARY ~ format_treatment + QPOLI_ATT, df_test, family='binomial'))
summary(glm(QCORR_IN01_BINARY ~ format_treatment + QACC_NEWS, df_test, family='binomial'))
summary(glm(QCORR_IN01_BINARY ~ format_treatment + QMIM_SEE_NEWS, df_test, family='binomial'))
summary(glm(QCORR_IN01_BINARY ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, family='binomial'))

#####
# Using interest measure QCORR_IN02
df1 <- df[!is.na(df$QCORR_IN02),]
test_res <- chisq.test(df1$format_treatment, df1$QCORR_IN02)
summary_stats <- df1[,c("format_treatment", "QCORR_IN02")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_IN02) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_IN02, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))

df_test <- df1
df_test <- df1[df1$format_treatment %in% c("Text", "Image"),]
df_test <- df1[df1$format_treatment %in% c("Text", "Audio"),]
df_test <- df1[df1$format_treatment %in% c("Image", "Audio"),]

df_test <- droplevels(df_test)
df_test$is_audio <- (df_test$format_treatment == "Audio")
print_summary_table(polr(QCORR_IN02 ~ format_treatment, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN02 ~ is_audio, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN02 ~ format_treatment + QPOLI_INTEREST, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN02 ~ format_treatment + QPOLI_ATT, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN02 ~ format_treatment + QACC_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN02 ~ format_treatment + QMIM_SEE_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN02 ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))

# Binary measure
test_res <- chisq.test(df1$format_treatment, df1$QCORR_IN02_BINARY)
summary_stats <- df1[,c("format_treatment", "QCORR_IN02_BINARY")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_IN02_BINARY) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_IN02_BINARY, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))

df_test <- df1
df_test <- df1[df1$format_treatment %in% c("Text", "Image"),]
df_test <- df1[df1$format_treatment %in% c("Text", "Audio"),]
df_test <- df1[df1$format_treatment %in% c("Image", "Audio"),]

df_test <- droplevels(df_test)
df_test$is_audio <- (df_test$format_treatment == "Audio")
summary(glm(QCORR_IN02_BINARY ~ format_treatment, df_test, family='binomial'))
summary(glm(QCORR_IN02_BINARY ~ is_audio, df_test, family='binomial'))
summary(glm(QCORR_IN02_BINARY ~ format_treatment + QPOLI_INTEREST, df_test, family='binomial'))
summary(glm(QCORR_IN02_BINARY ~ format_treatment + QPOLI_ATT, df_test, family='binomial'))
summary(glm(QCORR_IN02_BINARY ~ format_treatment + QACC_NEWS, df_test, family='binomial'))
summary(glm(QCORR_IN02_BINARY ~ format_treatment + QMIM_SEE_NEWS, df_test, family='binomial'))
summary(glm(QCORR_IN02_BINARY ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, family='binomial'))


#######
# Using interest measure QCORR_IN03
df1 <- df[!is.na(df$QCORR_IN03),]
test_res <- chisq.test(df1$format_treatment, df1$QCORR_IN03)
summary_stats <- df1[,c("format_treatment", "QCORR_IN03")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_IN03) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_IN03, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))

df_test <- df1
df_test <- df1[df1$format_treatment %in% c("Text", "Image"),]
df_test <- df1[df1$format_treatment %in% c("Text", "Audio"),]
df_test <- df1[df1$format_treatment %in% c("Image", "Audio"),]

df_test <- droplevels(df_test)
df_test$is_audio <- (df_test$format_treatment == "Audio")
print_summary_table(polr(QCORR_IN03 ~ format_treatment, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN03 ~ is_audio, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN03 ~ format_treatment + QPOLI_INTEREST, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN03 ~ format_treatment + QPOLI_ATT, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN03 ~ format_treatment + QACC_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN03 ~ format_treatment + QMIM_SEE_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(QCORR_IN03 ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))

# Binary measure
test_res <- chisq.test(df1$format_treatment, df1$QCORR_IN03_BINARY)
summary_stats <- df1[,c("format_treatment", "QCORR_IN03_BINARY")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, QCORR_IN03_BINARY) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=QCORR_IN03_BINARY, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("Within treatment fraction") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4)))

df_test <- df1
df_test <- df1[df1$format_treatment %in% c("Text", "Image"),]
df_test <- df1[df1$format_treatment %in% c("Text", "Audio"),]
df_test <- df1[df1$format_treatment %in% c("Image", "Audio"),]

df_test <- droplevels(df_test)
df_test$is_audio <- (df_test$format_treatment == "Audio")
summary(glm(QCORR_IN03_BINARY ~ format_treatment, df_test, family='binomial'))
summary(glm(QCORR_IN03_BINARY ~ is_audio, df_test, family='binomial'))
summary(glm(QCORR_IN03_BINARY ~ format_treatment + QPOLI_INTEREST, df_test, family='binomial'))
summary(glm(QCORR_IN03_BINARY ~ format_treatment + QPOLI_ATT, df_test, family='binomial'))
summary(glm(QCORR_IN03_BINARY ~ format_treatment + QACC_NEWS, df_test, family='binomial'))
summary(glm(QCORR_IN03_BINARY ~ format_treatment + QMIM_SEE_NEWS, df_test, family='binomial'))
summary(glm(QCORR_IN03_BINARY ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, family='binomial'))
