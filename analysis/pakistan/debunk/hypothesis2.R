library(ggplot2)
library(MASS)
library(dplyr)
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
                           long_duration=FALSE)

print_summary_table <- function(polr_fit) {
  ctable <- coef(summary(polr_fit))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- cbind(ctable, "p value" = p)
  return(round(ctable, 4))
}



############################################
# Hypothesis 2: belief change vs. format
# if belief moves toward inaccuracy of the claim, it is deemed as higher
df$belief_change <- ifelse(df$QMISINFO_BELIEF02 > df$QMISINFO_BELIEF01, "Higher",
                           ifelse(df$QMISINFO_BELIEF02 < df$QMISINFO_BELIEF01, "Lower",
                                  "Unchanged"))
df$belief_change <- factor(df$belief_change, levels=c("Lower", "Unchanged", "Higher"), ordered=TRUE)
df_test <- df
df_test <- df[df$format_treatment %in% c("Text", "Audio"),]
df_test <- df[df$format_treatment %in% c("Image", "Audio"),]
test_res <- chisq.test(df_test$format_treatment, df_test$belief_change)
summary_stats <- df[,c("format_treatment", "belief_change")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, belief_change) %>% 
  summarise(freq=n()/first(treatment_count))
summary_stats$percent_freq <- paste0(round(summary_stats$freq*100, 1), '%')
ggplot(summary_stats, aes(x=belief_change, y=freq*100, fill=format_treatment, label=percent_freq)) +
  geom_bar(stat='identity', position='dodge') +
  geom_text(size=6, position=position_dodge(0.9), hjust=0.5, vjust=-0.25) +
  ylab("% of Respondents") +
  labs(fill="Correction\nFormat") +
  coord_cartesian(ylim=c(0,51)) +
  ggtitle('Change in Belief') +
  theme(plot.title = element_text(hjust = 0.5, size=27),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size=26),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=26),
        legend.title = element_text(size=26), 
        legend.text = element_text(size=26))
ggsave("./results/pakistan/debunk/format_belief_change.png",  width = 10, height = 6)


df_test <- df
df_test <- df[df$format_treatment %in% c("Text", "Image"),]
df_test <- df[df$format_treatment %in% c("Text", "Audio"),]
df_test <- df[df$format_treatment %in% c("Image", "Audio"),]


df_test <- droplevels(df_test)
df_test$is_audio <- (df_test$format_treatment == "Audio")
print_summary_table(polr(belief_change ~ format_treatment, df_test, Hess=TRUE))
print_summary_table(polr(belief_change ~ is_audio, df_test, Hess=TRUE))
print_summary_table(polr(belief_change ~ format_treatment + QPOLI_INTEREST, df_test, Hess=TRUE))
print_summary_table(polr(belief_change ~ format_treatment + QPOLI_ATT, df_test, Hess=TRUE))
print_summary_table(polr(belief_change ~ format_treatment + QACC_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(belief_change ~ format_treatment + QMIM_SEE_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(belief_change ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
# interaction terms
print_summary_table(polr(belief_change ~ format_treatment + format_treatment*QCORR_IN01, df_test, Hess=TRUE))
print_summary_table(polr(belief_change ~ format_treatment + format_treatment*QCORR_IN01 + QPOLI_INTEREST, df_test, Hess=TRUE))
####
# belief change with degrees of change as numeric
df$belief_change_numeric <- (df$misinfo_belief_2_numeric - df$misinfo_belief_1_numeric)
test_res <- summary(aov(belief_change_numeric ~ format_treatment, df))
ggplot(df, aes(format_treatment, belief_change_numeric, fill = format_treatment)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  stat_summary(geom = "bar", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_se) +
  ggtitle(paste('ANOVA p-val:', round(test_res[[1]]$`Pr(>F)`[1], 4))) +
  labs(x="Correcton Format", y="Numeric Belief Change", fill="Correction\nFormat") +
  theme(plot.title = element_text(hjust = 0.5, size=20),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18), 
        legend.text = element_text(size=16))
ggsave("./results/pakistan/debunk/format_belief_change_numeric.png",  width = 10, height = 6)

df_test <- df
df_test <- df[df$format_treatment %in% c("Text", "Image"),]
df_test <- df[df$format_treatment %in% c("Text", "Audio"),]

df_test <- droplevels(df_test)
df_test$is_audio <- (df_test$format_treatment == "Audio")
summary(lm(belief_change_numeric ~ format_treatment, df_test))
summary(lm(belief_change_numeric ~ is_audio, df_test))
summary(lm(belief_change_numeric ~ format_treatment + QPOLI_INTEREST, df_test))
summary(lm(belief_change_numeric ~ format_treatment + QPOLI_ATT, df_test))
summary(lm(belief_change_numeric ~ format_treatment + QACC_NEWS, df_test))
summary(lm(belief_change_numeric ~ format_treatment + QMIM_SEE_NEWS, df_test))
summary(lm(belief_change_numeric ~ format_treatment + QMIM_DISCUSS_NEWS, df_test))
# interaction terms
summary(lm(belief_change_numeric ~ format_treatment + format_treatment*QCORR_IN01, df_test))
summary(lm(belief_change_numeric ~ format_treatment + format_treatment*QCORR_IN01 + QPOLI_INTEREST, df_test))

####
# belief change with  degrees of change as factor
df$belief_change_factor <- factor(df$belief_change_numeric,
                                  levels=c(-4,-3,-2,-1,0,1,2,3,4), ordered=TRUE)
test_res <- chisq.test(df$format_treatment, df$belief_change_factor)
summary_stats <- df[,c("format_treatment", "belief_change_factor")] %>% group_by(format_treatment) %>%
  mutate(treatment_count=n()) %>%
  group_by(format_treatment, belief_change_factor) %>% 
  summarise(freq=n()/first(treatment_count))
ggplot(summary_stats, aes(x=belief_change_factor, y=freq, fill=format_treatment)) +
  geom_bar(stat='identity', position='dodge') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste('Pearson Chi-squared p-val:', round(test_res$p.value, 4))) +
  labs(x="Change in Belief", y="Within Treatment Fraction", fill="Correction\nFormat")


df_test <- df
df_test <- df[df$format_treatment %in% c("Text", "Image"),]
df_test <- df[df$format_treatment %in% c("Text", "Audio"),]


df_test <- droplevels(df_test)
df_test$is_audio <- (df_test$format_treatment == "Audio")
print_summary_table(polr(belief_change_factor ~ format_treatment, df_test, Hess=TRUE))
print_summary_table(polr(belief_change_factor ~ is_audio, df_test, Hess=TRUE))
print_summary_table(polr(belief_change_factor ~ format_treatment + QPOLI_INTEREST, df_test, Hess=TRUE))
print_summary_table(polr(belief_change_factor ~ format_treatment + QPOLI_ATT, df_test, Hess=TRUE))
print_summary_table(polr(belief_change_factor ~ format_treatment + QACC_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(belief_change_factor ~ format_treatment + QMIM_SEE_NEWS, df_test, Hess=TRUE))
print_summary_table(polr(belief_change_factor ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE))
# interaction terms
print_summary_table(polr(belief_change_factor ~ format_treatment + format_treatment*QCORR_IN01, df_test, Hess=TRUE))
print_summary_table(polr(belief_change_factor ~ format_treatment + format_treatment*QCORR_IN01 + QPOLI_INTEREST, df_test, Hess=TRUE))

#model1 <- polr(belief_change_factor ~ format_treatment, df_test, Hess=TRUE)
#model2 <- polr(belief_change_factor ~ format_treatment + QMIM_DISCUSS_NEWS, df_test, Hess=TRUE)
#model3 <- polr(belief_change_factor ~ format_treatment + QMIM_DISCUSS_NEWS + QPOLI_INTEREST, df_test, Hess=TRUE)
model0 <- polr(belief_change_factor ~ 1, df_test, Hess=TRUE)
model1 <- polr(belief_change_factor ~ format_treatment, df_test, Hess=TRUE)
model2 <- polr(belief_change_factor ~ format_treatment + mim_discuss_news_numeric, df_test, Hess=TRUE)
model3 <- polr(belief_change_factor ~ format_treatment + mim_discuss_news_numeric + politics_interest_numeric, df_test, Hess=TRUE)


output_file <- "./results/pakistan/debunk/format_on_belief_change.html"
dep_var_description <- "Ordinal Change in Belief (Higher is more Accurate)"
indep_var_descriptions <- c("Image Format", "Text Format", "Numeric Discuss News on MIM",
                            "Numeric Interest in Politics")
title <- "Ordinal Regression: Randomized format treatment on change in belief"
stargazer(model1, model2, model3, type = "html",
          out = output_file,
          title = title,
          #report=('vc*sp'),
          report=('vc*s'),
          #omit = c("Constant"),
          covariate.labels = indep_var_descriptions,
          dep.var.labels.include = F,
          dep.var.caption = dep_var_description,
          column.sep.width = "0.03pt")