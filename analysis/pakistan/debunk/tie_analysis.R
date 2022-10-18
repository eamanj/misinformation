library(ggplot2)
library(gridExtra)
library(klaR)
library(cluster)
library(fpc)

setwd("~/research/misinformation/")
source("./analysis/pakistan/debunk/clean_data.R")
data <- process_data("./data/pakistan/debunk/Pakistan results – wave one - answers displayed as text.xlsx")

# which filtering to use?
df <- exclude_nonattentive(data,
                           no_treatment=TRUE,
                           real_tie=FALSE,
                           search_internet=FALSE,
                           attention_check=TRUE,
                           correct_misinfo=FALSE,
                           correct_correction=FALSE,
                           long_duration=FALSE)

# how closely self-reported tie strengths match the assigned tie strength treatment
round(prop.table(table(df$tie_strength01_numeric, df$tie_treatment, useNA="ifany"), margin=1), 2)
round(prop.table(table(df$tie_strength01_numeric, df$tie_strength02_numeric), margin=1), 2)
round(prop.table(table(df$tie_strength01_numeric, df$tie_strength04_numeric), margin=1), 2)

# how closely self-reported tie agreement match the assigned tie agreement treatment
round(prop.table(table(df$group_treatment, df$tie_agree_numeric), margin=1), 2)

# tie is well-informed in politics?
round(prop.table(table(df$QTIE_CRED01, useNA = "ifany")), 2)

# frequently discuss politics with tie?
round(prop.table(table(df$QTIE_POLI_FREQ, useNA = "ifany")), 2)

# distribution of tie role vs tie strength treatment
round(prop.table(table(df$tie_role, df$tie_treatment), margin=1), 3)

# Compare self report tie questions with tie treatment
strength_variables <- paste0("QTIE_STR", c(paste0("0", 1:9), as.character(10:12)))
plots <- list()
for(variable in strength_variables) {
  print(variable)
  test <- chisq.test(df$tie_treatment, df[[variable]])
  p <- ggplot(df, aes_string(variable, "(..count..)/sum(..count..)", fill = "tie_treatment")) +
    theme_bw() +
    theme(legend.position = "None",
          plot.title=element_text(size=9),
          axis.text=element_text(size=7),
          axis.title=element_text(size=9),
          legend.text=element_text(size=7),
          legend.title=element_text(size=9)) +
    geom_bar(position='dodge') +
    scale_x_discrete(labels=c("SD", "SWD", "NAND", "SWA", "SA")) +
    xlab(paste("Tie Strength:", variable)) + ylab("Fraction") +
    ggtitle(paste0("statistic: ", round(test$statistic, 3), "        ",
                   "p-val: ", round(test$p.value, 8)))
  
  plots[[variable]] <- p
}
p <- grid.arrange(grobs=plots, ncol=3)
p
ggsave("./results/tie_treatment_self_reported_strength.png", plot <- p, device = "png",
       width = 12, height = 12, units = "in")

# Compare self report tie agreement with tie treatment
variable <- "QTIE_AGREE"
test <- chisq.test(df$group_treatment, df[[variable]])
p <- ggplot(df, aes_string(variable, "(..count..)/sum(..count..)", fill = "group_treatment")) +
  theme_bw() +
  theme(legend.position = "None",
        plot.title=element_text(size=8),
        axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8)) +
  geom_bar(position='dodge') +
  xlab("Tie Agreement") + ylab("Fraction") +
  ggtitle(paste0("statistic: ", round(test$statistic, 3), "        ",
                 "p-val: ", round(test$p.value, 6)))
p
ggsave("./results/tie_agreement_treatment_self_reported_agreement.png", plot <- p, device = "png",
       width = 6, height = 4, units = "in")


df <- add_inferred_treatment_variables(df, "median")
round(prop.table(table(df$tie_treatment, df$inferred_tie_treatment, dnn=c("Treatment", "Self_reported")), 1), 4)
plot_data <- as.data.frame(prop.table(table(df$tie_treatment, df$inferred_tie_treatment,
                                            dnn=c("Treatment", "Self_reported")), 1))
ggplot(data = plot_data, aes(x=Treatment, y=Self_reported, fill=Freq)) + geom_tile()

plot_data <- as.data.frame(prop.table(table(df$tie_treatment, df$inferred_tie_treatment,
                                            dnn=c("Treatment", "Self_reported"))))
ggplot(data = plot_data, aes(x=Treatment, y=Self_reported, fill=Freq)) + geom_tile()
