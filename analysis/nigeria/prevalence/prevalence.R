library(ggplot2)


setwd("~/research/misinformation/")
prev_df <- read.csv('./analysis/nigeria/prevalence/prevalence_data.csv')
belief_df <- read.csv('./analysis/nigeria/prevalence/belief_data.csv')

prev_df$prev <- factor(prev_df$prev, levels=c('No', 'Maybe', 'Yes'), ordered=T)
prev_df$percent_freq <- paste0(round(prev_df$freq, 1), '%')
ggplot(prev_df, aes(x=prev, y=freq, fill=group, label=percent_freq)) +
  geom_bar(stat='identity', position='dodge') +
  geom_text(size=6, position=position_dodge(0.9), hjust=0.5, vjust=-0.25) +
  ylab("% of Respondents") +
  labs(fill="Claim Type") +
  coord_cartesian(ylim=c(0,55)) +
  ggtitle('Have you encountered this claim before?') +
  theme(plot.title = element_text(hjust = 0.5, size=27),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size=26),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=26),
        legend.title = element_text(size=26), 
        legend.text = element_text(size=26))
ggsave("./results/nigeria/prevalence/prevalence_by_claim_type.png",  width = 10, height = 6)

belief_df$belief <- factor(belief_df$belief, levels=c('Inaccurate', 'Unsure', 'Accurate'), ordered=T)
belief_df$percent_freq <- paste0(round(belief_df$freq, 1), '%')
ggplot(belief_df, aes(x=belief, y=freq, fill=group, label=percent_freq)) +
  geom_bar(stat='identity', position='dodge') +
  geom_text(size=6, position=position_dodge(0.9), hjust=0.5, vjust=-0.25) +
  ylab("% of Respondents") +
  labs(fill="Claim Type") +
  coord_cartesian(ylim=c(0,46)) +
  ggtitle('Did you believe this claim?') +
  theme(plot.title = element_text(hjust = 0.5, size=27),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size=26),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=26),
        legend.title = element_text(size=26), 
        legend.text = element_text(size=26))
ggsave("./results/nigeria/prevalence/belief_by_claim_type.png",  width = 10, height = 6)
