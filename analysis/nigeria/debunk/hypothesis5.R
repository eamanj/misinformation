library(ggplot2)

setwd("~/research/misinformation/")
tie_group_df <- read.csv('./analysis/nigeria/debunk/tie_group_data.csv')

# numeric measure
ggplot(tie_group_df, aes(x=group, mean, fill = group)) +
  theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_bar(stat='identity', position='dodge') +
  geom_errorbar(aes(ymin = mean-1.95*se, ymax=mean+1.95*se)) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(x="Self-reported Tie Group", y="Intention to Share Correction") +
  coord_cartesian(ylim=c(0,2.55)) +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size=26),
        axis.title.x = element_text(size=26),
        axis.title.y = element_text(size=26),
        legend.title = element_text(size=26), 
        legend.text = element_text(size=26))
ggsave("./results/nigeria/debunk/inferred_tie_group_numeric_sharing.png",  width = 6, height = 6)
