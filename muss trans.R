#Mussel transplant exp



muss.trans %>%
  group_by(Site, Location) %>% 
  summarise(per.surv = mean(muss.liveD3/Initial.mussel.number))
  
trans_mod <- lm(muss.liveD3 ~ Site*Location+0, data = muss.trans)
anotrans <- aov(muss.liveD3 ~ Site*Location, data = muss.trans)
anova(trans_mod)
summary(snail_mod)
TukeyHSD(anotrans)


muss.trans %>%
  group_by(Site, Location) %>% 
  summarise(per.surv = mean(muss.liveD3/Initial.mussel.number)) %>%
  ggplot(data = ., aes(x = Location, y = per.surv, colour = Site)) +
  geom_point(size = 5) +
  geom_errorbar(aes(x = Location, ymin = per.surv + (-SE(per.surv)), ymax = per.surv + (SE(per.surv)), width = 0.2)) +
  facet_wrap(~ Site) +
  ylim(0, 1) +
  labs(x = "", y = expression("Proportion of live mussels" )) +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size=25),
        axis.text.x = element_text(angle=30, vjust=1, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
  
  
  
  
  