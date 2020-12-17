bydate<- dft1 %>%
  group_by(COVCLINTRIAL, admonth) %>%
  #filter(!(COVCLINTRIAL == "No")) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(percent = (freq*100), rounded = round(percent,1))

bydate

bydate$admonth <- factor(bydate$admonth, levels = c("Before March", "March", "April", "May", "June","July", "August", "September"))

ggplot(bydate) +
  aes(x = admonth, fill = admonth, weight = freq) +
  geom_bar(position = "dodge") +
  ggtitle("Figure 2. Proportion of Enrollment in Clinical Trials by Admission Month") +
  xlab('Admission Month') + ylab('Proportion') +
  scale_fill_hue() +
  theme_minimal() +
  theme(legend.position = "none")

library(tidyr)

contingencia<-table(dft1$COVCLINTRIAL, dft1$admonth)

bymonth<-bydate %>% select(COVCLINTRIAL, admonth, n) %>% pivot_wider(names_from = admonth, values_from = n) %>% group_by(COVCLINTRIAL)

contingency<-print.data.frame(bymonth)
kable(contingency)

#Chisq Test
chisq.test(contingencia)

#Fisher Exact Test


