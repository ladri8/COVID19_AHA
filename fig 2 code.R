bydate<- dft1 %>%
  group_by(COVCLINTRIAL, admonth) %>%
  filter(!(COVCLINTRIAL == "No")) %>%
  filter (!(admonth == "Before March")) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(percent = (freq*100), rounded = round(percent,1))

bydate

bydate$admonth <- factor(bydate$admonth, levels = c("March", "April", "May", "June","July", "August", "September"))

ggplot(bydate) +
  aes(x = admonth, fill = admonth, weight = freq) +
  geom_bar(position = "dodge") +
  ggtitle("Figure 2. Proportion of Enrollment in Clinical Trials by Admission Month") +
  xlab('Admission Month') + ylab('Proportion') +
  scale_fill_hue() + scale_y_continuous(limits = c(0, 0.5), breaks=seq(0,0.5,0.1)) +
  theme_classic() + 
  theme(legend.position = "none")

# 2 x 8 Contingency Table

nobeforemarch <- dft1 %>% select(COVCLINTRIAL,admonth) %>% filter(!(admonth == "Before March")) %>% droplevels()

library(tidyr)

monthtable<-table(nobeforemarch$COVCLINTRIAL, nobeforemarch$admonth)

monthtable<-tibble(monthtable)

chisq.test(monthtable)

#Fisher Exact Test 
## Get rid of Before march for this test... 
#Tried Increasing workspace to 2e8, 2e9, 2e10 runs out of memory at 2e11
fisher.test(contingencia, workspace = 2e8)

#Age group Analyses 
agegroup <- table(dft1$COVCLINTRIAL, dft1$age_group)
agegroup

#Test 
chisq.test(agegroup)

#Sex Analyses
sextable <- table(dft1$COVCLINTRIAL, dft1$SEX)
addmargins(sextable)

#Chisq Tes Sex
chisq.test(sextable)

