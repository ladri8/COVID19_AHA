bydate<- dft1 %>%
  group_by(COVCLINTRIAL, admonth) %>%
  filter(!(COVCLINTRIAL == "No")) %>%
  filter (!(admonth == "BM")) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(percent = (freq*100), rounded = round(percent,1))

bydate

bydate$admonth <- factor(bydate$admonth, levels = c("March", "April", "May", "June","July", "August", "September"))

ggplot(bydate) +
  aes(x = admonth, weight = percent) +
  geom_bar(position = "dodge", fill = "#00BFC4") +
  ggtitle("Figure 2. Clinical Trial Enrollment Over Time") +
  xlab('Admission Month (2020)') + ylab('Proportion (%) of All Enrolled Patients') +
  scale_y_continuous(limits = c(0, 50), breaks=seq(0,50,10)) +
  theme_classic(base_size = 16) + 
  theme(legend.position = "none")

# 2 x 8 Contingency Table

nobeforemarch <- dft1 %>% select(COVCLINTRIAL,admonth) %>% filter(!(admonth == "BM")) %>% droplevels()

library(tidyr)

monthtable<-table(nobeforemarch$COVCLINTRIAL, nobeforemarch$admonth)

monthtable

chisq.test(monthtable)


#Fisher Exact Test 
## Get rid of Before march for this test... 
#Tried Increasing workspace to 2e8, 2e9, 2e10 runs out of memory at 2e11

#Age group Analyses 
agegroup <- table(dft1$COVCLINTRIAL, dft1$age_group)
agegroup

#Test 
chisq.test(agegroup)

sink("conttable_age.txt")

print(chisq.test(agegroup))
sink()


#Sex Analysis
sextable <- table(dft1$COVCLINTRIAL, dft1$SEX)
addmargins(sextable)

#Chisq Tes Sex
chisq.test(sextable)

#Save raw output in txt
sink("conttables.txt")

print("Admission Month")
print(monthtable)
print(chisq.test(monthtable))

print("Age Group")

print(agegroup)
print(chisq.test(agegroup))


print(chisq.test(sextable))



#### Fig 1 


p3 <- byrace2 %>% filter((!(COVCLINTRIAL == "No"))) %>% ggplot(aes(y= percent, x= reorder(RACEgroup, -percent), fill=COVCLINTRIAL))+ 
  geom_bar(stat="identity", width = 0.8) + 
  ggtitle("Figure 1.Percentage of Enrollment in Clinical Trials") +
  xlab('Race/Ethnicity') + ylab('Percentage') +
  geom_text(aes(x = RACEgroup, label = paste0(rounded), 
                colour = 'white',
                size= 2.7,
                position=position_stack(vjust=0.5)) +
              labs(fill='Enrolled')+
              scale_x_discrete(labels=c("Asian_PI" = "Asian and \n Pacific Islander",
                                        "Other" = "Native American",
                                        "Hispanic" = "Hispanic",
                                        "NHWhite" = "Non-Hispanic \n White",
                                        "Black" = "Non-Hispanic \n Black")) +
              scale_y_continuous(limits=c(0,25))+
              theme_classic()
            
p3

