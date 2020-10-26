dft1 %>%
  ggplot() +
  aes(x = RACEi, fill = RACEi) +
  geom_bar() +
  scale_fill_hue() +
  labs(title = "Enrolled Vs Not-Enrolled in Clinical Trials by Race") +
  theme_classic() +
  facet_wrap(vars(COVCLINTRIAL), scales = "free") +
  coord_flip()

ggsave(ggrc,device = NULL, scale= 1, width = 7, height = 13, units= "cm", dpi = 300, limitsize= TRUE)
