

Means_external_PCs_3d <- Dataset_new %>%                                               #subset dataset by Artifact Class and calculate means for PCs for each Class 
  group_by(Retouch) %>% 
  summarise(PC1_3d = mean(PC1_3d),
            PC2_3d = mean(PC2_3d),
            PC3_3d = mean(PC3_3d),
            PC4_3d = mean(PC4_3d))


Means_external_PCs_2d <- Dataset_new %>%                                               #subset dataset by Artifact Class and calculate means for PCs for each Class 
  group_by(Retouch) %>% 
  summarise(PC1_2d = mean(PC1_2d),
            PC2_2d = mean(PC2_2d),
            PC3_2d = mean(PC3_2d))


Means_PCs_sections <- Dataset_new_plus_sections %>%
  group_by(Retouch) %>% 
  summarise(PC1_section = mean(PC1_section),
            PC2_section = mean(PC2_section),
            PC3_section = mean(PC3_section))


PC2_PC3_3D <- ggplot(Dataset_new, aes(x = PC2_3d, 
                                y = PC3_3d,
                                color = Retouch)) +
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +
  geom_hline(yintercept = 0, 
             linetype = "dashed",
             color = "black",
             size = 1, 
             alpha = 0.5) +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +
  labs(x = "PC2 (20.2% of total variance)", y = "PC3 (8.8% of total variance)") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.text = element_text(size=12,face="bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold")) +
  scale_color_manual(values = c("chocolate3", "cyan4", "blue4", "darkviolet"),
                     labels = c("Alternate", "Direct bilateral", "Direct unilateral", "Inverse")) +
  geom_point(data = Means_external_PCs_3d, 
             size = 9)


PC2_PC3_3D

ggsave(PC2_PC3_3D, 
       filename = "PC2_PC3_External_3D.tiff", 
       width = 30, 
       height = 17, 
       dpi = 300, 
       units = "cm", 
       device = "tiff")



PC2_PC3_2D <- ggplot(Dataset_new, aes(x = PC2_2d, 
                                      y = PC3_2d,
                                      color = Retouch)) +
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +
  geom_hline(yintercept = 0, 
             linetype = "dashed",
             color = "black",
             size = 1, 
             alpha = 0.5) +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +
  labs(x = "PC2 (15.7% of total variance)", y = "PC3 (11% of total variance)") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.text = element_text(size=12,face="bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold")) +
  scale_color_manual(values = c("chocolate3", "cyan4", "blue4", "darkviolet"),
                     labels = c("Alternate", "Direct bilateral", "Direct unilateral", "Inverse")) +
  geom_point(data = Means_external_PCs_2d, 
             size = 9)


PC2_PC3_2D

ggsave(PC2_PC3_2D, 
       filename = "PC2_PC3_External_2D.tiff", 
       width = 30, 
       height = 17, 
       dpi = 300, 
       units = "cm", 
       device = "tiff")


boxplot_PC2_3d <- ggplot(Dataset_new, aes(x=Retouch, y=PC2_3d, fill=Retouch)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme(legend.position = "none",
        legend.text = element_text(size=20),
        axis.title = element_text(size=20, face="bold"),
        axis.text = element_text(size=21)) +
  labs(x = "Retouch", y = "PC2")


ggsave(boxplot_PC2_3d, 
       filename = "PC2_3d_external.tiff", 
       width = 25, 
       height = 17, 
       dpi = 300, 
       units = "cm", 
       device = "tiff")


boxplot_PC2_2d <- ggplot(Dataset_new, aes(x=Retouch, y=PC2_2d, fill=Retouch)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme(legend.position = "none",
        legend.text = element_text(size=20),
        axis.title = element_text(size=20, face="bold"),
        axis.text = element_text(size=21)) +
  labs(x = "Retouch", y = "PC2")


ggsave(boxplot_PC2_2d, 
       filename = "PC2_2d_external.tiff", 
       width = 25, 
       height = 17, 
       dpi = 300, 
       units = "cm", 
       device = "tiff")



boxplot_PC3_3d <- ggplot(Dataset_new, aes(x=Retouch, y=PC3_3d, fill=Retouch)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme(legend.position = "none",
        legend.text = element_text(size=20),
        axis.title = element_text(size=20, face="bold"),
        axis.text = element_text(size=21)) +
  labs(x = "Retouch", y = "PC3")


ggsave(boxplot_PC3_3d, 
       filename = "PC3_3d_external.tiff", 
       width = 25, 
       height = 17, 
       dpi = 300, 
       units = "cm", 
       device = "tiff")


boxplot_PC3_2d <- ggplot(Dataset_new, aes(x=Retouch, y=PC3_2d, fill=Retouch)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme(legend.position = "none",
        legend.text = element_text(size=20),
        axis.title = element_text(size=20, face="bold"),
        axis.text = element_text(size=21)) +
  labs(x = "Retouch", y = "PC3")


ggsave(boxplot_PC3_2d, 
       filename = "PC3_2d_external.tiff", 
       width = 25, 
       height = 17, 
       dpi = 300, 
       units = "cm", 
       device = "tiff")




# Cross-sections, PC1 to PC2 -------

PC1_PC2_section <- ggplot(Dataset_new_plus_sections, aes(x = PC1_section, 
                                      y = PC2_section,
                                      color = Retouch)) +
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +
  geom_hline(yintercept = 0, 
             linetype = "dashed",
             color = "black",
             size = 1, 
             alpha = 0.5) +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +
  labs(x = "PC1 (75.1% of total variance)", y = "PC2 (12.2% of total variance)") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.text = element_text(size=12,face="bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold")) +
  scale_color_manual(values = c("chocolate3", "cyan4", "blue4", "darkviolet"),
                     labels = c("Alternate", "Direct bilateral", "Direct unilateral", "Inverse")) +
  geom_point(data = Means_PCs_sections, 
             size = 9)


PC1_PC2_section

ggsave(PC1_PC2_section, 
       filename = "PC1_PC2_Sections.tiff", 
       width = 30, 
       height = 17, 
       dpi = 300, 
       units = "cm", 
       device = "tiff")






## Scatter plots ----

PC1toLength <- ggplot(data = Dataset_tools, aes(x = Length, y = PC1)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, color="red", fill="#69b3a2", se=TRUE) +
  labs(y= "PC1", x = "Length") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text = element_text(size=12,face="bold"))


ggsave(PC1toLength, 
       filename = "PC1_Length_blanks.tiff", 
       width = 25, 
       height = 17, 
       dpi = 300, 
       units = "cm", 
       device = 'tiff')
