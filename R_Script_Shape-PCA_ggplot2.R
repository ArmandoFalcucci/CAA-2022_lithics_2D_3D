
#R Script used to create the shape-PCA plots and boxplots in ggplot2


# Used Packages -----------------------------------------------------------

library(ggplot2)    #to create PCA plot; used package version: 3.3.5
library(tidyverse)  #to subset dataset to calculate means per group (Artifact Class); used package version: 1.3.0 


# Shape PCA Plot ----------------------------------------------------------

DATASET_1 <- read.csv('Dataset_1.csv', header = T, sep = ',', dec = ".")  #read/load CSV file dataset 

DATASET_1$ClassBlank <- as.factor(DATASET_1$ClassBlank) #define Artifact Class as factor vector    
DATASET_1$Unit <- as.factor(DATASET_1$Unit)


Means_PCs_ClassBlank_DATASET_1 <- DATASET_1 %>%                                               #subset dataset by Artifact Class and calculate means for PCs for each Class 
  group_by(ClassBlank) %>% 
  summarise(PC1 = mean(PC1),
            PC2 = mean(PC2),
            PC3 = mean(PC3),
            PC4 = mean(PC4))

Means_PCs_Unit_DATASET_1 <- DATASET_1 %>%                                               #subset dataset by Artifact Class and calculate means for PCs for each Class 
  group_by(Unit) %>% 
  summarise(PC1 = mean(PC1),
            PC2 = mean(PC2),
            PC3 = mean(PC3))


plot_1 <- ggplot(DATASET_1, aes(x = PC1, 
                         y = PC3,
                         color = ClassBlank)) +              #indicate dataset and aesthetics 
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +                            #define scatter point shape, size and opacity
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +                             #include horizontal line through y = 0 
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +                            #include dashed vertical line through x = 0 
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.text = element_text(size=12,face="bold")) +        #draw legend on left side
  scale_color_manual(values = c("chocolate3", "cyan4", "blue4", "darkviolet"),
                     labels = c("Blade blank", "Bladelet blank", "Blade retouched", "Bladelet retouched")) + #define colors of scatter points / lagend and rename Classes 
  geom_point(data = Means_PCs_ClassBlank_DATASET_1, 
             size = 9) +                                #add mean points to plot
  stat_ellipse(level = 0.95)                            #add 95% confidence ellipse

plot_1                                                    #display plot

ggsave(plot_1, 
       filename = "PC1_PC3_Dataset_1.tiff", 
       width = 30, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")                          #save plot as tiff image



plot_2 <- ggplot(DATASET_1, aes(x = PC1, 
                                y = PC2,
                                color = ClassBlank)) +              #indicate dataset and aesthetics 
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +                            #define scatter point shape, size and opacity
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +                             #include horizontal line through y = 0 
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +                            #include dashed vertical line through x = 0 
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.text = element_text(size=12,face="bold")) +        #draw legend on left side
  scale_color_manual(values = c("chocolate3", "cyan4", "blue4", "darkviolet"),
                     labels = c("Blade blank", "Bladelet blank", "Blade retouched", "Bladelet retouched")) + #define colors of scatter points / lagend and rename Classes 
  geom_point(data = Means_PCs_ClassBlank_DATASET_1, 
             size = 9) +                                #add mean points to plot
  stat_ellipse(level = 0.95)                            #add 95% confidence ellipse

plot_2                                                    #display plot

ggsave(plot_2, 
       filename = "PC1_PC2_Dataset_1.tiff", 
       width = 30, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")                          #save plot as tiff image




plot_3 <- ggplot(DATASET_1, aes(x = PC1, 
                                y = PC2,
                                color = Unit)) +              #indicate dataset and aesthetics 
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +                            #define scatter point shape, size and opacity
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +                             #include horizontal line through y = 0 
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +                            #include dashed vertical line through x = 0 
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.text = element_text(size=14),
        axis.title = element_text(size=16, face="bold")) +        #draw legend on left side
  scale_color_manual(values = c("chocolate3", "cyan4"),
                     labels = c("A1", "A2")) + #define colors of scatter points / lagend and rename Classes 
  geom_point(data = Means_PCs_Unit_DATASET_1, 
             size = 9) +                                #add mean points to plot
  stat_ellipse(level = 0.95)                            #add 95% confidence ellipse

plot_3                                                    #display plot

ggsave(plot_3, 
       filename = "PC1_PC2_Unit_Dataset_1.tiff", 
       width = 30, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")                          #save plot as tiff image


#-------------------------------------------------------------------------#

DATASET_2 <- read.csv('Dataset_2.csv', header = T, sep = ',', dec = ".")  #read/load CSV file dataset 

DATASET_2$Retouch <- as.factor(DATASET_2$Retouch) #define Artifact Class as factor vector    
DATASET_2$Typology <- as.factor(DATASET_2$Typology)


Means_PCs_Retouch_DATASET_2 <- DATASET_2 %>%                                               #subset dataset by Artifact Class and calculate means for PCs for each Class 
  group_by(Retouch) %>% 
  summarise(PC1 = mean(PC1),
            PC2 = mean(PC2),
            PC3 = mean(PC3),
            PC4 = mean(PC4))

Means_PCs_Typology_DATASET_2 <- DATASET_2 %>%                                               #subset dataset by Artifact Class and calculate means for PCs for each Class 
  group_by(Typology) %>% 
  summarise(PC1 = mean(PC1),
            PC2 = mean(PC2),
            PC3 = mean(PC3))


plot_4 <- ggplot(DATASET_2, aes(x = PC2, 
                                y = PC3,
                                color = Retouch)) +              #indicate dataset and aesthetics 
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +                            #define scatter point shape, size and opacity
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +                             #include horizontal line through y = 0 
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +                            #include dashed vertical line through x = 0 
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.text = element_text(size=12,face="bold")) +        #draw legend on left side
  scale_color_manual(values = c("chocolate3", "cyan4", "blue4", "darkviolet"),
                     labels = c("Alternate", "Direct bilateral", "Direct unilateral", "Inverse")) + #define colors of scatter points / lagend and rename Classes 
  geom_point(data = Means_PCs_Retouch_DATASET_2, 
             size = 9)                                 #add mean points to plot
  

plot_4                                                   #display plot

ggsave(plot_4, 
       filename = "PC2_PC3_Dataset_2.tiff", 
       width = 30, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")                          #save plot as tiff image



plot_5 <- ggplot(DATASET_2, aes(x = PC1, 
                                y = PC3,
                                color = Retouch)) +              #indicate dataset and aesthetics 
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +                            #define scatter point shape, size and opacity
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +                             #include horizontal line through y = 0 
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +                            #include dashed vertical line through x = 0 
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.text = element_text(size=12,face="bold")) +        #draw legend on left side
  scale_color_manual(values = c("chocolate3", "cyan4", "blue4", "darkviolet"),
                     labels = c("Alternate", "Direct bilateral", "Direct unilateral", "Inverse")) + #define colors of scatter points / lagend and rename Classes 
  geom_point(data = Means_PCs_Retouch_DATASET_2, 
             size = 9) +                                #add mean points to plot
  stat_ellipse(level = 0.90)                            #add 95% confidence ellipse

plot_5                                                   #display plot

ggsave(plot_5, 
       filename = "PC1_PC3_Dataset_2.tiff", 
       width = 30, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")                          #save plot as tiff image



plot_6 <- ggplot(DATASET_2, aes(x = PC1, 
                                y = PC2,
                                color = Retouch)) +              #indicate dataset and aesthetics 
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +                            #define scatter point shape, size and opacity
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +                             #include horizontal line through y = 0 
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +                            #include dashed vertical line through x = 0 
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.text = element_text(size=12,face="bold")) +        #draw legend on left side
  scale_color_manual(values = c("chocolate3", "cyan4", "blue4", "darkviolet"),
                     labels = c("Alternate", "Direct bilateral", "Direct unilateral", "Inverse")) + #define colors of scatter points / lagend and rename Classes 
  geom_point(data = Means_PCs_Retouch_DATASET_2, 
             size = 9)                                #add mean points to plot
  

plot_6                                                   #display plot

ggsave(plot_6, 
       filename = "PC1_PC2_Dataset_2.tiff", 
       width = 30, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")                          #save plot as tiff image




plot_7 <- ggplot(DATASET_2, aes(x = PC3, 
                                y = PC4,
                                color = Retouch)) +              #indicate dataset and aesthetics 
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +                            #define scatter point shape, size and opacity
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +                             #include horizontal line through y = 0 
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +                            #include dashed vertical line through x = 0 
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.text = element_text(size=12,face="bold")) +        #draw legend on left side
  scale_color_manual(values = c("chocolate3", "cyan4", "blue4", "darkviolet"),
                     labels = c("Alternate", "Direct bilateral", "Direct unilateral", "Inverse")) + #define colors of scatter points / lagend and rename Classes 
  geom_point(data = Means_PCs_Retouch_DATASET_2, 
             size = 9)                                #add mean points to plot
 

plot_7                                                   #display plot

ggsave(plot_7, 
       filename = "PC3_PC4_Retouch_Dataset_2.tiff", 
       width = 30, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")                          #save plot as tiff image




plot_8 <- ggplot(DATASET_2, aes(x = PC2, 
                                y = PC3,
                                color = Typology)) +              #indicate dataset and aesthetics 
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +                            #define scatter point shape, size and opacity
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +                             #include horizontal line through y = 0 
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +                            #include dashed vertical line through x = 0 
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.text = element_text(size=12,face="bold")) +        #draw legend on left side
  scale_color_manual(values = c("chocolate3", "cyan4", "blue4", "darkviolet"),
                     labels = c("Dufour sub-type Dufour (n = 44)", "Font-Yves (n = 36)", "Krems (n = 8)", "Retouched bladelet (n = 10)")) + #define colors of scatter points / lagend and rename Classes 
  geom_point(data = Means_PCs_Typology_DATASET_2, 
             size = 9)                                #add mean points to plot
  

plot_8                                                   #display plot

ggsave(plot_8, 
       filename = "PC2_PC3_Typology_Dataset_2.tiff", 
       width = 33, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")                          #save plot as tiff image



#### Boxplots with jitter with ggplot#####


boxplot_1a_Dataset_2 <- ggplot(DATASET_2, aes(x=Retouch, y=PC3, fill=Retouch)) +
  geom_boxplot() +
  theme(legend.position = "none",
        legend.text = element_text(size=20),
        axis.title = element_text(size=20, face="bold"),
        axis.text = element_text(size=21)) 

boxplot_1b_Dataset_2 <- boxplot_1a_Dataset_2 + geom_jitter(shape=16, position=position_jitter(0.2))


ggsave(boxplot_1b_Dataset_2, 
       filename = "PC3_Boxplot_Dataset_2.tiff", 
       width = 25, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")



boxplot_2a_Dataset_2 <- ggplot(DATASET_2, aes(x=Retouch, y=PC2, fill=Retouch)) +
  geom_boxplot() +
  theme(legend.position = "none",
        legend.text = element_text(size=20),
        axis.title = element_text(size=20, face="bold"),
        axis.text = element_text(size=21)) 

boxplot_2b_Dataset_2 <- boxplot_2a_Dataset_2 + geom_jitter(shape=16, position=position_jitter(0.2))

boxplot_2b_Dataset_2

ggsave(boxplot_2b_Dataset_2, 
       filename = "PC2_Boxplot_Dataset_2.tiff", 
       width = 25, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")


boxplot_3a_Dataset_2 <- ggplot(DATASET_2, aes(x=Retouch, y=PC1, fill=Retouch)) +
  geom_boxplot() +
  theme(legend.position = "none",
        legend.text = element_text(size=20),
        axis.title = element_text(size=20, face="bold"),
        axis.text = element_text(size=21)) 

boxplot_3b_Dataset_2 <- boxplot_3a_Dataset_2 + geom_jitter(shape=16, position=position_jitter(0.2))

boxplot_3b_Dataset_2

ggsave(boxplot_3b_Dataset_2, 
       filename = "PC1_Boxplot_Dataset_2.tiff", 
       width = 25, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")



boxplot_4a_Dataset_2 <- ggplot(DATASET_2, aes(x=Retouch, y=PC4, fill=Retouch)) +
  geom_boxplot() +
  theme(legend.position = "none",
        legend.text = element_text(size=20),
        axis.title = element_text(size=20, face="bold"),
        axis.text = element_text(size=21)) 

boxplot_4b_Dataset_2 <- boxplot_4a_Dataset_2 + geom_jitter(shape=16, position=position_jitter(0.2))

boxplot_4b_Dataset_2

ggsave(boxplot_4b_Dataset_2, 
       filename = "PC4_Boxplot_Dataset_2.tiff", 
       width = 25, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")



boxplot_5a_Dataset_2 <- ggplot(DATASET_2, aes(x=Retouch, y=Volume, fill=Retouch)) +
  geom_boxplot() +
  theme(legend.position = "none",
        legend.text = element_text(size=16),
        axis.title = element_text(size=16, face="bold"),
        axis.text = element_text(size=16)) 

boxplot_5b_Dataset_2 <- boxplot_5a_Dataset_2 + geom_jitter(shape=16, position=position_jitter(0.2))

boxplot_5b_Dataset_2

ggsave(boxplot_5b_Dataset_2, 
       filename = "Volume_Boxplot_Dataset_2.tiff", 
       width = 25, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")



#-------------------------------------------------------------------------#


DATASET_3 <- read.csv('Dataset_3.csv', header = T, sep = ',', dec = ".")  #read/load CSV file dataset 

DATASET_3$Retouch <- as.factor(DATASET_3$Retouch) #define Artifact Class as factor vector


Means_PCs_Retouch_DATASET_3 <- DATASET_3 %>%                                               #subset dataset by Artifact Class and calculate means for PCs for each Class 
  group_by(Retouch) %>% 
  summarise(PC1 = mean(PC1),
            PC2 = mean(PC2),
            PC3 = mean(PC3))



plot_9 <- ggplot(DATASET_3, aes(x = PC1, 
                                y = PC3,
                                color = Retouch)) +              #indicate dataset and aesthetics 
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +                            #define scatter point shape, size and opacity
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +                             #include horizontal line through y = 0 
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +                            #include dashed vertical line through x = 0 
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size=14,)) +        #draw legend on left side
  scale_color_manual(values = c("chocolate3", "cyan4", "blue4", "darkviolet"),
                     labels = c("Alternate", "Direct bilateral", "Direct unilateral", "Inverse")) + #define colors of scatter points / lagend and rename Classes 
  geom_point(data = Means_PCs_Retouch_DATASET_3, 
             size = 8)                               #add mean points to plot
                    

plot_9                                                   #display plot

ggsave(plot_9, 
       filename = "PC1_PC3_Upper_Cross_Section_Dataset_3.tiff", 
       width = 20, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")                          #save plot as tiff image



plot_10 <- ggplot(DATASET_3, aes(x = PC1, 
                                y = PC2,
                                color = Retouch)) +              #indicate dataset and aesthetics 
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +                            #define scatter point shape, size and opacity
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +                             #include horizontal line through y = 0 
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +                            #include dashed vertical line through x = 0 
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size=14)) +        #draw legend on left side
  scale_color_manual(values = c("chocolate3", "cyan4", "blue4", "darkviolet"),
                     labels = c("Alternate", "Direct bilateral", "Direct unilateral", "Inverse")) + #define colors of scatter points / lagend and rename Classes 
  geom_point(data = Means_PCs_Retouch_DATASET_3, 
             size = 8)

plot_10

ggsave(plot_10, 
       filename = "PC1_PC2_Upper_Cross_Section_Dataset_3.tiff", 
       width = 20, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")                          #save plot as tiff image


#--------------------------------------------------------------------------#

DATASET_4 <- read.csv('Dataset_4.csv', header = T, sep = ',', dec = ".")  #read/load CSV file dataset 

DATASET_4$Retouch <- as.factor(DATASET_4$Retouch) #define Artifact Class as factor vector


Means_PCs_Retouch_DATASET_4 <- DATASET_4 %>%                                               #subset dataset by Artifact Class and calculate means for PCs for each Class 
  group_by(Retouch) %>% 
  summarise(PC1 = mean(PC1),
            PC2 = mean(PC2),
            PC3 = mean(PC3))



plot_11 <- ggplot(DATASET_4, aes(x = PC1, 
                                 y = PC3,
                                 color = Retouch)) +              #indicate dataset and aesthetics 
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +                            #define scatter point shape, size and opacity
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +                             #include horizontal line through y = 0 
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +                            #include dashed vertical line through x = 0 
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size=14)) +        #draw legend on left side
  scale_color_manual(values = c("chocolate3", "cyan4", "blue4", "darkviolet"),
                     labels = c("Alternate", "Direct bilateral", "Direct unilateral", "Inverse")) + #define colors of scatter points / lagend and rename Classes 
  geom_point(data = Means_PCs_Retouch_DATASET_4, 
             size = 8)


plot_11                                                   #display plot

ggsave(plot_11, 
       filename = "PC1_PC3_Middle_Cross_Section_Dataset_4.tiff", 
       width = 20, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")                          #save plot as tiff image



plot_12 <- ggplot(DATASET_4, aes(x = PC1, 
                                 y = PC2,
                                 color = Retouch)) +              #indicate dataset and aesthetics 
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +                            #define scatter point shape, size and opacity
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +                             #include horizontal line through y = 0 
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +                            #include dashed vertical line through x = 0 
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size=14)) +        #draw legend on left side
  scale_color_manual(values = c("chocolate3", "cyan4", "blue4", "darkviolet"),
                     labels = c("Alternate", "Direct bilateral", "Direct unilateral", "Inverse")) + #define colors of scatter points / lagend and rename Classes 
  geom_point(data = Means_PCs_Retouch_DATASET_4, 
             size = 8)


plot_12                                                   #display plot

ggsave(plot_12, 
       filename = "PC1_PC2_Middle_Cross_Section_Dataset_4.tiff", 
       width = 20, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")                          #save plot as tiff image


#Below is a plot used only to create a legend that I needed for a figure in which I combined 2 scatterplots. The legend (at the bottom) was then cropped and pasted#

plot_13 <- ggplot(DATASET_4, aes(x = PC1, 
                                 y = PC2,
                                 color = Retouch)) +              #indicate dataset and aesthetics 
  geom_point(shape = 20,
             size = 6,
             alpha = 0.60) +                            #define scatter point shape, size and opacity
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5) +                             #include horizontal line through y = 0 
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black",
             size = 1, 
             alpha = 0.5)  +                            #include dashed vertical line through x = 0 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size=12)) +        #draw legend on left side
  scale_color_manual(values = c("chocolate3", "cyan4", "blue4", "darkviolet"),
                     labels = c("Alternate", "Direct bilateral", "Direct unilateral", "Inverse")) + #define colors of scatter points / lagend and rename Classes 
  geom_point(data = Means_PCs_Retouch_DATASET_4, 
             size = 8)


plot_13                                                   #display plot

ggsave(plot_13, 
       filename = "Legend.tiff", 
       width = 25, 
       height = 17, 
       dpi = 300, 
       units = "cm", 
       device = "tiff")                          #save plot as tiff image




##Boxplots with jitters of PC1 scores from Dataset 3 and 4


boxplot_6a_Dataset_3 <- ggplot(DATASET_3, aes(x=Retouch, y=PC1, fill=Retouch)) +
  geom_boxplot() +
  theme(legend.position = "none",
        legend.text = element_text(size=16),
        axis.title = element_text(size=16, face="bold"),
        axis.text = element_text(size=16)) 

boxplot_6b_Dataset_3 <- boxplot_6a_Dataset_3 + geom_jitter(shape=16, position=position_jitter(0.2))


boxplot_6b_Dataset_3

ggsave(boxplot_6b_Dataset_3, 
       filename = "PC1_Boxplot_Dataset_3.tiff", 
       width = 25, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")



boxplot_7a_Dataset_4 <- ggplot(DATASET_4, aes(x=Retouch, y=PC1, fill=Retouch)) +
  geom_boxplot() +
  theme(legend.position = "none",
        legend.text = element_text(size=16),
        axis.title = element_text(size=16, face="bold"),
        axis.text = element_text(size=16)) 

boxplot_7b_Dataset_4 <- boxplot_7a_Dataset_4 + geom_jitter(shape=16, position=position_jitter(0.2))


boxplot_7b_Dataset_4

ggsave(boxplot_7b_Dataset_4, 
       filename = "PC1_Boxplot_Dataset_4.tiff", 
       width = 25, 
       height = 17, 
       dpi = 600, 
       units = "cm", 
       device = "tiff")


