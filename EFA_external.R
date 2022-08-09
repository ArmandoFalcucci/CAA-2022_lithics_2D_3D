# Load package that we will use
library(Momocs)
library(ggplot2)
library(ggpubr)
library(readxl)
library(forcats)
library(dplyr)
library(tidyr)
library(viridis)

##### External Outline analysis - importing coordinates ----
lf <- list.files("outlines_external", pattern = "\\.txt$",full.names=T)

Dataset <- read.csv("Dataset.csv")

Coordinates_external <- import_txt(lf)

Shape_outlines <- Out(Coordinates_external, fac = dplyr::select(Dataset, c("Retouch", "Typology"))) #used to create more factors using dplyr instead of using fac = namedataframe$variable inside

tiff("All_outlines.tiff", width = 2024, height = 2024, res = 300)
panel(Shape_outlines, names=FALSE) #inspect the outlines
dev.off()

# GMM procedures -------
Shape_outlines_centered <- Momocs::coo_centre(Shape_outlines) # Used to center
Shape_outlines_centered_scaled <- Momocs::coo_scale(Shape_outlines_centered) # scale
Shape_outlines_centered_scaled <- Momocs::coo_slidedirection (Shape_outlines_centered_scaled)
Shape_outlines_centered_scaled_rotated <- Momocs::coo_rotatecenter(Shape_outlines_centered_scaled, theta = -pi/2)

# stack inspections, both centered and then scaled -------
stack(Shape_outlines_centered_scaled_rotated)

tiff("Shape_outlines_centered_scaled.tiff", width = 1024, height = 1024, res = 200) #to save plots
stack(Shape_outlines_centered_scaled_rotated)
dev.off()

# calibrate harmonic power ------
calibrate_harmonicpower_efourier(Shape_outlines_centered_scaled_rotated) #Estimates the number of harmonics required for the four Fourier methods implemented in Momocs

# EFA analysis-----
Shape_outlines_centered_scaled_rotated.EFA <- efourier(Shape_outlines_centered_scaled_rotated, nb.h=23) #Num. of harmonics set to 10 according to the 99%. Change according to the outcome of the previous test

#Mean shapes -----
# you can proceed right away with the mean shape comparison #to display the mean shape of the whole data set
tiff("Mean_shape_external.tiff", width = 1024, height = 1024, res = 200)
Shape_outlines_centered_scaled_rotated.EFA %>% MSHAPES() %>% coo_plot()
dev.off()

tiff("Shape_outlines_centered_scaled.tiff", width = 1024, height = 1024, res = 300) #to save plots
stack(Shape_outlines_centered_scaled_rotated)
dev.off()

# better to plot this stuff as mean shape per group
External_Mean_Shapes <- MSHAPES(Shape_outlines_centered_scaled_rotated.EFA, ~Retouch)
Out(External_Mean_Shapes$shp) %>% panel(names=TRUE)

tiff("Mean_shapes_by_retouch.tiff", width = 1824, height = 1824, res = 200) #to save plots
Out(External_Mean_Shapes$shp) %>% panel(names=TRUE)
dev.off()

# You can also perform a two-way comparison plotting them in a matrix
plot_MSHAPES(External_Mean_Shapes)
#and then save it:
tiff("External_shapes_compared.tiff", width = 2524, height = 2024, res = 300)
plot_MSHAPES(External_Mean_Shapes, size = 5/6)
dev.off()
#or you can do it with this method, using the harmonics required by the previous step: 
#Shape_outlines_centered_scaled_rotated %>% efourier(24) %>% MSHAPES(~Unit) %>% plot_MSHAPES()


### PCA: Principal Component Analysis on EFDs -----
Shape_outlines_centered_scaled_rotated.PCA <- PCA(Shape_outlines_centered_scaled_rotated.EFA)

write.csv(Shape_outlines_centered_scaled_rotated.PCA$x, "PCs_2d.csv")

### Linear discriminant analysis ------
Shape_outlines_centered_scaled_rotated.LDA <- LDA(Shape_outlines_centered_scaled_rotated.PCA, ~Retouch)


## Screeplot----
External_shapes_screeplot <- Momocs::scree_plot(Shape_outlines_centered_scaled_rotated.PCA, nax = 1:10)

ggsave(External_shapes_screeplot,
       filename = file.path("External_screeplot.tiff"),
       width = 7,
       height = 5,
       dpi = 320,
       units = "in",
       device = "tiff")


# Shape variation ----
External_shape_variation <- Momocs::PCcontrib(Shape_outlines_centered_scaled_rotated.PCA,nax = 1:3, sd.r = c(-2,-1,0,1,2)) 

#to change orientation
External_shape_variation_flipped <- External_shape_variation$gg +
  scale_y_continuous(labels(NULL)) +
  scale_x_reverse(labels(NULL)) +
  coord_flip()

ggsave(External_shape_variation_flipped,
       filename = file.path("External_shape_variation.tiff"),
       width = 4,
       height = 5,
       dpi = 300,
       units = "in",
       device = "tiff")


#----- Change a few things to produce figures and run analysis ----
External_PCscores <- read.csv("PCs_2d.csv") %>%
  rename(ID = X)

Dataset_new <- left_join(Dataset, External_PCscores[ ,c("PC1", "PC2", "PC3", "ID")], by = "ID") #using diplyr to select certain specific columns
# Dataset <- left_join(BlankTypes, PCScores[ ,c(1:4, 50)], by = "ID") #another way to do it with numbers

Dataset_new <- Dataset_new %>%
  rename(PC1_2d = PC1, PC2_2d = PC2, PC3_2d = PC3)


### Plot the results of the PCA and the confusion matrix of the LDA  using the basic Momocs functions---- 

plot_PCA(Shape_outlines_centered_scaled_rotated.PCA, ~Retouch)
plot(Shape_outlines_centered_scaled_rotated.PCA, ~Retouch)

plot_CV(Shape_outlines_centered_scaled_rotated.LDA, pc=T) #LDA matrix after cross-validation

plot_LDA(Shape_outlines_centered_scaled_rotated.LDA)


# PERMANOVA and Pairwise, 3D and 2D study ------
library(vegan)
library(pairwiseAdonis)

## 3D

## Create the Y matrix of variables under comparison:
Y_3d <- Dataset_new[, c("PC1_3d", "PC2_3d", "PC3_3d")]
## Perform a one-way NPMANOVA:
PERMANOVA_3d <- adonis2(Y_3d ~ Dataset_new$Retouch, method = "euclidean",
                     permutations = 10000)
## Pairwise differences

Pairwise_PERMANOVA_3d <- pairwise.adonis(Y_3d, Dataset_new$Retouch, sim.method = "euclidean",
                                      p.adjust.m = "bonferroni", perm = 10000)


## 2D

## Create the Y matrix of variables under comparison:
Y_2d <- Dataset_new[, c("PC1_2d", "PC2_2d", "PC3_2d")]
## Perform a one-way NPMANOVA:
PERMANOVA_2d <- adonis2(Y_2d ~ Dataset_new$Retouch, method = "euclidean",
                        permutations = 10000)
## Pairwise differences

Pairwise_PERMANOVA_2d <- pairwise.adonis(Y_2d, Dataset_new$Retouch, sim.method = "euclidean",
                                         p.adjust.m = "bonferroni", perm = 10000)


# Kruskall-Wallis, 2D and 3D study -------

library(rstatix)

Dataset_new %>%
kruskal_test(PC1_3d ~ Retouch) #NS

Dataset_new %>%
  kruskal_test(PC2_3d ~ Retouch) #< 0.01

dunn_test_PC2_3d <- Dataset_new %>%
dunn_test(PC2_3d ~ Retouch, p.adjust.method = "bonferroni")

Dataset_new %>%
  kruskal_test(PC3_3d ~ Retouch) #< 0.01

dunn_test_PC3_3d <- Dataset_new %>%
  dunn_test(PC3_3d ~ Retouch, p.adjust.method = "bonferroni")


Dataset_new %>%
  kruskal_test(PC4_3d ~ Retouch) #NS


Dataset_new %>%
  kruskal_test(PC1_2d ~ Retouch) #NS

Dataset_new %>%
  kruskal_test(PC2_2d ~ Retouch) # < 0.01

dunn_test_PC2_2d <- Dataset_new %>%
  dunn_test(PC2_2d ~ Retouch, p.adjust.method = "bonferroni")

Dataset_new %>%
  kruskal_test(PC3_2d ~ Retouch) # < 0.01

dunn_test_PC3_2d <- Dataset_new %>%
  dunn_test(PC3_2d ~ Retouch, p.adjust.method = "bonferroni")




#--------- PCA with size as well using covariance

shape_size.pca <- prcomp(Dataset_tools[,c("Length", "Width", "Thickness", "PC1", "PC2", "PC3")], center = TRUE,scale. = TRUE)
summary(shape_size.pca)

groups <- Dataset_tools %>%
  pull(Unit)

library(ggbiplot)
ggbiplot(shape_size.pca, ellipse = TRUE, groups = groups)

ggbiplot(shape_size.pca, choices = c(1,3), ellipse = TRUE, groups = groups)

