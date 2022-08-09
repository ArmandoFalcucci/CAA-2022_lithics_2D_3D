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
lf_section <- list.files("outlines_section", pattern = "\\.txt$",full.names=T)

#Dataset <- read.csv("Dataset.csv")

Coordinates_section <- import_txt(lf_section)

Shape_sections <- Out(Coordinates_section, fac = dplyr::select(Dataset, c("Retouch", "Typology"))) #used to create more factors using dplyr instead of using fac = namedataframe$variable inside

tiff("section_outlines.tiff", width = 2024, height = 2024, res = 300)
panel(Shape_sections, names=FALSE) #inspect the outlines
dev.off()

# GMM procedures -------
Shape_sections_centered <- Momocs::coo_centre(Shape_sections) # Used to center
Shape_sections_centered_scaled <- Momocs::coo_scale(Shape_sections_centered) # scale
Shape_sections_centered_scaled <- Momocs::coo_slidedirection (Shape_sections_centered_scaled, direction = "up")

# stack inspections, both centered and then scaled -------
stack(Shape_sections_centered_scaled)

tiff("Shape_sections_centered_scaled.tiff", width = 1024, height = 1024, res = 200) #to save plots
stack(Shape_sections_centered_scaled)
dev.off()

# calibrate harmonic power ------
calibrate_harmonicpower_efourier(Shape_sections_centered_scaled) #Estimates the number of harmonics required for the four Fourier methods implemented in Momocs

# EFA analysis-----
Shape_sections_centered_scaled.EFA <- efourier(Shape_sections_centered_scaled, nb.h=16) #Num. of harmonics set to 10 according to the 99%. Change according to the outcome of the previous test

#Mean shapes -----
# you can proceed right away with the mean shape comparison #to display the mean shape of the whole data set
tiff("Mean_shape_section.tiff", width = 1024, height = 1024, res = 200)
Shape_sections_centered_scaled.EFA %>% MSHAPES() %>% coo_plot()
dev.off()

# better to plot this stuff as mean shape per group
Sections_Mean_Shapes <- MSHAPES(Shape_sections_centered_scaled.EFA, ~Retouch)
Out(Sections_Mean_Shapes$shp) %>% panel(names=TRUE)

tiff("Sections_shapes_by_retouch.tiff", width = 1824, height = 1824, res = 200) #to save plots
Out(Sections_Mean_Shapes$shp) %>% panel(names=TRUE)
dev.off()

# You can also perform a two-way comparison plotting them in a matrix
plot_MSHAPES(Sections_Mean_Shapes)
#and then save it:
tiff("Sections_shapes_compared.tiff", width = 2524, height = 2024, res = 300)
plot_MSHAPES(Sections_Mean_Shapes, size = 5/6)
dev.off()
#or you can do it with this method, using the harmonics required by the previous step: 
#Shape_outlines_centered_scaled_rotated %>% efourier(24) %>% MSHAPES(~Unit) %>% plot_MSHAPES()


### PCA: Principal Component Analysis on EFDs -----
Shape_sections_centered_scaled.PCA <- PCA(Shape_sections_centered_scaled.EFA)

write.csv(Shape_sections_centered_scaled.PCA$x, "PCs_cross-sections.csv")

### Linear discriminant analysis ------
Shape_sections_centered_scaled.LDA <- LDA(Shape_sections_centered_scaled.PCA, ~Retouch)


## Screeplot----
Sections_shapes_screeplot <- Momocs::scree_plot(Shape_sections_centered_scaled.PCA, nax = 1:10)

ggsave(Sections_shapes_screeplot,
       filename = file.path("Cross-sections_screeplot.tiff"),
       width = 7,
       height = 5,
       dpi = 320,
       units = "in",
       device = "tiff")


# Shape variation ----
Sections_shape_variation <- Momocs::PCcontrib(Shape_sections_centered_scaled.PCA,nax = 1:3, sd.r = c(-2,-1,0,1,2))

tiff("Cross-sections_shape_variation.tiff", width = 1024, height = 1024, res = 300)
Sections_shape_variation
dev.off()


#----- Change a few things to produce figures and run analysis ----
Sections_PCscores <- read.csv("PCs_cross-sections.csv") %>%
  rename(ID = X)

Dataset_new_plus_sections <- left_join(Dataset_new, Sections_PCscores[ ,c("PC1", "PC2", "PC3", "ID")], by = "ID") #using diplyr to select certain specific columns
# Dataset <- left_join(BlankTypes, PCScores[ ,c(1:4, 50)], by = "ID") #another way to do it with numbers

Dataset_new_plus_sections <- Dataset_new_plus_sections %>%
  rename(PC1_section = PC1, PC2_section = PC2, PC3_section = PC3)


### Plot the results of the PCA and the confusion matrix of the LDA  using the basic Momocs functions---- 

plot_PCA(Shape_sections_centered_scaled.PCA, ~Retouch)
plot(Shape_sections_centered_scaled.PCA, ~Retouch)

plot_CV(Shape_sections_centered_scaled.LDA, pc=T) #LDA matrix after cross-validation

plot_LDA(Shape_sections_centered_scaled.LDA)


# PERMANOVA and Pairwise, Cross-section ------
library(vegan)
library(pairwiseAdonis)

## Create the Y matrix of variables under comparison:
Y_sections <- Dataset_new_plus_sections[, c("PC1_section", "PC2_section", "PC3_section")]
## Perform a one-way NPMANOVA:
PERMANOVA_sections <- adonis2(Y_sections ~ Dataset_new_plus_sections$Retouch, method = "euclidean",
                        permutations = 10000)
## Pairwise differences

Pairwise_PERMANOVA_sections <- pairwise.adonis(Y_sections, Dataset_new_plus_sections$Retouch, sim.method = "euclidean",
                                         p.adjust.m = "bonferroni", perm = 10000)



