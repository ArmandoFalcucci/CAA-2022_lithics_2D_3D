# Load package
library(Momocs)
library(ggplot2)

##### Outline analysis of the upper cross-section - importing coordinate from DiaOutline
lf <- list.files("Upper_Cross_Section", pattern = "\\.txt$",full.names=TRUE)  
lf1<-lf_structure(lf,  split = "Upper_Cross_Section/", trim.extension = FALSE)
lf2<-data.frame(gsub(".txt","",as.character(lf1$V2)))

Coordinates <- import_txt(lf) 
Upper_Cross_Section<-Out(Coordinates, fac = lf2)

panel(Upper_Cross_Section, palette=cool_summer, names = TRUE)

# GMM procedures
Upper_Cross_Section_centered <- Momocs::coo_centre(Upper_Cross_Section) # center
Upper_Cross_Section_centered_scaled <- Momocs::coo_scale(Upper_Cross_Section_centered) # scale
Upper_Cross_Section_centered_scaled <- Momocs::coo_slidedirection(Upper_Cross_Section_centered_scaled,
                                                                         direction = "up") 

# stack inspection
stack(Upper_Cross_Section_centered_scaled)

# calibrate harmonic power
calibrate_harmonicpower_efourier(Upper_Cross_Section_centered_scaled) #Estimates the number of harmonics required for the four Fourier methods implemented in Momocs

# EFA analysis
Upper_Cross_Section_centered_scaled.EFA <- efourier(Upper_Cross_Section_centered_scaled, nb.h=16) #Num. of harmonics set to 16 according to above

### PCA: Principal Component Analysis on EFDs
Upper_Cross_Section_centered_scaled.PCA <- PCA(Upper_Cross_Section_centered_scaled.EFA)
plot(Upper_Cross_Section_centered_scaled.PCA)

Upper_Cross_Section_Screeplot <- Momocs::scree_plot(Upper_Cross_Section_centered_scaled.PCA,
                                         nax = 1:8)                      #steps used to create the screeplot#


Upper_Cross_Section_Screeplot_plot <- Upper_Cross_Section_Screeplot + 
  theme_minimal() +
  theme(text = element_text(size=18))

Upper_Cross_Section_Screeplot_plot


ggsave(Upper_Cross_Section_Screeplot_plot,
       filename = file.path("Upper_Cross_Section_Screeplot.tiff"),
       width = 7,
       height = 5,
       dpi = 300,
       units = "in",
       device = "tiff")

# PCA shape variation
Upper_Cross_Section_Shape_Variation <- Momocs::PCcontrib(Upper_Cross_Section_centered_scaled.PCA,
                                                             nax = 1:3, 
                                                             sd.r = c(-2,-1,0,1,2))    #Steps to visualize shape variation (in this case PC1 to PC3)#


Upper_Cross_Section_Shape_Variation_Shape_Variation_plot <- Upper_Cross_Section_Shape_Variation$gg + 
  theme_test() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size=16))
ggsave(Upper_Cross_Section_Shape_Variation_Shape_Variation_plot,
       filename = file.path("Upper_Cross_Section_Shape_variation_plot.tiff"),
       width = 7,
       height = 5,
       dpi = 300,
       units = "in",
       device = "tiff")


## Export raw coordinates into CSV----------------------------------------------------------------

write.csv(Upper_Cross_Section_centered_scaled.PCA$x,"Upper_Cross_Section_Principal_Components.csv",row.names = TRUE)



##### Outline analysis of the Middle cross-section - importing coordinate from DiaOutline
le <- list.files("Middle_Cross_Section", pattern = "\\.txt$",full.names=TRUE)  
le1<-lf_structure(le,  split = "Middle_Cross_Section/", trim.extension = FALSE)
le2<-data.frame(gsub(".txt","",as.character(le1$V2)))

Coordinates_Middle <- import_txt(le) 
Middle_Cross_Section<-Out(Coordinates_Middle, fac = le2)

panel(Middle_Cross_Section, names = TRUE)

# GMM procedures
Middle_Cross_Section_centered <- Momocs::coo_centre(Middle_Cross_Section) # center
Middle_Cross_Section_centered_scaled <- Momocs::coo_scale(Middle_Cross_Section_centered) # scale
Middle_Cross_Section_centered_scaled <- Momocs::coo_slidedirection(Middle_Cross_Section_centered_scaled,
                                                                  direction = "up") 

# stack inspection
stack(Middle_Cross_Section_centered_scaled)

# calibrate harmonic power
calibrate_harmonicpower_efourier(Middle_Cross_Section_centered_scaled) #Estimates the number of harmonics required for the four Fourier methods implemented in Momocs

# EFA analysis
Middle_Cross_Section_centered_scaled.EFA <- efourier(Middle_Cross_Section_centered_scaled, nb.h=16) #Num. of harmonics set to 16 according to above

### PCA: Principal Component Analysis on EFDs
Middle_Cross_Section_centered_scaled.PCA <- PCA(Middle_Cross_Section_centered_scaled.EFA)
plot(Middle_Cross_Section_centered_scaled.PCA)

Middle_Cross_Section_Screeplot <- Momocs::scree_plot(Middle_Cross_Section_centered_scaled.PCA,
                                                    nax = 1:8)

Middle_Cross_Section_Screeplot_plot <- Middle_Cross_Section_Screeplot + 
  theme_minimal() +
  theme(text = element_text(size=18))

Middle_Cross_Section_Screeplot_plot

ggsave(Middle_Cross_Section_Screeplot_plot,
       filename = file.path("Middle_Cross_Section_Screeplot.tiff"),
       width = 7,
       height = 5,
       dpi = 300,
       units = "in",
       device = "tiff")

# PCA shape variation
Middle_Cross_Section_Shape_Variation <- Momocs::PCcontrib(Middle_Cross_Section_centered_scaled.PCA,
                                                         nax = 1:3, 
                                                         sd.r = c(-2,-1,0,1,2)) 


Middle_Cross_Section_Shape_Variation_Shape_Variation_plot <- Middle_Cross_Section_Shape_Variation$gg + 
  theme_test() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size=16))
ggsave(Middle_Cross_Section_Shape_Variation_Shape_Variation_plot,
       filename = file.path("Middle_Cross_Section_Shape_variation_plot.tiff"),
       width = 7,
       height = 5,
       dpi = 300,
       units = "in",
       device = "tiff")


## Export raw coordinates into CSV----------------------------------------------------------------

write.csv(Middle_Cross_Section_centered_scaled.PCA$x,"Middle_Cross_Section_Principal_Components.csv",row.names = TRUE)
