
##PACKAGES
require(ggplot2)
require(RColorBrewer)
require(dplyr)
require(ggmap)
require(spatialkernel)
require(Cairo)
require(yarrr)

## PLOT FUNCTION
test_plot <- function(c) 
{
  p <- ggplot(c, aes(x=Northing, y=Easting)) + 
    stat_density2d(aes(fill = ..level..), alpha=0.9, geom="polygon")+
    geom_point(colour="red")+scale_fill_gradientn(colours=rev(brewer.pal(7,"Spectral")), name="Movement Density") + 
    xlab("Northing") + ylab("Easting")+ 
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
      print(p)
}


# Create plotting index
df = mutate(df, row_index = 1:n())

# Loop through plots
for(i in 1:nrow(df)){
  png(sprintf("my_plot-%i.png", i))
       test_plot( filter(df, row_index <= i))
       dev.off()
}









