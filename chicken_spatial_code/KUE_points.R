# 1. Packages
install.packages("adehabitatHR")
library(adehabitatHR)         # Package for spatal analysis

# 2. Empty Dataframe
points <- data.frame(ID = double())
XY_cor <- data.frame(X = double(),
                     Y = double())
# 3. Assigning values (this will be our spatial coordinates)
set.seed(17)
for(i in c(1:100)){
  if(i >= 50){points[i, 1] <- 1}
  else {points[i, 1] <- 2}
  #XY_cor[i, 1] <- runif(1, -78.86887, -78.86440)
  XY_cor[i, 1] <- runif(1, -78.86887, -78.86440) ## *Error here.*
  XY_cor[i, 2] <- runif(1, 0.958533, 0.960777)} # works

# 4. Transform to SpatialDataframe
coordinates(points) <- XY_cor[, c("X", "Y")]   # attribute is ID with only
#two values

points@proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84
                          +no_defs")

points = spTransform(points, CRS("+proj=utm +zone=17 +south+ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  class(points)

# 5. Domain

x <- seq(736657.2, 738163.7, by=5) # resolution is the pixel size you
#desire   limits are based on the bounding box of observations
y <-seq(10105778.4, 10106506.5, by=5)
  xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE

xy@proj4string = CRS("+proj=utm +zone=17 +south +ellps=WGS84 +datum=WGS84
                      +units=m +no_defs")

class(xy)

#points$ID <- "1"


# 6. Kernel Density
kud_points <- kernelUD(points, h = "href")
image(kud_points)

# 7. Get the Volum
vud_points <- getvolumeUD(kud_points)

# 8. Get contour
levels <- c(50, 75, 95)
list <- vector(mode="list", length = 2)

list[[1]] <- as.image.SpatialGridDataFrame(vud_points[[1]])
list[[2]] <- as.image.SpatialGridDataFrame(vud_points[[2]])

# 9. Plot
par(mfrow = c(2, 1))
image(vud_points[[1]])
contour(list[[1]], add=TRUE, levels=levels)
image(vud_points[[2]])
contour(list[[2]], add=TRUE, levels=levels)

# 10. Get vertices (It will be fine)
vkde_points <- getverticeshr(kud_points, percent = 99,
                             unin = 'm', unout='m2')
plot(vkde_points)








