## Simulation of flight paths


# Setup -------------------------------------------------------------------
source("code/install_libraries.R")
library("trajr")



# test --------------------------------------------------------------------



coords <- data.frame(x = c(1, 1.5, 2, 2.5, 3, 4),
                     y = c(0, 0, 1, 1, 2, 1),
                     times = c(0, 1, 2, 3, 4, 5))
trj <- TrajFromCoords(coords)

par(mar = c(4, 4, 0.5, 0.5) + 0.1)
plot(trj)

