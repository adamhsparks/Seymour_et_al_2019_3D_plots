# front matter -----------------------------------------------------------------
# Author: Adam H. Sparks, adamhsparks@gmail.com
# Date: 2018-12-21
# Last Modified: 2020-05-01

# libraries --------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
library(pacman)

pacman::p_load("extrafont", "here")

extrafont::loadfonts(device = "postscript")
here::here()

# generate P and Zn values -----------------------------------------------------
P <- seq(0, 400, by = 10)
Zn <- seq(0, 60, by = 1)

# without AMF ------------------------------------------------------------------
eqn_5_fn <- function(Zn, P) {
  return(
    1.808 - 1.757 * exp(-0.00000441 * P ^ 2.311) +
      -1.811 * exp(-0.0619 * Zn ^ 1.965) +
      1.805 * exp((-0.00000441 * P ^ 2.311) - (0.0619 * Zn ^ 1.965))
  )
}

# plot without AMF -------------------------------------------------------------

Wt. <- outer(X = Zn, Y = P, eqn_5_fn)
Wt.[Wt. < 0] <- 0

postscript("Fig1.eps",
           height = 5.07874,
           width = 5.07874,
           family = "Arial Narrow",
           paper = "special",
           onefile = FALSE,
           horizontal = FALSE)
op <- par(mar = c(5, 4, 0.05, 0.05) + 0.1)
persp(x = Zn,
      y = P,
      z = Wt.,
      theta = -50,
      phi = 25,
      expand = 1,
      zlim = c(0, 2),
      col = "white",
      ltheta = 120,
      shade = NA,
      ticktype = "detailed",
      xlab = "\n\nZinc (mg/kg soil)",
      ylab = "\n\nPhosphorus (mg/kg soil)",
      zlab = "\n\nDry wt (g/plant) at 104 d",
      scale = TRUE,
      border = "black",
      lwd = 0.75)
par(op)
dev.off()

extrafont::embed_fonts("./Fig1.eps",
                       outfile = "./Fig1_embed.eps",
            options = "-dEPSCrop")

# with AMF ---------------------------------------------------------------------
eqn_6_fn <- function(Zn, P) {
  return(2.176 -
           0.6057 * exp(-0.0458 * P ^ 0.54) +
           -2.543 * exp(-0.3295 * Zn ^ 0.953) +
           1.412 * exp((-0.0458 * P ^ 0.54) - (0.3295 * Zn ^ 0.953)))
}

# plot with AMF ----------------------------------------------------------------
Wt. <- outer(X = Zn, Y = P, eqn_6_fn)
Wt.[Wt. < 0] <- 0

postscript("Fig2.eps",
           height = 5.07874,
           width = 5.07874,
           family = "Arial Narrow",
           paper = "special",
           onefile = FALSE,
           horizontal = FALSE)
op <- par(mar = c(5, 4, 0.05, 0.05) + 0.1)
persp(x = Zn,
      y = P,
      z = Wt.,
      theta = -50,
      phi = 25,
      expand = 1,
      zlim = c(0, 2),
      col = "white",
      ltheta = 120,
      shade = NA,
      ticktype = "detailed",
      xlab = "\n\nZinc (mg/kg soil)",
      ylab = "\n\nPhosphorus (mg/kg soil)",
      zlab = "\n\nDry wt (g/plant) at 104 d",
      scale = TRUE,
      border = "black",
      lwd = 0.75)
par(op)
dev.off()

extrafont::embed_fonts("./Fig2.eps",
                       outfile = "./Fig2_embed.eps",
                       options = "-dEPSCrop")
