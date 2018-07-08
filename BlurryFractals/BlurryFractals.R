library(ggplot2)
library(numDeriv)
library(gridExtra)
library(RanglaPunjab)

## Polynom: choose only one or try yourself
f  <- function (z) {z^3-1}        #Blurry 1
#f  <- function (z) {z^4+z-1}     #Blurry 2
#f  <- function (z) {z^9+(4*z)+5} #Blurry 3
z <- outer(seq(-2, 2, by = 0.01),1i*seq(-2, 2, by = 0.01),'+')
for (k in 1:5) z <- z-f(z)/matrix(grad(f, z), nrow=nrow(z))
## Supressing texts, titles, ticks, background and legend.
opt <- theme(legend.position="none",
             panel.background = element_blank(),
             axis.ticks=element_blank(), 
             axis.title=element_blank(), 
             axis.text =element_blank())
z <- data.frame(expand.grid(x=seq(ncol(z)), y=seq(nrow(z))), z=as.vector(exp(-Mod(f(z)))))

p1 <- ggplot(z, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=RanglaPunjab("SohniMahiwal")) + opt
p2 <- ggplot(z, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=RanglaPunjab("Phulkari")) + opt
p3 <- ggplot(z, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=RanglaPunjab("Gidha2")) + opt
p4 <- ggplot(z, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=RanglaPunjab("Jutti3")) + opt
# Arrange four plots in a 2x2 grid
grid.arrange(p1, p2, p3, p4, ncol=2)