library(ggplot2)
library(dplyr)
library(RanglaPunjab)

# From https://fronkonstin.com/2016/05/17/playing-with-julia-set/

setwd("C:/JuliaSet")
dir.create("output")
setwd("output")
f = function(z,c) exp(z^3)+c
# Grid of complex
z0 <- outer(seq(-2, 2, length.out = 1200),1i*seq(-2, 2, length.out = 1200),'+') %>% c()
opt <-  theme(legend.position="none",
              panel.background = element_rect(fill="white"),
              plot.margin=grid::unit(c(1,1,0,0), "mm"),
              panel.grid=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              axis.text=element_blank())
for (i in 1:35)
{
  z=z0
  # i iterations of f(z)
  for (k in 1:i) {
    z <- f(z, c=-0.621) 
    df=data.frame(x=Re(z0),
    y=Im(z0),
    z=as.vector(exp(-Mod(z)))) %>% na.omit() 
 }
  p=ggplot(df, aes(x=x, y=y, color=z)) + 
    geom_tile() + 
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    scale_colour_gradientn(colours=RanglaPunjab("Paranda"))+ opt
  ggsave(plot=p, file=paste0("plot", stringr::str_pad(i, 4, pad = "0"),".png"), width = 1.2, height = 1.2)
}
# Place the exact path where ImageMagick is installed
system('C:\\PROGRA~1\\ImageMagick-7.0.8-Q16\\magick.exe -delay 20 -loop 0 output/*.png ParandaJulia.gif')
# cleaning up
unlink("output",recursive = TRUE)