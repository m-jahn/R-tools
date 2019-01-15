library(colorspace)
library(lattice)

# custom ggplot2 theme
custom.ggplot <- ggplot2like()
custom.ggplot$axis.line$col <- "white"; custom.ggplot$axis.line$lwd <- 2
custom.ggplot$strip.border$col <- "white"; custom.ggplot$strip.border$lwd <- 2

# custom lattice theme
custom.lattice <- custom.theme(
  symbol=c("#00526D", rainbow_hcl(n=6, c=90, l=50)),
  fill=c("#00526D", rainbow_hcl(n=6, c=90, l=50)),
  region=c("#00526D", rainbow_hcl(n=6, c=90, l=50)),
  reference=1, bg=0, fg=1)
custom.lattice$strip.background$col <- c(grey(0.95), grey(0.85))
custom.lattice$strip.shingle$col <- c(grey(0.75), grey(0.65))
custom.lattice$superpose.line$lwd <- 1.5
custom.lattice$reference.line$col <- "#00526D"
custom.lattice$add.line$col <- "#00526D"
custom.lattice$add.text$cex <- 0.8
custom.lattice$par.main.text$cex <- 1
custom.lattice$box.umbrella$lty <- 1
custom.lattice$par.main.text$font <- 1
