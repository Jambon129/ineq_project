# plots -------------------------------------------------------------------
library(ggplot2)
library(scales)


####mean
p2mean <- svyby(~posttax, by=~year, design=silc.ph_adults, FUN = svymean)

plot_p2mean<-ggplot(p2mean, aes(x=year, y=posttax,)) +
  geom_line()  +
  geom_point() +
  scale_x_continuous(breaks=p2top$year)  +
  labs(x = "Jahr", 
       y = "Post-tax disposable income",
       title = "Mittleres Einkommen") +
  
  theme_minimal() 

plot_p2mean

####top10
p2top <- svyby(~posttax, by=~year, design=silc.ph_adults, FUN = svyquantile, 
               c(0.9), ci = TRUE)

plot_p2top<-ggplot(p2top, aes(x=year, y=posttax,)) +
  geom_line()  +
  geom_point() +
    scale_x_continuous(breaks=p2top$year)  +
  labs(x = "Jahr", 
     y = "Post-tax disposable income",
     title = "Top 10% Einkommen") +

  theme_minimal() 

plot_p2top


####median
p2median <- svyby(~posttax, by=~year, design=silc.ph_adults, FUN = svyquantile,
                  c(0.5), ci = TRUE)

plot_p2median<-ggplot(p2median, aes(x=year, y=posttax,)) +
  geom_line()  +
  geom_point() +
  scale_x_continuous(breaks=p2top$year)  +
  labs(x = "Jahr", 
       y = "Post-tax disposable income",
       title = "Medianeinkommen") +
  
  theme_minimal() 

plot_p2median

####80 to 20
p2svyqsr <- svyby(~posttax, by=~year, design=silc.ph_adults, FUN = svyqsr)

plot_p2svyqsr<-ggplot(p2svyqsr, aes(x=year, y=posttax,)) +
  geom_line()  +
  geom_point() +
  scale_x_continuous(breaks=p2top$year)  +
  labs(x = "Jahr", 
       y = "Post-tax disposable income",
       title = "S80/S20") +
  
  theme_minimal() 

plot_p2svyqsr

####gini
p2gini <- svyby(~posttax, by=~year, design=silc.ph_adults, FUN = svygini)

plot_p2gini<-ggplot(p2gini, aes(x=year, y=posttax,)) +
  geom_line()  +
  geom_point() +
  scale_x_continuous(breaks=p2top$year)  +
  labs(x = "Jahr", 
       y = "Post-tax disposable income",
       title = "Gini") +
  
  theme_minimal() 

plot_p2gini

# betaphase from here on --------------------------------------------------
##try plots in one together = problem scaling of y axis
names(p2gini) <- c("year", "posttaxgini","se") 
names(p2svyqsr) <- c("year", "posttaxsvyqsr", "se")
names(p2median) <- c("year", "posttaxmedian", "se")


fusion <- merge(p2gini, p2svyqsr, by="year")
fusion$se.x <- NULL
fusion$se.y <- NULL
fusion$se <- NULL




fusion1 <- merge(fusion, p2median, by = "year")
fusion1$se.x <- NULL
fusion1$se.y <- NULL
fusion1$se <- NULL



library(reshape2)
meltdf <- melt(fusion,id="year")


ggplot(meltdf,aes(x=year,y=value,colour=variable,group=variable)) + geom_line()

p<-ggplot(meltdf, aes(x=year, y=value,
                    group = variable, colour=variable)) +
  geom_line(aes(color = variable), size = 1)  +
  geom_point() +
  scale_x_continuous(breaks=p2top$year)  +
  labs(x = "Jahr", 
       y = "Post-tax disposable income",
       title = "Comparison") +
  theme_minimal()
p

## just meh




###need to look after labs but works
library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(plot_p2gini), ggplotGrob(plot_p2svyqsr), 
                ggplotGrob(plot_p2median), ggplotGrob(plot_p2mean), 
                ggplotGrob(plot_p2top), size = "last"))

###one plot
grid.newpage()


plot_p2gini

plot_p2mean



plot_p2svyqsr
plot_p2median
plot_p2top



comp_p2gini <- plot_p2gini + geom_line(colour = "blue") + 
                  geom_point(size = 2) +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


comp_p2median <-  plot_p2median + geom_line(colour = "red") + 
                    geom_point(shape = 17, size = 2) +
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


# extract gtables for each plot
library(gtable)
grid.newpage()

gtable1 <- ggplot_gtable(ggplot_build(xx))

gtable2 <- ggplot_gtable(ggplot_build(yy))




ppp <- c(subset(gtable1$layout, name == "panel", se = t:r))
gtable <- gtable_add_grob(gtable1, gtable2$grobs[[which(gtable2$layout$name == "panel")]], 
                          pp$t, pp$l, pp$b, pp$l)


# axis tweaks
ia <- which(gtable2$layout$name == "axis-l")
ga <- gtable2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
gtable <- gtable_add_cols(gtable, gtable2$widths[gtable2$layout[ia, ]$l], 
                          length(gtable$widths) - 1)
gtable <- gtable_add_grob(gtable, ax, pp$t, length(gtable$widths) - 1, pp$b)


# drawing the plot with two y-axis
grid.draw(gtable)


