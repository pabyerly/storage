library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

foxy=read.csv("kf_master_data.csv")
#just fox data
kf = filter(foxy, Species =="VUMA")

#ggplot2 hist, basic 
(rab_hist=ggplot(kf, aes(x=Rabbit)) +
  geom_histogram())

#fancy ggplot 

(rab_hist <- ggplot(kf, aes(x=Rabbit)) +
    geom_histogram(binwidth=5, colour="#76EEC6", fill="#BF3EFF") +    # Changing the binwidth and colours
    geom_vline(aes(xintercept=mean(Rabbit)),                         # Adding a line for mean abundance
               colour="red", linetype="dashed", size=1) +                # Changing the look of the line
    theme_bw() +                                                      # Changing the theme to get rid of the grey background
    ylab("Count\n") +                                                   # Changing the text of the y axis label
    xlab("\nRabbit Presence in Kit Fox Samples")  +                              # \n adds a blank line
    theme(axis.text.x=element_text(size=12),                            # Changing font size of axis labels
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             # Changing font size of axis titles
          axis.title.y=element_text(size=14, face="plain"),             # face="plain" changes font type, could also be italic, etc
          panel.grid.major.x=element_blank(),                           # Removing the grey grid lines
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm")))              # Putting a 1 cm margin around the plot

#filter data by season 
byseason=filter(foxy, Season==c("Summer", "Winter"))

#basic scatterplot
(scatter=ggplot(foxy, aes(x=Elevation, y=Rabbit, colour=Species))+geom_point())

#fancy scatterplot!
(scatter=ggplot(foxy, aes(x=Elevation, y=Rabbit, colour=Species)) +
    geom_point(size=2) +                                                # Changing point size
    geom_smooth(method=lm, aes(fill=Species)) +                    # Adding a linear model fit and colour-coding by country
    theme_bw() +
    scale_fill_manual(values = c("#76EEC6", "#BF3EFF")) +               # Adding custom colours
    scale_colour_manual(values = c("#76EEC6", "#BF3EFF"),               # Adding custom colours
                        labels=c("Coyote", "Kit Fox")) +                 # Adding labels for the legend
    ylab("Rabbit Presence in Dietary Samples\n") +                             
    xlab("\nElevation Type")  +
    theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),       # making the years at a bit of an angle
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                  # Removing the background grid lines                
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm")) +                    # Adding a 1cm margin around the plot
    theme(legend.text = element_text(size=12, face="italic"),                  # Setting the font for the legend text
          legend.title = element_blank(),                                      # Removing the legend title
          legend.position=c(0.9, 0.9)))                  # Setting the position for the legend - 0 is left/bottom, 1 is top/right

#fancy boxplot 
(fox_boxplot <- ggplot (foxy, aes(Species, Rabbit)) + geom_boxplot(aes(fill=Species)) +
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               # Adding custom colours
    scale_colour_manual(values = c("#EE7600", "#00868B")) +             # Adding custom colours
    ylab("Presence of Rabbit in Diet\n") +                             
    xlab("\nSpecies")  +
    theme(axis.text.x=element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                           # Removing the background grid lines                
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               # Adding a margin
          legend.position="none"))                                      # Removing the legend - not needed with only two factors

#arrange plots in a panel 
panel <- grid.arrange(fox_boxplot + ggtitle("(a)") + ylab("Dietary Occurence: Rabbit") + xlab("Species") +   # adding labels to the different plots
                        theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
                      scatter + ggtitle("(c)") + ylab("Dietary Occurence: Rabbit") + xlab("Habitat Type") +
                        theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")) +
                        theme(legend.text = element_text(size=12, face="italic"),               
                              legend.title = element_blank(),                                   
                              legend.position=c(0.85, 0.85)), # changing the legend position so that it fits within the panel
                      ncol=1) # ncol determines how many columns you have