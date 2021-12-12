source("data_clean.R")

#EDA Overall####################################################################

# Country <- c("W_Eur","CE_Eur","N_Amer","L_Amer","S_Asia","E_Asia","SE_Asia",
#              "MN_Africa","SS_Africa","Aus_New")

library(ggplot2)
library(RColorBrewer)

mean <- happiness %>% group_by(Region) %>% summarise(mean = mean(score))


#mean ~ Region Bar plot---------------------------------------------------------
ggplot(mean) + 
  geom_bar(aes(y = Region, x= mean, fill = Region), stat = "identity") +
  geom_text(aes(y = Region, x= mean, label = round(mean, 3)), size = 3, vjust = 0, hjust = 1) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(4, "Accent"))(10)) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  ggtitle("Average Happiness Scores in Ten Regions")


#mean ~ Region + year Bar plot--------------------------------------------------
mean2 <- happiness %>% group_by(Region, year) %>% summarise(mean = mean(score))
mean2$year <- as.character(mean2$year)
ggplot(mean2) + 
  geom_bar(aes(y = Region, x= mean, fill = year), position = "dodge", stat = "identity")+
  scale_fill_brewer(palette="Accent")+
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) + 
  ggtitle("Average yearly Happiness Scores in Ten Regions")


#density plot-------------------------------------------------------------------
library(hrbrthemes)
library(viridisLite)
library(viridis)
library(dplyr)

happiness %>%
  # filter(Region != "Australia and New Zealand") %>%
  ggplot(aes(x = score, color = Region, fill = Region)) +
  geom_density(alpha=0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  ylab("Density") +
  xlab("Happiness Score") +
  theme(axis.title.x = element_text(hjust = "0.5"),
        axis.title.y = element_text(hjust = "0.5"),
  ) +
  ggtitle("Density Plots for Scores from Ten Regions")
# theme(legend.position="none") +
# scale_fill_brewer(palette="Accent") +
# scale_color_brewer(palette="Accent")


happiness %>%
  # filter(Region != "Australia and New Zealand") %>%
  ggplot(aes(x = score, color = Region, fill = Region)) +
  geom_density(alpha=0.6) +
  facet_wrap(~ Region, ncol = 3) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  ylab("Density") +
  xlab("Happiness Score") +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = "0.5"),
        axis.title.y = element_text(hjust = "0.5"),
        plot.title = element_text(hjust = "0.5"),
        panel.spacing = unit(0.5, "lines")) +
  ggtitle("Density Plots for Scores from Ten Regions")

#There is nothing wrong with the density being greater than 1 at some points. 
#The area under the curve must be 1, but at specific points the density can be greater than 1. 

#density + bar------------------------------------------------------------------
ggplot(happiness, aes(x= score))+
  geom_histogram(aes(y=..density..), color="black", fill="white")+
  geom_density(alpha=.2,fill= "pink") +
  xlab("score") +
  ylab(paste("density")) +
  ggtitle("density of score")+
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = "0.5"),
        axis.title.y = element_text(hjust = "0.5"),
        plot.title = element_text(hjust = "0.5"))

#density ridges-----------------------------------------------------------------
library(ggridges)
ggplot(happiness, aes(x = score, y = Region, fill = Region)) +
  geom_density_ridges() +
  theme_ridges() + 
  scale_fill_brewer(palette="Accent") +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = "0.5"),
        axis.title.y = element_text(hjust = "0.5"),
        plot.title = element_text(hjust = "0.5")) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(4, "Accent"))(10)) +
  ggtitle("The Distributions of Happiness Score among Ten Regions")



#boxplot -----------------------------------------------------------------------

happiness %>%
  ggplot(aes(x = score, y = Region, fill = Region)) + 
  geom_boxplot(alpha=0.8) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(4, "Accent"))(10)) +
  theme_bw() +
  theme(legend.position="none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  ggtitle("Box plot for Happiness among Ten regions")


#corr-plot ---------------------------------------------------------------------
library(corrplot)

par(mfcol=c(1,1))
happiness$trust <- as.numeric(happiness$trust)
happiness <- na.omit(happiness)
corrplot(cor(happiness[,4:10]), tl.col="black", tl.srt=45)
#https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html


#scatterplot -------------------------------------------------------------------

library(car)
my_colors <- brewer.pal(nlevels(as.factor(happiness$Region)), "Set3")
scatterplotMatrix(data = happiness,
                  ~ score + social + life_ex + freedom + trust + generosity|Region, 
                  reg.line="" , smoother="", col=my_colors , 
                  smoother.args=list(col="grey") , cex=0.1, 
                  main="Scatter plot with Ten Regions"
)


#map----------------------------------------------------------------------------
library(rworldmap)

map <- map_data("world")
colnames(map)[5] <- "Country"
df <- left_join(happiness, map, by = "Country")

ggplot(df) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = score), color = "black", size = 0.5) + 
  coord_equal() +
  scale_fill_gradient(breaks=c(3,5,7,9)) +
  ggtitle("Happiness Score on World Map") +
  # scale_fill_gradient(low="cadetblue1", high="indianred1") +
  scale_fill_gradientn(colours = cm.colors(5)) +
  xlab("longtitude") + ylab("latitude") + labs(fill="Happiness Score") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = "0.5"))



#EDA category###################################################################

#variable v.s. outcome----------------------------------------------------------

# ggplot(happiness) +
#   geom_smooth(aes(x = GDP, y = score, color = Region, fill = Region), linetype = "longdash", se =F) +
#   geom_smooth(aes(x = GDP, y = score, linetype = "All"), color = "black") +
#   scale_color_manual(values = colorRampPalette(brewer.pal(4, "Accent"))(10)) +
#   scale_fill_manual(values = colorRampPalette(brewer.pal(4, "Accent"))(10)) +
#   scale_linetype_manual(name = "", values = c("All" = "solid")) +
#   theme_bw() +
#   theme(panel.border = element_blank(),
#         axis.line = element_line(colour = "black")) +
#   ggtitle("Average Happiness Scores in Ten Regions")

trend_plot <- function(variable = GDP, name = "GDP"){
  
  ggplot(happiness) +
    geom_smooth(aes(x = {{variable}}, y = score, color = Region, fill = Region), 
                linetype = "longdash", se =F, method = "lm") +
    geom_smooth(aes(x = {{variable}}, y = score, linetype = "All"), color = "black") +
    scale_color_manual(values = colorRampPalette(brewer.pal(4, "Accent"))(10)) +
    scale_fill_manual(values = colorRampPalette(brewer.pal(4, "Accent"))(10)) +
    scale_linetype_manual(name = "", values = c("All" = "solid")) +
    theme_bw() +
    theme(panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = "0.5")) +
    ggtitle(paste(name, "v.s. Scores in Ten Regions", sep = " "))
  
}


trend_plot <- function(variable = GDP, name = "GDP"){
  
  ggplot(happiness) +
    geom_smooth(aes(x = {{variable}}, y = score, color = Region, fill = Region), 
                linetype = "solid", se =F) +
    geom_smooth(aes(x = {{variable}}, y = score, linetype = "All"), color = "black") +
    scale_color_manual(values = colorRampPalette(brewer.pal(4, "Accent"))(10)) +
    scale_fill_manual(values = colorRampPalette(brewer.pal(4, "Accent"))(10)) +
    scale_linetype_manual(name = "", values = c("All" = "solid")) +
    theme_bw() +
    theme(panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = "0.5")) +
    ggtitle(paste(name, "v.s. Scores in Ten Regions", sep = " "))
  
}


trend_plot(GDP, "GDP")
trend_plot(social, "Social Support")
trend_plot(life_ex, "Life Expectancy")
trend_plot(freedom, "Freedom")

trend_plot(trust, "Absence of Corruption (Trust)")
trend_plot(generosity, "Generosity")


#-------------------------------------------------------------------------------

pplot <- function(variable = GDP, name = "GDP"){
  
  ggplot(happiness, aes(x = score, y = {{variable}})) +
    geom_point(alpha = 0.3,aes(color = Region)) +
    geom_smooth(aes(color = Region), se = F, method = "lm") +
    scale_fill_brewer(direction = -1) +
    labs(title = paste("Happiness scores v.s.", name), 
         x="GDP",
         y="score") +
    theme_bw() + 
    theme(panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = "0.5")) 
  
}
  
pplot(GDP, "GDP")
pplot(social, "Social Support") +theme(legend.position = "none")
pplot(life_ex, "Life Expectancy")
pplot(freedom, "Freedom")

pplot(trust, "Absence of Corruption (Trust)")
pplot(generosity, "Generosity")

#variables in Regions-----------------------------------------------------------

# mean_GDP <- happiness %>%
#   group_by(Region) %>%
#   summarise(mean = mean(GDP))
# 
# mean_GDP <- mean_GDP[order(-mean_GDP$mean),]
# mean_GDP$color_order <- as.character(formatC(1:10, flag = 0, width = 2))
# 
# ggplot(mean_GDP) +
#   geom_bar(aes(y = reorder(Region, mean), x= mean, fill = color_order), stat = "identity") +
#   geom_text(aes(y = reorder(Region, mean), x= mean, label = round(mean, 3)), 
#             size = 3, vjust = 0, hjust = 1) +
#   scale_fill_manual(values = colorRampPalette(brewer.pal(4, "Accent"))(10)) +
#   theme_bw() +
#   theme(legend.position = "none",
#         panel.grid = element_blank(),
#         panel.border = element_blank(),
#         axis.line = element_line(colour = "black")) +
#   ylab("Region") +
#   ggtitle(paste("Average", "GDP", "in Ten Regions"))


var_plot <- function(variable = GDP, name = "GDP", color_palette = "YlOrRd"){
  
  mean_v <- happiness %>%
    group_by(Region) %>%
    summarise(mean = mean({{variable}}))
  
  mean_v <- mean_v[order(mean_v$mean),]
  mean_v$color_order <- as.character(formatC(1:10, flag = 0, width = 2))
  
  ggplot(mean_v) + 
    geom_bar(aes(y = reorder(Region, mean), x= mean, fill = color_order), stat = "identity") +
    geom_text(aes(y = reorder(Region, mean), x= mean, label = round(mean, 3)), 
              size = 3, vjust = 0, hjust = 1) +
    scale_fill_manual(values = colorRampPalette(brewer.pal(3, color_palette))(10)) +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black")) +
    ylab("Region") +
    ggtitle(paste("Average", name, "in Ten Regions"))
  
}

var_plot(GDP, "GDP", "YlOrRd")
var_plot(social, "Social Support", "YlGnBu")
var_plot(life_ex, "Life Expectancy", "Set3")
var_plot(freedom, "Freedom", "RdPu")
var_plot(trust, "Absence of Corruption (Trust)", "Accent")
var_plot(generosity, "Generosity", "BuPu")


#variables in boxplot ----------------------------------------------------------

box_plot <- function(variable = GDP, name = "GDP", color_palette = "YlOrRd"){
  
  happiness %>%
    ggplot(aes(x = {{variable}}, y = Region, fill = Region)) + 
    geom_boxplot(alpha=0.8) +
    scale_fill_manual(values = colorRampPalette(brewer.pal(4, color_palette))(10)) +
    theme_bw() +
    theme(legend.position="none",
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black")) +
    ggtitle(paste("Box Plot of", name, "among Ten Regions"))
  
}

box_plot(GDP, "GDP", "YlOrRd")
box_plot(social, "Social Support", "YlGnBu")
box_plot(life_ex, "Life Expectancy", "Set3")
box_plot(freedom, "Freedom", "RdPu")
box_plot(trust, "Absence of Corruption (Trust)", "Accent")
box_plot(generosity, "Generosity", "BuPu")

#density plot - variables-------------------------------------------------------
ggplot(happiness, aes(x= score, fill = Region))+
  geom_histogram(aes(y=..density..), color="black")+
  geom_density(alpha=.2) +
  xlab("score") +
  ylab(paste("density")) +
  facet_wrap(~ Region, ncol = 3) + theme_bw()
  