
home_dir = "~/radius/Colombia/"
code_ext = "Code"
data_ext = "Data"
results_ext = "Results"
setwd(paste0(home_dir,code_ext))


library(readr)
library(dplyr)
library(igraph)
library(RColorBrewer)
library(gtools)
library(stargazer)
library(ggplot2)
library(lfe)
library(dotwhisker)
library(multcomp)
library(patchwork)

#### 1 Manual Coeffieicents ####
#### 1.1 Colombia ####
#### 1.1.1 Comm Only ####
## Session
b_c00 = 0.115

## 25% 
b_c25 = 0.124
se_c25 = 0.008

## 50% Sample
b_c50 = 0.132
se_c50 = 0.014
## 25% Sample
b_c75 = 0.152
se_c75 = 0.028

#### 1.1.2 Full ####
## Session
b_c00_full = 0.049

## 25% sample
b_c25_full = 0.051
se_c25_full = 0.009

## 50% Sample
b_c50_full = 0.048
se_c50_full = 0.015
## 25% Sample
b_c75_full = 0.048
se_c75_full = 0.036


#### 1.2 Tanzania ####
#### 1.2.1 Comm Only ####
## census
b_t00 = 0.167

## 25% sample
b_t25 = 0.234
se_t25 = 0.050 


## 50% sample
b_t50 = 0.267
se_t50 = 0.063
## 25% sample
b_t75 = 0.358
se_t75 = 0.091

#### 1.2.2 Full ####
## census 
b_t00_full = 0.011

## 25% sample
b_t25_full = 0.021 
se_t25_full = 0.022

## 50% estimate
b_t50_full = 0.005
se_t50_full = 0.035

## 25% estimate
b_t75_full = -0.019
se_t75_full = 0.092


variableNames = c("0%","25%","50%","75%")
betas = c(b_t00,b_t25,b_t50,b_t75,b_t00_full,b_t25_full,b_t50_full,b_t75_full)
sigmas = c(0,se_t25,se_t50,se_t75,0,se_t25_full,se_t50_full,se_t75_full)
results_df = data.frame(estimate = betas,std.error= sigmas,
                        model = c(rep("Comm. Only",4),rep("Full",4)), 
                        term = rep(variableNames,2))

## use this code for other graphs
g = dwplot(results_df, vars_order = c("75%","50%","25%","0%"),
           dot_args = list(aes(shape = model,colour = model))) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none") + #c(0.05, 0.95),
        #legend.justification = c(0, 1), 
        #legend.background = element_rect(colour="grey80"),
        #legend.title = element_blank()) + 
  xlab("Coefficient on Same Community") +
  ylab("Proportion of Nodes Dropped") +
  geom_vline(xintercept = b_t00, colour = "#00BFC4", linetype = 2) +
  geom_vline(xintercept = b_t00_full, colour = "#F8766D", linetype = 2) + 
  geom_vline(xintercept = 0, colour = "grey40", linetype = 2) +
  coord_flip() +
  annotate("rect", ymin = 0.4, ymax = 2.1, xmin = 0.36, xmax = 0.53,
           alpha = 1,fill = "white", colour= "grey80")+
  annotate("text", x = 0.48, y = 0.8, hjust = 0,vjust =0.5, label = "Full") +
  annotate("text", x = 0.42, y = 0.8, hjust = 0,vjust =0.5, label = "Comm. Only") +
  annotate("point", x = 0.48, y = 0.7, shape = 19, colour = "#F8766D") +
  annotate("point", x = 0.42, y = 0.7, shape = 17,colour = "#00BFC4")

g

setwd(paste0(home_dir,results_ext))
ggsave("tanzania_sim.pdf",width = 4, height = 4, dpi = 600, units = "in", device='pdf')

variableNames = c("0%","25%","50%","75%")
betas2 = c(b_c00,b_c25,b_c50,b_c75,b_c00_full,b_c25_full,b_c50_full,b_c75_full)
sigmas2 = c(0,se_c25,se_c50,se_c75,0,se_c25_full,se_c50_full,se_c75_full)
results_df2 = data.frame(estimate = betas2,std.error= sigmas2,
                        model = c(rep("Comm. Only",4),rep("Full",4)), 
                        term = rep(variableNames,2))

g2 = dwplot(results_df2, vars_order = c("75%","50%","25%","0%"),
            dot_args = list(aes(shape = model,colour = model))) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none") +
        #legend.position = c(0.05, 0.95),
        #legend.justification = c(0, 1), 
        #legend.background = element_rect(colour="grey80"),
        #legend.title = element_blank()) + 
  xlab("Coefficient on Same Community") +
  ylab("Proportion of Nodes Dropped") +
  geom_vline(xintercept = b_c00, colour = "#00BFC4", linetype = 2) +
  geom_vline(xintercept = b_c00_full, colour = "#F8766D", linetype = 2) + 
  geom_vline(xintercept = 0, colour = "grey40", linetype = 2) +
  coord_flip()
g2
ggsave("colombia_sim.pdf",width = 4, height = 4, dpi = 600, units = "in", device='pdf')

g | g2
