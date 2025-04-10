#### 1 Preliminary ####
#### 1.1 Set up workspace ####
home_dir = "~/radius/Colombia/"
data_ext = "Data"
figure_ext = "Figures"
library(readr)
library(dplyr)
library(igraph)
library(RColorBrewer)
library(gtools)
library(stargazer)
library(ggplot2)
library(lfe)
library(readxl)
library(dotwhisker)
library(dplyr)
library(dotwhisker)
library(multcomp)
library(patchwork)

#### 1.2 Control Panel ####
nudge = 0.1
nudgec = -0.1
colorb = "#00BFC4"
colorc = "#F8766D"

#### 1.3 Load Data ####
setwd(paste0(home_dir,data_ext))
any = read_xlsx(path = "cond_exp_any_transfer.xlsx")
recip = read_xlsx(path = "cond_exp_reciprocal_transfer.xlsx")
total = read_xlsx(path = "cond_exp_total_transfers.xlsx")
groups = read_xlsx(path = "cond_exp_samegroup.xlsx")
setwd(paste0(home_dir,figure_ext))

#### 2 Plotting ####
#### 2.1 Any Transfers ####

variableNames = c("Distance 1 \n+ Supported","Distance 1 \n+ Unsupported","Distance 2","Distance 3")
betas = any$coef_sum
sigmas = any$se
results_df = data.frame(estimate = betas,std.error= sigmas,model = c(rep("Between Communities",4),rep("Within Communities",4)), term = rep(variableNames,2))

g = dwplot(results_df) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1), 
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) + 
  xlab("Prob. of Any Transfers \nWithin Dyad") +
  ylab("Nyakatoke Network (Tanzania)") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  geom_segment(aes(x = betas[4], xend = betas[3],y = 1+nudge,yend = 2+nudge), linetype = 3, colour = colorb) +
  geom_segment(aes(x = betas[3], xend = betas[2],y = 2+nudge,yend = 3+nudge), linetype = 3, colour = colorb) +
  geom_segment(aes(x = betas[2], xend = betas[1],y = 3+nudge,yend = 4+nudge), linetype = 3, colour = colorb) +
  geom_segment(aes(x = betas[8], xend = betas[7],y = 1+nudgec,yend = 2+nudgec), linetype = 3, colour = colorc)+
  geom_segment(aes(x = betas[7], xend = betas[6],y = 2+nudgec,yend = 3+nudgec), linetype = 3, colour = colorc) +
  geom_segment(aes(x = betas[6], xend = betas[5],y = 3+nudgec,yend = 4+nudgec), linetype = 3, colour = colorc) +
  coord_flip()
g

ggsave("radii_any.pdf",width = 6, height = 4, dpi = 500, units = "in", device='pdf')


#### 2.2 Reciprocal Transfers ####

variableNames = c("Distance 1 \n+ Supported","Distance 1 \n+ Unsupported","Distance 2","Distance 3")
betas = recip$coef_sum
sigmas = recip$se
results_df = data.frame(estimate = betas,std.error= sigmas,model = c(rep("Between Communities",4),rep("Within Communities",4)), term = rep(variableNames,2))


g = dwplot(results_df) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1), 
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) + 
  xlab("Prob. of Recip. Transfers \nWithin Dyad") +
  ylab("Nyakatoke Network (Tanzania)") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  geom_segment(aes(x = betas[4], xend = betas[3],y = 1+nudge,yend = 2+nudge), linetype = 3, colour = colorb) +
  geom_segment(aes(x = betas[3], xend = betas[2],y = 2+nudge,yend = 3+nudge), linetype = 3, colour = colorb) +
  geom_segment(aes(x = betas[2], xend = betas[1],y = 3+nudge,yend = 4+nudge), linetype = 3, colour = colorb) +
  geom_segment(aes(x = betas[8], xend = betas[7],y = 1+nudgec,yend = 2+nudgec), linetype = 3, colour = colorc)+
  geom_segment(aes(x = betas[7], xend = betas[6],y = 2+nudgec,yend = 3+nudgec), linetype = 3, colour = colorc) +
  geom_segment(aes(x = betas[6], xend = betas[5],y = 3+nudgec,yend = 4+nudgec), linetype = 3, colour = colorc) +
  coord_flip()
g

ggsave("radii_recip.pdf",width = 6, height = 4, dpi = 500, units = "in", device='pdf')

#### 2.1 Total Transfers ####

variableNames = c("Distance 1 \n+ Supported","Distance 1 \n+ Unsupported","Distance 2","Distance 3")
betas = total$coef_sum
sigmas = total$se
results_df = data.frame(estimate = betas,std.error= sigmas,model = c(rep("Between Communities",4),rep("Within Communities",4)), term = rep(variableNames,2))

g = dwplot(results_df) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1), 
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) + 
  xlab("Total Flow of Transfers \nWithin Dyad (TZS)") +
  ylab("Nyakatoke Network (Tanzania)") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  geom_segment(aes(x = betas[4], xend = betas[3],y = 1+nudge,yend = 2+nudge), linetype = 3, colour = colorb) +
  geom_segment(aes(x = betas[3], xend = betas[2],y = 2+nudge,yend = 3+nudge), linetype = 3, colour = colorb) +
  geom_segment(aes(x = betas[2], xend = betas[1],y = 3+nudge,yend = 4+nudge), linetype = 3, colour = colorb) +
  geom_segment(aes(x = betas[8], xend = betas[7],y = 1+nudgec,yend = 2+nudgec), linetype = 3, colour = colorc)+
  geom_segment(aes(x = betas[7], xend = betas[6],y = 2+nudgec,yend = 3+nudgec), linetype = 3, colour = colorc) +
  geom_segment(aes(x = betas[6], xend = betas[5],y = 3+nudgec,yend = 4+nudgec), linetype = 3, colour = colorc) +
  coord_flip()
g

ggsave("radii_total.pdf",width = 6, height = 4, dpi = 500, units = "in", device='pdf')

#### 2.1 Risk Pooling Group Co-Membership ####

variableNames = c("Distance 1 \n+ Supported","Distance 1 \n+ Unsupported","Distance 2","Distance 3")
betas = groups$coef_sum
sigmas = groups$se
results_df = data.frame(estimate = betas,std.error= sigmas,model = c(rep("Between Communities",4),rep("Within Communities",4)), term = rep(variableNames,2))

g = dwplot(results_df,model_order = c("Between Communities","Within Communities"),
           dot_args = list(aes(shape = model,colour = model))) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none") + #c(0.05, 0.95),
        #legend.justification = c(0, 1), 
        #legend.background = element_rect(colour="grey80"),
        #legend.title = element_blank()) + 
  xlab("Excess Prob of Exp. Risk \nPooling Group Co-Membership") +
  ylab("Friends and Family Network") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  geom_segment(aes(x = betas[4], xend = betas[3],y = 1+nudge,yend = 2+nudge), linetype = 3, colour = colorb) +
  geom_segment(aes(x = betas[3], xend = betas[2],y = 2+nudge,yend = 3+nudge), linetype = 3, colour = colorb) +
  geom_segment(aes(x = betas[2], xend = betas[1],y = 3+nudge,yend = 4+nudge), linetype = 3, colour = colorb) +
  geom_segment(aes(x = betas[8], xend = betas[7],y = 1+nudgec,yend = 2+nudgec), linetype = 3, colour = colorc)+
  geom_segment(aes(x = betas[7], xend = betas[6],y = 2+nudgec,yend = 3+nudgec), linetype = 3, colour = colorc) +
  geom_segment(aes(x = betas[6], xend = betas[5],y = 3+nudgec,yend = 4+nudgec), linetype = 3, colour = colorc) +
  coord_flip() +
  annotate("rect", ymin = 0.5, ymax = 2.1, xmin = 0.22, xmax = 0.29,
           alpha = 1,fill = "white", colour= "grey80")+
  annotate("text", x = 0.27, y = 0.8, hjust = 0,vjust =0.5, label = "Within Communities") +
  annotate("text", x = 0.24, y = 0.8, hjust = 0,vjust =0.5, label = "Between Communities") +
  annotate("point", x = 0.27, y = 0.7, shape = 19, colour = "#F8766D") +
  annotate("point", x = 0.24, y = 0.7, shape = 17,colour = "#00BFC4")
g

ggsave("radii_samegroup.pdf",width = 6, height = 4, dpi = 500, units = "in", device='pdf')
