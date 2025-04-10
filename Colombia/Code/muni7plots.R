#### 1 Set-Up ####
home_dir = "~radius/Colombia/"
code_ext = "Code"
data_ext = "Data"
results_ext = "Results"
setwd(paste0(home_dir,code_ext))
library(readr)
library(dplyr)
library(igraph)
library(RColorBrewer)
library(gtools)
library(viridis)
library(ggplot2)

my_seed = 54301

## notes!!
## lots of things done by hand. 712 is the "origin" node, which may help when trying to fix things later

#### 2 Load and Process Data ####
#### 2.1 Load Data ####
setwd(paste0("../",data_ext))
network = read_csv("AttanasioEtAl2011Dyadic.csv")
setwd(paste0("../",results_ext))

#### 2.2 Subset Data ####
network7 = subset(network, subset = municode == 7)

#### 2.3 Clean Networks ####
nodes7 = unique(c(network7$iida,network7$iidb))
g_nodes = make_empty_graph(n = 0,directed = F) + vertices(nodes7)

## make the graph with the groups too
network7$either = ifelse(network7$frfam+network7$samegroup>0,1,0)
network7 = network7[,c("iida","iidb","frfam","samegroup","either")]
network7_group = subset(network7, subset = (either == 1))
g7_group = graph.data.frame(network7_group,directed = F)
g7_group = simplify(g7_group,remove.multiple = T,remove.loops = T,edge.attr.comb = "first")
g7_group = as.undirected(g7_group,mode = "collapse")
g7_group = igraph::union(g7_group,g_nodes)
## make graph for layout
g7_samegroup = delete_edges(g7_group,E(g7_group)[E(g7_group)$samegroup == 0])
g7_samegroup = simplify(g7_samegroup,remove.multiple = T,remove.loops = T)
g7_samegroup = as.undirected(g7_samegroup,mode = "collapse")
## color nodes with this
g7_comp = components(g7_samegroup)
## make graph for layout
g7_frfam = delete_edges(g7_group,E(g7_group)[E(g7_group)$frfam == 0])
g7_frfam = simplify(g7_frfam,remove.multiple = T,remove.loops = T)
g7_frfam = as.undirected(g7_frfam,mode = "collapse")

## make the normal graph
network7 = subset(network7, subset = (frfam == 1))
g7 = graph.data.frame(network7,directed = F)
g7 = simplify(g7,remove.multiple = T,remove.loops = T)
g7 = as.undirected(g7,mode = "collapse")
g7 = igraph::union(g7,g_nodes)

set.seed(my_seed)
cd7 = cluster_walktrap(g7,steps = 4)
cd_eb7 = cluster_edge_betweenness(g7)

#### 3 Vizualize Networks ####

edge_color = ifelse(E(g7_group)$frfam == 1, "black", "red")
#edge_color = ifelse(E(g7_group)$samegroup*E(g7_group)$frfam == 1, "green", edge_color)

#### 3.1 Group Network ####
degree7 = degree(g = g7)
labels_degree7 = as.character(degree7)
labels_degree7[7] = "O"

my_seed2 = 54303
pdf("group7.pdf", width = 10, height = 10)
set.seed(my_seed2)
plot(g7_group, layout = layout_nicely(g7_group), edge.arrow.size=0, vertex.size = 7.5,vertex.label=NA, edge.curved=0.1, edge.color=edge_color,vertex.color = g7_comp$membership)
dev.off()

pdf("group7_layout2.pdf", width = 10, height = 10)
set.seed(my_seed2)
plot(g7_group, layout = layout_nicely(g7_samegroup), edge.arrow.size=0, vertex.size = 7.5,vertex.label=NA, edge.curved=0.1, edge.color=edge_color,vertex.color = g7_comp$membership)
dev.off()

pdf("group7_layout3.pdf", width = 10, height = 10)
set.seed(my_seed2)
plot(g7_group, layout = layout_nicely(g7_frfam), edge.arrow.size=0, vertex.size = 7.5,vertex.label=NA, edge.curved=0.1, edge.color=edge_color,vertex.color = g7_comp$membership)
dev.off()

#tiff("group7.tiff", units="in", width=10, height=10, res=300)
#set.seed(my_seed2)
#plot(g7_group, layout = layout_nicely(g7_group), edge.arrow.size=0, vertex.size = 7.5,vertex.label=NA, edge.curved=0.1, edge.color=edge_color,vertex.color = g7_comp$membership)
#dev.off()

#tiff("group7_layout2.tiff", units="in", width=10, height=10, res=300)
#set.seed(my_seed2)
#plot(g7_group, layout = layout_nicely(g7_samegroup), edge.arrow.size=0, vertex.size = 7.5,vertex.label=NA, edge.curved=0.1, edge.color=edge_color,vertex.color = g7_comp$membership)
#dev.off()

#tiff("group7_layout3.tiff",  units="in", width=10, height=10, res=300)
#set.seed(my_seed2)
#plot(g7_group, layout = layout_nicely(g7_frfam), edge.arrow.size=0, vertex.size = 7.5,vertex.label=NA, edge.curved=0.1, edge.color=edge_color,vertex.color = g7_comp$membership)
#dev.off()

#setEPS()
#postscript("group7.eps",fonts=c("serif", "Palatino"),width = 10,height = 10)
#set.seed(my_seed2)
#plot(g7_group, layout = layout_nicely(g7_group), edge.arrow.size=0, 
#     vertex.size = 7.5,vertex.label=NA, edge.curved=0.1, 
#     edge.color=edge_color,vertex.color = g7_comp$membership)
#dev.off()

#### 3.2 Distances ####

distances7 = distances(g7)

my_resolution = 1000
my_palette = viridis::viridis(my_resolution,alpha = 1,begin = 0.33,end =1, direction = 1, option = "D")
distances_noninf = ifelse(distances7["712",] == Inf,NA,distances7["712",])
my_max = max(distances_noninf, na.rm = T)
my_min = min(distances_noninf, na.rm = T)
my_vector1 = (distances_noninf-my_min)/(my_max-my_min)
my_colors1 = my_palette[as.numeric(cut(my_vector1,breaks = my_resolution))]

my_seed = 54301
pdf("distance7.pdf", width = 10, height = 10)
set.seed(my_seed)
plot(cd7,g7, layout = layout_nicely, edge.arrow.size=0, vertex.size = 15,vertex.label=distances7["712",], edge.curved=0.1, edge.color="black",col = my_colors1)
dev.off()

#setEPS()
#postscript("distance7.eps",fonts=c("serif", "Palatino"),width = 10,height = 10)
#set.seed(my_seed)
#plot(g7, layout = layout_nicely, edge.arrow.size=0, vertex.size = 15,
#     vertex.label=distances7["712",], edge.curved=0.1, edge.color="black",
#     vertex.color = distances7["712",])
#dev.off()


supported7 = c(0,1,0,0,0,0,2,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0)
colors_support7 = c("white","deepskyblue","white","white","white","white","green3","white","white","deepskyblue","white","white","white","white","white","orange","deepskyblue","white","white","white","white","white")
labels_support7 = c(" ","SN"," "," "," "," ","O"," "," ","SN"," "," "," "," "," ","US","SN"," "," "," "," "," ")


pdf("supported7.pdf", width = 10, height = 10)
set.seed(my_seed)
plot(cd7,g7, layout = layout_nicely, edge.arrow.size=0, vertex.size = 15,
     vertex.label=labels_support7, edge.curved=0.1, edge.color="black",
     col = colors_support7)
dev.off()


#tiff("supported7.tiff", units = 'in',width = 10, height = 10,res = 300)
#set.seed(my_seed)
#plot(g7, layout = layout_nicely, edge.arrow.size=0, vertex.size = 15,vertex.label=labels_support7, edge.curved=0.1, edge.color="black",vertex.color = supported7)
#dev.off()


#setEPS()
#postscript("supported7.eps",fonts=c("serif", "Palatino"),width = 10,height = 10)
#set.seed(my_seed)
#plot(g7, layout = layout_nicely, edge.arrow.size=0, vertex.size = 15,
#     vertex.label=labels_support7, edge.curved=0.1, edge.color="black",
#     vertex.color = supported7)
#dev.off()


#### 3.3 Clustering ####
community7 = ifelse(cd7$membership == cd7$membership[7],1,0)
labels_community7 = ifelse(community7 == 1,"C"," ")
labels_community7[7] = "O"
colors_community = rep("palegreen2",length(nodes7))

pdf("community7.pdf", width = 10, height = 10)
set.seed(my_seed)
plot(cd7,g7, layout = layout_nicely, edge.arrow.size=0, vertex.size = 15,vertex.label=labels_community7, edge.curved=0.1, edge.color="black",col = colors_community)
dev.off()

#tiff("community7.tiff", units = 'in',width = 10, height = 10,res = 300)
#set.seed(my_seed)
#plot(g7, layout = layout_nicely, edge.arrow.size=0, vertex.size = 15,vertex.label=labels_community7, edge.curved=0.1, edge.color="black",vertex.color = cd7$membership)
#dev.off()

#setEPS()
#postscript("community7.eps",fonts=c("serif", "Palatino"),width = 10,height = 10)
#set.seed(my_seed)
#plot(g7, layout = layout_nicely, edge.arrow.size=0, vertex.size = 15,
#     vertex.label=labels_community7, edge.curved=0.1, edge.color="black",
#     vertex.color = cd7$membership)
#dev.off()

#### 3.4 Degree ####
pdf("degree7.pdf", width = 10, height = 10)
set.seed(my_seed)
plot(g7, layout = layout_nicely, edge.arrow.size=0, vertex.size = 15,vertex.label=labels_degree7, edge.curved=0.1, edge.color="black",vertex.color = degree7)
dev.off()

#tiff("degree7.tiff", units = 'in',width = 10, height = 10,res = 300)
#set.seed(my_seed)
#plot(g7, layout = layout_nicely, edge.arrow.size=0, vertex.size = 15,vertex.label=labels_degree7, edge.curved=0.1, edge.color="black",vertex.color = degree7)
#dev.off()

#setEPS()
#postscript("degree7.eps",fonts=c("serif", "Palatino"),width = 10,height = 10)
#set.seed(my_seed)
#plot(g7, layout = layout_nicely, edge.arrow.size=0, vertex.size = 15,
#     vertex.label=labels_degree7, edge.curved=0.1, edge.color="black",
#     vertex.color = degree7)
#dev.off()

#### 3.5 Betweenness ####
library(viridis)
betweenness7 = betweenness(g7)

my_resolution = 1000
my_palette = viridis::viridis(my_resolution,alpha = 1,begin = 0.33,end =1, direction = 1, option = "D")
my_max = max(betweenness7, na.rm = T)
my_min = min(betweenness7, na.rm = T)
my_vector1 = (betweenness7-my_min)/(my_max-my_min)
my_colors1 = my_palette[as.numeric(cut(my_vector1,breaks = my_resolution))]


pdf("betweenness7.pdf", width = 10, height = 10)
set.seed(my_seed)
plot(g7, layout = layout_nicely, edge.arrow.size=0.0, vertex.size = 15,vertex.label=round(betweenness7,1), edge.curved=0.1, edge.color="black",vertex.color = my_colors1)
dev.off()

#tiff("betweenness7.tiff", units = 'in',width = 10, height = 10,res = 300)
#set.seed(my_seed)
#plot(g7, layout = layout_nicely, edge.arrow.size=0.0, vertex.size = 15,vertex.label=round(betweenness7,1), edge.curved=0.1, edge.color="black",vertex.color = my_colors1)
#dev.off()

#setEPS()
#postscript("betweenness7.eps",fonts=c("serif", "Palatino"),width = 10,height = 10)
#set.seed(my_seed)
#plot(g7, layout = layout_nicely, edge.arrow.size=0.0, vertex.size = 15,
#     vertex.label=round(betweenness7,1), edge.curved=0.1, 
#     edge.color="black",vertex.color = my_colors1)
#dev.off()

## save this
pdf("dendrogram7.pdf", width = 5, height = 5)
plot_dendrogram(cd7)
title(ylab = "Cut", line = 2)
title(xlab = 'Node ID', line = 1)
dev.off()

gdf = as_tibble(data.frame(merge_number = 1:18,
                           modularity = cd7$modularity))

max_mod = round(max(gdf$modularity),3)

gg = gdf %>% 
  ggplot(mapping = aes(x = merge_number,y = modularity)) +
  geom_line() + xlab("Cut") + ylab("Modularity") +
  geom_vline(xintercept = 14,colour="grey20", linetype = "longdash") + 
  theme_minimal() + 
  annotate(x = 9,y = max_mod-0.03,geom = "text",label = paste0("Modularity maximized \nat cut 14 \nQ = ",max_mod))
gg
ggsave("modularity7.pdf",width = 5,height = 5,units = 'in',dpi = 600)

pdf("community_example7.pdf", width = 10, height = 10)
set.seed(my_seed)
plot(cd7,g7, layout = layout_nicely, edge.arrow.size=0, 
     vertex.size = 20, edge.curved=0.1, 
     edge.color="black")
dev.off()

pdf("eb_example7.pdf", width = 10, height = 10)
set.seed(my_seed)
plot(cd_eb7,g7, layout = layout_nicely, edge.arrow.size=0, 
     vertex.size = 20, edge.curved=0.1, 
     edge.color="black")
dev.off()



## merge graphs
## time to make a flip book

library(pals)
modularities = round(cd7$modularity,3)

#pdf("walktrap_merges_flipbook7.pdf", width = 10, height = 10)
for(s in 1:17){
  membership_s = cut_at(cd7, steps = s)
  palette_s = polychrome(n = max(membership_s))
  colors_s = palette_s[membership_s]
  splus1 = s + 1
  ptitle = paste0("Cut at ",splus1,". Modularity = ",modularities[splus1])
  
  pdf(paste0("walktrap_merges7_merge",s,".pdf"), width = 10, height = 10)
  set.seed(my_seed)
  plot(g7, vertex.color = colors_s,
       layout = layout_nicely, edge.arrow.size=0, 
       vertex.size = 20, edge.curved=0.1, 
       edge.color="black",
       main = ptitle)
  dev.off()
}







