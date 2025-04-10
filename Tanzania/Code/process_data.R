#### 0. Preliminary ####
##### 0.1 Set up workspace #####
home_dir = "~/radius/Tanzania/"
data_ext = "Data"
figure_ext = "Figures"
## update packages
#install.packages("readr")
#install.packages("tidyverse")
#install.packages("igraph")
#install.packages("RColorBrewer")
#install.packages("gtools")
#install.packages("stargazer")
#install.packages("ggplot2")
#install.packages("lfe")
## load packages
library(readr)
library(dplyr)
library(igraph)
library(RColorBrewer)
library(gtools)
library(stargazer)
library(ggplot2)
library(lfe)


##### 0.2 Functions #####
## function that builds closeness measure for multi-component graph
graph_closeness = function(graph){
  N = length(V(graph))
  d = distances(graph)
  c_total = 0
  n_total = 0
  for(i in 1:N){
    for(j in 1:N){
      if(i != j){
        if(d[i,j] != Inf){
          c_total = c_total + 1/d[i,j]
          n_total = n_total + 1
        }
      }
    }
  }
  c = c_total/n_total
  return(c)
}

lcu2ppp = 297.4

#### 1 Process Dyadic Data ####
#### 1.1 Load Data ####
#### 1.1.1 Dyadic Data ####
setwd(paste0(home_dir,data_ext))
dyadic = read_csv("dyadic.csv")
## desire to link (Comola and Fafchamps, 2014)
## but for now we use unilateral network
dyadic$frfam = NA
dyadic$frfam = ifelse(dyadic$reciprocal == "no link",0,1)
##  nodes
nodes = unique(c(dyadic$ha,dyadic$hb))

#### 1.1.2 Transfer Data ####
transfer_panel = read_csv("transfer_panel.csv")
transfer_panel = transfer_panel %>%
  filter(ha != "outside NYK" & hb != "outside NYK")

transfer_panel$ha = as.numeric(transfer_panel$ha)
transfer_panel$hb = as.numeric(transfer_panel$hb)

#### 1.1.3 Network Data ####
## load directed network and plot
network = read_csv("directed.csv")

##### 1.2 Compute Network Measures #####
## For each municipality:
##    build village network
##    detect communities
##    compute centrality: degree, ev, betweenness
##    find distance-2 connections
##    ...  distance-3 ...
##    collect other statistics at the network level

g = graph.data.frame(d = network,directed = F)
# remove any multiple links and self-loops
g = simplify(g,remove.multiple = T,remove.loops = T)
## make sure graph is undirected
g = as.undirected(g,mode = "collapse")

## make a new graph with all unique ids
g_nodes = make_empty_graph(n = 0,directed = F) + vertices(nodes)
## take the union to reintroduce singletons
g = igraph::union(g,g_nodes)

## convert to edgelist
el = as_edgelist(g)
el = data.frame(el)
el$frfam_alt = 1
el = as_tibble(el)
names(el) = c("ha","hb","frfam_alt")
el$ha = as.double(el$ha)
el$hb = as.double(el$hb)
el_b = el
names(el_b) = c("hb","ha","frfam_alt")
el_b = el_b %>% dplyr::select(ha,hb,frfam_alt)
el = bind_rows(el,el_b)
rm(el_b)

## run community detection algorithm
set.seed(54301)
cd = cluster_walktrap(g,steps = 4)
## store communities with network centralities
comm_a = data.frame(ha = as.numeric(V(g)$name),
                    comm_a = cd$membership,
                    deg_a = degree(g), 
                    ev_a = eigen_centrality(g)$vector, 
                    eb_a = betweenness(g))
comm_b = comm_a

## rename
names(comm_b) = c("hb","comm_b","deg_b","ev_b","eb_b")

## number of people in village
n = dim(comm_a)[1]

## construct all possible dyads:
##    use all permuations of unique id pairs
##    repeats = F means no self matches
comms = permutations(n,2,nodes,repeats = F)
comms = data.frame(comms)
names(comms) = c("ha","hb")
## join together frame with communities
comms = left_join(comms,comm_a, by = c("ha"))
comms = left_join(comms,comm_b, by = c("hb"))

## move muni network over to adjacency matrix
a = get.adjacency(g)
## square this matrix to get adj matrix of all distance 2 walks
a2 = a %*% a
## take out loops of distance 2
diag(a2) = 0
## ignore multiplicity of walks
a2[a2!=0] = 1 
## can also do av3: walks of distance 3
a3 = a2 %*% a
diag(a3) = 0
a3[a3!=0] = 1
## convert these matrix back to igraph objects
g2 <- graph.adjacency(a2)
g3 = graph.adjacency(a3)
## convert out to edgelist
el2 = as_edgelist(g2)
el2 = data.frame(el2)
names(el2) = c("ha","hb")
el2$ha = as.double(as.character(el2$ha))
el2$hb = as.double(as.character(el2$hb))

el3 = as_edgelist(g3)
el3 = data.frame(el3)
names(el3) = c("ha","hb")
el3$ha = as.double(as.character(el3$ha))
el3$hb = as.double(as.character(el3$hb))

## check if second degree connections exist
if(dim(el2)[1] == 0){
  ## build empty data frame here
  el2 <- data.frame(ha = double(),
                    hb = double(), 
                    frfam2 = double()) 
} else {
  ## fill data frame here
  el2$frfam2 = 1
}

## check if third degree connections exist (same)
if(dim(el3)[1] == 0){
  el3 <- data.frame(ha = double(),
                      hb = double(), 
                      frfam3 = double()) 
} else {
  el3$frfam3 = 1
}

#el3_b = el3
#names(el3_b) = c("hb","ha","frfam3")
#el3_b = el3_b %>% select(ha,hb,frfam3)
#el3 = bind_rows(el3,el3_b)
#rm(el3_b)

#### 2 Assemble and Save Dyadic Data ####
##### 2.1 Compile Data ##### 
## networks
comms = as_tibble(comms)
## distance-2 connections
el2 = as_tibble(el2)
## distance-3 connections
el3 = as_tibble(el3)

##### 2.2 Merge Datasets and Compute Dyadic Statistics #####
###### 2.2.1 Dyadic Data ######
comms$samecomm = ifelse(comms$comm_a == comms$comm_b,1,0)
dyadic = left_join(dyadic,comms,by = c("ha","hb"))
## does a path of length 2 from i reach j?
dyadic = left_join(dyadic,el2,by = c("ha","hb"))
dyadic$frfam2 = ifelse(is.na(dyadic$frfam2),0,dyadic$frfam2)
## does a path of length 3 from i reach j?
dyadic = left_join(dyadic,el3,by = c("ha","hb"))
dyadic$frfam3 = ifelse(is.na(dyadic$frfam3),0,dyadic$frfam3)
## is ij supported, i.e., directly connected and connected via a path of length 2
dyadic$supported = dyadic$frfam*dyadic$frfam2
## are i and j in any risk sharing groups together?
dyadic$samegroups = ifelse(dyadic$glinks > 0,1,0)
## now we amend frfam2 and frfam3 -- is there a closer path?
dyadic$frfam3 = ifelse(dyadic$frfam2 == 1 | dyadic$frfam == 1,0,dyadic$frfam3)
dyadic$frfam2 = ifelse(dyadic$frfam == 1,0,dyadic$frfam2)

## join el to check if alternative defitions works same way
dyadic = left_join(dyadic,el,by = c("ha","hb"))
dyadic$frfam_alt = ifelse(is.na(dyadic$frfam_alt),0,dyadic$frfam_alt)

#### 2.2.2 Transfer Data ####
#transfer_panel = transfer_panel %>%
#  group_by(ha,hb) %>% 
#  arrange(round,by_group = TRUE) %>%
#  mutate(bgiva_cumu = cumsum(bgiva))
  
transfers = transfer_panel %>% 
  group_by(ha,hb) %>% 
  summarise(.,bgiva = sum(bgiva,na.rm = T),
            arecb = sum(arecb,na.rm = T))

transfers2 = transfers
names(transfers2) = c("hb","ha","agivb","breca")
transfers = full_join(transfers,transfers2,by = c("ha","hb"))
rm(transfers2)
transfers$bgiva = ifelse(is.na(transfers$bgiva),0,transfers$bgiva)
transfers$arecb = ifelse(is.na(transfers$arecb),0,transfers$arecb)
transfers$agivb = ifelse(is.na(transfers$agivb),0,transfers$agivb)
transfers$breca = ifelse(is.na(transfers$breca),0,transfers$breca)

transfers$b_to_a = ifelse(transfers$bgiva>=transfers$arecb,transfers$bgiva,transfers$arecb)
transfers$a_to_b = ifelse(transfers$agivb>=transfers$breca,transfers$agivb,transfers$breca)
transfers$any_transfer = ifelse(transfers$a_to_b + transfers$b_to_a > 0,1,0)
transfers$reciprocal_transfer = ifelse(transfers$a_to_b*transfers$b_to_a > 0,1,0)
transfers$total_transfers = transfers$a_to_b + transfers$b_to_a


transfers = transfers %>% dplyr::select(ha,hb,any_transfer,reciprocal_transfer,total_transfers)

dyadic = left_join(dyadic,transfers,by = c("ha","hb"))
dyadic$any_transfer = ifelse(is.na(dyadic$any_transfer),0,dyadic$any_transfer)
dyadic$reciprocal_transfer = ifelse(is.na(dyadic$reciprocal_transfer),0,dyadic$reciprocal_transfer)
dyadic$total_transfers = ifelse(is.na(dyadic$total_transfers),0,dyadic$total_transfers)

dyadic$deg_sum = dyadic$deg_a + dyadic$deg_b
dyadic$deg_max = ifelse(dyadic$deg_a>=dyadic$deg_b,dyadic$deg_a,dyadic$deg_b)
dyadic$ev_sum = dyadic$ev_a + dyadic$ev_b
dyadic$eb_sum = dyadic$eb_a + dyadic$eb_b

dyadic$deg_diff = abs(dyadic$deg_a - dyadic$deg_b)
dyadic$ev_diff = abs(dyadic$ev_a - dyadic$ev_b)
dyadic$eb_diff = abs(dyadic$eb_a - dyadic$eb_b)

#### 2.2.3 Vector data ####
setwd(paste0(home_dir,data_ext))
covariates = read_csv("household.csv")
covariatesa = covariates %>% dplyr::select(hh,hhhage,hhhsex,hhhedu,religion,tribe,clancode,wealthTZS,cons)
names(covariatesa) = c("ha","hhhage_a","hhhsex_a","hhhedu_a","religion_a","tribe_a","clancode_a","wealth_a","cons_a")
covariatesb = covariates %>% dplyr::select(hh,hhhage,hhhsex,hhhedu,religion,tribe,clancode,wealthTZS,cons)
names(covariatesb) = c("hb","hhhage_b","hhhsex_b","hhhedu_b","religion_b","tribe_b","clancode_b","wealth_b","cons_b")
dyadic = left_join(dyadic,covariatesa,by = c("ha"))
dyadic = left_join(dyadic,covariatesb,by = c("hb"))
dyadic = dyadic %>% 
  mutate(age_sum = hhhage_a + hhhage_b,age_diff = abs(hhhage_a - hhhage_b),
         malehhh_a = ifelse(hhhsex_a == "male",1,0),malehhh_b = ifelse(hhhsex_b == "male",1,0),
         both_male = malehhh_a*malehhh_b,one_male = abs(malehhh_a - malehhh_b),
         highedu_a = ifelse(hhhedu_a == "finished primary" | hhhedu_a == "secondary",1,0),
         highedu_b = ifelse(hhhedu_b == "finished primary" | hhhedu_b == "secondary",1,0),
         both_highedu = highedu_a*highedu_b,
         both_lowedu = (1-highedu_a)*(1-highedu_b),
         one_highedu = abs(highedu_a - highedu_b),
         muslim_a = ifelse(religion_a == "Muslim",1,0),muslim_b = ifelse(religion_b == "Muslim",1,0),
         both_muslim = muslim_a*muslim_b,one_muslim = abs(muslim_a - muslim_b),
         same_tribe = ifelse(tribe_a == tribe_b,1,0),same_clan = ifelse(clancode_a == clancode_b,1,0),
         wealth_sum = wealth_a + wealth_b,wealth_diff = abs(wealth_a - wealth_b))

dyadic$consa_ppp = log((30.4/7)*dyadic$cons_a/lcu2ppp)
dyadic$consb_ppp = log((30.4/7)*dyadic$cons_b/lcu2ppp)
dyadic$difcons_ppp = abs(dyadic$consa_ppp - dyadic$consb_ppp)
dyadic$sumcons_ppp = dyadic$consa_ppp + dyadic$consb_ppp

#### 2.3 Save Dyadic Dataset ####
dyadic = dyadic %>% 
  dplyr::select(ha,hb,any_transfer,reciprocal_transfer,total_transfers,
                frfam,frfam2,frfam3,supported,samecomm,samegroups,
                age_sum,age_diff,both_male,one_male,both_highedu,both_lowedu,
                one_highedu,both_muslim,one_muslim,
                same_tribe,same_clan,wealth_sum,wealth_diff,
                difcons_ppp,sumcons_ppp)
write_csv(dyadic,file = "tanzania_dyadic_with_communities.csv",na = "")

#### 3 Network Statistics ####

summary(am_network <- lm(data = dyadic,formula = frfam ~ age_diff + age_sum + both_highedu + both_lowedu + difcons_ppp + sumcons_ppp))
summary(am_comm <- lm(data = dyadic,formula = samecomm ~ age_diff + age_sum + both_highedu + both_lowedu + difcons_ppp + sumcons_ppp))


#### 4 Preliminary Results ####
#### 4.1 Any Transfers ####
summary(main_specification <- 
  lm(data = dyadic,formula = any_transfer ~ supported + frfam + samecomm + samegroups))
summary(longer_walks <- 
          lm(data = dyadic,formula = any_transfer ~ supported + frfam + frfam2 + frfam3 + samecomm + samegroups))
summary(no_groups_interacted <- 
          lm(data = dyadic,formula = any_transfer ~ supported + frfam + frfam2 + frfam3 + samecomm +
               supported:samecomm + frfam:samecomm + frfam2:samecomm + frfam3:samecomm))
summary(fully_interacted <- 
          lm(data = dyadic,formula = any_transfer ~ supported + frfam + frfam2 + frfam3 + samecomm + samegroups +
             supported:samecomm + frfam:samecomm + frfam2:samecomm + frfam3:samecomm + samegroups:samecomm))

#### 4.2 Reciprocal Transfers ####
summary(main_specification <- 
          lm(data = dyadic,formula = reciprocal_transfer ~ supported + frfam + samecomm + samegroups))
summary(longer_walks <- 
          lm(data = dyadic,formula = reciprocal_transfer ~ supported + frfam + frfam2 + frfam3 + samecomm + samegroups))
summary(no_groups_interacted <- 
          lm(data = dyadic,formula = reciprocal_transfer ~ supported + frfam + frfam2 + frfam3 + samecomm +
               supported:samecomm + frfam:samecomm + frfam2:samecomm + frfam3:samecomm))
summary(fully_interacted <- 
          lm(data = dyadic,formula = reciprocal_transfer ~ supported + frfam + frfam2 + frfam3 + samecomm + samegroups +
               supported:samecomm + frfam:samecomm + frfam2:samecomm + frfam3:samecomm + samegroups:samecomm))

#### 4.3 Total Bilateral Transfers #### 
summary(main_specification <- 
          lm(data = dyadic,formula = total_transfers ~ supported + frfam + samecomm + samegroups))
summary(longer_walks <- 
          lm(data = dyadic,formula = total_transfers ~ supported + frfam + frfam2 + frfam3 + samecomm + samegroups))
summary(no_groups_interacted <- 
          lm(data = dyadic,formula = total_transfers ~ supported + frfam + frfam2 + frfam3 + samecomm +
               supported:samecomm + frfam:samecomm + frfam2:samecomm + frfam3:samecomm))
summary(fully_interacted <- 
          lm(data = dyadic,formula = total_transfers ~ supported + frfam + frfam2 + frfam3 + samecomm + samegroups +
               supported:samecomm + frfam:samecomm + frfam2:samecomm + frfam3:samecomm + samegroups:samecomm))


#### plots ####
setwd(paste0(home_dir,figure_ext))
my_seed = 54304
pdf("nyatakote_communities.pdf", width = 10, height = 10)
weights = ifelse(crossing(cd,g), 1, 10)
layout = layout_nicely(g, weights=weights)
set.seed(my_seed)
plot(cd,g, layout = layout, edge.arrow.size=0, vertex.size = 5,edge.curved=0.05, edge.color="grey20",vertex.label = NA)
dev.off()

png("nyatakote_communities.png", width = 10, height = 10,units = 'in',res = 500)
weights = ifelse(crossing(cd,g), 1, 10)
layout = layout_nicely(g, weights=weights)
set.seed(my_seed)
plot(cd,g, layout = layout, edge.arrow.size=0, vertex.size = 5,edge.curved=0.05, edge.color="grey20",vertex.label = NA)
dev.off()





setwd(paste0(home_dir,figure_ext))
gdf = data.frame(degree = degree(g))

gg = gdf %>%
  ggplot(aes(x = degree)) + geom_histogram(bins = 34) + 
  theme_minimal()
gg
ggsave(filename = "degree_distribuion.pdf",width = 6,height = 4,device = "pdf",units = "in",dpi = 600)

