#################################
# Italian, experiment 1
#################################

library(tidyverse)
library(gridExtra)

#colors
red = "#e41a1c"
light.red = "#f4a582"
middle = "#bababa"
white = "#ffffff"
light.blue = "#92c5de"
blue= "#377eb8"

#read in the data, make formatting changes
data=read_csv("Ne.ASC.Experiment.csv") %>%
  filter(experiment == 2 & (NeNon=="ne" | NeNon=="non")) %>%
  droplevels() %>%
  mutate(condition = fct_recode(NeNon, "control"="non", "ne" ="ne")) %>%
  mutate(condition = fct_relevel(condition, "control" ))%>%
  mutate(verb.class = fct_recode(as.factor(category), "Class 1"="1", "Class 2"="2", "Class 3"="3", "Class 4" ="4", "Class 5" = "5")) %>%
  mutate(verb = fct_relevel(verb, "arrivare","venire","cadere","entrare","morire","nascere","fiorire","marcire","rimanere","sopravvivere","bastare","apparire","ballare","nuotare","volare", "correre", "ridere", "lavorare", "suonare", "telefonare")) %>%
  mutate(verb = fct_recode(verb, "soprav." = "sopravvivere", "tele."="telefonare"))

###################
#bar plot of conditions by theoretical classes raw scores
###################

#subsets
c1 = filter(data, verb.class == "Class 1") %>% 
  droplevels()%>%
  group_by(verb.class, verb, condition) %>%
  summarise(mean = mean(judgment, na.rm=TRUE), se = sd(judgment, na.rm=TRUE)/sqrt(n()))

c2 = filter(data, verb.class == "Class 2") %>% 
  droplevels()%>%
  group_by(verb.class, verb, condition) %>%
  summarise(mean = mean(judgment, na.rm=TRUE), se = sd(judgment, na.rm=TRUE)/sqrt(n()))

c3 = filter(data, verb.class == "Class 3") %>% 
  droplevels()%>%
  group_by(verb.class, verb, condition) %>%
  summarise(mean = mean(judgment, na.rm=TRUE), se = sd(judgment, na.rm=TRUE)/sqrt(n()))

c4 = filter(data, verb.class == "Class 4") %>% 
  droplevels()%>%
  group_by(verb.class, verb, condition) %>%
  summarise(mean = mean(judgment, na.rm=TRUE), se = sd(judgment, na.rm=TRUE)/sqrt(n()))

c5 = filter(data, verb.class == "Class 5") %>% 
  droplevels()%>%
  group_by(verb.class, verb, condition) %>%
  summarise(mean = mean(judgment, na.rm=TRUE), se = sd(judgment, na.rm=TRUE)/sqrt(n()))


#add gray lines for fillers?
#the data set doesn't have the fillers in it, so we can't do this yet

# bounds=read_csv("mandarin.exp1.zscores.csv") %>%
#   filter(expected == "good" | expected == "bad") %>%
#   droplevels() %>%
#   group_by(code) %>%
#   summarise(item.mean = mean(zscores, na.rm=T)) %>%
#   ungroup() %>%
#   summarise(min = min(item.mean), max = max(item.mean))

#C1 plot with legend

lower = 1
upper = 7

c1.plot = ggplot(c1, aes(x = verb, y = mean, group=condition, fill = condition)) +
  #geom_hline(yintercept = bounds$min, size=1, color=gray(.75)) +
  #geom_hline(yintercept = bounds$max, size=1, color=gray(.75)) +
  geom_col(position="dodge", color="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3, position=position_dodge(.9)) +
  scale_fill_manual(values = c(gray(.75), blue)) +
  coord_cartesian(ylim = c(lower, upper)) +
  scale_y_continuous(breaks=c(lower:upper), labels=c(lower:upper)) +
  facet_grid(.~verb.class) +
  theme_bw() +
  theme(#legend.position = c(.4, .95),
    #legend.direction = "horizontal",
    legend.position = "none",
    #legend.background = element_blank(),
    #legend.key.size = unit(.3, "cm"),
    #legend.text = element_text(size = 7),
    #legend.title = element_blank(), 
    axis.title.x=element_blank(), 
    axis.title.y=element_blank())

c2.plot = ggplot(c2, aes(x = verb, y = mean, group=condition, fill = condition)) +
  #geom_hline(yintercept = bounds$min, size=1, color=gray(.75)) +
  #geom_hline(yintercept = bounds$max, size=1, color=gray(.75)) +
  geom_col(position="dodge", color="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3, position=position_dodge(.9)) +
  scale_fill_manual(values = c(gray(.75), light.blue)) +
  coord_cartesian(ylim = c(lower, upper)) +
  scale_y_continuous(breaks=c(lower:upper), labels=c(lower:upper)) +
  facet_grid(.~verb.class) +
  theme_bw()+
  theme(legend.position ="none", 
        legend.title = element_blank(), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

c3.plot = ggplot(c3, aes(x = verb, y = mean, group=condition, fill = condition)) +
  #geom_hline(yintercept = bounds$min, size=1, color=gray(.75)) +
  #geom_hline(yintercept = bounds$max, size=1, color=gray(.75)) +
  geom_col(position="dodge", color="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3, position=position_dodge(.9)) +
  scale_fill_manual(values = c(gray(.75), white)) +
  coord_cartesian(ylim = c(lower, upper)) +
  scale_y_continuous(breaks=c(lower:upper), labels=c(lower:upper)) +
  facet_grid(.~verb.class) +
  theme_bw()+
  theme(legend.position ="none", 
        legend.title = element_blank(), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

c4.plot = ggplot(c4, aes(x = verb, y = mean, group=condition, fill = condition)) +
  #geom_hline(yintercept = bounds$min, size=1, color=gray(.75)) +
  #geom_hline(yintercept = bounds$max, size=1, color=gray(.75)) +
  geom_col(position="dodge", color="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3, position=position_dodge(.9)) +
  scale_fill_manual(values = c(gray(.75), light.red)) +
  scale_y_continuous(breaks=c(lower:upper), labels=c(lower:upper)) +
  coord_cartesian(ylim = c(lower, upper)) +
  facet_grid(.~verb.class) +
  theme_bw()+
  theme(legend.position ="none", 
        legend.title = element_blank(), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

c5.plot = ggplot(c5, aes(x = verb, y = mean, group=condition, fill = condition)) +
  #geom_hline(yintercept = bounds$min, size=1, color=gray(.75)) +
  #geom_hline(yintercept = bounds$max, size=1, color=gray(.75)) +
  geom_col(position="dodge", color="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3, position=position_dodge(.9)) +
  scale_fill_manual(values = c(gray(.75), red)) +
  scale_y_continuous(breaks=c(lower:upper), labels=c(lower:upper)) +
  coord_cartesian(ylim = c(lower, upper)) +
  facet_grid(.~verb.class) +
  theme_bw() +
  theme(legend.position = c(.2, .95),
        #legend.direction = "horizontal",
        #legend.position = "none",
        legend.background = element_blank(),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(size = 7),
        legend.title = element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#arrange the grid
quartz(width=12, height=3)
grid.arrange(c1.plot, c2.plot, c3.plot, c4.plot, c5.plot,
             nrow=1,
             top = textGrob("Condition means by verb and category - NE-experiment 1", vjust = 1, gp = gpar(fontface = "bold", cex = 1.0)),
             left = textGrob("Results for individual verb - raw scores", rot = 90, vjust = 1, gp = gpar(fontsize = 8)))


#save
f <- arrangeGrob(c1.plot, c2.plot, c3.plot, c4.plot, c5.plot,
                 nrow=1,
                 top = textGrob("Condition means by verb and category - NE-experiment 1", vjust = 1, gp = gpar(fontface = "bold", cex = 1.0)),
                 left = textGrob("Results for individual verb - raw scores", rot = 90, vjust = 1, gp = gpar(fontsize = 8)))
ggsave(file="NE.conditions.by.theory.classes.rawscores.pdf", f, height=3, width=12)


###################
#bar plot of conditions by theoretical classes
###################

#subsets
c1 = filter(data, verb.class == "Class 1") %>% 
  droplevels()%>%
  group_by(verb.class, verb, condition) %>%
  summarise(mean = mean(zscores, na.rm=TRUE), se = sd(zscores, na.rm=TRUE)/sqrt(n()))

c2 = filter(data, verb.class == "Class 2") %>% 
  droplevels()%>%
  group_by(verb.class, verb, condition) %>%
  summarise(mean = mean(zscores, na.rm=TRUE), se = sd(zscores, na.rm=TRUE)/sqrt(n()))

c3 = filter(data, verb.class == "Class 3") %>% 
  droplevels()%>%
  group_by(verb.class, verb, condition) %>%
  summarise(mean = mean(zscores, na.rm=TRUE), se = sd(zscores, na.rm=TRUE)/sqrt(n()))

c4 = filter(data, verb.class == "Class 4") %>% 
  droplevels()%>%
  group_by(verb.class, verb, condition) %>%
  summarise(mean = mean(zscores, na.rm=TRUE), se = sd(zscores, na.rm=TRUE)/sqrt(n()))

c5 = filter(data, verb.class == "Class 5") %>% 
  droplevels()%>%
  group_by(verb.class, verb, condition) %>%
  summarise(mean = mean(zscores, na.rm=TRUE), se = sd(zscores, na.rm=TRUE)/sqrt(n()))


#add gray lines for fillers?
#the data set doesn't have the fillers in it, so we can't do this yet

# bounds=read_csv("mandarin.exp1.zscores.csv") %>%
#   filter(expected == "good" | expected == "bad") %>%
#   droplevels() %>%
#   group_by(code) %>%
#   summarise(item.mean = mean(zscores, na.rm=T)) %>%
#   ungroup() %>%
#   summarise(min = min(item.mean), max = max(item.mean))

#C1 plot with legend

lower = -1.5
upper = 1.5

c1.plot = ggplot(c1, aes(x = verb, y = mean, group=condition, fill = condition)) +
  #geom_hline(yintercept = bounds$min, size=1, color=gray(.75)) +
  #geom_hline(yintercept = bounds$max, size=1, color=gray(.75)) +
  geom_col(position="dodge", color="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3, position=position_dodge(.9)) +
  scale_fill_manual(values = c(gray(.75), blue)) +
  scale_y_continuous(limits=c(lower, upper)) +
  facet_grid(.~verb.class) +
  theme_bw() +
  theme(#legend.position = c(.4, .95),
    legend.position = "none",
    #legend.direction = "horizontal",
    #legend.background = element_blank(),
    #legend.key.size = unit(.3, "cm"),
    #legend.text = element_text(size = 7),
    #legend.title = element_blank(), 
    axis.title.x=element_blank(), 
    axis.title.y=element_blank())
  #theme(legend.position = c(.25, .15), legend.background = element_blank(), legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

c2.plot = ggplot(c2, aes(x = verb, y = mean, group=condition, fill = condition)) +
  #geom_hline(yintercept = bounds$min, size=1, color=gray(.75)) +
  #geom_hline(yintercept = bounds$max, size=1, color=gray(.75)) +
  geom_col(position="dodge", color="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3, position=position_dodge(.9)) +
  scale_fill_manual(values = c(gray(.75), light.blue)) +
  scale_y_continuous(limits=c(lower, upper)) +
  facet_grid(.~verb.class) +
  theme_bw()+
  theme(legend.position ="none", 
        legend.title = element_blank(), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

c3.plot = ggplot(c3, aes(x = verb, y = mean, group=condition, fill = condition)) +
  #geom_hline(yintercept = bounds$min, size=1, color=gray(.75)) +
  #geom_hline(yintercept = bounds$max, size=1, color=gray(.75)) +
  geom_col(position="dodge", color="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3, position=position_dodge(.9)) +
  scale_fill_manual(values = c(gray(.75), white)) +
  scale_y_continuous(limits=c(lower, upper)) +
  facet_grid(.~verb.class) +
  theme_bw()+
  theme(legend.position ="none", 
        legend.title = element_blank(), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

c4.plot = ggplot(c4, aes(x = verb, y = mean, group=condition, fill = condition)) +
  #geom_hline(yintercept = bounds$min, size=1, color=gray(.75)) +
  #geom_hline(yintercept = bounds$max, size=1, color=gray(.75)) +
  geom_col(position="dodge", color="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3, position=position_dodge(.9)) +
  scale_fill_manual(values = c(gray(.75), light.red)) +
  scale_y_continuous(limits=c(lower, upper)) +
  facet_grid(.~verb.class) +
  theme_bw()+
  theme(legend.position ="none", 
        legend.title = element_blank(), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

c5.plot = ggplot(c5, aes(x = verb, y = mean, group=condition, fill = condition)) +
  #geom_hline(yintercept = bounds$min, size=1, color=gray(.75)) +
  #geom_hline(yintercept = bounds$max, size=1, color=gray(.75)) +
  geom_col(position="dodge", color="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3, position=position_dodge(.9)) +
  scale_fill_manual(values = c(gray(.75), red)) +
  scale_y_continuous(limits=c(lower, upper)) +
  facet_grid(.~verb.class) +
  theme_bw() +
  theme(legend.position = c(.2, .95),
        #legend.direction = "horizontal",
        #legend.position = "none",
        legend.background = element_blank(),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(size = 7),
        legend.title = element_blank(), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#arrange the grid
quartz(width=12, height=3)
grid.arrange(c1.plot, c2.plot, c3.plot, c4.plot, c5.plot,
             nrow=1,
             top = textGrob("Condition means by verb and category - NE-experiment 1", vjust = 1, gp = gpar(fontface = "bold", cex = 1.0)),
             left = textGrob("Results for individual verb - zscores", rot = 90, vjust = 1, gp = gpar(fontsize = 8)))


#save
g <- arrangeGrob(c1.plot, c2.plot, c3.plot, c4.plot, c5.plot, 
                 nrow=1,
                 top = textGrob("Condition means by verb and category - NE-experiment 1", vjust = 1, gp = gpar(fontface = "bold", cex = 1.0)),
                 left = textGrob("Results for individual verb - zscores", rot = 90, vjust = 1, gp = gpar(fontsize = 8)))
ggsave(file="NE.conditions.by.theory.classes.pdf", g, height=3, width=12)


###################
#bar plot of differences by theoretical classes
###################

#calculate differences
differences = data %>%
  select(subject,verb.class, verb, condition, zscores) %>%
  pivot_wider(names_from = "condition", values_from = "zscores") %>%
  mutate(difference = `control` - `ne`) %>%
  group_by(verb.class, verb) %>%
  summarise(mean = mean(difference, na.rm=TRUE), se = sd(difference, na.rm=TRUE)/sqrt(n())) %>%
  ungroup()

#subsets
d1 = filter(differences, verb.class == "Class 1") %>% droplevels()
d2 = filter(differences, verb.class == "Class 2") %>% droplevels()
d3 = filter(differences, verb.class == "Class 3") %>% droplevels()
d4 = filter(differences, verb.class == "Class 4") %>% droplevels()
d5 = filter(differences, verb.class == "Class 5") %>% droplevels()

#C1 plot with legend

upper = 1.5
lower = -1.5

d1.plot = ggplot(d1, aes(x = verb, y = mean)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", fill=blue, col="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  facet_grid(.~verb.class) +
  theme_bw() +
  theme(legend.position ="none", axis.title.x=element_blank(), axis.title.y=element_blank())

d2.plot = ggplot(d2, aes(x = verb, y = mean)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", fill=light.blue, col="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  facet_grid(.~verb.class) +
  theme_bw() +
  theme(legend.position ="none", 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

d3.plot = ggplot(d3, aes(x = verb, y = mean)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", fill=white, col="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  facet_grid(.~verb.class) +
  theme_bw() +
  theme(legend.position ="none", 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

d4.plot = ggplot(d4, aes(x = verb, y = mean)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", fill=light.red, col="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  facet_grid(.~verb.class) +
  theme_bw() +
  theme(legend.position ="none", 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

d5.plot = ggplot(d5, aes(x = verb, y = mean)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", fill=red, col="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  facet_grid(.~verb.class) +
  theme_bw() +
  theme(legend.position ="none", 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#arrange the grid
quartz(width=12, height=3)
grid.arrange(d1.plot, d2.plot, d3.plot, d4.plot, d5.plot, 
             nrow=1,
             top = textGrob("Difference between conditions - NE-experiment 1", vjust = 1, gp = gpar(fontface = "bold", cex = 1.0)),
             left = textGrob("Mean of differences - zscores", rot = 90, vjust = 1, gp = gpar(fontsize = 8)))


#save
h <- arrangeGrob(d1.plot, d2.plot, d3.plot, d4.plot, d5.plot, 
                 nrow=1,
                 top = textGrob("Difference between conditions - NE-experiment 1", vjust = 1, gp = gpar(fontface = "bold", cex = 1.0)),
                 left = textGrob("Mean of differences - zscores", rot = 90, vjust = 1, gp = gpar(fontsize = 8)))

ggsave(file="NE.differences.by.theory.classes.pdf", h, height=3, width=12)

##################
#clustering and dendrogram
#cluster over differences because we think 1-vs-.75 should be the same as .5-vs.25
##################

library(NbClust)
library(dendextend)
library(factoextra)

#data for clusters
cdata = differences %>%
  select(verb, mean) %>%
  column_to_rownames(var = "verb")

#use hclust to create the clusters, set the order of leaves by hand...
clusters <- hclust(dist(cdata)) %>%
  as.dendrogram %>%
  rotate(c(4,3,6,7,8,9,5,13,14,15,11,12,17,16,18,20,19,10,2,1))


#look at it
plot(clusters)

#create a ggdend object
tree <- as.ggdend(clusters)

#creating a new data set for the segments lets me label them for fake faceting (to get the face-like title)
plottable = tree$segments %>%
  mutate(type = rep("Hierarchical clustering of difference scores", times=n()))

#call the segments and label text separately so that we have maximum flexibility; also put a white rectangle before them to hide the grid lines below 0, and some gray bars to indicate the 2/5 clusters 

barheight1=-.225
barheight2=-.275

tree.plot <- ggplot(plottable) +
  geom_rect(xmin=.2, xmax=20.8, ymin=-.75, ymax=-.1, fill=white)+
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
  facet_grid(.~type)+
  geom_text(data = tree$labels, aes(x, y, label = label),
            hjust = 1.1, angle = 90, size=3, col=c(middle, blue, blue, light.red, #5.25
                                                   blue, red, middle, light.blue, 
                                                   light.blue, light.blue, blue, 
                                                   light.red, middle, middle, 
                                                   light.blue, light.red, light.red, 
                                                   red, red, red))+
  scale_y_continuous(name="distance", limits=c(-.25, .75), breaks=seq(0, .75, by=.25)) +
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank()) +
  geom_segment(x=.6, xend=18.4, y=barheight1, yend=barheight1, color=middle, lwd=1.5) +
  geom_segment(x=18.6, xend=20.4, y=barheight1, yend=barheight1, color=middle, lwd=1.5) +
  geom_segment(x=.6, xend=2.4, y=barheight2, yend=barheight2, color=middle, lwd=1.5) +
  geom_segment(x=2.6, xend=7.4, y=barheight2, yend=barheight2, color=middle, lwd=1.5) +
  geom_segment(x=7.6, xend=17.4, y=barheight2, yend=barheight2, color=middle, lwd=1.5) +
  geom_segment(x=17.6, xend=18.4, y=barheight2, yend=barheight2, color=middle, lwd=1.5) +
  geom_segment(x=18.6, xend=20.4, y=barheight2, yend=barheight2, color=middle, lwd=1.5)

ggsave(plot=tree.plot, file="NE.differences.clusters.pdf", height=3.5, width=4)

##########################
#compare AIC of different numbers of clusters?
##########################

#get the cluster labels into the data set

#get 2 clusters
groups.two = cutree(clusters, k=2)
groups.two.df = data.frame(verb = names(groups.two), two = groups.two, row.names=c())

#get 3 clusters
groups.three = cutree(clusters, k=3)
groups.three.df = data.frame(verb = names(groups.three), three = groups.three, row.names=c())

#get 4 clusters
groups.four = cutree(clusters, k=4)
groups.four.df = data.frame(verb = names(groups.four), four = groups.four, row.names=c())

#get 5 clusters
groups.five = cutree(clusters, k=5)
groups.five.df = data.frame(verb = names(groups.five), five = groups.five, row.names=c())

#get 6 clusters
groups.six = cutree(clusters, k=6)
groups.six.df = data.frame(verb = names(groups.six), six = groups.six, row.names=c())

#get 7 clusters
groups.seven = cutree(clusters, k=7)
groups.seven.df = data.frame(verb = names(groups.seven), seven = groups.seven, row.names=c())

#get 8 clusters
groups.eight = cutree(clusters, k=8)
groups.eight.df = data.frame(verb = names(groups.eight), eight = groups.eight, row.names=c())

#get 9 clusters
groups.nine = cutree(clusters, k=9)
groups.nine.df = data.frame(verb = names(groups.nine), nine = groups.nine, row.names=c())

#get 10 clusters
groups.ten = cutree(clusters, k=10)
groups.ten.df = data.frame(verb = names(groups.ten), ten = groups.ten, row.names=c())

#get 11 clusters
groups.eleven = cutree(clusters, k=11)
groups.eleven.df = data.frame(verb = names(groups.eleven), eleven = groups.eleven, row.names=c())

#get 12 clusters
groups.twelve = cutree(clusters, k=12)
groups.twelve.df = data.frame(verb = names(groups.twelve), twelve = groups.twelve, row.names=c())

#get 13 clusters
groups.thirteen = cutree(clusters, k=13)
groups.thirteen.df = data.frame(verb = names(groups.thirteen), thirteen = groups.thirteen, row.names=c())

#get 14 clusters
groups.fourteen = cutree(clusters, k=14)
groups.fourteen.df = data.frame(verb = names(groups.fourteen), fourteen = groups.fourteen, row.names=c())

#get 15 clusters
groups.fifteen = cutree(clusters, k=15)
groups.fifteen.df = data.frame(verb = names(groups.fifteen), fifteen = groups.fifteen, row.names=c())

#get 16 clusters
groups.sixteen = cutree(clusters, k=16)
groups.sixteen.df = data.frame(verb = names(groups.sixteen), sixteen = groups.sixteen, row.names=c())

#get 17 clusters
groups.seventeen = cutree(clusters, k=17)
groups.seventeen.df = data.frame(verb = names(groups.seventeen), seventeen = groups.seventeen, row.names=c())

#get 18 clusters
groups.eighteen = cutree(clusters, k=18)
groups.eighteen.df = data.frame(verb = names(groups.eighteen), eighteen = groups.eighteen, row.names=c())

#get 19 clusters
groups.nineteen = cutree(clusters, k=19)
groups.nineteen.df = data.frame(verb = names(groups.nineteen), nineteen = groups.nineteen, row.names=c())

#get 20 clusters
groups.twenty = cutree(clusters, k=20)
groups.twenty.df = data.frame(verb = names(groups.twenty), twenty = groups.twenty, row.names=c())

#add cluster columns to the original data
data.cluster.columns = data %>%
  left_join(groups.two.df) %>%
  left_join(groups.three.df) %>%
  left_join(groups.four.df) %>%
  left_join(groups.five.df) %>%
  left_join(groups.six.df) %>%
  left_join(groups.seven.df) %>%
  left_join(groups.eight.df) %>%
  left_join(groups.nine.df) %>%
  left_join(groups.ten.df) %>%
  left_join(groups.eleven.df) %>%
  left_join(groups.twelve.df) %>%
  left_join(groups.thirteen.df) %>%
  left_join(groups.fourteen.df) %>%
  left_join(groups.fifteen.df) %>%
  left_join(groups.sixteen.df) %>%
  left_join(groups.seventeen.df) %>%
  left_join(groups.eighteen.df) %>%
  left_join(groups.nineteen.df) %>%
  left_join(groups.twenty.df) %>%
  mutate(two = as.factor(two)) %>%
  mutate(three = as.factor(three)) %>%
  mutate(four = as.factor(four)) %>%
  mutate(five = as.factor(five)) %>%
  mutate(six = as.factor(six)) %>%
  mutate(seven = as.factor(seven)) %>%
  mutate(eight = as.factor(eight)) %>%
  mutate(nine = as.factor(nine)) %>%
  mutate(ten = as.factor(ten)) %>%
  mutate(eleven = as.factor(eleven)) %>%
  mutate(twelve = as.factor(twelve)) %>%
  mutate(thirteen = as.factor(thirteen)) %>%
  mutate(fourteen = as.factor(fourteen)) %>%
  mutate(fifteen = as.factor(fifteen)) %>%
  mutate(sixteen = as.factor(sixteen)) %>%
  mutate(seventeen = as.factor(seventeen)) %>%
  mutate(eighteen = as.factor(eighteen)) %>%
  mutate(nineteen = as.factor(nineteen)) %>%
  mutate(twenty = as.factor(twenty)) %>%
  select(subject, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve, thirteen, fourteen, fifteen, sixteen, seventeen, eighteen, nineteen, twenty, verb, condition, item, zscores) 


####Models with conditions?

library(wiqid)
library(lmerTest)

one.lm=lmer(zscores~condition+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, item, verb, zscores))

two.lm = lmer(zscores~condition*two+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, two, item, verb, zscores))

three.lm = lmer(zscores~condition*three+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, three, item, verb, zscores))

four.lm = lmer(zscores~condition*four+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, four, item, verb, zscores))

five.lm = lmer(zscores~condition*five+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, five, item, verb, zscores))

six.lm = lmer(zscores~condition*six+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, six, item, verb, zscores))

seven.lm = lmer(zscores~condition*seven+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, seven, item, verb, zscores))

eight.lm = lmer(zscores~condition*eight+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, eight, item, verb, zscores))

nine.lm = lmer(zscores~condition*nine+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, nine, item, verb, zscores))

ten.lm = lmer(zscores~condition*ten+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, ten, item, verb, zscores))

eleven.lm = lmer(zscores~condition*eleven+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, eleven, item, verb, zscores))

twelve.lm = lmer(zscores~condition*twelve+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, twelve, item, verb, zscores))

thirteen.lm = lmer(zscores~condition*thirteen+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, thirteen, item, verb, zscores))

fourteen.lm = lmer(zscores~condition*fourteen+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, fourteen, item, verb, zscores))

fifteen.lm = lmer(zscores~condition*fifteen+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, fifteen, item, verb, zscores))

sixteen.lm = lmer(zscores~condition*sixteen+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, sixteen, item, verb, zscores))

seventeen.lm = lmer(zscores~condition*seventeen+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, seventeen, item, verb, zscores))

eighteen.lm = lmer(zscores~condition*eighteen+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, eighteen, item, verb, zscores))

nineteen.lm = lmer(zscores~condition*nineteen+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, nineteen, item, verb, zscores))

twenty.lm = lmer(zscores~condition*twenty+(1|subject)+(1|item), data = data.cluster.columns%>%select(subject, condition, twenty, item, verb, zscores))



#calculate AIC (original and corrected) and make it into a plottable vector
aics = tibble(AIC=c(
  AICc(one.lm),
  AICc(two.lm),
  AICc(three.lm),
  AICc(four.lm),
  AICc(five.lm),
  AICc(six.lm),
  AICc(seven.lm),
  AICc(eight.lm),
  AICc(nine.lm),
  AICc(ten.lm),
  AICc(eleven.lm),
  AICc(twelve.lm),
  AICc(thirteen.lm),
  AICc(fourteen.lm),
  AICc(fifteen.lm),
  AICc(sixteen.lm),
  AICc(seventeen.lm),
  AICc(eighteen.lm),
  AICc(nineteen.lm),
  AICc(twenty.lm),
  AIC(one.lm),
  AIC(two.lm),
  AIC(three.lm),
  AIC(four.lm),
  AIC(five.lm),
  AIC(six.lm),
  AIC(seven.lm),
  AIC(eight.lm),
  AIC(nine.lm),
  AIC(ten.lm),
  AIC(eleven.lm),
  AIC(twelve.lm),
  AIC(thirteen.lm),
  AIC(fourteen.lm),
  AIC(fifteen.lm),
  AIC(sixteen.lm),
  AIC(seventeen.lm),
  AIC(eighteen.lm),
  AIC(nineteen.lm), 
  AIC(twenty.lm)),
  clusters=rep(c(1:20), times=2),
  measure = c(rep(c("original", "corrected"), each=20)))%>%
  mutate(measure = fct_relevel(measure, "original")) %>%
  mutate(model = rep("AIC for LMER by class number", times=40))


#plot a point/line plot

upper.aics = max(aics$AIC)
lower.aics = min(aics$AIC)

ASC.aic.plot = ggplot(aics, aes(x=clusters, y=AIC, group=measure, color=measure)) +
  facet_grid(.~model) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_color_manual(values=c(blue,red)) +
  scale_x_continuous(limits=c(1,20), breaks=c(1:20), labels=c(1:20)) +
  scale_y_continuous(limits=c(lower.aics, upper.aics)) +
  ylab("AIC") +
  xlab("number of clusters") +
  theme(legend.position = c(.80, .50), legend.background = element_blank(), legend.title = element_blank(), axis.title.y=element_blank())

quartz(height=3, width=4)
ASC.aic.plot

ggsave(file="NE.aic.plot.pdf", ASC.aic.plot, height=3, width=4)


#########################
#Second approach: cluster validation from NbClust
#########################

#It can only run between 2 and 19 (not 1:20)
min=2
max=19

kl = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=19, index="kl")

ch = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="ch")

hartigan = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="hartigan")

ccc = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="ccc" )

scott = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="scott")

marriot = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="marriot")

tracew = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="tracew")

friedman = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="friedman")

rubin = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="rubin")

cindex = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="cindex")

db = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="db")

silhouette = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="silhouette")

duda = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="duda")

#beale = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="beale")

ratkowsky = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="ratkowsky")

ball = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="ball")

ptbiserial = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="ptbiserial")

#frey = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="frey")

mcclain = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="mcclain")

gamma = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="gamma")

gplus = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="gplus")

tau = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="tau")

dunn = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="dunn")

sdindex = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="sdindex")

sdbw = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="sdbw")

dindex = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="dindex")

hubert = NbClust(cdata, distance = "euclidean", method="complete", min.nc=min, max.nc=max, index="hubert")

#create a vector of the optimal cluster choice by each test
best=c(kl$Best.nc[[1]], ch$Best.nc[[1]],hartigan$Best.nc[[1]],ccc$Best.nc[[1]],scott$Best.nc[[1]],marriot$Best.nc[[1]],tracew$Best.nc[[1]],friedman$Best.nc[[1]],rubin$Best.nc[[1]],cindex$Best.nc[[1]],db$Best.nc[[1]],silhouette$Best.nc[[1]],duda$Best.nc[[1]],ratkowsky$Best.nc[[1]],ball$Best.nc[[1]],ptbiserial$Best.nc[[1]],mcclain$Best.nc[[1]],gamma$Best.nc[[1]],gplus$Best.nc[[1]],tau$Best.nc[[1]],dunn$Best.nc[[1]],sdindex$Best.nc[[1]], sdbw$Best.nc[[1]]) 

tests = tibble(x=1:length(best), clusters=best, experiment = "ASC", measure="Cluster validation tests")

#plot a histogram

validation.histogram = ggplot(tests, aes(x=clusters)) +
  geom_bar() +
  facet_grid(experiment~measure) +
  theme_bw() +
  scale_x_continuous(breaks=c(2:19), labels=c(2:19)) +
  scale_y_continuous(limits=c(0,10), breaks=c(0:10), labels=c(0:10)) +
  ylab("number of tests that prefer each number") +
  xlab("number of clusters")

ggsave(file="validation.histogram.pdf", validation.histogram, height=3, width=4)


n <- arrangeGrob(ASC.aic.plot, validation.histogram, nrow=1, widths=c(4,4))
ggsave(file="NE.cluster.selection.plot.pdf", n, height=3, width=8)

#########################
#bar plot of one cluster
########################
data.for.one.plot=data.cluster.columns %>%
  select(subject, verb, condition, zscores) %>%
  pivot_wider(names_from = "condition", values_from = "zscores") %>%
  mutate(difference = `control` - `ne`) %>%
  group_by(verb) %>%
  summarise(difference.mean = mean(difference, na.rm=TRUE), se = sd(difference, na.rm=TRUE)/sqrt(n())) %>%
  ungroup()

#Add a column "clusters"
cluster <- "clusters"
data.for.one.plot$cluster <- cluster

#get the order
#one.order = data.for.one.plot %>%
#  group_by(verb) %>%
#  summarise(grand.mean = mean(difference.mean))%>%
#  arrange(grand.mean)%>%
#  select(verb) %>%
#  as.vector()

#one.c1 = filter(data.for.one.plot, one.clusters == "clusters") %>% 
#  droplevels() %>%

one.c1 <-  data.for.one.plot %>%
  mutate(verb = fct_reorder(verb, difference.mean))

lower = -1.5
upper = 1.5

barheight1p <- -0.5
barheight2p <- -0.8
barheight3 <- -1.1
barheight4 <- -1.4

one.c1.plot = ggplot(one.c1, aes(x = verb, y = difference.mean, fill=verb)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", color="black") +
  geom_errorbar(aes(ymin=difference.mean-se, ymax=difference.mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  scale_fill_manual(values=c(middle, blue, blue, light.red, blue, red, middle, light.blue, light.blue, light.blue, blue, light.red, middle, middle, light.blue, light.red, light.red, red, red, red)) +
  facet_grid(.~cluster) +
  theme_bw() +
  theme(legend.position ="none", 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())+
  geom_segment(x=.6, xend=18.4, y=barheight1p, yend=barheight1p, color=middle, lwd=2) +
  geom_segment(x=18.6, xend=20.4, y=barheight1p, yend=barheight1p, color=middle, lwd=2) +
  geom_segment(x=.6, xend=7.4, y=barheight2p, yend=barheight2p, color=middle, lwd=2) +
  geom_segment(x=7.6, xend=18.4, y=barheight2p, yend=barheight2p, color=middle, lwd=2) +
  geom_segment(x=18.6, xend=20.4, y=barheight2p, yend=barheight2p, color=middle, lwd=2) +
  geom_segment(x=.6, xend=7.4, y=barheight3, yend=barheight3, color=middle, lwd=2) +
  geom_segment(x=7.6, xend=17.4, y=barheight3, yend=barheight3, color=middle, lwd=2) +
  geom_segment(x=17.6, xend=18.4, y=barheight3, yend=barheight3, color=middle, lwd=2) +
  geom_segment(x=18.6, xend=20.4, y=barheight3, yend=barheight3, color=middle, lwd=2) +
  geom_segment(x=.6, xend=2.4, y=barheight4, yend=barheight4, color=middle, lwd=2) +
  geom_segment(x=2.6, xend=7.4, y=barheight4, yend=barheight4, color=middle, lwd=2) +
  geom_segment(x=7.6, xend=17.4, y=barheight4, yend=barheight4, color=middle, lwd=2) +
  geom_segment(x=17.6, xend=18.4, y=barheight4, yend=barheight4, color=middle, lwd=2) +
  geom_segment(x=18.6, xend=20.4, y=barheight4, yend=barheight4, color=middle, lwd=2)


#arrange the grid
quartz(width=12, height=3)
one.c1.plot

#save
ggsave(file="NE.one.clusters.differences.pdf", one.c1.plot, height=3, width=12)


#########################
#bar plot of two clusters
#########################

data.for.two.plot=data.cluster.columns %>%
  select(subject, two, verb, condition, zscores) %>%
  pivot_wider(names_from = "condition", values_from = "zscores") %>%
  mutate(difference = `control` - `ne`) %>%
  group_by(two, verb) %>%
  summarise(difference.mean = mean(difference, na.rm=TRUE), se = sd(difference, na.rm=TRUE)/sqrt(n())) %>%
  ungroup()

#get the order
two.order = data.for.two.plot %>%
  group_by(two) %>%
  summarise(grand.mean = mean(difference.mean))%>%
  arrange(grand.mean)%>%
  select(two) %>%
  as.vector()

#relevel and recode based on the orders; just going to do this by hand for now

data.for.two.plot = data.for.two.plot %>%
  mutate(two.clusters = fct_relevel(two, "1", "2")) %>%
  mutate(two.clusters = fct_recode(two.clusters,  "cluster 1" = "1", "cluster 2" = "2"))

#subsets
two.c1 = filter(data.for.two.plot, two.clusters == "cluster 1") %>% 
  droplevels() %>%
  mutate(verb = fct_reorder(verb, difference.mean))

two.c2 = filter(data.for.two.plot, two.clusters == "cluster 2") %>% 
  droplevels() %>%
  mutate(verb = fct_reorder(verb, difference.mean))


#C1 plot with legend

lower = -1.5
upper = 1.5

two.c1.plot = ggplot(two.c1, aes(x = verb, y = difference.mean, fill=verb)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", color="black") +
  geom_errorbar(aes(ymin=difference.mean-se, ymax=difference.mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  scale_fill_manual(values=c(middle, blue, blue, light.red, blue, red, middle, light.blue, light.blue, light.blue, blue, light.red, middle, middle, light.blue, light.red, light.red, red)) +
  facet_grid(.~two.clusters) +
  theme_bw() +
  theme(legend.position ="none", axis.title.x=element_blank(), axis.title.y=element_blank())

two.c2.plot = ggplot(two.c2, aes(x = verb, y = difference.mean, fill=verb)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", color="black") +
  geom_errorbar(aes(ymin=difference.mean-se, ymax=difference.mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  scale_fill_manual(values=c(red, red)) +
  facet_grid(.~two.clusters) +
  theme_bw() +
  theme(legend.position ="none", axis.title.x=element_blank(), axis.title.y=element_blank())

#the widths of the facets are set by hand, I don't know a better way.

#arrange the grid
quartz(width=12, height=3)
grid.arrange(two.c1.plot, two.c2.plot, nrow=1, widths=c(18,2))

#save
j <- arrangeGrob(two.c1.plot, two.c2.plot, nrow=1, widths=c(18,2))
ggsave(file="NE.two.clusters.differences.pdf", j, height=3, width=12)

#########################
#bar plot of five clusters
#########################

data.for.five.plot=data.cluster.columns %>%
  select(subject, five, verb, condition, zscores) %>%
  pivot_wider(names_from = "condition", values_from = "zscores") %>%
  mutate(difference = `control` - `ne`) %>%
  group_by(five, verb) %>%
  summarise(difference.mean = mean(difference, na.rm=TRUE), se = sd(difference, na.rm=TRUE)/sqrt(n())) %>%
  ungroup()

#get the order
five.order = data.for.five.plot %>%
  group_by(five) %>%
  summarise(grand.mean = mean(difference.mean))%>%
  arrange(grand.mean)%>%
  select(five) %>%
  as.vector()

#relevel and recode based on the orders; just going to do this by hand for now

data.for.five.plot = data.for.five.plot %>%
  mutate(five.clusters = fct_relevel(five, "2", "1", "3", "4", "5")) %>%
  mutate(five.clusters = fct_recode(five.clusters,  "cluster 1" = "2", "cluster 2" = "1", "cluster 3" = "3", "cluster 4" = "4", "cluster 5" = "5"))

#subsets
five.c1 = filter(data.for.five.plot, five.clusters == "cluster 1") %>% 
  droplevels() %>%
  mutate(verb = fct_reorder(verb, difference.mean))

five.c2 = filter(data.for.five.plot, five.clusters == "cluster 2") %>% 
  droplevels() %>%
  mutate(verb = fct_reorder(verb, difference.mean))

five.c3 = filter(data.for.five.plot, five.clusters == "cluster 3") %>% 
  droplevels() %>%
  mutate(verb = fct_reorder(verb, difference.mean))

five.c4 = filter(data.for.five.plot, five.clusters == "cluster 4") %>% 
  droplevels() %>%
  mutate(verb = fct_reorder(verb, difference.mean))

five.c5 = filter(data.for.five.plot, five.clusters == "cluster 5") %>% 
  droplevels() %>%
  mutate(verb = fct_reorder(verb, difference.mean))

#C1 plot with legend

lower = -1.5
upper = 1.5

five.c1.plot = ggplot(five.c1, aes(x = verb, y = difference.mean, fill=verb)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", color="black") +
  geom_errorbar(aes(ymin=difference.mean-se, ymax=difference.mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  scale_fill_manual(values=c(middle, blue)) +
  facet_grid(.~five.clusters) +
  theme_bw() +
  theme(legend.position ="none", axis.title.x=element_blank(), axis.title.y=element_blank())

five.c2.plot = ggplot(five.c2, aes(x = verb, y = difference.mean, fill=verb)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", color="black") +
  geom_errorbar(aes(ymin=difference.mean-se, ymax=difference.mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  scale_fill_manual(values=c(blue, light.red, blue, red, middle)) +
  facet_grid(.~five.clusters) +
  theme_bw() +
  theme(legend.position ="none", axis.title.x=element_blank(), axis.title.y=element_blank())

five.c3.plot = ggplot(five.c3, aes(x = verb, y = difference.mean, fill=verb)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", color="black") +
  geom_errorbar(aes(ymin=difference.mean-se, ymax=difference.mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  scale_fill_manual(values=c(light.blue, light.blue, light.blue, blue, light.red, middle, middle, light.blue, light.red, light.red)) +
  facet_grid(.~five.clusters) +
  theme_bw() +
  theme(legend.position ="none", axis.title.x=element_blank(), axis.title.y=element_blank())

five.c4.plot = ggplot(five.c4, aes(x = verb, y = difference.mean, fill=verb)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", color="black") +
  geom_errorbar(aes(ymin=difference.mean-se, ymax=difference.mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  scale_fill_manual(values=c(red)) +
  facet_grid(.~five.clusters) +
  theme_bw() +
  theme(legend.position ="none", axis.title.x=element_blank(), axis.title.y=element_blank())

five.c5.plot = ggplot(five.c5, aes(x = verb, y = difference.mean, fill=verb)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", color="black") +
  geom_errorbar(aes(ymin=difference.mean-se, ymax=difference.mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  scale_fill_manual(values=c(red, red)) +
  facet_grid(.~five.clusters) +
  theme_bw() +
  theme(legend.position ="none", axis.title.x=element_blank(), axis.title.y=element_blank())

#the widths of the facets are set by hand, I don't know a better way.

#arrange the grid
quartz(width=12, height=3)
grid.arrange(five.c1.plot, five.c2.plot, five.c3.plot, five.c4.plot, five.c5.plot, nrow=1, widths=c(2,5,10,1,2))

#save
k <- arrangeGrob(five.c1.plot, five.c2.plot, five.c3.plot, five.c4.plot, five.c5.plot, nrow=1, widths=c(2,5,10,1,2))
ggsave(file="NE.five.clusters.differences.pdf", k, height=3, width=12)


#########################
#bar plot of three clusters
#########################

data.for.three.plot=data.cluster.columns %>%
  select(subject, three, verb, condition, zscores) %>%
  pivot_wider(names_from = "condition", values_from = "zscores") %>%
  mutate(difference = `control` - `ne`) %>%
  group_by(three, verb) %>%
  summarise(difference.mean = mean(difference, na.rm=TRUE), se = sd(difference, na.rm=TRUE)/sqrt(n())) %>%
  ungroup()

#get the order
three.order = data.for.three.plot %>%
  group_by(three) %>%
  summarise(grand.mean = mean(difference.mean))%>%
  arrange(grand.mean)%>%
  select(three) %>%
  as.vector()

#relevel and recode based on the orders; just going to do this by hand for now

data.for.three.plot = data.for.three.plot %>%
  mutate(three.clusters = fct_relevel(three, "1", "2", "3")) %>%
  mutate(three.clusters = fct_recode(three.clusters,  "cluster 1" = "1", "cluster 2" = "2", "cluster 3" = "3"))

#subsets
three.c1 = filter(data.for.three.plot, three.clusters == "cluster 1") %>% 
  droplevels() %>%
  mutate(verb = fct_reorder(verb, difference.mean))

three.c2 = filter(data.for.three.plot, three.clusters == "cluster 2") %>% 
  droplevels() %>%
  mutate(verb = fct_reorder(verb, difference.mean))

three.c3 = filter(data.for.three.plot, three.clusters == "cluster 3") %>% 
  droplevels() %>%
  mutate(verb = fct_reorder(verb, difference.mean))


#C1 plot with legend

lower = -1.5
upper = 1.5

three.c1.plot = ggplot(three.c1, aes(x = verb, y = difference.mean, fill=verb)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", color="black") +
  geom_errorbar(aes(ymin=difference.mean-se, ymax=difference.mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  scale_fill_manual(values=c(middle, blue, blue, light.red, blue, red, middle)) +
  facet_grid(.~three.clusters) +
  theme_bw() +
  theme(legend.position ="none", axis.title.x=element_blank(), axis.title.y=element_blank())

three.c2.plot = ggplot(three.c2, aes(x = verb, y = difference.mean, fill=verb)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", color="black") +
  geom_errorbar(aes(ymin=difference.mean-se, ymax=difference.mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  scale_fill_manual(values=c(light.blue, light.blue, light.blue, blue, light.red, middle, middle, light.blue, light.red, light.red, red)) +
  facet_grid(.~three.clusters) +
  theme_bw() +
  theme(legend.position ="none", axis.title.x=element_blank(), axis.title.y=element_blank())

three.c3.plot = ggplot(three.c3, aes(x = verb, y = difference.mean, fill=verb)) +
  #geom_col(position="identity", fill=gray(.75)) +
  geom_col(position="identity", color="black") +
  geom_errorbar(aes(ymin=difference.mean-se, ymax=difference.mean+se), width=.3) +
  scale_y_continuous(limits=c(lower, upper)) +
  scale_fill_manual(values=c(red, red)) +
  facet_grid(.~three.clusters) +
  theme_bw() +
  theme(legend.position ="none", axis.title.x=element_blank(), axis.title.y=element_blank())


#the widths of the facets are set by hand, I don't know a better way.

#arrange the grid
quartz(width=12, height=3)
grid.arrange(three.c1.plot, three.c2.plot, three.c3.plot, nrow=1, widths=c(7,11,2))

#save
k <- arrangeGrob(three.c1.plot, three.c2.plot, three.c3.plot, nrow=1, widths=c(7,11,2))
ggsave(file="NE.three.clusters.differences.pdf", k, height=3, width=12)
