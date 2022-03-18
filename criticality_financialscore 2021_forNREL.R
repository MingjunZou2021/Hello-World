##Criticality assessment

library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)
library(reshape2)
library(tidyverse)
#windowsFonts(A = windowsFont("Times New Roman"))

##################
## plot 1: plain##
##################

rm(list=ls())
#Load Scores. Format Columns: Symbol, Vertical, Horizontal (raw scores)
scores <-read.csv("D:/CSM/CMI/CMI dropbox/2021 Appendix and Data/finalscores_sales_Jun12021MT.csv", header=TRUE)
scores<-finalscores_sales_Jun12021MT
names(scores)[1]="Symbol"
scores_tbl=tbl_df(scores)
scores_tbl
select(scores_tbl, Symbol, Vertical_2020, Vertical_2021, Horizontal_2020, Horizontal_2021)%>%
  gather(Position_year, Value, Vertical_2020:Horizontal_2021)%>%
  mutate(Year=as.numeric(str_sub(Position_year, -4)))%>%
  mutate(Position=str_sub(Position_year, 1,-6))%>%
  select(Symbol, Year, Position, Value)%>%
  spread(Year, Value)%>%
  mutate(Diff='2020')
?mutate


head(scores)
##The scores are normalized use observed max/min. The normalized scores range from min to 1.
p <- ggplot(scores, aes(x = (Horizontal_2021/max(Horizontal_2021)), y = (Vertical_2021/max(Vertical_2021))))
p <- p + geom_point(size = 1.5, color = "gray20")
p <- p + geom_text_repel(aes(label = Symbol),size = 4, family = "Times New Roman", color = "black")
p <- p + scale_x_continuous(breaks = c(0,1.08),labels = c("0", "1"), limits = c(0,1.08))
p <- p + scale_y_continuous(breaks = c(-0.11,1.11),labels = c("0", "1"), limits = c(-0.08,1.08))
p

p <- p + xlab("Likelihood of Supply Disruption")
p <- p + ylab(strwrap("Impact of Supply Disruption",width = 20))
p <- p + theme(axis.text=element_text(size=10, colour = "black"),                        ##axis' text font
               text=element_text(family="Times New Roman", size=10),                                   ##axis title text font
               legend.position="none",                                                   ##no legend
               panel.background=element_rect(fill='white', colour = "black"),            ##no background, black border
               panel.grid.major = element_blank(),                                       ##no grid
               panel.grid.minor = element_blank(),
               axis.ticks = element_blank(),                                             ##remove the tick marks
               axis.title.y = element_text(margin = margin(t = 0, r = -7, b = 0, l = 0),angle=360, vjust = 0.5, hjust=1))##position of y title


p <- p + geom_vline(xintercept=0.175957182, linetype="dotdash", colour = "black")
p <- p + geom_text(aes(x=0.08, label="Copper", y=0.05), colour="grey30", angle=90, vjust = 1.2, family = "Times New Roman", size = 6.5)
p <- p + geom_vline(xintercept=0.311580805, linetype="dotted", colour = "black")
p <- p + geom_text(aes(x=0.22, label="Iron", y=0.08), colour="grey30", angle=90, vjust = 1.2, family = "Times New Roman", size = 6.5)

ggsave("plain_2021.png", plot = last_plot(), width = 15.5, height = 15, units = "cm", dpi = 1000)

p

###########################
## plot 2: K-mean cluster##
###########################
#Question: Why do we use K-mean method? Or, What information does it give?

# "k-means clustering aims to partition n observations into k clusters in which each observation belongs 
# to the cluster with the nearest mean, serving as a prototype of the cluster." (Wiki)

# Therefor, using k-mean clustering, we find the group of elements with similar scores. This method and general
# clustering mehtods do not serve other purposes. This implies that if we want to incorporate the feature of
# ranking into the figure, we should not present the K-mean figure alone. Even though the K-mean method does not
# rank, it can compare as it clusters the elements with similar scores. If we want, we can cliam that those
# elements in the smae cycle have similar criticality. ...Can we?

# Normalization is important as it impacts Euclidean distance. Certianly, it is crucial for all other methods.

# Question: How to determine the optimal number of clustering-K.
#Elbow method: check the cost function
#How many group do we want to have? What is the later purpose?
#Which quality of the figure are we looking for? 
#The ability of ranking?
rm(list=ls())
Scores =read.csv("D:/CSM/CMI/CMI dropbox/2021 Appendix and Data/finalscores_sales_Jun12021MT.csv", header=T)
head(Scores)
names(Scores)[1]="Symbol"
VScoresResized=(Scores[,9])/(max(Scores[,9]))
HScoresResized=(Scores[,10])/(max(Scores[,10]))
mydata <- data.frame(HScoresResized, VScoresResized)
mydata

# Elbow method
# https://rstudio-pubs-static.s3.amazonaws.com/92318_20357e6dd99742eb90232c60c626fa90.html
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15){
  wss[i] <- sum(kmeans(mydata,centers=i)$withinss) 
}

png('k_mean.png')
par(mar=c(4,5,2,2))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within clusters sum of squares", cex.lab=1.5, cex.axis=1.5, cex=1.5)
dev.off()

#It is reasonable to choose 3. Adding another cluster will not reduce the within group variance a lot.



######
####
###
##
#K-means cluster and draw the graph 
rm(list=ls())
Scores =read.csv("D:/CSM/CMI/CMI dropbox/2021 Appendix and Data/finalscores_sales_Jun12021MT.csv", header=T)
names(Scores)[1]="Symbol"
VScoresResized=(Scores[,9])/(max(Scores[,9]))
HScoresResized=(Scores[,10])/(max(Scores[,10]))
Scores$Cluster=factor(kmeans(cbind(VScoresResized, HScoresResized),centers=3)$cluster)
head(Scores)  
Scores$hor=Scores$Horizontal_2021/max(Scores$Horizontal_2021)
Scores$ver=Scores$Vertical_2021/max(Scores$Vertical_2021)


ggplot(data = Scores, aes(x = (Horizontal_2021/max(Horizontal_2021)), y = (Vertical_2021/max(Vertical_2021)), col= Cluster)) +
        geom_point(size = 1.5,col = "white")+
        geom_point(size=1.5,alpha=.5)+
        geom_text_repel(aes(label= Symbol),family="Times New Roman",size=4)+
        
        
        scale_x_continuous(breaks=c(0,1.08),labels = c("Lower", "Higher     "))+
        scale_y_continuous(breaks=c(-0.11,1.11),labels = c("Lower", "Higher"))+
        
        xlab("Likelihood of Supply Disruption")+
        ylab("Impact of Supply Disruption")+
        
        coord_cartesian(xlim=c(0,1.08),ylim=c(-0.08,1.08))+
        stat_ellipse(data=filter(Scores, Cluster==1), aes(y=ver, x=hor), level=.90, lty=2, lwd=1)+
        stat_ellipse(data=filter(Scores, Cluster==2), aes(y=ver, x=hor), level=.90, lty=2, lwd=1)+
        stat_ellipse(data=filter(Scores, Cluster==3), aes(y=ver, x=hor), level=.90, lty=2, lwd=1)+
        stat_ellipse(data=filter(Scores, Cluster==4), aes(y=ver, x=hor), level=.90, lty=2, lwd=1)+
        
        theme(legend.position="none",
              axis.text=element_text(size=18, colour = "black"),
              text=element_text(family="Times New Roman", size=18),
              panel.background=element_rect(fill='white', colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks = element_blank(),
              axis.title.y = element_text(margin = margin(t = 0, r = -7, b = 0, l = 0)))+
        
        geom_vline(xintercept=0.175957182, linetype="dotdash", colour = "black")+
        geom_text(aes(x=0.08, label="Copper", y=0.05), colour="grey30", angle=90, vjust = 1.2, family = "Times New Roman", size = 5.5)+
        geom_vline(xintercept=0.311580805, linetype="dotted", colour = "black")+
        geom_text(aes(x=0.22, label="Iron", y=0.08), colour="grey30", angle=90, vjust = 1.2, family = "Times New Roman", size = 5.5)


ggsave("kmean_cluster_4.png", plot = last_plot(), width = 15.5, height = 15, units = "cm", dpi = 1000)


################
#An alternative#
################

# https://uc-r.github.io/kmeans_clustering

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#prepare the data, confirm columns according to the CSV



#####
####
###
##
#Elbow method to find the best cluster numbers for k-means
rm(list=ls())
Scores =read.csv("D:/CSM/CMI/CMI dropbox/2021 Appendix and Data/finalscores_sales_Jun12021MT.csv", header=T)
names(Scores)[1]="Symbol"
VScoresResized=(Scores[,9])/(max(Scores[,9]))
HScoresResized=(Scores[,10])/(max(Scores[,10]))
df <- data.frame(HScoresResized, VScoresResized)
rownames(df) <- Scores$Symbol
plot(df)

k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)
k6 <- kmeans(df, centers = 6, nstart = 25)

str(k3)


p3 <- fviz_cluster(k5, data = df,axes = c(0,1),stand = F, 
                   main = NULL,
                   xlab = "Likelihood of Supply Disruption",
                   ylab = "Impact of Supply Disruption",
                   repel = F,
                   
                   ggtheme =theme(
                     legend.position="none",
                     axis.text=element_text(size=12, colour = "black"),
                     text=element_text(family="Times New Roman", size=12),
                     #How to change the font of points' label?
                     panel.background=element_rect(fill='white', colour = "black"),
                     panel.grid.major = element_blank(),                                       
                     panel.grid.minor = element_blank(),
                     axis.ticks.length = unit(0, "cm"),
                     axis.text.x = element_text(color = "black"),
                     axis.title.y = element_text(margin = margin(t = 0, r = -7, b = 0, l = 0))
                   )
)
p3
p3 <- p3 + scale_x_continuous(breaks = c(0,1),labels = c("Lower", "Higher"))
p3 <- p3 + scale_y_continuous(breaks = c(0,1),labels = c("Lower", "Higher"))
p3
ggsave("kmean_cluster_opt2_5.png", plot = p3, width = 15.5, height = 15, units = "cm", dpi = 1000)


#grid.arrange(p3, p4, p5, p6, nrow = 2)
fviz_nbclust(df, kmeans, method = "wss")


############################
## plot 3: hyperbola level##
############################
rm(list=ls())

##hyperbola levels
fn1 <-function(x) (0.3/7)/x
fn2 <-function(x) (1.3/7)/x
fn3 <-function(x) (2.3/7)/x
fn4 <-function(x) (3.3/7)/x
fn5 <-function(x) (4.3/7)/x
fn6 <-function(x) (5.3/7)/x

#Load Scores. Format Columns: Symbol, Vertical, Horizontal (raw scores)
Scores =read.csv("D:/CSM/CMI/CMI dropbox/2021 Appendix and Data/finalscores_sales_Jun12021MT.csv", header=T)
names(Scores)[1]="Symbol"
##The scores are normalized use observed max/min. The normalized scores range from min to 1.


###
##
#draw the graph of likelihood of supply distruption and impact of supply distruption
p <- ggplot(data = Scores, aes(x = (Horizontal_2021/max(Horizontal_2021)), y = (Vertical_2021/max(Vertical_2021))))
p= p+stat_function(fun = fn1, color = "blue", size = 1.0, n=1000, xlim = c(0,1.2))+
  stat_function(fun = fn2, color = "blue", size = 1.0, n=1000, xlim = c(0,1.2))+
  stat_function(fun = fn3, color = "blue", size = 1.0, n=1000, xlim = c(0,1.2))+
  stat_function(fun = fn4, color = "blue", size = 1.0, n=1000, xlim = c(0,1.2))+
  stat_function(fun = fn5, color = "blue", size = 1.0, n=1000, xlim = c(0,1.2))+
  
  geom_vline(xintercept=0.175957182, linetype="dotdash", colour = "black")+
  geom_text(aes(x=0.08, label="Copper", y=0.05), colour="grey30", angle=90, vjust = 1.2, family = "Times New Roman", size = 6.5)+
  geom_vline(xintercept=0.311580805, linetype="dotted", colour = "black")+
  geom_text(aes(x=0.22, label="Iron", y=0.08), colour="grey30", angle=90, vjust = 1.2, family = "Times New Roman", size = 6.5)
p <- p + geom_point(size = 2.5, color = "grey20")
p <- p + geom_text_repel(aes(label = Symbol),size = 6, family = "Times New Roman", color = "black")
p <- p + scale_x_continuous(breaks = c(0,1.08),labels = c("Lower", "Higher     "))
p <- p + scale_y_continuous(breaks = c(-0.11,1.11),labels = c("Lower", "Higher"))

p <- p + coord_cartesian(xlim=c(0,1.08),ylim=c(-0.08,1.08))
p <- p + xlab("Likelihood of Supply Disruption")
p <- p + ylab("Impact of Supply Disruption")

p <- p + theme(axis.text=element_text(size=24, colour = "black"),                        ##axis' text font
               text=element_text(family="Times New Roman", size=24),                                   ##axis title text font
               legend.position="none",                                                   ##no legend
               panel.background=element_rect(fill='white', colour = "black"),            ##no background, black border
               panel.grid.major = element_blank(),                                       ##no grid
               panel.grid.minor = element_blank(),
               axis.ticks = element_blank(),                                             ##remove the tick marks
               axis.title.y = element_text(margin = margin(t = 0, r = -7, b = 0, l = 0)))##position of y title
p
ggsave("level5.png", plot = last_plot(), width = 15.5, height = 15, units = "cm", dpi = 1000)



######
#####
####
###
##
#Major differences likelihood of supply disruption by different color
Scores =read.csv("D:/CSM/CMI/CMI dropbox/2021 Appendix and Data/finalscores_sales_Jun12021MT.csv", header=T)
Scores<-Scores[-16,] #### Remove Nickel. It was added in 2021. 
names(Scores)[1]="Symbol"
head(Scores)
Scores_d=filter(Scores, Difference>=5)
dim(Scores_d)
p <- ggplot(Scores_d, aes(x = Horizontal_2021, y = (Vertical_2021/max(Vertical_2021))))
p <- p + geom_point(aes(x=Horizontal_2020, y=(Vertical_2020/max(Vertical_2020)), color = "Blue"), size = 4)
p
p <- p + geom_point(aes(x=Horizontal_2021, y=(Vertical_2021/max(Vertical_2021)), color = "Red"), size = 4)+
  scale_colour_manual(name="Year",values = c("Blue","Red"), labels=c("2020", "2021"))
p <- p + scale_x_continuous(breaks = c(0,1.08),labels = c("Lower", "Higher"), limits = c(0,1.08))
p
p <- p + scale_y_continuous(breaks = c(-0.11,1.11),labels = c("Lower", "Higher"), limits = c(-0.08,1.08))
p
p <- p + geom_text(aes(label = Symbol),size = 4, family = "Times New Roman", color = "darkred", check_overlap=T, nudge_x=0.00, nudge_y = 0.04)
p <- p + geom_text(aes(x=Horizontal_2020, y=(Vertical_2020/max(Vertical_2020)), label = Symbol),size = 4, family = "Times New Roman", color = "darkblue", check_overlap=T, nudge_x=-0.00, nudge_y = -0.04)
p
p <- p + xlab("Likelihood of Supply Disruption")
p <- p + ylab("Impact of Supply Disruption")
p <- p + theme(axis.text=element_text(size=12, colour = "black"),                        ##axis' text font
               text=element_text(family="Times New Roman", size=12),                                   ##axis title text font
               legend.position=c(0.9,0.3),                                                   ##legend
               panel.background=element_rect(fill='white', colour = "black"),            ##no background, black border
               panel.grid.major = element_blank(),                                       ##no grid
               panel.grid.minor = element_blank(),
               axis.ticks = element_blank(),                                             ##remove the tick marks
               axis.title.y = element_text(margin = margin(t = 0, r = -7, b = 0, l = 0)))##position of y title

p1 <- p + geom_vline(xintercept=0.175957182, linetype="dotdash", colour = "black")
p <- p1 + geom_text(aes(x=0.08, label="Copper", y=0.05), colour="grey30", angle=90, vjust = 1.2, family = "Times New Roman", size = 4.5)
p <- p + geom_vline(xintercept=0.311580805, linetype="dotted", colour = "black")
p <- p + geom_text(aes(x=0.22, label="Iron", y=0.08), colour="grey30", angle=90, vjust = 1.2, family = "Times New Roman", size = 4.5)
p
ggsave("diff.png", plot = last_plot(), width = 15.5, height = 15, units = "cm", dpi = 1000)


####
###
##
#By indicator  transformed scores "all components bar in graph"
Scores =read.csv("D:/CSM/CMI/CMI dropbox/2021 Appendix and Data/finalscores_sales_Jun12021MT.csv", header=T)
names(Scores)[1]="Symbol"
Scores$Symbol=as.character(Scores$Symbol)
Scores[[9,1]]="C"
head(Scores)
ggplot(Scores, aes(x=reorder(Symbol, Vertical_2021), y = Vertical_2021, fill =Symbol))+ ##if interested all material in the same color remove this 'fill=symbol'))
                   geom_bar(stat="identity", col="darkblue")+ ##fill=symbol)+
  scale_fill_manual(values = c("blue", "blue", "blue", "blue", "red", "blue" ,"blue","red","blue","blue", "blue","blue","blue","blue","red","blue","blue","red","blue","blue","blue",
                               "red","blue","blue","blue"))+    ###set color for wind energy materials 
  theme(axis.text=element_text(size=12, colour = "black"),                        ##axis' text font
        text=element_text(family="Times New Roman", size=12),                                   ##axis title text font
       # legend.position=c(0.9,0.3),                                                   ##legend
        panel.background=element_rect(fill='white', colour = "black"),            ##no background, black border
        panel.grid.major = element_blank(),                                       ##no grid
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),                                             ##remove the tick marks
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))+##position of y title
  labs(x="", y="Transformed Scores")
ggsave("Vertical.png", plot = last_plot(), width = 25, height = 15, units = "cm", dpi = 1000)



hor=read.csv("D:/CSM/CMI/CMI dropbox/2021 Appendix and Data/indexes_Jun012021MT.csv")
head(hor)
names(hor)[1]="Symbol"
hor$Symbol=as.character(hor$Symbol)
hor[[9,1]]="C"
ggplot(hor[!(hor$Symbol=="Cu" | hor$Symbol=="Fe*"),], aes(x=reorder(Symbol, scr), y = scr))+geom_bar(stat="identity", col="darkblue", fill="blue")+
  theme(axis.text=element_text(size=12, colour = "black"),                        ##axis' text font
        text=element_text(family="Times New Roman", size=12),                                   ##axis title text font
        legend.position=c(0.9,0.3),                                                   ##legend
        panel.background=element_rect(fill='white', colour = "black"),            ##no background, black border
        panel.grid.major = element_blank(),                                       ##no grid
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),                                             ##remove the tick marks
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))+##position of y title
  labs(x="", y="Likelihood of Supply Disruption")+ylim(0,1)

ggsave("scr.png", plot = last_plot(), width = 25, height = 15, units = "cm", dpi = 1000)




####
###
##
# Adjusted Producer Diversity(APD)
ggplot(hor[!(hor$Symbol=="Cu" | hor$Symbol=="Fe*"),], aes(x=reorder(Symbol, apd), y = apd, fill=Symbol))+
  geom_bar(stat="identity", col="darkblue")+ #, fill="blue")+
  scale_fill_manual(values = c("blue", "blue", "blue", "blue", "red", "blue" ,"blue","red","blue","blue", "blue","blue",
                               "blue","blue","red","blue","blue","red","blue","blue","blue","red","blue","blue","blue"))+
  theme(axis.text=element_text(size=12, colour = "black"),                        ##axis' text font
        text=element_text(family="Times New Roman", size=12),                                   ##axis title text font
        #legend.position=c(0.9,0.3),                                                   ##legend
        panel.background=element_rect(fill='white', colour = "black"),            ##no background, black border
        panel.grid.major = element_blank(),                                       ##no grid
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),                                             ##remove the tick marks
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))+##position of y title
  labs(x="", y="Adjusted Producer Diversity")+ylim(0,1)
ggsave("apd.png", plot = last_plot(), width = 25, height = 15, units = "cm", dpi = 1000)



######
#####
####
###
##
# risk of demand shock (combine current share and weighted demand growth)
ggplot(hor[!(hor$Symbol=="Cu" | hor$Symbol=="Fe*"),], aes(x=reorder(Symbol, rds), y = rds, fill=Symbol))+
  geom_bar(stat="identity", col="darkblue")+ #, fill="blue")+
  scale_fill_manual(values = c("blue", "blue", "blue", "blue", "red", "blue" ,"blue","red","blue","blue", "blue","blue",
                               "blue","blue","red","blue","blue","red","blue","blue","blue","red","blue","blue","blue"))+
  theme(axis.text=element_text(size=12, colour = "black"),                        ##axis' text font
        text=element_text(family="Times New Roman", size=12),                                   ##axis title text font
       # legend.position=c(0.9,0.3),                                                   ##legend
        panel.background=element_rect(fill='white', colour = "black"),            ##no background, black border
        panel.grid.major = element_blank(),                                       ##no grid
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),                                             ##remove the tick marks
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))+##position of y title
  labs(x="", y="Risk of Demand Shock")+ylim(0,1)
ggsave("rds.png", plot = last_plot(), width = 25, height = 15, units = "cm", dpi = 1000)




#########
########
#######
#####
####
###
##
# market codependence 

ggplot(hor[!(hor$Symbol=="Cu" | hor$Symbol=="Fe*"),], aes(x=reorder(Symbol, cod), y = cod, fill=Symbol))+
  geom_bar(stat="identity", col="darkblue")+ #, fill="blue")+
  scale_fill_manual(values = c("blue", "blue", "blue", "blue", "red", "blue" ,"blue","red","blue","blue", "blue","blue",
                               "blue","blue","red","blue","blue","red","blue","blue","blue","red","blue","blue","blue"))+
  theme(axis.text=element_text(size=12, colour = "black"),                        ##axis' text font
        text=element_text(family="Times New Roman", size=12),                                   ##axis title text font
        #legend.position=c(0.9,0.3),                                                   ##legend
        panel.background=element_rect(fill='white', colour = "black"),            ##no background, black border
        panel.grid.major = element_blank(),                                       ##no grid
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),                                             ##remove the tick marks
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))+##position of y title
  labs(x="", y="Market Codependence")+ylim(0,1)
ggsave("cod.png", plot = last_plot(), width = 25, height = 15, units = "cm", dpi = 1000)




######
#####
####
###
##
# current share of risk of demand shock
ggplot(hor[!(hor$Symbol=="Cu" | hor$Symbol=="Fe*"),], aes(x=reorder(Symbol, cs), y = cs, fill=Symbol))+
  geom_bar(stat="identity", col="darkblue")+ #, fill="blue")+
  scale_fill_manual(values = c("blue", "blue", "blue", "blue", "red", "blue" ,"blue","red","blue","blue", "blue","blue",
                               "blue","blue","red","blue","blue","red","blue","blue","blue","red","blue","blue","blue"))+
  theme(axis.text=element_text(size=12, colour = "black"),                        ##axis' text font
        text=element_text(family="Times New Roman", size=12),                                   ##axis title text font
       # legend.position=c(1,0.3),                                                   ##legend
        panel.background=element_rect(fill='white', colour = "black"),            ##no background, black border
        panel.grid.major = element_blank(),                                       ##no grid
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),                                             ##remove the tick marks
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))+##position of y title
  labs(x="", y="Current Share (%)")+ylim(0,100)
ggsave("cs.png", plot = last_plot(), width = 25, height = 15, units = "cm", dpi = 1000)



#####
####
###
##
# risk of demand shock weighted demand growth
ggplot(hor[!(hor$Symbol=="Cu" | hor$Symbol=="Fe*"),], aes(x=reorder(Symbol, wdg), y = wdg, fill=Symbol))+
  geom_bar(stat="identity", col="darkblue")+##, fill="blue")+
  scale_fill_manual(values = c("blue", "blue", "blue", "blue", "red", "blue" ,"blue","red","blue","blue", 
                               "blue","blue","blue","blue","red","blue","blue","red","blue","blue","blue",
                               "red","blue","blue","blue"))+
  theme(axis.text=element_text(size=12, colour = "black"),                        ##axis' text font
        text=element_text(family="Times New Roman", size=12),                                   ##axis title text font
        #legend.position=c(0.9,0.3),                                                   ##legend
        panel.background=element_rect(fill='white', colour = "black"),            ##no background, black border
        panel.grid.major = element_blank(),                                       ##no grid
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),                                             ##remove the tick marks
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))+##position of y title
  labs(x="", y="Weighted Demand Growth (%)")+ylim(-5,20)
ggsave("wdg.png", plot = last_plot(), width = 25, height = 15, units = "cm", dpi = 1000)


#######
######
#####
####
###
##
# to build the graph about wind energy materials likelihood of supply distruption
hor=as.tibble(hor)
hor <- hor %>% mutate( high = ifelse( Symbol == "Fe*"|Symbol=="Cu", "yes", "no" ) )

ggplot(hor, aes(x=reorder(Symbol, scr), y = scr, fill=Symbol))+geom_bar(stat="identity", col="darkblue")+
  scale_fill_manual(values = c("blue", "blue", "blue", "blue", "blue", "red", "blue", "blue" ,"blue","red","blue","blue", "blue","blue","blue","blue","red","blue","blue","red","blue","blue","blue",
                               "red","blue","blue","blue"))+
  theme(axis.text=element_text(size=12, colour = "black"),                        ##axis' text font
        text=element_text(family="Times New Roman", size=12),                                   ##axis title text font
        #legend.position=c(0.9,0.3),                                                   ##legend
        panel.background=element_rect(fill='white', colour = "black"),            ##no background, black border
        panel.grid.major = element_blank(),                                       ##no grid
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),                                             ##remove the tick marks
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))+##position of y title
  labs(x="", y="Likelihood of Supply Disruption", caption="*Fe at pig iron production stage")+ylim(0,1)
 # scale_fill_manual( values = c( "yes"="green", "no"="blue" ), guide = FALSE )
ggsave("scr2.png", plot = last_plot(), width = 25, height = 15, units = "cm", dpi = 1000)

ggplot(hor, aes(x=reorder(Symbol, apd), y = apd, fill=high))+geom_bar(stat="identity", col="darkblue")+
  theme(axis.text=element_text(size=12, colour = "black"),                        ##axis' text font
        text=element_text(family="Times New Roman", size=12),                                   ##axis title text font
        legend.position=c(0.9,0.3),                                                   ##legend
        panel.background=element_rect(fill='white', colour = "black"),            ##no background, black border
        panel.grid.major = element_blank(),                                       ##no grid
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),                                             ##remove the tick marks
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))+##position of y title
  labs(x="", y="Adjusted Producer Diversity", caption="*Fe at pig iron production stage")+ylim(0,1)+
  scale_fill_manual( values = c( "yes"="green", "no"="blue" ), guide = FALSE )
ggsave("apd2.png", plot = last_plot(), width = 25, height = 15, units = "cm", dpi = 1000)



#####
####
###
##
# aggregated substitutability index 
ggplot(hor[!(hor$Symbol=="Cu" | hor$Symbol=="Fe*"),], aes(x=reorder(Symbol, subs), y = subs, fill=Symbol))+
  geom_bar(stat="identity", col="darkblue")+ #, fill="blue")+
  scale_fill_manual(values = c("blue", "blue", "blue", "blue", "red", "blue" ,"blue","red","blue","blue", "blue","blue","blue","blue","red","blue","blue","red","blue","blue","blue",
                               "red","blue","blue","blue"))+
  theme(axis.text=element_text(size=12, colour = "black"),                        ##axis' text font
        text=element_text(family="Times New Roman", size=12),                                   ##axis title text font
       # legend.position=c(0.9,0.3),                                                   ##legend
        panel.background=element_rect(fill='white', colour = "black"),            ##no background, black border
        panel.grid.major = element_blank(),                                       ##no grid
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),                                             ##remove the tick marks
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)))+##position of y title
  labs(x="", y="Aggregated Substitutability Index")+ylim(0,3)
ggsave("subs.png", plot = last_plot(), width = 25, height = 15, units = "cm", dpi = 1000)
