library(readxl)
library(gplots)
library(igraph)
library(dplyr)
library(psych)
library(ggplot2)
library(corrplot)
library(tidyr)

data <- read_excel("Data with CST.xlsx")
View(data)

## -------------- co-occurrence of all-time scores --------------------
data_model <- data[, c(2:12, 62,
                       14:24, 65,
                       26:36, 64,
                       #38:48, 63,
                       51,52)]
str(data_model)

## co-occurrence network 
igraph.group <- cooccurence_plot(data = data_model, 
                                 cor.cutoff = 0.8)
plot(igraph.group, edge.curved=TRUE, layout=layout.circle,
     vertex.label.cex=0.7, 
     main = "Co-Occurrence Network with cor-cut-off 0.5",
     #vertex.label.dist=-2,
     #vertex.label.degree = 2.2,
     vertex.size = 11)


## function for plotting co-occurrence network
## input: data, cutoff values for the correlation
cooccurence_plot <- function(data, cor.cutoff){
  occor.group1 <- corr.test(data, use="pairwise",
                            method="spearman",adjust="fdr",alpha=.05)
  occor.r.group1<- occor.group1$r 
  occor.p.group1<- occor.group1$p 
  #remove those r<0.7, p>0.05.
  occor.r.group1[occor.p.group1>0.05|abs(occor.r.group1)<cor.cutoff] <- 0 
  # set NA to be 0
  occor.p.group1[is.na(occor.p.group1)] <- 0
  occor.r.group1[is.na(occor.r.group1)] <- 0
  
  igraph.group1 <- graph_from_adjacency_matrix(occor.r.group1,mode="undirected",
                                               weighted=TRUE,diag=FALSE)
  # set edge color positive correlation pink color, negative blue.
  E.color.group1 <- ifelse(E(igraph.group1)$weight>0, "pink",
                           ifelse(E(igraph.group1)$weight<0, "blue","grey")) 
  E(igraph.group1)$color <- as.character(E.color.group1)
  
  #change edge width
  E(igraph.group1)$width <- abs(E(igraph.group1)$weight)*5
  
  E(igraph.group1)$weight <- NA
  
  #V(igraph.group1)$color <- c(rep('yellow', 7), 
  #                            rep('green', 4),
  #                            'red', 'blue', 'blue')
  
  return(igraph.group1)
}


## ----------------- Longitudinal changes of CST -----------------------
## -------------------- Phenotype ---------------------------------
retrievedata <- data.frame(DogID = data$DogID, 
                           Time3 = data$CST.Time3,
                           Time6 = data$CST.Time6, 
                           Time10 = data$CST.Time10,
                           Time12 = data$CST.Time12)
head(retrievedata)

retrievedata$final_disposition <- data$final_disposition
retrieve_long <- gather(retrievedata, Time, value, Time3:Time12)
retrieve_long$Time[which(retrieve_long$Time=='Time3')] <- 3
retrieve_long$Time[which(retrieve_long$Time=='Time6')] <- 6
retrieve_long$Time[which(retrieve_long$Time=='Time10')] <- 10
retrieve_long$Time[which(retrieve_long$Time=='Time12')] <- 12
retrieve_long$Time <- as.numeric(retrieve_long$Time)

retrieve_long$value <- round(retrieve_long$value, digits = 0)

## line chart
ggplot(retrieve_long, aes(x=Time, y=value, group = DogID, color = final_disposition)) + 
  geom_line(position=position_jitter(w=0, h=0.05)) + theme_classic() +
  ggtitle('CST') + 
  theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks=c(3,6,10,12),
                     labels=c("3 Month", "6 Month", "10 Month", "12 Month"))+
  theme(
    plot.title = element_text(size=14, face="bold.italic"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text(size=14, face="bold"),
    axis.text.y = element_text(size=14, face="bold"))


## smoothing lines
ggplot(retrieve_long, aes(x=Time, y=value, group = DogID, color = final_disposition)) + 
  theme_classic() + ylim(1,4) +
  ggtitle('CST') + 
  geom_smooth(aes(group = final_disposition, fill = final_disposition), method = 'lm') +
  theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks=c(3,6,10,12),
                     labels=c("3 Month", "6 Month", "10 Month", "12 Month"))+
  theme(
    plot.title = element_text(size=14, face="bold.italic"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text(size=14, face="bold"),
    axis.text.y = element_text(size=14, face="bold"))

##  bar plot with mean and SD
test <- retrieve_long
test$Time <- as.factor(test$Time)

a <- test %>% group_by(Time, final_disposition) %>% 
  summarise(mean = mean(value),
            sd = sd(value))
a$final_disposition <- as.factor(a$final_disposition)
a <- as.data.frame(a)

ggplot(a, aes(x=Time, y=mean, fill = final_disposition)) +
  geom_bar(stat="identity", position="dodge", alpha=0.7) +
  geom_errorbar(aes(x=Time, ymin=mean-sd, ymax=mean+sd,
                    group = final_disposition), width=0.3, 
                colour="black", alpha=0.9, size=1.2,
                position=position_dodge(.9)) + theme_classic()+
  ylab("Score") + ggtitle('CST') + ylim(0, 4.2) +
  theme(
    plot.title = element_text(size=14, face="bold.italic"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text(size=14, face="bold"),
    axis.text.y = element_text(size=14, face="bold")) +
  scale_x_discrete(labels=c("3 Month", "6 Month", "10 Month", "12 Month"))

slope <- rep(0, 180)
for (i in 1:180) {
  temp <- retrieve_long[which(retrieve_long$DogID==retrievedata$DogID[i]), ]
  fit <- lm(value~Time, data = temp)
  slope[i] <- fit$coefficients[2]
}  


finaldata <- data.frame(DogID = data$DogID,Slope = slope,
                        final_disposition = data$final_disposition)
finaldata$final_disposition <- as.factor(finaldata$final_disposition)

wilcox.test(Slope~final_disposition, data = finaldata)

wilcox.test(finaldata$Slope[which(finaldata$final_disposition=='Sale Quality')],
                        conf.int = TRUE)
wilcox.test(finaldata$Slope[which(finaldata$final_disposition=='Washout')],
                           conf.int = TRUE)



