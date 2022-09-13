# -------------------------------------------------------------------------------------------- #
# - FILE NAME:   Herbivory_Learning.R         
# - DATE:        09/09/2022
# - TITLE: Learning takes time: Biotic resistance by native herbivores increases through the invasion process.
# - AUTHORS: Jorge Santamaría, Raül Golo, Jana Verdura, Fiona Tomas, Enric Ballesteros, Teresa Alcoverro, Rohan Arthur, Emma Cebrian
# - SCRIPT: J. Santamaría (jsantamaria@ceab.csic.es)
# - JOURNAL: Ecology Letters
# -------------------------------------------------------------------------------------------- #

# DISCLAMER: This script has been developed by an ecologist, not a programer, 
# so please take into account that the code may have room to be optimized. 
# Positive feedback will always be more than welcome.


# Script Content------------------------------------------------ 
# 1. Load libraries and data
# 2. Assessment of herbivore preference
# 3. Analysis of invasive algae consumption by native herbivores
# 4. Assess potential differences in the Electivity Index between years



#########################################################
######## 1. Load libraries and data #####################
#########################################################

### Load required packages
library(dplyr)
library(ggplot2)
library(ggpubr)
library(car)
library(lme4)
library(emmeans)
library(visreg)
library(phia)
library(sjPlot)
library(effects)
library(FSA)



### Set workind directory to the place where you have the data stored
setwd("DataPath")


#########################################################
######## 2. Assessment of herbivore preference ##########
#########################################################

### Load the data
load("Preference_Assessment.RData")
summary(Preference_Assessment)


## Group data and get the summarized information
# 1) By Time since invasion
Time_pair <- group_by(Preference_Assessment, Time.since.Invasion, Treatment, Species) %>%
  summarise(mean_consumed=mean(Per_Consumed), sd=sd(Per_Consumed), se=sd(Per_Consumed)/sqrt(length(Species)), n=length(Species))
Time_pair

# 2) By Abundance
Abundance_pair <- group_by(Preference_Assessment, Caulerpa.in.Site, Treatment, Species) %>%
  summarise(mean_consumed=mean(Per_Consumed), sd=sd(Per_Consumed), se=sd(Per_Consumed)/sqrt(length(Species)), n=length(Species))
Abundance_pair

# 3) By Time since Invasion and Abundance
Abundance_pair <- group_by(Preference_Assessment, Time.since.Invasion, Caulerpa.in.Site, Treatment, Species) %>%
  summarise(mean_consumed=mean(Per_Consumed), sd=sd(Per_Consumed), se=sd(Per_Consumed)/sqrt(length(Species)), n=length(Species))
Abundance_pair


##### Plot data
### OLD-HIGH POPULATIONS
ggplot(subset(Abundance_pair, Time.since.Invasion=="Old" & Caulerpa.in.Site=="High"), aes(x=Treatment, y=mean_consumed, fill=Species)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean_consumed-se, ymax=mean_consumed+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  scale_y_continuous(name = c(expression(paste(bold("Mean percentage of algae consumed ± S.E")))), limit=c(0,100), breaks=seq(0,100,10)) +
  ggtitle("Old High populations") +
  ## scale_x_discrete(limits=c("October", "July")) +  If you want to change the order of the levels in the x axis and only choose some
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 1.8, size=20),
        axis.title.x = element_text(vjust= -0.5, size=20),
        axis.text.y = element_text(size=18, face="bold"),
        axis.text.x = element_text(size=18, face="bold"), 
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18), 
        legend.position = "bottom",
        plot.title = element_text(size=20, face="bold", hjust=0.5))


### OLD-LOW POPULATIONS
ggplot(subset(Abundance_pair, Time.since.Invasion=="Old" & Caulerpa.in.Site=="Low"), aes(x=Treatment, y=mean_consumed, fill=Species)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean_consumed-se, ymax=mean_consumed+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  scale_y_continuous(name = c(expression(paste(bold("Mean percentage of algae consumed ± S.E")))), limit=c(0,100), breaks=seq(0,100,10)) +
  ggtitle("Old Low populations") +
  ## scale_x_discrete(limits=c("October", "July")) +  If you want to change the order of the levels in the x axis and only choose some
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 1.8, size=20),
        axis.title.x = element_text(vjust= -0.5, size=20),
        axis.text.y = element_text(size=18, face="bold"),
        axis.text.x = element_text(size=18, face="bold"), 
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18), 
        legend.position = "bottom",
        plot.title = element_text(size=20, face="bold", hjust=0.5))


### RECENT-HIGH POPULATIONS
ggplot(subset(Abundance_pair, Time.since.Invasion=="Recent" & Caulerpa.in.Site=="High"), aes(x=Treatment, y=mean_consumed, fill=Species)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean_consumed-se, ymax=mean_consumed+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  scale_y_continuous(name = c(expression(paste(bold("Mean percentage of algae consumed ± S.E")))), limit=c(0,100), breaks=seq(0,100,10)) +
  ggtitle("Recent High populations") +
  ## scale_x_discrete(limits=c("October", "July")) +  If you want to change the order of the levels in the x axis and only choose some
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 1.8, size=20),
        axis.title.x = element_text(vjust= -0.5, size=20),
        axis.text.y = element_text(size=18, face="bold"),
        axis.text.x = element_text(size=18, face="bold"), 
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18), 
        legend.position = "bottom",
        plot.title = element_text(size=20, face="bold", hjust=0.5))


### RECENT-LOW POPULATIONS
ggplot(subset(Abundance_pair, Time.since.Invasion=="Recent" & Caulerpa.in.Site=="Low"), aes(x=Treatment, y=mean_consumed, fill=Species)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean_consumed-se, ymax=mean_consumed+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  scale_y_continuous(name = c(expression(paste(bold("Mean percentage of algae consumed ± S.E")))), limit=c(0,100), breaks=seq(0,100,10)) +
  ggtitle("Recent Low populations") +
  ## scale_x_discrete(limits=c("October", "July")) +  If you want to change the order of the levels in the x axis and only choose some
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 1.8, size=20),
        axis.title.x = element_text(vjust= -0.5, size=20),
        axis.text.y = element_text(size=18, face="bold"),
        axis.text.x = element_text(size=18, face="bold"), 
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18), 
        legend.position = "bottom",
        plot.title = element_text(size=20, face="bold", hjust=0.5))



############## ASSESS PREFERENCE CONSIDERING TIME SINCE INVASION AND ABUNDANCE AT THE SAME TIME #########

###### OLD-HIGH POPULATIONS ####
Old_High<-subset(Preference_Assessment, Time.since.Invasion=="Old" & Caulerpa.in.Site=="High")

##### PadiC
# Compute the difference per Treatment
d <- with(subset(Old_High, Treatment=="PadiC"), Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value = 0.27 so the data is normally distributed

with(subset(Old_High, Treatment=="PadiC"), qqPlot(Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"]))

### Check for equality of variances (Homocesdasticity)
res.ftest <- var.test(Per_Consumed ~ Species, data = subset(Old_High, Treatment=="PadiC"))
res.ftest
## p-value = 0.11 so the variances are equal

### We do a Student t-test because data is normally distributed and variances are equal
## Student t-test for normally distributed data
PadiCOld <- t.test(Per_Consumed ~ Species, data = subset(Old_High, Treatment=="PadiC"), paired = TRUE)
PadiCOld
### p-value < 0.05 so the means are significantly different and Sarpa prefers Caulerpa (according to the higher consumption of this species)

#### Do boxplot
ggplot(subset(Old_High, Treatment=="PadiC"), aes(x=Species, y=Per_Consumed, fill=Species)) +
  geom_boxplot() +
  scale_y_continuous(limit=c(0,100), breaks=seq(0,100,10),name = "Percentage of algae consumed") +
  ggtitle("OLD-HIGH") +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        # plot.title = element_text(size=14, face="bold", hjust=0.5),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 0.5, size=13),
        axis.title.x = element_text(vjust= -0.5, size=13),
        axis.text.y = element_text(size=11, face="bold"),
        axis.text.x = element_text(size=11, face="bold"),
        plot.title = element_text(size=20, face="bold", hjust=0.5))

with(subset(Old_High, Treatment=="PadiC" & Species=="Caulerpa"), mean(Per_Consumed))
with(subset(Old_High, Treatment=="PadiC" & Species=="Padina"), mean(Per_Consumed))


##############
########## CystoC
# Compute the difference per Treatment
d <- with(subset(Old_High, Treatment=="CystoC"), Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value = 0.98 so the data is normally distributed

with(subset(Old_High, Treatment=="CystoC"), qqPlot(Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"]))

### Check for equality of variances (Homocesdasticity)
res.ftest <- var.test(Per_Consumed ~ Species, data = subset(Old_High, Treatment=="CystoC"))
res.ftest
## p-value = 0.70 so the variances are equal

### We do a Student t-test because data is normally distributed and variances are equal
#### Wilcoxon test for not normally distributed data
CystoCMen <- t.test(Per_Consumed ~ Species, data = subset(Old_High, Treatment=="CystoC"), paired = TRUE)
CystoCMen
### p-value <0.05 so there are significant differences and Sarpa prefers Caulerpa (according to the higher consumption of this species)


#### Do boxplot
ggplot(subset(Old_High, Treatment=="CystoC"), aes(x=Species, y=Per_Consumed, fill=Species)) +
  geom_boxplot() +
  scale_y_continuous(limit=c(0,100), breaks=seq(0,100,10),name = "Percentage of algae consumed") +
  ggtitle("OLD-HIGH") +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        # plot.title = element_text(size=14, face="bold", hjust=0.5),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 0.5, size=13),
        axis.title.x = element_text(vjust= -0.5, size=13),
        axis.text.y = element_text(size=11, face="bold"),
        axis.text.x = element_text(size=11, face="bold"),
        plot.title = element_text(size=20, face="bold", hjust=0.5))

with(subset(Old_High, Treatment=="CystoC" & Species=="Caulerpa"), mean(Per_Consumed))
with(subset(Old_High, Treatment=="CystoC" & Species=="Cystoseira"), mean(Per_Consumed))



#########
###### OLD-LOW POPULATIONS ####
Old_Low<-subset(Preference_Assessment, Time.since.Invasion=="Old" & Caulerpa.in.Site=="Low")

##### PadiC
# Compute the difference per Treatment
d <- with(subset(Old_Low, Treatment=="PadiC"), Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value = 0.26 so the data is normally distributed

with(subset(Old_Low, Treatment=="PadiC"), qqPlot(Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"]))

### Check for equality of variances (Homocesdasticity)
res.ftest <- var.test(Per_Consumed ~ Species, data = subset(Old_Low, Treatment=="PadiC"))
res.ftest
## p-value < 0.05 so the variances are NOT equal

### We do a Wilcoxon test  because variances are not equal
## Wilcoxon test
PadiCOld <- wilcox.test(Per_Consumed ~ Species, data = subset(Old_Low, Treatment=="PadiC"), conf.int=TRUE, paired = TRUE)
PadiCOld
### p-value < 0.05 so the means are significantly different and Sarpa does not have a preference

## Calculate z from Wilcoxon test
Z <-  qnorm(PadiCOld$p.value/2)
Z


#### Do boxplot
ggplot(subset(Old_Low, Treatment=="PadiC"), aes(x=Species, y=Per_Consumed, fill=Species)) +
  geom_boxplot() +
  scale_y_continuous(limit=c(0,100), breaks=seq(0,100,10),name = "Percentage of algae consumed") +
  ggtitle("OLD-LOW") +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        # plot.title = element_text(size=14, face="bold", hjust=0.5),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 0.5, size=13),
        axis.title.x = element_text(vjust= -0.5, size=13),
        axis.text.y = element_text(size=11, face="bold"),
        axis.text.x = element_text(size=11, face="bold"),
        plot.title = element_text(size=20, face="bold", hjust=0.5))

with(subset(Old_Low, Treatment=="PadiC" & Species=="Caulerpa"), mean(Per_Consumed))
with(subset(Old_Low, Treatment=="PadiC" & Species=="Padina"), mean(Per_Consumed))


##############
########## CystoC
# Compute the difference per Treatment
d <- with(subset(Old_Low, Treatment=="CystoC"), Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value = 0.25 so the data is normally distributed

with(subset(Old_Low, Treatment=="CystoC"), qqPlot(Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"]))

### Check for equality of variances (Homocesdasticity)
res.ftest <- var.test(Per_Consumed ~ Species, data = subset(Old_Low, Treatment=="CystoC"))
res.ftest
## p-value = 0.07 so the variances are equal

### We do a Student t-test because data is normally distributed and variances are equal
#### Wilcoxon test for not normally distributed data
CystoCMen <- t.test(Per_Consumed ~ Species, data = subset(Old_Low, Treatment=="CystoC"), paired = TRUE)
CystoCMen
### p-value <0.05 so there are significant differences and Sarpa prefers Caulerpa


#### Do boxplot
ggplot(subset(Old_Low, Treatment=="CystoC"), aes(x=Species, y=Per_Consumed, fill=Species)) +
  geom_boxplot() +
  scale_y_continuous(limit=c(0,100), breaks=seq(0,100,10),name = "Percentage of algae consumed") +
  ggtitle("OLD-LOW") +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        # plot.title = element_text(size=14, face="bold", hjust=0.5),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 0.5, size=13),
        axis.title.x = element_text(vjust= -0.5, size=13),
        axis.text.y = element_text(size=11, face="bold"),
        axis.text.x = element_text(size=11, face="bold"),
        plot.title = element_text(size=20, face="bold", hjust=0.5))

with(subset(Old_Low, Treatment=="CystoC" & Species=="Caulerpa"), mean(Per_Consumed))
with(subset(Old_Low, Treatment=="CystoC" & Species=="Cystoseira"), mean(Per_Consumed))



#########
###### RECENT-HIGH POPULATIONS ####
Recent_High<-subset(Preference_Assessment, Time.since.Invasion=="Recent" & Caulerpa.in.Site=="High")

##### PadiC
# Compute the difference per Treatment
d <- with(subset(Recent_High, Treatment=="PadiC"), Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value < 0.05 so the data is NOT normally distributed

with(subset(Recent_High, Treatment=="PadiC"), qqPlot(Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"]))

### Check for equality of variances (Homocesdasticity)
res.ftest <- var.test(Per_Consumed ~ Species, data = subset(Recent_High, Treatment=="PadiC"))
res.ftest
## p-value = 0.06 so the variances are equal

### We do a Wilcoxon test because data is not normally distributed
#### Wilcoxon test for not normally distributed data
PadiCRecent <- wilcox.test(Per_Consumed ~ Species, data = subset(Recent_High, Treatment=="PadiC"), conf.int=TRUE, paired = TRUE)
PadiCRecent
### p-value=0.44 so there are NOT significant differences and Sarpa does not prefer Caulerpa over Padina

## Calculate z from Wilcoxon test
Z <-  qnorm(PadiCRecent$p.value/2)
Z


#### Do boxplot
ggplot(subset(Recent_High, Treatment=="PadiC"), aes(x=Species, y=Per_Consumed, fill=Species)) +
  geom_boxplot() +
  scale_y_continuous(limit=c(0,100), breaks=seq(0,100,10),name = "Percentage of algae consumed") +
  ggtitle("RECENT-HIGH") +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        # plot.title = element_text(size=14, face="bRecent", hjust=0.5),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 0.5, size=13),
        axis.title.x = element_text(vjust= -0.5, size=13),
        axis.text.y = element_text(size=11, face="bold"),
        axis.text.x = element_text(size=11, face="bold"),
        plot.title = element_text(size=20, face="bold", hjust=0.5))

with(subset(Recent_High, Treatment=="PadiC" & Species=="Caulerpa"), mean(Per_Consumed))
with(subset(Recent_High, Treatment=="PadiC" & Species=="Padina"), mean(Per_Consumed))


##############
########## CystoC
# Compute the difference per Treatment
d <- with(subset(Recent_High, Treatment=="CystoC"), Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value = 0.99 so the data is normally distributed

with(subset(Recent_High, Treatment=="CystoC"), qqPlot(Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"]))

### Check for equality of variances (Homocesdasticity)
res.ftest <- var.test(Per_Consumed ~ Species, data = subset(Recent_High, Treatment=="CystoC"))
res.ftest
## p-value = 0.22 so the variances are equal

### We do a Student t-test because the data is normal and the variances are equal
#### Student t-test for normally distributed data
CystoCMen <- t.test(Per_Consumed ~ Species, data = subset(Recent_High, Treatment=="CystoC"), paired = TRUE)
CystoCMen
### p-value=0.44 so there are NOT significant differences and Sarpa does not prefer Caulerpa over Cystoseira

#### Do boxplot
ggplot(subset(Recent_High, Treatment=="CystoC"), aes(x=Species, y=Per_Consumed, fill=Species)) +
  geom_boxplot() +
  scale_y_continuous(limit=c(0,100), breaks=seq(0,100,10),name = "Percentage of algae consumed") +
  ggtitle("RECENT-HIGH") +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        # plot.title = element_text(size=14, face="bRecent", hjust=0.5),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 0.5, size=13),
        axis.title.x = element_text(vjust= -0.5, size=13),
        axis.text.y = element_text(size=11, face="bold"),
        axis.text.x = element_text(size=11, face="bold"),
        plot.title = element_text(size=20, face="bold", hjust=0.5))

with(subset(Recent_High, Treatment=="CystoC" & Species=="Caulerpa"), mean(Per_Consumed))
with(subset(Recent_High, Treatment=="CystoC" & Species=="Cystoseira"), mean(Per_Consumed))



#########
###### RECENT-LOW POPULATIONS ####
Recent_Low<-subset(Preference_Assessment, Time.since.Invasion=="Recent" & Caulerpa.in.Site=="Low")

##### PadiC
# Compute the difference per Treatment
d <- with(subset(Recent_Low, Treatment=="PadiC"), Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value=0.99 so the data is normally distributed

with(subset(Recent_Low, Treatment=="PadiC"), qqPlot(Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"]))

### Check for equality of variances (Homocesdasticity)
res.ftest <- var.test(Per_Consumed ~ Species, data = subset(Recent_Low, Treatment=="PadiC"))
res.ftest
## p-value=0.23 so the variances are qual

### We do a Student t test
#### Student t test
PadiCRecent <- t.test(Per_Consumed ~ Species, data = subset(Recent_Low, Treatment=="PadiC"), paired = TRUE)
PadiCRecent
### p-value=0.55 so there are NOT significant differences and Sarpa does not prefer Caulerpa over Padina

#### Do boxplot
ggplot(subset(Recent_Low, Treatment=="PadiC"), aes(x=Species, y=Per_Consumed, fill=Species)) +
  geom_boxplot() +
  scale_y_continuous(limit=c(0,100), breaks=seq(0,100,10),name = "Percentage of algae consumed") +
  ggtitle("RECENT-LOW") +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        # plot.title = element_text(size=14, face="bRecent", hjust=0.5),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 0.5, size=13),
        axis.title.x = element_text(vjust= -0.5, size=13),
        axis.text.y = element_text(size=11, face="bold"),
        axis.text.x = element_text(size=11, face="bold"),
        plot.title = element_text(size=20, face="bold", hjust=0.5))

with(subset(Recent_Low, Treatment=="PadiC" & Species=="Caulerpa"), mean(Per_Consumed))
with(subset(Recent_Low, Treatment=="PadiC" & Species=="Padina"), mean(Per_Consumed))


##############
########## CystoC
# Compute the difference per Treatment
d <- with(subset(Recent_Low, Treatment=="CystoC"), Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value < 0.05 so the data is NOT normally distributed

with(subset(Recent_Low, Treatment=="CystoC"), qqPlot(Per_Consumed[Species== "Caulerpa"] - Per_Consumed[Species != "Caulerpa"]))

### Check for equality of variances (Homocesdasticity)
res.ftest <- var.test(Per_Consumed ~ Species, data = subset(Recent_Low, Treatment=="CystoC"))
res.ftest
## p-value = 0.63 so the variances are equal

### We do a Wilcoxon test because the data is NOT normal
#### Wilcoxon test
CystoCMen <- wilcox.test(Per_Consumed ~ Species, data = subset(Recent_Low, Treatment=="CystoC"), conf.int=TRUE, paired = TRUE)
CystoCMen
### p-value=0.78 so there are NOT significant differences and Sarpa does not prefer Caulerpa over Cystoseira

## Calculate z from Wilcoxon test
Z <-  qnorm(CystoCMen$p.value/2)
Z


#### Do boxplot
ggplot(subset(Recent_Low, Treatment=="CystoC"), aes(x=Species, y=Per_Consumed, fill=Species)) +
  geom_boxplot() +
  scale_y_continuous(limit=c(0,100), breaks=seq(0,100,10),name = "Percentage of algae consumed") +
  ggtitle("RECENT") +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        # plot.title = element_text(size=14, face="bRecent", hjust=0.5),
        legend.title = element_text(size=13, face="bold"),
        legend.text = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 0.5, size=13),
        axis.title.x = element_text(vjust= -0.5, size=13),
        axis.text.y = element_text(size=11, face="bold"),
        axis.text.x = element_text(size=11, face="bold"),
        plot.title = element_text(size=20, face="bold", hjust=0.5))

with(subset(Recent_Low, Treatment=="CystoC" & Species=="Caulerpa"), mean(Per_Consumed))
with(subset(Recent_Low, Treatment=="CystoC" & Species=="Cystoseira"), mean(Per_Consumed))




#######################################################################################
#######################################################################################
######## 3. Analysis of invasive algae consumption by native herbivores ###############
#######################################################################################

### Load the data
load("Algae_Consumption.RData")
summary(Algae_Consumption)


#####################################################
########## PRESENCE OF CAULERPA IN PELLETS ##########

### Summary by Time.since.Invasion and Caulerpa.in.Site
PelletsPresTimeAbun <- mutate(Algae_Consumption, Presence=if_else(Caulerpa_Presence==0, "NoCaulerpa", "Caulerpa")) %>%
  group_by(Time.since.Invasion, Caulerpa.in.Site, Presence) %>%
  summarise(Number=length(Presence))%>%
  mutate(Prop=(Number*100)/sum(Number))
PelletsPresTimeAbun


ggplot(subset(PelletsPresTimeAbun, Presence=="Caulerpa"), aes(x=Time.since.Invasion, y=Prop, fill=Caulerpa.in.Site))+
  geom_bar(position=position_dodge(), stat="identity") +
  scale_y_continuous(name = c(expression(paste(bold("Proportion of pellets with "), bolditalic("Caulerpa cylindracea")))), limit=c(0,100), breaks=seq(0,100,10)) +
  scale_fill_discrete("Presence") +
  ggtitle(expression(paste(bold ("Proportion of "),bolditalic("Sarpa salpa"), bold(" pellets´ with "), bolditalic("Caulerpa cylindracea")))) +
  ## scale_x_discrete(limits=c("October", "July")) +  If you want to change the order of the levels in the x axis and only choose some
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 1.8, size=20),
        axis.title.x = element_text(vjust= -0.5, size=20),
        axis.text.y = element_text(size=18, face="bold"),
        axis.text.x = element_text(size=18, face="bold"), 
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18), 
        legend.position = "bottom",
        plot.title = element_text(size=20, face="bold", hjust=0.5))


PelletsPresTimeAbun$Interaction <- interaction(PelletsPresTimeAbun$Time.since.Invasion, PelletsPresTimeAbun$Caulerpa.in.Site)
PelletsPresTimeAbun$Interaction <- factor(PelletsPresTimeAbun$Interaction, levels=c("Old.High", "Old.Low", "Recent.High", "Recent.Low"))
PelletsPresTimeAbun$Presence <- factor(PelletsPresTimeAbun$Presence, levels=c("NoCaulerpa","Caulerpa"))

ggplot(PelletsPresTimeAbun, aes(x=Interaction, y=Prop, fill=Presence))+
  geom_bar(position="stack", stat="identity") +
  scale_y_continuous(name = c(expression(paste(bold("Proportion of pellets with "), bolditalic("Caulerpa cylindracea")))), limit=c(0,100), breaks=seq(0,100,10)) +
  scale_fill_discrete("Presence") +
  ggtitle(expression(paste(bold ("Proportion of "),bolditalic("Sarpa salpa"), bold(" pellets´ with "), bolditalic("Caulerpa cylindracea")))) +
  ## scale_x_discrete(limits=c("October", "July")) +  If you want to change the order of the levels in the x axis and only choose some
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 1.8, size=20),
        axis.title.x = element_text(vjust= -0.5, size=20),
        axis.text.y = element_text(size=18, face="bold"),
        axis.text.x = element_text(size=18, face="bold"), 
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18), 
        legend.position = "bottom",
        plot.title = element_text(size=20, face="bold", hjust=0.5))



###### Model to assess the presence of Caulerpa in pellets ####
### As we have data on the presence of Caulerpa in the pellets, we will do a binomial model
## Caulerpa presence in relation to Time since Invasion and Caulerpa Abundance in the site

pelletpres_mod <- glm(Caulerpa_Presence~Time.since.Invasion*Caulerpa.in.Site, family=binomial(link="logit"), data=Algae_Consumption)
## No significant interaction
summary(pelletpres_mod)
Anova(pelletpres_mod)
## Significant interaction

## Assess significant levels in the interaction between factors
pairs(emmeans(pelletpres_mod, ~ Time.since.Invasion|Caulerpa.in.Site))
pairs(emmeans(pelletpres_mod, ~ Caulerpa.in.Site|Time.since.Invasion))

plot(resid(pelletpres_mod) ~ fitted(pelletpres_mod))
abline(h=0, lty=2)
lines(smooth.spline(resid(pelletpres_mod) ~ fitted(pelletpres_mod)))


## Different ways to visualize the interaction between factors
visreg(pelletpres_mod, "Time.since.Invasion", by="Caulerpa.in.Site", scale="response")


visreg(pelletpres_mod, "Time.since.Invasion", by="Caulerpa.in.Site", scale="response", overlay=T, gg=T) +
  scale_y_continuous(name = c(expression(paste(bold("Proportion of salpas with "), bolditalic("C. cylindracea "), bold("in pellets")))), limit=c(0,1), breaks=seq(0,1,0.1)) +
  ggtitle("Caulerpa consumption") +
  ## scale_x_discrete(limits=c("October", "July")) +  If you want to change the order of the levels in the x axis and only choose some
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 1.8, size=20),
        axis.title.x = element_text(vjust= -0.5, size=20),
        axis.text.y = element_text(size=18, face="bold"),
        axis.text.x = element_text(size=18, face="bold"), 
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18), 
        legend.position = "bottom",
        plot.title = element_text(size=20, face="bold", hjust=0.5))



ModelPelletPres<-interactionMeans(pelletpres_mod)
plot(ModelPelletPres)


plot_model(pelletpres_mod, vline.color = "black")
plot_model(pelletpres_mod, vline.color = "black", show.values = T, value.offset = .3)
plot_model(pelletpres_mod, vline.color= "black", show.values = T, type = "pred", value.offset = .4)
plot_model(pelletpres_mod, vline.color= "black", show.values = T, type = "int", value.offset = .4)
plot_model(pelletpres_mod, vline.color= "black", show.values = T, type = "resid", value.offset = .4)


plot(allEffects(pelletpres_mod), type="response")



################################################################################
########## PROPORTION OF CAULERPA IN PELLETS (PER CAPITA CONSUMPTION) ##########

### Summary by Time.since.Invasion and Caulerpa.in.Site
PelletsPerTimeAbun <- group_by(Algae_Consumption, Time.since.Invasion, Caulerpa.in.Site) %>%
  summarise(mean_consumed=mean(Caulerpa_Per), sd=sd(Caulerpa_Per), se=sd(Caulerpa_Per)/sqrt(length(Salpa_Size)), n=length(Salpa_Size))
PelletsPerTimeAbun

ggplot(PelletsPerTimeAbun, aes(x=Time.since.Invasion, y=mean_consumed, fill=Caulerpa.in.Site)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean_consumed-se, ymax=mean_consumed+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  scale_y_continuous(name = c(expression(paste(bold("Mean percentage in pelletes ± S.E")))), limit=c(0,60), breaks=seq(0,60,10)) +
  ggtitle(expression(paste(bold ("Percentage of "), bolditalic("Caulerpa cylindracea"), bold(" in pellets")))) +
  ## scale_x_discrete(limits=c("October", "July")) +  If you want to change the order of the levels in the x axis and only choose some
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 1.8, size=20),
        axis.title.x = element_text(vjust= -0.5, size=20),
        axis.text.y = element_text(size=18, face="bold"),
        axis.text.x = element_text(size=18, face="bold"), 
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18), 
        legend.position = "bottom",
        plot.title = element_text(size=20, face="bold", hjust=0.5))


###### Model to assess the proportion of Caulerpa in pellets (Per capita consumption) ####
### As we have data on the proportion of Caulerpa in the pellets, we will do a binomial model
## Caulerpa proportion in relation to Time since Invasion and Caulerpa Abundance in the site

# Trnasform variable to integer to perform the binomial model
Algae_Consumption$Caulerpa_Per <- as.integer(Algae_Consumption$Caulerpa_Per)
Algae_Consumption$No.Caulerpa_Per <- as.integer(Algae_Consumption$No.Caulerpa_Per)

pelletprop_mod <- glm(cbind(Caulerpa_Per, No.Caulerpa_Per)~Time.since.Invasion*Caulerpa.in.Site, family=binomial(link="logit"), data=Algae_Consumption)
summary(pelletprop_mod)
Anova(pelletprop_mod)
## Significant interaction

## Assess significant levels in the interaction between factors
pairs(emmeans(pelletprop_mod, ~ Time.since.Invasion|Caulerpa.in.Site))
pairs(emmeans(pelletprop_mod, ~ Caulerpa.in.Site|Time.since.Invasion))


plot(resid(pelletprop_mod) ~ fitted(pelletprop_mod))
abline(h=0, lty=2)
lines(smooth.spline(resid(pelletprop_mod) ~ fitted(pelletprop_mod)))


## Different ways to visualize the interaction between factors
visreg(pelletprop_mod, "Time.since.Invasion", by="Caulerpa.in.Site", overlay=T, scale="response") 

visreg(pelletprop_mod, "Time.since.Invasion", by="Caulerpa.in.Site", overlay=T, scale="response", gg=T) +
  scale_y_continuous(name = c(expression(paste(bold("Proportion of "), bolditalic("C. cylindracea "), bold("in pellets")))), limit=c(0,1), breaks=seq(0,1,0.1)) +
  ggtitle("Caulerpa consumption") +
  ## scale_x_discrete(limits=c("October", "July")) +  If you want to change the order of the levels in the x axis and only choose some
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 1.8, size=20),
        axis.title.x = element_text(vjust= -0.5, size=20),
        axis.text.y = element_text(size=18, face="bold"),
        axis.text.x = element_text(size=18, face="bold"), 
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18), 
        legend.position = "bottom",
        plot.title = element_text(size=20, face="bold", hjust=0.5))


ModelPelletProp<-interactionMeans(pelletprop_mod)
plot(ModelPelletProp)


plot_model(pelletprop_mod, vline.color = "black")
plot_model(pelletprop_mod, vline.color = "black", show.values = T, value.offset = .3)
plot_model(pelletprop_mod, vline.color= "black", show.values = T, type = "pred", value.offset = .4)
plot_model(pelletprop_mod, vline.color= "black", show.values = T, type = "int", value.offset = .4)
plot_model(pelletprop_mod, vline.color= "black", show.values = T, type = "resid", value.offset = .4)


plot(allEffects(pelletprop_mod), type="response")




##############################################################################################
##############################################################################################
######## 4. Assess potential differences in the Electivity Index between years ###############
##############################################################################################


### Load the data
load("Ivlev_Comparison.RData")
summary(Ivlev_Comparison)


#### Calculate Ivlev Indexes per location and year ####
Ivlev_Location <- group_by(Ivlev_Comparison, Location, Year) %>%
  mutate(ivlev=(Caulerpa_Per-Caulerpa.in.community)/(Caulerpa_Per+Caulerpa.in.community)) %>%
  summarise(mean_ivlev=mean(ivlev), se=sd(ivlev)/sqrt(length(Location)))
Ivlev_Location

Ivlev_Comparison$Ivlev <- ((Ivlev_Comparison$Caulerpa_Per-Ivlev_Comparison$Caulerpa.in.community)/(Ivlev_Comparison$Caulerpa_Per+Ivlev_Comparison$Caulerpa.in.community))


#### Compare Ivlev Indexes between years at each location ####

#### CABRERA####
## Plot data
ggboxplot(subset(Ivlev_Comparison, Location=="Cabrera"), x="Year", y ="Ivlev", 
          color = "Year", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("2007", "2008", "2020"),
          ylab = "Ivlev Index", xlab = "Year")

p <- ggline(subset(Ivlev_Comparison, Location=="Cabrera"), x="Year", y ="Ivlev", 
            add=c("mean_se", "jitter"),
            add.params=list(size=2),
            order = c("2007", "2008", "2020"),
            point.size=2,
            ylab = "Ivlev Index", xlab = "Year")
ggpar(p, ylim = c(-1.0, 1.0))

par(mfrow=c(1,1))

## Do Anova to check normality of residuals
Cabrera.aov <- aov(Ivlev~Year, data=subset(Ivlev_Comparison, Location=="Cabrera"))
summary(Cabrera.aov)

## Check normality
plot(Cabrera.aov, 2)
shapiro.test(residuals(Cabrera.aov)) ## Data is not normal

## Check homogeneity of variances across groups
plot(Cabrera.aov, 1)
leveneTest(Ivlev~Year, data=subset(Ivlev_Comparison, Location=="Cabrera")) ## Variances are homocedastic.


##### As the data is not normal, we have to use a non-paramtetric test
## We use the Kruskall-Wallis Test
Cabrera.krus <- kruskal.test(Ivlev~Year, data=subset(Ivlev_Comparison, Location=="Cabrera"))
Cabrera.krus ## Significant differences between years

## Most appropriate method to perform pairwise comparisons after Kruskall Wallis test is to use the Dunn Test
dunnTest(Ivlev~Year, data=subset(Ivlev_Comparison, Location=="Cabrera"), method="bh")
## Significant differences in the Ivlev Index between 2007-2020 and 2008-2020.



#### ROSES ####
## Plot data
ggboxplot(subset(Ivlev_Comparison, Location=="Catalunya"), x="Year", y ="Ivlev", 
          color = "Year", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("2018", "2019", "2020"),
          ylab = "Ivlev Index", xlab = "Year")

p <- ggline(subset(Ivlev_Comparison, Location=="Catalunya"), x="Year", y ="Ivlev", 
            add=c("mean_se", "jitter"),
            add.params=list(shape=17, size=2.5),
            shape=17,
            order = c("2018", "2019", "2020"),
            ylab = "Ivlev Index", xlab = "Year")

ggpar(p, ylim = c(-1, 1))

par(mfrow=c(1,1))

## Do Anova to check normality of residuals
Roses.aov <- aov(Ivlev~Year, data=subset(Ivlev_Comparison, Location=="Catalunya"))
summary(Roses.aov)


## Check normality
plot(Roses.aov, 2)
shapiro.test(residuals(Roses.aov)) ## Data is not normal

## Check homogeneity of variances across groups
plot(Roses.aov, 1)
leveneTest(Ivlev~Year, data=subset(Ivlev_Comparison, Location=="Catalunya")) ## Variances are heteroscedastic.


### As the data is not normal and heteroscedastic, we have to use a non-paramtetric test
## We use the Kruskall-Wallis Test
Roses.krus <- kruskal.test(Ivlev~Year, data=subset(Ivlev_Comparison, Location=="Catalunya"))
Roses.krus
## No significant differences between years


## Not necessary to do the pairwise comparison as the variable "Year" is not significant
## Most appropriate method to perform pairwise comparisons after Kruskall Wallis test is to use the Dunn Test
dunnTest(Ivlev~Year, data=subset(Ivlev_Comparison, Location=="Catalunya"), method="bh")
## No significant differences in the Ivlev Index between years.
