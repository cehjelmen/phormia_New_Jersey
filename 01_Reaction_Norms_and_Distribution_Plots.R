####Phormia dev time and weights project###
##Weidner Data
##Sept 22, 2026

#####read in packages#########
library(ggplot2)
library(dplyr)
library(viridis)
library(ggpubr)
########read in data.######## 
##data has been reformated from original data and cleaned up to make approriate
###visualization
pup.weight<-read.csv("pup_weight.csv")
ad.weight<-read.csv("adult_weight.csv")
dev.time<-read.csv("dev_time.csv")

####Fixing data structure issues#########
#make factors
pup.weight <- pup.weight %>% 
  mutate(across(c(Year, Temp, Region), as.factor))
#order factors for region to be north to south rather than alphabetical
pup.weight$Region<-factor(pup.weight$Region, levels=c("N", "C", "S"))

#For adult weight
ad.weight <- ad.weight %>% 
  mutate(across(c(Year, Temp, Region), as.factor))
ad.weight$Weight<-as.numeric(ad.weight$Weight)
ad.weight$Region<-factor(ad.weight$Region, levels=c("N", "C", "S"))

#development time
dev.time <- dev.time %>% 
  mutate(across(c(Year, Temp, Region), as.factor))
dev.time$ldev<-as.numeric(dev.time$ldev)
dev.time$Region<-factor(dev.time$Region, levels=c("N", "C", "S"))


# ad.weight.2<-na.omit(ad.weight)

# #distribution
# ggplot(ad.weight.2, aes(x=Weight))+
#   geom_density()

####Pupal weight Reaction Norm####
#get summary info for pupal weight
sum_df <- pup.weight %>%
  group_by(Year, Region, Temp) %>%
  summarise(
    n    = n(),
    mean = mean(Weight, na.rm = TRUE),
    sd   = sd(Weight, na.rm = TRUE),
    se   = sd / sqrt(n),
    ci   = qt(0.975, n - 1) * se,
    med  = median(Weight, na.rm = TRUE),
    q25  = quantile(Weight, 0.25, na.rm = TRUE),
    q75  = quantile(Weight, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(region = factor(Region, levels = c("N", "C", "S")),
         year   = factor(Year))
#this is for the visualization
pd <- position_dodge(width = 0.01)

pup.reac<-ggplot(sum_df, aes(x = factor(Temp), y = mean,
                   colour = Region, group = Region)) +
  geom_line(position = pd, linewidth = 1.3) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                width = 0.3, position = pd) +
  geom_point(position = pd, size = 4) +
  facet_wrap(~ year) +
  scale_color_viridis_d(begin=0.2, end=0.85)+
  labs(x = "Temperature (°C)", y = "Weight (g)",
       colour = "Region") +
  ggtitle("Pupal Weight")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size=16),
        axis.text.x=element_text(size=14, face="bold", color="black"),
        axis.text.y=element_text(size=13, face="bold", color="black"),
        axis.title=element_text(size=16, face="bold"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        legend.position="bottom",
        plot.title=element_text(size=16, face="bold")
        )

####Adult Weight Reaction Norm####
#summary info
sum_df.ad <- ad.weight %>%
  group_by(Year, Region, Temp) %>%
  summarise(
    n    = n(),
    mean = mean(Weight, na.rm = TRUE),
    sd   = sd(Weight, na.rm = TRUE),
    se   = sd / sqrt(n),
    ci   = qt(0.975, n - 1) * se,
    med  = median(Weight, na.rm = TRUE),
    q25  = quantile(Weight, 0.25, na.rm = TRUE),
    q75  = quantile(Weight, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(region = factor(Region, levels = c("N", "C", "S")),
         year   = factor(Year))
#for visualization
pd.ad <- position_dodge(width = 0.01)

ad.reac<-ggplot(sum_df.ad, aes(x = factor(Temp), y = mean,
                   colour = Region, group = Region)) +
  geom_line(position = pd.ad, linewidth = 1.3) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                width = 0.3, position = pd) +
  geom_point(position = pd.ad, size = 4) +
  facet_wrap(~ Year) +
  scale_color_viridis_d(begin=0.2, end=0.85)+
  labs(x = "Temperature (°C)", y = "Weight (g)",
       colour = "Region") +
  #theme_classic(base_size = 14) +
  ggtitle("Adult Weight")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size=16),
        axis.text.x=element_text(size=14, face="bold", color="black"),
        axis.text.y=element_text(size=13, face="bold", color="black"),
        axis.title=element_text(size=16, face="bold"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        legend.position="bottom",
        plot.title=element_text(size=16, face="bold")
  )


####larval dev time reaction norm####
sum_df.ldev <- dev.time %>%
  group_by(Year, Region, Temp) %>%
  summarise(
    n    = n(),
    mean = mean(ldev, na.rm = TRUE),
    sd   = sd(ldev, na.rm = TRUE),
    se   = sd / sqrt(n),
    ci   = qt(0.975, n - 1) * se,
    med  = median(ldev, na.rm = TRUE),
    q25  = quantile(ldev, 0.25, na.rm = TRUE),
    q75  = quantile(ldev, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(region = factor(Region, levels = c("N", "C", "S")),
         year   = factor(Year))
#as above
pd <- position_dodge(width = 0.01)

ldev.reac<-ggplot(sum_df.ldev, aes(x = factor(Temp), y = mean,
                      colour = Region, group = Region)) +
  geom_line(position = pd, linewidth = 1.3) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                width = 0.3, position = pd) +
  geom_point(position = pd, size = 4) +
  facet_wrap(~ Year) +
  scale_color_viridis_d(begin=0.2, end=0.85)+
  labs(x = "Temperature (°C)", y = "Dev. Time (h)",
       colour = "Region") +
  #theme_classic(base_size = 14) +
  ggtitle("Larval")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size=16),
        axis.text.x=element_text(size=14, face="bold", color="black"),
        axis.text.y=element_text(size=13, face="bold", color="black"),
        axis.title=element_text(size=16, face="bold"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        legend.position="bottom",
        plot.title=element_text(size=16, face="bold")
  )

####pup dev time reaction norm####
sum_df.pdev <- dev.time %>%
  group_by(Year, Region, Temp) %>%
  summarise(
    n    = n(),
    mean = mean(pdev, na.rm = TRUE),
    sd   = sd(pdev, na.rm = TRUE),
    se   = sd / sqrt(n),
    ci   = qt(0.975, n - 1) * se,
    med  = median(pdev, na.rm = TRUE),
    q25  = quantile(pdev, 0.25, na.rm = TRUE),
    q75  = quantile(pdev, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(region = factor(Region, levels = c("N", "C", "S")),
         year   = factor(Year))

pd <- position_dodge(width = 0.01)

pdev.reac<-ggplot(sum_df.pdev, aes(x = factor(Temp), y = mean,
                        colour = Region, group = Region)) +
  geom_line(position = pd, linewidth = 1.3) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                width = 0.3, position = pd) +
  geom_point(position = pd, size = 4) +
  facet_wrap(~ Year) +
  scale_color_viridis_d(begin=0.2, end=0.85)+
  labs(x = "Temperature (°C)", y = "Dev. Time (h)",
       colour = "Region") +
  #theme_classic(base_size = 14) +
  ggtitle("Pupal")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size=16),
        axis.text.x=element_text(size=14, face="bold", color="black"),
        axis.text.y=element_text(size=13, face="bold", color="black"),
        axis.title=element_text(size=16, face="bold"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        legend.position="bottom",,
        plot.title=element_text(size=16, face="bold")
  )

####total dev time reaction norm####
sum_df.tdev <- dev.time %>%
  group_by(Year, Region, Temp) %>%
  summarise(
    n    = n(),
    mean = mean(tdev, na.rm = TRUE),
    sd   = sd(tdev, na.rm = TRUE),
    se   = sd / sqrt(n),
    ci   = qt(0.975, n - 1) * se,
    med  = median(tdev, na.rm = TRUE),
    q25  = quantile(tdev, 0.25, na.rm = TRUE),
    q75  = quantile(tdev, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(region = factor(Region, levels = c("N", "C", "S")),
         year   = factor(Year))

pd <- position_dodge(width = 0.01)
#Total development reaction norm
tdev.reac<-ggplot(sum_df.tdev, aes(x = factor(Temp), y = mean,
                        colour = Region, group = Region)) +
  geom_line(position = pd, linewidth = 1.3) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                width = 0.3, position = pd) +
  geom_point(position = pd, size = 4) +
  facet_wrap(~ Year) +
  scale_color_viridis_d(begin=0.2, end=0.85)+
  labs(x = "Temperature (°C)", y = "Dev. Time (h)",
       colour = "Region") +
  #theme_classic(base_size = 14) +
  theme_bw()+
  ggtitle("Total")+
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size=16),
        axis.text.x=element_text(size=14, face="bold", color="black"),
        axis.text.y=element_text(size=13, face="bold", color="black"),
        axis.title=element_text(size=16, face="bold"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        legend.position="bottom",
        plot.title=element_text(size=16, face="bold")
  )


dodge=position_dodge(width=1)

#facet plots
ggarrange(pup.reac, ad.reac, nrow=2, labels="auto", common.legend = TRUE,
          legend="bottom",
          font.label = list(size=24, face="bold"))
ggarrange(ldev.reac, pdev.reac, tdev.reac, nrow=3, labels="auto", common.legend = TRUE,
          legend="bottom",
          font.label = list(size=24, face="bold"))

dodge=position_dodge(width=1)

#####boxplots with posthoc values plotted
#boxplots dev time
####Lavel Development Time########
#these labels for to include significance values resulting from previous
##statistical tests
label_df.20<-data.frame(Temp=20,
                        x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(425, 400, 435, 430, 380, 405),
                        label=c("a","b", "ab", "ab", "b", "ab"))
label_df.25<-data.frame(Temp=25,
                        x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(265, 255, 255, 265, 285, 330),
                        label=c("AB","A", "B", "B", "C", "BC"))
#the boxplots
ldev.box<-ggplot(dev.time, aes(x=Year, y=ldev, fill=Region))+
  geom_boxplot(position=dodge)+
  facet_wrap(~Temp, labeller = labeller(Temp = c("20" = "20°C", 
                                                 "25" = "25°C")))+
  geom_text(data=label_df.20, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  geom_text(data=label_df.25, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  scale_fill_viridis_d(begin=0.1, end=0.8)+
  theme_bw()+
  ylab("Dev. Time (h)")+
  theme(strip.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold", color="black"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=14, face="bold"),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14),
        legend.position="bottom",
        strip.background = element_rect(fill="grey95"))
print(ldev.box)

###This is for pupal dev. time##############
#sig. labels
label_pdf.20<-data.frame(Temp=20,
                         x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(250, 255, 250, 260, 265, 250),
                         label=c("a","a", "a", "b", "b", "b"))
label_pdf.25<-data.frame(Temp=25,
                         x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(195, 175, 190, 190, 170, 175),
                         label=c("AB","A", "B", "A", "A", "A"))
####the boxplot
pdev.box<-ggplot(dev.time, aes(x=Year, y=pdev, fill=Region))+
  geom_boxplot(position=dodge)+
  facet_wrap(~Temp, labeller = labeller(Temp = c("20" = "20°C", 
                                                 "25" = "25°C")))+
  geom_text(data=label_pdf.20, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  geom_text(data=label_pdf.25, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  scale_fill_viridis_d(begin=0.1, end=0.8)+
  theme_bw()+
  ylab("Dev. Time (h)")+
  theme(strip.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold", colour="black"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=14, face="bold"),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14),
        legend.position="bottom",
        strip.background = element_rect(fill="grey95"))
print(pdev.box)
######Total Development time
####labels of significance
label_tdf.20<-data.frame(Temp=20,
                         x=c(0.66, 1, 1.40, 1.66, 2, 2.33), y=c(620, 575, 565, 555, 580, 580),
                         label=c("a","a", "a", "b", "b", "b"))
label_tdf.25<-data.frame(Temp=25,
                         x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(390, 400, 400, 410, 420, 460),
                         label=c("AB","A", "B", "A", "AB", "AB"))
###the boxplot
tdev.box<-ggplot(dev.time, aes(x=Year, y=tdev, fill=Region))+
  geom_boxplot(position=dodge)+
  facet_wrap(~Temp, labeller = labeller(Temp = c("20" = "20°C", 
                                                 "25" = "25°C")))+
  geom_text(data=label_tdf.20, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  geom_text(data=label_tdf.25, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  scale_fill_viridis_d(begin=0.1, end=0.8)+
  theme_bw()+
  ylab("Dev. Time (h)")+
  theme(strip.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold", color="black"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=14, face="bold"),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14),
        legend.position="bottom",
        strip.background = element_rect(fill="grey95"))
print(tdev.box)
####arranging boxplots in one visualization#####
ggarrange(ldev.box, pdev.box, tdev.box,labels="auto", nrow=3,
          common.legend = TRUE, legend="bottom",
          font.label = list(size=18, face="bold"))

####Violin plots####
##violin plots are better for showing the distribution of the data
###rather than boxplots. This plot will plot both together
###labels and formating info#
dodge=position_dodge(width=1)
label_df.20<-data.frame(Temp=20,
                     x=c(0.66, 1, 1.33, 1.75, 2, 2.33), y=c(425, 400, 430, 430, 380, 405),
                     label=c("a","b", "ab", "ab", "b", "ab"))
label_df.25<-data.frame(Temp=25,
                        x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(265, 255, 255, 265, 285, 330),
                        label=c("AB","A", "B", "B", "C", "BC"))
###Larvel Development time violin plot
ldev.viol<-ggplot(dev.time, aes(x=Year, y=ldev, fill=Region))+
  geom_violin(width=1, position=dodge, alpha=1)+
  geom_boxplot(width=0.3, alpha=0.2, position=dodge, color="grey35")+
  facet_wrap(~Temp, labeller = labeller(Temp = c("20" = "20°C", 
                                                    "25" = "25°C")))+
  geom_text(data=label_df.20, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  geom_text(data=label_df.25, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  scale_fill_viridis_d(begin=0.2, end=0.85)+
  theme_bw()+
  ylab("Dev. Time (h)")+
  theme(strip.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold", color="black"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=14, face="bold"),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14),
        legend.position="bottom",
        strip.background = element_rect(fill="grey95"))
print(ldev.viol)

####pupal development time###
#labels
label_pdf.20<-data.frame(Temp=20,
                        x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(305, 260, 330, 265, 270, 320),
                        label=c("a","a", "a", "b", "b", "b"))
label_pdf.25<-data.frame(Temp=25,
                        x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(190, 215, 190, 190, 175, 210),
                        label=c("AB","A", "B", "A", "A", "A"))
###the actual plot
pdev.viol<-ggplot(dev.time, aes(x=Year, y=pdev, fill=Region))+
  geom_violin(width=1, position=dodge, alpha=1)+
  geom_boxplot(width=0.3, alpha=0.2, position=dodge, color="grey35")+
  facet_wrap(~Temp, labeller = labeller(Temp = c("20" = "20°C", 
                                                 "25" = "25°C")))+
  geom_text(data=label_pdf.20, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  geom_text(data=label_pdf.25, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  scale_fill_viridis_d(begin=0.2, end=0.85)+
  theme_bw()+
  ylab("Dev. Time (h)")+
  theme(strip.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold", color="black"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=14, face="bold"),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14),
        legend.position="bottom",
        strip.background = element_rect(fill="grey95"))
print(pdev.viol)
###Total Development time#####
#labels of significance#
label_tdf.20<-data.frame(Temp=20,
                         x=c(0.66, 1, 1.40, 1.66, 2, 2.33), y=c(620, 575, 615, 555, 580, 580),
                         label=c("a","a", "a", "b", "b", "b"))
label_tdf.25<-data.frame(Temp=25,
                         x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(390, 400, 400, 410, 420, 460),
                         label=c("AB","A", "B", "A", "AB", "AB"))
###the violin plot
tdev.viol<-ggplot(dev.time, aes(x=Year, y=tdev, fill=Region))+
  geom_violin(width=1, position=dodge, alpha=1)+
  geom_boxplot(width=0.3, alpha=0.2, position=dodge, color="grey35")+
  facet_wrap(~Temp, labeller = labeller(Temp = c("20" = "20°C", 
                                                 "25" = "25°C")))+
  geom_text(data=label_tdf.20, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  geom_text(data=label_tdf.25, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  scale_fill_viridis_d(begin=0.2, end=0.85)+
  theme_bw()+
  ylab("Dev. Time (h)")+
  theme(strip.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold", color="black"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=14, face="bold"),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14),
        legend.position="bottom",
        strip.background = element_rect(fill="grey95"))
print(tdev.viol)

####arranging development violin plots in one figure####
ggarrange(ldev.viol, pdev.viol, tdev.viol, nrow=3, labels="auto", common.legend = TRUE, legend="bottom",
          font.label = list(size=18, face="bold"))

####visualizations for weight####
#boxplots weight
###this is the labels for significance
label_df.20<-data.frame(Temp=20,
                        x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(0.05, 0.045, 0.045, 0.055, 0.05, 0.04),
                        label=c("a","a", "a", "a", "a", "a"))
label_df.25<-data.frame(Temp=25,
                        x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(0.058, 0.05, 0.045, 0.05, 0.045, 0.055),
                        label=c("A","ABC", "B", "C", "A", "C"))
###Pupal weight boxplot
pup.box<-ggplot(pup.weight, aes(x=Year, y=Weight, fill=Region))+
  geom_boxplot(position=dodge)+
  facet_wrap(~Temp, labeller = labeller(Temp = c("20" = "20°C", 
                                                 "25" = "25°C")))+
  geom_text(data=label_df.20, aes(x=x, y=y, label=label), inherit.aes=F,
            size=6, fontface="bold")+
  geom_text(data=label_df.25, aes(x=x, y=y, label=label), inherit.aes=F,
            size=6, fontface="bold")+
  scale_fill_viridis_d(begin=0.1, end=0.8)+
  theme_bw()+
  ylab("Pupal Weight (g)")+
  theme(strip.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold", color="black"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=14, face="bold"),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14),
        legend.position="bottom",
        strip.background = element_rect(fill="grey95"))
print(pup.box)

####adult weight boxplot####
#labels
label_adf.20<-data.frame(Temp=20,
                        x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(0.011, 0.012, 0.011, 0.011, 0.011, 0.011),
                        label=c("ab","a", "b", "ab", "ab", "a"))
label_adf.25<-data.frame(Temp=25,
                        x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(0.0135, 0.013, 0.012, 0.012, 0.011, 0.011),
                        label=c("AB","A", "B", "AB", "B", "AB"))
#the boxplot
ad.box<-ggplot(ad.weight, aes(x=Year, y=Weight, fill=Region))+
  geom_boxplot(position=dodge)+
  facet_wrap(~Temp, labeller = labeller(Temp = c("20" = "20°C", 
                                                 "25" = "25°C")))+
  geom_text(data=label_adf.20, aes(x=x, y=y, label=label), inherit.aes=F,
            size=6, fontface="bold")+
  geom_text(data=label_adf.25, aes(x=x, y=y, label=label), inherit.aes=F,
            size=6, fontface="bold")+
  scale_fill_viridis_d(begin=0.1, end=0.8)+
  theme_bw()+
  ylab("Adult Dry Weight (g)")+
  theme(strip.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold", color="black"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=14, face="bold"),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14),
        legend.position="bottom",
        strip.background = element_rect(fill="grey95"))
print(ad.box)
###arranging weights in boxplot figure####
ggarrange(pup.box, ad.box, labels="auto", nrow=2,
          common.legend = TRUE, legend="bottom",
          font.label = list(size=18, face="bold"))
####Weights in violin plots##########
#pupal weights
#labels of significance
label_df.20<-data.frame(Temp=20,
                        x=c(0.66, 1, 1.33, 1.66, 2, 2.4), y=c(0.055, 0.045, 0.045, 0.048, 0.046, 0.06),
                        label=c("a","a", "a", "a", "a", "a"))
label_df.25<-data.frame(Temp=25,
                        x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(0.055, 0.05, 0.047, 0.05, 0.045, 0.055),
                        label=c("A","ABC", "B", "C", "A", "C"))
##the plot##
pup.viol<-ggplot(pup.weight, aes(x=Year, y=Weight, fill=Region))+
  geom_violin(width=1, position=dodge, alpha=1)+
  geom_boxplot(width=0.3, alpha=0.2, position=dodge, color="grey35")+
  facet_wrap(~Temp, labeller = labeller(Temp = c("20" = "20°C", 
                                                 "25" = "25°C")))+
  geom_text(data=label_df.20, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  geom_text(data=label_df.25, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  scale_fill_viridis_d(begin=0.2, end=0.85)+
  theme_bw()+
  ylab("Pupal Weight (g)")+
  theme(strip.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold", color="black"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=14, face="bold"),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14),
        legend.position="bottom",
        strip.background = element_rect(fill="grey95"))
print(pup.viol)

###now for adult weight####
#labels
label_adf.20<-data.frame(Temp=20,
                         x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(0.0105, 0.011, 0.0105, 0.0105, 0.0105, 0.0105),
                         label=c("ab","a", "b", "ab", "ab", "a"))
label_adf.25<-data.frame(Temp=25,
                         x=c(0.66, 1, 1.33, 1.66, 2, 2.33), y=c(0.0135, 0.013, 0.012, 0.011, 0.011, 0.011),
                         label=c("AB","A", "B", "AB", "B", "AB"))
#the plot
ad.viol<-ggplot(ad.weight, aes(x=Year, y=Weight, fill=Region))+
  geom_violin(width=1, position=dodge, alpha=1)+
  geom_boxplot(width=0.3, alpha=0.2, position=dodge, color="grey35")+
  facet_wrap(~Temp, labeller = labeller(Temp = c("20" = "20°C", 
                                                 "25" = "25°C")))+
  geom_text(data=label_adf.20, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  geom_text(data=label_adf.25, aes(x=x, y=y, label=label), inherit.aes=F, 
            size=6, fontface="bold")+
  scale_fill_viridis_d(begin=0.2, end=0.85)+
  theme_bw()+
  ylab("Adult Dry Weight (g)")+
  theme(strip.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold", color="black"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=14, face="bold"),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14),
        legend.position="bottom",
        strip.background = element_rect(fill="grey95"))
print(ad.viol)
####arrangning the weights in one plot, violin####
ggarrange(pup.viol, ad.viol, labels="auto", nrow=2,
          common.legend = TRUE, legend="bottom",
          font.label = list(size=18, face="bold"))
