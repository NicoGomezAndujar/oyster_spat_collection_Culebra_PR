#Authors: Nicolas Gomez * Megan Considine
#Created:  1/4/2025 ; Culebra, Puerto Rico
#Last Updated:2/11/2026
#---Purpose: Exploratory data processing and analysis of spat.df collector data####

#-Packages ------------------------------------------------------------------------
#install.packages("")
library(tidyverse)
library(devtools)
library(readxl)
library(ggplot2)
library(EnvStats) # to add sample size to plots
library(RColorBrewer)
library("ggsci")# nature journal palette: scale_color_npg() 	 #the palette type is: "nrc" #palette geneator is:al_npg()
library(patchwork) #to output letters on each panel in combination with ggplot2 
library(forcats)
library(hrbrthemes)
library(ggthemes)
library(scales)
library(gridExtra)
library(ggpubr) 
library(cowplot)
library(RColorBrewer)
library(pd)


#set working directory and read data file

#Megan working directory: 
setwd("~/Mujeres de Islas/SK/R folder")

#---Nico working directory: ----
setwd("C:/Users/ngome/OneDrive/Desktop/Sk 2023/data/SK_ostras/SK_ostras_R_project/ostrasCulebraSK")

#Read Data files: ----

###DATA WRANGLING SPAT DATA FRAME--------------------------------
spat.df <- read.csv('Spat_Collection_Data.csv') #Excel file with the raw data from Google drive
view(spat.df)
#what's in the data frame?
is.data.frame(spat.df) #yes
#view(spat.df)
#head(spat.df)

summary(spat.df)

#create month column
spat.df$month <- 0
spat.df$month <- ifelse(spat.df$Collection.period == 'C1', 'May-June 2024', spat.df$month)
spat.df$month <- ifelse(spat.df$Collection.period == 'C2', 'July-Aug 2024', spat.df$month)
spat.df$month <- ifelse(spat.df$Collection.period == 'C3', 'Sept-Oct 2024', spat.df$month)
spat.df$month <- ifelse(spat.df$Collection.period == 'C4', 'Nov-Dec 2024', spat.df$month)
spat.df$month <- ifelse(spat.df$Collection.period == 'C5', 'Jan-Feb 2025', spat.df$month)
spat.df$month <- ifelse(spat.df$Collection.period == 'C6', 'Mar-May 2025', spat.df$month)


#make sure dependent variables are numeric
spat.df$abundance_mangrove <- as.numeric(spat.df$abundance_mangrove)
spat.df$abundance_atlantic <- as.numeric(spat.df$abundance_atlantic)

#make sure month is a factor: 
spat.df$month <- factor(spat.df$month)
#view(spat.df)



###MERGE SPAT DATA FRAME WITH TEMP & SALINITY-------------------------

#read in salinity (edited cvs file to have columns "Site.code" and "Collection.period")
salinity.df <- read.csv('Mean_hobo/Mean_Salinity_Collectors.csv')
summary(salinity.df)
head(salinity.df)
head(spat.df)

# Merge spat.df and salinity.df on 'Site.code' and 'Collection.period'
spat.s <- 0
spat.s <- merge(spat.df, salinity.df, by = c("Site.code", "Collection.period"))
summary(spat.s)


#read in shallow temp (edited cvs file to have columns "Site.code" and "Collection.period")
temps.df <- read.csv('Mean_hobo/Mean_Shallow_temp_collector.csv')

# Merge spat.df and tempd.df on 'Site.code' and 'Collection.period'
spat.st <- 0
spat.st <- merge(spat.s, temps.df, by = c("Site.code", "Collection.period"))


###EXTRA CODE FOR MERGING SHALLOW AND DEEP TEMP, AND CORRELATION FOR SHALLOW + DEEP
#MAKE DATAFRAME COMBINED TEMP BY TREATMENT - SHALLOW + DEEP
#read in temp combined (edited cvs file to have columns "Site.code", "Collection.period" and "Treatment")
temp2 <- read.csv('Mean_hobo/Mean_temp.csv')
spat.st2 <- 0
spat.st2 <- merge(spat.s, temp2, by = c("Site.code", "Collection.period", "Treatment"))

summary(spat.st2)
view(spat.st2)

#MAKE DATA FRAME HAVE 2 COLUMNS 1 SHALLOW TEMP AND 2 DEEP TEMP AVERAGED ACROSS TREATMENT
#read in deep temp (edited cvs file to have columns "Site.code" and "Collection.period")
#tempd.df <- read.csv('Mean_deep_temp_collector.csv')

# Merge spat.df and tempd.df on 'Site.code' and 'Collection.period'
#spat.st1 <- 0
#spat.st1 <- merge(spat.s, tempd.df, by = c("Site.code", "Collection.period"))

#correlation between shallow and deep temp
#cor(spat.st2$mean.temp_s,spat.st2$mean.temp_d, use = "pairwise.complete.obs") 
#0.6957, strong enough correlation to drop 1 and go with 1 temp type
#wilcox.test(spat.st2$mean.temp_s, spat.st2$mean.temp_d, paired=TRUE) 



###0----EXTRA CODE FOR MERGING SHALLOW AND DEEP TEMP, AND CORRELATION FOR SHALLOW + DEEP-----
#MAKE DATAFRAME COMBINED TEMP BY TREATMENT - SHALLOW + DEEP
#read in temp combined (edited cvs file to have columns "Site.code", "Collection.period" and "Treatment")
tempd.df <- read_xlsx('Mean_hobo/Mean_deep_temp_collector.xlsx')

# Merge spat.df and tempd.df on 'Site.code' and 'Collection.period'
spat.std <- 0
spat.std <- merge(spat.s, tempd.df, by = c("Site.code", "Collection.period"))


#MAKE DATA FRAME HAVE 2 COLUMNS 1 SHALLOW TEMP AND 2 DEEP TEMP AVERAGED ACROSS TREATMENT
#read in deep temp (edited cvs file to have columns "Site.code" and "Collection.period")
#tempd.df <- read.csv('Mean_deep_temp_collector.csv')

# Merge spat.df and tempd.df on 'Site.code' and 'Collection.period'
#spat.st1 <- 0
#spat.st1 <- merge(spat.s, tempd.df, by = c("Site.code", "Collection.period"))

#-----correlation between shallow and deep temp-----
#cor(spat.st2$mean.temp_s,spat.st2$mean.temp_d, use = "pairwise.complete.obs") 
#0.6957, strong enough correlation to drop 1 and go with 1 temp type
#wilcox.test(spat.st2$mean.temp_s, spat.st2$mean.temp_d, paired=TRUE) 

#Correlation between shallow temp and salinity
view(spat.st)
cor(spat.st$mean.temp_s,spat.st$mean.ppt, use = "pairwise.complete.obs")

p3 <- ggplot(spat.st, aes(x=mean.ppt, y=mean.temp_s)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  facet_wrap(~Site.code, scales="free_y")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text = element_text( size = 11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank(),
        strip.text = element_text(size = 11))+
  labs(x="Salinity (ppt)" , y="Shallow Water Temperature °C")
p3
#-0.6474, 

Corr_SST_S_CPI <- spat.st %>%
  filter(Site.code == "CPI") %>%
  summarize(cor = cor(mean.temp_s,mean.ppt, use = "pairwise.complete.obs"))
Corr_SST_S_CPI
Corr_SST_S_LPE <- spat.st %>%
  filter(Site.code == "LPE") %>%
  summarize(cor = cor(mean.temp_s,mean.ppt, use = "pairwise.complete.obs"))
Corr_SST_S_LPE
Corr_SST_S_FUL <- spat.st %>%
  filter(Site.code == "FUL") %>%
  summarize(cor = cor(mean.temp_s,mean.ppt, use = "pairwise.complete.obs"))
Corr_SST_S_FUL

wilcox.test(spat.st$mean.temp_s,spat.st$mean.ppt, paired=TRUE) 

#-----Correlations between Mangrove abundance and salinity: 
Corr_SST_M_CPI <- spat.st %>%
  filter(Site.code == "FUL") %>%
  summarize(cor = cor(abundance_mangrove,mean.ppt, use = "pairwise.complete.obs"))
Corr_SST_M_CPI

###GLM models------------------------------------------------------------------
#check VIF
#install.packages("car")
library(car)
#install.packages("lme4")
library(lme4)
#install.packages("MASS")
library(MASS)


spat.st$Site <- factor(spat.st$Site)
spat.st$Treatment <- factor(spat.st$Treatment)
spat.st$month <- factor(spat.st$month)
spat.st$mean.ppt <- as.numeric(spat.st$mean.ppt)
spat.st$mean.temp_s <- as.numeric(spat.st$mean.temp_s)
spat.st$abundance_mangrove <- as.numeric(spat.st$abundance_mangrove)
spat.st$abundance_atlantic <- as.numeric(spat.st$abundance_atlantic)

hist(spat.st$abundance_mangrove)
hist(spat.st$abundance_atlantic)

summary(spat.st)


####POISSON distribution
#only fixed factors
mangrove_sp <- glm(abundance_mangrove ~ Site + month + Treatment, family=poisson, data=spat.st)
summary (mangrove_sp)
car::Anova(mangrove_sp, type=2)
vif(mangrove_sp)

atlantic_sp <- glm(abundance_atlantic ~ Site + month + Treatment, family=poisson, data=spat.st)
summary (atlantic_sp)
car::Anova(atlantic_sp, type=2)
vif(atlantic_sp)

#temp and salinity (temp is 1 temp per site and collection period)
mangrove_ts <- glm(abundance_mangrove ~ Site + month + Treatment + mean.temp_s + mean.ppt, family=poisson, data=spat.st)
summary (mangrove_ts)
car::Anova(mangrove_ts, type=2)
vif(mangrove_ts)

atlantic_ts <- glm(abundance_atlantic ~ Site + month + Treatment + mean.temp_s + mean.ppt, family=poisson, data=spat.st)
summary (atlantic_ts)
car::Anova(atlantic_ts, type=2)
vif(atlantic_ts)



m_pois <- glm(
  abundance_mangrove ~ Site + month + Treatment + mean.temp_s + mean.ppt,
  family = poisson,
  data = spat.st
)
install.packages("sanwich")
library(sandwich)
install.packages("lmtest")
library(lmtest)
library(car)

coeftest(m_pois, vcov = vcovHC(m_pois, type = "HC0"))

# Compute robust covariance matrix
robust_vcov <- vcovHC(m_pois, type = "HC0")

# Type II Wald test using robust SEs
car::Anova(m_pois, type=2, vcov.=robust_vcov)


a_pois <- glm(
  abundance_atlantic ~ Site + month + Treatment + mean.temp_s + mean.ppt,
  family = poisson,
  data = spat.st
)

coeftest(a_pois, vcov = vcovHC(m_pois, type = "HC0"))

# Compute robust covariance matrix
robust_vcov <- vcovHC(a_pois, type = "HC0")

# Type II Wald test using robust SEs
car::Anova(a_pois, type=2, vcov.=robust_vcov)


#only temp. remove salinity. (temp is 1 temp per site and collection period) 
#brings vif value down for collection period
mangrove_t <- glm(abundance_mangrove ~ Site + month + Treatment + mean.temp_s, family=poisson, data=spat.st)
summary (mangrove_t)
car::Anova(mangrove_t, type=2)
vif(mangrove_t)

atlantic_t <- glm(abundance_atlantic ~ Site + month + Treatment + mean.temp_s, family=poisson, data=spat.st)
summary (atlantic_t)
car::Anova(atlantic_t, type=2)
vif(atlantic_t)


#only temp. remove salinity. (temp is 1 temp per site, collection period, AND DEPTH) 
mangrove_t2 <- glm(abundance_mangrove ~ Site + month + Treatment + mean.temp, family=poisson, data=spat.st2)
summary (mangrove_t2)
car::Anova(mangrove_t2, type=2)
vif(mangrove_t2)

atlantic_t2 <- glm(abundance_atlantic ~ Site + month + Treatment + mean.temp, family=poisson, data=spat.st2)
summary (atlantic_t2)
car::Anova(atlantic_t2, type=2)
vif(atlantic_t2)



####GAUSSIAN distribution
#only fixed factors
mangrove_sp <- glm(abundance_mangrove ~ Site + month + Treatment, family=gaussian, data=spat.st)
summary (mangrove_sp)
car::Anova(mangrove_sp, type=2)
vif(mangrove_sp)

atlantic_sp <- glm(abundance_atlantic ~ Site + month + Treatment, family=gaussian, data=spat.st)
summary (atlantic_sp)
car::Anova(atlantic_sp, type=2)
vif(atlantic_sp)

#temp and salinity (temp is 1 temp per site and collection period)
mangrove_ts <- glm(abundance_mangrove ~ Site + month + Treatment + mean.temp_s + mean.ppt, family=gaussian, data=spat.st)
summary (mangrove_ts)
car::Anova(mangrove_ts, type=2)
vif(mangrove_ts)

atlantic_ts <- glm(abundance_atlantic ~ Site + month + Treatment + mean.temp_s + mean.ppt, family=gaussian, data=spat.st)
summary (atlantic_ts)
car::Anova(atlantic_ts, type=2)
vif(atlantic_ts)

#only temp. remove salinity. (temp is 1 temp per site and collection period) 
#brings vif value down for collection period
mangrove_t <- glm(abundance_mangrove ~ Site + month + Treatment + mean.temp_s, family=gaussian, data=spat.st)
summary (mangrove_t)
car::Anova(mangrove_t, type=2)
vif(mangrove_t)

atlantic_t <- glm(abundance_atlantic ~ Site + month + Treatment + mean.temp_s, family=gaussian, data=spat.st)
summary (atlantic_t)
car::Anova(atlantic_t, type=2)
vif(atlantic_t)


#only temp. remove salinity. (temp is 1 temp per site, collection period, AND DEPTH) 
mangrove_t2 <- glm(abundance_mangrove ~ Site + month + Treatment + mean.temp, family=gaussian, data=spat.st2)
summary (mangrove_t2)
car::Anova(mangrove_t2, type=2)
vif(mangrove_t2)

atlantic_t2 <- glm(abundance_atlantic ~ Site + month + Treatment + mean.temp, family=gaussian, data=spat.st2)
summary (atlantic_t2)
car::Anova(atlantic_t2, type=2)
vif(atlantic_t2)




####NEGATIVE BINOMIAL distribution
#only fixed factors, no temp or sal
mangrove_sp <- glm.nb(abundance_mangrove ~ Site + month + Treatment, data=spat.st)
summary (mangrove_sp)
car::Anova(mangrove_sp, type=2)
vif(mangrove_sp)

#include control = glm.control(maxit = 100) to avoid a warning message. R gave up too early.
atlantic_sp <- glm.nb(abundance_atlantic ~ Site + month + Treatment, data=spat.st, control = glm.control(maxit = 100))
summary (atlantic_sp)
car::Anova(atlantic_sp, type=2)
vif(atlantic_sp)

#temp and sal
mangrove_ts <- glm.nb(abundance_mangrove ~ Site + month + Treatment + mean.ppt + mean.temp_s, data=spat.st, control = glm.control(maxit = 100))
summary (mangrove_ts)
car::Anova(mangrove_ts, type=2)
vif(mangrove_ts)
#WARNING

atlantic_ts <- glm.nb(abundance_atlantic ~ Site + month + Treatment + mean.ppt + mean.temp_s, data=spat.st, control = glm.control(maxit = 100))
summary (atlantic_ts)
car::Anova(atlantic_ts, type=2)
vif(atlantic_ts)


#TEMP
mangrove_t <- glm.nb(abundance_mangrove ~ Site + month + Treatment + mean.temp_s, data=spat.st, control = glm.control(maxit = 100))
summary (mangrove_t)
car::Anova(mangrove_t, type=2)
vif(mangrove_t)

atlantic_t <- glm.nb(abundance_atlantic ~ Site + month + Treatment + mean.temp_s, data=spat.st, control = glm.control(maxit = 100))
summary (atlantic_t)
car::Anova(atlantic_t, type=2)
vif(atlantic_t)

#TEMP using the temp per depth
mangrove_t2 <- glm.nb(abundance_mangrove ~ Site + month + Treatment + mean.temp, data=spat.st2, control = glm.control(maxit = 100))
summary (mangrove_t2)
car::Anova(mangrove_t2, type=2)
vif(mangrove_t2)

atlantic_t2 <- glm.nb(abundance_atlantic ~ Site + month + Treatment + mean.temp, data=spat.st2, control = glm.control(maxit = 100))
summary (atlantic_t2)
car::Anova(atlantic_t2, type=2)
vif(atlantic_t2)

#Sal and TEMP, using the temp per depth
mangrove_st2 <- glm.nb(abundance_mangrove ~ Site + month + Treatment + mean.temp + mean.ppt, data=spat.st2, control = glm.control(maxit = 100))
summary (mangrove_st2)
car::Anova(mangrove_st2, type=2)
vif(mangrove_st2)

atlantic_st2 <- glm.nb(abundance_atlantic ~ Site + month + Treatment + mean.temp + mean.ppt, data=spat.st2, control = glm.control(maxit = 100))
summary (atlantic_st2)
car::Anova(atlantic_st2, type=2)
vif(atlantic_st2)



#temp and salinity trying zero inflated negative binomial
# multicollinearity is very high
install.packages("pscl")   # run once
library(pscl)

m_zinb <- zeroinfl(
  abundance_mangrove ~ Site + month + Treatment + mean.temp_s + mean.ppt |
    Site + month,
  dist = "negbin",
  data = spat.st
)
summary (m_zinb)
car::Anova(m_zinb, type=2)
vif(m_zinb)


m_pos <- glm.nb(
  abundance_mangrove ~ Site + month + Treatment + mean.temp_s + mean.ppt,
  data = subset(spat.st, abundance_mangrove > 0), control = glm.control(maxit = 100)
)
summary (m_pos)
car::Anova(m_zinb, type=2)
vif(m_zinb)


####negative binomial vs. poisson

#check for overdispersion poisson
sum(residuals(atlantic_sp, type="pearson")^2) / df.residual(atlantic_sp)
# = 28.42 ; If this is >> 1, Poisson is struggling ??? NB is reasonable.
sum(residuals(mangrove_sp, type="pearson")^2) / df.residual(mangrove_sp)
# = 380.1422 ; If this is >> 1, Poisson is struggling ??? NB is reasonable.

#check for overdispersion NB
sum(residuals(atlantic_sp, type="pearson")^2) / df.residual(atlantic_sp)
# = 1.267 ; If this is >> 1, Poisson is struggling ??? NB is reasonable.
sum(residuals(mangrove_sp, type="pearson")^2) / df.residual(mangrove_sp)
# = 0.946 ; If this is >> 1, Poisson is struggling ??? NB is reasonable.

#check for 0s
table(spat.st$Site, spat.st$Treatment)
table(spat.st$month)
tapply(spat.st$abundance_mangrove, spat.st$Site, summary)



### FIGURES ####

#----Plot the odd ratios of the FINAL MODEL:------------------- 
#install.packages("sjPlot")


atlantic <- readRDS(final_atlantic_model.rds)
mangrove <- readRDS(final_mangrove_model.rds)


load(final_atlantic_model.rds)
library("sjPlot")
sjPlot::set_theme(
  base = theme_bw(),       # Use a standard ggplot2 theme as a base
  title.color = "black", # Color of the plot title
  axis.textcolor = "black", # Color of axis text
  axis.title.size = 1.2,   # Size of axis titles
  geom.label.size = 3      # Size of labels inside the plot
)
#P.imbricata odd ratios: 
or_A <-plot_model(atlantic_spst, type = "std", show.values = TRUE, value.offset = .3, 
                  axis.labels = c(
                    "mean.ppt" = "Mean Salinity" ,
                    "Treatment.shallow" = "Shallow Depth",
                    "SiteLuis Peña"="Luis Peña (PCR)",
                    "SitePunta Soldado"="Punta Soldado (PSO)",
                    "SiteFulladosa"="Fulladosa (FUL)",
                    "SiteLa Pelá"="La Pelá (LPE)",
                    "mean.temp_s"="Mean Temperature",
                    "monthMay-June 2024"="May-June 2024",
                    "monthNov-Dec 2024"="Nov-Dec 2024",
                    "monthJuly-Aug 2024"="Jul-Aug 2024",
                    "monthSept-Oct 2024"="Sep-Oct 2024",
                    "monthMar-May 2025"="Mar-May 2025" ),
                  sort.est = TRUE, dot.size = 3, line.size = 1.5)+
  labs(title = "", x = "Predictors", y = "Odds Ratio")
or_A
or_M <-plot_model(mangrove_spst, type = "std", show.values = TRUE, auto.label = FALSE, 
                  axis.labels = c(
                    "mean.ppt" = "Mean Salinity" ,
                    "Treatment.shallow" = "Shallow Depth",
                    "SiteLuis Peña"="Luis Peña (PCR)",
                    "SitePunta Soldado"="Punta Soldado (PSO)",
                    "SiteFulladosa"="Fulladosa (FUL)",
                    "SiteLa Pelá"="La Pelá (LPE)",
                    "mean.temp_s"="Mean Temperature",
                    "monthMay-June 2024"="May-June 2024",
                    "monthNov-Dec 2024"="Nov-Dec 2024",
                    "monthJuly-Aug 2024"="Jul-Aug 2024",
                    "monthSept-Oct 2024"="Sep-Oct 2024",
                    "monthMar-May 2025"="Mar-May 2025" ),
                  value.offset = .3,
                  sort.est = TRUE,dot.size = 3, 
                  line.size = 1.5)+
  labs(title = "", x = "Predictors", y = "Odds Ratio")
or_M

# The predictors labels can be changed: but the labels must exactly match the order of terms as they appear on the plot's axis (from bottom to top)

#Combine: 
library(cowplot)
library(ggpubr)
odd <-ggarrange(or_A, or_M, 
                labels = c("a) P. imbricata", "b) C. rhizophorae"),
                ncol = 1, nrow = 2,  
                #label.x = 1,
                #label.y = 1,
                hjust = -0.25,
                vjust = 0.9,
                font.label = list(size = 11, color = "black", face = "bold", family = NULL),#grid
                align = "v" )
#widths = 1,
#heights = 4)
odd


#boxplot mangrove abundance v site -----
M_abun_v_site  <- ggplot(spat.st, aes(x= Site.code , y= abundance_mangrove , fill= Site.code)) +
  #geom_violin(width=1)+
  geom_boxplot(width = 1, alpha=0.2) + 
  geom_jitter(width = 0.07, size=1, color="black")  +   # Jitter can show concentration, but can also be deceiving so be clear in the description
  stat_summary(fun=mean, geom="point", shape=18, size=2.5, color="black", fill="black") +
  scale_fill_npg()+ 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text = element_text( size = 11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position="none",
        legend.title = element_blank(),
        strip.text = element_text(size = 11))+
  labs(x="Site" , y="C. rhizophorae Abundance")+
  annotate("text",x = 4.4, y = 6000, label = "p<0.001",size =3.75)
M_abun_v_site

#boxplot mangrove abundance vs depth------
M_abun_v_depth  <- ggplot(spat.st, aes(x= Treatment , y= abundance_mangrove , fill= Treatment , color= Treatment)) +
  #geom_violin(width=1)+
  geom_boxplot(width = .4, color="black", alpha=0.2) + 
  geom_jitter(width = 0.07, size=1, color="black")  + 
  scale_fill_brewer(palette = "Blues")+
  stat_summary(fun=mean, geom="point", shape=18, size=2.5, color="black", fill="black") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text = element_text( size = 11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position="none",
        legend.title = element_blank(),
        strip.text = element_text(size = 11))+
  labs(x="Depth" , y="")+ #not including title for y axis for combined fig
  annotate("text",x = 2.17, y = 6000, label = "p=0.8929",size =3.75)
M_abun_v_depth


#boxplot mangrove abundance v month-----
M_abun_v_month  <- ggplot(transform(spat.st, month=factor(month,levels=c("May-June 2024","July-Aug 2024","Sept-Oct 2024","Nov-Dec 2024","Jan-Feb 2025","Mar-May 2025"))),
aes(x= month , y= abundance_mangrove , color= abundance_mangrove)) +
  #geom_violin(width=1)+
  geom_boxplot(width = .4, color="black", alpha=0.2) + 
  geom_jitter(width = 0.07, size=1, color="black")  +  
  scale_fill_brewer(palette = "Blues")+
  stat_summary(fun=mean, geom="point", shape=18, size=2.5, color="black", fill="black") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text = element_text( size = 11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position="none",
        legend.title = element_blank(),
        strip.text = element_text(size = 11))+
  guides(x =  guide_axis(angle = 30))+
  labs(x="Season" , y="")+ #not including title for y axis for combined fig 
  annotate("text",x = 5.5, y = 6000, label = "p<0.001",size =3.75)
M_abun_v_month


#boxplot atlantic abundance v site ------
A_abun_v_site  <- ggplot(spat.st, aes(x= Site.code , y= abundance_atlantic , fill= Site)) +
  #geom_violin(width=1)+
  geom_boxplot(width = .7, alpha=0.2) +
  geom_jitter(width = 0.07, size=1, color="black")  +  
  stat_summary(fun=mean, geom="point", shape=18, size=2.5, color="black", fill="black") +
  scale_fill_npg()+ 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text = element_text( size = 11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position="none",
        legend.title = element_blank(),
        strip.text = element_text(size = 11))+
  labs(x="" , y="P. imbricata Abundance")+ #not including title for x axis for combined fig
  annotate("text",x = 4.5, y = 250, label = "p<0.001",size =3.75)
A_abun_v_site

#boxplot atlantic abundance vs depth----
A_abun_v_depth  <- ggplot(spat.st, aes(x= Treatment , y= abundance_atlantic, fill= Treatment)) +
  #geom_violin(width=1)+
  geom_boxplot(width = .5, color="black", alpha=0.2) + 
  geom_jitter(width = 0.07, size=1, color="black")  +
  scale_fill_brewer(palette = "Blues")+
  stat_summary(fun=mean, geom="point", shape=18, size=2.5, color="black", fill="black") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text = element_text( size = 11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position="none",
        legend.title = element_blank(),
        strip.text = element_text(size = 11))+
  labs(x="" , y="")+#not including title for y and x axis for combined fig
  annotate("text",x = 2.17, y = 250, label = "p=0.20107",size =3.75)
A_abun_v_depth

#boxplot atlantic abundance v month-------
A_abun_v_month  <- ggplot(transform(spat.st, month=factor(month,levels=c("May-June 2024","July-Aug 2024","Sept-Oct 2024","Nov-Dec 2024","Jan-Feb 2025","Mar-May 2025"))),
aes(x= month , y= abundance_atlantic , color= abundance_atlantic)) +
  #geom_violin(width=1)+
  geom_boxplot(width = .4, color="black", alpha=0.2) + 
  geom_jitter(width = 0.07, size=1, color="black")  +  
  scale_fill_brewer(palette = "Blues")+
  stat_summary(fun=mean, geom="point", shape=18, size=2.5, color="black", fill="black") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text = element_text( size = 11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position="none",
        legend.title = element_blank(),
        strip.text = element_text(size = 11))+
  labs(x="" , y="")+ #not including title for y axis for combined fig
  guides(x =  guide_axis(angle = 30))+
  annotate("text",x = 5.5, y = 250, label = "p<0.001",size =3.75)
A_abun_v_month






#combine mangrove figures#######
library(cowplot)
library(ggpubr)
  #need to adjust site and month names shorter to fit
all_mangrove_sp <-ggarrange(M_abun_v_site, M_abun_v_depth, M_abun_v_month,
                    labels = c("d)", "e)", "f)"),
                    ncol = 3, nrow = 1,  
                    #label.x = 1,
                    #label.y = 1,
                    hjust = -1,
                    vjust = 0.9,
                    font.label = list(size = 11, color = "black", face = "bold", family = NULL),#grid
                    align = "hv" )
                    #widths = 1,
                    #heights = 4)
all_mangrove_sp

#combine all atlantic figures
all_atlantic_sp <-ggarrange(A_abun_v_site, A_abun_v_depth, A_abun_v_month,
                            labels = c("a)", "b)", "c)"),
                            ncol = 3, nrow = 1,  
                            #label.x = 1,
                            #label.y = 1,
                            hjust = -1,
                            vjust = 0.9,
                            font.label = list(size = 11, color = "black", face = "bold", family = NULL),#grid
                            align = "hv" )
#widths = 1,
#heights = 4)
all_atlantic_sp

#boxplot mangrove & atlantic
all_sp <-ggarrange( all_atlantic_sp, all_mangrove_sp,
                            labels = c(),
                            ncol = 1, nrow = 2,  
                            #label.x = 1,
                            #label.y = 1,
                            hjust = -1,
                            vjust = 0.9,
                            font.label = list(size = 11, color = "black", face = "bold", family = NULL),#grid
                            align = "hv" )
#widths = 1,
#heights = 4)
all_sp
ggsave("figures/all_sp.png")

