



## install the packages you needed, then library it

# Libraries
library(here)         # relative paths
library(tidyverse)    # tidy functions
library(knitr)        # knit functions
library(lme4)         # mixed-effects regressions
library(lmerTest)  
library(kableExtra)   # extra markdown functions
library(lsr)          # cohenD()
library(broom.mixed)  # tidy()
library(AICcmodavg)   # predictSE()
library(cowplot)      # plot_grid()
library(ggrepel)      # geom_text_repel
library(gridExtra)    # aesthetics 
library(permutes)     # permutation testing
library(doParallel)   # parallel computing 
library(purrr)        # map functions
library(sjPlot)       # clean tables
library(ggstance)     # vertical position dodge

## Data

#The general purpose of these experiments was to determine the relative contributions emotions and rewards have on decisions to punish

#```{r read_data}
# Specify relative paths
#rm(list=ls())
dir_analysis <- here() # should be where analysis script is stored
dir_parent <- dir_analysis %>% str_remove("/analyses")
dir_data <- str_c(dir_analysis, "/data")
dir_graphs <- str_c(dir_parent, "/graphs")


# study data


study_data <- read.csv(str_c(dir_data, "/study_behavior.csv"))



## Standardize variables for UG
## scale 

study_behavior <- study_data %>%
  select(sub, trial, unfairness, RP, VP, AP, VR, AR, choice, reward, RPE, VPE, APE, gender) %>%
  mutate(VPE_scale = as.numeric(scale(VPE, center = F)), 
         APE_scale = as.numeric(scale(APE, center = F)), 
         RPE_scale = as.numeric(scale(RPE, center = F)), 
         VP_scale = as.numeric(scale(VP, center = F)), 
         AP_scale = as.numeric(scale(AP, center = F)), 
         RP_scale = as.numeric(scale(RP, center = F)), 
         VR_scale = as.numeric(scale(VR, center = F)), 
         AR_scale = as.numeric(scale(AR, center = F)), 
         unfairness_norm = as.numeric(scale(unfairness)))

write.csv(study_behavior,file='normalized_behavior.csv')


##correlation
cor(study_behavior$unfairness_norm,study_behavior$RPE_scale)
cor.test(study_behavior$unfairness_norm,study_behavior$RPE_scale)


cor(study_behavior$unfairness_norm,study_behavior$choice)
cor.test(study_behavior$unfairness_norm,study_behavior$choice)

cor(study_behavior$unfairness_norm,study_behavior$VPE_scale)
cor.test(study_behavior$unfairness_norm,study_behavior$VPE_scale)

cor(study_behavior$unfairness_norm,study_behavior$APE_scale)
cor.test(study_behavior$unfairness_norm,study_behavior$APE_scale)

# correlation for prediction error
cor(study_behavior$RPE_scale,study_behavior$VPE_scale)
cor.test(study_behavior$RPE_scale,study_behavior$VPE_scale)


############################ for all subjects 

all_model <- glmer(choice ~ VPE_scale + APE_scale + RPE_scale + (1 + VPE_scale + APE_scale + RPE_scale|sub), 
                      data = study_behavior,
                      family = binomial, 
                      control=glmerControl(optimizer='bobyqa', 
                                           optCtrl=list(maxfun=2e5)))

library(car)

vif(all_model)
summary(all_model)

#effect size
tab_model(all_model)

## beta comparison
all_coefs <- fixef(all_model)
all_se <- sqrt(diag(vcov(all_model)))
z_score <- (as.numeric(all_coefs ["RPE_scale"]) - as.numeric(all_coefs["VPE_scale"])) / (sqrt(all_se[2]^2 + all_se[4]^2))
pvalue <- pnorm(-abs(z_score))
z_score
pvalue

# Table and Figure all

table_data <- study_behavior

table <- glmer(choice ~ VPE_scale + APE_scale + RPE_scale + (1 + VPE_scale + APE_scale + RPE_scale|sub), 
                 data = table_data,
                 family = binomial, 
                 control = glmerControl(optimizer = "bobyqa"))

xRange1 <- with(table_data, seq(round(min(VPE_scale, na.rm=T), 1), round(max(VPE_scale, na.rm=T), 1), by = .1))
xRange2 <- with(table_data, seq(round(min(APE_scale, na.rm=T), 1), round(max(APE_scale, na.rm=T), 1), by = .1))
xRange3 <- with(table_data, seq(round(min(RPE_scale, na.rm=T), 1), round(max(RPE_scale, na.rm=T), 1), by = .1))

predict1 <- with(table_data, expand.grid(VPE_scale=xRange1, APE_scale=0, RPE_scale = 0))
predict2 <- with(table_data, expand.grid(VPE_scale=0, APE_scale=xRange2, RPE_scale = 0))
predict3 <- with(table_data, expand.grid(VPE_scale=0, APE_scale=0, RPE_scale = xRange3))

predictedInterval1 <- data.frame(AICcmodavg::predictSE(table, newdata=predict1, type="response", print.matrix=T))
predictedInterval2 <- data.frame(AICcmodavg::predictSE(table, newdata=predict2, type="response", print.matrix=T))
predictedInterval3 <- data.frame(AICcmodavg::predictSE(table, newdata=predict3, type="response", print.matrix=T))

plot_data1 <- bind_cols(predict1, predictedInterval1)
plot_data2 <- bind_cols(predict2, predictedInterval2)
plot_data3 <- bind_cols(predict3, predictedInterval3)

theme_zg=theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(color="black",size=1.5),
               text = element_text(size = 16,color="black"), plot.title = element_blank(),legend.title=element_blank(),legend.position=c(0.76, 0.88),axis.text = element_text(size = 16,color='black'),axis.ticks = element_line(color='black', size=1.5))

fig_plot <- ggplot() + 
  # Valence PE
  geom_line(data = plot_data1, aes(x = VPE_scale, y = fit, color = "Valence PE"), size = 1.5) + 
  geom_ribbon(data = plot_data1, aes(x = VPE_scale, ymax = fit + se.fit, ymin = fit - se.fit, fill = "Valence PE"), alpha = .2) + 
  # Arousal PE
  geom_line(data = plot_data2, aes(x = APE_scale, y = fit, color = "Arousal PE"), size = 1.5) + 
  geom_ribbon(data = plot_data2, aes(x = APE_scale, ymax = fit + se.fit, ymin = fit - se.fit, fill = "Arousal PE"), alpha = .2) + 
  # Reward PE
  geom_line(data = plot_data3, aes(x = RPE_scale, y = fit, color = "Reward PE"), size = 1.5) + 
  geom_ribbon(data = plot_data3, aes(x = RPE_scale, ymax = fit + se.fit, ymin = fit - se.fit, fill = "Reward PE"), alpha = .2) + 
  scale_color_manual(name = "Color", values = c("#982b2b","#0074b3","#e5ce81")) + # arousal, reward, valence order (Paul Tol's colors)
  scale_fill_manual(name = "Color", values = c("#982b2b","#0074b3","#e5ce81")) + 
  scale_y_continuous(labels = scales::percent, name = "P(Reject)") +
  coord_cartesian(xlim = c(-5.0, 5.), ylim = c(0, 1)) + 
  scale_x_continuous(name = "Prediction Errors") +
  annotate("text", label = "***", x = -0.9, y = .85, size = 10, color = "#0074b3", angle = -70) + # reward
  annotate("text", label = "***", x = 1.3, y = .28, size = 10, color = "#982b2b", angle = 50) + # arousal
  annotate("text", label = "***", x = -2.3, y = .25, size = 10, color = "#e5ce81", angle = -60) + # valence
  geom_vline(xintercept = 0, lty = 3, size=1) + 
  theme_bw() + ggtitle('All subjects')+theme_zg
  
fig_plot

ggsave(filename = paste0(dir_graphs, "/fig2_all.tiff"), plot=fig_plot, width = 4.5, height = 5.88, dpi = 400, units = "in", device='tiff')


#################################################################################################################

######for female and male separately 
female_group <- study_behavior %>% filter(gender == "female")

female_model <- glmer(choice ~ VPE_scale + APE_scale + RPE_scale + (1 + VPE_scale + APE_scale + RPE_scale|sub), 
                          data = female_group,
                          family = binomial, 
                          control=glmerControl(optimizer='bobyqa', 
                                               optCtrl=list(maxfun=2e5)))

vif(female_model)
summary(female_model)

#effect size
tab_model(female_model)

## beta comparison
female_coefs <- fixef(female_model)
female_se <- sqrt(diag(vcov(female_model)))
z_score <- (as.numeric(female_coefs ["RPE_scale"]) - as.numeric(female_coefs["APE_scale"])) / (sqrt(female_se[2]^2 + female_se[4]^2))
pvalue <- pnorm(-abs(z_score))
z_score
pvalue


## for male
male_group <- study_behavior %>% filter(gender == "male")

male_model <- glmer(choice ~ VPE_scale + APE_scale + RPE_scale + (1 + VPE_scale + APE_scale + RPE_scale|sub), 
                      data = male_group,
                      family = binomial, 
                      control=glmerControl(optimizer='bobyqa', 
                                           optCtrl=list(maxfun=2e5)))

vif(male_model)
summary(male_model)

#effect size
tab_model(male_model)


# beta comparison
male_coefs <- fixef(male_model)
male_se <- sqrt(diag(vcov(male_model)))
z_score <- (as.numeric(male_coefs ["RPE_scale"]) - as.numeric(male_coefs["APE_scale"])) / (sqrt(male_se[2]^2 + male_se[4]^2))
pvalue <- pnorm(-abs(z_score))
z_score
pvalue


##########female and male
## female
tableS1_data <- study_behavior %>% 
  filter(gender == "female")

tableS1 <- glmer(choice ~ VPE_scale + APE_scale + RPE_scale + (1 + VPE_scale + APE_scale + RPE_scale|sub), 
                  data = tableS1_data,
                  family = binomial, 
                  control = glmerControl(optimizer = "bobyqa"))
summary(tableS1)

xRange1 <- with(tableS1_data, seq(round(min(VPE_scale, na.rm=T), 1), round(max(VPE_scale, na.rm=T), 1), by = .1))
xRange2 <- with(tableS1_data, seq(round(min(APE_scale, na.rm=T), 1), round(max(APE_scale, na.rm=T), 1), by = .1))
xRange3 <- with(tableS1_data, seq(round(min(RPE_scale, na.rm=T), 1), round(max(RPE_scale, na.rm=T), 1), by = .1))

predict1 <- with(tableS1_data, expand.grid(VPE_scale=xRange1, APE_scale=0, RPE_scale = 0))
predict2 <- with(tableS1_data, expand.grid(VPE_scale=0, APE_scale=xRange2, RPE_scale = 0))
predict3 <- with(tableS1_data, expand.grid(VPE_scale=0, APE_scale=0, RPE_scale = xRange3))

predictedInterval1 <- data.frame(AICcmodavg::predictSE(tableS1, newdata=predict1, type="response", print.matrix=T))
predictedInterval2 <- data.frame(AICcmodavg::predictSE(tableS1, newdata=predict2, type="response", print.matrix=T))
predictedInterval3 <- data.frame(AICcmodavg::predictSE(tableS1, newdata=predict3, type="response", print.matrix=T))

plot_data1 <- bind_cols(predict1, predictedInterval1)
plot_data2 <- bind_cols(predict2, predictedInterval2)
plot_data3 <- bind_cols(predict3, predictedInterval3)


theme_zg=theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(color="black",size=1.5),
               text = element_text(size = 16,color="black"), plot.title = element_blank(),legend.title=element_blank(),legend.position="none",axis.text = element_text(size = 16,color='black'),axis.ticks = element_line(color='black', size=1.5))

fig1a_plot1 <- ggplot() + 
  # Valence PE
  geom_line(data = plot_data1, aes(x = VPE_scale, y = fit, color = "Valence PE"), size = 1.5) + 
  geom_ribbon(data = plot_data1, aes(x = VPE_scale, ymax = fit + se.fit, ymin = fit - se.fit, fill = "Valence PE"), alpha = .2) + 
  # Arousal PE
  geom_line(data = plot_data2, aes(x = APE_scale, y = fit, color = "Arousal PE"), size = 1.5) + 
  geom_ribbon(data = plot_data2, aes(x = APE_scale, ymax = fit + se.fit, ymin = fit - se.fit, fill = "Arousal PE"), alpha = .2) + 
  # Reward PE
  geom_line(data = plot_data3, aes(x = RPE_scale, y = fit, color = "Reward PE"), size = 1.5) + 
  geom_ribbon(data = plot_data3, aes(x = RPE_scale, ymax = fit + se.fit, ymin = fit - se.fit, fill = "Reward PE"), alpha = .2) + 
  scale_color_manual(name = "Color", values = c("#982b2b","#0074b3","#e5ce81")) + # arousal, reward, valence order (Paul Tol's colors)
  scale_fill_manual(name = "Color", values = c("#982b2b","#0074b3","#e5ce81")) + 
  scale_y_continuous(labels = scales::percent, name = "P(Reject)") +
  coord_cartesian(xlim = c(-5.0, 5.), ylim = c(0, 1)) + 
  scale_x_continuous(name = "Prediction Errors") +
  annotate("text", label = "***", x = -2.7, y = .58, size = 10, color = "#e5ce81", angle = -62) + # valence
  annotate("text", label = "***", x = 2.9, y = .5, size = 10, color = "#982b2b", angle = 60) + # arousal
  annotate("text", label = "***", x = -0.6,y = .85, size = 10, color = "#0074b3", angle = -70) + # reward
  geom_vline(xintercept = 0, lty = 3) + 
  theme_bw() + ggtitle('Female subjects')+ theme_zg

fig1a_plot1

ggsave(filename = paste0(dir_graphs, "/fig2_female.tiff"), plot=fig1a_plot1, width = 4.5, height = 5.88, dpi = 400, units = "in", device='tiff')



## male
tableS2_data <- study_behavior %>% 
  filter(gender == "male")

tableS2 <- glmer(choice ~ VPE_scale + APE_scale + RPE_scale + (1 + VPE_scale + APE_scale + RPE_scale|sub), 
                 data = tableS2_data,
                 family = binomial, 
                 control = glmerControl(optimizer = "bobyqa"))
summary(tableS2)

xRange1 <- with(tableS2_data, seq(round(min(VPE_scale, na.rm=T), 1), round(max(VPE_scale, na.rm=T), 1), by = .1))
xRange2 <- with(tableS2_data, seq(round(min(APE_scale, na.rm=T), 1), round(max(APE_scale, na.rm=T), 1), by = .1))
xRange3 <- with(tableS2_data, seq(round(min(RPE_scale, na.rm=T), 1), round(max(RPE_scale, na.rm=T), 1), by = .1))

predict1 <- with(tableS2_data, expand.grid(VPE_scale=xRange1, APE_scale=0, RPE_scale = 0))
predict2 <- with(tableS2_data, expand.grid(VPE_scale=0, APE_scale=xRange2, RPE_scale = 0))
predict3 <- with(tableS2_data, expand.grid(VPE_scale=0, APE_scale=0, RPE_scale = xRange3))

predictedInterval1 <- data.frame(AICcmodavg::predictSE(tableS2, newdata=predict1, type="response", print.matrix=T))
predictedInterval2 <- data.frame(AICcmodavg::predictSE(tableS2, newdata=predict2, type="response", print.matrix=T))
predictedInterval3 <- data.frame(AICcmodavg::predictSE(tableS2, newdata=predict3, type="response", print.matrix=T))

plot_data1 <- bind_cols(predict1, predictedInterval1)
plot_data2 <- bind_cols(predict2, predictedInterval2)
plot_data3 <- bind_cols(predict3, predictedInterval3)

theme_zg=theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(color="black",size=1.5),
               text = element_text(size = 16,color="black"), plot.title = element_blank(),legend.title=element_blank(),legend.position="none",axis.text = element_text(size = 16,color='black'),axis.ticks = element_line(color='black', size=1.5))
fig1a_plot2 <- ggplot() + 
  # Valence PE
  geom_line(data = plot_data1, aes(x = VPE_scale, y = fit, color = "Valence PE"), size = 1.5) + 
  geom_ribbon(data = plot_data1, aes(x = VPE_scale, ymax = fit + se.fit, ymin = fit - se.fit, fill = "Valence PE"), alpha = .2) + 
  # Arousal PE
  geom_line(data = plot_data2, aes(x = APE_scale, y = fit, color = "Arousal PE"), size = 1.5) + 
  geom_ribbon(data = plot_data2, aes(x = APE_scale, ymax = fit + se.fit, ymin = fit - se.fit, fill = "Arousal PE"), alpha = .2) + 
  # Reward PE
  geom_line(data = plot_data3, aes(x = RPE_scale, y = fit, color = "Reward PE"), size = 1.5) + 
  geom_ribbon(data = plot_data3, aes(x = RPE_scale, ymax = fit + se.fit, ymin = fit - se.fit, fill = "Reward PE"), alpha = .2) + 
  scale_color_manual(name = "Color", values = c("#982b2b","#0074b3","#e5ce81")) + # arousal, reward, valence order (Paul Tol's colors)
  scale_fill_manual(name = "Color", values = c("#982b2b","#0074b3","#e5ce81")) + 
  scale_y_continuous(labels = scales::percent, name = "P(Reject)") +
  coord_cartesian(xlim = c(-5.0, 5.), ylim = c(0, 1)) + 
  scale_x_continuous(name = "Prediction Errors") +
  annotate("text", label = "***", x = -3.6, y = .35, size = 10, color = "#e5ce81", angle = -60) + # valence
  annotate("text", label = "***", x = 2.6, y = .25, size = 10, color = "#982b2b", angle = 34) + # arousal
  annotate("text", label = "***", x = -0.7, y = .68, size = 10, color = "#0074b3", angle = -75) + # reward
  geom_vline(xintercept = 0, lty = 3) + 
  theme_bw() + ggtitle('Male subjects')+ theme_zg

fig1a_plot2

ggsave(filename = paste0(dir_graphs, "/fig2_male.tiff"), plot=fig1a_plot2, width =4.5, height = 5.88, dpi = 400, units = "in", device='tiff')


##################################
# Figure 1b for gender effects on VPE, APE, RPE, respectively
# original color for gender, "#f47720", "#459943"

fig1b_data <- study_behavior %>% 
  select(sub, choice, gender, VPE_scale, APE_scale, RPE_scale) %>% 
  mutate(gender_binary = ifelse(gender == "female", 0, 1)) # female - 1, male - 0

fig1b_model <- glmer(choice ~ VPE_scale*gender_binary + APE_scale*gender_binary + RPE_scale*gender_binary + (1 + VPE_scale + APE_scale + RPE_scale|sub), 
                     data = fig1b_data,
                     family = binomial, 
                     control = glmerControl(optimizer = "bobyqa"))

summary(fig1b_model)

# Graph
xRange1 <- with(fig1b_data, seq(round(min(VPE_scale, na.rm=T), 1), round(max(VPE_scale, na.rm=T), 1), by = .1))
xRange2 <- with(fig1b_data, seq(round(min(APE_scale, na.rm=T), 1), round(max(APE_scale, na.rm=T), 1), by = .1))
xRange3 <- with(fig1b_data, seq(round(min(RPE_scale, na.rm=T), 1), round(max(RPE_scale, na.rm=T), 1), by = .1))

predict1 <- with(fig1b_data, expand.grid(VPE_scale=xRange1, APE_scale=0, RPE_scale = 0, gender_binary = c(0, 1)))
predict2 <- with(fig1b_data, expand.grid(VPE_scale=0, APE_scale=xRange2, RPE_scale = 0, gender_binary = c(0, 1)))
predict3 <- with(fig1b_data, expand.grid(VPE_scale=0, APE_scale=0, RPE_scale = xRange3, gender_binary = c(0, 1)))

predictedInterval1 <- data.frame(predictSE(fig1b_model, newdata=predict1, type="response", print.matrix=T))
predictedInterval2 <- data.frame(predictSE(fig1b_model, newdata=predict2, type="response", print.matrix=T))
predictedInterval3 <- data.frame(predictSE(fig1b_model, newdata=predict3, type="response", print.matrix=T))

plot_data1 <- bind_cols(predict1, predictedInterval1) %>% 
  mutate(gender_binary = case_when(gender_binary == 0 ~ "Female", 
                                       gender_binary == 1 ~ "Male"))
plot_data2 <- bind_cols(predict2, predictedInterval2) %>% 
  mutate(gender_binary = case_when(gender_binary == 0 ~ "Female", 
                                       gender_binary == 1 ~ "Male"))
plot_data3 <- bind_cols(predict3, predictedInterval3) %>% 
  mutate(gender_binary = case_when(gender_binary == 0 ~ "Female", 
                                       gender_binary == 1 ~ "Male"))


theme_zg=theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(color="black",size=1.5),
                        text = element_text(size = 16,color="black"), plot.title = element_text(hjust = 0.5),legend.title=element_blank(),legend.position="none",axis.text = element_text(size = 16,color='black'),axis.ticks = element_line(color='black', size=1.5))

# Figure 1b
fig1b_plot1 <- ggplot() + 
  # Valence PE
  geom_line(data = plot_data1, aes(x = VPE_scale, y = fit, color = gender_binary), size = 1.5) + 
  geom_ribbon(data = plot_data1, aes(x = VPE_scale, ymax = fit + se.fit, ymin = fit - se.fit, fill = gender_binary), alpha = .2) + 
  scale_y_continuous(labels = scales::percent, name = "p(Reject)") +
  xlab("Valence PE") + 
  scale_color_manual(name = "Gender", values = c("#f47720", "#459943")) + # Paul Tol
  scale_fill_manual(name = "Gender", values = c("#f47720", "#459943")) + # Paul Tol
  coord_cartesian(ylim = c(0, 1)) + 
  geom_vline(xintercept = 0, lty = 3,size=1) + 
  #annotate("segment", x = -3, xend = 3, y = 1, yend = 1) + 
  #annotate("text", label = "**", x = 0, y = 1.02) + 
  theme_bw() + theme_zg

fig1b_plot1 

fig1b_plot2 <- ggplot() + 
  # Arousal PE
  geom_line(data = plot_data2, aes(x = APE_scale, y = fit, color = gender_binary), size = 1.5) + 
  geom_ribbon(data = plot_data2, aes(x = APE_scale, ymax = fit + se.fit, ymin = fit - se.fit, fill = gender_binary), alpha = .2) + 
  scale_y_continuous(labels = scales::percent, name = "p(Reject)") +
  coord_cartesian(ylim = c(0, 1)) + 
  xlab("Arousal PE") + 
  scale_color_manual(name = "Gender", values = c("#f47720", "#459943")) + # Paul Tol
  scale_fill_manual(name = "Gender", values = c("#f47720", "#459943")) + # Paul Tol
  geom_vline(xintercept = 0, lty = 3,size=1) + 
  #annotate("segment", x = -3, xend = 3, y = 1, yend = 1) + 
  #annotate("text", label = "*", x = 0, y = 1.02) + 
  theme_bw() + theme_zg

fig1b_plot2 

fig1b_plot3 <- ggplot() + 
  geom_line(data = plot_data3, aes(x = RPE_scale, y = fit, color = gender_binary), size = 1.5) + 
  geom_ribbon(data = plot_data3, aes(x = RPE_scale, ymax = fit + se.fit, ymin = fit - se.fit, fill = gender_binary), alpha = .2) + 
  scale_y_continuous(labels = scales::percent, name = "p(Reject)") +
  coord_cartesian(ylim = c(0, 1)) + 
  scale_color_manual(name = "Gender", values = c("#f47720", "#459943")) + # Paul Tol
  scale_fill_manual(name = "Gender", values = c("#f47720", "#459943")) + # Paul Tol
  xlab("Reward PE") + 
  geom_vline(xintercept = 0, lty = 3, size=1) + 
  #annotate("segment", x = -3, xend = 3, y = 1, yend = 1) + 
  #annotate("text", label = "n.s.", x = 0, y = 1.02) + 
  theme_bw() + theme_zg+theme(legend.position = c(0.8,0.91))

fig1b_plot3


# Save separately
ggsave(filename = paste0(dir_graphs, "/fig2b_VPE.tiff"), plot=fig1b_plot1, width =4.5, height = 5.88, dpi = 400, units = "in", device='tiff')
ggsave(filename = paste0(dir_graphs, "/fig2b_APE.tiff"), plot=fig1b_plot2, width =4.5, height = 5.88, dpi = 400, units = "in", device='tiff')
ggsave(filename = paste0(dir_graphs, "/fig2b_RPE.tiff"), plot=fig1b_plot3, width =4.5, height = 5.88, dpi = 400, units = "in", device='tiff')