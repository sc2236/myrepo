# Task 1: Two-Armed Bandits: 
library("tidyverse")
library("afex")
library("emmeans")
theme_set(theme_bw(base_size = 15) + theme(legend.position="bottom")) 

bandit <- read_csv("/Users/chenshin/Desktop/2-armed-bandits.csv") 
glimpse (bandit) 

# Transform categorical variables into factors 
bandit <- bandit %>%  
  mutate(id = factor(id), 
         gender= factor(gender, levels= c(1,2), labels= c("female", "male")),
         condition= factor(condition, levels=c("baseline","play-only","study-A-then-play","study-B-then-play")),
         option= factor(option))

# To understand the data 
bandit %>%
  group_by(id, condition) %>%
  summarise(n = n()) %>% 
  group_by(condition) %>%
  summarise(n = n()) 

bandit %>% 
  group_by(id) %>% 
  summarize(n=n(), 
            n_items=length(unique(condition))) %>% 
  summarize(n_participants= length(unique(id)), 
            m_trials= mean(n), 
            sd_trials= sd(n), 
            m_item= mean(n_items), 
            sd_item=sd(n_items)) 

# 203 participants, 2 trials each, roughly 50 participants in each condition. 

glimpse(bandit) 

# RQ 1: 
# Calculate the bias by subtracting B from A 
bandit_bias <- bandit %>%  pivot_wider(names_from = option, values_from = estimate) %>% mutate (bias=A-B) 

# Run the ANOVA for dv bias 
biasanova <- aov_ez("id","bias", bandit_bias, between= "condition") 
biasanova     

# Anova Table (Type 3 tests) 
# Response: bias
# Effect     df    MSE    F  ges p.value
# 1 condition 3, 199 607.13 0.66 .010    .579

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1 

# Followup tests using emmeans and pairwise contrasts: 
em1 <- emmeans(biasanova, "condition") 
summary(em1) 
# condition         emmean   SE  df lower.CL upper.CL
# baseline            8.80 3.52 199    1.855     15.7
# play-only           9.28 3.48 199    2.408     16.2
# study-A-then-play   5.93 3.35 199   -0.686     12.5
# study-B-then-play   3.18 3.48 199   -3.692     10.1

# Confidence level used: 0.95 

con <- list(
  baseline_playonly = c(-1, 1, 0, 0),
  baseline_studyA = c(-1, 0, 1, 0), 
  baseline_studyB = c(-1, 0, 0, 1) 
) 

contrast(em1, con, adjust = "holm") 
#contrast          estimate   SE  df t.ratio p.value 
#baseline_playonly    0.484 4.95 199   0.098  1.0000 
#baseline_studyA     -2.870 4.86 199  -0.590  1.0000 
#baseline_studyB     -5.616 4.95 199  -1.134  0.7747 
#P value adjustment: holm method for 3 tests 

contrast(em1, con, adjust = "none") 
# contrast          estimate   SE  df t.ratio p.value 
# baseline_playonly    0.484 4.95 199   0.098  0.9222 
# baseline_studyA     -2.870 4.86 199  -0.590  0.5556 
# baseline_studyB     -5.616 4.95 199  -1.134  0.2582 

# Plotting 
afex_plot(biasanova, "condition",data_geom = ggbeeswarm::geom_quasirandom)+
  coord_cartesian(ylim = c(-100, 100)) +
  geom_line(aes(group = 1)) +
  labs(x= "Condition",y = "Bias", title="Bias across Conditions") + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# RQ2: 
accuracy <- bandit %>%
  pivot_wider(names_from = option, values_from = estimate) %>%
  mutate(accuracy_A = ((A - 75))/100, accuracy_B = ((B - 75))/100) 
glimpse(accuracy) 

# Putting A and B together: 
accuracy1 <- accuracy %>%
  select(condition, accuracy_A)
colnames(accuracy1)[colnames(accuracy1) == "accuracy_A"] <- "accuracy" 

accuracy2 <- accuracy %>%
  select(condition, accuracy_B)
colnames(accuracy2)[colnames(accuracy2) == "accuracy_B"] <- "accuracy" 

accuracybig <- rbind(accuracy1, accuracy2) %>%
  mutate(id = row_number())

# ANOVA: 
accuracyab <- aov_ez("id", "accuracy", accuracybig, between = "condition") 
accuracyab 

# emmeans 
emmeans (accuracyab, "condition") 
emmeans(accuracyab, "condition") %>% pairs() %>% update(by = NULL, adjust = "holm") 

# Now A and B separately: 
accuracyA <- aov_ez("id", "accuracy_A", accuracy, between = "condition") 
summary(accuracyA) 
# EDITING ON GITHUB HAHAHHA 

# Anova Table (Type 3 tests) 
# Response: accuracy_A
# num Df den Df      MSE      F      ges  Pr(>F)  
# condition      3    199 0.023513 3.6141 0.051668 0.01421 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

emmeans(accuracyA, "condition") %>% pairs() %>% update(by = NULL, adjust = "holm") 

accuracyB <- aov_ez("id", "accuracy_B", accuracy, between = "condition") 
# summary(accuracyB) 
# Anova Table (Type 3 tests)

#Response: accuracy_B
#num Df den Df      MSE      F      ges Pr(>F)
#condition      3    199 0.034518 1.1525 0.017077 0.3291 

emmeans(accuracyB, "condition") %>% pairs() %>% update(by = NULL, adjust = "holm") 

# Plotting: 
library(ggplot2)
library(cowplot) 

p1 <- afex_plot(accuracyab, "condition",data_geom = ggbeeswarm::geom_quasirandom) +geom_point(alpha = 0.5, color = "gray")+ 
  ylim(-0.75, 0.75)+  geom_line(aes(group = 1)) +  labs(x= "Condition",y = "Accuracy", title="Estimation Accuracy (aggregate) across Conditions") + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
p2 <- afex_plot(accuracyA, "condition",data_geom = ggbeeswarm::geom_quasirandom)+ 
  ylim(-0.75, 0.75)+geom_line(aes(group = 1)) +  labs(x= "Condition",y = "Accuracy", title="Estimation Accuracy (A) across Conditions") + theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

p3 <- afex_plot(accuracyB, "condition",data_geom = ggbeeswarm::geom_quasirandom) + 
  ylim(-0.75, 0.75)+geom_line(aes(group = 1)) +labs(x= "Condition",y = "Accuracy", title="Estimation Accuracy (B) across Conditions")+ theme(plot.title = element_text(hjust = 0.5, face = "bold"))

plot_grid(p1,p2,p3, ncol=3)
