library(tidyverse)
library(scales)
library(gridExtra)
library(broom)

df <- read_csv("project_data.csv")

#Table of groups
df %>% 
  group_by(discount, color) %>% 
  summarise(amt = dollar(mean(`Amount spent`))) %>% 
  spread(color, amt) %>% 
  rename(" " = discount) %>%
  grid.table(rows = NULL)
  
#ANOVA test for any difference in means
df %>% 
  rename(amt_spent = `Amount spent`) %>% 
  unite(category, color, discount) %>% 
  lm(amt_spent ~ factor(category), .) %>% 
  anova %>% 
  tidy

#TukeyHSD test to measure amount of difference and in which groups
df %>% 
  rename(amt_spent = `Amount spent`) %>% 
  unite(category, color, discount) %>% 
  aov(amt_spent ~ factor(category), .) %>% 
  TukeyHSD(conf.level = .95) %>% 
  tidy %>% 
  separate(comparison, c("compare", "to"), "-") %>% 
  arrange(adj.p.value) %>% 
  mutate(adj.p.value = round(adj.p.value, 4))

#Diagnostic plots for one-way anova
par(mfrow = c(2,2)) #Set viewport to show 4 plots

df %>% 
  rename(amt_spent = `Amount spent`) %>% 
  unite(category, color, discount) %>% 
  aov(amt_spent ~ factor(category), .) %>% 
  plot

par(mfrow = c(1,1)) #Return viewport to one plot


#Visualize differences in mean amount spent per group

df %>% 
  rename(amt_spent = `Amount spent`) %>% 
  unite(category, color, discount) %>% 
  aov(amt_spent ~ factor(category), .) %>% 
  TukeyHSD(conf.level = .95, ordered = T) %>% 
  tidy %>% 
  mutate(adj.p.value = round(adj.p.value, 4),
         isSignificant = factor(ifelse(adj.p.value < .05, 1, 0))) %>% 
  ggplot(aes(x = reorder(comparison, -estimate), 
             ymin = conf.low, 
             y = estimate, 
             ymax = conf.high, 
             alpha = isSignificant)) +
    geom_bar(stat = "identity") +
    geom_errorbar(color = "red", width = .25) +
    labs(title = "Difference in mean amount spent between each group",
         y = "Difference in average amount spent") +
    scale_y_continuous(labels = dollar) +
    scale_alpha_discrete(range = (c(.4,1))) +
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 90))
    


#########  T-tests invalid for multiple comparisons   ###############
# 
# #Split into group of interest vs all other groups for t.test
# ro <- df %>% filter(discount == "Off" & color == "Red")
# bo <- df %>% filter(discount == "Off" & color == "Black")
# go <- df %>% filter(discount == "Off" & color == "Green")
# rm <- df %>% filter(discount == "Minus" & color == "Red")
# bm <- df %>% filter(discount == "Minus" & color == "Black")
# gm <- df %>% filter(discount == "Minus" & color == "Green")
# all_others <- df %>% filter(discount != "Off" & color != "Red")
# 
# #Test for normality of distributions
# hist(ro$`Amount spent`)
# hist(all_others$`Amount spent`)
# qqnorm(ro$`Amount spent`) 
# qqline(ro$`Amount spent`, col = 2) #Roughly normal
# qqnorm(all_others$`Amount spent`)
# qqline(all_others$`Amount spent`, col = 2) #Roughly normal
# 
# #Test for equal variances
# var(ro$`Amount spent`)
# var(all_others$`Amount spent`) #Variances are not equal, use Welch's t-test
# 
# #Welch's t-test used
# t.test(x = ro$`Amount spent`, 
#        y = all_others$`Amount spent`,
#        alternative = "greater",
#        var.equal = F) #Red/Off is significantly more than the average of all the other sub-groups
# 
# 
# # Split 2nd place subgroup tor t-test
# t.test(x = ro$`Amount spent`, 
#        y = bm$`Amount spent`,
#        alternative = "greater",
#        var.equal = F) #Red/Off is significantly more than second place subgroup
# 

#Distributions of performance screen designs, bimodal on best performing design
ggplot() +
  geom_density(data = ro, aes(`Amount spent`), fill = "red", alpha = .5) +
  geom_vline(xintercept = median(ro$`Amount spent`), color = "red") +
  geom_density(data = rm, aes(`Amount spent`), fill = "red", alpha = .1) +
  geom_vline(xintercept = median(rm$`Amount spent`), color = "red", linetype = "dotted") +
  geom_density(data = bo, aes(`Amount spent`), fill = "black", alpha = .1) +
  geom_vline(xintercept = median(bo$`Amount spent`), linetype = "dotted") +
  geom_density(data = bm, aes(`Amount spent`), fill = "black", alpha = .5) +
  geom_vline(xintercept = median(bm$`Amount spent`)) +
  geom_density(data = go, aes(`Amount spent`), fill = "green", alpha = .1) +
  geom_vline(xintercept = median(go$`Amount spent`), color = "green", linetype = "dotted") +
  geom_density(data = gm, aes(`Amount spent`), fill = "green", alpha = .5) +
  geom_vline(xintercept = median(gm$`Amount spent`), color = "green") +
  theme(panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()
        ) +
  scale_x_continuous(labels = dollar) +
  labs(title = "Distribution of best-performing design is bi-modal")

#Best performing screen design
df %>% 
  rename(amt_spent = `Amount spent`) %>% 
  unite(category, color, discount) %>% 
  mutate(category = factor(category),
         isRedOff = factor(ifelse(category == "Red_Off", 1, 0))) %>% 
  ggplot(aes(reorder(category, -amt_spent), amt_spent, alpha = isRedOff)) +
  stat_summary(fun.y = mean, geom = "bar") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .3, color = "red") +
  labs(title = 'A screen with red letters that says "10% off" performs best',
       x = "Condition",
       y = "Average amount spent") +
  scale_x_discrete(labels = c("Color: Red\nSign: 10% Off",
                              "Color: Black\nSign: -%10",
                              "Color: Red\nSign: -%10",
                              "Color: Black\nSign: 10% Off",
                              "Color: Green\nSign: -10%",
                              "Color: Green\nSign: 10% Off")
  ) +
  scale_y_continuous(labels = dollar) +
  scale_alpha_manual(values = c(.4,1)) +
  theme(panel.background = element_blank(),
        legend.position = "none")


#Make table of predicted values for each category with no interactions

data_frame(discount = c(rep(c("Minus"), 3), rep(c("Off"), 3)),
           color = rep(c("Black", "Green", "Red"), 2)
           ) %>% 
  bind_cols(predictions = predict(lm(`Amount spent` ~ ., df), .)) %>% 
  mutate(predictions = dollar(predictions)) %>% 
  spread(color, predictions) %>%
  rename(" " = discount) %>% 
  grid.table(rows = NULL)
  
#Make table of predicted values for each category WITH interactions (same as conditional means table)

data_frame(discount = c(rep(c("Minus"), 3), rep(c("Off"), 3)),
           color = rep(c("Black", "Green", "Red"), 2)
) %>% 
  bind_cols(predictions = predict(lm(`Amount spent` ~ . + color * discount, df), .)) %>% 
  mutate(predictions = dollar(predictions)) %>% 
  spread(color, predictions) %>% 
  rename(" " = discount) %>% 
  grid.table(rows = NULL)


#Show summaries of linear model with and without interaction
lm(`Amount spent` ~ ., df) %>% summary
lm(`Amount spent` ~ . + color * discount, df) %>% summary





