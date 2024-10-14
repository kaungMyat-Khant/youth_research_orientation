rm(list = ls())
set.seed(123)
data.male <- data.frame(gender = rep("male", n=100), age = sample(18:40,100,replace = T), match_age = sample(18:22, 100, replace = T) )
data.female <- data.frame(gender = rep("female",n=100), age = sample(18:40, 100, replace = T))
data.female$match_age <- data.female$age + sample(c(-3,-2,-1,0,1,2,3),100,replace = T)
data.female$match_age <- replace(data.female$match_age,
                                          data.female$match_age < 18,
                                          18)
data <- rbind(data.male, data.female)
library(tidyverse)
set.seed(111)
data <- sample_n(data, 10000, replace = T)
 data <- data %>% 
   mutate(id = 1:nrow(data)) %>% 
   relocate(id, .before = everything())

data %>% pivot_wider(id_cols = id,
                     ,names_from = gender, values_from = c(age, match_age), values_fill = 99)

data %>%
  pivot_wider(id_cols = id,
              names_from = gender, 
              values_from = c(age, match_age), 
              values_fill = 99) %>%
  pivot_longer(cols = -id,
               names_to = c(".value", "gender"),
               names_pattern = "(age|match_age)_(female|male)") %>%
  mutate(across(c(age, match_age), ~ na_if(., 99))) %>% filter(complete.cases(.))


summary(data)
prop.table(table(data$gender))


ggplot(data = data, mapping = aes(x = age, y = match_age, colour = gender))+
  geom_jitter()+
  labs(
    x = "Age of the dataing app users",
    y = "Matched partners'age",
    title = "Function between Age and Attractiveness to Opposite Sex",
    colour = "Gender of the dating app user"
  )


ggplot(data = data, mapping = aes(x = age, y = match_age, colour = gender))+
  geom_smooth(linewidth = 1.5,se = F)+
  scale_color_manual(values = c("pink","lightblue"))+
  labs(
    x = "Age of the dataing app users",
    y = "Matched partners'age",
    title = "Function between Age and Attractiveness to Opposite Sex",
    subtitle = "Men are usually attracted to younger age,\nwhile women are attracted to more or less similar age",
    colour = "Gender of the dating app user"
  )+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "wheat1", colour = "black"),
        legend.position = "bottom")


  