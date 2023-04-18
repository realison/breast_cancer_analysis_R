library(tidyverse) #a must-have in R 
library(janitor) #for examining and cleaning dirty data
library(ggpubr) #for creating and customizing ggplot2
library(scales) #scaling used by ggplots
library(RColorBrewer) #for beautiful color palettes
library(purrr)
library(cowplot)
rm(list = ls())

#read csv
dir <- "/Users/xirao/Downloads/cancer/breast-cancer.csv"
cancer <- read_csv(file = dir)

#inspect Nas
cancer %>% 
  map(.,~sum(is.na(.))) %>% 
  simplify() %>% 
  tibble(col = names(.),NAs=.) %>% 
  print(n=32)
     
#inspect duplicated data
tibble( `number of observation` = cancer %>% nrow(),
        `number of unique rows` =cancer %>% unique() %>% nrow())

#statistics distribution
summary_cancer <- cancer %>% 
  select(-id,-diagnosis) %>% 
  imap(~ enframe(c(summary(.x)), name = "Metric", value = .y)) %>% 
  reduce(full_join, by = "Metric") %>%
  modify_if(is.numeric, replace_na, 0)

summary_cancer <- summary_cancer %>% 
  t() %>% 
  row_to_names(row_number = 1)

summary_cancer %>%
  as.data.frame() %>% 
  mutate_all(.,.funs=as.numeric) %>% 
  mutate(Cols = rownames(.)) %>%
  filter(str_detect(Cols,"mean")) %>% 
  ggplot(.,aes(Cols)) +
  geom_boxplot(
    aes(ymin = Min., lower = `1st Qu.`, middle = Median, upper = `3rd Qu.`, ymax = Max.),
    stat = "identity"
  )+
  geom_point(aes(y=Mean),color="darkred",size=1)+
  geom_text(aes(y=Mean,label=Mean),size=3,hjust=1,vjust=-1)+
  facet_wrap(~Cols,nrow=2,scales = "free")+
  theme(axis.text.x= element_text())+
  theme_bw()



#EDA

cancer %>% 
  count(diagnosis) %>% 
  arrange(n) %>% 
  mutate(diagnosis=as.factor(diagnosis),
  diagnosis = fct_inorder(diagnosis) )%>% 
  print(.) %>% 
  ggplot(aes(diagnosis,n,group=diagnosis))+
  geom_bar(aes(fill = factor(diagnosis)),stat="identity",show.legend = F)

#density distribution

cancer %>% 
  select(-contains("se")) %>% 
  mutate(diagnosis=as.factor(diagnosis),
         diagnosis = fct_inorder(diagnosis) )%>% 
  pivot_longer(cols = c(3:22),
               names_to = "Type",
               values_to = "Value") %>% 
  arrange(Type) %>% 
  ggplot(aes(Value,group =diagnosis))+
  geom_density(aes(fill=diagnosis),alpha=0.5)+
  facet_wrap(~Type,ncol=4,scales = "free")


#visualize the distribution of different measures group by diagnosis outcomes

mean_df <- cancer %>% 
  select(contains(c("mean","diagnosis"))) %>% 
  group_by(diagnosis) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop') %>% 
  pivot_longer(cols = c("radius_mean":"fractal_dimension_mean"),
               names_to = "Type",
               values_to = "Value")
#standard deviation
sd_df <- cancer %>% 
  select(contains(c("mean","diagnosis"))) %>% 
  group_by(diagnosis) %>% 
  summarise(across(everything(), sd),
            .groups = 'drop') %>% 
  pivot_longer(cols = c("radius_mean":"fractal_dimension_mean"),
               names_to = "Type",
               values_to = "sd_Value")

cancer %>% 
  select(contains(c("mean","diagnosis"))) %>% 
  pivot_longer(cols = c("radius_mean":"fractal_dimension_mean"),
               names_to = "Type",
               values_to = "Value") %>% 
  ggplot(.,aes(Value),group=diagnosis)+
  geom_histogram(aes(fill = diagnosis),alpha=0.4)+
  geom_vline(data=mean_df,aes(xintercept=Value,color=diagnosis),
             linetype="dashed",show.legend = F)+
  facet_wrap(~Type,scales = "free",ncol = 3)+
  scale_fill_manual(values = c( "blue","red"))+
  scale_color_manual(values=c("blue","red"))+
  theme_bw()


mean_df %>% 
  left_join(x=.,
            y=sd_df,
            by=c("diagnosis","Type")) %>% 
  pivot_longer(cols=c("Value","sd_Value"),
               names_to = "Value Type",
               values_to = "Value") %>%
  arrange(desc(Value))%>% 
  mutate(`Value Type`= as.factor(`Value Type`),
         `Value Type`= fct_inorder(`Value Type`)) %>% 
  ggplot(.,aes(y=Value),group=`Value Type`)+
  geom_bar(aes(x=diagnosis,fill=`Value Type`),stat = "identity",position = "dodge")+
  facet_wrap(~Type,scales = "free",ncol = 3)+
  scale_fill_manual(values=c("lightblue","pink"))+
  theme_bw()

install.packages("corrplot")
library(corrplot)

corr_df <- cancer %>% 
  select(-id,-diagnosis) %>% 
  select(contains(c("mean","diagnosis")))
 
corrplot(cor(corr_df),order = 'AOE')


#A matrix of the visual representation of the relationship between the 6 most highly correlated variables: 1. radius_mean 2. parameter_mean3. area_mean 4. compactness_mean 5. concavity_mean 6. concave_points_mean

library(GGally)
cancer %>% 
  select(diagnosis,radius_mean,perimeter_mean,area_mean,compactness_mean,concavity_mean,`concave points_mean`) %>% 
  #chart.Correlation(., histogram=F)
  ggpairs(.,column=2:7, aes(color = diagnosis,  # Color by group (cat. variable)
                alpha = 0.5),diag = list(continuous = "blankDiag"),
          upper = list(continuous = "cor"),
          lower = list(continuous = "points", combo = "dot_no_facet"))+
  scale_color_manual(values = c("yellowgreen","orange"))
  

#by drawing the scatter plot of those strong correlated parameters, we could see the B/M group are clearly clustered.
# for instance, we could observe that  M group located in the bigger mean radius and bigger perimeter mean area

#regression model
model_df <- cancer %>% 
  select(diagnosis,radius_mean,perimeter_mean,area_mean,compactness_mean,concavity_mean,`concave points_mean`) %>%
  mutate(diagnosis = str_replace_all(diagnosis,"M","1"),
         diagnosis = str_replace_all(diagnosis,"B","0")) %>% 
  mutate(diagnosis=as.numeric(diagnosis)) 

mod1 <- glm(formula = diagnosis ~radius_mean+perimeter_mean ,data=model_df,family = binomial)
summary(mod1)
library(car)
Anova(mod1)
install.packages("pROC")
library(pROC)
roc(model_df$diagnosis,model_df$radius_mean,plot=T)









