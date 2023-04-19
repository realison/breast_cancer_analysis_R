# breast_cancer_analysis_R


## Dataset
Breast cancer is the most common malignancy among women, accounting for nearly 1 in 3 cancers diagnosed among women in the United States, and it is the second leading cause of cancer death among women. Breast Cancer occurs as a results of abnormal growth of cells in the breast tissue, commonly referred to as a Tumor. A tumor does not mean cancer - tumors can be benign (not cancerous), pre-malignant (pre-cancerous), or malignant (cancerous). Tests such as MRI, mammogram, ultrasound and biopsy are commonly used to diagnose breast cancer performed.

A diagnosis technique that uses the FNA (Fine Needle Aspiration), which is a quick and simple procedure to perform, which removes some fluid or cells from a breast lesion or cyst with a fine needle similar to a blood sample needle.

This is an analysis of the Breast Cancer Wisconsin (Diagnostic) DataSet, obtained from Kaggle. This data set was created by Dr. William H. Wolberg, physician at the University Of Wisconsin Hospital at Madison, Wisconsin,USA. To create the dataset Dr. Wolberg used ???uid samples, taken from patients with solid breast masses and an easy-to-use graphical computer program called Xcyt, which is capable of perform the analysis of cytological features based on a digital scan. The program uses a curve-???tting algorithm, to compute ten features from each one of the cells in the sample, than it calculates the mean value, extreme value and standard error of each feature for the image, returninga 30 real-valuated vector

Attribute Information:
ID number 2) Diagnosis (M = malignant, B = benign

Ten real-valued features are computed for each cell nucleus:

radius (mean of distances from center to points on the perimeter)

texture (standard deviation of gray-scale values)

perimeter area

smoothness (local variation in radius lengths)

compactness (perimeter^2 / area - 1.0)

concavity (severity of concave portions of the contour)

concave points (number of concave portions of the contour)

symmetry

fractal dimension (“coastline approximation” - 1)

## Data Inspection


```
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

```

569 observation was recorded in this dataset.No NA or duplicated data was found in this dataset. 

## Exploratory Data Analysis

212 observations are diagnosed as Malignant, 357 are diagnosed as benign. A bar chart shows the comparation.

```
#EDA

cancer %>% 
  count(diagnosis) %>% 
  arrange(n) %>% 
  mutate(diagnosis=as.factor(diagnosis),
  diagnosis = fct_inorder(diagnosis) )%>% 
  print(.) %>% 
  ggplot(aes(diagnosis,n,group=diagnosis))+
  geom_bar(aes(fill = factor(diagnosis)),stat="identity",show.legend = F)
  
```

 diagnosis     n
 
  <fct>     <int>
  
1 M           212

2 B           357

![p1](https://user-images.githubusercontent.com/93932414/232930883-d4a951b0-ccf7-464c-b65e-266d76e7f428.png)


a statistics distribution include mean and standard deviation was calculated. I only filtered the columns contain mean value not the worst and se. With the boxplot, we can see the mean value of different diagnosis dimensions and its distribution. 

```

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


```


![p2](https://user-images.githubusercontent.com/93932414/232934664-63f96a80-0578-4148-a0ec-098195c560ad.png)


now, I will explore the density of each column and split observations by diagnosis outcomes.

```
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

```
![p3](https://user-images.githubusercontent.com/93932414/232935010-8eef0013-e5b5-48bc-b641-2d7b5c29aa35.png)

For some parameters we can see the malignant observations have a more spread and right skewed distribution. For this plot I contained mean and worst columns.

Next, we will have use the histogram plot and a mean vertical line to display the distribution of parameters that contain mean.


```
mean_df <- cancer %>% 
  select(contains(c("mean","diagnosis"))) %>% 
  group_by(diagnosis) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop') %>% 
  pivot_longer(cols = c("radius_mean":"fractal_dimension_mean"),
               names_to = "Type",
               values_to = "Value")

p4 <- cancer %>% 
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
  
  ```

![p4](https://user-images.githubusercontent.com/93932414/232935591-f6ae6246-5218-4785-a9ab-4b1a5976841b.png)

To compare the spread of distribution, I introduce the standard deviation and use it to compare with the mean value. Still, I compare the M and B group respectively. 

We could find some parameters have a low sd, which mean the distribution close to its mean.

```
#standard deviation
sd_df <- cancer %>% 
  select(contains(c("mean","diagnosis"))) %>% 
  group_by(diagnosis) %>% 
  summarise(across(everything(), sd),
            .groups = 'drop') %>% 
  pivot_longer(cols = c("radius_mean":"fractal_dimension_mean"),
               names_to = "Type",
               values_to = "sd_Value")
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
  
  ```
  
  ![p5](https://user-images.githubusercontent.com/93932414/232935782-56913ec6-1de3-4823-9e02-8ed9a295eb0f.png)


## Correlation Analysis

Then, I did correlation analysis. I used the corrplot package to draw the correlation plot. And we could found some parameters with strong correlations. We will dive into those parameters later.

```

install.packages("corrplot")
library(corrplot)

corr_df <- cancer %>% 
  select(-id,-diagnosis) %>% 
  select(contains(c("mean","diagnosis")))
 
corrplot(cor(corr_df),order = 'AOE')

```

![p6](https://user-images.githubusercontent.com/93932414/232936475-c1616e95-e3bc-4d9a-a2cb-d344dd745cf6.png)


Then, I filtered those parameters, and use GGally package to visualize the correlation matrix. We could find observations are separated clear clusters.

A matrix of the visual representation of the relationship between the 6 most highly correlated variables: 1. radius_mean 2. parameter_mean3. area_mean 4. compactness_mean 5. concavity_mean 6. concave_points_mean


```

library(GGally)
cancer %>% 
  select(diagnosis,radius_mean,perimeter_mean,area_mean,compactness_mean,concavity_mean,`concave points_mean`) %>% 
  #chart.Correlation(., histogram=F)
  ggpairs(.,column=2:7, aes(color = diagnosis,  # Color by group (cat. variable)
                alpha = 0.5),diag = list(continuous = "blankDiag"),
          upper = list(continuous = "cor"),
          lower = list(continuous = "points", combo = "dot_no_facet"))+
  scale_color_manual(values = c("yellowgreen","orange"))
  
  ```
  
by drawing the scatter plot of those strong correlated parameters, we could see the B/M group are clearly clustered.

for instance, we could observe that  M group located in the bigger mean radius and bigger perimeter mean area


![p7](https://user-images.githubusercontent.com/93932414/232936781-eb6154d3-6e7f-414b-a7d0-1af03913dcfb.png)


## Regression Model

A logistic model was also built to explore the correlation, however, I don't explore too much in building the best model. pROC package was introduced to build the ROC AUC curve

```
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

```



