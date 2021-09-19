---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: ""
draft: false
image: pic10.jpg
keywords: ""
slug: homeworkthree
title: Ipsum
---


---
title: "Session 6: Homework 3"
author: "Your name goes here"
date: "`r Sys.Date()`"
output:
  html_document:
    theme: flatly
    highlight: zenburn
    number_sections: yes
    toc: yes
    toc_float: yes
    code_folding: show
editor_options: 
  chunk_output_type: inline
---


```{r, setup, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r load-libraries, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(GGally)
library(readxl)
library(here)
library(skimr)
library(janitor)
library(broom)
library(tidyquant)
library(infer)
library(openintro)
```

# Youth Risk Behavior Surveillance

Every two years, the Centers for Disease Control and Prevention conduct the [Youth Risk Behavior Surveillance System (YRBSS)](https://www.cdc.gov/healthyyouth/data/yrbs/index.htm) survey, where it takes data from high schoolers (9th through 12th grade), to analyze health patterns. You will work with a selected group of variables from a random sample of observations during one of the years the YRBSS was conducted.

## Load the data

This data is part of the `openintro` textbook and we can load and inspect it. There are observations on 13 different variables, some categorical and some numerical. The meaning of each variable can be found by bringing up the help file:

?yrbss

```{r}
data(yrbss)
glimpse(yrbss)
```

Before you carry on with your analysis, it's is always a good idea to check with `skimr::skim()` to get a feel for missing values, summary statistics of numerical variables, and a very rough histogram.
```{r}
skim(yrbss)
```


## Exploratory Data Analysis

You will first start with analyzing the `weight` of participants in kilograms. Using visualization and summary statistics, describe the distribution of weights. How many observations are we missing weights from?

The distribution of weight is right(positive) skewed. 1004 observations are missed in the weight from. 

```{r, eda_on_weight}
skim(yrbss)

ggplot(data = yrbss)+
  geom_density(aes(x = weight), kernel = "gaussian") 

```

Next, consider the possible relationship between a high schooler’s weight and their physical activity. Plotting the data is a useful first step because it helps us quickly visualize trends, identify strong associations, and develop research questions.

Let’s create a new variable in the dataframe `yrbss`, called `physical_3plus` , which will be `yes` if they are physically active for at least 3 days a week, and `no` otherwise. You may also want to calculate the number and % of those who are and are not active for more than 3 days. Use the `count()` function and see if you get the same results as `group_by()... summarise()`

  
```{r, mutate_and_count}
yrbss <- yrbss %>% 
  # count(physically_active_7d) %>% 
  mutate(yrbss,physical_3plus = ifelse(physically_active_7d >= 3,"yes","no"))


yrbss_count <- yrbss %>% 
  count(physical_3plus) %>% 
  mutate(perc = n/sum(n))
yrbss_count
```
Can you provide a 95% confidence interval for the population proportion of high schools that are *NOT* active 3 or more days per week?

  
Make a boxplot of `physical_3plus` vs. `weight`. Is there a relationship between these two variables? What did you expect and why?

There is a slightly positive relationship between thse two variables. I expected that there would be positive relationship between physical_3plus and weight because if people did more execrises per week, they would have more muscles which would be much heavier that fat.

```{r, boxplot}
ybrss_physical3minus <- 
  filter(yrbss, physical_3plus == "no")
# ybrss_physical3minus
# yrbss
ggplot(data = yrbss,aes(x = physical_3plus, y =weight))+
  geom_boxplot()

```

## Confidence Interval

Boxplots show how the medians of the two distributions compare, but we can also compare the means of the distributions using either a confidence interval or a hypothesis test. Note that when we calculate the mean, SD, etc. weight in these groups using the mean function, we must ignore any missing values by setting the `na.rm = TRUE`.


```{r, ci_using_formulas}
# t.test(weight ~ physical_3plus, data = yrbss)
yrbss %>% 
  group_by(physical_3plus) %>% 
  summarise(mean_weight = mean(weight,na.rm = TRUE))
```

There is an observed difference of about 1.77kg (68.44 - 66.67), and we notice that the two confidence intervals do not overlap. It seems that the difference is at least 95% statistically significant. Let us also conduct a hypothesis test.

## Hypothesis test with formula

Write the null and alternative hypotheses for testing whether mean weights are different for those who exercise at least times a week and those who don’t.

The null hypotheses : The mean weights of people who exercise at least times a week is the same as the mean weights of people who don't.
The alternative hypothesesz: The mean weights of people who exercise at least times a week is significantly different from the mean weights of people who don't.

```{r, t_test_using_R}
t.test(weight ~ physical_3plus, data = yrbss)
```



## Hypothesis test with `infer`


Next, we will introduce a new function, `hypothesize`, that falls into the infer workflow. You will use this method for conducting hypothesis tests.

But first, we need to initialize the test, which we will save as `obs_diff`.

```{r, calc_obs_difference}
obs_diff <- yrbss %>%
  specify(weight ~ physical_3plus) %>%
  calculate(stat = "diff in means", order = c("yes", "no"))
obs_diff
```



Notice how you can use the functions specify and calculate again like you did for calculating confidence intervals. Here, though, the statistic you are searching for is the difference in means, with the order being yes - no != 0.

After you have initialized the test, you need to simulate the test on the null distribution, which we will save as null.


```{r, hypothesis_testing_using_infer_package}

null_dist <- yrbss %>%
  # specify variables
  specify(weight ~ physical_3plus) %>%
  
  # assume independence, i.e, there is no difference
  hypothesize(null = "independence") %>%
  
  # generate 1000 reps, of type "permute"
  generate(reps = 1000, type = "permute") %>%
  
  # calculate statistic of difference, namely "diff in means"
  calculate(stat = "diff in means", order = c("yes", "no"))
null_dist
```


Here, `hypothesize` is used to set the null hypothesis as a test for independence, i.e., that there is no difference between the two population means. In one sample cases, the null argument can be set to *point* to test a hypothesis relative to a point estimate.

Also, note that the `type` argument within generate is set to permute, which is the argument when generating a null distribution for a hypothesis test.

We can visualize this null distribution with the following code:

```{r}
ggplot(data = null_dist, aes(x = stat)) +
  geom_histogram()
```


Now that the test is initialized and the null distribution formed, we can visualise to see how many of these null permutations have a difference of at least `obs_stat` of `r obs_diff %>% pull() %>% round(2)`?

We can also calculate the p-value for your hypothesis test using the function `infer::get_p_value()`.

```{r}

null_dist %>% visualize() +
  shade_p_value(obs_stat = obs_diff, direction = "two-sided")

null_dist %>%
  infer::get_p_value(obs_stat = obs_diff, direction = "two_sided")

```

This the standard workflow for performing hypothesis tests.

# IMDB ratings: Differences between directors

Recall the IMBD ratings data. I would like you to explore whether the mean IMDB rating for Steven Spielberg and Tim Burton are the same or not. I have already calculated the confidence intervals for the mean ratings of these two directors and as you can see they overlap. 


```{r directors, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "directors.png"), error = FALSE)
```

First, I would like you to reproduce this graph. You may find `geom_errorbar()` and `geom_rect()` useful.

In addition, you will run a hpothesis test. You should use both the `t.test` command and the `infer` package to simulate from a null distribution, where you assume zero difference between the two.

> Before anything, write down the null and alternative hypotheses, as well as the resulting test statistic and the associated t-stat or p-value. At the end of the day, what do you conclude?

You can load the data and examine its structure

```{r load-movies-data}
movies <- read_csv(here::here("data", "movies.csv"))
glimpse(movies)
```

Your R code and analysis should go here. If you want to insert a blank chunk of R code you can just hit `Ctrl/Cmd+Alt+I` 

```{r}
ci_data <- movies %>% 
  filter(director %in% c("Steven Spielberg","Tim Burton")) %>% 
  group_by(director) %>% 
  summarise(mean_rating = mean(rating),
            count = n(),
            margin_of_error = qt(0.975, count-1)*sd(rating)/sqrt(count),
            rating_lower = mean_rating - margin_of_error,
            rating_upper = mean_rating + margin_of_error)

ggplot(ci_data,aes(x = mean_rating, y = reorder(director, desc(director)), colour = director)) + 
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = rating_lower, xmax = rating_upper), width = 0.2, size = 1) +
  geom_rect(aes(xmin=max(rating_lower), xmax=min(rating_upper), ymin=0, ymax=Inf), color='grey', alpha=0.2) +
  geom_text(aes(label = round(mean_rating, digits = 2), x = mean_rating), size = 6, colour = "black", nudge_y = 0.15)+ 
  geom_text(aes(label = round(rating_lower, digits = 2), x = rating_lower), size = 4, colour = "black",nudge_y = 0.15) +
  geom_text(aes(label = round(rating_upper, digits = 2), x = rating_upper), size = 4, colour = "black",nudge_y = 0.15) +
  labs(title = "Do Spielberg and Burton have the same mean IMDB ratings?", subtitle = "95% confidence intervals overlap" , x = "Mean IMDB Rating", y = " ") + theme_bw() + theme(legend.position = "none")
```


# Omega Group plc- Pay Discrimination


At the last board meeting of Omega Group Plc., the headquarters of a large multinational company, the issue was raised that women were being discriminated in the company, in the sense that the salaries were not the same for male and female executives. A quick analysis of a sample of 50 employees (of which 24 men and 26 women) revealed that the average salary for men was about 8,700 higher than for women. This seemed like a considerable difference, so it was decided that a further analysis of the company salaries was warranted. 

You are asked to carry out the analysis. The objective is to find out whether there is indeed a significant difference between the salaries of men and women, and whether the difference is due to discrimination or whether it is based on another, possibly valid, determining factor. 

## Loading the data


```{r load_omega_data}
omega <- read_csv(here::here("data", "omega.csv"))
glimpse(omega) # examine the data frame
```

## Relationship Salary - Gender ?

The data frame `omega`  contains the salaries for the sample of 50 executives in the company. Can you conclude that there is a significant difference between the salaries of the male and female executives?

Note that you can perform different types of analyses, and check whether they all lead to the same conclusion 

.	Confidence intervals
.	Hypothesis testing
.	Correlation analysis
.	Regression


Calculate summary statistics on salary by gender. Also, create and print a dataframe where, for each gender, you show the mean, SD, sample size, the t-critical, the SE, the margin of error, and the low/high endpoints of a 95% condifence interval

```{r, confint_single_valiables}
# Summary Statistics of salary by gender
mosaic::favstats (salary ~ gender, data=omega)

# Dataframe with two rows (male-female) and having as columns gender, mean, SD, sample size, 
# the t-critical value, the standard error, the margin of error, 
# and the low/high endpoints of a 95% condifence interval


```

> What can you conclude from your analysis? A couple of sentences would be enough

You can also run a hypothesis testing, assuming as a null hypothesis that the mean difference in salaries is zero, or that, on average, men and women make the same amount of money. You should tun your hypothesis testing using `t.test()` and with the simulation method from the `infer` package.

```{r, hypothesis_testing}
# hypothesis testing using t.test() 
t.test(salary ~ gender, data=omega)

# hypothesis testing using infer package
obs_diff_2 <- omega %>%
  specify(salary ~ gender) %>%
  calculate(stat = "diff in means", order = c("female", "male"))

obs_diff_2

null_dist_2 <- omega %>%
  # specify variables
  specify(salary ~ gender) %>%
  
  # assume independence, i.e, there is no difference
  hypothesize(null = "independence") %>%
  
  # generate 1000 reps, of type "permute"
  generate(reps = 1000, type = "permute") %>%
  
  # calculate statistic of difference, namely "diff in means"
  calculate(stat = "diff in means", order = c("female", "male"))

null_dist_2

null_dist_2 %>% visualize()


```

```{r}
ggplot(data = null_dist_2, aes(x = stat)) +
  geom_histogram()

null_dist_2 %>% visualize() +
  shade_p_value(obs_stat = obs_diff_2, direction = "two-sided")

null_dist_2 %>%
  infer::get_p_value(obs_stat = obs_diff_2, direction = "two_sided")
```


> What can you conclude from your analysis? A couple of sentences would be enough

According to our calculation by t.test, our p-value for the hypothesis test is 0.0001651, which is smaller than 0.05 and hence statistically significant. The p-value we retrieved from our simulations with the infer package is also infinitesmal and statistically significant. There is strong evidence against the null hypothesis and the -8696.29 years (64542.84-73239.13) of difference we estimated in our sample means is really different from zero, therefore there is a significant difference. 

We can safely conclude the salary levels of male executives is, on average, 8696.29 units more than female counterparts. 


## Relationship Experience - Gender?

At the board meeting, someone raised the issue that there was indeed a substantial difference between male and female salaries, but that this was attributable to other reasons such as differences in experience. A questionnaire send out to the 50 executives in the sample reveals that the average experience of the men is approximately 21 years, whereas the women only have about 7 years experience on average (see table below).

```{r, experience_stats}
# Summary Statistics of salary by gender
favstats (experience ~ gender, data=omega)

```

Based on this evidence, can you conclude that there is a significant difference between the experience of the male and female executives? Perform similar analyses as in the previous section. Does your conclusion validate or endanger your conclusion about the difference in male and female salaries?  

```{r}
# hypothesis testing using t.test() 
t.test(experience ~ gender, data=omega)

# hypothesis testing using infer package
obs_diff_3 <- omega %>%
  specify(experience ~ gender) %>%
  calculate(stat = "diff in means", order = c("female", "male"))

obs_diff_3

null_dist_3 <- omega %>%
  # specify variables
  specify(experience ~ gender) %>%
  
  # assume independence, i.e, there is no difference
  hypothesize(null = "independence") %>%
  
  # generate 1000 reps, of type "permute"
  generate(reps = 1000, type = "permute") %>%
  
  # calculate statistic of difference, namely "diff in means"
  calculate(stat = "diff in means", order = c("female", "male"))

null_dist_3

null_dist_3 %>% visualize()
```

```{r}
ggplot(data = null_dist_3, aes(x = stat)) +
  geom_histogram()

null_dist_3 %>% visualize() +
  shade_p_value(obs_stat = obs_diff_3, direction = "two-sided")

null_dist_3 %>%
  infer::get_p_value(obs_stat = obs_diff_3, direction = "two_sided")
```

According to our calculation by t.test, our p-value for the hypothesis test is 1.225e-05, which is smaller than 0.05 and hence statistically significant. The p-value we retrieved from our simulations with the infer package is also infinitesmal and statistically significant. There is strong evidence against the null hypothesis and the -13.7 years (7.38-21.13) of difference we estimated in our sample means is really different from zero, therefore there is a significant difference. 

We can safely conclude the experience levels of male executives is, on average, 13.7 years more than female counterparts. 

## Relationship Salary - Experience ?

Someone at the meeting argues that clearly, a more thorough analysis of the relationship between salary and experience is required before any conclusion can be drawn about whether there is any gender-based salary discrimination in the company.

Analyse the relationship between salary and experience. Draw a scatterplot to visually inspect the data


```{r, salary_exp_scatter}
ggplot(omega, aes(x = experience, y = salary)) + geom_point() +geom_smooth(se = FALSE)
```
In general, there is a moderate positive relationship between executives' salary and experience. However, the strength of this relationship becomes weaker as the experience level increases, suggesting that experience may be an important factor influencing the salary levels for early-careers and not so much important for the already experienced professionals. 

## Check correlations between the data
You can use `GGally:ggpairs()` to create a scatterplot and correlation matrix. Essentially, we change the order our variables will appear in and have the dependent variable (Y), salary, as last in our list. We then pipe the dataframe to `ggpairs()` with `aes` arguments to colour by `gender` and make ths plots somewhat transparent (`alpha  = 0.3`).

```{r, ggpairs}
omega %>% 
  select(gender, experience, salary) %>% #order variables they will appear in ggpairs()
  ggpairs(aes(colour=gender, alpha = 0.3))+
  theme_bw()
```

> Look at the salary vs experience scatterplot. What can you infer from this plot? Explain in a couple of sentences

The salary vs experience scatterplot shows that female executives in this sample are more concentrated on the left side of the plot, which means that generally they have lower experience levels than their male counterparts. Given the 0.812 correlation between salary and experience levels for female executives, which is higher than the 0.661 correlation for males, it is reasonable that we see a stronger relationship between salary and experience levels for early career professionals. 

However, we cannot safely conclude whether this difference in salary level between female and male executives is due to gender difference or difference in experience levels. More research is required to investigate the causal relationship on salary levels and the two different factors. 


# Challenge 1: Brexit plot

Using your data manipulation and visualisation skills, please use the Brexit results dataframe (the same dataset you used in the pre-programme assignement) and produce the following plot. Use the correct colour for each party; google "UK Political Party Web Colours" and find the appropriate hex code for colours, not the default colours that R gives you.

```{r brexit_challenge, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "brexit.png"), error = FALSE)
directory <- getwd()
directory
myfile = "brexit_results.csv"
filepath=paste0(directory,"/",myfile[1],sep="")
filepath
data_brex_results <- read_csv(filepath)

skim(data_brex_results)

brex_results_long <- data_brex_results %>% 
  pivot_longer(cols = 2:5, #columns 3 to 5
               names_to = "indicator",
               values_to = "value")

brex_results_long %>% 
  # filter( %in% c("France", "Spain", "Italy", "United Kingdom", "Thailand", "China")) %>%
  ggplot(aes(x=value, y = leave_share, colour=indicator))+
  scale_color_manual(values = c("#0087dc","#d50000","#FDBB30","#EFE600"))+
  # geom_line(aes(x = value,y = leave_share,color = indicator),size = 0.5)+
  geom_point(size= 0.7)+
  geom_smooth(method = 'lm', formula =y~x)+
  coord_fixed(ratio=0.5)+
  # facet_grid(rows = vars(indicator),
             # cols = vars(country), 
             # scales = "free")+
  # theme_bw()+
  # theme(legend.position = "none")+
  # scale_y_continuous(labels = scales::label_percent())+
  labs(
    title = "How political affliation translated to Brexit Voting",
    x = "Party % in the UK 2015 general election",
    y = "Leave % in the 2016 Brexit reference"
  )+
theme(legend.position="bottom")
ggsave("brexresultslong.jpg")
# 

```

# Challenge 2: CDC COVID-19 Public Use Data

The [CDC Covid-19 Case Surveillance Data](https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf) is a case surveillance public use dataset with 12 elements for all COVID-19 cases shared with CDC and includes demographics, any exposure history, disease severity indicators and outcomes, presence of any underlying medical conditions and risk behaviors. You can see the variables from 


```{r covid_data, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "cdc_data.png"), error = FALSE)
```


There are well over 28 million entries of individual, and we will work with SQLlite database, rather than a CSV file. Let's produce two graphs that show death % rate:

1. by age group, sex, and whether the patient had co-morbidities or not
2. by age group, sex, and whether the patient was admited to Intensive Care Unit (ICU) or not.


```{r covid_challenge, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "covid_death_rate_comorbidities.png"), error = FALSE)
knitr::include_graphics(here::here("images", "covid_death_rate_icu.png"), error = FALSE)
```

To do this, you will have to think what dplyr verbs to use to select, filter, group_by, etc. You will then use the example shown in https://mam2022.netlify.app/reference/reference_sql/#establish-a-connection-with-the-sqlite-database-1 to use dplyr, dbplyr, and ggplot to produce these graphs.

1. by age group, sex, and whether the patient had co-morbidities or not

```{r}
library(tidyverse)
library(RSQLite)
library(dbplyr)
library(DBI)
library(janitor)

# set up a connection to sqlite database

cdc_db <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "cdc_data.db"
)

# browse the tables in the database
DBI::dbListTables(cdc_db)

# Set these tables up as database
cdc_data <- dplyr::tbl(cdc_db, "cdc")

cdc_data

# Calculate Covid death % by age group, sex 
# and presence of co-morbidities (query1) 
query1 <-   cdc_data %>%
  filter(current_status == "Laboratory-confirmed case")%>%
  arrange(death_yn) %>%
  select(!onset_dt) %>%
  select(!pos_spec_dt) %>%
  # Create the features needed
  mutate(medcond_yn = case_when(medcond_yn == "Missing" ~ as.character("Unknown"), TRUE ~ medcond_yn),
         age_group = case_when(age_group == "NA" ~ as.character("Missing"), TRUE ~ age_group),
         sex = case_when(sex == "NA"|"Unknown" ~ as.character("Missing"),
#                         sex == "Unknown" ~ as.character("Missing"),
                         TRUE ~ sex))%>%
  
  group_by(age_group, sex, medcond_yn) 


# what kind of a thing is query1?
class(query1)

# Generate actual SQL commands: We can either use dbplyr::sql_render() or dplyr::show_query()
dbplyr::sql_render(query1)

# execute query and retrieve results in a tibble (dataframe). 
query1_tibble <- query1 %>% 
  collect() # collect runs the SQL query and returns the output of your dplyr pipe se

query1_tibble
```


```{r}
# Filter off the missing and unknown datapoints to keep only interested ones and recode the comorbity feature 
# into desired format to produce the dataset used for plotting
death_rate_plot <- query1_tibble %>%
  filter(sex == "Male" | sex == "Female")%>%
  filter(!age_group == "Missing") %>%
  filter(medcond_yn == "Yes"| medcond_yn == "No") %>% 
  summarise(Yes = count(death_yn == "Yes"), total = n()) %>%
  mutate(rate = Yes / total)%>%
  mutate(medcond_yn = case_when(medcond_yn == "Yes" ~ as.character("Comorbities"), 
                                medcond_yn == "No" ~ as.character("No comorbities"), 
                                TRUE ~ medcond_yn))

```

```{r out.width = "100%"}
# Create the plot
ggplot(death_rate_plot, aes(y = age_group, x = rate*100)) +
  geom_col(fill = 'salmon') +
  geom_text(aes(label = round(rate*100,digits = 1)),
            size = 3,
            nudge_x = 3) +
  facet_wrap(~ sex + medcond_yn ) +
  theme_bw()+
  labs(title = "COVID Death % by Age, Sex and Presence of Comorbities", 
       x = "Death Rate (%)",
       y = '') +
  NULL
```


2. by age group, sex, and whether the patient was admited to Intensive Care Unit (ICU) or not.

```{r}
# Calculate Covid death % 
# by age group, sex and ICU admission (query2) 
query2 <-   cdc_data %>%
  # dplyr commands like 
  # select, filter, group_by, summarise...
  filter(current_status == "Laboratory-confirmed case")%>%
  arrange(death_yn) %>%
  select(!onset_dt) %>%
  select(!pos_spec_dt) %>%
  mutate(icu_yn = case_when(icu_yn == "Unknown" ~ as.character("Missing"), TRUE ~ icu_yn),
         age_group = case_when(age_group == "NA" ~ as.character("Missing"), TRUE ~ age_group),
         sex = case_when(sex == "NA" ~ as.character("Missing"), 
                         sex == "Unknown" ~ as.character("Missing"), 
                         TRUE ~ sex))%>%
  group_by(age_group, sex, icu_yn) 

query2

# what kind of a thing is query2?
class(query2)

# Generate actual SQL commands: We can either use dbplyr::sql_render() or dplyr::show_query()
dbplyr::sql_render(query2)

# execute query and retrieve results in a tibble (dataframe). 
query2_tibble <- query2 %>% 
  collect() # collect runs the SQL query and returns the output of your dplyr pipe se

query2_tibble
```


```{r}
# Create features similarly to previous section
death_rate_plot_icu <- query2_tibble %>%
  summarise(Yes = count(death_yn == "Yes"), total = n()) %>%
  mutate(rate = Yes / total) %>%
  filter(sex == "Male"|sex == "Female")%>%
  filter(!age_group == "Missing") %>%
  filter(icu_yn == "Yes"|icu_yn == "No") %>% 
  mutate(icu_yn = case_when(icu_yn == "Yes" ~ as.character("ICU"),
                            icu_yn == "No" ~ as.character("No ICU"), 
                            TRUE ~ icu_yn))

```

```{r out.width = "100%"}
# Create the plot
ggplot(death_rate_plot_icu, aes(y = age_group, x = rate*100)) +
  geom_col(fill = 'lightblue') +
  geom_text(aes(label = round(rate*100,digits = 1)),
            size = 3,
            nudge_x = 5) +
  facet_wrap(~ sex + icu_yn ) +
  theme_bw()+
  labs(title = "COVID Death % by Age, Sex and ICU Admission", 
       x = "Death Rate (%)",
       y = '') +
  NULL

```


# Challenge 2:GDP components over time and among countries

At the risk of oversimplifying things, the main components of gross domestic product, GDP are personal consumption (C), business investment (I), government spending (G) and net exports (exports - imports). You can read more about GDP and the different approaches in calculating at the [Wikipedia GDP page](https://en.wikipedia.org/wiki/Gross_domestic_product).

The GDP data we will look at is from the [United Nations' National Accounts Main Aggregates Database](https://unstats.un.org/unsd/snaama/Downloads), which contains estimates of total GDP and its components for all countries from 1970 to today. We will look at how GDP and its components have changed over time, and compare different countries and how much each component contributes to that country's GDP. The file we will work with is [GDP and its breakdown at constant 2010 prices in US Dollars](http://unstats.un.org/unsd/amaapi/api/file/6) and it has already been saved in the Data directory. Have a look at the Excel file to see how it is structured and organised


```{r read_GDP_data}

UN_GDP_data  <-  read_excel(here::here("data", "Download-GDPconstant-USD-countries.xls"), # Excel filename
                sheet="Download-GDPconstant-USD-countr", # Sheet name
                skip=2) # Number of rows to skip

```

 The first thing you need to do is to tidy the data, as it is in wide format and you must make it into long, tidy format. Please express all figures in billions (divide values by `1e9`, or $10^9$), and you want to rename the indicators into something shorter.

> make sure you remove `eval=FALSE` from the next chunk of R code-- I have it there so I could knit the document

```{r reshape_GDP_data}

tidy_GDP_data  <-  UN_GDP_data %>% 
  pivot_longer(cols = 4:51, names_to = "Year", values_to = "Value") %>%
  mutate(Value = Value/(10^9))
glimpse(tidy_GDP_data)


# Let us compare GDP components for these 3 countries
country_list <- c("United States","India", "Germany")
```

First, can you produce this plot?


```{r gdp1, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "gdp1.png"), error = FALSE)
```

```{r}
three_country_data <- tidy_GDP_data %>% 
  filter(Country %in% country_list) %>% 
  filter(IndicatorName %in% c("Gross capital formation" ,"Exports of goods and services" ,"General government final consumption expenditure" , "Household consumption expenditure (including Non-profit institutions serving households)", "Imports of goods and services")) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  group_by(Year)
        
ggplot(three_country_data, mapping = aes(x=Year, y=Value , colour=IndicatorName)) +geom_line() + 
  facet_wrap(~Country) +
  labs(title = "GDP components over time", subtitle = "In constant 2010 USD", colour = "Components of GDP", x = element_blank(), y = "Billion US$") + 
  scale_colour_discrete(breaks = c("Gross capital formation" ,"Exports of goods and services" ,"General government final consumption expenditure" , "Household consumption expenditure (including Non-profit institutions serving households)", "Imports of goods and services"), labels = c("Gross capital formation" ,"Exports" ,"Government expenditure" , "Household expenditure", "Imports") ) +
  theme_bw()
```


Secondly, recall that GDP is the sum of Household Expenditure (Consumption *C*), Gross Capital Formation (business investment *I*), Government Expenditure (G) and Net Exports (exports - imports). Even though there is an indicator `Gross Domestic Product (GDP)` in your dataframe, I would like you to calculate it given its components discussed above.

> What is the % difference between what you calculated as GDP and the GDP figure included in the dataframe?

```{r}
wide_GDP_data <- tidy_GDP_data %>% 
  pivot_wider(names_from = "IndicatorName", values_from  = "Value") %>% 
  clean_names()

wide_GDP_data %>% 
  mutate(GDP_calculated = gross_capital_formation + general_government_final_consumption_expenditure+ household_consumption_expenditure_including_non_profit_institutions_serving_households+ exports_of_goods_and_services - imports_of_goods_and_services) %>% 
  select(country, year, GDP_calculated, gross_domestic_product_gdp)

```



```{r gdp2, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "gdp2.png"), error = FALSE)
```


> What is this last chart telling you? Can you explain in a couple of paragraphs the different dynamic among these three countries? 

This chart shows the separate trends of the proportions of 4 components of GDP. We can see that in terms of household expenditure, which is the biggest component of all three countries' GDP, its proportion is relatively stable for Germany and United States and decreasing for India. Germany has the most stable household expediture level between 50% and 60%. The United States' household expenditure exhibits a slightly increasing trend from 60% to 70%. India's household expenditure has a sharp decreasing trend since 1980. Such differences may be due to the fact that Germany and the US are both developed countries and are already stable in development speed. 

Gross capital formation, which indicates the level of investment slightly decreases over time in Germany, slightly increased in the US, and increased significantly in India especially since 2000. Hence, we can reasonably speculate that India might have gone through a period of rapid development, with households decreasing their consumption, saving more money, and initiating more investments. 

Government expenditure has been stable in Germany and India, but exhibits a decreasing trend for the US, probably due to a tightening government budget and increasing level of debt. It is worth noting that India's gross capital formation proportion in GDP has always beeen 10-30% above the government expenditure proportion level, while these two components have been competing for Germany and the US, both fluctuating at around 20%. 

Net exports have been stable for India and the US before 2000 and both experienced a decrease of about 5% after then. Germany's net export proportion level has also been stable until 2000 and experienced an increase afterwards, which suggests that it became more of an exporter than importer than it was before 2000. 


> If you want to, please change `country_list <- c("United States","India", "Germany")` to include your own country and compare it with any two other countries you like


# Deliverables

There is a lot of explanatory text, comments, etc. You do not need these, so delete them and produce a stand-alone document that you could share with someone. Knit the edited and completed R Markdown file as an HTML document (use the "Knit" button at the top of the script editor window) and upload it to Canvas.

# Details

- Who did you collaborate with: TYPE NAMES HERE
- Approximately how much time did you spend on this problem set: ANSWER HERE
- What, if anything, gave you the most trouble: ANSWER HERE


**Please seek out help when you need it,** and remember the [15-minute rule](https://mam2022.netlify.app/syllabus/#the-15-minute-rule){target=_blank}. You know enough R (and have enough examples of code from class and your readings) to be able to do this. If you get stuck, ask for help from others, post a question on Slack-- and remember that I am here to help too!  

> As a true test to yourself, do you understand the code you submitted and are you able to explain it to someone else? 


# Rubric

Check minus (1/5): Displays minimal effort. Doesn't complete all components. Code is poorly written and not documented. Uses the same type of plot for each graph, or doesn't use plots appropriate for the variables being analyzed. 

Check (3/5): Solid effort. Hits all the elements. No clear mistakes. Easy to follow (both the code and the output). 

Check plus (5/5): Finished all components of the assignment correctly and addressed both challenges. Code is well-documented (both self-documented and with additional comments as necessary). Used tidyverse, instead of base R. Graphs and tables are properly labelled. Analysis is clear and easy to follow, either because graphs are labeled clearly or you've written additional text to describe how you interpret the output.



