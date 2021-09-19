---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: Lorem Etiam Nullam
draft: false
image: pic09.jpg
keywords: ""
slug: homeworktwo
title: Magna
---

---
title: "AM01 Homework 2: MAM Study Group 9"
author: "Aman Sharma, Stephen Zhu, Vera Meanti, Rishabh Kaushik, Sarah Wu, Ziyue Chen"
date: "`r Sys.Date()`"
output:
  html_document:
    theme: flatly
    highlight: zenburn
    number_sections: yes
    toc: yes
    toc_float: yes
    code_folding: show
---


```{r, setup, include=FALSE}
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


```{r load-libraries, include=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(httr)
library(readxl)
library(vroom)
```

# Climate change and temperature anomalies 

If we wanted to study climate change, we can find data on the *Combined Land-Surface Air and Sea-Surface Water Temperature Anomalies* in the Northern Hemisphere at [NASA's Goddard Institute for Space Studies](https://data.giss.nasa.gov/gistemp). The [tabular data of temperature anomalies can be found here](https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.txt)

To define temperature anomalies you need to have a reference, or base, period which NASA clearly states that it is the period between 1951-1980.

Run the code below to load the file:

```{r weather_data, cache=TRUE}

weather <- 
  read_csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.csv", 
           skip = 1, 
           na = "***")

```

Two objectives for this section:

1. Select the year and the twelve month variables from the `weather` dataset. We do not need the others (J-D, D-N, DJF, etc.) for this assignment. Hint: use `select()` function.

1. Convert the dataframe from wide to 'long' format. Hint: use `gather()` or `pivot_longer()` function. Name the new dataframe as `tidyweather`, name the variable containing the name of the month as `month`, and the temperature deviation values as `delta`.

```{r tidyweather}

tidyweather <- weather %>%
  select(1:13) %>%
pivot_longer(cols=c(2:13), names_to = "month", values_to = "delta")
tidyweather

```

## Plotting Information

Let us plot the data using a time-series scatter plot, and add a trendline. To do that, we first need to create a new variable called `date` in order to ensure that the `delta` values are plot chronologically. 

```{r scatter_plot}

tidyweather <- tidyweather %>%
  rename(year = Year) %>%
  mutate(date = ymd(paste(as.character(year), month, "1")),
         month = month(date, label=TRUE))
tidyweather

weather_plot <- ggplot(tidyweather, aes(x=date, y = delta))+
  geom_point()+
  geom_smooth(color="red") +
  theme_bw() +
  labs (
    title = "Change in Temperature Deviation over time", x="Date", y="Temperature Deviation")
weather_plot

```

Is the effect of increasing temperature more pronounced in some months? Use `facet_wrap()` to produce a seperate scatter plot for each month, again with a smoothing line. Your chart should human-readable labels; that is, each month should be labeled "Jan", "Feb", "Mar" (full or abbreviated month names are fine), not `1`, `2`, `3`. 

```{r facet_wrap}

plot_bymonth <- weather_plot + facet_wrap(~month)
plot_bymonth

```


```{r intervals}

# Organising the data into type periods and filtering

comparison <- tidyweather %>% 
  filter(year>= 1881) %>%     #remove years prior to 1881
  #create new variable 'interval', and assign values based on criteria below:
  mutate(interval = case_when(
    year %in% c(1881:1920) ~ "1881-1920",
    year %in% c(1921:1950) ~ "1921-1950",
    year %in% c(1951:1980) ~ "1951-1980",
    year %in% c(1981:2010) ~ "1981-2010",
    TRUE ~ "2011-present"
  ))
comparison

```

```{r density_plot}

# Creating a density plot grouped by time periods

ggplot(comparison, aes(x=delta, fill=interval))+
  geom_density(alpha=0.2) +   #density plot with tranparency set to 20%
  theme_bw() +                #theme
  labs (
    title = "Density Plot for Monthly Temperature Anomalies",
    y     = "Density",        #changing y-axis label to sentence case
    x = "Temperature Deviation"
  )

```

So far, we have been working with monthly anomalies. However, we might be interested in average annual anomalies. We can do this by using `group_by()` and `summarise()`, followed by a scatter plot to display the result. 

```{r averaging}

#creating yearly averages
average_annual_anomaly <- tidyweather %>% 
  group_by(year) %>%   #grouping data by Year
  
  # creating summaries for mean delta 
  # use `na.rm=TRUE` to eliminate NA (not available) values 
  summarise('average_annual_delta'= mean(delta,na.rm=TRUE))

#plotting the data:
ggplot(average_annual_anomaly, aes(x=year, y= average_annual_delta))+
  geom_point() +
  
  #Fit the best fit line, using LOESS method
  geom_smooth() +
  
  #change to theme_bw() to have white background + black frame around plot
  theme_bw() +
  labs (
    title = "Average Yearly Anomaly",
    y     = "Average Annual Delta"
  )

NULL


```


## Confidence Interval for `delta`

[NASA points out on their website](https://earthobservatory.nasa.gov/world-of-change/decadaltemp.php) that 

> A one-degree global change is significant because it takes a vast amount of heat to warm all the oceans, atmosphere, and land by that much. In the past, a one- to two-degree drop was all it took to plunge the Earth into the Little Ice Age.

Your task is to construct a confidence interval for the average annual delta since 2011, both using a formula and using a bootstrap simulation with the `infer` package. Recall that the dataframe `comparison` has already grouped temperature anomalies according to time intervals; we are only interested in what is happening  between 2011-present.

```{r, calculate_CI_using_formula}

formula_ci <- average_annual_anomaly %>% 
  dplyr::filter(year >= 2011)
formula_ci

formula_ci %>%
  summarise(mean.aad = mean(average_annual_delta, na.rm = TRUE),
    sd.aad = sd(average_annual_delta, na.rm = TRUE),
            n.aad = n()) %>%
  mutate(se.aad = sd.aad / sqrt(n.aad),
         lower.ci.aad = mean.aad - qt(1 - (0.05 / 2), n.aad - 1) * se.aad,
         upper.ci.aad = mean.aad + qt(1 - (0.05 / 2), n.aad - 1) * se.aad)

formula_ci
```


```{r, calculate_CI_using_bootstrap}

# use the infer package to construct a 95% CI for delta
library(infer)

set.seed(1234)
bootstrap_CI <- average_annual_anomaly %>%
  filter(year >= 2011) %>%
  specify(response = average_annual_delta) %>%
  generate(reps=1000, type = "bootstrap") %>%
  calculate(stat="mean")
bootstrap_CI

percentile_ci <- bootstrap_CI %>%
  get_confidence_interval(level=0.95,type = "percentile")
percentile_ci

```

> What is the data showing us? Please type your answer after (and outside!) this blockquote. You have to explain what you have done, and the interpretation of the result. One paragraph max, please!

Since 2011, we are 95% confident that the true average annual delta is between 0.95 and 1.18.
Also, we can see that for the 1st method (formula) we have used a relatively small sample size and this generated a relatively wider confidence interval than for the 2nd method (bootstrap).
For the bootstrap, we used 1000 reps and therefore the confidence interval was narrower.

# General Social Survey (GSS)

The [General Social Survey (GSS)](http://www.gss.norc.org/) gathers data on American society in order to monitor and explain trends in attitudes, behaviours, and attributes. Many trends have been tracked for decades, so one can see the evolution of attitudes, etc in American Society.

In this assignment we analyze data from the **2016 GSS sample data**, using it to estimate values of *population parameters* of interest about US adults. The GSS sample data file has 2867 observations of 935 variables, but we are only interested in very few of these variables and you are using a smaller file.


```{r, read_gss_data, cache=TRUE}
gss <- read_csv(here::here("data", "smallgss2016.csv"), 
                na = c("", "Don't know",
                       "No answer", "Not applicable"))

glimpse(gss)
skim(gss)
```

You will also notice that many responses should not be taken into consideration, like "No Answer", "Don't Know", "Not applicable", "Refused to Answer".

We will be creating 95% confidence intervals for population parameters. The variables we have are the following:

- hours and minutes spent on email weekly. The responses to these questions are recorded in the `emailhr` and `emailmin` variables. For example, if the response is 2.50 hours, this would be recorded as emailhr = 2 and emailmin = 30.
- `snapchat`, `instagrm`, `twitter`: whether respondents used these social media in 2016
- `sex`: Female - Male
- `degree`: highest education level attained

```{r}

# Combining the hours and minutes variables 

gss_hrmin <- gss %>%
  mutate(emailhr = as.numeric(emailhr)) %>%
mutate(emailmin = as.numeric(emailmin)) %>%
  mutate_all(funs(replace(.,is.na(.),0))) %>%
   mutate(emailhrmin = (emailhr + (emailmin / 60)))
gss_hrmin
         
# Calculating the confidence interval for the combined variable

formula_emails_ci <- gss_hrmin %>%
  summarise(mean.hrmin = mean(emailhrmin, na.rm = TRUE),
    sd.hrmin = sd(emailhrmin, na.rm = TRUE),
            n.hrmin = n()) %>%
  mutate(se.hrmin = sd.hrmin / sqrt(n.hrmin),
         lower.ci.hrmin = mean.hrmin - qt(1 - (0.05 / 2), n.hrmin - 1) * se.hrmin,
         upper.ci.hrmin = mean.hrmin + qt(1 - (0.05 / 2), n.hrmin - 1) * se.hrmin)

formula_emails_ci



```

```{r}

gss_socialmedia3 <- gss %>%
mutate(snapchat = as.numeric(snapchat)) %>%
mutate(instagrm = as.numeric(instagrm)) %>%
mutate(twitter=as.numeric(twitter))
gss_socialmedia3


gss %>% 
  filter(!snapchat == "NA") %>% 
  dplyr::group_by(sex) %>% 
  count(snapchat) %>% 
  mutate(prop = n/sum(n))

```

We are finding the confidence interval for the difference in proportion of males and females who use social media.

To calculate this we will use the formula for the standard error of the differences in proportions.

```{r}

male_prop_yes <- 0.2172471
male_prop_no <- 1 - male_prop_yes
female_prop_yes <- 0.2340702
female_prop_no <- 1 - female_prop_yes
propdiff <- male_prop_yes - female_prop_yes

sqrt( (male_prop_yes*male_prop_no)/(472+131) + (female_prop_yes*female_prop_no)/(589+180) )

# The standard error is 0.02269678
se_socialmedia <- 0.02269678

# The lower bound:
  CI_lowerbound <- propdiff - 1.96 * se_socialmedia
  CI_upperbound <- propdiff + 1.96 * se_socialmedia

  CI_lowerbound
  CI_upperbound
    
```


## Instagram and Snapchat, by sex

Can we estimate the *population* proportion of Snapchat or Instagram users in 2016?

1. Create a  new variable, `snap_insta` that is *Yes* if the respondent reported using any of Snapchat (`snapchat`) or Instagram (`instagrm`), and *No* if not. If the recorded value was NA for both of these questions, the value in your new variable should also be NA.
1. Calculate the proportion of Yes’s for `snap_insta` among those who answered the question, i.e. excluding NAs.
1. Using the CI formula for proportions, please construct 95% CIs for men and women who used either Snapchat or Instagram

```{r}

# Creating the new variables
socialmediausage <- gss %>%
  mutate(snaporinsta = case_when(snapchat == "Yes" & instagrm == "Yes" ~ "Yes", snapchat == "Yes" | instagrm == "Yes" ~ "Yes", snapchat == "No" & instagrm == "No" ~ "No", snapchat == "NA" & instagrm == "NA" ~ "NA" ))

# Calculating the proportions of Yes and those who answered
share_yes <- socialmediausage %>% 
  count(snaporinsta == "Yes") %>% 
  mutate(n/sum(n)*100)

share_yes_no <- socialmediausage %>%
  filter(snaporinsta != "NA")

share_yes_no %>% 
  count(snaporinsta == "Yes") %>% 
  mutate(n/sum(n)*100)

# Social media usage
socialmediastats <- socialmediausage %>%
  group_by(snaporinsta) %>%
  count(snaporinsta) %>%
  pivot_wider(names_from = snaporinsta, values_from = n) %>%
  mutate(proportion_yes = Yes/(Yes+No))

socialmediastats

# 95% confidence interval for social media usage
ci_socmedia <- socialmediausage %>%
  group_by(sex, snaporinsta) %>%
  count(snaporinsta) %>%
  pivot_wider(names_from = snaporinsta, values_from = n) %>%
  mutate(mean = Yes/(Yes+No), se = sqrt(mean*(1-mean)/(Yes+No)), t_critical = qt(0.975, (Yes+No)-1), lower = mean - t_critical*se, upper = mean + t_critical*se)

ci_socmedia

# Raw number of men and women using social media

ci_gender <- socialmediausage %>%
  group_by(sex, snaporinsta) %>%
  count(snaporinsta)

ci_gender1 <- socialmediausage %>%
  group_by(sex, snaporinsta) %>%
  count(snaporinsta) %>%
  pivot_wider(names_from = snaporinsta, values_from = n)

ci_gender1
```

The confidence intervals for men and women do not overlap. We can reject the null hypothesis that these two genders have the same levels of average social media usage, at the 95% significance level.

## Twitter, by education level

Can we estimate the *population* proportion of Twitter users by education level in 2016?. 

There are 5 education levels in variable `degree` which, in ascneding order of years of education, are Lt high school, High School, Junior college, Bachelor, Graduate. 

1. Turn `degree` from a character variable into a factor variable. Make sure the order is the correct one and that levels are not sorted alphabetically which is what R by default does. 
1. Create a  new variable, `bachelor_graduate` that is *Yes* if the respondent has either a `Bachelor` or `Graduate` degree. As before, if the recorded value for either was NA, the value in your new variable should also be NA.
1. Calculate the proportion of `bachelor_graduate` who do (Yes) and who don't (No) use twitter. 
1. Using the CI formula for proportions, please construct two 95% CIs for `bachelor_graduate` vs whether they use (Yes) and don't (No) use twitter. 
1. Do these two Confidence Intervals overlap?

```{r}

# Transforming 'degree' in to a factor variable and non alphabetical
twitter_users <- gss %>%
  group_by(degree) %>%
  count(twitter)
twitter_users

level_order <- c("Lt high school", "High school", "Junior college", "Bachelor", "Graduate")

twitter_count <- gss %>% 
  group_by(degree) %>%
  mutate(degree = factor(degree, levels = level_order))

# Creating new variable bachelor_graduate
twitter_edu <- twitter_count %>%
  mutate(bachelor_graduate = case_when(degree == "Bachelor" | degree == "Graduate" ~ "Yes", degree == "High school" | degree == "Junior college" | degree == "Lt high school" ~ "No", degree == "NA" ~ "NA" ))

# Proportion of bachelor_graduate
twitter_data <- twitter_edu %>%
  group_by(bachelor_graduate, twitter) %>%
  count(bachelor_graduate, twitter) %>%
  filter(bachelor_graduate == "Yes") %>%
  pivot_wider(names_from = twitter, values_from = n) %>%
  mutate(twitter_yes = Yes/(Yes+No))

twitter_data_set <- twitter_edu %>%
  group_by(bachelor_graduate, twitter) %>%
  filter(bachelor_graduate == "Yes", twitter != "NA") %>%
  summarise(count = n()) %>% 
  mutate(per_tw = count/sum(count))

twitter_data_set

# Confidence intervals with 95% of Yes and No, and summary

twitter_CI_yes <- twitter_edu %>%
  group_by(bachelor_graduate, twitter) %>%
  count(bachelor_graduate, twitter) %>%
  pivot_wider(names_from = twitter, values_from = n) %>%
  mutate(mean = Yes/(Yes+No), se = sqrt(mean*(1-mean)/(Yes+No)), t_critical = qt(0.975, (Yes+No)-1), lower = mean - t_critical*se, upper = mean + t_critical*se)

twitter_CI_no <- twitter_edu %>%
  group_by(bachelor_graduate, twitter) %>%
  count(bachelor_graduate, twitter) %>%
  pivot_wider(names_from = twitter, values_from = n) %>%
  mutate(mean = No/(Yes+No), se = sqrt(mean*(1-mean)/(Yes+No)), t_critical = qt(0.975, (Yes+No)-1), lower = mean - t_critical*se, upper = mean + t_critical*se)

twitter_CI_yes

twitter_CI_no

twitter_CI_summary <- twitter_data_set %>%
  summarise(count, per_tw, se = sqrt((per_tw*(1-per_tw)/sum(count))), t_critical = qt(0.975, count-1), margin_of_error = t_critical * se, lower = per_tw - margin_of_error, higher = per_tw + margin_of_error) 

twitter_CI_summary
```

**Overlap?**
We can see the confidence intervals do not overlap. What this means is that we are 95% confident that the population parameters are different from each other.

## Email usage

Can we estimate the *population* parameter on time spent on email weekly?

1. Create a new variable called `email` that combines `emailhr` and `emailmin` to reports the number of minutes the respondents spend on email weekly.
1. Visualise the distribution of this new variable. Find the mean and the median number of minutes respondents spend on email weekly. Is the mean or the median a better measure of the typical amoung of time Americans spend on email weekly? Why?
1. Using the `infer` package, calculate a 95% bootstrap confidence interval for the mean amount of time Americans spend on email weekly. Interpret this interval in context of the data, reporting its endpoints in “humanized” units (e.g. instead of 108 minutes, report 1 hr and 8 minutes). If you get a result that seems a bit odd, discuss why you think this might be the case.
1. Would you expect a 99% confidence interval to be wider or narrower than the interval you calculated above? Explain your reasoning.

```{r}
# Creating email usage variable
email_usage <- gss
  
email_usage[email_usage == "NA"] <- NA 

email_visualise <- email_usage %>% 
  na.omit() %>% 
  mutate(emailmin = as.integer(emailmin),
         emailhr = as.integer(emailhr),
         email = emailmin + emailhr*60)

# Visualizing the distribution of new variable

ggplot(data = email_visualise, aes(x = email)) + geom_boxplot() +
  labs(title = "Email Usage Distribution (Weekly)", x = "Minutes per week", y = "") +
  NULL

ggplot(data = email_visualise, aes(x = email)) + geom_histogram() + labs(title = "Email Usage Distribution (Weekly)", x = "Minutes per week", y = "Number of people") +
NULL
                        
# 95% CI bootstrap for mean time Americans spend on email weekly
library(infer)

email_95 <- email_visualise %>%
  specify(response = email) %>%
  generate (reps = 1000, type = "bootstrap") %>%
  calculate (stat = "mean")

email_95_ci <- email_95 %>%
    get_confidence_interval(level = 0.95, type = "percentile")

email_95_ci

# 99% CI bootstrap for mean time Americans spend on email weekly
email_99 <- email_visualise %>%
  specify(response = email) %>%
  generate (reps = 1000, type = "bootstrap") %>%
  calculate (stat = "mean")

email_99_ci <- email_99 %>%
    get_confidence_interval(level = 0.99, type = "percentile")

email_99_ci

```

**Do we expect the 99% interval to be wider or narrower than the 95%?**
I expect a 99% confidence interval to be wider because the t critical value that we multiply the standard error by would be larger. the 95% confidence interval is a subset of the 99% interval. As we expand the interval, we are 'more sure' that the true population parameter lies in this range.

# Biden's Approval Margins

As we saw in class, fivethirtyeight.com has detailed data on [all polls that track the president's approval ](https://projects.fivethirtyeight.com/biden-approval-ratings)

```{r, cache=TRUE}
# Import approval polls data directly off fivethirtyeight website
approval_polllist <- read_csv('https://projects.fivethirtyeight.com/biden-approval-data/approval_polllist.csv') 

# Use `lubridate` to fix dates, as they are given as characters.
approval_polllist <- approval_polllist %>% 
  mutate(modeldate = mdy(modeldate),
         startdate = mdy(startdate),
         enddate = mdy(enddate),
         createddate = mdy(createddate),
         timestamp = hms(timestamp))

glimpse(approval_polllist)
skim(approval_polllist)
```

## Create a plot

What I would like you to do is to calculate the average net approval rate (approve- disapprove) for each week since he got into office. I want you plot the net approval, along with its 95% confidence interval. There are various dates given for each poll, please use `enddate`, i.e., the date the poll ended.

Also, please add an orange line at zero. Your plot should look like this:

```{r}
# Calculate the weekly net approval rate, along with sample size and standard deviation
approval_byweek <- approval_polllist %>%
  mutate(net_approval = approve - disapprove,
         week = week(enddate),
         year = year(enddate)) %>% 
  group_by(week) %>% 
  summarise(net_approval_mean = mean(net_approval),
            n = n(),
            sd = sd(net_approval))

# Create the line plot with 95% confidence interval surrounding it
approval_byweek %>%  
  ggplot(aes(x = week, y = net_approval_mean)) +
  geom_ribbon(aes(ymin = net_approval_mean - qt(0.975,df = n-1)*sd/sqrt(n),
              ymax = net_approval_mean + qt(0.975, df = n-1)*sd/sqrt(n)),
              fill = 'grey90',color = 'orangered') +
  geom_point(color = 'orange') +
  geom_line(color = 'orange') +
  geom_smooth(se = F) +
  geom_line(aes(x=week,y=0),color = 'orange', size = 2) +
  labs(title = 'Estimating Weekly Net Approval Margin for Joe Biden',
       x = 'Week of Year 2021',
       y = 'Average Net Approval Margin(%)') +
  theme_bw()
  NULL
```


```{r trump_margins, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "biden_approval_margin.png"), error = FALSE)
```

## Compare Confidence Intervals

Compare the confidence intervals for `week 3` and `week 25`. Can you explain what's going on? One paragraph would be enough.

Week 3 has an extremely large confidence interval whereas week 25 a very small one. This occurred likely because of the difference in sample sizes in the two weeks. On his first week, Biden received very few polls so there 95% confidence interval for the true approval rate is very large; on the other hand, he may have received a lot more polls on week 25 so the 95% confidence interval computed is much narrower.

# Gapminder revisited

Recall the `gapminder` data frame from the gapminder package. That data frame contains just six columns from the larger [data in Gapminder World](https://www.gapminder.org/data/). In this part, you will join a few dataframes with more data than the 'gapminder' package. Specifically, you will look at data on 


- Life expectancy at birth (life_expectancy_years.csv)
- GDP per capita in constant 2010 US$ (https://data.worldbank.org/indicator/NY.GDP.PCAP.KD)
- Female fertility: The number of babies per woman (https://data.worldbank.org/indicator/SP.DYN.TFRT.IN)
- Primary school enrollment as % of children attending primary school (https://data.worldbank.org/indicator/SE.PRM.NENR)
- Mortality rate, for under 5, per 1000 live births (https://data.worldbank.org/indicator/SH.DYN.MORT)
- HIV prevalence (adults_with_hiv_percent_age_15_49.csv): The estimated number of people living with HIV per 100 population of age group 15-49.

You must use the `wbstats` package to download data from the World Bank. The relevant World Bank indicators are `SP.DYN.TFRT.IN`, `SE.PRM.NENR`, `NY.GDP.PCAP.KD`, and `SH.DYN.MORT`

```{r, get_data, cache=TRUE}

# load gapminder HIV data
hiv <- read_csv(here::here("data","adults_with_hiv_percent_age_15_49.csv"))
life_expectancy <- read_csv(here::here("data","life_expectancy_years.csv"))

# get World bank data using wbstats
indicators <- c("SP.DYN.TFRT.IN","SE.PRM.NENR", "SH.DYN.MORT", "NY.GDP.PCAP.KD")


library(wbstats)

worldbank_data <- wb_data(country="countries_only", #countries only- no aggregates like Latin America, Europe, etc.
                          indicator = indicators, 
                          start_date = 1960, 
                          end_date = 2016)

# get a dataframe of information regarding countries, indicators, sources, regions, indicator topics, lending types, income levels,  from the World Bank API 
countries <-  wbstats::wb_cachelist$countries

glimpse(hiv)

clean_hiv <- pivot_longer(hiv, 2:34, names_to="year", values_to="hiv_prevalence",values_drop_na= FALSE)
glimpse(clean_hiv)

glimpse(life_expectancy)
clean_lifeexp <-  pivot_longer(life_expectancy, 2:302, names_to="year", values_to="life_exp", values_drop_na= FALSE)
glimpse(clean_lifeexp)

```

You have to join the 3 dataframes (life_expectancy, worldbank_data, and HIV) into one. You may need to tidy your data first and then perform [join operations](http://r4ds.had.co.nz/relational-data.html). Think about what type makes the most sense **and explain why you chose it**.

1. What is the relationship between HIV prevalence and life expectancy? Generate a scatterplot with a smoothing line to report your results. You may find faceting useful

```{r}

rel_hiv_lifeexp <- inner_join(clean_hiv,clean_lifeexp) 
ggplot(rel_hiv_lifeexp, aes(x=hiv_prevalence, y=life_exp)) +
  geom_point() + 
  geom_smooth() +
  theme_bw() +
  ylim(c(40,85)) + 
  labs( 
    title = "Relationship between HIV prevalence and life expectancy",
    x = "Number of people with Hiv per 100 population of age 15-49",
    y = "Life Expectancy at birth"
    ) + 
  NULL


```

1. What is the relationship between fertility rate and GDP per capita? Generate a scatterplot with a smoothing line to report your results. You may find facetting by region useful

It looks like in most regions the higher the GDP per Capita, the lower the number of babies per women.

```{r}
library(countrycode)
worldbank_data_clean <- worldbank_data %>%
  rename(GDP_per_capita = NY.GDP.PCAP.KD,
         female_fertility = SP.DYN.TFRT.IN,
         primary_school_enrollment = SE.PRM.NENR,
         mortality_rate = SH.DYN.MORT) %>%
  mutate(region= countrycode(sourcevar = country, origin = "country.name",destination = "region"))

skim(worldbank_data_clean)

fertility_per_GDP <- select(worldbank_data_clean,country,GDP_per_capita,female_fertility,region)
  ggplot(fertility_per_GDP, aes(x=female_fertility, y=GDP_per_capita))+
    geom_point()+
    geom_smooth()+
    theme_bw() +
    ylim(c(0,150000)) +
     scale_y_log10() +
    labs(
       title = "Relationship between fertility rate and GDP per capita",
    x = "Female Fertility rate (number of babies per woman)",
    y = "GDP per capita"
    ) +
    facet_wrap(~region)
  NULL
```

1. Which regions have the most observations with missing HIV data? Generate a bar chart (`geom_col()`), in descending order.

The region with the most missing HIV data is sub-saharan africa. 
```{r}
hiv_data <- clean_hiv %>% 
  mutate(region = countrycode(sourcevar = country, origin = "country.name",destination = "region")) %>%
  filter(is.na(hiv_prevalence)) %>%
               group_by(region) %>% 
               count() %>%
               arrange(desc(n))

ggplot(hiv_data, aes(x= reorder(region, -n), y= n)) +
  geom_col() +
  labs (y="Missing HIV data", x="Regions",
        title="Regions with missing HIV data",)
  NULL


```

1. How has mortality rate for under 5 changed by region? In each region, find the top 5 countries that have seen the greatest improvement, as well as those 5 countries where mortality rates have had the least improvement or even deterioration.

There are some limitations to this analysis as a lot of data is missing for this time frame. 

```{r}

mortality_data <- select(worldbank_data_clean, mortality_rate,country,region,date)  %>%
        filter(date== "1960"| date=="2016") %>%
  pivot_wider(names_from = "date", 
              values_from = "mortality_rate")

colnames(mortality_data) = c("country","region","start_date","end_date")

change_mortality <- mortality_data %>%
  mutate(change=(end_date-start_date)) %>% 
  group_by(region) %>%
  summarize(country,change) %>%
  arrange(region,desc(change))
  
  change_mortality
  
  mortality_top_5 <- change_mortality %>%
      arrange(desc(change)) %>%
      group_by(region)
      head(mortality_top_5,5) 
    mortality_top_5
      
  mortality_bottom_5 <- change_mortality %>%
      arrange(change) %>%
      group_by(region)
       head(mortality_bottom_5,5)
  
  ggplot(mortality_top_5, aes(x=country, y=change)) + 
    geom_col() + 
    labs(title="Mortality rate",
       x="Country",
       y="Mortality improvement" ) +
    facet_wrap(~region)
  
   ggplot(mortality_bottom_5, aes(x=country, y=change)) + 
    geom_col() + 
    labs(title="Mortality rate",
       x="Country",
       y="Mortality improvement" ) + 
     facet_wrap(~region)
  
   NULL



```

1. Is there a relationship between primary school enrollment and fertility rate?

The graph shows that there is a relationship between primary school enrollment and the number of babies per woman, as we can see the percentage of children that go to primary school is higher in the countries were the number of babies per woman is lower .

```{r}

rel_primschool_fertility <- select(worldbank_data_clean,primary_school_enrollment, female_fertility)
ggplot(rel_primschool_fertility, aes(x=female_fertility, y=primary_school_enrollment)) + 
  geom_point()+
  geom_smooth()+
  labs(title="Relationship between primary school enrollment and fertility rate",
       x="Fertility Rate (number of babies per woman)",
       y="% of children attending primary school")
NULL

```

# Challenge 1: Excess rentals in TfL bike sharing

Recall the TfL data on how many bikes were hired every single day. We can get the latest data by running the following

```{r, get_tfl_data, cache=TRUE}
url <- "https://data.london.gov.uk/download/number-bicycle-hires/ac29363e-e0cb-47cc-a97a-e216d900a6b0/tfl-daily-cycle-hires.xlsx"

# Download TFL data to temporary file
httr::GET(url, write_disk(bike.temp <- tempfile(fileext = ".xlsx")))

# Use read_excel to read it as dataframe
bike0 <- read_excel(bike.temp,
                   sheet = "Data",
                   range = cell_cols("A:B"))

# change dates to get year, month, and week
bike <- bike0 %>% 
  clean_names() %>% 
  rename (bikes_hired = number_of_bicycle_hires) %>% 
  mutate (year = year(day),
          month = lubridate::month(day, label = TRUE),
          week = isoweek(day))
```



We can easily create a facet grid that plots bikes hired by month and year.

```{r tfl_month_year_grid, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "tfl_distributions_monthly.png"), error = FALSE)
```

Look at May and Jun and compare 2020 with the previous years. What's happening?

However, the challenge I want you to work on is to reproduce the following two graphs.

```{r tfl_absolute_monthly_change, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "tfl_monthly.png"), error = FALSE)
```

The second one looks at percentage changes from the expected level of weekly rentals. The two grey shaded rectangles correspond to Q2 (weeks 14-26) and Q4 (weeks 40-52).

```{r tfl_percentage_change, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "tfl_weekly.png"), error = FALSE)
```

For both of these graphs, you have to calculate the expected number of rentals per week or month between 2016-2019 and then, see how each week/month of 2020-2021 compares to the expected rentals. Think of the calculation `excess_rentals = actual_rentals - expected_rentals`. 

Should you use the mean or the median to calculate your expected rentals? Why?

In creating your plots, you may find these links useful:

- https://ggplot2.tidyverse.org/reference/geom_ribbon.html
- https://ggplot2.tidyverse.org/reference/geom_tile.html 
- https://ggplot2.tidyverse.org/reference/geom_rug.html

```{r}
# Compute the mean of number of bikes hired for each month during the time period 2016-19
bike_1619_mean <- 
  bike %>% 
  filter(year == c(2016:2019)) %>% 
  group_by(month) %>% 
  summarise(mean_1619 = mean(bikes_hired))

# Compute mean of number of bikes hired for each month in each year during the time period 2016-21
bike_monthly <-
  bike %>% 
  filter(year %in% c(2016:2021)) %>% 
  group_by(month,year) %>% 
  summarise(mean = mean(bikes_hired)) %>% 
  #mutate(delta = bikes_hired - mean) %>% 
  arrange(year,month)

# Join the two datasets to create dataset used in plotting, and add some features needed
bike_monthly_plot <- left_join(bike_monthly,bike_1619_mean,by = 'month') %>% 
  mutate(increase = ifelse(mean > mean_1619,mean-mean_1619,0),
         decrease = ifelse(mean < mean_1619,mean-mean_1619,0))

# Create the plot
bike_monthly_plot %>% 
  ggplot(aes(x = month)) +
  geom_ribbon(aes(ymin = mean_1619,
                  ymax = mean_1619 + increase),
                  fill = 'lightgreen',
              group=1) +
  geom_ribbon(aes(ymin = mean_1619 + decrease,ymax=mean_1619),fill = 'salmon',group=1) +
  geom_line(aes(y = mean_1619, group=1),color = 'blue',size = 1) +
  geom_line(aes(y = mean, group=1), color = 'grey70') +
  facet_wrap(~year) +
  labs(title = 'Monthly Changes in Tfl Bike Rentals', subtitle = 'Blue line represents monthly average from 2016-19', y = 'Bike Rentals', x = 'Month') +
  theme_bw() +
  NULL

```


The second one looks at percentage changes from the expected level of weekly rentals. The two grey shaded rectangles correspond to Q2 (weeks 14-26) and Q4 (weeks 40-52).

```{r tfl_percent_change, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "tfl_weekly.png"), error = FALSE)
```

```{r}
# Compute the mean of number of bikes hired for each week during the time period 2016-19
bike_1619_mean_weekly <- 
  bike %>% 
  filter(year == c(2016:2019)) %>% 
  group_by(week) %>% 
  summarise(mean_1619 = mean(bikes_hired))

# Compute mean of number of bikes hired for each week in each year during the time period 2016-21
bike_weekly <-
  bike %>% 
  filter(year %in% c(2016:2021)) %>% 
  group_by(week,year) %>% 
  summarise(mean = mean(bikes_hired)) %>% 
  #mutate(delta = bikes_hired - mean) %>% 
  arrange(year,week)

# Join the two datasets to create dataset used in plotting, and add some features needed
bike_weekly_plot <- left_join(bike_weekly,bike_1619_mean_weekly,by = 'week') %>% 
  mutate(percent_change = (mean - mean_1619)/mean_1619*100,
         percent_increase = ifelse(percent_change>0,percent_change,0),
         percent_decrease = ifelse(percent_change<0,percent_change,0),
         )

# create the plot
bike_weekly_plot %>% 
  ggplot(aes(x = week, y = percent_change)) +
  geom_rect(aes(xmin = 14,xmax = 26, ymin = -75, ymax = 125), fill = 'grey90') +
  geom_rect(aes(xmin = 40,xmax = 52, ymin = -75, ymax = 125), fill = 'grey90') +
  geom_line(groups=1) +
  geom_area(aes(y = percent_increase),fill='lightgreen') +
  geom_area(aes(y = percent_decrease),fill='salmon') +
  geom_rug(sides = 'b',color = ifelse(bike_weekly_plot$percent_change>0,'lightgreen','salmon')) +
  labs(title = 'Weekly Changes in Tfl Bike Rentals', subtitle = '% change from 2016-19 weekly averages', x = 'Week',y = 'Percentage Change (%)') +
  facet_wrap(~year) +
  theme_bw() +
  NULL

  
```


# Details

- Who did you collaborate with: Study Group 9
- Approximately how much time did you spend on this problem set: 6
- What, if anything, gave you the most trouble: Challenge 1 

> As a true test to yourself, do you understand the code you submitted and are you able to explain it to someone else? 

# Rubric

Check minus (1/5): Displays minimal effort. Doesn't complete all components. Code is poorly written and not documented. Uses the same type of plot for each graph, or doesn't use plots appropriate for the variables being analyzed. 

Check (3/5): Solid effort. Hits all the elements. No clear mistakes. Easy to follow (both the code and the output). 

Check plus (5/5): Finished all components of the assignment correctly and addressed both challenges. Code is well-documented (both self-documented and with additional comments as necessary). Used tidyverse, instead of base R. Graphs and tables are properly labelled. Analysis is clear and easy to follow, either because graphs are labeled clearly or you've written additional text to describe how you interpret the output.