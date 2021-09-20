---
categories:
- ""
- ""
date: "2017-10-31T22:26:13-05:00"
description: Nullam et orci eu lorem consequat tincidunt vivamus et sagittis magna
  sed nunc rhoncus condimentum sem. In efficitur ligula tate urna. Maecenas massa
  sed magna lacinia magna pellentesque lorem ipsum dolor. Nullam et orci eu lorem
  consequat tincidunt. Vivamus et sagittis tempus.
draft: false
image: pic08.jpg
keywords: ""
slug: homeworkone
title: Tempus
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

```{r load-libraries, warning=FALSE, message=FALSE, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(fivethirtyeight)
library(here)
library(skimr)
library(janitor)
library(vroom)
library(tidyquant)
library(rvest) # to scrape wikipedia page
```

# Where Do People Drink The Most Beer, Wine And Spirits?

```{r, load_alcohol_data}
library(fivethirtyeight)
data(drinks)

```

What are the variable types? Any missing values we should worry about? 

```{r glimpse_skim_data}
# Briefly view the dataset with glimpse() and check for missing values using skim(), same applies for use of these two
# functions below
glimpse(drinks)
skim(drinks)

```
All variables are numbers (integer or double) apart from the country which is character and there are no missing values in the dataset.

Make a plot that shows the top 25 beer consuming countries

```{r beer_plot}

drinks %>%
  slice_max(order_by = beer_servings, n=25) %>% 
  ggplot(aes(y=reorder(country,beer_servings),x=beer_servings)) +
  geom_col() +
  labs(title = 'Top 25 Beer Consuming Countries', y = 'country') +
  NULL

```

Make a plot that shows the top 25 wine consuming countries

```{r wine_plot}

drinks %>%
  slice_max(order_by = wine_servings, n=25) %>% 
  ggplot(aes(y=reorder(country,wine_servings),x=wine_servings)) +
  geom_col() +
  labs(title = 'Top 25 Wine Consuming Countries', y = 'country') +
  NULL

```

Finally, make a plot that shows the top 25 spirit consuming countries
```{r spirit_plot}

drinks %>%
  slice_max(order_by = spirit_servings, n=25) %>%
  ggplot(aes(y=reorder(country,spirit_servings),x=spirit_servings)) +
  geom_col() +
  labs(title = 'Top 25 Spirit Consuming Countries', y = 'country') +
  NULL

```

What can you infer from these plots? Don't just explain what's in the graph, but speculate or tell a short story (1-2 paragraphs max).

Across all three types of drinks, beer was consumed the most by the top 25 countries compared to spirits and wine. This can be potentially due to the difference in alcohol level in the drinks and price: wine is the most expensive so people drink less even for those who like it a lot, and people are biologically capable of drinking more beer than spirits in general. Another interesting point to note is that the highest consumption for spirit peaks at over 400 servings in Grenada, whereas this number is only around 350 for both wine and beer. Furthermore, the choice of drinks seems to be highly correlated with geography of the countries: most wine lovers are located in Europe, countries that drink beer a lot are mostly situated in Africa, whereas spirit is the favourite for the Caribbean countries. Both weather conditions and cultural traditions can be contributing to these behaviours. Last but not least, we find that most countries are only ranked high in only one, if not none, of the three rankings: likely because that people can only drink so much alcohol everyday.

# Analysis of movies- IMDB dataset

We will look at a subset sample of movies, taken from the [Kaggle IMDB 5000 movie dataset](https://www.kaggle.com/carolzhangdc/imdb-5000-movie-dataset)


```{r,load_movies, warning=FALSE, message=FALSE}
movies <- read_csv(here::here("data", "movies.csv"))
glimpse(movies)

```

Besides the obvious variables of `title`, `genre`, `director`, `year`, and `duration`, the rest of the variables are as follows:

- `gross` : The gross earnings in the US box office, not adjusted for inflation
- `budget`: The movie's budget 
- `cast_facebook_likes`: the number of facebook likes cast memebrs received
- `votes`: the number of people who voted for (or rated) the movie in IMDB 
- `reviews`: the number of reviews for that movie
- `rating`: IMDB average rating 

## Use your data import, inspection, and cleaning skills to answer the following:

- Are there any missing values (NAs)? Are all entries distinct or are there duplicate entries?
```{r}
skim(movies)

```
There are no missing values in the dataset.

- Produce a table with the count of movies by genre, ranked in descending order
```{r}
movies %>% 
  group_by(genre) %>% 
  summarise(movie_count = n())

```

- Produce a table with the average gross earning and budget (`gross` and `budget`) by genre. Calculate a variable `return_on_budget` which shows how many $ did a movie make at the box office for each $ of its budget. Ranked genres by this `return_on_budget` in descending order

```{r}
movies %>% 
  group_by(genre) %>% 
  summarise(mean_gross = mean(gross),
            mean_budget = mean(budget)) %>% 
  mutate(return_on_budget = mean_gross / mean_budget) %>% 
  arrange(desc(return_on_budget))
```

- Produce a table that shows the top 15 directors who have created the highest gross revenue in the box office. Don't just show the total gross amount, but also the mean, median, and standard deviation per director.

```{r}
movies %>% 
  group_by(director) %>% 
  summarise(total_gross = sum(gross),
            mean_gross = mean(gross),
            median_gross = median(gross),
            sd_gross = sd(gross)) %>% 
  slice_max(total_gross, n=15) %>% 
  arrange(desc(total_gross))
```


- Finally, ratings. Produce a table that describes how ratings are distributed by genre. We don't want just the mean, but also, min, max, median, SD and some kind of a histogram or density graph that visually shows how ratings are distributed. 

```{r}

# Creating the variables
movies %>% 
  group_by(genre) %>% 
  summarise(mean_rating = mean(rating),
            min_rating = min(rating),
            max_rating = max(rating),
            median_rating = median(rating),
            sd_rating = sd(rating)) %>% 
  arrange(desc(mean_rating))

# Making the overall histogram
ggplot(movies,aes(x=rating)) +
  geom_histogram() +
  labs(title = 'Overall Ratings Distribution')

# Making histograms for each genre (free scale)
ggplot(movies,aes(x=rating)) +
  geom_histogram() +
  facet_wrap(~genre,scales = 'free') +
  labs(title = 'Ratings Distribution by Genre (free scale)')

# Making histograms for each genre (uniform scale)
ggplot(movies,aes(x=rating)) +
  geom_histogram() +
  facet_wrap(~genre) +
  labs(title = 'Ratings Distribution by Genre (uniform scale)') +
  NULL


```


## Use `ggplot` to answer the following

  - Examine the relationship between `gross` and `cast_facebook_likes`. Produce a scatterplot and write one sentence discussing whether the number of facebook likes that the cast has received is likely to be a good predictor of how much money a movie will make at the box office. What variable are you going to map to the Y- and X- axes?
  
  
```{r, gross_on_fblikes}

# Plotting the scatter plot
ggplot(movies,aes(x = cast_facebook_likes, y = gross)) +
  geom_point() +
  geom_smooth() +
  labs(title = 'Cast_facebook_likes vs. Gross') +
  NULL
  
```
Cast_facebook_likes is used as the independent variable and placed on x-axis, whereas gross is to be predicted and placed on y-axis as dependent variables. We can see that the line of best fit suggests that there is a positive correlation between the two variables when the facebook likes are low. However, this relationship is very **insignificant** and it is inverted after facebook likes exceeds around 300,000, potentially due to presence of outliers with high facebook likes but low gross. Overall facebook likes is not a great predictor for gross.

  - Examine the relationship between `gross` and `budget`. Produce a scatterplot and write one sentence discussing whether budget is likely to be a good predictor of how much money a movie will make at the box office.

```{r, gross_on_budget}

# Plotting the scatter plot
ggplot(movies,aes(x = budget, y = gross)) +
  geom_point() +
  geom_smooth() +
  labs(title = 'Budget vs. Gross') +
  NULL

```
We observe a **relatively strong** positive correlation between the budget and gross of movies. Budget is not the perfect predictor but can predict the gross to a degree.

  - Examine the relationship between `gross` and `rating`. Produce a scatterplot, faceted by `genre` and discuss whether IMDB ratings are likely to be a good predictor of how much money a movie will make at the box office. Is there anything strange in this dataset?

```{r, gross_on_rating}

# Plotting multiple scatter plots by genre
ggplot(movies,aes(x = rating, y = gross)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~genre) +
  labs(title = 'Ratings vs. Gross by Genre') +
  NULL
```
We observe a decent level of positive correlation between rating and gross for most genres with sufficient data points. Thus, rating is a decent predictor to predict the gross of movies overall. However, there are quite a number of genres with insufficient data points and there is one strange thing: the documentary genre shows a negative relationship between rating and gross.

# Returns of financial stocks

> You may find useful the material on [finance data sources](https://mam2022.netlify.app/reference/finance_data/). 

We will use the `tidyquant` package to download historical data of stock prices, calculate returns, and examine the distribution of returns. 

We must first identify which stocks we want to download data for, and for this we must know their ticker symbol; Apple is known as AAPL, Microsoft as MSFT, McDonald's as MCD, etc. The file `nyse.csv` contains 508 stocks listed on the NYSE, their ticker `symbol`, `name`, the IPO  (Initial Public Offering) year, and the sector and industry the company is in.

```{r load_nyse_data, message=FALSE, warning=FALSE}

# Importing and viewing the dataset
nyse <- read_csv(here::here("data","nyse.csv"))
glimpse(nyse)
skim(nyse)

```

Based on this dataset, create a table and a bar plot that shows the number of companies per sector, in descending order

```{r companies_per_sector}

# Arranging the dataset
per_sector <- nyse %>% 
  group_by(sector) %>% 
  count() %>% 
  arrange(desc(n))

# Plotting the column chart
ggplot(per_sector,aes(x=n,y=reorder(sector,n))) +
  geom_col() +
  labs(title = 'Number of Companies by Sector', y = 'sector')
  NULL

```

Next, let's choose some stocks and their ticker symbols and download some data. You **MUST** choose 6 different stocks from the ones listed below; You should, however, add `SPY` which is the SP500 ETF (Exchange Traded Fund).


```{r get_price_data, message=FALSE, warning=FALSE, cache=TRUE}

# Scraping the stock data

myStocks <- c("AAPL","JPM","DPZ","ANF","TSLA","XOM","SPY" ) %>%
  tq_get(get  = "stock.prices",
         from = "2011-01-01",
         to   = "2021-08-31") %>%
  group_by(symbol) 

glimpse(myStocks) # examine the structure of the resulting data frame
```

Financial performance analysis depend on returns; If I buy a stock today for 100 and I sell it tomorrow for 101.75, my one-day return, assuming no transaction costs, is 1.75%. So given the adjusted closing prices, our first step is to calculate daily and monthly returns.


```{r calculate_returns, message=FALSE, warning=FALSE, cache=TRUE}
# Calculating daily returns

myStocks_returns_daily <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily_returns",
               cols = c(nested.col))  

# Calculating monthly  returns

myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 

# Calculating yearly returns

myStocks_returns_annual <- myStocks %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               type       = "arithmetic",
               col_rename = "yearly_returns",
               cols = c(nested.col))

```

Create a table where you summarise monthly returns for each of the stocks and `SPY`; min, max, median, mean, SD.

```{r summarise_monthly_returns}

# Creating summary statistics

myStocks_returns_monthly_summary <- myStocks_returns_monthly %>% 
  group_by(symbol) %>% 
  summarise(min_return = min(monthly_returns),
            max_return = max(monthly_returns),
            median_return = median(monthly_returns),
            mean_return = mean(monthly_returns),
            SD_return = sd(monthly_returns)) %>% 
  arrange(mean_return)

myStocks_returns_monthly_summary

```


Plot a density plot, using `geom_density()`, for each of the stocks
```{r density_monthly_returns}

# Plotting returns for each stock

myStocks_returns_monthly %>% 
  ggplot(aes(x=monthly_returns)) +
  geom_density() +
  facet_wrap(~symbol) +
  labs(title = 'Monthly Return Distribution of Stocks') +
  NULL
```

What can you infer from this plot? Which stock is the riskiest? The least risky?

> TYPE YOUR ANSWER AFTER (AND OUTSIDE!) THIS BLOCKQUOTE.

We can see that all plots peak at a monthly return around zero percent. The more risky the stock is, its peak around zero should be lower and it should have more extreme return values. Thus, the most risky stock is TSLA, and the least risky one is SPY, likely because it is a combination of multiple stocks within a portfolio. In this way, fluctuations for individual stocks will not affect SPY's monthly return too much.

Finally, make a plot that shows the expected monthly return (mean) of a stock on the Y axis and the risk (standard deviation) in the X-axis. Please use `ggrepel::geom_text_repel()` to label each stock

```{r risk_return_plot}
# Plotting summary statistics for each stock

ggplot(myStocks_returns_monthly_summary,aes(x=SD_return, y=mean_return, label=symbol)) +
  geom_point() +
  ggrepel::geom_text_repel() +
  labs(title = 'Risk vs. Mean Monthly Return by Stock') +
  NULL
```

What can you infer from this plot? Are there any stocks which, while being riskier, do not have a higher expected return?

In general, stocks with higher returns are accompanied with higher risks. ANF underperforms as it has a much higher risk (SD_return) than all the other stocks, yet has a lower average return than all the other stocks except XOM. By contrast, DPZ is a very attractive stock because it generates second highest return while carrying less volatility (risk). Although TSLA has the highest return, it also carries the highest risk and may only be suitable for those with strong risk-bearing capability.


# On your own: IBM HR Analytics

For this task, you will analyse a data set on Human Resoruce Analytics. The [IBM HR Analytics Employee Attrition & Performance data set](https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset) is a fictional data set created by IBM data scientists.  Among other things, the data set includes employees' income, their distance from work, their position in the company, their level of education, etc. A full description can be found on the website.


First let us load the data

```{r}

hr_dataset <- read_csv(here::here("data", "datasets_1067_1925_WA_Fn-UseC_-HR-Employee-Attrition.csv"))
glimpse(hr_dataset)

```

```{r}
# Clean the dataset
hr_cleaned <- hr_dataset %>% 
  clean_names() %>% 
  mutate(
    education = case_when(
      education == 1 ~ "Below College",
      education == 2 ~ "College",
      education == 3 ~ "Bachelor",
      education == 4 ~ "Master",
      education == 5 ~ "Doctor"
    ),
    environment_satisfaction = case_when(
      environment_satisfaction == 1 ~ "Low",
      environment_satisfaction == 2 ~ "Medium",
      environment_satisfaction == 3 ~ "High",
      environment_satisfaction == 4 ~ "Very High"
    ),
    job_satisfaction = case_when(
      job_satisfaction == 1 ~ "Low",
      job_satisfaction == 2 ~ "Medium",
      job_satisfaction == 3 ~ "High",
      job_satisfaction == 4 ~ "Very High"
    ),
    performance_rating = case_when(
      performance_rating == 1 ~ "Low",
      performance_rating == 2 ~ "Good",
      performance_rating == 3 ~ "Excellent",
      performance_rating == 4 ~ "Outstanding"
    ),
    work_life_balance = case_when(
      work_life_balance == 1 ~ "Bad",
      work_life_balance == 2 ~ "Good",
      work_life_balance == 3 ~ "Better",
      work_life_balance == 4 ~ "Best"
    )
  ) %>% 
  select(age, attrition, daily_rate, department,
         distance_from_home, education,
         gender, job_role,environment_satisfaction,
         job_satisfaction, marital_status,
         monthly_income, num_companies_worked, percent_salary_hike,
         performance_rating, total_working_years,
         work_life_balance, years_at_company,
         years_since_last_promotion)

```

Produce a one-page summary describing this dataset. Here is a non-exhaustive list of questions:
```{r}
head(hr_cleaned)
```

1. How often do people leave the company (`attrition`)
```{r}
hr_cleaned %>% 
  group_by(attrition) %>% 
  count()
```
Out of the 1470 employees included in the dataset, 237 left, which makes up 19% of sample.

1. How are `age`, `years_at_company`, `monthly_income` and `years_since_last_promotion` distributed? can you roughly guess which of these variables is closer to Normal just by looking at summary statistics? 
```{r}
# Distribution of age
ggplot(hr_cleaned,aes(x = age)) +
  geom_histogram() +
  labs(title = 'age') + 
  NULL

# Distribution of years at company
ggplot(hr_cleaned,aes(x = years_at_company)) +
  geom_histogram() +
  labs(title = 'years_at_company') + 
  NULL

# Distribution of monthly income
ggplot(hr_cleaned,aes(x = monthly_income)) +
  geom_histogram() +
  labs(title = 'monthly_income') + 
  NULL

# Distribution of years since last promotion
ggplot(hr_cleaned,aes(x = years_since_last_promotion)) +
  geom_histogram() +
  labs(title = 'years_since_last_promotion') + 
  NULL
```
By roughly looking at the histograms, age has a distribution closest to normal distribution.

1. How are `job_satisfaction` and `work_life_balance` distributed? Don't just report counts, but express categories as % of total
```{r}
# job satisfaction 
hr_cleaned %>% 
  group_by(job_satisfaction) %>% 
  summarise(count = n()) %>% 
  mutate(ratio = count/sum(count))

# work life balance
hr_cleaned %>% 
  group_by(work_life_balance) %>% 
  summarise(count = n()) %>% 
  mutate(ratio = count/sum(count))
```


1. Is there any relationship between monthly income and education? Monthly income and gender?
```{r}
# Box plots showing monthly income by education
hr_cleaned %>% 
  ggplot(aes(x=reorder(education,monthly_income),y=monthly_income)) +
  geom_boxplot() +
  labs(title = 'Monthly Income by Education', x = 'education') +
  NULL

# Box plots showing monthly income by gender
hr_cleaned %>% 
  ggplot(aes(x=reorder(gender,monthly_income),y=monthly_income)) +
  geom_boxplot() +
  labs(title = 'Monthly Income by Gender', x = 'gender') + 
  NULL

```
From the box plots, we can see that there is a slight but not too consistent relationship between monthly income and education. Employees with higher education level tend to earn mean higher income, with the exception of mean income of bachelors being lower than college graduates. However, the level of education does not seem to affect the maximum income one can get as in each level there are some outliers with extremely high income. It is also worth noting that doctor has the larger interquartile range, showing the large difference in incomes even among doctors.

On the other hand, the by gender box plots show that that mean income of females is slightly higher than males. This difference is larger in the upper quartile. Two genders' maximum incomes are still similar. Overall, there may be a weak relationship between monthly income and gender.

1. Plot a boxplot of income vs job role. Make sure the highest-paid job roles appear first
```{r}
hr_cleaned %>% 
  ggplot(aes(x=reorder(job_role,desc(monthly_income)),y=monthly_income)) +
  geom_boxplot() +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  labs(title = 'Monthly Income by Job Role', x = 'job_role') +
  NULL
```

1. Calculate and plot a bar chart of the mean (or median?) income by education level.
```{r}
hr_cleaned %>% 
  group_by(education) %>% 
  summarise(mean_monthly_income = mean(monthly_income)) %>% 
  ggplot(aes(x=mean_monthly_income,y=reorder(education,mean_monthly_income))) +
  geom_col() +
  labs(title = 'Mean Monthly Income by Education Level', y = 'Education Level') +
  NULL
```

1. Plot the distribution of income by education level. Use a facet_wrap and a theme from `ggthemes`
```{r}
hr_cleaned %>% 
  ggplot(aes(x = monthly_income)) +
  geom_histogram() +
  facet_wrap(~reorder(education,monthly_income)) +
  ggthemes::theme_economist() +
  labs(title = 'Distribution of Monthly Income by Education Level') +
  NULL
```


1. Plot income vs age, faceted by `job_role`
```{r}
hr_cleaned %>% 
  ggplot(aes(x = age, y = monthly_income)) +
  geom_point() +
  facet_wrap(~reorder(job_role,monthly_income)) +
  labs(title = 'Income vs. Age by Job Role') +
  NULL
```



# Challenge 1: Replicating a chart

The purpose of this exercise is to reproduce a plot using your `dplyr` and `ggplot2` skills. Read the  article [The Racial Factor: There's 77 Counties Which Are Deep Blue But Also Low-Vaxx. Guess What They Have In Common?](https://acasignups.net/21/07/18/racial-factor-theres-77-counties-which-are-deep-blue-also-low-vaxx-guess-what-they-have) and have a look at the attached figure.

```{r challenge1, echo=FALSE, out.width="90%"}
knitr::include_graphics(here::here("images", "vaxxes_by_state_red_blue_every_county_070321_1.jpeg"), error = FALSE)
```


You dont have to worry about the blue-red backgound and don't worry about replicating it exactly, try and see how far you can get. You're encouraged to work together if you want to and exchange tips/tricks you figured out-- and even though the figure in the original article is from early July 2021, you can use the most recent data.

Some hints to get you started:

1. To get vaccination by county, we will use [data from the CDC](https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh) 
1. You need to get [County Presidential Election Returns 2000-2020](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ)
1. Finally, you also need an estimate of the [population of each county](https://www.ers.usda.gov/webdocs/DataFiles/48747/PopulationEstimates.csv?v=2232)
 


```{r, echo=FALSE, cache=TRUE}

# Download CDC vaccination by county
cdc_url <- "https://data.cdc.gov/api/views/8xkx-amqh/rows.csv?accessType=DOWNLOAD"
vaccinations <- vroom(cdc_url) %>% 
  janitor::clean_names() %>% 
  filter(fips != "UNK") # remove counties that have an unknown (UNK) FIPS code

# Download County Presidential Election Returns
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
election2020_results <- vroom(here::here("data", "countypres_2000-2020.csv")) %>% 
  janitor::clean_names() %>% 
  
  # just keep the results for the 2020 election
  filter(year == "2020") %>% 
  
  # change original name county_fips to fips, to be consistent with the other two files
  rename (fips = county_fips)

# Download county population data
population_url <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/PopulationEstimates.csv?v=2232"
population <- vroom(population_url) %>% 
  janitor::clean_names() %>% 
  
  # select the latest data, namely 2019
  select(fips = fip_stxt, pop_estimate_2019) %>% 
  
  # pad FIPS codes with leading zeros, so they are always made up of 5 characters
  mutate(fips = stringi::stri_pad_left(fips, width=5, pad = "0"))

```

```{r}
head(population)
skim(population)
head(election2020_results)
skim(election2020_results)
skim(vaccinations)
head(vaccinations)

filter(vaccinations, fips == "48037")
county_vaccination_rate <-
  vaccinations %>%
  group_by(fips) %>% 
  summarise(complete_rate_max = max(series_complete_pop_pct))

county_vaccination_rate

vaccinations_cmplt_pct <-
  select(vaccinations, fips,recip_county,recip_state,completeness_pct)
library(scales)

TRUMP_Supportor <-
  filter(election2020_results,party == "REPUBLICAN") %>% 
  mutate(Trump_Vote = 100*(candidatevotes/totalvotes))

TRUMP_Supportor
TRUMP_Supportor_2 <-
  TRUMP_Supportor %>%
  group_by(fips) %>% 
  summarise(Trump_Vote_all = sum(Trump_Vote))

TRUMP_Supportor

filter(TRUMP_Supportor_2, fips == "13189")

every_us_county <-
  inner_join(TRUMP_Supportor_2, county_vaccination_rate, by ="fips")
new_every_us_county <-
  inner_join(every_us_county,population, by = "fips")
new_every_us_county_2 <-
  filter(new_every_us_county, complete_rate_max > 0 )

# filter(new_every_us_county, fips == "13189")
# filter(new_every_us_county, complete_rate_max > 0 )
# new_every_us_county %>% 
skim(new_every_us_county)
Trump_Vote_all
rects <- data.frame(xstart = seq(0,45,45), xend = seq(55,100,45),col = letters[2:1])
library(ggplot2)
library(ggpmisc)

ggplot()+
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = 0, ymax = 100,fill = col), alpha = 0.4)+
  # geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(data = new_every_us_county_2,aes(x = Trump_Vote_all,y = complete_rate_max,size = pop_estimate_2019),colour = "darkblue",fill = "black",alpha = 0.2 )+
  geom_smooth(data = new_every_us_county_2,aes(x = Trump_Vote_all,y = complete_rate_max),method = lm)+
  # stat_cor(label.x = 3, p.accuracy = 0.001, r.accuracy = 0.01) +
  # stat_poly_eq(aes(label = ..eq.label..),formula = formula,parse = TRUE, geom = "text",label.x = 3,label.y = 28, hjust = 0)

  # stat_cor(method = "pearson",label.x = 3, label.y = 30) 
  # stat_cor(method = "pearson",label.x = 3, label.y = 30) +
  # stat_poly_eq(aes(label = ..eq.label..),formula = formula,parse = TRUE, geom = "text",label.x = 3,label.y = 28, hjust = 0)+
  coord_fixed(ratio=1.3)

ggsave("plot.png")



# #Fake data 
# dat <- data.frame(x = 1:100, y = cumsum(rnorm(100))) 
# #Breaks for background rectangles 
# rects <- data.frame(xstart = seq(0,80,20), xend = seq(20,100,20), col = letters[1:5]) #As Baptiste points out, the order of the geom's matters, so putting your data as last will #make sure that it is plotted "on top" of the background rectangles. Updated code, but #did not update the JPEG...I think you'll get the point. 
# ggplot() + geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = 0, ymax = 100, fill = col), alpha = 0.4) + geom_line(data = dat, aes(x,y)) 
```

# Challenge 2: Opinion polls for the 2021 German elections

The Guardian newspaper has an [election poll tracker for the upcoming German election](https://www.theguardian.com/world/2021/aug/20/german-election-poll-tracker-who-will-be-the-next-chancellor).
The list of the opinion polls since Jan 2021 can be found at [Wikipedia](https://en.wikipedia.org/wiki/Opinion_polling_for_the_2021_German_federal_election) and your task is to reproduce the graph similar to the one produced by the Guardian. 


The following code will scrape the wikipedia page and import the table in a dataframe.


```{r, scrape_wikipedia_polling_data, warnings= FALSE, message=FALSE}
url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2021_German_federal_election"
# https://www.economist.com/graphic-detail/who-will-succeed-angela-merkel
# https://www.theguardian.com/world/2021/jun/21/german-election-poll-tracker-who-will-be-the-next-chancellor


# get tables that exist on wikipedia page 
tables <- url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called polls 
# Use purr::map() to create a list of all tables in URL
polls <- map(tables, . %>% 
             html_table(fill=TRUE)%>% 
             janitor::clean_names())


# list of opinion polls
election_polls_germany <- polls[[1]] %>% # the first table on the page contains the list of all opinions polls
  slice(2:(n()-1)) %>%  # drop the first row, as it contains again the variable names and last row that contains 2017 results
  mutate(
         # polls are shown to run from-to, e.g. 9-13 Aug 2021. We keep the last date, 13 Aug here, as the poll date
         # and we extract it by picking the last 11 characters from that field
         end_date = str_sub(fieldwork_date, -11),
         
         # end_date is still a string, so we convert it into a date object using lubridate::dmy()
         end_date = dmy(end_date),
         
         # we also get the month and week number from the date, if we want to do analysis by month- week, etc.
         month = month(end_date),
         week = isoweek(end_date)
         )
```

```{r}
glimpse(polls)
```


```{r}

election_polls_germany %>% select(union,spd,af_d, fdp, linke, grune, end_date) %>%
  pivot_longer(.,cols= c(union,spd,af_d,fdp,linke,grune), 
               names_to = "Parties", values_to = "val") %>%
  ggplot(aes(x= end_date, y= val, fill= Parties, 
             colour= Parties))+
  geom_point(alpha=0.3)+
  geom_smooth(se=FALSE)+
  scale_y_continuous(labels=function(x) paste0(x,"%"))+
  scale_x_date(date_labels = "%b %y")+
  xlab("Date")+
  ylab("Percentage Votes (Polls)")+
  labs(title= "Polls Data for Germany Election 2021")+
  scale_color_manual(values=c("#0088FF", "#FFFF99", "#448844", "#DD1199", 
                              "#FF5811", "#001100"))+ 
  theme(plot.title=element_text(hjust=0.5))+
  knitr::opts_chunk$set(fig.width=unit(10,"cm"), fig.height=unit(10,"cm"))



```

