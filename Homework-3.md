Homework 3
================
Olivia Wagner
10/10/2019

## Problem 1

``` r
# Initalize Data #
data('instacart')

# How many aisles are there? Which aisles are the most ordered from? #
instacart_aisles = instacart %>% 
  group_by(aisle_id) %>% 
  summarize(total_aisle_order = n()) %>% 
  arrange(desc(total_aisle_order)) 
 

# Make a plot that shows the number of items ordered in each aisle. limit to aisles with more than 10000 items. 

instacart_aisles_plot = instacart_aisles %>% 
  filter(total_aisle_order >= 10000) 

order_by_aisle_plot = ggplot(instacart_aisles_plot, aes(x = aisle_id, y = total_aisle_order)) + 
  geom_bar(stat = 'identity') + 
  ggtitle('Number of Orders per Aisle')+
  xlab('Aisle ID')+
  ylab('Total Number of Orders')
order_by_aisle_plot
```

![](Homework-3_files/figure-gfm/problem%201-1.png)<!-- -->

``` r
# Most popular items in each aisle #

instacart_item_pop = instacart %>% 
  filter(aisle %in% c('baking ingredients', 'dog food care', 'packaged vegetables fruits') )%>%
  group_by(aisle, product_id, product_name) %>% 
  summarize(number_of_products = n()) %>%
  arrange(desc(number_of_products)) %>% 
  ungroup(product_id, product_name)%>%
  group_by(aisle) %>%
  arrange(aisle, desc(number_of_products)) %>%
  mutate(product_rank = rank(number_of_products)) %>%
  top_n(n=3) %>%
  mutate(product_rank = rank(-product_rank)) %>%
  select(-product_id) 
```

    ## Selecting by product_rank

``` r
item_popularity_table = knitr::kable(instacart_item_pop, caption = 'Top Three products ordered in each Aisle')
item_popularity_table
```

| aisle                      | product\_name                                 | number\_of\_products | product\_rank |
| :------------------------- | :-------------------------------------------- | -------------------: | ------------: |
| baking ingredients         | Light Brown Sugar                             |                  499 |             1 |
| baking ingredients         | Pure Baking Soda                              |                  387 |             2 |
| baking ingredients         | Cane Sugar                                    |                  336 |             3 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |                   30 |             1 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |                   28 |             2 |
| dog food care              | Small Dog Biscuits                            |                   26 |             3 |
| packaged vegetables fruits | Organic Baby Spinach                          |                 9784 |             1 |
| packaged vegetables fruits | Organic Raspberries                           |                 5546 |             2 |
| packaged vegetables fruits | Organic Blueberries                           |                 4966 |             3 |

Top Three products ordered in each Aisle

``` r
# mean hour of the day for pink lady apples and coffe ice cream orders #

instacart_mean_hod = instacart %>%
  filter(product_name %in% c('Pink Lady Apples', 'Coffee Ice Cream')) %>%
  group_by(product_name, order_dow) %>%
  summarize(mean_hod = mean(order_hour_of_day))%>%
  pivot_wider(names_from = order_dow, values_from = mean_hod) 

mean_hod_table = knitr::kable(instacart_mean_hod, caption = 'Average Hour in which Pink Lady Apples and Coffee Ice Cream is ordered throughout the Week')
mean_hod_table
```

| product\_name    |        0 |        1 |        2 |        3 |        4 |        5 |        6 |
| :--------------- | -------: | -------: | -------: | -------: | -------: | -------: | -------: |
| Coffee Ice Cream | 13.77419 | 14.31579 | 15.38095 | 15.31818 | 15.21739 | 12.26316 | 13.83333 |
| Pink Lady Apples | 13.44118 | 11.36000 | 11.70213 | 14.25000 | 11.55172 | 12.78431 | 11.93750 |

Average Hour in which Pink Lady Apples and Coffee Ice Cream is ordered
throughout the Week

**Description:** The data set is a sample of orders from the online
grocery store Instacart. The data is organized first by the order ID and
then by the individual products in the order which are further
categorized by product type and name. For each order ID there is a
single user ID which correlates to the purchaser, as well as the last
time the user purchased products from Instacart. The variable eval\_set
also indicates that this data set is derived from a much larger data
set, and was used for predicitive analyitics likely pertaining to the
purchase frequency, amount and product type. In all, there are 1,384,617
rows and 15 columns of data.

  - There are 134 aisles, and the most items are ordered from the 83rd
    aisle (150609). This is narrowly larger than the 24th aisle
    (150473). After these two aisles, there is a dramatic drop in the
    number of items ordered.

  - The resulting plot shows us what we discovered previously; that
    there are two aisles which stand out in total number of items
    ordered, and the number of orders from the other aisles drop
    significantly.

  - The table showing the top three items the baking items, dog food and
    pacakged vegetable cateogries shows a much larger demand for
    pacakged vegetables than for dog food care and baking items. Dog
    food care had very few items being purchased each week on instacart,
    while baking items were moderately popular.

  - The table of pink lady apple and coffee ice cream purchases shows a
    tendency for pink lady apples to be purchased earlier in the day, on
    average, than coffee ice cream. Additionally there seems to be an
    weekend effect on coffee ice cream purchases where it was purchased
    earlier in the day on the weekend as opposed to weekdays.

## Problem 2

``` r
library(p8105.datasets)
data("brfss_smart2010") 

# Clean Data #
brfss = janitor::clean_names(brfss_smart2010) %>% print()
```

    ## # A tibble: 134,203 x 23
    ##     year locationabbr locationdesc class topic question response
    ##    <int> <chr>        <chr>        <chr> <chr> <chr>    <chr>   
    ##  1  2010 AL           AL - Jeffer… Heal… Over… How is … Excelle…
    ##  2  2010 AL           AL - Jeffer… Heal… Over… How is … Very go…
    ##  3  2010 AL           AL - Jeffer… Heal… Over… How is … Good    
    ##  4  2010 AL           AL - Jeffer… Heal… Over… How is … Fair    
    ##  5  2010 AL           AL - Jeffer… Heal… Over… How is … Poor    
    ##  6  2010 AL           AL - Jeffer… Heal… Fair… Health … Good or…
    ##  7  2010 AL           AL - Jeffer… Heal… Fair… Health … Fair or…
    ##  8  2010 AL           AL - Jeffer… Heal… Heal… Do you … Yes     
    ##  9  2010 AL           AL - Jeffer… Heal… Heal… Do you … No      
    ## 10  2010 AL           AL - Jeffer… Heal… Unde… Adults … Yes     
    ## # … with 134,193 more rows, and 16 more variables: sample_size <int>,
    ## #   data_value <dbl>, confidence_limit_low <dbl>,
    ## #   confidence_limit_high <dbl>, display_order <int>,
    ## #   data_value_unit <chr>, data_value_type <chr>,
    ## #   data_value_footnote_symbol <chr>, data_value_footnote <chr>,
    ## #   data_source <chr>, class_id <chr>, topic_id <chr>, location_id <chr>,
    ## #   question_id <chr>, respid <chr>, geo_location <chr>

``` r
brfss = brfss %>% 
  filter(topic == 'Overall Health') %>%
  mutate(response = as.factor(response)) %>%
  mutate(response = fct_relevel(response, c('Poor', 'Fair', 'Good', 'Very good', 'Excellent'))) %>%
  arrange(response) 

## 2002 Which 7 states were observed at 7 or more locations? #

brfss_state_by_location = brfss %>% 
  select(year, locationabbr, locationdesc) %>% 
  group_by(year, locationabbr) %>% 
  summarize(location_count = n_distinct(locationdesc)) %>%
  filter(location_count >= 7, year %in% c(2002, 2010)) 

## Excellent Responses ##

brfss_excellent_resp = brfss %>% 
  filter(response == 'Excellent') %>%
  select(year, locationabbr, data_value) %>%
  group_by(year, locationabbr) %>%
  summarize(mean_data_val = mean(data_value)) 

spaghetti_brfss_plot = ggplot(brfss_excellent_resp, aes(y = mean_data_val, x = year, color = locationabbr))+
  geom_line() +
  ggtitle('Average Data Value by Year')+
  ylab('Average Data Value')+
  xlab('Year')


spaghetti_brfss_plot
```

    ## Warning: Removed 3 rows containing missing values (geom_path).

![](Homework-3_files/figure-gfm/problem%202-1.png)<!-- -->

``` r
# Plot for 2006/2010 responses in NY #

ny_nrfss_data = brfss %>% 
  filter(locationabbr == 'NY', year %in% c(2010, 2006)) %>%
  group_by(year, locationdesc, response) 

ny_nrfss_plot = ggplot(ny_nrfss_data, aes(x = data_value, fill = factor(response))) +
  geom_density(alpha = .5)+
  facet_grid(~year) + 
  viridis::scale_fill_viridis(discrete = TRUE)+
  ggtitle('Distribution of Data Value by Categorical Response')

ny_nrfss_plot
```

![](Homework-3_files/figure-gfm/problem%202-2.png)<!-- -->

  - In 2002, Connecticut, Florida, Massachusetts, North Carolina, New
    Jersey and Pennsylvania had 7 or more locations. In 2010,
    California, Colorado, FLorida, Massachusetts, Maryland, North
    Carolina, Nebraska, New Jersey, New York, Ohio, Pennsylvania, South
    Carolina, Texas and Washington all had 7 or more locations.

  - The spaghetti plot shows the trend of data values for Excellent
    responses between the years of 2002 and 2010, broken down by state.
    The most striking thing about this graph is that there is a certain
    disjointness about the trends in between the lines, and Wisconsins
    data values are particularly lower than other states.

  - In the density plot, there seems to be some evening out between
    number of responses for category between the years 2006 and 2010.
    That is to say the distribution of responses in 2010 are less skewed
    than they are in 2006.

## Problem 3

``` r
accel_data = read_csv('./Problem 3/accel_data.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   day = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
accel_data = janitor::clean_names(accel_data)


# Tidy Data #

accel_data_tidy = accel_data %>% 
  pivot_longer(cols = starts_with('activity'), names_to = 'minute', values_to = 'activity') %>%
  mutate(minute = gsub('activity_', '', minute)) %>%
  mutate(weekday_vs_weekend = ifelse(day == 'Saturday', 'weekend', ifelse(day == 'Sunday', 'weekend', 'weekday'))) 

accel_data_tidy
```

    ## # A tibble: 8,820 x 6
    ##     week day_id day    minute activity weekday_vs_weekend
    ##    <dbl>  <dbl> <chr>  <chr>     <dbl> <chr>             
    ##  1     1      1 Friday 1          88.4 weekday           
    ##  2     1      1 Friday 2          82.2 weekday           
    ##  3     1      1 Friday 3          64.4 weekday           
    ##  4     1      1 Friday 4          70.0 weekday           
    ##  5     1      1 Friday 5          75.0 weekday           
    ##  6     1      1 Friday 6          66.3 weekday           
    ##  7     1      1 Friday 7          53.8 weekday           
    ##  8     1      1 Friday 8          47.8 weekday           
    ##  9     1      1 Friday 9          55.5 weekday           
    ## 10     1      1 Friday 10         43.0 weekday           
    ## # … with 8,810 more rows

``` r
# Table of total activity across days #

aggregate_accelerometer_data = accel_data_tidy %>% 
  group_by(week, day_id, day) %>%
  summarize(total_activity = sum(activity)) 

aggregate_accelerometer_table = knitr::kable(aggregate_accelerometer_data, caption = 'Total Activity per Day')
aggregate_accelerometer_table
```

| week | day\_id | day       | total\_activity |
| ---: | ------: | :-------- | --------------: |
|    1 |       1 | Friday    |       10158.289 |
|    1 |       2 | Monday    |         252.000 |
|    1 |       3 | Saturday  |        7944.000 |
|    1 |       4 | Sunday    |       35989.000 |
|    1 |       5 | Thursday  |        9918.511 |
|    1 |       6 | Tuesday   |       10440.978 |
|    1 |       7 | Wednesday |       10085.978 |
|    2 |       8 | Friday    |        9877.000 |
|    2 |       9 | Monday    |        9854.000 |
|    2 |      10 | Saturday  |        7371.000 |
|    2 |      11 | Sunday    |        2654.000 |
|    2 |      12 | Thursday  |       24089.000 |
|    2 |      13 | Tuesday   |       19451.000 |
|    2 |      14 | Wednesday |        3912.000 |
|    3 |      15 | Friday    |       12080.000 |
|    3 |      16 | Monday    |       13516.000 |
|    3 |      17 | Saturday  |        4122.000 |
|    3 |      18 | Sunday    |       14688.000 |
|    3 |      19 | Thursday  |        9062.000 |
|    3 |      20 | Tuesday   |        5513.000 |
|    3 |      21 | Wednesday |        3714.000 |
|    4 |      22 | Friday    |         252.000 |
|    4 |      23 | Monday    |       11746.000 |
|    4 |      24 | Saturday  |         252.000 |
|    4 |      25 | Sunday    |         252.000 |
|    4 |      26 | Thursday  |       19380.000 |
|    4 |      27 | Tuesday   |        2895.000 |
|    4 |      28 | Wednesday |        8417.000 |
|    5 |      29 | Friday    |        7241.000 |
|    5 |      30 | Monday    |        5094.000 |
|    5 |      31 | Saturday  |         252.000 |
|    5 |      32 | Sunday    |         252.000 |
|    5 |      33 | Thursday  |        6327.000 |
|    5 |      34 | Tuesday   |        4128.000 |
|    5 |      35 | Wednesday |        6214.000 |

Total Activity per Day

``` r
# 24-Activity plot by Day #

accelerometer_plot = ggplot(accel_data_tidy, aes(y = activity, x = minute, color = factor(day)))+
  geom_line()+
  ggtitle('Accelerometer Activity over Time')+
  ylab('Activity')+
  xlab('Minute of the Day')+
  theme(axis.text.x = element_text(angle = 90))

accelerometer_plot
```

![](Homework-3_files/figure-gfm/problem%203-1.png)<!-- -->

  - The resulting Tidy data set includes the variables week, day\_id,
    day, minute, activity, and weekday\_vs\_weekend. The data is
    organized by the week, then day of that week, then minute of that
    day. Each day is assigned a single day ID, and for each minute there
    is a record of acivity in the accelerometer. The data set has 6
    columns and 8,820 rows.

  - The table of acelerometer data by day shows the level of activity
    moves up and down quite frequently. Every few days there is a spike
    in activity and then it returns to average activity.

  - The accelerometer graph shows that there seems to be greater
    activity on Sunday and Thursday than the other days of the week.
    Ironically, the greatest activity observed occurs on a Wednesday,
    even though the graph tends to show the least activity occuring on a
    Wednesday.
