Load scripts!
-------------

    active <- read.csv("~/Desktop/Practice/activity.csv")

    data <- active %>% 
      mutate(new_date = lubridate::date(date)) %>% 
      select(steps, interval, new_date)

Mean of total number of steps taken per day?
--------------------------------------------

    #calculate the total number of steps taken per day

    addsteps <- data %>% 
      group_by(new_date) %>% 
      summarise(add_steps = sum(steps)) 

    addsteps

    ## # A tibble: 61 x 2
    ##    new_date   add_steps
    ##    <date>         <int>
    ##  1 2012-10-01        NA
    ##  2 2012-10-02       126
    ##  3 2012-10-03     11352
    ##  4 2012-10-04     12116
    ##  5 2012-10-05     13294
    ##  6 2012-10-06     15420
    ##  7 2012-10-07     11015
    ##  8 2012-10-08        NA
    ##  9 2012-10-09     12811
    ## 10 2012-10-10      9900
    ## # ... with 51 more rows

    #make histogram of the total number of steps taken each day

    addsteps %>% 
      ggplot(aes(add_steps)) + geom_histogram()

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

![](PA1_template_files/figure-markdown_strict/mean%20of%20steps-1.png)

    #mean and median

    data %>% 
      group_by(new_date) %>% 
      summarise(add_steps = sum(steps)) %>% 
      summarise(mean_totalsteps = mean(add_steps, na.rm = TRUE), median_totalsteps = median(add_steps, na.rm = TRUE))

    ## # A tibble: 1 x 2
    ##   mean_totalsteps median_totalsteps
    ##             <dbl>             <int>
    ## 1           10766             10765

What is the average daily activity pattern?
-------------------------------------------

    data %>%
      group_by(interval) %>% 
      summarize(mean_steps = mean(steps, na.rm = TRUE)) %>% 
      ggplot(aes(interval, mean_steps)) +
      geom_line() +
      geom_point()

![](PA1_template_files/figure-markdown_strict/average%20activity%20pattern-1.png)

    data %>%
      group_by(interval) %>% 
      summarize(mean_steps = mean(steps, na.rm = TRUE)) %>%
      filter(mean_steps == max(mean_steps))

    ## # A tibble: 1 x 2
    ##   interval mean_steps
    ##      <int>      <dbl>
    ## 1      835        206

    #interval 835 has the highest average steps at approximately 206

Imputing missing values!
------------------------

    data %>% 
      select(steps) %>% 
      summarise(sum(is.na(.)))

    ##   sum(is.na(.))
    ## 1          2304

    #there are 2304 nas in the steps data

    #imputing strategy - replace nas with mean for the interval and create new dataset

    map <- data %>%
      group_by(interval) %>% 
      summarize(mean_steps = as.integer(mean(steps, na.rm = TRUE))) 

    join <- merge(data, map, by = "interval")

    final <-join %>%
      mutate(steps_final = coalesce(steps, mean_steps))

    #make histogram of the total number of steps taken each day

    final %>% 
      ggplot(aes(steps_final)) + geom_histogram()

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](PA1_template_files/figure-markdown_strict/imputing%20missing%20values-1.png)

    #mean and median

    final %>% 
      group_by(new_date) %>% 
      summarise(steps_final = sum(steps_final)) %>% 
      summarise(mean_totalsteps = mean(steps_final), median_totalsteps = median(steps_final))

    ## # A tibble: 1 x 2
    ##   mean_totalsteps median_totalsteps
    ##             <dbl>             <int>
    ## 1           10750             10641

The mean and median are now both slightly lower. The histogram now
includes many more 0s.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    weekend_identify <-final %>%
      mutate(day_of_week = weekdays(new_date)) %>% 
      mutate(weekend_flag = factor(ifelse(day_of_week == c("Saturday", "Sunday"), "weekend", "not_weekend")))

    head(weekend_identify)

    ##   interval steps   new_date mean_steps steps_final day_of_week
    ## 1        0    NA 2012-10-01          1           1      Monday
    ## 2        0     0 2012-11-23          1           0      Friday
    ## 3        0     0 2012-10-28          1           0      Sunday
    ## 4        0     0 2012-11-06          1           0     Tuesday
    ## 5        0     0 2012-11-24          1           0    Saturday
    ## 6        0     0 2012-11-15          1           0    Thursday
    ##   weekend_flag
    ## 1  not_weekend
    ## 2  not_weekend
    ## 3  not_weekend
    ## 4  not_weekend
    ## 5      weekend
    ## 6  not_weekend

    final_graph <- weekend_identify %>%
      group_by(weekend_flag, interval) %>% 
      summarise(final_steps = mean(steps_final)) %>% 
      ggplot(aes(interval, final_steps)) +
      facet_grid(.~ weekend_flag, space = "free") +
      geom_line() +
      geom_point()

    final_graph

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-1-1.png)
