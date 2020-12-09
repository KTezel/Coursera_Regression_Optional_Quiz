
## Provided by the Coursera project
install.packages("devtools")
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)

devtools::install_github("jhudsl/matahari")
library(matahari)

dance_start(value = FALSE, contents = FALSE)

## Install ggplot
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")

## Look at the data
dim(college)
head(college)
summary(college)

## Not many missing points
sum(is.na(college))

## Plot data
ggplot(college, aes(median)) + geom_histogram()
ggplot(college, aes(major_category, median)) + geom_point()


## Plot median income per major category ordered descending
library(dplyr)
library(forcats)
library(scales)
college %>% 
        mutate(major_category = fct_reorder(major_category, median)) %>%
        ggplot(aes(major_category, median)) +
        geom_boxplot() +
        scale_y_continuous(labels = dollar_format()) +
        coord_flip()

## Bar plot medians ordered descending
college %>% group_by(major_category) %>% 
                summarize(median = median(median)) %>%
        mutate(major_category = fct_reorder(major_category, median)) %>%
        ggplot(aes(major_category, median)) +
        geom_col() +
        scale_y_continuous(labels = dollar_format()) +
        coord_flip()

## Is the median salary column normally distributed?
## As we can see from the Q-Q plot, the data doesn't follow a normal distribution.
## There are some outliers, which might have some very high leverage.
qqnorm(college$median)
qqline(college$median, col = "blue")

## Highest salaries
college %>% arrange(desc(median)) %>% select(major_category, median)

##Counting sample size per major category
aggregate(college$sample_size, by=list(college$major_category), FUN=sum)
## Interdisciplinary category has a very low sample size compared to other majors.
## This might be one of the reasons why it is at the top of the list.
## We might have to remove it from our data due to low sample size

## How does gender affect the income?
## Multiply total column with genders
college$women <- round(college$total*college$perc_women,0)
college$men <- round(college$total*college$perc_men,0)

## Summarize genders per major category
college_gender <- rbind(data.frame(college$major_category, "gender" = college$men, "type" = "men"),
                        data.frame(college$major_category, "gender" = college$women, "type" = "women")
)

## Plotting stacked bar by gender totals
college %>% 
        arrange(desc(total)) %>%
        mutate(major_category = fct_reorder(major_category, median)) %>%
        gather(Gender, Number, men, women) %>%
        select(major_category, Gender, Number) %>%
        arrange(desc(Number)) %>%
        ggplot(aes(major_category, Number, fill = Gender)) + 
        scale_y_continuous(labels = comma) +
        geom_col() + 
        coord_flip()

##Summarizing salary and total sample by gender and major_category
by_major_category <- college %>%
        filter(!is.na(total)) %>%
        group_by(major_category) %>%
        summarise(Men = sum(men),
                     Women = sum(women),
                     Total = sum(total),
                     MedianSalary = median(median)) %>%
        mutate(ShareWomen = Women / Total) %>%
        arrange(desc(Total))

## Looking at the correlation between the women's share and median salary
if(!require("ggrepel")) install.packages("ggrepel"); library("ggrepel")
by_major_category %>%
        ggplot(aes(ShareWomen, MedianSalary)) + 
        geom_smooth(method = "lm") + 
        geom_text_repel(aes(label = major_category), force = .1) +
        geom_point()

## Get a subset of the college data. We will use this subset to look at the correlations
subset_college <- subset(college, select = -c(rank, major, major_category))
subset_college <- na.omit(subset_college)

## Looking at correlations
library(GGally)
all <- ggpairs(data = select(subset_college, c(major_code, total, perc_women, median, perc_employed_fulltime:perc_low_wage_jobs)), cardinality_threshold = 17)
cor(subset_college)

## Plot correlation
college.cor <- cor(subset_college, use = c("pairwise.complete.obs"))
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")
corrplot(cor(college.cor))


## Looking at different models
fit1 <- lm(median ~ factor(major_category), data=college)
fit2 <- lm(median ~ factor(major_category)*perc_women, data=college)

## Anova table
anova(fit, fit2)

## Model summaries
summary(fit1)
summary(fit2)

## Major category doesn't do a good job in explaining the median salaries.
## Bringing gender into account increases the R^2; however, this is still not enough.
## Save analysis file on desktop
dance_save(getwd())
