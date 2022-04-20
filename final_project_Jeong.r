library(dplyr)
library(ggplot2)
library(scales)

covid <- read.csv("/Users/yurijeong/Desktop/owid-covid-data.csv")
head(covid)
tail(covid)
colnames(covid)

#filter by country - only india
india <- covid %>% filter(location == "India")
summary(india)
str(india)

india$date = as.Date(india$date, "%Y-%m-%d")

#filter by year
first_year <- india %>% filter(date < as.Date("2021-01-01"))
first_year[is.na(first_year)] = 0 #replace NA values to 0
summary(first_year)

second_year <- india %>% filter(date > as.Date("2020-12-31"))
second_year[is.na(second_year)] = 0
head(second_year)

remove_nov <- second_year %>% filter(date < as.Date("2021-11-01"))
summary(remove_nov)

#remove scientific notation
options(scipen = 100)

#covid test group by month in 2020
test_monthly <- first_year %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(month, year) %>%
  summarise(total = sum(new_tests))

#change numeric to month abb
test_monthly$month <- as.numeric(test_monthly$month)
test_monthly$month <- month.abb[test_monthly$month]

test_monthly$month = factor(test_monthly$month, levels = month.abb)

#india covid-19 test in 2020
ggplot(test_monthly) +
  aes(x = month, y = total, group = 1) +
  geom_point() +
  geom_line() +
  labs(title = "India Covid-19 Test in 2020", y = "Number of Test", x = "Month") +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 1)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_x_discrete(limits = month.abb)

#covid test group by month in 2021
test_monthly2 <- remove_nov %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(month, year) %>%
  summarise(total = sum(new_tests))

#change numeric to month abb
test_monthly2$month <- as.numeric(test_monthly2$month)
test_monthly2$month <- month.abb[test_monthly2$month]

test_monthly2$month = factor(test_monthly2$month, levels = month.abb)

#india covid-19 test in 2021
ggplot(test_monthly2) +
  aes(x = month, y = total, group = 1) +
  geom_point() +
  geom_line() +
  labs(title = "India Covid-19 Test in 2021", y = "Number of Test", x = "Month") +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 1)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_x_discrete(limits = month.abb)

#covid cases group by month in 2020
monthly <- first_year %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(month, year) %>%
  summarise(total = sum(new_cases))

#change numeric to month abb
monthly$month <- as.numeric(monthly$month)
monthly$month <- month.abb[monthly$month]

monthly$month = factor(monthly$month, levels = month.abb)

#india covid-19 case development in 2020
ggplot(monthly) +
  aes(x = month, y = total, group = 1) +
  geom_point(color = "red") +
  geom_line(color = "red") +
  labs(title = "India Covid-19 cases in 2020", y = "Cases", x = "Month") +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 1)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_x_discrete(limits = month.abb)

#covid cases group by month in 2021
monthly2 <- remove_nov %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(month, year) %>%
  summarise(total = sum(new_cases))

#change numeric to month abb
monthly2$month <- as.numeric(monthly2$month)
monthly2$month <- month.abb[monthly2$month]

monthly2$month = factor(monthly2$month, levels = month.abb)

#india covid-19 case development in 2021 
ggplot(monthly2) +
  aes(x = month, y = total, group = 1) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  labs(title = "India Covid-19 cases in 2021", y = "Cases", x = "Month") +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 1)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_x_discrete(limits = month.abb)

#india death distribution in 2020
sum(is.na(first_year$new_deaths))
first_year$new_deaths[is.na(first_year$new_deaths)] <- 0

ggplot(first_year) +
  aes(x = date, y = new_deaths) +
  geom_point() +
  labs(title = "Number of Death distribution in 2020", y = "Number of Death", x = "Date") +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 1)) +
  scale_y_continuous(labels = comma)

#india death distribution in 2021
sum(is.na(remove_nov$new_deaths))

ggplot(remove_nov) +
  aes(x = date, y = new_deaths) +
  geom_point() +
  labs(title = "Number of Death distribution in 2021", y = "Number of Death", x = "Date") +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 1)) +
  scale_y_continuous(labels = comma)


################
#relationship between new test and new cases
#2020
lmTest <- lm(new_cases ~ new_tests, data = first_year)
summary(lmTest)

par(mfrow=c(2,2))
plot(lmTest)
par(mfrow=c(1,1))

plot(first_year$new_tests, first_year$new_cases, 
     main = "Covid-19 Test and Case in 2020", xlab = "Covid-19 Test", ylab = "Covid-19 Case")

#2021
lmTest2 <- lm(new_cases ~ new_tests, data = remove_nov)
summary(lmTest2)

par(mfrow=c(2,2))
plot(lmTest2)
par(mfrow=c(1,1))

plot(remove_nov$new_tests, remove_nov$new_cases, 
     main = "Covid-19 Test and Case in 2021", xlab = "Covid-19 Test", ylab = "Covid-19 Case")

#relationship between new cases and new deaths 
#2020

hist(first_year$new_deaths, main = "Death Frequency in 2020", xlab = "Number of Death")

lmFirstDeath <- lm(new_deaths ~ new_cases, data = first_year)
summary(lmFirstDeath)

par(mfrow=c(2,2))
plot(lmFirstDeath)
par(mfrow=c(1,1))

plot(first_year$new_cases, first_year$new_deaths, 
     main = "Covid-19 Cases and Number of Death in 2020", xlab = "Covid-19 Case", ylab = "Number of Death")
abline(lmFirstDeath, col = "red")

#2021
hist(remove_nov$new_deaths, main = "Death Frequency in 2021", xlab = "Number of Death")

lmSecondDeath <- lm(new_deaths ~ new_cases, data = remove_nov)
summary(lmSecondDeath)

par(mfrow=c(2,2))
plot(lmSecondDeath)
par(mfrow=c(1,1))

plot(remove_nov$new_cases, remove_nov$new_deaths, 
     main = "Covid-19 Cases and Number of Death in 2021", xlab = "Covid-19 Case", ylab = "Number of Death")

abline(lmSecondDeath, col = "blue")
