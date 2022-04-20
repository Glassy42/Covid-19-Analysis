# Covid-19 Analysis
### Author: Yuri Jeong

This project is to answer the below questions:
- Is there a relationship between number of Covid-19 tests conducted and recorded Covid-19 cases?
- Did the number of Covid-19 cases have an impact on the number of deaths in India 2020 and 2021?

Hypothesis testing was conducted and linear models were created to confirm the relationshiop between variables. The diagnostic plots for each model were created as well.

# Conclusion
The null hypothesis for all questions is that there is no relationship between variables and alternative hypothesis is that there is a relationship. First, the Covid-19 test and case numbers both in 2020 and 2021 did not have a relationship. The graph shows there was a subtle positive relationship at the beginning of 2020, potentially meaning the test was only available for people who had strong Covid-19 symptoms and hence most tests resulted in a positive result. However, later the test were probably more widely available, even with slight symptoms and people were more aware of Covid-19 so that the test result did not impact to the Covid-19 cases. Hence, the null hypothesis is not rejected, meaning there is no relationship between Covid-19 test and case numbers. Secondly, the number of deaths in 2020 shows a strong positive relationship, meaning Covid-19 had severe impact to entire mortality rate in 2020. Therefore, we reject null hypothesis, in other words, there is a relation between Covid-19 case and numbers of death in 2020. However, death numbers did not directly relate to the Covid-19 in 2021, probably meaning that policies such as keeping distance, wearing mask or vaccinations might have protected from Covid-19. Hence, we do not reject null hypothesis, meaning there is no relation between Covid-19 case and numbers of death in 2021.

# References
Data on COVID-19 (coronavirus) by Our World in Data, https://github.com/owid/covid-19-data/ tree/master/public/data (Dec 12, 2021) Selva Prabhakaran, Linear Regression, http://r-statistics.co/Linear-Regression.html (Dec 12, 2021) 
University of Virginia Library, Understanding Diagnostic Plots for Linear Regression Analysis, https://data.library.virginia.edu/diagnostic-plots/ (Dec 13, 2021) 
Zach, How to Interpret a Scale-Location Plot (With Examples), Statology, Nov 25.2020, https:// www.statology.org/scale-location-plot/ (Dec 13, 2021) 
Zach, Understanding the Null Hypothesis for Linear Regression, Statology, May 14.2021, https:// www.statology.org/null-hypothesis-for-linear-regression/ (Dec 13, 2021) 
Zach, What is a Residuals vs. Leverage Plot? (Definition & Example), Statology, Sep 7.2021, https://www.statology.org/residuals-vs-leverage-plot/ (Dec 13, 2021)