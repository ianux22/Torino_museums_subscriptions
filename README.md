# Predictive models for Torino’s museums subscription churn rate

#### [Click here to read the full report in PDF](https://github.com/ianux22/Torino_museums_subscriptions/blob/main/Patricelli_Previous_Work.pdf) ####


This project was about predicting and analyzing the customers who renew the subscription of Piemonte’s museums (and who doesn’t), using the 2014 data to predict the 2015 renewals using the R programming language. In the end, I created a Random Forest model able to predict correctly almost 75% of the renewals. 

## Step 1: Exploratory Data Analysis and visualizations 

In the first part of the project I focused on data cleaning and visualizations using the R package Ggplot2 to see the variable distribution between people who renew and who don't.
I also used a shapefile with the borders of all the cities of Italy, focusing on north Italy and Piemonte region, to see the distribution on the territory.
Using the shapefiles we can clearly see that people who live close to Turin are more likely to renew the subscription.

## Step 2: Market basket analysis and custmer segmentation 

In the second phase, I analyzed what customers visit and what are their most recurring patterns. To do this, I used association rules: In fact, we can consider the customers' visits to museums as transactions. 
Market basket analysis via association rules shows what are the most popular museum and the patterns that lead to a particular museum. I also used the Ggplot 2 package to plot those rules.
Next, I used Self organising maps to cluster the users into 3 groups.

## Step 3: Logit model for econometrical approach 

Here, I used a logit model to explore what are the most important features when predicting renewals. Looking at the coefficients of the logit model we are able to understand how much a feature is important when predicting renewals. We can easily notice that the features related to the residency of the users has a big impact on the propbability to renew.

## Step 4: Models to predict churn

In the final phase, we use 3 models to predict the renewals and then I evaluated them looking at the Expected Profit and their ROC curve.
The winner is a Random forest model with about 75% of accuracy.
