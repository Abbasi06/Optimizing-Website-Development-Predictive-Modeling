# Optimizing-Website-Development-Predictive-Modeling
## Project Overview
This Case Study investigates features that influences the production output of website development teams and performs diagnostics to evaluate model performance. With the production data from 13 three-person website development teams (Project Manager, designer and developer) between January 2001 to August 2002. This project involves building a multiple linear regression model with possible polynomial and interaction terms to identify impactful predictors and assess model quality using diagnostic checks.

## Project Objectives
- Identify the influential features that influence the website production output
- Train and develop a best subset regression model to predict the target variable i.e. website production output
- Evaluate the model using diagnostic checks to identify influential outliers and assess multicollinearity
- Evaluate the build model performance through K-cross validation

## Dataset Description
| Variable Number | Variable Name        | Description                                                              |
|------------------|----------------------|--------------------------------------------------------------------------|
| 1                | Identification Number | Unique ID for each observation (1–73).                                   |
| 2                | Websites Delivered    | Number of websites completed and delivered to customers during the quarter. |
| 3                | Backlog of Orders     | Number of website orders in the backlog at the close of the quarter.      |
| 4                | Team Number           | Unique team number (1–13).                                               |
| 5                | Team Experience       | Number of months the team has been working together.                      |
| 6                | Process Change        | Whether a process change occurred during the quarter (1 if yes, 0 otherwise). |
| 7                | Year                  | Year of production (2001 or 2002).                                       |
| 8                | Quarter               | Quarter of production (1, 2, 3, or 4).                                   |

## Methodology
- Best Subset Model Development: Conducted automatic search procedure or step-wise regression methods like forward and backward model selection and evaluated using metrics like adjusted R-square and Bayesian Information Criterion (BIC)
- Diagnostic Checks: Checked for influential outliers using DFBETAS, DFITTS and Cook's Distance
- Assessed Multicollinearity using Variation Inflation Factor (VIF)

## Technology Stack
- R Programming on R-studio: For data preprocessing, modeling and diagnostics
- Regression Modeling: Step-wise regression model with multiple predictors, polynomial and interaction terms
- Google slides for creating powerpoint presentation

## Key Insights
- Significant Predictors: The backlog of orders, team experience, and process change were the most influential variables influencing production output
- Production Trends: When the process changes happened, production increased significantly during second and third quarters of 2002
- Subset Model: Trained and developed best subset model using step function
- Identified Outliers: Using Leverage and student residuals, identified potential outliers in X and Y direction
- Influential Points: Observations with high DFBETAS, Cook’s Distance and DIFITS were flagged and removed to improve model accuracy
- Multicollinearity issues: Predictors with high multicollinearity features i.e. Variation Inflation Factor > 2 were removed and addressed
- Hypothesis Testing: Conducted Anova table test inorder to pinpoint the irrelevant features and removed them

## Learning Outcomes
- Gained hands-on-experience with regression modeling techniques and R programming language
- Applied and learned hypothesis testing (Anova, t-test), diagnostics checks(DFBETAS, Cook’s Distance), and addressed Multicollinearity(VIF) to evaluate the model quality and performance
- Developed skills in identifying and mitigating outliers and multicollinearity in regression analysis
