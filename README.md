# Fast Food Nutritional Recommender

## Overview

Fast Food Nutritional Recommender is an interactive data-driven application that helps users identify fast food menu items that best align with their dietary goals and nutritional constraints.

The application analyzes menu items from major fast-food chains and ranks them using a custom **Conformity Score**, which measures how closely each item matches a selected diet profile based on macronutrient distribution.

Rather than focusing solely on calories, the system evaluates protein, carbohydrate, and fat composition while allowing users to apply additional nutritional constraints such as fiber, sugar, cholesterol, and calorie limits.

---

## Key Features

### Personalized Diet Matching

* Select from predefined diet plans:

  * Low Carb
  * Low Fat
  * Balanced
  * High Carb

* Create custom diets by specifying:

  * Protein %
  * Carbohydrate %
  * Fat %

### Conformity Score Engine

A custom scoring algorithm ranks menu items according to how closely their macronutrient composition matches a target diet profile.

Users can:

* Adjust macro importance weights
* Compare restaurants
* Identify top-performing menu items
* Filter results by minimum conformity score

### Nutritional Filtering

Filter menu items using:

* Calories
* Protein
* Carbohydrates
* Fat
* Fiber
* Sugar
* Cholesterol

### Interactive Data Visualization

The application provides:

* Restaurant comparison charts
* Distribution analysis of menu items
* Top-performing item rankings
* Nutritional composition pie charts
* Interactive Plotly visualizations

---

## Dataset

The analysis is based on publicly available nutritional information from six major fast-food chains:

* McDonald's
* Burger King
* Wendy's
* KFC
* Taco Bell
* Pizza Hut

The original dataset was obtained from Kaggle and underwent extensive cleaning and normalization before analysis.

---

## Data Processing Pipeline

### Data Cleaning

* Missing value handling
* Duplicate removal
* Invalid nutritional value filtering
* Data type conversion
* Column normalization

### Nutritional Calculations

Calorie contributions are calculated using standard nutritional assumptions:

* Protein = 4 kcal/g
* Carbohydrates = 4 kcal/g
* Fat = 9 kcal/g
* Fiber = 2 kcal/g

For each menu item, the system computes:

* Total calories from macros
* Percentage of calories from protein
* Percentage of calories from carbohydrates
* Percentage of calories from fat

---

## Conformity Score Methodology

The Conformity Score measures how closely a menu item aligns with a target dietary profile.

The score is based on weighted differences between:

* Target Protein %
* Target Carbohydrate %
* Target Fat %

and

* Actual Protein %
* Actual Carbohydrate %
* Actual Fat %

Higher scores indicate better alignment with the selected diet.

This approach allows users to move beyond simple calorie counting and evaluate food choices based on overall nutritional quality and dietary objectives.

---

## Technologies Used

### Data Analysis

* R
* dplyr
* tidyr
* readr

### Data Visualization

* ggplot2
* plotly
* patchwork

### Web Application Development

* Shiny
* DT
* shinyWidgets
* shinyBS

### Data Processing

* Data Cleaning
* Feature Engineering
* Nutritional Analytics

---

## Example Use Case

A user following a low-carbohydrate diet with a 700-calorie meal budget can:

1. Select the Low Carb diet profile.
2. Set a calorie range.
3. Specify additional nutritional constraints.
4. Generate ranked menu recommendations.
5. Compare fast-food chains based on nutritional compatibility.

The application identifies which menu items and restaurants best satisfy the user's dietary goals.

---

## Future Improvements

* Real-time menu data integration
* Mobile application deployment
* Restaurant location services
* Machine learning-based recommendation systems
* Personalized user profiles
* Meal recommendation history
* AI-powered nutritional assistant

---

## Project Goal

The objective of this project is to demonstrate how data analysis, nutritional science, and interactive visualization can be combined to support healthier decision-making in fast-food environments.

The project emphasizes practical data cleaning, feature engineering, scoring methodologies, and interactive analytics while addressing a real-world consumer problem.
