# Nevada 2024 Presidential Voter Turnout Model

## Overview

This project builds a logistic regression model to predict voter turnout in Nevada's 2024 presidential general election using an official Nevada Secretary of State voter file. The model generates individual turnout probabilities for every registered voter and scores them into targeting deciles for campaign use.

The model predicted 1,402,034 ballots cast, within 5.6% of the certified actual total of 1,484,840. Under-prediction was concentrated among voters who registered after 2020 and had no presidential history for the model to learn from.

---

## Data

The analysis uses an official Nevada Secretary of State voter file containing:
- **2,265,969 registered voters**
- **8,688,065 voter history records**

**Note:** The raw voter file data is not included in this repository. The dataset was obtained through a faculty research request to the Nevada Secretary of State and is restricted to classroom use. The R script and summary outputs are shared here to demonstrate methodology.

---

## Dependent Variable

Voted in either the 2016 OR 2020 presidential general election (union). This specification was chosen to:
- Capture presidential-cycle participation patterns
- Avoid COVID-driven turnout inflation from 2020 alone
- Avoid structural exclusion of post-2016 registrants that 2016 alone would cause

---

## Predictors

- **Midterm voting history index** - count of 2010, 2014, 2018, and 2022 generals voted in
- **Age buckets**
- **Party registration**
- **Active voter status**
- **New voter flag**
- **Phone on file**
- **Clark County indicator**
- **Washoe County indicator**

---

## Methods

- Logistic regression
- Individual turnout probability scoring for all 2.2M voters
- Decile scoring for targeting purposes
- Validation against certified 2024 election results

---

## Key Findings

- Past voting behavior dominated all other predictors
- Four of four midterm voters were nearly guaranteed to turn out
- Model explained 56% of deviance
- 5.6% prediction error vs certified results
- Under-prediction concentrated among ~702,000 post-2020 registrants with no presidential history

---

## Repository Contents

- `GVPT685Fina;.R` - Full R script including data processing, modeling, and decile scoring
- `GVPT685 Codebook.xlsx` - Summary spreadsheet of model outputs and decile distributions

---

## Author

Maxx Margob | MS Applied Political Analytics, University of Maryland
