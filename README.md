# 🧠 SHARE Wave 7 – Depression & Positive Attitudes Analysis

**This project explores psychological well-being among older European adults** using data from the [SHARE Wave 7](https://share-project.org) survey (Release 9.0.0).  
By analyzing responses from individuals aged **50 to 64**, we aim to uncover:

- 🌞 **Positive attitudes toward life** (sense of meaning, optimism, energy)
- 😔 **Depressive symptoms** (loneliness, lack of control, sadness)

We apply **factor analysis**, **clustering**, and **regression modeling** to examine how these latent psychological dimensions relate to:
- 👥 Demographics (age, gender, education)
- 💰 Income and employment
- 🏥 Health conditions
- 🤝 Social and cognitive activity

The goal is to better understand **mental health profiles** across countries and populations at higher risk of emotional distress.

---

## 📂 Project Structure
...

SHARE-Wave7-Depression-Study/
│
├── Modules/ # Raw SHARE .sav files (ac, ph, br)
├── Plots/ # Output graphs (01 to 15)
├── SHARE_main_analysis.R # Full R script
└── README.md


---

## 🛠️ Methods & Workflow

### 🔹 Step 1: Data Preparation
- Load `.sav` files from SHARE modules: activities (`ac`), physical health (`ph`), demographics (`br`)
- Clean and recode survey responses (e.g. Refusals → NA)
- Restrict to respondents aged 50–64 and currently employed
- Merge activity and demographic data

### 🔹 Step 2: Factor Analysis
- Use `psych::fa()` with 2-factor oblimin rotation
- Extract:
  - MR1 = Positive attitude
  - MR2 = Depressive symptoms
- Visualize factor diagram and density plots

### 🔹 Step 3: Country Clustering
- Aggregate MR1 and MR2 means by country
- Use elbow method to choose number of clusters
- Apply k-means and visualize:
  - Clusters in MR1–MR2 space
  - Geographic distribution via Google GeoChart

### 🔹 Step 4: Profiling
Analyze MR2 variation by:
- 👥 Gender
- 👵 Age group (50–54, 55–59, 60–64)
- 💰 Income level (tertiles)
- 🎓 Education level
- 🏥 Chronic health conditions
- 🤝 Social activity

### 🔹 Step 5: Regression Modeling
- Fit linear model predicting MR2
- Visualize coefficients and confidence intervals

---

## 📊 Output Gallery (`Plots/`)

| No. | Filename                            | Description                            |
|-----|-------------------------------------|----------------------------------------|
| 01  | `01_factor_structure.png`           | Factor diagram                         |
| 02  | `02_MR1_positive_attitude.png`      | MR1 distribution                       |
| 03  | `03_MR2_depressive_symptoms.png`    | MR2 distribution                       |
| 04  | `04_item_factor_correlations.png`   | Correlation heatmap (items × factors)  |
| 05  | `05_elbow_method_kmeans.png`        | Elbow method for choosing `k`          |
| 06  | `07_clusters_scatterplot.png`       | MR1 vs MR2 with k-means clusters       |
| 07  | *(interactive)*                     | GoogleVis GeoChart of clusters         |
| 08–14 | `08_...` to `14_...`              | Profiling plots (age, gender, income…) |
| 15  | `15_linear_regression_coefficients.png` | Regression results visualization |

---

## 📦 Required R Packages

```r
library(haven)
library(psych)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cluster)
library(factoextra)
library(googleVis)
library(ggrepel)
library(broom)

```

📚 Data Source
This project uses data from:

SHARE Wave 7 – Survey of Health, Ageing and Retirement in Europe
📦 Release 9.0.0, accessed under research agreement.
🔗 https://share-project.org

⚠️ SHARE data is protected and not included in this repository.
You must request access via SHARE-ERIC for any usage.

✍️ Author
Sergey Filipov
R Enthusiast | Social Science Analyst
📅 Last updated: June 2025
🔗 GitHub: @SergeyFilipov

📄 License
This repository is shared for academic and educational purposes only.
All usage of SHARE data must comply with the official licensing terms of SHARE-ERIC.


