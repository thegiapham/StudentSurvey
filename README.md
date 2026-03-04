# Academic Performance in aliance with life style

## Link 
  - github repo: https://github.sydney.edu.au/tpha0822/DATA2902.git 
  - shinyapps link: https://tpha0822.shinyapps.io/CompleteApp/

This Shiny application lets you **explore a cleaned survey dataset** without writing any R code.  
It focuses on two common inferential questions:

1. **Independence Test** – visualise a contingency table, plot grouped bar-charts, and run a χ² test for two categorical variables.  
2. **T-Test** – compare the means of a numeric response across exactly two levels of a grouping factor, with diagnostic plots and automatic choice between **Welch t-test** and **Wilcoxon rank-sum** when normality fails. 
--- 
## Quick Start 
The app expects a file called **cleaned.csv** in the same folder.  
All required packages—`shiny`, `ggplot2`, `shinythemes`, `RColorBrewer`, `here`, and `rsconnect`—are loaded at the top of the script.

---
## Features

* **Responsive sidebar**  
  – Dropdowns list only variables relevant to each analysis.  
  – Long names are displayed in full but wrap neatly (CSS tweak).

* **Independence Test tab**  
  - Select *row* and *column* factors.  
  - **Contingency Table**: dynamic, updates instantly.  
  - **Visualisation**: grouped bar-chart, optional axis flip.  
  - **Test Statistics**: χ² test with Monte-Carlo p-value when expected counts < 5.

* **T-Test tab**  
  - Pick one grouping factor (must have 2 levels) and one numeric response.  
  - **Plots**  
    - Side-by-side boxplots.  
    - Dual QQ-plots with red reference lines; x-axis kept symmetric so outliers don’t push the bulk off-centre.  
  - **Test Output**  
    - Choose one of the tails (`two.sided`, `greater`, `less`).  
    - App runs **Shapiro–Wilk** normality tests on each sample.  
      • Both normal → `t.test()`  
      • Otherwise → `wilcox.test()`  
    - Full test object printed (t/W statistic, df, p-value, confidence interval, group means/medians).

---
## Customisation tips

* **Variable lists**  
  - `cat_choices` and `t_choices` are defined near the top; edit as needed.
* **Label shortening**  
  - If you later re-enable truncated labels, wrap the choice vectors with a helper like  
    `make_choices()` that sets `names()` on the vector.
* **Styling**  
  - Change the theme by swapping `shinytheme("flatly")` for any Bootswatch name (e.g., `cosmo`, `darkly`).  
  - Modify colours in the plots via `scale_fill_brewer()` or manual palettes.

---
## Advantage 
  - The UI and UX are interactive and easy to use with tabs.
## Disadvantage  

* **Interaction with the app** 
  - If you click of the side panel after open the tab, you might expect it to close the tab, but the current code can't do so. 
  - For t test, only normality (shapiro test) test is checked, variance equality is not assessed. The current code only allow two-samples t test.  
  - Plots are represented well, but are not interactive, this can be furtherly done with plotly.
  - Users cannot download the charts or table. 
