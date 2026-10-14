# Is Public Capital a Productive Factor? A Spatial Econometrics Approach

This project investigates whether public capital contributes to economic productivity in the United States using spatial panel econometric methods.

The analysis uses a balanced panel of 48 U.S. states from 1970 to 1986, sourced from Baltagi (1995). The empirical framework examines the relationship between Gross State Product (GSP), private capital, labor, and public capital, accounting for spatial dependence and potential endogeneity.

## Methodology

- Spatial panel econometric models.
- Instrumental variable estimation.
- Robustness checks across alternative specifications and controls.
- Disaggregation of public capital into highways and streets, water and sewer facilities, and other public buildings and structures.

## Main Findings

The results suggest that public capital does not have a positive and robust effect on state-level output. Instrumental variable estimates indicate a negative relationship, potentially reflecting inefficiencies in public investment. The disaggregated analysis suggests that this negative effect is primarily associated with public buildings and structures.

## Data and reproducibility

- **Panel data:** The `Produc` dataset is available directly through the R package `plm` (`data(Produc, package = "plm")`). The analysis uses 48 U.S. states observed from 1970 to 1986. The spatial weights matrix `usaww` is also loaded in the script.
- **Geographic data:** The mapping section requires the U.S. Census Bureau's **2018 Cartographic Boundary File for U.S. States (1:5,000,000)**, including `cb_2018_us_state_5m.rar` and its companion files (`.shx`, `.dbf`, `.prj`, etc.). 
- **Software:** R.
- **Execution:** Run `public_capital_spatial_econometrics.R` from a project directory containing the extracted geographic files. Some sections of the original research script may require additional preparation or debugging before end-to-end execution.


## References
-   Baltagi, B. H. (1995). Econometric Analysis of Panel Data. Wiley.

-   Baltagi, B. H., & Pinnoi, N. (1995). Public capital stock and state productivity growth: Further evidence from an error components model. Empirical Economics, 20(2), 351-359.

