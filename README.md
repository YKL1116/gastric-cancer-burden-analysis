# Gastric cancer burden analysis

R analysis scripts for a study of gastric cancer burden in China, Japan and South Korea, with global maps and an analysis of SDI-related inequalities across 34 study locations.

## Analysis guide

| Script | Analysis |
| --- | --- |
| 01 | Global map of age-standardized incidence |
| 02 | Global map of age-standardized mortality |
| 03 | Annual incidence, mortality and DALY rate trends |
| 04 | Percentage change in absolute burden for Table 2 |
| 05 | Visualization of exported Joinpoint results for all nine country-by-outcome panels |
| 06 | Within-age-group standardization and age-stratified trends |
| 07 | Age and sex distributions in 2023 |
| 08 | Sex-stratified trends |
| 09 | Decomposition into growth, aging and epidemiological change, including Tables S44 and S45 |
| 10 | SDI-related inequalities, age-standardized sensitivity analysis and leave-one-location-out analyses, including Tables S46 and S47 |
| 11 | Smoking and high-sodium diet attributable fractions and rates |

## Running an analysis

1. Download the repository and open its top-level folder as the working directory in R or RStudio.
2. Install the packages listed at the top of the selected script. The scripts use the native R pipe and require R 4.1 or later. See the individual scripts for package version requirements.
3. Place the required input files in a local folder named `data` with the filenames expected by the script. Original data and intermediate Joinpoint exports must be supplied separately. The map scripts read GLOBOCAN workbooks, while the other analyses use GBD data or derived workbooks. Script 05 reads yearly observed and modeled series exported from Joinpoint.
4. Run the selected script from the top-level folder. For example, run the following in the R console to generate the sex-stratified trends figure.

```r
source("08_Figure3d_f_sex_trends.R", encoding = "UTF-8")
```

Scripts list their output paths and any directory overrides near the top. Their numbering provides an index for the analyses. Each script can be run independently when its inputs are available.
