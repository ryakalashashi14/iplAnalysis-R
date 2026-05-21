# iplAnalysis-R
Visualization of data using  R programming 
# Cricket Info Visualization

A compact R project that explores sample cricket performance data and creates simple visualizations with base R.

## What it does

- Loads a sample cricket player dataset
- Summarizes batting and bowling performance
- Generates charts for top run scorers, wicket takers, team totals, and role distribution
- Creates a text report with the key cricket insights

## Dataset

The project includes a sample CSV file in `data/cricket_info.csv`.

You can replace it with your own cricket stats as long as the same columns are kept.

## Project Structure

```text
cricket-info-visualization/
  data/
    cricket_info.csv
  output/
    .gitkeep
  src/
    cricket_visualization.R
  run_cricket_info.R
  README.md
```

## How to Run

Open R or RStudio and run:

```r
source("run_cricket_info.R")
```

If `Rscript` is available on your machine, you can also run:

```powershell
Rscript run_cricket_info.R
```

## Output Files

After running the project, the `output/` folder will contain:

- `top_run_scorers.png`
- `top_wicket_takers.png`
- `runs_vs_strike_rate.png`
- `team_runs.png`
- `team_wickets.png`
- `role_distribution.png`
- `cricket_summary.txt`

## Portfolio-ready summary

> Built a cricket info visualization project in R that analyzes sample player performance data and generates charts for batting, bowling, team totals, and role distribution.

