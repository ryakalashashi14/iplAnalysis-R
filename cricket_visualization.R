read_cricket_data <- function(path) {
  if (!file.exists(path)) {
    stop("Input data file not found: ", path)
  }

  data <- read.csv(path, stringsAsFactors = FALSE)
  required_columns <- c(
    "player",
    "team",
    "role",
    "matches",
    "runs",
    "balls_faced",
    "strike_rate",
    "fours",
    "sixes",
    "wickets"
  )

  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
  }

  numeric_columns <- c("matches", "runs", "balls_faced", "strike_rate", "fours", "sixes", "wickets")
  for (column in numeric_columns) {
    data[[column]] <- as.numeric(data[[column]])
  }

  data
}

ensure_output_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}

save_png <- function(path, width = 1200, height = 800, plot_fun) {
  png(filename = path, width = width, height = height, res = 144)
  on.exit(dev.off(), add = TRUE)
  plot_fun()
}

order_desc <- function(data, column) {
  data[order(data[[column]], decreasing = TRUE), , drop = FALSE]
}

save_horizontal_barplot <- function(data, value_column, title, xlab, output_file, fill_color) {
  top_players <- head(order_desc(data, value_column), 10)
  plot_data <- top_players[order(top_players[[value_column]]), , drop = FALSE]

  save_png(output_file, plot_fun = function() {
    par(mar = c(5, 12, 4, 2))
    bar_positions <- barplot(
      height = plot_data[[value_column]],
      names.arg = plot_data$player,
      horiz = TRUE,
      las = 1,
      col = fill_color,
      border = NA,
      xlab = xlab,
      main = title,
      xlim = c(0, max(plot_data[[value_column]]) * 1.2)
    )
    text(
      x = plot_data[[value_column]],
      y = bar_positions,
      labels = plot_data[[value_column]],
      pos = 4,
      cex = 0.8
    )
  })
}

save_role_distribution <- function(data, output_file) {
  role_counts <- sort(table(data$role), decreasing = TRUE)

  save_png(output_file, plot_fun = function() {
    par(mar = c(6, 5, 4, 2))
    bar_positions <- barplot(
      role_counts,
      col = "#6a5acd",
      border = NA,
      ylab = "Players",
      main = "Player Roles in the Cricket Dataset",
      ylim = c(0, max(role_counts) * 1.2)
    )
    text(
      x = bar_positions,
      y = role_counts,
      labels = as.integer(role_counts),
      pos = 3,
      cex = 0.9
    )
  })
}

save_scatter_plot <- function(data, output_file) {
  role_colors <- c(
    "Batsman" = "#1f77b4",
    "Wk-Batsman" = "#2ca02c",
    "All-rounder" = "#ff7f0e",
    "Bowler" = "#d62728"
  )
  point_colors <- role_colors[data$role]
  point_sizes <- 0.8 + pmin(data$sixes, 30) / 20

  save_png(output_file, plot_fun = function() {
    par(mar = c(5, 5, 4, 2))
    plot(
      data$runs,
      data$strike_rate,
      pch = 19,
      col = point_colors,
      cex = point_sizes,
      xlab = "Runs Scored",
      ylab = "Strike Rate",
      main = "Runs vs Strike Rate",
      xlim = c(0, max(data$runs) * 1.1),
      ylim = c(min(data$strike_rate) * 0.9, max(data$strike_rate) * 1.1)
    )
    abline(h = pretty(data$strike_rate), col = "#dddddd", lty = 3)
    abline(v = pretty(data$runs), col = "#dddddd", lty = 3)
    legend(
      "bottomright",
      legend = names(role_colors),
      col = role_colors,
      pch = 19,
      bty = "n",
      title = "Role"
    )

    top_batters <- head(order_desc(data, "runs"), 5)
    text(
      x = top_batters$runs,
      y = top_batters$strike_rate,
      labels = top_batters$player,
      pos = 3,
      cex = 0.7
    )
  })
}

save_team_plot <- function(data, value_column, title, ylab, output_file, fill_color) {
  team_summary <- aggregate(data[[value_column]], by = list(team = data$team), FUN = sum)
  names(team_summary)[2] <- value_column
  team_summary <- order_desc(team_summary, value_column)

  save_png(output_file, plot_fun = function() {
    par(mar = c(10, 5, 4, 2))
    bar_positions <- barplot(
      team_summary[[value_column]],
      names.arg = team_summary$team,
      las = 2,
      col = fill_color,
      border = NA,
      ylab = ylab,
      main = title,
      ylim = c(0, max(team_summary[[value_column]]) * 1.2),
      cex.names = 0.85
    )
    text(
      x = bar_positions,
      y = team_summary[[value_column]],
      labels = team_summary[[value_column]],
      pos = 3,
      cex = 0.8
    )
  })

  team_summary
}

write_summary_report <- function(data, runs_by_team, wickets_by_team, output_file) {
  top_run_scorer <- data[which.max(data$runs), ]
  top_wicket_taker <- data[which.max(data$wickets), ]
  role_counts <- sort(table(data$role), decreasing = TRUE)
  batting_top_5 <- head(order_desc(data, "runs"), 5)
  bowling_top_5 <- head(order_desc(data, "wickets"), 5)

  lines <- c(
    "Cricket Info Visualization Report",
    "=================================",
    "",
    paste("Total players analyzed:", nrow(data)),
    paste("Unique teams:", length(unique(data$team))),
    paste("Average strike rate:", sprintf("%.2f", mean(data$strike_rate))),
    paste("Top run scorer:", top_run_scorer$player, paste0("(", top_run_scorer$runs, " runs)")),
    paste("Top wicket taker:", top_wicket_taker$player, paste0("(", top_wicket_taker$wickets, " wickets)")),
    paste("Best batting team:", runs_by_team$team[1], paste0("(", runs_by_team[[2]][1], " runs)")),
    paste("Best bowling team:", wickets_by_team$team[1], paste0("(", wickets_by_team[[2]][1], " wickets)")),
    "",
    "Player roles:",
    paste0("- ", names(role_counts), ": ", as.integer(role_counts), collapse = "\n"),
    "",
    "Top 5 run scorers:",
    paste0("- ", batting_top_5$player, ": ", batting_top_5$runs, " runs", collapse = "\n"),
    "",
    "Top 5 wicket takers:",
    paste0("- ", bowling_top_5$player, ": ", bowling_top_5$wickets, " wickets", collapse = "\n")
  )

  writeLines(lines, output_file)
}

main <- function(
  input_path = "data/cricket_info.csv",
  output_dir = "output"
) {
  data <- read_cricket_data(input_path)
  ensure_output_dir(output_dir)

  save_horizontal_barplot(
    data = data,
    value_column = "runs",
    title = "Top Run Scorers",
    xlab = "Runs",
    output_file = file.path(output_dir, "top_run_scorers.png"),
    fill_color = "#1f77b4"
  )

  save_horizontal_barplot(
    data = data,
    value_column = "wickets",
    title = "Top Wicket Takers",
    xlab = "Wickets",
    output_file = file.path(output_dir, "top_wicket_takers.png"),
    fill_color = "#d62728"
  )

  save_scatter_plot(
    data = data,
    output_file = file.path(output_dir, "runs_vs_strike_rate.png")
  )

  runs_by_team <- save_team_plot(
    data = data,
    value_column = "runs",
    title = "Team Runs",
    ylab = "Total Runs",
    output_file = file.path(output_dir, "team_runs.png"),
    fill_color = "#2ca02c"
  )

  wickets_by_team <- save_team_plot(
    data = data,
    value_column = "wickets",
    title = "Team Wickets",
    ylab = "Total Wickets",
    output_file = file.path(output_dir, "team_wickets.png"),
    fill_color = "#ff7f0e"
  )

  save_role_distribution(
    data = data,
    output_file = file.path(output_dir, "role_distribution.png")
  )

  write_summary_report(
    data = data,
    runs_by_team = runs_by_team,
    wickets_by_team = wickets_by_team,
    output_file = file.path(output_dir, "cricket_summary.txt")
  )

  cat("Cricket visualization project complete.\n")
  cat("Artifacts saved to:", normalizePath(output_dir), "\n")

  invisible(list(
    data = data,
    runs_by_team = runs_by_team,
    wickets_by_team = wickets_by_team
  ))
}

