
library(dplyr)
library(data.table)
library(duckdb)
library(ggplot2)
library(microbenchmark)
library(profvis)

# Function to generate random categories
random_categories <- function(size, num_categories) {
  sapply(1:num_categories, function(x) paste(sample(c(letters, LETTERS), size, replace = TRUE), collapse = ""))
}

# Function to generate synthetic data
generate_data <- function(n, num_categories) {
  set.seed(42)
  categories1 <- random_categories(5, num_categories)
  categories2 <- random_categories(5, num_categories)
  categories3 <- random_categories(5, num_categories)
  
  data <- data.frame(
    category1 = sample(categories1, n, replace = TRUE),
    category2 = sample(categories2, n, replace = TRUE),
    category3 = sample(categories3, n, replace = TRUE),
    value = runif(n, min = 0, max = 100)
  )
  
  data
}

# Define the size of the data
n <- 100e6
num_categories <- 100

# Generate the DataFrame
df <- generate_data(n, num_categories)
df_dt <- as.data.table(df)

# Implementation with dplyr
dplyr_groupby <- function(df, group_cols) {
  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      sum_value = sum(value),
      mean_value = mean(value),
      std_value = sd(value),
      .groups = 'drop'
    ) %>%
    arrange(across(all_of(group_cols)))
}

# Implementation with data.table
datatable_groupby <- function(df, group_cols) {
  df[, .(
    sum_value = sum(value),
    mean_value = mean(value),
    std_value = sd(value)
  ), by = group_cols][order(group_cols)]
}

# Implementation with DuckDB
duckdb_conn <- dbConnect(duckdb::duckdb(), ":memory:")

duckdb_groupby <- function(df, group_cols) {
  dbWriteTable(duckdb_conn, "df", df, overwrite = TRUE)
  group_by_str <- paste(group_cols, collapse = ", ")
  query <- sprintf(
    "SELECT %s, 
            SUM(value) as sum_value, 
            AVG(value) as mean_value, 
            STDDEV(value) as std_value 
     FROM df 
     GROUP BY %s 
     ORDER BY %s",
    group_by_str, group_by_str, group_by_str
  )
  dbGetQuery(duckdb_conn, query)
}

# Different groupby configurations to test
groupby_configs <- list(
  c("category1"),
  c("category1", "category2"),
  c("category1", "category2", "category3")
)

# Function to run benchmarks and visualize results
run_benchmarks <- function() {
  results <- list()
  
  for (config in groupby_configs) {
    config_str <- paste(config, collapse = "_")
    
    dplyr_time <- microbenchmark(dplyr_groupby(df, config), times = 5)
    dt_time <- microbenchmark(datatable_groupby(df_dt, config), times = 5)
    duckdb_time <- microbenchmark(duckdb_groupby(df, config), times = 5)
    
    dplyr_mem <- profvis::profvis({
      dplyr_groupby(df, config)
    })
    
    dt_mem <- profvis::profvis({
      datatable_groupby(df_dt, config)
    })
    
    duckdb_mem <- profvis::profvis({
      duckdb_groupby(df, config)
    })
    
    results[[config_str]] <- list(
      dplyr_time = summary(dplyr_time)$mean,
      dt_time = summary(dt_time)$mean,
      duckdb_time = summary(duckdb_time)$mean,
      dplyr_mem = dplyr_mem$prof_output,
      dt_mem = dt_mem$prof_output,
      duckdb_mem = duckdb_mem$prof_output
    )
    
    print(sprintf("\nGroupby with columns: %s", config_str))
    print(sprintf("dplyr: %.4f ms", summary(dplyr_time)$mean))
    print(sprintf("data.table: %.4f ms", summary(dt_time)$mean))
    print(sprintf("DuckDB: %.4f ms", summary(duckdb_time)$mean))
  }
  
  results
}

results <- run_benchmarks()

# Visualization
df_results <- do.call(rbind, lapply(names(results), function(config) {
  data.frame(
    config = config,
    method = c("dplyr", "data.table", "DuckDB"),
    time = c(results[[config]]$dplyr_time, results[[config]]$dt_time, results[[config]]$duckdb_time)
  )
}))

ggplot(df_results, aes(x = config, y = time, fill = method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Performance Comparison: Execution Time", y = "Execution Time (ms)", x = "Groupby Configuration") +
  theme_minimal()
