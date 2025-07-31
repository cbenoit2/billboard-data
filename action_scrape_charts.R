# Scrape billboard charts

## Can scrape current charts based on a list of chart slugs.

# Setup
library(tidyverse)
library(rvest)
library(lubridate)
library(here)
library(stringr)

charts_list = c("hot-100", "billboard-200")

for (chart in charts_list) {
  # make the url
  scrape_url = paste0("https://www.billboard.com/charts/", chart)
  
  # get initial scrape
  scrape_first = read_html(scrape_url)
  
  # pull date from scrape
  chart_date = scrape_first |> 
    html_element("div#chart-date-picker") |> 
    html_attr("data-date")
  
  
  # pull year from scrape
  chart_year = year(chart_date)
  
  # process results into a tibble
  scrape_tibble = scrape_first |>
    html_elements("ul.o-chart-results-list-row") |>
    html_text2() |> 
    as_tibble() |>
    rename(raw_text = value)
  
  # clean up the tibble
  scrape_clean = scrape_tibble |>
    mutate(
      # Remove "NEW" and "RE-ENTRY" indicators
      data_cleaned = str_remove_all(raw_text, " NEW\n|NEW"),
      data_cleaned = str_remove_all(data_cleaned, " RE- ENTRY\n|RE- ENTRY|RE-ENTRY"),
      
      # Split the cleaned string into lines for easier extraction
      lines = str_split(data_cleaned, "\n"),
      
      # Extract current_week
      current_week = map_chr(lines, ~ .x[1]),
      
      # Extract title
      title = map_chr(lines, ~ .x[2]),
      
      # Extract performer
      performer = map_chr(lines, ~ .x[3]),
      
      # Extract last_week, peak_pos, and wks_on_chart
      last_week = map_chr(data_cleaned, ~ {
        val = str_extract(.x, "(?<=LW\\n)\\d+")
        if (is.na(val)) "-" else val
      }),
      peak_pos = map_chr(data_cleaned, ~ {
        str_extract(.x, "(?<=PEAK\\n)\\d+")
      }),
      wks_on_chart = map_chr(data_cleaned, ~ {
        str_extract(.x, "(?<=WEEKS\\n)\\d+")
      })
    ) |>
    select(current_week, title, performer, last_week, peak_pos, wks_on_chart) |>
    mutate(chart_week = chart_date) |>
    select(chart_week, everything())
  
  # name path to save file
  folder_path = paste0("data-scraped/", chart, "/", chart_year, "/")
  
  # create the directory if it doesn't exist
  if (!dir.exists(here(folder_path))) {
    dir.create(here(folder_path), recursive=TRUE)
  }
  
  # write the file
  scrape_clean |> write_csv(here(folder_path, paste0(chart_date, ".csv")))
}
