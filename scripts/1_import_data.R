
source("scripts/0_setup.R")

raw <- read_excel("data/raw/fy26-30-adopted-capital-open-data.xlsx") |> 
  clean_names() |> 
  ## Rename to align with data dictionary
  rename(
    pm_dept = pm_department,
    auth_existing = authorization_existing,
    auth_new = authorization_fy,
    auth_future = authorization_future,
    grant_new = grant_fy,
    go_year_0 = capital_year_0,
    go_year_1 = capital_year_1,
    go_year_25 = capital_year_25,
  )

write_rds(raw, "data/clean/capital_data.rds")
