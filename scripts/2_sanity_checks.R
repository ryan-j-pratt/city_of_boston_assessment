
source("scripts/0_setup.R")

capital_data <- read_rds("data/clean/capital_data.rds")


# Total Project Budget Decomposition -------------------------------------------

sums <- capital_data |> 
  mutate(
    auth_total = across(starts_with("auth")) |> rowSums(),
    grant_budget_total = across(
      c("grant_existing", "grant_new", "grant_future")
    ) |> rowSums(),
    go_total = across(starts_with("go")) |> rowSums(),
    grant_exp_total = across(
      c("grant_expended", "grant_year_0", "grant_year_1", "grant_year_25")
    ) |> rowSums(),
    auth_mismatch_flag = ifelse(auth_total == go_total, 0, 1),
    grant_mismatch_flag = ifelse(grant_budget_total == grant_exp_total, 0, 1),
    total_project_budget_allocated = across(
      c("auth_total", "grant_budget_total", "external_funds")
    ) |> rowSums(),
    total_mismatch_flag = ifelse(total_project_budget == total_project_budget_allocated, 0, 1)
  )

sum_checks <- sums |> 
  summarize(
    check_auth = sum(auth_mismatch_flag), 
    check_grant = sum(grant_mismatch_flag),
    check_total = sum(total_mismatch_flag)
  )
