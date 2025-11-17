
source("scripts/0_setup.R")

capital_data <- read_rds("data/clean/capital_data.rds")


# Exposure to grant funding ----------------------------------------------------

make_plots <- function(exclude_bps = FALSE) {
  
  if (exclude_bps == TRUE) {
    capital_data_filtered <- capital_data |> 
      filter(department != "Boston Public Schools")
  } else {
    capital_data_filtered <- capital_data
  }
  
  summary_phase <- capital_data_filtered |> 
    group_by(project_status) |>
    summarize(
      sum_grant_year_25 = sum(grant_year_25),
      sum_grant_year_1 = sum(grant_year_1),
      sum_grant_year_0 = sum(grant_year_0),
      sum_grant_expended = sum(grant_expended)
    ) |> 
    mutate(
      project_status = factor(project_status, levels = c(
        "Study Underway",
        "New Project",
        "To Be Scheduled",
        "Implementation Underway",
        "In Design",
        "In Construction",
        "Annual Program"
      )),
      project_status = fct_recode(
        project_status,
        "Implementation\nUnderway" = "Implementation Underway"
      )
    )
  
  summary_phase_long <- summary_phase |> 
    pivot_longer(
      cols = starts_with("sum"),
      names_to = "grant_label",
      names_prefix = "sum_grant_",
      values_to = "grant_value"
    ) |> 
    mutate(
      grant_label = fct_inorder(grant_label),
      grant_label = fct_recode(
        grant_label,
        "Planned for future" = "year_25",
        "Budgeted for FY26" = "year_1",
        "Budgeted for FY25" = "year_0",
        "Already spent" = "expended"
      )
    )
  
  output_2 <- ggplot(summary_phase_long, aes(fill=grant_label, y=grant_value, x=project_status)) + 
    geom_bar(position="stack", stat="identity") +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0, size = 18)
    ) +
    scale_fill_manual(values = c("#D2D2D2","#1871bd","#061622","#58585B")) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.01)),
      labels = scales::label_currency(scale = 1/1000000),
      limits = c(0, 175000000),
      n.breaks = 7
    ) +
    labs(
      title = "The Capital Plan May Be Exposed To Short-Term Grant Volatility",
      subtitle = ifelse(exclude_bps == TRUE, "Excluding Boston Public Schools Projects", ""),
      y = "Grant Spending (in millions)",
      x = NULL,
      fill = "Portion of Funding Which Is:"
    )
  
}

ggsave("export/figure_2a.pdf", make_plots(FALSE), width = 11, height = 8.5)
ggsave("export/figure_2b.pdf", make_plots(TRUE), width = 11, height = 8.5)
