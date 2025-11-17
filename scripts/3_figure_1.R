
source("scripts/0_setup.R")

capital_data <- read_rds("data/clean/capital_data.rds")

# Current vs. Future Funding ---------------------------------------------------

summary_all <- capital_data |>
  summarize(
    total_auth_future = sum(auth_future),
    total_bonds = sum(auth_existing) + sum(auth_new) + sum(auth_future),
    frac_auth_future = sum(auth_future)/total_bonds,
    frac_auth_new = sum(auth_new)/total_bonds,
    frac_auth_existing = sum(auth_existing)/total_bonds
  ) |>
  mutate(
    department = "Citywide",
    project_name = "Citywide",
  )

summary_department <- capital_data |> 
  group_by(department) |>
  summarize(
    total_auth_future = sum(auth_future),
    total_bonds = sum(auth_existing) + sum(auth_new) + sum(auth_future),
    frac_auth_future = sum(auth_future)/total_bonds,
    frac_auth_new = sum(auth_new)/total_bonds,
    frac_auth_existing = sum(auth_existing)/total_bonds
  ) |> 
  filter(frac_auth_future > 0) |> 
  arrange(frac_auth_future, desc(frac_auth_existing)) |> 
  bind_rows(summary_all) |> 
  mutate(department = fct_inorder(str_wrap(department, width = 12)),
         department = fct_relevel(department, "Citywide", after = 0))

summary_department_long <- summary_department |> 
  pivot_longer(
    cols = starts_with("frac"),
    names_to = "frac_label",
    names_prefix = "frac_",
    values_to = "frac_value"
  ) |> 
  mutate(
    frac_label = fct_inorder(frac_label),
    frac_label = fct_recode(
      frac_label, 
      "Planned for future authorization" = "auth_future",
      "Authorized this year" = "auth_new",
      "Already authorized" = "auth_existing"
    ),
    label_y = ifelse(frac_label == "Already authorized", 1, NA),
    total_bonds = ifelse(frac_label == "Already authorized", paste("$",format(round(total_bonds/1000000), big.mark = ",")), NA),
  )

output_1a <- ggplot(summary_department_long, aes(fill=frac_label, y=frac_value, x=department)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(y = label_y, label = total_bonds, vjust = -0.5)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0, size = 18)
  ) +
  scale_fill_manual(values = c("#D2D2D2","#1871bd","#58585B")) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .03)),
    labels = scales::label_percent()
  ) +
  labs(
    title = "The Capital Plan Includes Unfunded Future Commitments",
    subtitle = "Total Planned Authorizations (in millions)",
    y = NULL,
    x = NULL,
    fill = "Portion of Funding Which Is:"
  )

ggsave("export/figure_1a.pdf", output_1a, width = 11, height = 8.5)

make_department_plot <- function(department_name) {
  
  string_department_name <- glue("{str_wrap(department_name, width = 12)}")
  
  summary_department_to_bind <- summary_department |> 
    filter(department == string_department_name) |> 
    mutate(project_name = string_department_name)
  
  summary_one_department <- capital_data |> 
    filter(department == department_name) |>
    mutate(
      total_bonds = auth_existing + auth_new + auth_future,
      frac_auth_future = auth_future/total_bonds,
      frac_auth_new = auth_new/total_bonds,
      frac_auth_existing = auth_existing/total_bonds
    ) |> 
    filter(frac_auth_future > 0) |> 
    arrange(frac_auth_future, desc(frac_auth_existing)) |> 
    bind_rows(summary_all) |> 
    bind_rows(summary_department_to_bind) |> 
    mutate(project_name = fct_inorder(str_wrap(project_name, width = 12)),
           project_name = fct_relevel(project_name, "Citywide", after = 0),
           project_name = fct_relevel(project_name, string_department_name, after = 1))
  
  summary_one_department_long <- summary_one_department |> 
    pivot_longer(
      cols = starts_with("frac"),
      names_to = "frac_label",
      names_prefix = "frac_",
      values_to = "frac_value"
    ) |> 
    mutate(
      frac_label = fct_inorder(frac_label),
      frac_label = fct_recode(
        frac_label, 
        "Planned for future authorization" = "auth_future",
        "Authorized this year" = "auth_new",
        "Already authorized" = "auth_existing"
      ),
      label_y = ifelse(frac_label == "Already authorized", 1, NA),
      total_bonds = ifelse(frac_label == "Already authorized", paste("$",format(round(total_bonds/1000000), big.mark = ",")), NA),
    )
  
  plot <- ggplot(summary_one_department_long, aes(fill=frac_label, y=frac_value, x=project_name)) + 
    geom_bar(position="stack", stat="identity") +
    geom_text(aes(y = label_y, label = total_bonds, vjust = -0.5)) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0, size = 18)
    ) +
    scale_fill_manual(values = c("#D2D2D2","#1871bd","#58585B")) +
    scale_y_continuous(
      expand = expansion(mult = c(0, .03)),
      labels = scales::label_percent()
    ) +
    labs(
      title = str_wrap(glue("The Capital Plan Includes Unfunded Future Commitments for the {department_name}"), width = 86),
      subtitle = "Total Planned Authorizations (in millions)",
      y = NULL,
      x = NULL,
      fill = "Portion of Funding Which Is:"
    )
}

unfunded_departments <- c(
  "Boston Planning and Development Agency",
  "Boston Public Library",
  "Boston Public Schools",
  "Environment Department",
  "Fire Department",
  "Parks and Recreation Department",
  "Public Health Commission",
  "Public Works Department"
)

output_1b_i <- map(unfunded_departments, make_department_plot)

ggsave("export/figure_1b_i.pdf", output_1b_i, width = 11, height = 8.5)
