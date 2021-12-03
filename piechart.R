

df_donutchart <- 
  adm_pop_long %>% 
  filter(states == "Alabama" &
  adm_or_pop == "Admissions",
  year == 2020) %>% 
  group_by(metric, year) %>% 
  summarise(total = sum(total)) %>% 
  filter(metric == "Other" | metric == "Supervision Violations") %>% select(-year)

df_donutchart$overall <- sum(df_donutchart$total)

df_donutchart <- df_donutchart %>% group_by(metric) %>% 
  mutate(percentage = total / overall,
         hover_text = paste0(metric, ": ", total)) %>%
  mutate(percentage_label = paste0(round(100 * percentage, 0), "%")) %>% 
  select(-overall)

df_donutchart <- as.data.frame(df_donutchart)

donut_plot <- ggplot(df_donutchart, aes(y = total, fill = metric)) +
  geom_bar_interactive(
    aes(x = 1, tooltip = hover_text),
    width = 0.5,
    stat = "identity",
    show.legend = FALSE
  ) +
  annotate(
    geom = "text",
    x = 0,
    y = 0,
    label = df_donutchart[["percentage_label"]][df_donutchart[["metric"]] == "Supervision Violations"],
    size = 20,
    color = "#E18731"
  ) +
  scale_fill_manual(values = c(Other = "#DAEAF2", `Supervision Violations` = "#E18731")) +
  coord_polar(theta = "y") +
  theme_void()

ggiraph(ggobj = donut_plot)

