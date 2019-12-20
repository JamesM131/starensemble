old <- old_comparison_table %>%
  ungroup() %>%
  # select(-c(name, date_time)) %>%
  rename(old_prediction = predicted, old_difference = difference)
new <- comparison_table %>%
  rename(new_prediction = prediction, new_difference = difference) %>%
  select(-actual)



compare <- bind_cols(comparison_data, comparison_table) %>%
  left_join(old_comparison_table, by = c("date_time", "name")) %>%
  select(date_time, name, actual = actual.x, new_prediction = prediction, old_prediction = predicted, new_difference = difference.x, old_difference = difference.y, everything(), -c(lon, lat, actual.y, max_temperature))


compare %>%
  filter(old_difference >=5 | is.nan(old_difference)) %>%
  ggplot(aes(x = new_difference)) +
  geom_histogram()


compare %>%
  ggplot(aes(x = old_difference, y = new_difference, colour = actual)) +
  geom_point()


compare %>%
  rowid_to_column() %>%
  filter(new_difference >= 7)  %>% View()


bind_cols(old, new) %>%
  pivot_longer(ends_with("difference"), names_to = "Version") %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~Version)

compare %>%
  filter(is.nan(old_prediction)) %>%
  ggplot(aes(x = new_difference)) +
  geom_histogram(bins = 16)



compare %>%
  select(ends_with("prediction")) %>%
  pivot_longer(cols = 1:2) %>%
  mutate(NA_Value = is.na(value)) %>%
  group_by(name) %>%
  summarise(failed_predictions = sum(NA_Value), total_predictions = n()) %>%
  mutate(percent_failed = scales::percent(failed_predictions/total_predictions))


comparison_data[20, ]

View(partition_list[[52]])




old_comparison_table


comparison_table

a <- partition_list[[86]][[1]][[4]][[1]]

fit <- loess(max_temperature ~ dist, data = a, weights = 1/(perp_dist), span = 0.9)
fit_data <- fit %>%
  broom::augment()

modelr::rmse(fit, a)

cor(a$dist, a$max_temperature)

actual <- compare[86 ,"actual"][[1]]
a %>% ggplot(aes(x = dist, y = max_temperature)) +
  geom_point(aes(colour = perp_dist)) +
  geom_ribbon(data = fit_data, aes(x = dist, ymin = .fitted - .se.fit, ymax = .fitted + .se.fit), fill = "grey70", alpha = 0.6) +
  geom_line(data = fit_data, aes(x = dist, y = .fitted, colour = 1)) +
  geom_point(data = modelr::add_predictions(tibble(dist = 0), fit), aes(x = dist, y = pred), colour = "red") +
  geom_point(data = tibble(dist = 0, pred = actual), aes(x = dist, y = pred), colour = "green")

