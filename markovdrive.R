library(nflfastR)
library(dplyr)
library(markovchain)
library(gt)
library(gtExtras)
library(ggplot2)
library(ggpmisc)
library(ggpath)
library(ggtext)

pbp_data <- load_pbp(2001:2023) %>%
  filter(!is.na(fixed_drive_result), !is.na(down), !is.na(yardline_100), !is.na(ydstogo)) %>%
  mutate(yardline_100_group_num = ceiling(yardline_100/5), yardline_100_group = paste0(yardline_100_group_num * 5 - 4, "-", yardline_100_group_num * 5),
         ydstogo_group = case_when(
           ydstogo <= 3 ~ "1-3",
           ydstogo <= 7 ~ "4-7",
           ydstogo <= 10 ~ "8-10",
           ydstogo <= 15 ~ "11-15",
           ydstogo <= 20 ~ "16-20",
           ydstogo > 20 ~ ">20"
         ), state = paste0(down, "/", ydstogo_group, "/", yardline_100_group))


condensed_data <- pbp_data %>%
  select(play_id, game_id, drive, state, fixed_drive_result)

absorptions <- condensed_data %>%
  group_by(game_id, drive) %>%
  summarize(state = last(fixed_drive_result))

condensed_data <- condensed_data %>% mutate(is_absorption = 0)
absorptions <- absorptions %>% mutate(is_absorption = 1)

condensed_data <- condensed_data %>% select(-fixed_drive_result)

full_data <- rbind(condensed_data %>% select(-play_id), absorptions) %>%
  group_by(game_id, drive) %>%
  arrange(game_id, drive, is_absorption) %>%
  select(-is_absorption) %>%
  mutate(next_state = lead(state)) %>%
  ungroup() 

unique_states <- full_data %>%
  count(state)

transition_matrix <- matrix(0, nrow = length(unique_states$state), ncol = length(unique_states$state), dimnames = list(unique_states$state, unique_states$state))

for (i in 1:nrow(full_data)) {
  current_state <- full_data$state[i]
  next_state <- full_data$next_state[i]
  
  if (!is.na(next_state)) {
    ci <- match(current_state, unique_states$state)
    ni <- match(next_state, unique_states$state)
    transition_matrix[ci, ni] <- transition_matrix[ci, ni] + 1
  }
}

for (i in 1:nrow(transition_matrix)) {
  transition_matrix[i, ] <- transition_matrix[i, ] / sum(transition_matrix[i, ])
}

transition_matrix[is.nan(transition_matrix)] <- 0

absorption_states <- unique_states$state[(nrow(unique_states) - 8):nrow(unique_states)]

for (state in absorption_states) {
  transition_matrix[state, state] <- 1
}

mc <- new("markovchain", states = unique_states$state, transitionMatrix = transition_matrix)

absorption_probs <- absorptionProbabilities(mc) 
absorption_probs_df <- as.data.frame(absorption_probs)
absorption_probs_df$state <- rownames(absorption_probs)

absorption_probs_df <- absorption_probs_df %>%
  mutate(markov_ep = 7 * Touchdown + 3 * `Field goal` - 2 * Safety - 7 * `Opp touchdown`)

absorption_data <- absorption_probs_df %>% select(state, markov_ep)

markov_pbp <- left_join(pbp_data, absorption_data, by = "state") %>%
  group_by(game_id, drive) %>%
  mutate(epa = ep - lag(ep), markov_epa = markov_ep - lag(markov_ep), abs_diff = abs(epa - markov_epa)) %>%
  select(play_id, game_id, posteam, defteam, desc, state, ep, markov_ep, epa, markov_epa, abs_diff)

off_epa_leaders <- markov_pbp %>%
  group_by(posteam) %>%
  summarize(off_markov_epa = mean(markov_epa, na.rm = TRUE), off_epa = mean(epa, na.rm = TRUE)) %>%
  arrange(-off_markov_epa) %>%
  mutate(off_markov_epa = round(off_markov_epa, 3), off_epa = round(off_epa, 3))

def_epa_leaders <- markov_pbp %>%
  group_by(defteam) %>%
  summarize(def_markov_epa = mean(markov_epa, na.rm = TRUE), def_epa = mean(epa, na.rm = TRUE)) %>%
  arrange(def_markov_epa) %>%
  mutate(def_markov_epa = round(def_markov_epa, 3), def_epa = round(def_epa, 3))

logos <- teams_colors_logos %>%
  select(team = team_abbr, team_name, team_logo_espn)

off_epa_leaders <- left_join(off_epa_leaders, logos, by = c("posteam"="team")) %>%
  select(team_logo_espn, team_name, off_markov_epa, off_epa)

def_epa_leaders <- left_join(def_epa_leaders, logos, by = c("defteam"="team")) %>%
  select(team_logo_espn, team_name, def_markov_epa, def_epa)

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption = gt_align_caption("Data from <b>nflverse</b>", "Amrit Vignesh | <b>@avsportsanalyst</b>")

off_epa_table <- off_epa_leaders %>% gt() %>%
  gt_img_rows(columns = team_logo_espn, height = 40) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(team_logo_espn, team_name, off_markov_epa, off_epa)
  ) %>%
  gt_hulk_col_numeric(c(off_markov_epa, off_epa)) %>%
  cols_label(
    team_logo_espn = md(""),
    team_name = md("**Team**"),
    off_markov_epa = md("**Markov EPA/Play**"),
    off_epa = md("**XGB EPA/Play**"),
  ) %>%
  tab_header(
    title = "2001-2023 NFL Offensive EPA/Play Leaders",
    subtitle = md("*Using a **Markov Chain** In Comparison To The **XGB** Model*")
  ) %>%
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(team_name, off_markov_epa, off_epa)
    )
  )

gtsave(off_epa_table, "off_epa_table.png", vwidth = 2500, vheight = 2000)

def_epa_table <- def_epa_leaders %>% gt() %>%
  gt_img_rows(columns = team_logo_espn, height = 40) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(team_logo_espn, team_name, def_markov_epa, def_epa)
  ) %>%
  gt_hulk_col_numeric(c(def_markov_epa, def_epa), reverse = TRUE) %>%
  cols_label(
    team_logo_espn = md(""),
    team_name = md("**Team**"),
    def_markov_epa = md("**Markov EPA/Play**"),
    def_epa = md("**XGB EPA/Play**"),
  ) %>%
  tab_header(
    title = "2001-2023 NFL Defensive EPA/Play Leaders",
    subtitle = md("*Using a **Markov Chain** In Comparison To The **XGB** Model*")
  ) %>%
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(team_name, def_markov_epa, def_epa)
    )
  )

gtsave(def_epa_table, "def_epa_table.png", vwidth = 2500, vheight = 2000)

off_epa_plot <- off_epa_leaders %>%
  ggplot(aes(x = off_epa, y = off_markov_epa)) +
  geom_hline(yintercept = mean(off_epa_leaders$off_markov_epa), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(off_epa_leaders$off_epa), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_smooth(method = "lm") + 
  stat_poly_eq() +
  geom_from_path(aes(path = team_logo_espn), height = 0.075) +
  labs(x = "XGB EPA/Play",
       y = "Markov EPA/Play",
       title = "Comparing Offensive EPA/Play: Markov Chain vs XGBoost",
       caption = "Data from **nflverse** | Amrit Vignesh | **@avsportsanalyst**") + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), plot.caption = element_markdown(hjust = 0.5)) 

ggsave("off_epa_plot.png", off_epa_plot)

def_epa_plot <- def_epa_leaders %>%
  ggplot(aes(x = def_epa, y = def_markov_epa)) +
  geom_hline(yintercept = mean(def_epa_leaders$def_markov_epa), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(def_epa_leaders$def_epa), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_smooth(method = "lm") + 
  stat_poly_eq() +
  geom_from_path(aes(path = team_logo_espn), height = 0.075) +
  labs(x = "XGB EPA/Play",
       y = "Markov EPA/Play",
       title = "Comparing Defensive EPA/Play: Markov Chain vs XGBoost",
       caption = "Data from **nflverse** | Amrit Vignesh | **@avsportsanalyst**") + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), plot.caption = element_markdown(hjust = 0.5)) 

ggsave("def_epa_plot.png", def_epa_plot)