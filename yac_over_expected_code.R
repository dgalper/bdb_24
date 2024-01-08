# Read in packages
library(tidyverse)
library(tidytext)
library(dlookr)  
library(nflverse)
library(ggthemes)
library(tidyverse)
library(nflfastR)
library(ggthemes)
library(nflplotR)
library(modelr)
library(ggimage)
library(gt)
library(gtExtras)
library(coefplot)


# Read in all the data
games_df <- read_csv("games.csv")

players_df <- read_csv("players.csv")

plays_df <- read_csv("plays.csv")

tackles_df <- read_csv("tackles.csv")

# Make master tracking df
tracking_df <- rbind(read_csv("tracking_week_1.csv"),read_csv("tracking_week_2.csv"), 
                     read_csv("tracking_week_3.csv"), read_csv("tracking_week_4.csv"),
                     read_csv("tracking_week_5.csv"), read_csv("tracking_week_6.csv"),
                     read_csv("tracking_week_7.csv"), read_csv("tracking_week_8.csv"),
                     read_csv("tracking_week_9.csv"))


# Make master df
master_df <- tracking_df %>%
  left_join(plays_df, by = c("gameId", "playId")) %>%
  left_join(tackles_df, by = c("gameId", "playId", "nflId")) %>%
  left_join(games_df, by = c("gameId"))


# This function grabs the coordinates and movement info of the ball carrier on each possession
add_ball_carrier_tracking <- function(df) {
  bc_df <- df %>%
    filter(displayName == ballCarrierDisplayName) %>%
    select(gameId, playId, frameId, ballCarrierDisplayName, bc_x = x, bc_y = y, bc_s = s, bc_a = a, bc_dis = dis, bc_o = o, bc_dir = dir) 
  
  df %>%
    left_join(bc_df, by = c("gameId", "playId", "frameId", "ballCarrierDisplayName"))
}

# Add in ball carrier tracking and find distance from bc
master_df <- add_ball_carrier_tracking(master_df) %>%
  mutate(dist_from_bc = sqrt((x - bc_x)**2 + (y - bc_y)**2))




# If team is driving towards home endzone, then 270 degress is exactly towards LOS
# If team is driving towards away endzone, then 90 degrees is exactly towards LOS
# Let's use the direction of the ball carrier at the snap to figure out which way the LOS is
# Write a function to get the orientation based off the ball carrier
add_orientation <- function(df) {
  or_df <- df %>%
    filter(ballCarrierId == nflId,
           frameId == 1) %>%
    mutate(towards_home = if_else(o >= 180, 1, 0)) %>%
    select(gameId,playId,towards_home)
  df %>%
    left_join(or_df,  by = c("gameId", "playId"))
}

# Add in orientation
master_df <- add_orientation(master_df)


# Add orientations and player motions for bc and defender 
# Ideal orientation for a ball carrier when being tackled is 0
# Ideal orientation for a defender when making a tackle is 180
# A tackle with defender 90 degree orientation and motion is from the side
# A tackle with defender 0 degree orientation and motion is a tackle from behind
master_df <- master_df %>%
  mutate(def_o_deg_from_ez = if_else(towards_home == 1, abs(270 - o), abs(90 - o)),
         def_dir_deg_from_ez = if_else(towards_home == 1, abs(270 - dir), abs(90 - dir)),
         bc_o_deg_from_ez = if_else(towards_home == 1, abs(270 - bc_o), abs(90 - bc_o)),
         bc_dir_deg_from_ez = if_else(towards_home == 1, abs(270 - bc_dir), abs(90 - bc_dir)))


# Write a function to add in yac for each game
add_yac <- function(df){
  yac_df <- df %>%
    filter(displayName == "football",
           event %in% c("first_contact", "tackle")) %>%
    mutate(yards_after_first_contact = if_else(towards_home == 1, lag(x) - x, x - lag(x))) %>%
    filter(event == "tackle") %>%
    select(gameId,playId,yards_after_first_contact)
  df %>%
    left_join(yac_df, by = c("gameId", "playId"))
}

# Add in the yac
master_df <- add_yac(master_df)


# We have to get the player who made the first contact
# Write a function to add in the player who made first contact and how far away they were
add_first_contact <- function(df){
  first_con_df <- df %>%
    filter(event == "first_contact",
           displayName != ballCarrierDisplayName,
           displayName != "football",
           club == defensiveTeam) %>%
    group_by(gameId, playId) %>%
    summarize(min_dist_from_bc = min(dist_from_bc),
              first_contact_displayName = displayName[which.min(dist_from_bc)])
  df %>%
    left_join(first_con_df, by = c("gameId", "playId"))
}


# Add in and fix yac
master_df <- add_first_contact(master_df) %>%
  mutate(yards_after_first_contact = if_else(yards_after_first_contact < 0, 0, yards_after_first_contact))


# Filter down to first contact events
first_contact_df <- master_df %>%
  filter(event == "first_contact",
         displayName == first_contact_displayName,
         !is.na(yards_after_first_contact))

# Add in line of scrimmage variables: true LOS and location from LOS
add_los_data <- function(df){
  # Now get how far the ball traveled from the first frame to the first contact
  football_snap_df <- tracking_df %>%
    filter(displayName == "football",
           frameId == 1) %>%
    select(gameId, playId, snap_x = x)
  football_fc_df <- tracking_df %>%
    filter(displayName == "football",
           event == "first_contact") %>%
    select(gameId, playId, fc_x = x)
  los_df <- football_snap_df %>%
    left_join(football_fc_df, by = c("gameId", "playId"))
  print(los_df)
  df %>%
    left_join(los_df, by = c("gameId", "playId"))
}

# Add in los data. Then, find the yardage that the ball has traveled from the los using the orientation column
first_contact_df <- add_los_data(first_contact_df) %>%
  mutate(yards_before_fc = if_else(towards_home == 1, snap_x - fc_x, fc_x - snap_x))


# Write function to add in weights
add_weights <- function(df){
  weights <- players_df %>%
    select(nflId, weight)
  df %>%
    left_join(weights, by = "nflId") %>%
    left_join(weights, by = c("ballCarrierId" = "nflId"))
}

# Add in weights
model_ready_df <- add_weights(first_contact_df) %>%
  rename(def_weight = weight.x, bc_weight = weight.y, def_s = s)


# Build our linear regression
lm_yac <- lm(yards_after_first_contact ~ def_o_deg_from_ez + def_dir_deg_from_ez + bc_o_deg_from_ez + bc_dir_deg_from_ez + def_s + bc_s + def_weight + bc_weight + min_dist_from_bc + absoluteYardlineNumber + yards_before_fc,
             data = model_ready_df)
summary(lm_yac)


# Standardize coefficients
standardized_coefs <- scale(coef(lm_yac, scale = TRUE))

normal_coefs <- coef(lm_yac, scale = TRUE)

# Extract variable names and their standardized coefficients
var_names <- names(coefs)
var_coefs <- as.numeric(standardized_coefs)

# Make a df of scaled vals and names
scaled_coeff_df <- data.frame(names = var_names, scaled_coeff = var_coefs) %>%
  filter(names != "(Intercept)")

# Plot the scaled variable importance
scaled_coeff_df %>%
  ggplot(aes(x = scaled_coeff, y = fct_reorder(names, scaled_coeff))) +
  geom_point(size = 10, color = "lightblue") +
  theme_bw() +
  labs(x = "Scaled Variable Importance",
       y = "Variable",
       title = "YAC Over Expected Model Scaled Variable Importance",
       caption = "Daniel Galper | @Daniel_Galper | Data: Big Data Bowl 24") +
  theme(plot.title = element_text(size = 75, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 50),
        plot.caption = element_text(size = 30),
        axis.text = element_text(size = 65),
        axis.title = element_text(size = 55))

ggsave("var-importance.png", width = 14, height = 10, dpi = "retina")

# Let's use the lm to make preds and then group by defender
added_preds_df <- model_ready_df %>%
  add_predictions(lm_yac) %>%
  mutate(yac_over_expected = yards_after_first_contact - pred,
         positive_yac_oe = if_else(yac_over_expected >= 0, 1, 0))


# Let's see how defender's fared
defender_performance <- added_preds_df %>%
  group_by(club,first_contact_displayName,nflId) %>%
  summarize(total_yac_oe = sum(yac_over_expected),
            avg_yac_oe = mean(yac_over_expected),
            yac_oe_success_rate = mean(positive_yac_oe),
            plays = n()) %>%
  ungroup() 

# Let's see the best ball carriers
bc_performance <- added_preds_df %>%
  group_by(possessionTeam,ballCarrierDisplayName,ballCarrierId) %>%
  summarize(total_yac_oe = sum(yac_over_expected),
            avg_yac_oe = mean(yac_over_expected),
            yac_oe_success_rate = mean(positive_yac_oe),
            plays = n()) %>%
  ungroup() 

# Make nice tables of top and bottom 10 defenders and ball carriers
top_defenders <- defender_performance %>%
  filter(plays >= 20) %>%
  ungroup() %>%
  arrange(total_yac_oe) %>%
  head(10) %>%
  mutate(rank = row_number()) %>%
  left_join(players_df, by = c("first_contact_displayName" = "displayName")) %>%
  left_join(teams_colors_logos, by = c("club" = "team_abbr")) %>%
  select(rank,team_logo_espn,position,first_contact_displayName, plays, total_yac_oe, avg_yac_oe,yac_oe_success_rate)


top_defenders_gt <- top_defenders %>%
  gt() %>%
  cols_align(align = "center") %>%
  cols_label(first_contact_displayName = 'Player',
             rank = "Rank",
             team_logo_espn = "Team",
             position = "Pos.",
             total_yac_oe = "YAC Over Expected",
             avg_yac_oe = "YAC Over Expected/Play",
             yac_oe_success_rate = "YAC Over Expected Succ. %",
             plays = "Plays") %>%
  tab_header(title = md(" Defensive YAC Over Expected Leaders"),
             subtitle = md("Weeks 1-9, 2022 | Min. of 20 plays as first contact defender")) %>%
  gtExtras::gt_img_rows(team_logo_espn) %>% 
  gtExtras::gt_theme_espn() %>%
  opt_align_table_header(align = "center") %>%
  tab_footnote(footnote = "Daniel Galper | @Daniel_Galper") %>%
  fmt_number(columns = total_yac_oe, decimals = 1) %>%
  fmt_number(columns = avg_yac_oe, decimals = 2) %>%
  fmt_percent(columns = yac_oe_success_rate, decimals = 1) %>%
  gt_color_rows(total_yac_oe, palette = "ggsci::green_material", direction = -1) %>%
  gt_color_rows(avg_yac_oe, palette = "ggsci::green_material", direction = -1) %>%
  gt_color_rows(yac_oe_success_rate, palette = "ggsci::green_material", direction = -1) 

gtsave(top_defenders_gt, "xyac-defnders.png")


# Now do the ball carriers

top_bcs <- bc_performance %>%
  filter(plays >= 20) %>%
  arrange(-total_yac_oe) %>%
  head(10) %>%
  mutate(rank = row_number()) %>%
  left_join(players_df, by = c("ballCarrierDisplayName" = "displayName")) %>%
  left_join(teams_colors_logos, by = c("possessionTeam" = "team_abbr")) %>%
  select(rank,team_logo_espn,position,ballCarrierDisplayName, total_yac_oe, avg_yac_oe, yac_oe_success_rate,plays)



top_bc_gt <- top_bcs %>%
  gt() %>%
  cols_align(align = "center") %>%
  cols_label(ballCarrierDisplayName = 'Player',
             rank = "Rank",
             team_logo_espn = "Team",
             position = "Pos.",
             total_yac_oe = "Total YAC Over Expected",
             avg_yac_oe = "YAC Over Expected/Touch",
             yac_oe_success_rate = "YAC Over Expected Succ. %",
             plays = "Touches") %>%
  tab_header(title = md("YAC Over Expected Leaders"),
             subtitle = md("Weeks 1-9, 2022 | Min. of 20 touches")) %>%
  gtExtras::gt_img_rows(team_logo_espn) %>% 
  gtExtras::gt_theme_espn() %>%
  opt_align_table_header(align = "center") %>%
  tab_footnote(footnote = "Daniel Galper | @Daniel_Galper") %>%
  fmt_number(columns = total_yac_oe, decimals = 1) %>%
  fmt_number(columns = avg_yac_oe, decimals = 2) %>%
  fmt_percent(columns = yac_oe_success_rate, decimals = 1) %>%
  gt_color_rows(total_yac_oe, palette = "ggsci::green_material") %>%
  gt_color_rows(avg_yac_oe, palette = "ggsci::green_material") %>%
  gt_color_rows(yac_oe_success_rate, palette = "ggsci::green_material") 

gtsave(top_bc_gt, "xyac-bc.png")



# Now let's compile team performances
# Team defense
team_def_performance <- added_preds_df %>%
  group_by(club) %>%
  summarize(total_yac_oe_def = sum(yac_over_expected),
            avg_yac_oe_def = mean(yac_over_expected),
            positive_yac_oe_success_rate_def = mean(positive_yac_oe),
            plays_def = n()) %>%
  ungroup() 


# Team offense
team_off_performance <- added_preds_df %>%
  group_by(possessionTeam) %>%
  summarize(total_yac_oe_off = sum(yac_over_expected),
            avg_yac_oe_off = mean(yac_over_expected),
            positive_yac_oe_success_rate_off = mean(positive_yac_oe),
            plays_off = n()) %>%
  ungroup() 

# Join the two and add in logos
team_yac_stats <- team_off_performance %>%
  left_join(team_def_performance, by = c("possessionTeam" = "club")) %>%
  left_join(teams_colors_logos, by = c("possessionTeam" = "team_abbr"))

# Now, let's look at which teams won the yac battle
team_yac_stats %>%
  ggplot(aes(x = total_yac_oe_off, y = total_yac_oe_def)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  theme_bw() +
  scale_y_reverse() +
  geom_hline(yintercept = mean(team_yac_stats$total_yac_oe_def), linetype = "dashed") +
  geom_vline(xintercept = mean(team_yac_stats$total_yac_oe_off), linetype = "dashed") +
  labs(x = "Total Off. YAC Over Expected",
       y = "Total Def. YAC Over Expected",
       title = "How Teams Created/Prevented YAC Over Expected",
       subtitle = "Weeks 1-9 of 2022 NFL season",
       caption = "Daniel Galper | @Daniel_Galper | Data: Big Data Bowl 24") +
  theme(plot.title = element_text(size = 80, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 50),
        plot.caption = element_text(size = 30),
        axis.text = element_text(size = 75),
        axis.title = element_text(size = 65))

ggsave("yac-team-level.png", width = 14, height = 10, dpi = "retina")

# Make a scatterplot of model predictions and actual yac values
added_preds_df %>%
  ggplot(aes(x = pred, y = yards_after_first_contact)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Predicted YAC",
       y = "Actual YAC",
       title = "Scatterplot of Predicted and Actual YAC Values",
       caption = "Daniel Galper | @Daniel_Galper | Data: Big Data Bowl 24") +
  theme(plot.title = element_text(size = 80, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 50),
        plot.caption = element_text(size = 30),
        axis.text = element_text(size = 75),
        axis.title = element_text(size = 65))

ggsave("yac-pred-scatterplot.png", width = 14, height = 10, dpi = "retina")


# Now let's make a scatterplot of success rate vs avg yac where dot size is touches for offense and defense
# Defender charts
defender_performance_filt <- defender_performance %>%
  filter(plays >= 20) %>%
  left_join(teams_colors_logos, by = c("club" = "team_abbr"))

defender_performance_filt %>%
  ggplot(aes(x = yac_oe_success_rate, y = avg_yac_oe, size = plays)) +
  geom_point(aes(fill = team_color, color = team_color2), stat = "identity", size = 6.5) +
  scale_color_identity(aesthetics = c("fill","color")) +
  ggrepel::geom_text_repel(aes(label = first_contact_displayName), size = 15, max.overlaps = 4) +
  scale_y_reverse() +
  scale_x_reverse() +
  geom_hline(yintercept = mean(defender_performance_filt$avg_yac_oe), linetype = "dashed") +
  geom_vline(xintercept = mean(defender_performance_filt$yac_oe_success_rate), linetype = "dashed") +
  theme_bw() +
  labs(x = "YAC Over Expected Success Rate",
       y = "YAC Over Expected/Play",
       title = "The Best and Worst Defenders at Preventing YAC Over Expected",
       subtitle = "Weeks 1-9 of 2022 NFL season, dot size based on number of plays as first contact defender",
       caption = "Daniel Galper | @Daniel_Galper | Data: Big Data Bowl 24") +
  theme(plot.title = element_text(size = 80, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 50),
        plot.caption = element_text(size = 30),
        axis.text = element_text(size = 75),
        axis.title = element_text(size = 65),
        legend.position = "none")

ggsave("def-performance-scat.png", width = 14, height = 10, dpi = "retina")

# Ball carrier charts
bc_performance_filt <- bc_performance %>%
  filter(plays >= 20) %>%
  left_join(teams_colors_logos, by = c("possessionTeam" = "team_abbr"))

bc_performance_filt %>%
  ggplot(aes(x = yac_oe_success_rate, y = avg_yac_oe, size = plays)) +
  geom_point(aes(fill = team_color, color = team_color2), stat = "identity") +
  scale_color_identity(aesthetics = c("fill","color")) +
  ggrepel::geom_text_repel(aes(label = ballCarrierDisplayName), size = 15, max.overlaps = 4) +
  geom_hline(yintercept = mean(bc_performance_filt$avg_yac_oe), linetype = "dashed") +
  geom_vline(xintercept = mean(bc_performance_filt$yac_oe_success_rate), linetype = "dashed") +
  theme_bw() +
  labs(x = "YAC Over Expected Success Rate",
       y = "YAC Over Expected/Play",
       title = "The Best and Worst Ball Carriers at Generating YAC Over Expected",
       subtitle = "Weeks 1-9 of 2022 NFL season, dot size based on number of touches",
       caption = "Daniel Galper | @Daniel_Galper | Data: Big Data Bowl 24") +
  theme(plot.title = element_text(size = 80, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 50),
        plot.caption = element_text(size = 30),
        axis.text = element_text(size = 75),
        axis.title = element_text(size = 65),
        legend.position = "none")

ggsave("bc-performance-scat.png", width = 14, height = 10, dpi = "retina")
