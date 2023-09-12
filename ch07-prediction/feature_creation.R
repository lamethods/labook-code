################################################ 
# Features based on counts of learning actions
################################################ 
#
# - Total number of each type of learning actions 
# - Average number of (all types of) actions per day
# - Entropy of action counts per day
#

# Function for computing the total number of each type of learning action, up to the current week
# 
# Note: Learning action types include the following seven: "Assignment", "Practicals", "Group_work", 
# "Course_view", "Instructions", "Social” and “Course_materials”; the latter gathers actions related 
# to all topic-specific course materials ("General", "Applications", "Theory",  "Ethics", "Feedback", "La_types")
total_counts_per_action_type <- function(events_data, current_week){
  events_data |>
    filter(week <= current_week) |>
    mutate(action = ifelse(startsWith(action, "Materials"), "Course_materials", action)) |>
    count(user, action) |>
    pivot_wider(id_cols = user, 
                names_from = action, 
                names_glue = "{action}_cnt",
                values_from = n,
                values_fill = 0)
}


# Function for computing average (median) number of learning actions per day, up to the current week
avg_action_cnt_per_day <- function(events_data, current_week) {
  events_data |>
    filter(week <= current_week) |>
    group_by(user, date) |>
    summarise(daily_action_cnt = n()) |>
    ungroup() |>
    group_by(user) |>
    summarise(avg_daily_cnt = median(daily_action_cnt)) |>
    ungroup()
}


# Function for computing entropy of action counts per day, up to the current week
daily_cnt_entropy <- function(events_data, current_week){
  events_data |>
    filter(week <= current_week) |>
    group_by(user) |>
    mutate(tot_action_cnt = n()) |>
    ungroup() |>
    group_by(user, date) |>
    reframe(daily_action_cnt = n(),
              daily_action_prop = daily_action_cnt/tot_action_cnt) |>
    ungroup() |>
    group_by(user) |>
    summarise(entropy_daily_cnts = -1*sum(daily_action_prop * log2(daily_action_prop))) |>
    select(user, entropy_daily_cnts)
}


###################################
# Features based on session counts
###################################
#
# - Total number of learning sessions
# - Average (median) session length (in seconds)
# - Entropy of session length
#

# Function for computing total number of sessions, average (median) session length (time), 
# and entropy of session length, up to the current week
session_based_features <- function(events_data, current_week) {
  events_data |>
    filter(week <= current_week) |>
    distinct(user, session_id, session_len) |>
    group_by(user) |>
    mutate(session_cnt = n(),
           avg_session_len = median(session_len),
           tot_session_len = sum(session_len)) |>
    ungroup() |>
    group_by(user, session_id) |>
    mutate(session_len_prop = session_len/tot_session_len) |>
    ungroup() -> session_stats
  
  session_stats |>
    group_by(user) |>
    mutate(session_len_entropy = -1*sum(session_len_prop * log2(session_len_prop))) |>
    ungroup() |>
    distinct(user, session_cnt, avg_session_len, session_len_entropy)
}

############################################################################
# Features based on active days (= days with at least one learning session)
############################################################################
#
# - Number of active days
# - Average time distance between two consecutive active days
#

# Function for computing the number of active days, up to the current week
active_days_count <- function(events_data, current_week) {
  events_data |>
    filter(week <= current_week) |>
    group_by(user) |>
    summarise(active_days_cnt = n_distinct(date)) |>
    ungroup()
}


# Function for computing avg. (median) time distance between two consecutive active days, up to the current week
active_days_avg_time_dist <- function(events_data, current_week) {
  events_data |>
    filter(week <= current_week) |>
    distinct(user, date) |>
    group_by(user) |>
    arrange(date) |>
    mutate(day_dist = as.numeric(date - lag(date), units="days")) |>
    summarise(avg_aday_dist = median(day_dist, na.rm = TRUE)) |>
    ungroup() -> avg_aday_dist_df
  
  # for student with only 1 active day, avg_aday_dist will be NA, which cases problems when building a model
  # replace NAs with a large number (e.g., 2 x max distance), thus indicating that a student rarely (if ever) 
  # got back to the course activities
  max_avg_aday_dist <- max(avg_aday_dist_df$avg_aday_dist, na.rm = TRUE)
  avg_aday_dist_df |>
    mutate(avg_aday_dist = ifelse(test = is.na(avg_aday_dist), yes = max_avg_aday_dist * 2, no = avg_aday_dist))
  
}

###############################################################
# Putting it all together: create data sets for model building  
###############################################################

# Function that makes use of the above functions to compute all event-based features, 
# for the given week (as current_week) 
create_event_based_features <- function(events_data, current_week) {
  
  f1 <- total_counts_per_action_type(events_data, current_week)
  f2 <- avg_action_cnt_per_day(events_data, current_week)
  f3 <- daily_cnt_entropy(events_data, current_week)
  f4 <- session_based_features(events_data, current_week)
  f5 <- active_days_count(events_data, current_week)
  f6 <- active_days_avg_time_dist(events_data, current_week)
  
  f1 |> 
    inner_join(f2, by='user') |>
    inner_join(f3, by='user') |>
    inner_join(f4, by='user') |>
    inner_join(f5, by='user') |>
    inner_join(f6, by='user') -> features
  
  features
}


# Function that adds the outcome variable to the feature set and creates a data set to be used for 
# building a regression model that predict the final grade
create_dataset_for_grade_prediction <- function(events_data, current_week, grades) {
  features <- create_event_based_features(events_data, current_week)
  grades |> 
    select(user, Final_grade) |>
    inner_join(features, by="user")
} 


# Function that adds the outcome variable to the feature set and 
# creates a data set to be used for building a classification model 
# that predicts the course success category (high / low) for each student
create_dataset_for_course_success_prediction <- function(events_data, 
                                                      current_week, 
                                                      grades) {
  features <- create_event_based_features(events_data, current_week)
  grades |>
    select(user, Course_outcome) |>
    inner_join(features, by="user")
} 


