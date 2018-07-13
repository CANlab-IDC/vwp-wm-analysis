excluded_subjects_ <- c("102","108","112","116","125","127")
# excluded_select_incorrect <- paste(crit_incorrect_msgs$Subject, crit_incorrect_msgs$TRIAL_INDEX)
# excluded_digit_span_incorrect <- paste(digit_span$Subject, digit_span$TRIAL_INDEX)
digit_span_ <- read.csv("rawdata/Digit incorrect_Exp1.csv", header=T, col.names=c("TRIAL_ID"))
practice_trials <- factor(c(1, 2, 3, 4, 37, 38, 39, 40))
# keepcritcorrect <- function(.data, IncludeFiller) {
#   incorrect_msgs <- .data[grepl("SELECTION_INCORRECT", .data$SAMPLE_MESSAGE), c("Subject","TRIAL_INDEX")];
#   excluded_select_incorrect <- paste(incorrect_msgs$Subject, incorrect_msgs$TRIAL_INDEX)
#   return (.data[
#     (as.logical(IncludeFiller) | .data$critical == "y")
#     & !(.data$Subject %in% excluded_subjects)
#     & !(paste(.data$Subject, .data$TRIAL_INDEX) %in% excluded_select_incorrect)
#     & !(paste(.data$Subject, .data$TRIAL_INDEX) %in% excluded_digit_span_incorrect)
#   ,])
#          
# }
addTrialId <- function(.data) {
  .data %>%
      mutate(TRIAL_ID = paste(Subject, TRIAL_INDEX, sep = ":"))
}
addTimeRanks <- function(.data, time_col = "Time") {
  .data %>%
      mutate(Time.pct_rk = percent_rank(Time), Time.min_rk = min_rank(Time)) %>%
      arrange(Time)
}
findKeybdSelections <- function(.data) {
  .data %>%
      filter(grepl("KEYBD_SELECT", SAMPLE_MESSAGE)) %>%
      select(TRIAL_ID)
}
findTouches <- function(.data) {
  .data %>%
    select(TRIAL_ID,Subject,TRIAL_INDEX,Time,SAMPLE_MESSAGE,condition,load,wmcap,LEFT_INTEREST_AREA_LABEL,RIGHT_INTEREST_AREA_LABEL,IA_LABEL) %>%
    filter(grepl("TOUCH_", SAMPLE_MESSAGE)) %>%
    mutate(Touched = sub(".*TOUCH_(\\w*);?.*", "\\1", SAMPLE_MESSAGE)) %>%
    addTimeRanks
}
findSelections <- function(.data) {
  .data %>%
      select(TRIAL_ID,Subject,TRIAL_INDEX,Time,SAMPLE_MESSAGE,condition,load,wmcap,LEFT_INTEREST_AREA_LABEL,RIGHT_INTEREST_AREA_LABEL,IA_LABEL) %>%
      filter(grepl("SELECTION", SAMPLE_MESSAGE)) %>%
      mutate(Correct = grepl("SELECTION_CORRECT", SAMPLE_MESSAGE)) %>%
      addTimeRanks
}
findIncorrectSelections <- function(.data) {
  .data %>%
      findSelections %>%
      filter(!Correct) %>%
      addTimeRanks
}
findCorrectSelections <- function(.data) {
  .data %>%
      findSelections %>%
      filter(Correct) %>%
      addTimeRanks
}
findCorrectCritSelections <- function(.data) {
  .data %>%
      findCorrectSelections %>%
      filter(condition != 0) %>%
      addTimeRanks
}
addExcludeColumns <- function(.data, digit_span = digit_span_, excluded_subjects = excluded_subjects_) {
  inc_dig_ids <- digit_span                        %>% pull(TRIAL_ID);
  inc_sel_ids <- .data %>% findIncorrectSelections %>% pull(TRIAL_ID);
  kbd_sel_ids <- .data %>% findKeybdSelections     %>% pull(TRIAL_ID);
  return (
    .data %>%
      addTrialId %>%
      mutate(
        isPracticeTrial  = factor(TRIAL_INDEX) %in% practice_trials,
        isIncorrectTrial = paste(Subject, TRIAL_INDEX, sep = ":") %in% inc_sel_ids,
        isIncorrectDigit = paste(Subject, TRIAL_INDEX, sep = ":") %in% inc_dig_ids,
        isKeyboardSelect = paste(Subject, TRIAL_INDEX, sep = ":") %in% kbd_sel_ids,
        isExcludeSubject = Subject  %in% excluded_subjects
      )
  )
}
exclude_subjects = function(.data) {
  .data %>% filter(critical == "y")
}
exclude_filler = function(.data) {
  .data %>% filter(critical == "y")
}
exclude_practice = function(.data) {
  .data %>% filter(!isPracticeTrial)
}
exclude_inc_sel = function(.data) {
  .data %>% filter(!isIncorrectTrial)
}
exclude_inc_dig = function(.data) {
  .data %>% filter(!isIncorrectDigit)
}
exclude_kbd_sel = function(.data) {
  .data %>% filter(!isKeyboardSelect)
}

exclude_bad_sub = function(.data) {
  .data %>% filter(!isExcludeSubject)
}
exclude_all <- function(.data) {
  .data %>%
    addExcludeColumns %>%
    exclude_filler %>%
    exclude_practice %>%
    exclude_inc_sel %>%
    exclude_inc_dig %>%
    exclude_kbd_sel %>%
    exclude_bad_sub
}
check_model_grad <- function(mod) {
  relgrad <- with(mod@optinfo$derivs,solve(Hessian,gradient));
  return (max(pmin(abs(relgrad),abs(mod@optinfo$derivs$gradient))));
}
keepUsefulCols <- function(.data) {
  .data %>%
    select(
            Subject,
            TRIAL_INDEX,
            SAMPLE_INDEX,
            SAMPLE_MESSAGE,           
            TIMESTAMP,
            LAST_Position,
            condition,                
            Item,
            critical,
            distractor,
            filler_1,      
            filler_2,
            load,
            version,
            withha_onset,             
            withoutha_onset,
            Event,
            Time,                     
            EyeRecorded,
            EyeSelected,
            IA_ID,                    
            IA_LABEL,
            IA_Data
    )
}
collect_trial_stats <- function(.data) {
  .data %>%
    group_by(Subject, TRIAL_INDEX) %>%
    summarize(
      TrialSampleLength = n(),
      TrialLength = max(Time)
    ) %>%
    ungroup %>%
    merge(y = .data %>% findSelections %>% addExcludeColumns %>% rename_at(vars(contains("Time")), function(t) paste("Selection.", t, sep="")), by = c("Subject", "TRIAL_INDEX")) %>% 
    merge(y = .data %>% findTouches %>% select(Subject, TRIAL_INDEX, contains("Time")) %>% rename_at(vars(contains("Time")), function(t) paste("Touch.", t, sep="")), by = c("Subject", "TRIAL_INDEX"));
}
trim_to_selection <- function(.data) {
  .data %>%
    filter(Time <= Selection.Time)
}

mutate_rows <- function(.data, .p, ...) {
  .p <- rlang::enquo(.p)
  .p_lgl <- rlang::eval_tidy(.p, .data)
  .data[.p_lgl, ] <- .data[.p_lgl, ] %>% mutate(...)
  .data
}

pad_trials <- function(.data, start_val=-200, end_val = 3500, step = 2, .fill_type = "Target") {
  .data %>%
    group_by(Subject, TRIAL_INDEX) %>%
    arrange(Time) %>%
    pad_int(
      by = "Time",
      start_val = start_val,
      end_val = end_val,
      step = step
    ) %>%
    filter(Time < end_val) %>%
    (switch(
      .fill_type,
      Target = (
        . %>%
          replace_na(IA_ID = 1, IA_LABEL = factor("Target"), IA_Data = factor("Contains_IA_Looks")) %>%
          fill(everything())
      ),
      LOCF = (. %>% fill(everything()))
    )) %>%
    ungroup
}
scale_dat <- function(.data) {
  .data %>%
    mutate_if(is.numeric, funs(scale(., center = TRUE, scale = max(., rm.na=TRUE)/100)))
}
# dat3_pad <- dat3 %>%
#   filter(Time<=5500) %>%
#   pad_int("Time", start_val=-200, end_val = 5500, step=2, group=c("Subject", "TRIAL_INDEX")) %>%
#   fill_by_function(last)

#  select(condition, wmcap, load, Subject, Time, starts_with("IA")) %>%
#  pad(by="TimeDt",interval="milliseconds",start_val=origin+days(1)-milliseconds(200),end_val = origin+days(1)+milliseconds(6500)) %>%
#  fill_with_function(last)

# c("110 9",   "101 16",  "136 16" , "101 21" , "108 21" , "112 21" , "101 22" , "108 35",  "114 45"  ,"110 52" , "102 53","114 58" , "128 69",  "106 71",  "111t 71", "114 71",  "127 71",  "110 72" , "112 72")
#(function(.data) {
#  out <- select(.data, )
#  for (i in .data) {
#    
#  }
#})(dat5)