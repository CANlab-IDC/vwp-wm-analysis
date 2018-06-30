# excluded_subjects <- c("102","108","112","116","125","127")
# excluded_select_incorrect <- paste(crit_incorrect_msgs$Subject, crit_incorrect_msgs$TRIAL_INDEX)
digit_span2 <- read.csv("rawdata/Digit incorrect_Exp1.csv", header=T, col.names=c("TRIAL_ID"))
# excluded_digit_span_incorrect <- paste(digit_span$Subject, digit_span$TRIAL_INDEX)


#wmcaps <- 

# keepcritcorrect <- function(x, IncludeFiller) {
#   incorrect_msgs <- x[grepl("SELECTION_INCORRECT", x$SAMPLE_MESSAGE), c("Subject","TRIAL_INDEX")];
#   excluded_select_incorrect <- paste(incorrect_msgs$Subject, incorrect_msgs$TRIAL_INDEX)
#   return (x[
#     (as.logical(IncludeFiller) | x$critical == "y")
#     & !(x$Subject %in% excluded_subjects)
#     & !(paste(x$Subject, x$TRIAL_INDEX) %in% excluded_select_incorrect)
#     & !(paste(x$Subject, x$TRIAL_INDEX) %in% excluded_digit_span_incorrect)
#   ,])
#          
# }
addTrialId <- function(x) {
  return (
    x %>%
      mutate(., TRIAL_ID = paste(Subject, TRIAL_INDEX, sep = ":"))
  )
}
addExcludeColumns <- function(x) {
  inc_sel_ids <- findIncorrectSelections(x) %>% pull(., TRIAL_ID);
  inc_dig_ids <- digit_span2                %>% pull(., TRIAL_ID);
  kbd_sel_ids <- findKeybdSelections(x)     %>% pull(., TRIAL_ID);
  return (
    x %>%
      mutate(.,
             isIncorrectTrial = TRIAL_ID %in% inc_sel_ids,
             isIncorrectDigit = TRIAL_ID %in% inc_dig_ids,
             isKeyboardSelect = TRIAL_ID %in% kbd_sel_ids
      )
  )
}
exclude_filler = function(data) {
  data %>% filter(critical == "y")
}
exclude_inc_sel = function(data) {
  data %>% filter(!isIncorrectTrial)
}
exclude_inc_dig = function(data) {
  data %>% filter(!isIncorrectDigit)
}
exclude_kbd_sel = function(data) {
  data %>% filter(!isKeyboardSelect)
}
.exclude <- function(data) {
  data %>%
    addExcludeColumns(.) %>%
    filter_(.dots = lazyeval::lazy_dots(...))
}
exclude <- function(x, ...=c(exclude_filler, exclude_inc_sel, exclude_inc_dig, exclude_kbd_sel)) {
  return (
    x %>%
      addExcludeColumns(.) %>%
      filter_(.dots = lazyeval::lazy_dots(...))
  )
}
addTimeRanks <- function(x) {
  return (
    x %>%
      mutate(., Time.pct_rk = percent_rank(Time), Time.min_rk = min_rank(Time)) %>%
      arrange(., Time)
  );
}
findKeybdSelections <- function(x) {
  return (
    x %>%
      filter(., grepl("KEYBD_SELECT", SAMPLE_MESSAGE)) %>%
      select(., TRIAL_ID)
  )
}
findSelections <- function(x) {
  return (
    x %>%
      select(.,TRIAL_ID,Subject,TRIAL_INDEX,Time,SAMPLE_MESSAGE,condition,load,wmcap,LEFT_INTEREST_AREA_LABEL,RIGHT_INTEREST_AREA_LABEL) %>%
      filter(., grepl("SELECTION", SAMPLE_MESSAGE)) %>%
      mutate(., Correct = grepl("SELECTION_CORRECT", SAMPLE_MESSAGE)) %>%
      addTimeRanks(.)
  )
}
findIncorrectSelections <- function(x) {
  return (
    findSelections(x) %>%
      filter(., !Correct) %>%
      addTimeRanks(.)
  )
}
findCorrectSelections <- function(x) {
  return (
    findSelections(x) %>%
      filter(., Correct) %>%
      addTimeRanks(.)
  )
}
findCorrectCritSelections <- function(x) {
  return (
    findCorrectSelections(x) %>%
      filter(., condition != 0) %>%
      addTimeRanks(.)
  )
}
check_model_grad <- function(mod) {
  relgrad <- with(mod@optinfo$derivs,solve(Hessian,gradient));
  return (max(pmin(abs(relgrad),abs(mod@optinfo$derivs$gradient))));
}
keepUsefulCols <- function(x) {
 return (x %>% select(., Subject,
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
 )) 
}
pad_trials <- function(x) {
  x_pad <- x %>%
    #filter(., Time<=5500) %>%
    pad_int(.,
            "Time",
            start_val=-200,
            #end_val = 5500,
            step=2,
            group="TRIAL_ID"
    ) %>%
   #fill_by_value(., LEFT_INTEREST_AREA_ID, RIGHT_INTEREST_AREA_ID, value=1 ) %>%
   #fill_by_value(., LEFT_INTEREST_AREA_LABEL, RIGHT_INTEREST_AREA_LABEL, value="Target" ) %>%
    fill_by_function(last);
  return (x_pad);
}
# dat3_pad <- dat3 %>%
#   filter(., Time<=5500) %>%
#   pad_int(., "Time", start_val=-200, end_val = 5500, step=2, group=c("Subject", "TRIAL_INDEX")) %>%
#   fill_by_function(last)

#  select(., condition, wmcap, load, Subject, Time, starts_with("IA")) %>%
#  pad(by="TimeDt",interval="milliseconds",start_val=origin+days(1)-milliseconds(200),end_val = origin+days(1)+milliseconds(6500)) %>%
#  fill_with_function(last)

# c("110 9",   "101 16",  "136 16" , "101 21" , "108 21" , "112 21" , "101 22" , "108 35",  "114 45"  ,"110 52" , "102 53","114 58" , "128 69",  "106 71",  "111t 71", "114 71",  "127 71",  "110 72" , "112 72")
#(function(x) {
#  out <- select(x, )
#  for (i in x) {
#    
#  }
#})(dat5)