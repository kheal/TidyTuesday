#Practice with gt with TidyTuesday dataset
#from https://github.com/jakepscott/TidyTuesday2020-9-08/blob/master/Table_Creation.R

# Loading Libraries -------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(tidytext)
library(gt)
library(paletteer)
library(finalfit)

# Loading Data ------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-08')

friends <- tuesdata$friends
friends_info <- tuesdata$friends_info
friends_emotions <- tuesdata$friends_emotions

# Prepping Data -----------------------------------------------------------
full_data <- friends_emotions %>% left_join(friends)

character_emotions <- full_data %>% filter(speaker %in% c("Ross Geller","Joey Tribbiani","Chandler Bing",
                                                          "Rachel Green","Phoebe Buffay", "Monica Geller")) %>% 
   count(speaker,emotion) %>% group_by(speaker) %>% mutate(total=sum(n)) %>% ungroup() %>% 
  mutate(percent=n/total*100) %>% 
  separate(col = "speaker", sep = " ", into=c("speaker","lastname")) %>% select(-lastname)

chars <- character_emotions %>% select(-n) %>% 
    pivot_wider(names_from = "emotion", values_from=c("percent"))

(show <- character_emotions %>% mutate(show_words=sum(n)) %>% group_by(emotion) %>% 
    mutate(show_emotion_n=sum(n)) %>% ungroup() %>% mutate(show_percent=show_emotion_n/show_words*100) %>% 
    select(emotion,show_words,show_percent) %>% distinct() %>% 
    pivot_wider(names_from="emotion",values_from="show_percent") %>% 
    mutate(speaker="Overall") %>% relocate(speaker,.before="show_words") %>% 
    rename("total"=show_words))

(table_data <- rbind(show,chars))



diff_func <- function(emotion="Joyful") {
  vector <- vector()
  for (i in 1:nrow(table_data)) {
    vector[i] <- (table_data %>% pull(emotion))[i]-(table_data %>% pull(emotion))[1]
  }
  return(vector)
}

joy_diff <- diff_func("Joyful")
Mad_diff <- diff_func("Mad")
Neutral_diff <- diff_func("Neutral")
Peaceful_diff <- diff_func("Peaceful")
Power_diff <- diff_func("Powerful")
Sad_diff <- diff_func("Sad")
Scared_diff <- diff_func("Scared")

new_table <- tibble("speaker"=table_data$speaker, "Joyful"=diff_func("Joyful"),
                    "Mad"=diff_func("Mad"), "Neutral"=diff_func("Neutral"),
                    "Peaceful"=diff_func("Peaceful"),"Powerful"=diff_func("Powerful"),
                    "Sad"=diff_func("Sad"), "Scared"=diff_func("Neutral"))

new_table <- new_table %>% tail(6) %>% mutate(across(Joyful:Scared,round_tidy,2))%>% 
    mutate(Joyful=case_when(Joyful>=0~paste0("+",Joyful),
                            TRUE~Joyful), #sprintf keeps the trailing zero, so we get 1.50 rather than 1.5
           Mad=case_when(Mad>=0~paste0("+",Mad),
                         TRUE~Mad),
           Neutral=case_when(Neutral>=0~paste0("+",Neutral),
                             TRUE~Neutral),
           Peaceful=case_when(Peaceful>=0~paste0("+",Peaceful),
                              TRUE~Peaceful),
           Powerful=case_when(Powerful>=0~paste0("+",Powerful),
                              TRUE~Powerful),
           Sad=case_when(Sad>=0~paste0("+",Sad),
                         TRUE~Sad),
           Scared=case_when(Scared>=0~paste0("+",Scared),
                            TRUE~Scared))

new_table<- rbind(table_data[1,] %>% select(-total) %>% mutate(across(where(is.numeric),round,2)) %>% 
                     mutate(across(where(is.numeric),paste0,"%")),new_table)

(New_table <- cbind(new_table,table_data$total) %>% rename("Lines"=`table_data$total`) %>% 
    relocate(`Lines`,.before="Joyful"))


#Parameters for table
#outlier is 1 sd, seems to exclusive
outlier <- c(joy_diff,Mad_diff,Neutral_diff,Peaceful_diff,Power_diff,Sad_diff,Scared_diff) %>% 
  as_tibble() %>% filter(value!=0) %>% pull(value) %>% sd()

outlier <- 1

gt2 <- New_table %>% arrange(desc(`Lines`)) %>% 
  gt() %>%
  tab_header(title = md("**The One Where Rachel is Mad and Phoebe is Sad**"),
             subtitle = md("*Difference between the share of lines spoken by a given character in a given category and the share of overall lines in that category*")) %>%
  cols_label(speaker="") %>%
  cols_align(align = "right",
             columns = 3:9) %>% 
  cols_align(align = "center",
             columns = 2) %>%
  tab_style(style = list(cell_text(weight = "bold", align = "right")),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(style = list(cell_borders(sides = "bottom",
                                      color = "black",
                                      weight = px(2),
                                      style = "dashed")),
            locations = list(cells_body(rows = 1))) %>%
  tab_options(column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    heading.align = "left")  %>%
  tab_footnote(footnote = "Grey highlight indicates a difference greater than 1 percentage point",
    locations = cells_body(columns = 4,
      rows = 2)) %>%
  tab_style(style = list(cell_text(color = "green")),
    locations = cells_body(columns = 3,
                             rows = as.numeric(Joyful) > 0 & as.numeric(Joyful) < 6)) %>% 
  tab_source_note(md("**Table**: @jakepscott2020 | **Data**: Friends Package")) %>%
  tab_style(style = list(cell_fill(color = scales::alpha("grey", 0.7))),
            locations = list(cells_body(columns = 3,
                                        rows = abs(as.numeric(Joyful)) > outlier & 
gt2
