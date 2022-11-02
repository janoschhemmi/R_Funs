## get simple c, from model 3x3 
get_cm <- function(model, name){

  model$err.rate
  require(caret)
  ref <- model$y
  pre <- model$predicted
  
  AA <- caret::confusionMatrix(as.factor(pre), as.factor(ref))
  table <- as.data.frame.matrix(AA$table)
  AA$byClass
  ## get count ref 
  count_ref <- as.data.frame(ref) %>% group_by(ref) %>% tally() %>% as.data.frame()
  omi  <- c( (sum(table[2:4,1])* 100)/ sum(table[1:4,1]),(sum(table[c(1,3,4),2])* 100)/ sum(table[1:4,2]), (sum(table[c(1:2,4),3])* 100)/ sum(table[1:4,3]), (sum(table[c(1:3),4])* 100)/ sum(table[1:4,4])  ) 
  comi <- c( (sum(table[1,2:4])* 100)/ sum(table[1,1:4]),(sum(table[2,c(1,3,4)])* 100)/ sum(table[2,1:4]), (sum(table[3,c(1:2,4)])* 100)/ sum(table[3,1:4]), (sum(table[4,1:3])* 100)/ sum(table[4,1:4])  ) 

  cm <- cbind(rep(name,4),c("Fire","Harvest","No_Dis","Insect"),table, count_ref$n, rowSums(table),omi,comi, as.data.frame.matrix(AA$byClass)[,1:2], c(NA,NA,NA,AA$overall[1]))
  colnames(cm) <- c("model","Disturbance","Fire","Harvest","No_Dis","Insect","Reference","Predicted","Omission", "Commission","Sensitivity","Specificity","OA")
  return(cm)
}

get_cm_2cl <- function(model, name){

  require(caret)
  ref <- model$y
  pre <- model$predicted
  
  AA <- caret::confusionMatrix(as.factor(pre), as.factor(ref))
  table <- as.data.frame.matrix(AA$table)
  AA$byClass
  ## get count ref 
  count_ref <- as.data.frame(ref) %>% group_by(ref) %>% tally() %>% as.data.frame()
  omi  <- c( (sum(table[2,1])* 100)/ sum(table[1:2,1]),(sum(table[c(1),2])* 100)/ sum(table[1:2,2])  ) 
  comi <- c( (sum(table[1,2])* 100)/ sum(table[1,1:2]),(sum(table[2,c(1)])* 100)/ sum(table[2,1:2])  ) 
  
  cm <- cbind(rep(name,2),c("Dis","No_Dis"),table, count_ref$n, rowSums(table),omi,comi, c(NA,AA$overall[1]))
  colnames(cm) <- c("model","Disturbance","Dis","No_Dis","Reference","Predicted","Omission", "Commission","OA")
  return(cm)
}

## get simple c, from model 3x3 
get_cm_two_cols <- function(c1, c2, name){
  # name <- "1990 - 2020"
  # c1 <- breaks_S2_sub$brandsat_ref 
  # c2 <- breaks_S2_sub$predicted
  require(caret)
  
  ## c1::ref
  ## c1::pre
  
  ref <- c2
  pre <- c1
  
  AA <- caret::confusionMatrix(as.factor(pre), as.factor(ref))
  table <- as.data.frame.matrix(AA$table)
  
  ## get count ref 
  count_ref <- as.data.frame(ref) %>% group_by(ref) %>% tally() %>% as.data.frame()
  omi  <- c( (sum(table[2:4,1])* 100)/ sum(table[1:4,1]),(sum(table[c(1,3,4),2])* 100)/ sum(table[1:4,2]), (sum(table[c(1:2,4),3])* 100)/ sum(table[1:4,3]), (sum(table[c(1:3),4])* 100)/ sum(table[1:4,4])  ) 
  comi <- c( (sum(table[1,2:4])* 100)/ sum(table[1,1:4]),(sum(table[2,c(1,3,4)])* 100)/ sum(table[2,1:4]), (sum(table[3,c(1:2,4)])* 100)/ sum(table[3,1:4]), (sum(table[4,c(1:3)])* 100)/ sum(table[4,1:4])  ) 
  
  cm <- cbind(rep(name,4),c("Fire","Harvest","Undisturbed","Insect"),table, count_ref$n, rowSums(table),omi,comi, as.data.frame.matrix(AA$byClass)[,1:2], c(NA,NA,NA,AA$overall[1]))
  colnames(cm) <- c("Time span / model","Disturbance","Fire","Harvest","Undisturbed","Insect","Reference","Predicted","Omission", "Commission","Sensitivity","Specificity","OA")
  return(cm)
}


# read data
load_comments <- function() {
  bb_comment <- read_csv(file.path(tsync_p, "tsync_plots_bb_jan_interpretations_comment.csv"), show_col_types = FALSE)
  fires_comments <- read_csv(file.path(tsync_p, "tsync_fires_interpretations_comment.csv"), show_col_types = FALSE)
  fires_post1_comments <- read_csv(file.path(tsync_p, "tsync_fires_post_training1_interpretations_comment.csv"), show_col_types = FALSE)
  fires_post2_comments <- read_csv(file.path(tsync_p, "tsync_fires_post_training2_interpretations_comment.csv"), show_col_types = FALSE)
  df_comments <- rbind(bb_comment, fires_comments, fires_post1_comments,fires_post2_comments)
  rm(bb_comment, fires_comments,fires_post1_comments, fires_post2_comments)
  return(df_comments)
}

load_inters <- function() {
  bb_interpretations <- read_csv(file.path(tsync_p, "tsync_plots_bb_jan_interpretations.csv"), show_col_types = FALSE)
  fires_interpretations <- read_csv(file.path(tsync_p, "tsync_fires_interpretations.csv"), show_col_types = FALSE)
  fires_post1interpretations <- read_csv(file.path(tsync_p, "tsync_fires_post_training1_interpretations.csv"), show_col_types = FALSE)
  fires_post2interpretations <- read_csv(file.path(tsync_p, "tsync_fires_post_training2_interpretations.csv"), show_col_types = FALSE)
  
  df_inters <- rbind(fires_interpretations, fires_post1interpretations,fires_post2interpretations, bb_interpretations)
  rm(fires_interpretations, bb_interpretations,fires_post1interpretations,fires_post2interpretations)
  return(df_inters)
}


## color for Fire 
bg_picker_fire <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("white","lightblue"),
    domain = c(800,900) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(0,800))
  ifelse(x < 800, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

## color for Harvest 
bg_picker_harvest <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("white","lightblue"),
    domain = c(1000,1100) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(0,1000))
  ifelse(x < 1000, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

## color for no dis
bg_picker_no_dis <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("white","lightblue"),
    domain = c(4450,4550) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(0,4450))
  ifelse(x < 4450, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

## omission error 
bg_picker_omi_fire  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(15,20) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(20,100))
  ifelse(x > 20, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

bg_picker_omi_harvest  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(25,30) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(30,100))
  ifelse(x > 30, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

bg_picker_omi_no_dis  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(4.5,10) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(10,100))
  ifelse(x > 10, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

bg_picker_comi_fire  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(5,11) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(11,100))
  ifelse(x > 11, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

bg_picker_comi_harvest  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(20,25) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(25,100))
  ifelse(x > 25, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}
bg_picker_comi_no_dis  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(8,13) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(13,100))
  ifelse(x > 13, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}
################################################################################
####     funs for equal subsampling 

## color for Fire 
bg_picker2_fire <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("white","lightblue"),
    domain = c(880,960) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(0,880))
  ifelse(x < 880, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

## color for Harvest 
bg_picker2_harvest <-  function (x) {
  f_col <- scales::col_numeric(
    palette = c("white","lightblue"),
    domain = c(880,960) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(0,880))
  ifelse(x < 880, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

## color for no dis
bg_picker2_no_dis <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("white","lightblue"),
    domain = c(880,960) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(0,880))
  ifelse(x < 880, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

## omission error 
bg_picker2_omi_fire  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(10,15) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(15,100))
  ifelse(x > 15, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

bg_picker2_omi_harvest  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(11,16) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(16,100))
  ifelse(x > 16, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

bg_picker2_omi_no_dis  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(14,19) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(19,100))
  ifelse(x > 19, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

bg_picker2_comi_fire  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(5,11) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(11,100))
  ifelse(x > 11, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

bg_picker2_comi_harvest  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(17,22) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(22,100))
  ifelse(x > 22, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}
bg_picker2_comi_no_dis  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(10,17) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(17,100))
  ifelse(x > 17, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

## 3 two combined

## color for Fire 
bg_picker3_fire <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("white","lightblue"),
    domain = c(880,960) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(0,880))
  ifelse(x < 880, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

## color for Harvest 
bg_picker3_harvest <-  function (x) {
  f_col <- scales::col_numeric(
    palette = c("white","lightblue"),
    domain = c(880,960) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(0,880))
  ifelse(x < 880, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

## color for no dis
bg_picker3_no_dis <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("white","lightblue"),
    domain = c(880,960) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(0,880))
  ifelse(x < 880, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

## omission error 
bg_picker3_omi_fire  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(9,15) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(15,100))
  ifelse(x > 15, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

bg_picker3_omi_harvest  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(11,16) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(16,100))
  ifelse(x > 16, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

bg_picker3_omi_no_dis  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(13,19) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(19,100))
  ifelse(x > 19, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

bg_picker3_comi_fire  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(5,11) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(11,100))
  ifelse(x > 11, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}

bg_picker3_comi_harvest  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(17,22) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(22,100))
  ifelse(x > 22, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}
bg_picker3_comi_no_dis  <- function (x) {
  f_col <- scales::col_numeric(
    palette = c("lightblue","white"),
    domain = c(10,17) )
  f_no_col <- scales::col_numeric(
    palette = c("white"),
    domain = c(17,100))
  ifelse(x > 17, f_no_col(x), f_col(x))
  # min and max range can be also set dynamically 
}


print_grouped_as_flex <- function(grouped, outfile){
  #grouped <- cm.group
  
  as_flextable(grouped) %>%
  theme_booktabs() %>%
  hline(part = 'header', border = officer::fp_border(color = "black", width = 3)) %>%
  vline(j=c("Disturbance","Insect","Predicted"),   border = officer::fp_border(color= "black", style = "solid", width = 1.45)) %>%
  vline(j=c("Fire","Harvest","Reference","Commission"),   border = officer::fp_border(color= "gray", style = "dashed", width = 1.45)) %>%
  colformat_double(j = c("Predicted"), digits = 0) %>%
  colformat_double(j = c("Omission","Commission","Sensitivity","Specificity","OA"), digits = 2) %>%
  
  hline(i = ~Disturbance %in% c("Fire","Harvest"), border = officer::fp_border(color= "gray", style = "dashed")) %>%
  #
  # bg(i = ~ Disturbance == "Fire" ,j = ~ Fire, bg=bg_picker_fire) %>%
  # bg(i = ~ Disturbance == "Harvest" ,j = ~ Harvest, bg=bg_picker_harvest) %>%
  # bg(i = ~ Disturbance == "No_Dis" , j = ~ No_Dis, bg=bg_picker_no_dis) %>%
  # ## Omission
  # bg(i = ~ Disturbance == "Fire" , j = ~ Omission, bg=bg_picker_omi_fire) %>%
  # bg(i = ~ Disturbance == "Harvest" , j = ~ Omission, bg=bg_picker_omi_harvest) %>%
  # bg(i = ~ Disturbance == "No_Dis" , j = ~ Omission,  bg=bg_picker_omi_no_dis) %>%
  # ## commission
  # bg(i = ~ Disturbance == "Fire" ,  j = ~ Commission, bg=bg_picker_comi_fire) %>%
  # bg(i = ~ Disturbance == "Harvest" , j = ~ Commission,  bg=bg_picker_comi_harvest) %>%
# bg(i = ~ Disturbance == "No_Dis" , j = ~ Commission, bg=bg_picker_comi_no_dis) %>%
save_as_image(outfile,
              zoom = 3, webshot = "webshot")}

print_grouped_as_flex_2cl <- function(grouped, outfile){
  #grouped <- cm.group
  
  as_flextable(grouped) %>%
    theme_booktabs() %>%
    hline(part = 'header', border = officer::fp_border(color = "black", width = 3)) %>%
    vline(j=c("Disturbance","Predicted","No_Dis"),   border = officer::fp_border(color= "black", style = "solid", width = 1.45)) %>%
    vline(j=c("Reference","Commission"),   border = officer::fp_border(color= "gray", style = "dashed", width = 1.45)) %>%
    colformat_double(j = c("Predicted"), digits = 0) %>%
    colformat_double(j = c("Omission","Commission","OA"), digits = 2) %>%
    
    hline(i = ~Disturbance %in% c("Dis"), border = officer::fp_border(color= "gray", style = "dashed")) %>%
    #
    # bg(i = ~ Disturbance == "Fire" ,j = ~ Fire, bg=bg_picker_fire) %>%
    # bg(i = ~ Disturbance == "Harvest" ,j = ~ Harvest, bg=bg_picker_harvest) %>%
    # bg(i = ~ Disturbance == "No_Dis" , j = ~ No_Dis, bg=bg_picker_no_dis) %>%
    # ## Omission
    # bg(i = ~ Disturbance == "Fire" , j = ~ Omission, bg=bg_picker_omi_fire) %>%
    # bg(i = ~ Disturbance == "Harvest" , j = ~ Omission, bg=bg_picker_omi_harvest) %>%
    # bg(i = ~ Disturbance == "No_Dis" , j = ~ Omission,  bg=bg_picker_omi_no_dis) %>%
    # ## commission
    # bg(i = ~ Disturbance == "Fire" ,  j = ~ Commission, bg=bg_picker_comi_fire) %>%
    # bg(i = ~ Disturbance == "Harvest" , j = ~ Commission,  bg=bg_picker_comi_harvest) %>%
  # bg(i = ~ Disturbance == "No_Dis" , j = ~ Commission, bg=bg_picker_comi_no_dis) %>%
  save_as_image(outfile,
                zoom = 3, webshot = "webshot")}
