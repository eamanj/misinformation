library(readxl)

add_treatment_variables <- function(data) {
  # subject treatment
  data$polarizing_subject <- (data$MRK_SUBJECT_1 == "Yes")
  data$nonpolarizing_subject <- (data$MRK_SUBJECT_2 == "Yes")
  stopifnot(all(rowSums(data[,c("polarizing_subject", "nonpolarizing_subject")]) == 1))
  data$subject_treatment <- ifelse(data$polarizing_subject, "political", "nonpolitical")
  # utility treatment
  data$high_utility <- (data$MRK_UTIL_1 == "Yes")
  data$low_utility <- (data$MRK_UTIL_2 == "Yes")
  stopifnot(all(rowSums(data[,c("high_utility", "low_utility")]) == 1))
  data$utility_treatment <- ifelse(data$high_utility, "high", "low")
  # plausibility treatment
  data$high_plausibility <- (data$MRK_PLAUS_1 == "Yes")
  data$low_plausibility <- (data$MRK_PLAUS_2 == "Yes")
  stopifnot(all(rowSums(data[,c("high_plausibility", "low_plausibility")]) == 1))
  data$plausibility_treatment <- ifelse(data$high_plausibility, "high", "low")

  # figure out which message1 political/utility was shown
  data$health_useful <- data$MESSAGE1_IMG_1 == "Yes"
  data$health_notuseful <- data$MESSAGE1_IMG_2 == "Yes"
  data$bjp_useful <- data$MESSAGE1_IMG_3 == "Yes"
  data$bjp_notuseful <- data$MESSAGE1_IMG_4 == "Yes"
  data$inc_useful <- data$MESSAGE1_IMG_5 == "Yes"
  data$inc_notuseful <- data$MESSAGE1_IMG_6 == "Yes"
  # ensure only one image1 is set to true
  stopifnot(all(rowSums(data[,c("health_useful", "health_notuseful",
                                "bjp_useful", "bjp_notuseful",
                                "inc_useful", "inc_notuseful")]) == 1))
  # assign a string treatment of the first image shown
  data$image1_treatment <-                           "health_useful"
  data[data$health_notuseful, "image1_treatment"] <- "health_notuseful"
  data[data$bjp_useful, "image1_treatment"] <-       "bjp_useful"
  data[data$bjp_notuseful, "image1_treatment"] <-    "bjp_notuseful"
  data[data$inc_useful, "image1_treatment"] <-       "inc_useful"
  data[data$inc_notuseful, "image1_treatment"] <-    "inc_notuseful"
  
  # figure out which message2 political/utility/plausibility was shown
  data$health_useful_real <- data$MESSAGE2_IMG_1 == "Yes"
  data$health_useful_fake <- data$MESSAGE2_IMG_2 == "Yes"
  data$health_notuseful_real <- data$MESSAGE2_IMG_3 == "Yes"
  data$health_notuseful_fake <- data$MESSAGE2_IMG_4 == "Yes"
  data$bjp_useful_real <- data$MESSAGE2_IMG_5 == "Yes"
  data$bjp_useful_fake <- data$MESSAGE2_IMG_6 == "Yes"
  data$bjp_notuseful_real <- data$MESSAGE2_IMG_7 == "Yes"
  data$bjp_notuseful_fake <- data$MESSAGE2_IMG_8 == "Yes"
  data$inc_useful_real <- data$MESSAGE2_IMG_9 == "Yes"
  data$inc_useful_fake <- data$MESSAGE2_IMG_10 == "Yes"
  data$inc_notuseful_real <- data$MESSAGE2_IMG_11 == "Yes"
  data$inc_notuseful_fake <- data$MESSAGE2_IMG_12 == "Yes"
  # ensure only one image2 is set to true
  stopifnot(all(rowSums(data[,c("health_useful_real", "health_useful_fake", "health_notuseful_real", "health_notuseful_fake",
                                "bjp_useful_real", "bjp_useful_fake", "bjp_notuseful_real", "bjp_notuseful_fake",
                                "inc_useful_real", "inc_useful_fake", "inc_notuseful_real", "inc_notuseful_fake")]) == 1))
  # assign a string treatment of the second image shown
  data$image2_treatment <-                                "health_useful_real"
  data[data$health_useful_fake, "image2_treatment"] <-    "health_useful_fake"
  data[data$health_notuseful_real, "image2_treatment"] <- "health_notuseful_real"
  data[data$health_notuseful_fake, "image2_treatment"] <- "health_notuseful_fake"
  data[data$bjp_useful_real, "image2_treatment"] <-       "bjp_useful_real"
  data[data$bjp_useful_fake, "image2_treatment"] <-       "bjp_useful_fake"
  data[data$bjp_notuseful_real, "image2_treatment"] <-    "bjp_notuseful_real"
  data[data$bjp_notuseful_fake, "image2_treatment"] <-    "bjp_notuseful_fake"
  data[data$inc_useful_real, "image2_treatment"] <-       "inc_useful_real"
  data[data$inc_useful_fake, "image2_treatment"] <-       "inc_useful_fake"
  data[data$inc_notuseful_real, "image2_treatment"] <-    "inc_notuseful_real"
  data[data$inc_notuseful_fake, "image2_treatment"] <-    "inc_notuseful_fake"
  return(data)
}

add_aux_variables <- function(data) {
 
  data$survey_duration_mins <- as.numeric(data$DataCollection_FinishTime - data$DataCollection_StartTime)
  
  data$bjp_feel <- data$GRID_QGROUPIDEA_THERMO_1_QGROUPIDEA_THERMO
  data$inc_feel <- data$GRID_QGROUPIDEA_THERMO_2_QGROUPIDEA_THERMO
  data$hinduism_feel <- data$GRID_QGROUPIDEA_THERMO_3_QGROUPIDEA_THERMO
  data$islam_feel <- data$GRID_QGROUPIDEA_THERMO_4_QGROUPIDEA_THERMO
  
  
  #########
  # add variables on level of attention
  data$real_individual <- (data$QIND_SCREEN == "Yes") 
  data$real_group <- (data$QGROUP_SCREEN == "Yes") 
  data$searched_internet <- (data$QSEARCH_SCREEN == "Yes") 
  data$correct_attention_check <- (data$QATT_SCREEN == 'puce')
  # survey took too long
  data$too_long <- (data$survey_duration_mins > 80)
  
  return(data)
}


process_data <- function(data_file) {
  data <- read_excel(data_file)
  data$QATT_SCREEN <- tolower(data$QATT_SCREEN)
  data <- add_treatment_variables(data)
  #data <- add_numeric_variables(data)
  #data <- convert_to_factor(data)
  data <- add_aux_variables(data)
  
  return(data)
}



exclude_nonattentive <- function(data, real_individual, real_group, search_internet,
                                 attention_check, long_duration) {
  df <- data
  if (real_individual) {
    df <- df[df$real_individual,]
  }
  if (real_group) {
    df <- df[df$real_group,]
  }
  if (search_internet) {
    df <- df[!df$searched_internet,]
  }
  if (attention_check) {
    df <- df[df$correct_attention_check,]
  }
  if (long_duration) {
    df <- df[!df$too_long,]
  }
  return(df)
}

print_attentive_stats <- function(data) {
  percent_unreal_ind <- round(1 - (sum(data$real_individual) / nrow(data)), 4) * 100
  percent_unreal_group <- round(1 - (sum(data$real_group) / nrow(data)), 4) * 100
  percent_searched_internet <- round(sum(data$searched_internet) / nrow(data), 4) * 100
  percent_failed_attention <- round(1 - sum(data$correct_attention_check) / nrow(data), 4) * 100
  cat(paste0(percent_unreal_ind, '% of subjects entered an unreal name for individual contact.\n'))
  cat(paste0(percent_unreal_group, '% of subjects entered an unreal name for group contact.\n'))
  cat(paste0(percent_searched_internet, '% of subjects searched the internet during survey.\n'))
  cat(paste0(percent_failed_attention, '% of subjects failed the color attention check.\n'))
}
  
  
