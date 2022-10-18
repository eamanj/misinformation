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
  data$opp_useful <- data$MESSAGE1_IMG_3 == "Yes"
  data$pti_useful <- data$MESSAGE1_IMG_4 == "Yes"
  data$opp_notuseful <- data$MESSAGE1_IMG_5 == "Yes"
  data$pti_notuseful <- data$MESSAGE1_IMG_6 == "Yes"
  # ensure only one image1 is set to true
  stopifnot(all(rowSums(data[,c("health_useful", "health_notuseful",
                                "pti_useful", "pti_notuseful",
                                "opp_useful", "opp_notuseful")]) == 1))
  # assign a string treatment of the first image shown
  data$image1_treatment <-                           "health_useful"
  data[data$health_notuseful, "image1_treatment"] <- "health_notuseful"
  data[data$pti_useful, "image1_treatment"] <-       "pti_useful"
  data[data$pti_notuseful, "image1_treatment"] <-    "pti_notuseful"
  data[data$opp_useful, "image1_treatment"] <-       "opp_useful"
  data[data$opp_notuseful, "image1_treatment"] <-    "opp_notuseful"
  
  # figure out which message2 political/utility/plausibility was shown
  data$health_useful_real <- data$MESSAGE2_IMG_1 == "Yes"
  data$health_useful_fake <- data$MESSAGE2_IMG_2 == "Yes"
  data$health_notuseful_real <- data$MESSAGE2_IMG_3 == "Yes"
  data$health_notuseful_fake <- data$MESSAGE2_IMG_4 == "Yes"
  data$opp_useful_real <- data$MESSAGE2_IMG_5 == "Yes"
  data$opp_useful_fake <- data$MESSAGE2_IMG_6 == "Yes"
  data$pti_useful_real <- data$MESSAGE2_IMG_7 == "Yes"
  data$pti_useful_fake <- data$MESSAGE2_IMG_8 == "Yes"
  data$opp_notuseful_real <- data$MESSAGE2_IMG_9 == "Yes"
  data$opp_notuseful_fake <- data$MESSAGE2_IMG_10 == "Yes"
  data$pti_notuseful_real <- data$MESSAGE2_IMG_11 == "Yes"
  data$pti_notuseful_fake <- data$MESSAGE2_IMG_12 == "Yes"
  # ensure only one image2 is set to true
  stopifnot(all(rowSums(data[,c("health_useful_real", "health_useful_fake", "health_notuseful_real", "health_notuseful_fake",
                                "pti_useful_real", "pti_useful_fake", "pti_notuseful_real", "pti_notuseful_fake",
                                "opp_useful_real", "opp_useful_fake", "opp_notuseful_real", "opp_notuseful_fake")]) == 1))
  # assign a string treatment of the second image shown
  data$image2_treatment <-                                "health_useful_real"
  data[data$health_useful_fake, "image2_treatment"] <-    "health_useful_fake"
  data[data$health_notuseful_real, "image2_treatment"] <- "health_notuseful_real"
  data[data$health_notuseful_fake, "image2_treatment"] <- "health_notuseful_fake"
  data[data$pti_useful_real, "image2_treatment"] <-       "pti_useful_real"
  data[data$pti_useful_fake, "image2_treatment"] <-       "pti_useful_fake"
  data[data$pti_notuseful_real, "image2_treatment"] <-    "pti_notuseful_real"
  data[data$pti_notuseful_fake, "image2_treatment"] <-    "pti_notuseful_fake"
  data[data$opp_useful_real, "image2_treatment"] <-       "opp_useful_real"
  data[data$opp_useful_fake, "image2_treatment"] <-       "opp_useful_fake"
  data[data$opp_notuseful_real, "image2_treatment"] <-    "opp_notuseful_real"
  data[data$opp_notuseful_fake, "image2_treatment"] <-    "opp_notuseful_fake"
  return(data)
}

add_aux_variables <- function(data) {
  data$survey_duration_mins <- as.numeric(data$DataCollection_FinishTime - data$DataCollection_StartTime)
  
  # add variables on level of attention
  data$real_individual <- (grepl("Yes", data$QIND_SCREEN))
  data$real_group <- (grepl("Yes", data$QGROUP_SCREEN))
  data$searched_internet <- (grepl("Yes", data$QSEARCH_SCREEN))
  # color checks
  correct_patterns <- c("arag", "arg", "arq", "aurg", "aurq", "أرج", "اراغ", "ارغ", "ارگ")
  data$correct_attention_check <- (grepl(paste(correct_patterns, collapse='|'), data$QATT_SCREEN))
  correct_patterns2 <- c("red", "green")
  data$correct_attention_check2 <- (grepl(paste(correct_patterns2, collapse='|'), data$QATT_SCREEN02))
  data$correct_both_attention_checks <- (data$correct_attention_check & data$correct_attention_check2)
  data$correct_atleast_one_attention_check <- (data$correct_attention_check | data$correct_attention_check2)
  data$correct_only_one_attention_check <- (data$correct_atleast_one_attention_check & !data$correct_both_attention_checks)
  # survey took too long (99% quantile)
  data$too_long <- (data$survey_duration_mins > quantile(data$survey_duration_mins, 0.99, na.rm=TRUE))
  
  return(data)
}


process_data <- function(data_file) {
  data <- read_excel(data_file)
  data$QATT_SCREEN <- tolower(data$QATT_SCREEN)
  data$QATT_SCREEN02 <- tolower(data$QATT_SCREEN02)
  data <- add_treatment_variables(data)
  #data <- add_numeric_variables(data)
  #data <- convert_to_factor(data)
  data <- add_aux_variables(data)
  
  return(data)
}



exclude_nonattentive <- function(data, real_individual, real_group, search_internet,
                                 attention_check, attention_check2, long_duration) {
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
  if (attention_check2) {
    df <- df[df$correct_attention_check2,]
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
  percent_failed_attention2 <- round(1 - sum(data$correct_attention_check2) / nrow(data), 4) * 100
  percent_failed_both_attentions <- round(1 - sum(data$correct_atleast_one_attention_check) / nrow(data), 4) * 100
  percent_failed_atleast_one_attention <- round(1 - sum(data$correct_both_attention_checks) / nrow(data), 4) * 100
  cat(paste0(percent_unreal_ind, '% of subjects entered an unreal name for individual contact.\n'))
  cat(paste0(percent_unreal_group, '% of subjects entered an unreal name for group contact.\n'))
  cat(paste0(percent_searched_internet, '% of subjects searched the internet during survey.\n'))
  cat(paste0(percent_failed_attention, '% of subjects failed the first color attention check.\n'))
  cat(paste0(percent_failed_attention2, '% of subjects failed the second color attention check.\n'))
  cat(paste0(percent_failed_both_attentions, '% of subjects failed both color attention check.\n'))
  cat(paste0(percent_failed_atleast_one_attention, '% of subjects failed at least one color attention check.\n'))
}          
  
