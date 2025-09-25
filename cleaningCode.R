library(tidyverse)
library(gendercoder)
library(janitor)
library(here) 
library(dplyr)
library(stringr)
library(forcats)
library(gt)
library(purrr)
cl = read.csv(here("cleaned_data.csv"))
cl = cl %>% 
  #For any row that is totally empty, drop it. 
  remove_empty(c("rows", "cols")) 
cl$age <- ifelse(cl$age > 50 | is.na(cl$age) | cl$age < 10, as.integer(mean(cl$age, na.rm=TRUE)), cl$age)
cl$age <- as.integer(cl$age)

cl$target_grade <- ifelse(cl$target_grade == "Fail", NA, cl$target_grade)

#Gender cleaning
cl$gender <- ifelse(cl$gender %in% c("NVIDIA RTX5090", "armored helicopter", "gender is a scam created by bathroom companies to sell more bathrooms. [female]", "helicopter", "assasin"), NA, cl$gender) 
cl$gender <- ifelse(cl$gender == "As in social role ? Male", "male", cl$gender) 
cl$gender <- toupper(cl$gender)
cl$gender[cl$gender %in% c("FEMALE", "F", "GIRL", "FEMAL", "FAMALE")] <- "F" 
cl$gender[cl$gender %in% c("MALE", "M", "MALE ♂", "MAN", "BLOKE")] <- "M" 
cl$gender <- ifelse(cl$gender %in% c("N/A", "HUMAN"), NA, cl$gender) 
cl$gender[!cl$gender %in% c("F", "M", NA)] <- "Others"

cl <- na.omit(cl)
cl$gender <- ifelse(cl$gender == "As in social role ? Male", "male", cl$gender) 
cl <- cl %>%                       # your original data frame
  mutate(across(
    where(is.character),           # look only at character columns
    ~ na_if(.x, "unknown")         # turn every "unknown" into NA
  )) %>% 
  drop_na()

cl <- cl %>% 
  
  ## ------------------------
## 1. assignment_preference
## ------------------------
mutate(
  assignment_preference = case_when(
    grepl("draw up.*schedule", assignment_preference, ignore.case = TRUE) ~ "schedule",
    grepl("do them immediately",   assignment_preference, ignore.case = TRUE) ~ "immediately",
    grepl("cram at the last second",assignment_preference, ignore.case = TRUE) ~ "cram",
    TRUE ~ NA_character_                                  # anything else → drop later
  )
) %>% 
  
  ## ----------------------------------------
## 2. trimester_or_semester - recode & keep
## ----------------------------------------
mutate(
  trimester_or_semester = case_when(
    grepl("trimester", trimester_or_semester, ignore.case = TRUE) &
      grepl("semester",  trimester_or_semester, ignore.case = TRUE)             ~ "trimester and semester",
    grepl("trimester", trimester_or_semester, ignore.case = TRUE)             ~ "trimester",
    grepl("semester",  trimester_or_semester, ignore.case = TRUE)             ~ "semester",
    TRUE ~ NA_character_                                  # anything else → drop later
  )
) %>% 
  
  ## ---------------------------------------------------
## 3. tendency_yes_no  (strip “More”, quotes, blanks)
## ---------------------------------------------------
mutate(
  tendency_yes_no = case_when(
    grepl("\\byes\\b", tendency_yes_or_no, ignore.case = TRUE) ~ "yes",
    grepl("\\bno\\b",  tendency_yes_or_no, ignore.case = TRUE) ~ "no",
    TRUE ~ NA_character_
  )
) %>% 
  
  ## ---------------------------------------------------
## 4. rent  (free-text; keep only yes/no references)
## ---------------------------------------------------
mutate(
  rent = case_when(
    grepl("\\byes\\b", pay_rent, ignore.case = TRUE) ~ "yes",
    grepl("\\bno\\b",  pay_rent, ignore.case = TRUE) ~ "no",
    TRUE ~ NA_character_
  )
) %>% 
  
  ## ---------------------------------------------
## 5. move_to_stall_choice  (A:/B:/C: → first/…)
## ---------------------------------------------
mutate(
  move_to_stall_choice = case_when(
    grepl("^\\s*A:", stall_choice, ignore.case = TRUE) ~ "first",
    grepl("^\\s*B:", stall_choice, ignore.case = TRUE) ~ "middle",
    grepl("^\\s*C:", stall_choice, ignore.case = TRUE) ~ "last",
    TRUE ~ NA_character_
  )
) %>% 
  
  ## -------------------------------------------------------
## 6. Final housekeeping:  drop any rows now containing NA
##    and convert cleaned columns to factors in one go
## -------------------------------------------------------
drop_na(
  assignment_preference,
  trimester_or_semester,
  tendency_yes_or_no,
  rent,
  stall_choice
) %>% 
  mutate(across(
    c(assignment_preference,
      trimester_or_semester,
      tendency_yes_or_no,
      rent,
      stall_choice),
    as.factor
  ))
unique(cl$tendency_yes_or_no)
cl$tendency_yes_or_no = as.character(cl$tendency_yes_or_no)
cl <- cl %>% 
  # -------------------------------------------------
# 1. tendency_yes_or_no  ── “More \"Yes\"” → yes, etc.
# -------------------------------------------------
mutate(
  tendency_yes_or_no = recode(tendency_yes_or_no,
                              `More "Yes"` = "yes",
                              `More "No"`  = "no",
                              .default     = tendency_yes_or_no           # leave plain “yes”/“no” unchanged
  ),
  tendency_yes_or_no = factor(tendency_yes_or_no)
) %>% 
  
  # -----------------------------------------------
# 2. stall_choice  ── starts-with A:/B:/C: mapping
# -----------------------------------------------
mutate(
  stall_choice = as.character(stall_choice),
  stall_choice = case_when(
    grepl("^\\s*A:", stall_choice) ~ "first",
    grepl("^\\s*B:", stall_choice) ~ "middle",
    grepl("^\\s*C:", stall_choice) ~ "last",
    TRUE                          ~ NA_character_        # anything else → drop later
  ),
  stall_choice = factor(stall_choice)
) %>% 
  
  # -----------------------------
# 3. living_arrangements factor
# -----------------------------
mutate(living_arrangements = factor(living_arrangements)) %>% 
  
  # -----------------------------------------------------------
# 4. weekly_alcohol  ── fix concatenated values (1015 ⇒ 15…)
# -----------------------------------------------------------
mutate(
  weekly_alcohol = case_when(
    weekly_alcohol == 1015 ~ 15,
    weekly_alcohol ==  510 ~ 10,
    weekly_alcohol == 1520 ~ 20,
    TRUE                   ~ weekly_alcohol
  )
) %>% 
  
  # -----------------------------
# 5. believe_in_aliens factor
# -----------------------------
mutate(believe_in_aliens = factor(believe_in_aliens))

cl <- cl %>% 
  mutate(
    # keep as character while we fix formats
    height = as.character(height),
    
    height = case_when(
      ## ---- explicit one-off fixes ---------------------------------
      height == "410.00" ~ "140.00",
      height ==  "6.00"  ~ "160.00",
      
      ## ---- 1.??   → multiply by 100  (1.70  → 170)
      str_detect(height, "^1\\.\\d+$") ~ as.character(as.numeric(height) * 100),
      
      ## ---- 1x.??  → multiply by 10   (16.8 → 168)
      str_detect(height, "^1\\d\\.\\d+$") ~ as.character(as.numeric(height) * 10),
      
      ## ---- two-digit values (e.g., 68.00, 59.00) → prepend “1”
      str_detect(height, "^[0-9]{2}\\.\\d+$") ~ paste0("1", height),
      
      ## ---- three-digit outliers beginning with “5” (e.g., 510.00, 511.00)
      ##      treat as centimetres already but missing “1” prefix  → subtract 350
      str_detect(height, "^5\\d\\d\\.\\d+$") ~ as.character(as.numeric(height) - 350),
      
      ## ---- any remaining numeric strings stay unchanged
      TRUE ~ height
    ),
    
    # convert back to numeric
    height = as.numeric(height)
  )
cl$height[cl$height == 410] <- 140
cl$height[cl$height == 511] <- 151
cl$height[cl$height == 510] <- 150
cl$height[cl$height == 51]  <- 151
cl$height[cl$height == 68]  <- 168
cl$height[cl$height == 6]   <- 160
cl$height[cl$height == 53]  <- 153
cl$height[cl$height == 17]  <- 170
cl$height[cl$height == 59]  <- 159 
cl$height[cl$height == 61]  <- 161
unique(cl$height) 

cl <- cl %>%
  mutate(commute = str_split(commute, ",\\s*")) %>%
  mutate(commute = lapply(commute, function(x) paste(sort(x), collapse = ", ")))
unique(cl$)
cl <- cl %>%
  mutate(work_status = as.factor(work_status))
cl <- cl %>% 
  filter(average_daily_sleep != -Inf) %>% 
  mutate(average_daily_sleep = str_extract_all(average_daily_sleep, "\\d+(\\.\\d+)?")) %>%
  mutate(average_daily_sleep = lapply(average_daily_sleep, as.numeric)) %>%
  mutate(average_daily_sleep = sapply(average_daily_sleep, max, na.rm = TRUE)) 
unique(cl$average_daily_sleep)

# Keep rows with time (has ":")
cl <- cl %>%
  filter(str_detect(as.character(usual_bedtime), ":")) %>%
  mutate(usual_bedtime = format(as.POSIXct(usual_bedtime), "%H:%M:%S"))
cl <- cl %>%
  mutate(sibling_count = as.numeric(sibling_count)) %>%
  filter(!is.na(sibling_count))

cl <- cl %>%
  mutate(allergy_count = str_trim(tolower(as.character(allergy_count)))) %>%
  mutate(allergy_count = case_when(
    allergy_count %in% c("i don't know", "shellfish", "-", "null", "", "na") ~ NA_character_,
    allergy_count %in% c("no", "none", "nothing", "zero") ~ "0",
    allergy_count %in% c("one", "a") ~ "1",
    allergy_count == "two" ~ "2",
    allergy_count == "three" ~ "3",
    TRUE ~ allergy_count
  )) %>%
  mutate(allergy_count = as.numeric(allergy_count)) %>%
  filter(!is.na(allergy_count))
cl <- cl %>%
  mutate(diet_style = str_trim(tolower(diet_style))) %>%
  filter(!diet_style %in% c("instagram carnivore diet", "none", "no diet")) %>%
  mutate(diet_style = ifelse(diet_style == "normal", "Omnivorous", str_to_title(diet_style)))
cl <- cl %>%
  mutate(favourite_letter = tolower(favourite_letter)) %>%
  filter(favourite_letter != "all good for me")
cl <- cl %>%
  mutate(drivers_license = tolower(drivers_license)) %>%
  mutate(drivers_license = case_when(
    drivers_license "Learner License" ~ "Yes",
    TRUE ~ str_to_title(drivers_license)  # Capitalize others
  )) %>%
  mutate(drivers_license = as.factor(drivers_license))
cl <- cl %>%
  # Lowercase `country_of_brith` first
  mutate(country_of_birth = tolower(country_of_birth)) %>%
  
  # Convert selected columns to factors
  mutate(across(c(
    computer_os,
    steak_preference,
    dominant_hand,
    assignments_on_time,
    used_r_before,
    university_year,
    country_of_birth, 
    enrolled_unit, 
    relationship_status
  ), as.factor))

cl <- cl %>%
  mutate(drivers_license = as.factor(drivers_license)
unique(cl$drivers_license)
cl <- cl[, -which(names(cl) == "drivesr_license")]

cl <- cl %>% 
  mutate(
    # work as lowercase text for easier pattern‐matching
    daily_short_video_time = tolower(as.character(daily_short_video_time)),
    
    # convert every entry to a numeric count of hours
    daily_short_video_time = case_when(
      # 1) explicit “never” → 0 h
      daily_short_video_time %in% c("never")                     ~ 0,
      
      # 2) values like “1-2 hours”, “2-3”  → mean of the range
      str_detect(daily_short_video_time, "^\\d+(\\.\\d+)?-\\d+") ~ {
        nums <- str_extract_all(daily_short_video_time, "\\d+(\\.\\d+)?")[[1]]
        mean(as.numeric(nums))
      },
      
      # 3) mixed formats such as “3h30m”, “1hr24min”, “3h30” …
      str_detect(daily_short_video_time, "(\\d+)h(\\d+)")        ~ {
        h <- as.numeric(str_extract(daily_short_video_time,  "\\d+(?=h)"))
        m <- as.numeric(str_extract(daily_short_video_time,  "(?<=h)\\d+"))
        h + m/60
      },
      
      # 4) pure hours with text, e.g. “4 hours”, “1.5 hrs”, “1 hour ish”
      str_detect(daily_short_video_time, "\\d+\\s?h") |
        str_detect(daily_short_video_time, "\\d+\\s?hr") |
        str_detect(daily_short_video_time, "\\d+\\s?hours?")       ~
        as.numeric(str_extract(daily_short_video_time, "\\d+(\\.\\d+)?")),
      
      # 5) pure minutes, e.g. “30 min”, “45mins”, “50 minutes”, “10 minutes?”
      str_detect(daily_short_video_time, "\\d+\\s?min")          ~
        as.numeric(str_extract(daily_short_video_time, "\\d+(\\.\\d+)?")) / 60,
      
      # 6) numbers only (interpreted directly as hours)  → “10”, “8.0”, “0.1”, “3+”
      str_detect(daily_short_video_time, "^\\d+(\\.\\d+)?\\+?$") ~
        as.numeric(str_remove(daily_short_video_time, "\\+$")),
      
      # 7) fall-back: anything unparseable → NA
      TRUE ~ NA_real_
    )
  )

cl$favourite_anime <- ifelse(
  grepl("(^$|no|none|don't|do not|never|not sure|don't have|dont have|n/a|nothing|/|^-$|NA|dog|cat|disney|spongebob|tom and jerry|avatar|border collie|jackie chen|scooby-doo|three body problem|usagi|tiger)", 
        cl$favourite_anime, 
        ignore.case = TRUE),
  NA,
  cl$favourite_anime
)
cl$favourite_anime <- ifelse(cl$favourite_anime == "dont wstch", NA, cl$favourite_anime) 

cl$fluent_languages <- sapply(cl$fluent_languages, function(x) {
  # Extract all numeric values (integers or decimals) from the string
  nums <- as.numeric(unlist(regmatches(x, gregexpr("\\d+(\\.\\d+)?", x))))
  
  # If any numbers are found, return the max (to handle ranges like "2-3"), else NA
  if (length(nums) > 0) {
    return(max(nums, na.rm = TRUE))
  } else {
    return(NA_real_)
  }
})
cl$fluent_languages <- ifelse(cl$fluent_languages == "1 fluently, 6 conversationally", "1", cl$fluent_languages) 
cl$fluent_languages <- ifelse(cl$fluent_languages == "One", "1", cl$fluent_languages)   
cl$fluent_languages <- ifelse(cl$fluent_languages == "1.5", "1", cl$fluent_languages)   
# Text to number mapping
word_to_number <- c(
  "zero" = 0, "one" = 1, "two" = 2, "three" = 3, "four" = 4,
  "five" = 5, "six" = 6, "seven" = 7, "eight" = 8, "nine" = 9,
  "ten" = 10
)

# Clean and convert readable_languages column
cl$readable_languages <- sapply(cl$readable_languages, function(x) {
  # 1. Extract digit-based numbers
  nums <- as.numeric(unlist(regmatches(x, gregexpr("\\d+(\\.\\d+)?", x))))
  
  # 2. If no digit-based numbers, check for word-based numbers
  if (length(nums) == 0) {
    words <- tolower(unlist(strsplit(x, "[^a-zA-Z]+")))  # split into words
    matched_words <- words[words %in% names(word_to_number)]
    
    if (length(matched_words) > 0) {
      nums <- word_to_number[matched_words]
    }
  }
  
  # 3. Return max if numbers found, else NA
  if (length(nums) > 0) {
    return(max(nums, na.rm = TRUE))
  } else {
    return(NA_real_)
  }
})

cl$readable_languages <- ifelse(cl$readable_languages == "1.5", "1", cl$readable_languages)  
cl$pay_rent <- cl$rent
cl$commute <- cl$commute_clean
library(dplyr)

cl <- cl %>%
  select(-tendency_yes_no, -move_to_stall_choice, -commute_clean, -rent)


# 1. Cleaning function: extract numeric values, take max if range, else NA
clean_shoe <- function(x) {
  nums <- as.numeric(unlist(regmatches(x, gregexpr("\\d+(\\.\\d+)?", x))))
  if (length(nums) > 0) {
    return(max(nums, na.rm = TRUE))
  } else {
    return(NA_real_)
  }
}

# 2. Apply cleaning to cl$shoe_size
cl$shoe_size <- sapply(cl$shoe_size, clean_shoe)

# 3. (Optional) Convert from EU sizes to Australian sizes (rough estimate)
# Note: Adjust mapping depending on your data context (men’s or women’s sizes)
eu_to_au <- function(eu_size) {
  return(eu_size - 33)  # rough estimate for men’s sizes
}
undo <- function(size) {
  return(size + 33)  # rough estimate for men’s sizes
}
cl$shoe_size <- eu_to_au(cl$shoe_size)
cl$shoe_size <- undo(cl$shoe_size)

# Dummy conversion functions - replace with your actual conversions
eu_to_au <- function(size) { size - 33 }
us_to_au <- function(size) { size + 1 }
uk_to_au <- function(size) { size + 1.5 }

convert_shoe_size <- function(size) {
  if (is.na(size)) return(NA_real_)
  
  # Assuming input is numeric
  if (size >= 35 && size <= 50) {
    # Likely EU size
    return(eu_to_au(size))
  } else if (size >= 5 && size <= 15) {
    # Likely US or UK size - need more logic here if you want to distinguish US vs UK
    # For now assume US:
    return(us_to_au(size))
  } else if (size >= 3 && size < 5) {
    # Possibly UK or kids sizes (very rare)
    return(uk_to_au(size))
  } else {
    # Out of expected range - return NA
    return(NA_real_)
  }
}

# Convert column (first convert to numeric, coercing errors to NA)
cl$shoe_size_num <- as.numeric(cl$shoe_size)

# Apply conversion
shoe_size_au <- sapply(cl$shoe_size_num, convert_shoe_size)
cl$shoe_size <- shoe_size_au




cl <- cl %>% 
  mutate(
    books_read_highschool = str_to_lower(as.character(books_read_highschool)),
    
    # extract every numeric token (allows decimals)
    books_read_highschool = str_extract_all(books_read_highschool,
                                            "\\d+\\.?\\d*"),
    
    # convert the token list → single numeric value
    books_read_highschool = map_dbl(books_read_highschool, function(v) {
      if (length(v) == 0)            return(NA_real_)      # no digits → NA
      nums <- as.numeric(v)
      
      if (length(nums) == 1)         return(nums)          # single number
      mean(nums)                                          # ranges (e.g. 50-100) → mid-point
    })
  )


clean_to_litres <- function(txt) {
  txt  <- str_to_lower(txt)

  # pull every numeric fragment (allows decimals)
  nums <- str_extract_all(txt, "\\d+\\.?\\d*")[[1]]
  if (length(nums) == 0) return(NA_real_)
  nums <- as.numeric(nums)

  # helper → average if we captured a range (e.g. "1-2")
  mean_or_single <- if (length(nums) > 1) mean(nums) else nums[1]

  # 1) explicit ‘cup’ → convert using 1 cup ≈ 0.25 L
  if (str_detect(txt, "cup"))    return(mean_or_single * 0.25)

  # 2) contains “ml”, “mL”, “mls” … → convert mL → L
  if (str_detect(txt, "ml"))     return(mean_or_single / 1000)

  # 3) contains “l”, “litre”, “liters” (but not “ml”) → already litres
  if (str_detect(txt, "\\bl|litre|liter")) return(mean_or_single)

  # 4) hyphen range like “1-2” with no unit → average, assume litres
  if (str_detect(txt, "\\d\\s*-\\s*\\d"))  return(mean_or_single)

  # 5) bare number — treat as litres **unless** implausibly huge (>30 L)
  if (mean_or_single <= 30)      return(mean_or_single)

  # otherwise can’t be sure → NA
  NA_real_
}

cl <- cl %>%
  mutate(
    daily_water_intake_l = map_dbl(as.character(daily_water_intake_l),
                                   clean_to_litres)
  ) 
cl$target_grade <- as.factor(cl$target_grade)
cl$commute <- sapply(cl$commute, \(x) paste(unlist(x), collapse = ", "))

write.csv(cl, "cleaned.csv", row.names = FALSE)






