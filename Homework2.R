#### Homework 2 ####

library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)

# Problem 1

sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)


sales_clean <- sales_raw

# make item_clean remove extra spaces and convert to lowercase
sales_clean <- sales_clean |>
  mutate(
    item_clean = str_to_lower(
      str_squish(item_raw)
    )
  )

# use parse_number to create amount preserves missing values as NA
sales_clean <- sales_clean |>
  mutate(
    amount = parse_number(amount_raw)
  )

# use parse_number to create discount_rate as a proportion
sales_clean <- sales_clean |>
  mutate(
    discount_rate = parse_number(discount_raw) / 100
  )

# net_amount is amount * (1 - discount_rate)
sales_clean <- sales_clean |>
  mutate(
    net_amount = amount * (1 - discount_rate)
  )

# case_when(
# condition_1 ~ result_1,
# condition_2 ~ result_2

sales_clean <- sales_clean |>
  mutate(
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      TRUE ~ "small"
    )
  )

# display sale id, item clean, amount, discount rate, net amount, and size

sales_clean |>
  select(
    sale_id,
    item_clean,
    amount,
    discount_rate,
    net_amount,
    size
  )


sales_codes <- sales_clean

# make code clean trim spaces and uppercase
sales_codes <- sales_codes |>
  mutate(
    code_clean = str_to_upper(
      str_trim(code_raw)
    )
  )

sales_codes <- sales_codes |>
  mutate(
    valid_code = str_detect(
      code_clean,
      "^(ST|ON)-[0-9]{3}$"
    )
  )

invalid_codes <- sales_codes |>
  filter(!valid_code) |>
  select(
    sale_id,
    code_raw,
    code_clean
  )

invalid_codes

# create valid code parts with only valid rows
valid_code_parts <- sales_codes |>
  filter(valid_code)



# split code clean into channel and number
valid_code_parts <- valid_code_parts |>
  separate_wider_delim(
    code_clean,
    delim = "-",
    names = c("channel", "number"),
    cols_remove = FALSE
  )



valid_code_parts |>
  select(
    sale_id,
    code_clean,
    channel,
    number
  )

# create sale tags from sale id and tags in sales clean
sale_tags <- sales_clean |>
  select(
    sale_id,
    tags
  )

# split tags into more than 1 row using ;
sale_tags <- sale_tags |>
  separate_longer_delim(
    tags,
    delim = ";"
  )

sale_tags

# explain the difference between separate wider vs separate longer delim
# separate_wider_delim() splits a string into one or more columns while separate_longer_delim()
# splits a string into one or more rows


# create customer summary with summarize and .by customer id
customer_summary <- sales_clean |>
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if (sum(!is.na(net_amount)) == 0) {
      NA_real_
    } else {
      sum(net_amount >= 20, na.rm = TRUE) /
        sum(!is.na(net_amount))
    },
    .by = customer_id
  )

customer_summary |> arrange(customer_id)

# C1 numerator is 0 because none of the known net amounts are greater than or equal to 20
# C1 denominator is 1 because C1 has one sale with a known net amount so its large share is 0/1 which is 0


sales_factors <- sales_clean |>
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(
      factor(item_clean),
      n = 2,
      other_level = "Other"
    )
  )

levels(sales_factors$item_factor)

levels(sales_factors$item_group)

sales_factors |>
  count(item_factor)

sales_factors |>
  count(item_group)

# Changing level order does not combine categories it simply rearranges the sequence
# the existing categories appear and does not change the number of categories
# Combining categories merges tow or more different factor levels into a single
# category this reduces the total number of unique levels


# Problem 2

reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)
customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)


# create reports labeled sort by form and line
reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()

# create reports clean
reports_clean <- reports_labeled |>
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )

reports_clean |>
  select(
    form,
    line,
    customer_id,
    round,
    primary_amount,
    backup_amount,
    amount
  )

# coalesce() returns the first non missing value that appears
# so primary amount is kept even when it is zero
# as zero is not treated a missing value in the context of the problem
# the backup us used only if the primary amount is NA which includes -999

# create reports complete mark original rows before using complete()
reports_to_complete <- reports_clean |>
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE)

reports_complete <- reports_to_complete |>
  complete(
    customer_id,
    round = 1:3,
    fill = list(recorded_row = FALSE)
  )

# before 
reports_clean |>
  summarise(
    original_rows = n(),
    added_rows = 0,
    missing_amounts = sum(is.na(amount))
  )

# after
reports_complete |>
  summarise(
    original_rows = sum(recorded_row),
    added_rows = sum(!recorded_row),
    missing_amounts = sum(is.na(amount))
  )

reports_complete

# explicit is C3 round 1 is it is in the original report but the amount is missing
# implicit is C1 round 2 as it is not in the original report and has an amount of NA since it was added by complete()


# check customers customer id and customer/round for any duplicates
customers |>
  summarise(
    missing_ids = sum(is.na(customer_id)),
    duplicate_ids = sum(duplicated(customer_id))
  )


# empty so no combination appears more than once
reports_clean |>
  count(customer_id, round) |>
  filter(n > 1)


# create customers marked 
customers_marked <- customers |>
  mutate(customer_found = TRUE)

# left joun reports clean and customers marked
reports_joined <- reports_clean |>
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  )

reports_joined <- reports_joined |>
  mutate(
    customer_found = coalesce(customer_found, FALSE)
  )

reports_joined |>
  select(
    customer_id,
    round,
    amount,
    region,
    customer_found
  )


# anti join to find customer ids only in reports clean
reports_only <- reports_clean |>
  distinct(customer_id) |>
  anti_join(
    customers,
    by = join_by(customer_id),
    na_matches = "never"
  )

reports_only

# anti join to find customer ids only in customers
customers_only <- customers |>
  distinct(customer_id) |>
  anti_join(
    reports_clean,
    by = join_by(customer_id),
    na_matches = "never"
  )

customers_only

# customer found is true for C3 since its id exists in customers but its
# region is missing customer found is false for C4 since its id does not
# exists in customers so customer found tells us the difference from
# a customers who exists but has a missing region from a customer
# whose record does not exists

# customers_bad is never mention anywhere in the homework instructions
# so I am making my own with a duplicate C2 
# adding a second record of C2 that has a different region 
customers_bad <- bind_rows(
  customers,
  tibble(
    customer_id = "C2",
    region = "West"
  )
)

# left join many to many
reports_duplicated <- reports_clean |>
  left_join(
    customers_bad,
    by = join_by(customer_id),
    relationship = "many-to-many",
    na_matches = "never"
  )

# compare row counts before and after
reports_clean |>
  summarise(
    rows = n(),
    total_amount = sum(amount, na.rm = TRUE)
  )

reports_duplicated |>
  summarise(
    rows = n(),
    total_amount = sum(amount, na.rm = TRUE)
  )


# the original total is 75 which increase to 95 as an additonal amount of
# 20 is added from the duplicate C2

# repeated report of C2
reports_duplicated |>
  count(customer_id, round) |>
  filter(n > 1)

# if "many-to-one" was used the join would throw an error since C2 would
# match more than one row in customers_bad "many-to-many" allows the join to work
# but does not fix the conflicting information it just allows multiple matches
# which can duplicate reports and cause total to increase

# create reports wide
reports_wide <- reports_complete |>
  select(
    customer_id,
    round,
    amount
  ) |>
  pivot_wider(
    names_from = round,
    values_from = amount,
    names_prefix = "round_"
  )

reports_wide

# I would predict 5 rows would remain after pivoting back to long with
# values_drop_na = TRUE as there are 5 non missing values

# values_drop_na = TRUE removes rows where their amounts are NA this counts
# for original reports with unknown amounts and the new customer round combinations added
# it does not determine the values that were never recorded for example
# the missing amount for C3 in round 1 is lost instead of being recovered



# Problem 3


two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  
  # Calculate sample sizes, means, and variances
  nx <- length(x)
  ny <- length(y)
  
  mean_x <- mean(x)
  mean_y <- mean(y)
  
  var_x <- var(x)
  var_y <- var(y)
  
  # Choose the appropriate two-sample t-test
  if (equal_var == TRUE) {
    
    # Pooled two-sample t-test
    sp2 <- ((nx - 1) * var_x + (ny - 1) * var_y) /
      (nx + ny - 2)
    
    standard_error <- sqrt(sp2 * (1 / nx + 1 / ny))
    
    df <- nx + ny - 2
    
    method <- "Pooled"
    
  } else {
    
    # Welch's two-sample t-test
    standard_error <- sqrt(var_x / nx + var_y / ny)
    
    df <- (var_x / nx + var_y / ny)^2 /
      ((var_x / nx)^2 / (nx - 1) +
         (var_y / ny)^2 / (ny - 1))
    
    method <- "Welch"
  }
  
  # Calculate the t-statistic
  t_stat <- (mean_x - mean_y) / standard_error
  
  # Calculate the p-value
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  
  # Determine whether to reject the null hypothesis
  reject <- p_value < alpha
  
  
  # Return the results as a list
  return(list(
    method = method,
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    reject = reject
  ))
}


x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

function_welch <- two_sample_t(x_check, y_check)
function_pooled <- two_sample_t(x_check, y_check, equal_var = TRUE)

function_welch
function_pooled


# check function using t-test
t_test_welch <- t.test(
  x_check,
  y_check,
  var.equal = FALSE
)


t_test_pooled <- t.test(
  x_check,
  y_check,
  var.equal = TRUE
)


t_test_welch
t_test_pooled

# Compare the function results with t.test()
welch_differences <- c(
  t_stat = abs(
    function_welch$t_stat - t_test_welch$statistic
  ),
  df = abs(
    function_welch$df - t_test_welch$parameter
  ),
  p_value = abs(
    function_welch$p_value - t_test_welch$p.value
  )
)

pooled_differences <- c(
  t_stat = abs(
    function_pooled$t_stat - t_test_pooled$statistic
  ),
  df = abs(
    function_pooled$df - t_test_pooled$parameter
  ),
  p_value = abs(
    function_pooled$p_value - t_test_pooled$p.value
  )
)

welch_differences
pooled_differences

# Check whether all differences are smaller than 1e-8
all(welch_differences < 1e-8)
all(pooled_differences < 1e-8)


B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05
scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

set.seed(2026)

# Allocate storage for all simulation results
total_rows <- nrow(scenarios) * B * 2

simulation_results <- data.frame(
  scenario = character(total_rows),
  replicate = integer(total_rows),
  method = character(total_rows),
  t_stat = numeric(total_rows),
  df = numeric(total_rows),
  p_value = numeric(total_rows),
  reject = logical(total_rows)
)

# create row index
row_index <- 1

# Loop through each scenario and repeat B times
for (i in 1:nrow(scenarios)) {
  for (b in 1:B) {
    # Generate independent samples
    x <- rnorm(
      n = n_x,
      mean = scenarios$mean_x[i],
      sd = scenarios$sd_x[i]
    )
    
    y <- rnorm(
      n = n_y,
      mean = scenarios$mean_y[i],
      sd = scenarios$sd_y[i]
    )
    
    # Apply Welch's t-test and save the results
    welch_result <- two_sample_t(x, y, equal_var = FALSE, alpha = alpha)
    
    simulation_results[row_index, ] <- list(
      scenarios$scenario[i],
      b,
      welch_result$method,
      welch_result$t_stat,
      welch_result$df,
      welch_result$p_value,
      welch_result$reject
    )
    
    row_index <- row_index + 1
    
    # Apply the pooled t-test to the same samples and save results
    
    pooled_result <- two_sample_t(x, y, equal_var = TRUE, alpha = alpha)
  
    simulation_results[row_index, ] <- list(
      scenarios$scenario[i],
      b,
      pooled_result$method,
      pooled_result$t_stat,
      pooled_result$df,
      pooled_result$p_value,
      pooled_result$reject
    )
    
    row_index <- row_index + 1
  }
  
}

# Display the first six rows and total number of rows
head(simulation_results)
nrow(simulation_results)

# create simulation summary 

simulation_summary <- simulation_results |>
  group_by(scenario, method) |>
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .groups = "drop"
  )


simulation_summary |>
  mutate(
    mean_t = round(mean_t, 3),
    sd_t = round(sd_t, 3),
    rejection_rate = round(rejection_rate, 3)
  )


# REPORT
# When population means are equal rejection rates should be around 0.05 but
# random simulation variation may cause small differences. When the population means
# differ the rejection rate should increase as they have greater power to detect the difference.
# Higher power is not enough to judge a test as a test with an exaggerated
# Type I error rate may reject the null too often when it is actually true.