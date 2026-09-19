#### Homework 1 ####
student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

## PROBLEM 1


# convert section to a factor with levels in the order "A", "B"
section <- factor(section, levels = c("A", "B"))

class(section)

# create a data frame named students containing all five vectors
students <- data.frame(
  student_id = student_id,
  section = section,
  quiz1 = quiz1,
  quiz2 = quiz2,
  passed = passed
)

dim(students)

# create score_matrix, with students as rows, quizzes as columns, student IDs as row names, and quiz1 and quiz2 as column names
score_matrix <- matrix(
  c(quiz1, quiz2),
  nrow = length(student_id),
  ncol = 2,
  dimnames = list(student_id, c("quiz1","quiz2"))
)

length(student_id)
class(student_id)
dim(score_matrix)

# create course_record, a list containing course = "R Programming", scores = students, and a named vector cutoffs = c(pass = 70, excellent = 90)
course_record <- list(
  course = "R Programming",
  scores = students,
  cuttoff = c(pass = 70, excellent = 90)
)

length(course_record)

# In four or fewer sentences, compare a vector, list, matrix, factor, and data frame
# A vector is a one dimensional structure that stores values of the same type. A list can store different types of objects that have
# different lengths while a matrix stores values in row and columns and must be the same type throughout. A factor represents categorical data
# using levels. A data frame is a two dimensional structure where its columns can have differnt types of data.



# extract student S04’s second quiz score from score_matrix
score_matrix["S04", "quiz2"]

# extract the first two matrix rows while preserving matrix dimensions
score_matrix[1:2, 1:2, drop = FALSE]

# extract course from course_record once with [, once with [[, and once with $
course_record["course"]
course_record[["course"]]
course_record$course

#  briefly explain the difference between ‘[’, ‘[[’ and ‘$’.
# [ selects a subset or element
# [[ extracts one element from a list and returns that element
# $ extracts a named element from a list using it name



# WITHOUT LOOPS 

# add average, the row-wise mean of the two quizzes, to students

# df$average creates a new column called average to the data frame
students$average <- (students$quiz1 + students$quiz2) / 2


# add excellent, equal to TRUE when average >= 90
students$excellent <- students$average >= 90

# select section A students whose average is at least 80;
section_a <- students$section == "A" & students$average >= 80 


# return only student_id, section, and average for that subset
students[section_a, c("student_id", "section", "average")]

# create a named numeric vector of all student averages, using student IDs as names
student_averages <- students$average
names(student_averages) <- students$student_id



# Briefly explain why your calculations are vectorized
# They are vectorized because R preforms the operations on the whole vector at once
# instead of using a loop to go through each element one at a time

csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"


## PROBLEM 2

# Use read.csv() with its text argument to to create measurements. Treat both blank fields and the text "NA" as missing.
measurements <- read.csv(
  text = csv_text,
  na.strings = c("", "NA")
)

# inspect the result with head(), str(), dim(), and names()
head(measurements)
str(measurements)
dim(measurements)
names(measurements)

# count missing values in each column
# sums each column where is.na creates a matrix of TRUE and FALSE of missing values where TRUE is 1 and FALSE is 0
colSums(is.na(measurements))

# create measurements_complete using complete.cases()
# select all columns of rows without NA
measurements_complete <- measurements[complete.cases(measurements),]

# report the sample IDs removed by the complete-case filter
measurements$sample_id[!complete.cases(measurements)]

# explain why x == NA is not a valid missing value test
# x == NA is not a valid test because it compares a value to NA which produces NA instead of producing TRUE or FALSE.
# is.na(x) should be used as it does produce TRUE or FALSE instead of comparing



# WIHTOUT A LOOP ARGUMENT

# convert site and status to factors and report their levels
measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)

levels(measurements$site)
levels(measurements$status)

# create temp_f = temp_c * 9 / 5 + 32
measurements$temp_f <- measurements$temp_c * 9 / 5 + 32


# create ph_below_7, preserving missing pH values
# if ph is missing return NA otherwise check if ph is < 7
measurements$ph_below_7 <- ifelse(
  is.na(measurements$ph),
  NA,
  measurements$ph < 7
)

# select complete observations from North or South whose status is "ok"
selected <- complete.cases(measurements) & 
  measurements$site %in% c("North", "South") &
  measurements$status == "ok"

# return only sample_id, site, temp_c, temp_f, and ph
measurements[selected, c("sample_id","site","temp_f","ph")]

# calculate the overall mean Celsius temperature and the South-site mean, ignoring missing values
mean(measurements$temp_c, na.rm = TRUE)
mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)


A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

# compute A * B and A%*%B report the dimensions of both results and explain briefly how element-wise multiplication differs
# from matrix multiplication
A * B
dim(A * B)

A %*% B
dim(A %*% B)

# A * B multiplies the corresponding elements of the two matrices row 1 column 1 of A is multiplied to row 1 column 1 of B.
# A %*% B multiplies a row of A by a column of B and adds the products row 1 of A is multiplied to column 1 of B and the products are added
# giving row 1 column 1 in the new matrix

## PROBLEM 3

student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)





# Use if, else if, and else to return grades A through F. Return NA_character_ when the score is missing. 
# Then test the function using the code below, and report its output.

grade_one <- function(
  score,
  a_min = 90,
  b_min = 80,
  c_min = 70,
  d_min = 60
) {
  if (is.na(score)){
    return(NA_character_)
  
  } else if (score >= a_min) {
    return("A")
    
  } else if (score >= b_min) {
    return("B")
    
  } else if (score >= c_min) {
    return("C")
      
  } else if (score >= d_min) {
    return("D")
  
  } else {
    return("F")
  }
  
}

grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

# Output
# NA
# "A"
# "B"
# "B"
# "C"

# the function checks if a score is missing otherwise it will check the grade boundaries from highest to lowest and return the letter grade corresponding to the boundary



# Create a character vector named grades with the same length as scores, initially filled with NA_character_.
# Use one for loop with seq_along(scores) to call grade_one(scores[i]) and store the result in grades[i].
# Because grade_one() already handles missing values, no separate missing-value condition is needed.
# After the loop, assign student_id as the names of grades, display the completed vector, and explain in one
# sentence what i represents during the loop.

grades <- rep(NA_character_, length(scores))

for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}

names(grades) <- student_id

grades

# i represents the current position in the scores vector. 



# It should return a named numeric vector containing total count, missing count, mean, standard deviation,
# minimum, and maximum. Round the four statistics to digits places. Call it once with defaults and once
# with named arguments.

summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  # create a named numeric vector   
   result <- c(
    total_count = length(x),
    missing_count = sum(is.na(x)),
    mean = mean(x, na.rm = na.rm),
    standard_deviation = sd(x, na.rm = na.rm),
    minimum = min(x, na.rm = na.rm),
    maximum = max(x, na.rm = na.rm)
  )
  
  # round the statistics to digit places
  result[c("mean","standard_deviation","minimum","maximum")] <-
    round(result[c("mean","standard_deviation","minimum","maximum")], digits)
  
  return(result)
}

summarize_scores(scores)
summarize_scores(x = scores, na.rm = TRUE, digits = 1)

summarize_scores( x = scores, na.rm = TRUE, digits = 2 )


# Also write plot_scores <- function(x, ...) so that it plots scores against their positions.

plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}

plot_scores(
  scores,
  type = "b",
  pch = 19,
  xlab = "Position",
  ylab = "Score",
  main = "Student Scores"
)

# ... allow us to pass any number of additional arguments to the plot() function inside the function plot_scores()

