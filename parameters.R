#####################
# report parameters #
#####################


# threshold of number of patients waiting over 52 weeks to run models for
threshold <- 50

# Waiting time target (weeks)
waiting_time_target <- 52

# primary target to achieve
# NOTE: It is not possible to calculate 100% achievement
target <- 0.99

achievement_value <- qexp(target)

# secondary waiting list target (18 weeks)
waiting_time_target_18 <- 18

# set target date for 52 wks
target_date <- as.Date("2026-03-31")

# secondary target to achieve
# NOTE: It is not possible to calculate 100% achievement
target_18 <- 0.92

achievement_value_18 <- qexp(target_18)

target_date_18 <- as.Date("2027-03-31")

# calibration window
# (How many months of past data to model on usually 6 or 12)

calibration <- 12

# These can be changed to support the main report.  However a number of features
# are currently hardcoded in the output report.  If parameters are changed, please
# double check that this is reflected in the output

