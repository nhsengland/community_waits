# run file

# name of region to run report for
region <- "SOUTH WEST"

source('libraries.R')

source('parameters.R')

source('data_functions.R')

source('data_load_wrangle.R')

quarto_render(input = 'output_report.qmd',
              output_file = paste0(format(Sys.Date(), '%y%m'),
                                   '_', 
                                   region, 
                                   '_comm_waits_12.html'))