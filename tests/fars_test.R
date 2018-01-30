library(fars)
expect_that(fars_summarize_years(c('2013','2014')),is_a('data.frame'))
