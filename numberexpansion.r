

num_years = 1
this_year <- lubridate::year(lubridate::today())

even <- this_year %% 2 == 0

if (even) {
    evenyears <- this_year + 2 * 0:num_years
    oddyears <- evenyears + 1
} else {
    oddyears <- this_year + 2 * 0:num_years
    evenyears <- oddyears + 1
}

oddtext = paste0(trimws(paste(oddyears, collapse = ", ")), ", etc.")
eventext = paste0(trimws(paste(evenyears, collapse = ", ")), ", etc.")


yearstext <- function(t, num_years = 2) {

    this_year <- lubridate::year(lubridate::today())

    if (this_year %% 2 == 0) {
        evenyears <- this_year + 2 * 0:(num_years - 1)
        oddyears <- evenyears + 1
    } else {
        oddyears <- this_year + 2 * 0:(num_years - 1)
        evenyears <- oddyears + 1
    }

    if (t == "odd") {
        return(paste0(trimws(paste(oddyears, collapse = ", ")), ", etc."))
    }
    else if (t == "even") {
        return(paste0(trimws(paste(evenyears, collapse = ", ")), ", etc."))
    }
    else return("")
}


yearstext("even")


this_year = 2021

eveny = this_year  + this_year %% 2 + 2 * 0:1
oddy = eveny - 1


yearstext2 <- function(t, num_years = 2) {

    this_year <- lubridate::year(lubridate::today())

    evenyears <- this_year  + this_year %% 2 + 2 * 0:(num_years - 1)
    oddyears <- evenyears - 1

    if (t == "odd") {
        return(paste0(trimws(paste(oddyears, collapse = ", ")), ", etc."))
    }
    else if (t == "even") {
        return(paste0(trimws(paste(evenyears, collapse = ", ")), ", etc."))
    }
    else return("")
}

yearstext2("even")