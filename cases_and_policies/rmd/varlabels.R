varlabels <-
  data.frame(
  t(matrix(c(
    "dlogdc", "$\\Delta \\log \\Delta C_{it}$",
    "log(cases[t]-cases[t-7]) - log(cases[t-7] - cases[t-14])", "NYT",TRUE,

    "dlogdd", "$\\Delta \\log \\Delta D_{it}$",
    "log(deaths[t]-deaths[t-7]) - log(deaths[t-7] - deaths[t-14])", "NYT",TRUE,

    "dlogtests", "$\\Delta \\log T_{it}$",
    "log(tests from t to t-7) - log(tests from t-7 to t-14)",
    "Covid Tracking Project",TRUE,

    "testrate", "tests per 1000", "tests in past week per 1000 people",
    "Covid Tracking Project",TRUE,

    "logdc", "$\\log \\Delta C_{it}$",
    "log(cases[t]-cases[t-7])", "NYT", TRUE,

    "logdd", "$\\log \\Delta D_{it}$",
    "log(deaths[t]-deaths[t-7])", "NYT", TRUE,

    "logt", "log(days since 2020-01-15)", "log(days since January 15, 2020)", "", TRUE,
    

    "testratedc", "$\\frac{\\Delta{D}}{\\Delta{C}} T_{it}$",
    "tests in past week time deaths in past week divided by cases in past week",
    "NYT, Covid Tracking Project",TRUE,

    "Population.2018", "population", "Population in 2018",
    "Raifman et al",TRUE,

    "Square.Miles", "square miles", "State area in square miles",
    "Raifman et al",TRUE,

    "Percent.Unemployed..2018.", "Unemployment",
    "State unemployment in 2018", "Raifman et al",TRUE,

    "Percent.living.under.the.federal.poverty.line..2018.", "Poverty",
    "% under poverty line in 2018", "Raifman et al",TRUE,

    "Percent.at.risk.for.serious.illness.due.to.COVID", "Comorbidity",
    "% at risk for serious illness due to COVID", "Raifman et al",TRUE,

    "pmovie", "closed movie theaters",
    "closed movie theaters", "Raifman et al",TRUE,

    "pshelter", "stay at home",
    "stay at home / shelter in place", "Raifman et al",TRUE,

    "pk12", "closed K-12 schools", "closed K-12 schools",
    "Raifman et al",TRUE,

    "prestaurant","closed restaurants", "closed restaurants except takeout",
    "Raifman et al",TRUE,

    "pnonessential","closed non-essent bus", "closed non-essential businesses",
    "Raifman et al",TRUE,

    "psoe","state of emergency","state of emergency", "Raifman et al",TRUE,
    
    # "pindex","ave of four policy vars","the average of stay-at-hom and closures of movie theaters, 
    # reataurants, and non-essential businesses", "Raifman et al",TRUE,
    
    "pindex","business closure policies","the average of closures of movie theaters, 
    reataurants, and non-essential businesses", "Raifman et al",TRUE,
    
    "party","Governor's party","Governor's party", "OPENICPSR",TRUE,
    
    "vote","Trump voting shares","Trump voting shares", "MIT Election Lab",TRUE,
    
    "logvote","log(Trump voting shares)","log(Trump voting shares)", "MIT Election Lab",TRUE,
    
    "z.mask","mask wearing rates","z-score for mask wearing rates", "YouGov",TRUE,
     
    "mask_percent","mask wearing rates","mask wearing rates", "YouGov",TRUE,
    
    "pmaskbus", "masks for employees",
    "mandate face mask use by employees in public facing businesses",
    "Raifman et al",TRUE,

    "pmask.april", "masks*April",
    "",
    "Raifman et al",TRUE,

    "pmask.may", "masks*May",
    "",
    "Raifman et al",TRUE,


    "pmaskall", "masks in public spaces",
    "mandate face mask use by all individuals in public spaces",
    "Raifman et al",TRUE,

    "workplaces", "workplaces",
    "workplaces percent change from baseline",
    "Google",TRUE,

    "residential", "residential",
    "residential percent change from baseline",
    "Google",TRUE,

    "retail", "retail",
    "retail and recreation percent change from baseline",
    "Google",TRUE,

    "grocery", "grocery",
    "grocery and pharmacy percent change from baseline",
    "Google",TRUE,

    "transit", "transit",
    "transit percent change from baseline",
    "Google",TRUE,

    "workplaces_percent_change_from_baseline", "workplaces intensity",
    "workplaces percent change from baseline",
    "Google",  FALSE,

    "residential_percent_change_from_baseline", "residential intensity",
    "residential percent change from baseline",
    "Google",  FALSE,

    "retail_and_recreation_percent_change_from_baseline", "retail intensity",
    "retail and recreation percent change from baseline",
    "Google",  FALSE,

    "grocery_and_pharmacy_percent_change_from_baseline", "grocery intensity",
    "grocery and pharmacy percent change from baseline",
    "Google",  FALSE,

    "transit_stations_percent_change_from_baseline", "transit intensity",
    "transit percent change from baseline",
    "Google",  FALSE,

    "parks_percent_change_from_baseline", "parks intensity",
    "parks percent change from baseline",
    "Google", FALSE,

    "percentchangehours", "% change hours", "","Homebase", FALSE,

    "percentchangebusinesses", "% change businesses", "","Homebase",
    FALSE,

    "distance_traveled_from_home", "distance traveled", "",
    "Safegraph", FALSE,

    "completely_home_device_count", "Completely home count", "", "Safegraph", FALSE,

    "median_home_dwell_time", "Median home dwell time", "", "Safegraph", FALSE,

    "part_time_work_behavior_devices", "Part-time count", "", "Safegraph", FALSE,

    "full_time_work_behavior_devices", "Full-time count", "", "Safegraph", FALSE,

    "portion_completely_home", "Completely home portion", "", "Safegraph", FALSE,

    "portion_full_time_work", "Full-time portion", "", "Safegraph", FALSE,

    "portion_part_time_work", "Part-time portion", "", "Safegraph", FALSE,

    "daily_distance_diff", "Distance change", "", "Unacast", FALSE,

    "daily_visitation_diff", "Visits change", "", "Unacast", FALSE,

    "encounters_rate", "Encounters rate", "", "Unacast", FALSE,

    "log_encounters_rate","log(1+encounters rate)", "", "Unacast", FALSE,

    "zero", "", "", "", FALSE,
    "(0.000)", "", "", "", FALSE
    ), nrow=5)), stringsAsFactors=FALSE )
names(varlabels) <- c("Variable","Label","Description","Source","in.regressions")


#' Returns the variable label for a variable name or array of variable names
getlabel <- function(v) sapply(v, function(x)
  varlabels[varlabels$Variable==x,"Label"])

#' Replace variable names with variable labels in text tbl
relabel <- function(tbl) {
  vars <- varlabels$Variable
  vars <- vars[order(nchar(vars), decreasing=TRUE)]
  for (v in vars) {
    tbl <- gsub(v, getlabel(v), tbl, fixed=TRUE)
  }
  tbl
}
