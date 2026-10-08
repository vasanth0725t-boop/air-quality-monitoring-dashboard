set.seed(2026)

cities <- c(
  "Chennai", "Coimbatore", "Bangalore", "Hyderabad",
  "Mumbai", "Delhi", "Pune", "Kochi"
)

n <- 500L
dates <- as.Date("2025-01-01") + sample(0:364, n, replace = TRUE)
city <- sample(rep(cities, length.out = n))
city_factor <- c(
  Chennai = 1.00, Coimbatore = 0.72, Bangalore = 0.88, Hyderabad = 1.08,
  Mumbai = 1.12, Delhi = 2.05, Pune = 0.92, Kochi = 0.68
)
seasonal_factor <- 1 + 0.32 * cos(2 * pi * (as.integer(format(dates, "%j")) - 15) / 365)
factor <- city_factor[city] * seasonal_factor

temperature_baseline <- c(
  Chennai = 29, Coimbatore = 27, Bangalore = 24, Hyderabad = 28,
  Mumbai = 28, Delhi = 25, Pune = 25, Kochi = 28
)
humidity_baseline <- c(
  Chennai = 69, Coimbatore = 65, Bangalore = 62, Hyderabad = 54,
  Mumbai = 73, Delhi = 54, Pune = 61, Kochi = 78
)

temperature <- pmin(
  41,
  pmax(13, rnorm(n, temperature_baseline[city] + 2 * sin(2 * pi * as.integer(format(dates, "%j")) / 365), 3))
)
humidity <- pmin(98, pmax(22, rnorm(n, humidity_baseline[city], 10)))
PM2_5 <- pmax(4, rlnorm(n, log(19 * factor), 0.38))
PM10 <- pmax(8, PM2_5 * 1.55 + rnorm(n, 8, 8))
NO2 <- pmax(4, rnorm(n, 22 * factor, 7))
SO2 <- pmax(1, rnorm(n, 7 * factor, 3))
CO <- pmax(0.1, rnorm(n, 0.45 * factor, 0.18))
O3 <- pmax(5, rnorm(n, 35 + 7 * sin(2 * pi * as.integer(format(dates, "%j")) / 365), 12))

AQI <- round(pmin(
  300,
  pmax(0, PM2_5 * 1.55 + PM10 * 0.45 + NO2 * 0.18 + SO2 * 0.45 + CO * 8 + O3 * 0.12 + rnorm(n, 0, 6))
))
AQI_Category <- cut(
  AQI,
  breaks = c(-Inf, 50, 100, 150, 200, Inf),
  labels = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy"),
  right = TRUE
)

air_quality <- data.frame(
  Record_ID = sprintf("AQ-%04d", seq_len(n)),
  Date = dates,
  City = city,
  Temperature = round(temperature, 1),
  Humidity = round(humidity, 1),
  PM2_5 = round(PM2_5, 1),
  PM10 = round(PM10, 1),
  NO2 = round(NO2, 1),
  SO2 = round(SO2, 1),
  CO = round(CO, 2),
  O3 = round(O3, 1),
  AQI = AQI,
  AQI_Category = as.character(AQI_Category),
  stringsAsFactors = FALSE
)
air_quality <- air_quality[order(air_quality$Date, air_quality$City), ]

dir.create("data", showWarnings = FALSE)
write.csv(air_quality, file = file.path("data", "air_quality.csv"), row.names = FALSE)
message("Wrote ", nrow(air_quality), " synthetic records to data/air_quality.csv")
