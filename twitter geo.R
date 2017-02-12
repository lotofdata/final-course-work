


timezone = test2[["timezone"]]

timezone= gsub("\\s*\\([^\\)]+\\)","", timezone)

timezone = gsub("Central Time", "USA", timezone)
timezone = gsub("Eastern Time", "USA", timezone)
timezone = gsub("Pacific Time", "USA", timezone)
timezone = gsub("Mountain Time", "USA", timezone)
timezone = gsub("Atlantic Time", "Canada", timezone)
timezone = gsub("JST", "Japan", timezone)
timezone = gsub("Athens", "Greece", timezone)
timezone = gsub("Central America", "Honduras", timezone)
timezone = gsub("Central Time", "USA", timezone)
timezone = gsub("Central Time", "USA", timezone)
timezone = gsub("Central Time", "USA", timezone)

geo <- data.frame(timezone)

timezone










library(DataCombine)

# Create original data
ABData <- data.frame(a = c("Eastern Time (US & Canada)", "Pacific Time", "Central Time (US & Canada)", "Hamburg, DE", "Oslo, NO"),
                     b = c(8, 0.1, 3, 2, 1))

# Create replacements data frame
Replaces <- data.frame(from = c("Pacific Time",
                                "Eastern Time US Canada",
                                "Central Time (US & Canada)",
                                "Atlantic Time (Canada)",
                                "Mountain Time (US & Canada)"),
                       to = c("USA","USA","USA","Canada","USA"))

# Replace patterns and return full data frame
AB <- FindReplace(data = ABData, Var = "a", replaceData = Replaces,
                       from = "from", to = "to", exact = FALSE)

# Replace patterns and return the Var as a vector
ABNewVector <- FindReplace(data = f, Var = "timezone", replaceData = Replaces,
                           from = "from", to = "to", vector = TRUE)


f = data.frame(timezone)




decode <- function(x, search, replace, default = NULL) {
  
  # build a nested ifelse function by recursion
  decode.fun <- function(search, replace, default = NULL)
    if (length(search) == 0L) {
      function(x) if (is.null(default)) x else rep(default, length(x))
    } else {
      function(x) ifelse(x == search[1L], replace[1L],
                         decode.fun(tail(search,  -1L),
                                    tail(replace, -1L),
                                    default)(x))
    }
  
  return(decode.fun(search, replace, default)(x))
}




g = decode(f, search  =c("Pacific Time", "Eastern Time (US & Canada)", "Central Time (US & Canada)","Atlantic Time (Canada)","Mountain Time (US & Canada)"),
       
       replace = c("USA", "USA", "USA", "Canada", "USA"))



decode(basket, search  = c("banana", "orange"),
       replace = c("apple", "pineapple"))
# [1] "apple"   "apple"   "lemon"   "pineapple"   "pineapple"   "pear"   "cherry"   

decode(basket, search  = c("banana", "orange"),
       replace = c("apple", "pineapple"),
       default = "fig")
# [1] "fig"   "apple"   "fig"   "pineapple"   "pineapple"   "fig"   "fig"  










































geo = ()
  "International Date Line West" => "Pacific/Midway",
  "Midway Island"                => "Pacific/Midway",
  "American Samoa"               => "Pacific/Pago_Pago",
  "Hawaii"                       => "Pacific/Honolulu",
  "Alaska"                       => "America/Juneau",
  "Pacific Time (US & Canada)"   => "America/Los_Angeles",
  "Tijuana"                      => "America/Tijuana",
  "Mountain Time (US & Canada)"  => "America/Denver",
  "Arizona"                      => "America/Phoenix",
  "Chihuahua"                    => "America/Chihuahua",
  "Mazatlan"                     => "America/Mazatlan",
  "Central Time (US & Canada)"   => "America/Chicago",
  "Saskatchewan"                 => "America/Regina",
  "Guadalajara"                  => "America/Mexico_City",
  "Mexico City"                  => "America/Mexico_City",
  "Monterrey"                    => "America/Monterrey",
  "Central America"              => "America/Guatemala",
  "Eastern Time (US & Canada)"   => "America/New_York",
  "Indiana (East)"               => "America/Indiana/Indianapolis",
  "Bogota"                       => "America/Bogota",
  "Lima"                         => "America/Lima",
  "Quito"                        => "America/Lima",
  "Atlantic Time (Canada)"       => "America/Halifax",
  "Caracas"                      => "America/Caracas",
  "La Paz"                       => "America/La_Paz",
  "Santiago"                     => "America/Santiago",
  "Newfoundland"                 => "America/St_Johns",
  "Brasilia"                     => "America/Sao_Paulo",
  "Buenos Aires"                 => "America/Argentina/Buenos_Aires",
  "Montevideo"                   => "America/Montevideo",
  "Georgetown"                   => "America/Guyana",
  "Greenland"                    => "America/Godthab",
  "Mid-Atlantic"                 => "Atlantic/South_Georgia",
  "Azores"                       => "Atlantic/Azores",
  "Cape Verde Is."               => "Atlantic/Cape_Verde",
  "Dublin"                       => "Europe/Dublin",
  "Edinburgh"                    => "Europe/London",
  "Lisbon"                       => "Europe/Lisbon",
  "London"                       => "Europe/London",
  "Casablanca"                   => "Africa/Casablanca",
  "Monrovia"                     => "Africa/Monrovia",
  "UTC"                          => "Etc/UTC",
  "Belgrade"                     => "Europe/Belgrade",
  "Bratislava"                   => "Europe/Bratislava",
  "Budapest"                     => "Europe/Budapest",
  "Ljubljana"                    => "Europe/Ljubljana",
  "Prague"                       => "Europe/Prague",
  "Sarajevo"                     => "Europe/Sarajevo",
  "Skopje"                       => "Europe/Skopje",
  "Warsaw"                       => "Europe/Warsaw",
  "Zagreb"                       => "Europe/Zagreb",
  "Brussels"                     => "Europe/Brussels",
  "Copenhagen"                   => "Europe/Copenhagen",
  "Madrid"                       => "Europe/Madrid",
  "Paris"                        => "Europe/Paris",
  "Amsterdam"                    => "Europe/Amsterdam",
  "Berlin"                       => "Europe/Berlin",
  "Bern"                         => "Europe/Berlin",
  "Rome"                         => "Europe/Rome",
  "Stockholm"                    => "Europe/Stockholm",
  "Vienna"                       => "Europe/Vienna",
  "West Central Africa"          => "Africa/Algiers",
  "Bucharest"                    => "Europe/Bucharest",
  "Cairo"                        => "Africa/Cairo",
  "Helsinki"                     => "Europe/Helsinki",
  "Kyiv"                         => "Europe/Kiev",
  "Riga"                         => "Europe/Riga",
  "Sofia"                        => "Europe/Sofia",
  "Tallinn"                      => "Europe/Tallinn",
  "Vilnius"                      => "Europe/Vilnius",
  "Athens"                       => "Europe/Athens",
  "Istanbul"                     => "Europe/Istanbul",
  "Minsk"                        => "Europe/Minsk",
  "Jerusalem"                    => "Asia/Jerusalem",
  "Harare"                       => "Africa/Harare",
  "Pretoria"                     => "Africa/Johannesburg",
  "Kaliningrad"                  => "Europe/Kaliningrad",
  "Moscow"                       => "Europe/Moscow",
  "St. Petersburg"               => "Europe/Moscow",
  "Volgograd"                    => "Europe/Volgograd",
  "Samara"                       => "Europe/Samara",
  "Kuwait"                       => "Asia/Kuwait",
  "Riyadh"                       => "Asia/Riyadh",
  "Nairobi"                      => "Africa/Nairobi",
  "Baghdad"                      => "Asia/Baghdad",
  "Tehran"                       => "Asia/Tehran",
  "Abu Dhabi"                    => "Asia/Muscat",
  "Muscat"                       => "Asia/Muscat",
  "Baku"                         => "Asia/Baku",
  "Tbilisi"                      => "Asia/Tbilisi",
  "Yerevan"                      => "Asia/Yerevan",
  "Kabul"                        => "Asia/Kabul",
  "Ekaterinburg"                 => "Asia/Yekaterinburg",
  "Islamabad"                    => "Asia/Karachi",
  "Karachi"                      => "Asia/Karachi",
  "Tashkent"                     => "Asia/Tashkent",
  "Chennai"                      => "Asia/Kolkata",
  "Kolkata"                      => "Asia/Kolkata",
  "Mumbai"                       => "Asia/Kolkata",
  "New Delhi"                    => "Asia/Kolkata",
  "Kathmandu"                    => "Asia/Kathmandu",
  "Astana"                       => "Asia/Dhaka",
  "Dhaka"                        => "Asia/Dhaka",
  "Sri Jayawardenepura"          => "Asia/Colombo",
  "Almaty"                       => "Asia/Almaty",
  "Novosibirsk"                  => "Asia/Novosibirsk",
  "Rangoon"                      => "Asia/Rangoon",
  "Bangkok"                      => "Asia/Bangkok",
  "Hanoi"                        => "Asia/Bangkok",
  "Jakarta"                      => "Asia/Jakarta",
  "Krasnoyarsk"                  => "Asia/Krasnoyarsk",
  "Beijing"                      => "Asia/Shanghai",
  "Chongqing"                    => "Asia/Chongqing",
  "Hong Kong"                    => "Asia/Hong_Kong",
  "Urumqi"                       => "Asia/Urumqi",
  "Kuala Lumpur"                 => "Asia/Kuala_Lumpur",
  "Singapore"                    => "Asia/Singapore",
  "Taipei"                       => "Asia/Taipei",
  "Perth"                        => "Australia/Perth",
  "Irkutsk"                      => "Asia/Irkutsk",
  "Ulaanbaatar"                  => "Asia/Ulaanbaatar",
  "Seoul"                        => "Asia/Seoul",
  "Osaka"                        => "Asia/Tokyo",
  "Sapporo"                      => "Asia/Tokyo",
  "Tokyo"                        => "Asia/Tokyo",
  "Yakutsk"                      => "Asia/Yakutsk",
  "Darwin"                       => "Australia/Darwin",
  "Adelaide"                     => "Australia/Adelaide",
  "Canberra"                     => "Australia/Melbourne",
  "Melbourne"                    => "Australia/Melbourne",
  "Sydney"                       => "Australia/Sydney",
  "Brisbane"                     => "Australia/Brisbane",
  "Hobart"                       => "Australia/Hobart",
  "Vladivostok"                  => "Asia/Vladivostok",
  "Guam"                         => "Pacific/Guam",
  "Port Moresby"                 => "Pacific/Port_Moresby",
  "Magadan"                      => "Asia/Magadan",
  "Srednekolymsk"                => "Asia/Srednekolymsk",
  "Solomon Is."                  => "Pacific/Guadalcanal",
  "New Caledonia"                => "Pacific/Noumea",
  "Fiji"                         => "Pacific/Fiji",
  "Kamchatka"                    => "Asia/Kamchatka",
  "Marshall Is."                 => "Pacific/Majuro",
  "Auckland"                     => "Pacific/Auckland",
  "Wellington"                   => "Pacific/Auckland",
  "Nuku'alofa"                   => "Pacific/Tongatapu",
  "Tokelau Is."                  => "Pacific/Fakaofo",
  "Chatham Is."                  => "Pacific/Chatham",
  "Samoa"                        => "Pacific/Apia"
)