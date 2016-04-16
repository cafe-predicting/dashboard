# M5P regression model created using RWeka's M5P function and translatted into R code.
predictCustomerAmount <- function(dayOfWeek, minute, temperature, precipitation) {
  
  # Tree for the regression model
  
  if (minute <= 877.5) {
    if (minute <= 367.5) {
      return(lm1(dayOfWeek, minute, temperature, precipitation))
    } else {
      if (minute <= 787.5) {
        if (minute <= 682.5) {
          if (minute <= 442.5) {
            if (minute <= 412.5) {
              return(lm2(dayOfWeek, minute, temperature, precipitation))
            } else {
              if (minute <= 427.5) {
                return(lm3(dayOfWeek, minute, temperature, precipitation))
              } else {
                return(lm4(dayOfWeek, minute, temperature, precipitation))
              }
            }
          } else {
            if (minute <= 592.5) {
              if (temperature <= 33.728) {
                if (dayOfWeek == "SATURDAY" || dayOfWeek == "SUNDAY") {
                  return(lm5(dayOfWeek, minute, temperature, precipitation))
                } else {
                  if (minute <= 487.5) {
                    return(lm6(dayOfWeek, minute, temperature, precipitation))
                  } else {
                    if (minute <= 532.5) {
                      return(lm7(dayOfWeek, minute, temperature, precipitation))
                    } else {
                      return(lm8(dayOfWeek, minute, temperature, precipitation))
                    }
                  }
                }
              } else {
                if (minute <= 562.5) {
                  return(lm9(dayOfWeek, minute, temperature, precipitation))
                } else {
                  return(lm10(dayOfWeek, minute, temperature, precipitation))
                }
              }
            } else {
              if (minute <= 652.5) {
                if (minute <= 622.5) {
                  return(lm11(dayOfWeek, minute, temperature, precipitation))
                } else {
                  return(lm12(dayOfWeek, minute, temperature, precipitation))
                }
              } else {
                if (minute <= 667.5) {
                  return(lm13(dayOfWeek, minute, temperature, precipitation))
                } else {
                  if (dayOfWeek == "THURSDAY" || dayOfWeek == "SATURDAY" || dayOfWeek == "SUNDAY") {
                    if (temperature <= 35.537) {
                      return(lm14(dayOfWeek, minute, temperature, precipitation))
                    } else {
                      return(lm15(dayOfWeek, minute, temperature, precipitation))
                    }
                  } else {
                    if (temperature <= 37.976) {
                      return(lm16(dayOfWeek, minute, temperature, precipitation))
                    } else {
                      return(lm17(dayOfWeek, minute, temperature, precipitation))
                    }
                  }
                }
              }
            }
          }
        } else {
          if (minute <= 757.5) {
            return(lm18(dayOfWeek, minute, temperature, precipitation))
          } else {
          return(lm19(dayOfWeek, minute, temperature, precipitation))
          }
        }
      } else {
        return(lm20(dayOfWeek, minute, temperature, precipitation))
      }
    }
  } else {
    return(lm21(dayOfWeek, minute, temperature, precipitation))
  }
}

# Define day of week constants
monday <- "MONDAY"
tuesday <- "TUESDAY"
wednesday <- "WEDNESDAY"
thursday <- "THURSDAY"
friday <- "FRIDAY"
saturday <- "SATURDAY"
sunday <- "SUNDAY"

# Define precipitation constants
clear <- "Clear"
clouds <- "Clouds"
drizzle <- "Drizzle"
fog <- "Fog"
rain <- "Rain"
mist <- "Mist"
snow <- "Snow"


# All linear functions for the model tree.

# Linear functions are defined as a constant multiplied by one of the independent variables and then summed up.
# For the independent variables that are factors, and not numeric, the constant is multiplied against a boolean expression so that if
#   the expression is false, the constant multiplies by 0, and if true it is multiplied by 1.
lm1 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    0.001 * minute 
    - 0.0066 * temperature 
    + 0.1977 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.2033 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 0.5148 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1151 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.2876
  )
}

lm2 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    0.041 * minute 
    - 0.008 * temperature 
    + 0.2353 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.257 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 5.7747 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    - 16.7541
  )
}

lm3 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    0.2223 * minute 
    - 0.008 * temperature 
    + 0.2353 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.257 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 2.2589 * (dayOfWeek == saturday || dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 5.9695 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    - 94.0477
  )
}

lm4 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    0.2423 * minute 
    - 0.008 * temperature 
    + 0.2353 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.257 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 6.0319 * (dayOfWeek == saturday || dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 5.2157 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.9235 * (dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    + 1.1354 * (dayOfWeek == friday)
    - 102.6086
  )
}

lm5 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    -0.003 * minute 
    - 0.0857 * temperature 
    + 0.2353 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.257 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 11.2191 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    + 4.1937
  )
}

lm6 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    -0.003 * minute 
    - 0.1392 * temperature 
    + 0.2353 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.257 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 10.3636 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    + 11.1423
  )
}

lm7 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    -0.0013 * minute 
    - 0.2286 * temperature 
    + 0.2353 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.257 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 10.3636 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    + 14.2364
  )
}

lm8 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    -0.0176 * minute 
    - 0.1668 * temperature 
    + 0.2353 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.257 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 10.3636 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    + 20.3591
  )
}

lm9 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    0.0248 * minute 
    - 0.008 * temperature 
    + 0.2353 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.257 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 6.6787 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    + 1.3997
  )
  
}

lm10 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    -0.0108 * minute 
    - 0.008 * temperature 
    + 0.2353 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.257 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 6.6787 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    + 16.7788
  )
}

lm11 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    -0.0372 * minute 
    - 0.008 * temperature 
    + 0.2353 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.257 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 10.937 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    + 23.2608
  )
}

lm12 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    -0.0819 * minute 
    - 0.008 * temperature 
    + 0.2353 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.257 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 7.7363 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    + 52.0061
  )
}

lm13 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    0.1897 * minute 
    - 0.008 * temperature 
    + 0.2353 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 1.1339 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 9.6794 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.8675 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    - 126.7732
  )
}

lm14 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    0.1776 * minute 
    - 0.008 * temperature 
    + 0.2353 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 1.0791 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 8.7993 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 2.3618 * (dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    - 116.9068
  )
}

lm15 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    0.1776 * minute 
    - 0.008 * temperature 
    + 0.2353 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 1.0791 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 8.7993 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 2.3618 * (dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    - 116.7471
  )
}

lm16 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    0.1776 * minute 
    - 0.0317 * temperature 
    + 0.2353 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 1.7228 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 8.7993 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 1.7495 * (dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    - 114.4619
  )
}

lm17 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    0.1776 * minute 
    - 0.0258 * temperature 
    + 0.4239 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 1.8414 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 8.7993 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 1.7495 * (dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    - 115.0859
  )
}

lm18 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    -0.0314 * minute 
    - 0.1849 * temperature 
    + 1.4629 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 1.0406 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.257 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 35.5686 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    - 6.5521 * (dayOfWeek == friday)
    + 29.7939
  )
}

lm19 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    -0.4318 * minute 
    - 0.008 * temperature 
    + 7.0522 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 4.2915 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.257 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 17.3502 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1966 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.085 * (dayOfWeek == monday || dayOfWeek == friday)
    - 1.9118 * (dayOfWeek == friday)
    + 332.7769
  )
}

lm20 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    -0.0299 * minute 
    - 0.0169 * temperature 
    + 0.4359 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0578 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.5529 * (precipitation == rain || precipitation == mist)
    + 0.086 * (precipitation == mist)
    + 2.1091 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 2.2708 * (dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.4813 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    - 0.2952 * (dayOfWeek == monday || dayOfWeek == friday)
    + 26.0812
  )
}

lm21 <- function(dayOfWeek, minute, temperature, precipitation) {
  return(
    0 * minute 
    - 0.0014 * temperature 
    + 0.1507 * (precipitation == fog || precipitation == snow || precipitation == clouds || precipitation == rain || precipitation == mist)
    - 0.0995 * (precipitation == clouds || precipitation == rain || precipitation == mist)
    + 0.0626 * (precipitation == rain || precipitation == mist)
    + 0.1482 * (precipitation == mist)
    + 0.1857 * (dayOfWeek == thursday || dayOfWeek == tuesday || dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.052 * (dayOfWeek == wednesday || dayOfWeek == monday || dayOfWeek == friday)
    + 0.1546
  )
}