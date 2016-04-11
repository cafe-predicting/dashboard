# M5P regression model created using RWeka's M5P function and translatted into R code.
predictCustomerAmount <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  
  # Tree for the regression model
  
  if (minute <= 907.5) {
    if (minute <= 367.5) {
      return(lm1(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
    } else {
      if (minute <= 787.5) {
        if (minute <= 682.5) {
          if (minute <= 442.5) {
            if (minute <= 412.5) {
              return(lm2(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
            } else {
              if (minute <= 427.5) {
                return(lm3(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
              } else {
                return(lm4(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
              }
            }
          } else {
            if (minute <= 592.5) {
              if (temperature <= 32.855) {
                if (dayOfWeek == "SATURDAY" || dayOfWeek == "SUNDAY") {
                  return(lm5(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
                } else {
                  if (minute <= 487.5) {
                    return(lm6(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
                  } else {
                    if (minute <= 532.5) {
                      return(lm7(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
                    } else {
                      return(lm8(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
                    }
                  }
                }
              } else {
                if (minute <= 562.5) {
                  return(lm9(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
                } else {
                  return(lm10(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
                }
              }
            } else {
              if (minute <= 652.5) {
                if (minute <= 622.5) {
                  return(lm11(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
                } else {
                  return(lm12(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
                }
              } else {
                return(lm13(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
              }
            }
          }
        } else {
          if (minute <= 757.5) {
            if (dayOfWeek == "THURSDAY" || dayOfWeek == "SATURDAY" || dayOfWeek == "SUNDAY") {
              return(lm14(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
            } else {
              if (dayOfWeek != "FRIDAY") {
                if (minute <= 727.5) {
                  if (minute <= 697.5) {
                    return(lm15(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
                  } else {
                    return(lm16(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
                  }
                } else {
                  return(lm17(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
                }
              } else {
                return(lm18(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
              }
            }
          } else {
            return(lm19(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
          }
        }
      } else {
        return(lm20(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
      }
    }
  } else {
    return(lm21(dayOfMonth, dayOfWeek, minute, temperature, precipitation))
  }
}

# All linear functions for the model tree.

lm1 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 0.5092, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.1236, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2037, 0)
  
  return(dayOfWeekConst + (0.0007*minute) - (0.0043*temperature) + precipitationConst1 + precipitationConst2 - 0.2288)
}

lm2 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SUNDAY", 5.7682, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek == "MONDAY" || dayOfWeek == "TUESDAY" || dayOfWeek == "FRIDAY", 0.5069, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0851, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst1 - dayOfWeekConst2 + (0.0403*minute) - (0.0025*temperature) + precipitationConst1 + precipitationConst2 - 16.5062)
}

lm3 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SUNDAY", 2.3848, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 5.9363, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0851, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst1 + dayOfWeekConst2 + (0.2297*minute) - (0.0025*temperature) + precipitationConst1 + precipitationConst2 - 97.2641)
}

lm4 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SUNDAY", 6.1373, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 4.9726, 0)
  dayOfWeekConst3 <- ifelse(dayOfWeek == "FRIDAY", 0.7104, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0851, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst1 + dayOfWeekConst2 + dayOfWeekConst3 + (0.2644*minute) - (0.0025*temperature) + precipitationConst1 + precipitationConst2 - 112.0913)
}

lm5 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 11.7935, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek == "MONDAY" || dayOfWeek == "FRIDAY", 0.448, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0851, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst1 - dayOfWeekConst2 - (0.003*minute) - (0.0025*temperature) + precipitationConst1 + precipitationConst2 + 1.7007)
}

lm6 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 11.3821, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek == "MONDAY" || dayOfWeek == "FRIDAY", 0.448, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0851, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return((0.1515*dayOfMonth) + dayOfWeekConst1 - dayOfWeekConst2 - (0.003*minute) - (0.0025*temperature) + precipitationConst1 + precipitationConst2 + 6.6113)
}

lm7 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 11.3821, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek == "MONDAY" || dayOfWeek == "FRIDAY", 0.448, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0851, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst1 - dayOfWeekConst2 - (0.0136*minute) - (0.0025*temperature) + precipitationConst1 + precipitationConst2 + 17.126)
}

lm8 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 11.3821, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek == "MONDAY" || dayOfWeek == "FRIDAY", 0.448, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0851, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst1 - dayOfWeekConst2 - (0.0325*minute) - (0.0025*temperature) + precipitationConst1 + precipitationConst2 + 25.3004)
}

lm9 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 6.3587, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek == "MONDAY" || dayOfWeek == "FRIDAY", 5.7027, 0)
  dayOfWeekConst3 <- ifelse(dayOfWeek == "FRIDAY", 4.7094, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0851, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst1 - dayOfWeekConst2 + dayOfWeekConst3 + (0.0574*minute) - (0.0025*temperature) + precipitationConst1 + precipitationConst2 - 13.5431)
}

lm10 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 6.3587, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek == "MONDAY" || dayOfWeek == "FRIDAY", 1.5701, 0)
  dayOfWeekConst3 <- ifelse(dayOfWeek == "FRIDAY", 0.9091, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0851, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return((0.1866*dayOfMonth) + dayOfWeekConst1 - dayOfWeekConst2 + dayOfWeekConst3 - (0.003*minute) - (0.0025*temperature) + precipitationConst1 + precipitationConst2 + 11.1758)
}

lm11 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 10.7534, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0851, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst - (0.0274*minute) - (0.0025*temperature) + precipitationConst1 + precipitationConst2 + 16.8867)
}

lm12 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 7.6669, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek != "THURSDAY" && dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 0.6188, 0)
  dayOfWeekConst3 <- ifelse(dayOfWeek == "MONDAY" || dayOfWeek == "TUESDAY" || dayOfWeek == "FRIDAY", 0.5924, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0851, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst1 + dayOfWeekConst2 - dayOfWeekConst3 - (0.0252*minute) - (0.0025*temperature) + precipitationConst1 + precipitationConst2 + 15.4688)
}

lm13 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 5.9809, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek != "THURSDAY" && dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 2.5171, 0)
  dayOfWeekConst3 <- ifelse(dayOfWeek == "FRIDAY", 1.8395, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0851, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst1 + dayOfWeekConst2 - dayOfWeekConst3 + (0.4427*minute) - (0.0025*temperature) + precipitationConst1 + precipitationConst2 - 291.4329)
}

lm14 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 35.9277, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek == "FRIDAY", 3.0336, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0851, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst1 - dayOfWeekConst2 - (0.0254*minute) - (0.0762*temperature) + precipitationConst1 + precipitationConst2 + 22.0164)
}

lm15 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 13.5541, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek == "MONDAY" || dayOfWeek == "FRIDAY", 1.0055, 0)
  dayOfWeekConst3 <- ifelse(dayOfWeek == "FRIDAY", 3.5666, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 2.9616, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst1 - dayOfWeekConst2 - dayOfWeekConst3 + (0.0468*minute) - (0.2313*temperature) + precipitationConst1 + precipitationConst2 + 0.9412)
}

lm16 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 13.5541, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek == "MONDAY" || dayOfWeek == "FRIDAY", 1.0055, 0)
  dayOfWeekConst3 <- ifelse(dayOfWeek == "FRIDAY", 3.5666, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 2.9616, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst1 - dayOfWeekConst2 - dayOfWeekConst3 + (0.01*minute) - (0.2273*temperature) + precipitationConst1 + precipitationConst2 + 29.0306)
}

lm17 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 13.5541, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek == "MONDAY" || dayOfWeek == "FRIDAY", 1.0055, 0)
  dayOfWeekConst3 <- ifelse(dayOfWeek == "FRIDAY", 3.5666, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 3.2782, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst1 - dayOfWeekConst2 - dayOfWeekConst3 - (0.0784*minute) - (0.2427*temperature) + precipitationConst1 + precipitationConst2 + 87.7833)
}

lm18 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 13.5541, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek == "MONDAY" || dayOfWeek == "FRIDAY", 1.6758, 0)
  dayOfWeekConst3 <- ifelse(dayOfWeek == "FRIDAY", 4.521, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 2.8349, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst1 - dayOfWeekConst2 - dayOfWeekConst3 - (0.0534*minute) - (0.1342*temperature) + precipitationConst1 + precipitationConst2 + 64.3095)
}

lm19 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 20.0823, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek == "FRIDAY", 2.0371, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0851, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.2597, 0)
  
  return(dayOfWeekConst1 - dayOfWeekConst2 - (0.4477*minute) - (0.0527*temperature) + precipitationConst1 + precipitationConst2 + 347.5983)
}

lm20 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst1 <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 3.4198, 0)
  dayOfWeekConst2 <- ifelse(dayOfWeek == "MONDAY" || dayOfWeek == "FRIDAY", 1.3201, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0851, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.4297, 0)
  
  return(dayOfWeekConst1 + dayOfWeekConst2 - (0.0236*minute) - (0.0025*temperature) + precipitationConst1 + precipitationConst2 + 20.0143)
}

lm21 <- function(dayOfMonth, dayOfWeek, minute, temperature, precipitation) {
  dayOfWeekConst <- ifelse(dayOfWeek != "SATURDAY" && dayOfWeek != "SUNDAY", 0.1937, 0)
  precipitationConst1 <- ifelse(precipitation == "Clouds" || precipitation == "Rain" || precipitation == "Snow" || precipitation == "Mist", 0.0559, 0)
  precipitationConst2 <- ifelse(precipitation == "Snow" || precipitation == "Mist", 0.1491, 0)
  
  return(dayOfWeekConst - (0.0001*minute) + precipitationConst1 + precipitationConst2 + 0.0396)
}