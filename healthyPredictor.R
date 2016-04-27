# healthyPredictor.R
# Ryan Zembrodt
# Bayesian network tree created from naiveBayes algorithm
# Predicts whether or not a customer will buy a healthy food item.

healthyPredictor <- function(boughtHealthy, dayOfWeek, hour, gender, age, advHealth, advTemp, precipitation) {

  # Probability of boughtHealthy given dayOfWeek, hour, gender, age, advHealth, advTemp, and precipitation.
  # P(boughtHealthy | dayOfWeek,hour,gender,age,advHealth,advTemp,precipitation) = 
  #   (
  #     P(boughtHealthy) * P(dayOfWeek | boughtHealthy) * P(hour | boughtHealthy) * P(gender | boughtHealthy) * P(age | boughtHealthy)
  #      * P(advHealth | boughtHealthy) * P(advTemp | boughtHealthy) * P(precipitation | boughtHealthy)
  #   ) / (
  #     P(dayOfWeek) * P(hour) * P(gender) * P(age) * P(advHealth) * P(advTemp) * P(precipitation)
  #   )
  return((aPrioriTable(boughtHealthy)
         * dayOfWeekCPT(boughtHealthy, dayOfWeek)
         * hourCPT(boughtHealthy, hour)
         * genderCPT(boughtHealthy, gender)
         * ageCPT(boughtHealthy, age)
         * advHealthCPT(boughtHealthy, advHealth)
         * advTempCPT(boughtHealthy, advTemp)
         * precipitationCPT(boughtHealthy, precipitation))
         / (dayOfWeekTable(dayOfWeek) * hourTable(hour) * genderTable(gender) * ageTable(age) * advHealthTable(advHealth) * advTempTable(advTemp) * precipitationTable(precipitation)))
}

# P(boughtHealthy)
aPrioriTable <- function(boughtHealthy) {
  if (boughtHealthy) {
    return(0.7738474)
  } else {
    return(0.2261526)
  }
}

# P(dayOfWeek | boughtHealthy)
dayOfWeekCPT <- function(boughtHealthy, dayOfWeek) {
  if (dayOfWeek == "Monday") {
    if (boughtHealthy) {
      return(0.2058845)
    } else {
      return (0.2475521)
    }
  }
  else if (dayOfWeek == "Tuesday") {
    if (boughtHealthy) {
      return(0.2641426)
    } else {
      return (0.1749937)
    }
  }
  else if (dayOfWeek == "Wednesday") {
    if (boughtHealthy) {
      return(0.2060313)
    } else {
      return (0.2355009)
    }
  }
  else if (dayOfWeek == "Thursday") {
    if (boughtHealthy) {
      return(0.1997212)
    } else {
      return (0.1390911)
    }
  }
  else if (dayOfWeek == "Friday") {
    if (boughtHealthy) {
      return(0.2028622)
    } else {
      return (0.1242204)
    }
  }
}

# P(dayOfWeek)
dayOfWeekTable <- function(dayOfWeek) {
  if (dayOfWeek == "Monday") {
    return(0.2153077)
  }
  else if (dayOfWeek == "Tuesday") {
    return(0.2439814)
  }
  else if (dayOfWeek == "Wednesday") {
    return(0.2126959)
  }
  else if (dayOfWeek == "Thursday") {
    return(0.1860095)
  }
  else if (dayOfWeek == "Friday") {
    return(0.1420055)
  }
}

# P(hour | boughtHealthy)
hourCPT <- function(boughtHealthy, hour) {
  if (hour == 0) {
    if (boughtHealthy) {
      return(0.0002934918)
    } else {
      return(0)
    }
  }
  else if (hour == 1) {
    if (boughtHealthy) {
      return(0.00007337295)
    } else {
      return(0.0002510670)
    }
  }
  else if (hour == 2) {
    if (boughtHealthy) {
      return(0)
    } else {
      return(0)
    }
  }
  else if (hour == 3) {
    if (boughtHealthy) {
      return(0.0003668648)
    } else {
      return(0.0007532011)
    }
  }
  else if (hour == 4) {
    if (boughtHealthy) {
      return(0.00007337295)
    } else {
      return(0)
    }
  }
  else if (hour == 5) {
    if (boughtHealthy) {
      return(0.001027221)
    } else {
      return(0.004519207)
    }
  }
  else if (hour == 6) {
    if (boughtHealthy) {
      return(0.005869836)
    } else {
      return(0.01029375)
    }
  }
  else if (hour == 7) {
    if (boughtHealthy) {
      return(0.07058478)
    } else {
      return(0.09239267)
    }
  }
  else if (hour == 8) {
    if (boughtHealthy) {
      return(0.1346394)
    } else {
      return(0.1699724)
    }
  }
  else if (hour == 9) {
    if (boughtHealthy) {
      return(0.1178370)
    } else {
      return(0.1476274)
    }
  }
  else if (hour == 10) {
    if (boughtHealthy) {
      return(0.06214689)
    } else {
      return(0.06214689)
    }
  }
  else if (hour == 11) {
    if (boughtHealthy) {
      return(0.1989141)
    } else {
      return(0.1599297)
    }
  }
  else if (hour == 12) {
    if (boughtHealthy) {
      return(0.3017096)
    } else {
      return(0.1990962)
    }
  }
  else if (hour == 13) {
    if (boughtHealthy) {
      return(0.05957884)
    } else {
      return(0.07381371)
    }
  }
  else if (hour == 14) {
    if (boughtHealthy) {
      return(0.02333260)
    } else {
      return(0.03188551)
    }
  }
  else if (hour == 15) {
    if (boughtHealthy) {
      return(0.01005209)
    } else {
      return(0.01405975)
    }
  }
  else if (hour == 16) {
    if (boughtHealthy) {
      return(0.004622496)
    } else {
      return(0.005774542)
    }
  }
  else if (hour == 17) {
    if (boughtHealthy) {
      return(0.006016582)
    } else {
      return(0.004268140)
    }
  }
  else if (hour == 18) {
    if (boughtHealthy) {
      return(0.0008804755)
    } else {
      return(0.001255335)
    }
  }
  else if (hour == 19) {
    if (boughtHealthy) {
      return(0.0007337295)
    } else {
      return(0.0007532011)
    }
  }
  else if (hour == 20) {
    if (boughtHealthy) {
      return(0.0002934918)
    } else {
      return(0.001004268)
    }
  }
  else if (hour == 21) {
    if (boughtHealthy) {
      return(0.0002934918)
    } else {
      return(0.0002510670)
    }
  }
  else if (hour == 22) {
    if (boughtHealthy) {
      return(0.001757469)
    } else {
      return(0.0003668648)
    }
  }
  else if (hour == 23) {
    if (boughtHealthy) {
      return(0)
    } else {
      return(0.0002934918)
    }
  }
}

# P(hour)
hourTable <- function(hour) {
  if (hour == 0) {
    return(0.0002271179)
  }
  else if (hour == 1) {
    return(0.0001135589)
  }
  else if (hour == 2) {
    return(0)
  }
  else if (hour == 3) {
    return(0.0004542357)
  }
  else if (hour == 4) {
    return(0.0000567794)
  }
  else if (hour == 5) {
    return(0.001816943)
  }
  else if (hour == 6) {
    return(0.006870316)
  }
  else if (hour == 7) {
    return(0.07551669)
  }
  else if (hour == 8) {
    return(0.14263)
  }
  else if (hour == 9) {
    return(0.1245742)
  }
  else if (hour == 10) {
    return(0.06626164)
  }
  else if (hour == 11) {
    return(0.1900977)
  }
  else if (hour == 12) {
    return(0.2785033)
  }
  else if (hour == 13) {
    return(0.06279809)
  }
  else if (hour == 14) {
    return(0.02526686)
  }
  else if (hour == 15) {
    return(0.01095844)
  }
  else if (hour == 16) {
    return(0.004883034)
  }
  else if (hour == 17) {
    return(0.005621167)
  }
  else if (hour == 18) {
    return(0.000965251)
  }
  else if (hour == 19) {
    return(0.0007381331)
  }
  else if (hour == 20) {
    return(0.0004542357)
  }
  else if (hour == 21) {
    return(0.0002838973)
  }
  else if (hour == 22) {
    return(0.0006813536)
  }
  else if (hour == 23) {
    return(0.0002271179)
  }
}

# P(gender | boughtHealthy)
genderCPT <- function(boughtHealthy, gender) {
  if (gender == "Male") {
    if (boughtHealthy) {
      return(0.5883044)
    } else {
      return (0.6673362)
    }
  }
  else if (gender == "Female") {
    if (boughtHealthy) {
      return(0.4116956)
    } else {
      return (0.3326638)
    }
  }
}

# P(gender)
genderTable <- function(gender) {
  if (gender == "Male") {
    return(0.6061776)
  }
  else if (gender == "Female") {
    return(0.3938224)
  }
}

# P(age | boughtHealthy)
ageCPT <- function(boughtHealthy, age) {
  if (age == "Adult") {
    if (boughtHealthy) {
      return(0.5246166263)
    } else {
      return (0.3540045192)
    }
  }
  else if (age == "Child") {
    if (boughtHealthy) {
      return(0.0562770563)
    } else {
      return (0.0421792619)
    }
  }
  else if (age == "Senior") {
    if (boughtHealthy) {
      return(0.0149680828)
    } else {
      return (0.0007532011)
    }
  }
  else if (age == "Young Adult") {
    if (boughtHealthy) {
      return(0.4041382346)
    } else {
      return (0.6030630178)
    }
  }
}

# P(age)
ageTable <- function(age) {
  if (age == "Adult") {
    return(0.4860323)
  }
  else if (age == "Child") {
    return(0.0530888)
  }
  else if (age == "Senior") {
    return(0.01175335)
  }
  else if (age == "Young Adult") {
    return(0.4491256)
  }
}

# P(advHealth | boughtHealthy)
advHealthCPT <- function(boughtHealthy, advHealth) {
  if (advHealth == "Healthy") {
    if (boughtHealthy) {
      return(0.5880842)
    } else {
      return (0.5169470)
    }
  }
  else if (advHealth == "Unhealthy") {
    if (boughtHealthy) {
      return(0.4119158)
    } else {
      return (0.4830530)
    }
  }
}

# P(advHealth)
advHealthTable <- function(advHealth) {
  if (advHealth == "Healthy") {
    return(0.5719964)
  }
  else if (advHealth == "Unhealthy") {
    return(0.4280036)
  }
}

# P(advTemp | boughtHealthy)
advTempCPT <- function(boughtHealthy, advTemp) {
  if (advTemp == "Cold") {
    if (boughtHealthy) {
      return(0.4057524)
    } else {
      return (0.3745920)
    }
  }
  else if (advTemp == "Hot") {
    if (boughtHealthy) {
      return(0.5942476)
    } else {
      return (0.6254080)
    }
  }
}

# P(advTemp)
advTempTable <- function(advTemp) {
  if (advTemp == "Cold") {
    return(0.3987054)
  }
  else if (advTemp == "Hot") {
    return(0.6012946)
  }
}

# P(precipitation | boughtHealthy)
precipitationCPT <- function(boughtHealthy, precipitation) {
  if (precipitation == "Clear") {
    if (boughtHealthy) {
      return(0.4354684863)
    } else {
      return (0.3951795129)
    }
  }
  else if (precipitation == "Clouds") {
    if (boughtHealthy) {
      return(0.2439650745)
    } else {
      return (0.2801908109)
    }
  }
  else if (precipitation == "Drizzle") {
    if (boughtHealthy) {
      return(0.0004402377)
    } else {
      return (0.0010042681)
    }
  }
  else if (precipitation == "Mist") {
    if (boughtHealthy) {
      return(0.1813779441)
    } else {
      return (0.1757469244)
    }
  }
  else if (precipitation == "Rain") {
    if (boughtHealthy) {
      return(0.0939907550)
    } else {
      return (0.1287973889)
    }
  }
  else if (precipitation == "Snow") {
    if (boughtHealthy) {
      return(0.0447575024)
    } else {
      return (0.0190810947)
    }
  }
}

# P(precipitation)
precipitationTable <- function(precipitation) {
  if (precipitation == "Clear") {
    return(0.426357)
  }
  else if (precipitation == "Clouds") {
    return(0.2521576)
  }
  else if (precipitation == "Drizzle") {
    return(0.0005677947)
  }
  else if (precipitation == "Mist") {
    return(0.1801045)
  }
  else if (precipitation == "Rain") {
    return(0.1018624)
  }
  else if (precipitation == "Snow") {
    return(0.03895072)
  }
}