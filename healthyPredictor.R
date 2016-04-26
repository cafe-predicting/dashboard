# healthyPredictor.R
# Ryan Zembrodt
# Bayesian network tree created from naiveBayes algorithm
# Predicts whether or not a customer will buy a healthy food item.

healthyPredictor <- function(dayOfWeek, hour, gender, age, advHealth, advTemp, precipitation) {
  boughtHealthyCount <- 0
  iterations <- 10000
  
  boughtHealthyDOW <- FALSE
  boughtHealthyGender <- FALSE
  boughtHealthyAge <- FALSE
  boughtHealthyAH <- FALSE
  boughtHealthyAT <- FALSE
  boughtHealthyPrec <- FALSE
  
  for (i in 1:iterations) {
    
    
    boughtHealthyDOW <- biasRand(dayOfWeekCPT(boughtHealthyDOW, dayOfWeek))
    boughtHealthyGender <- biasRand(genderCPT(boughtHealthyGender, gender))
    boughtHealthyAge <- biasRand(ageCPT(boughtHealthyAge, age))
    boughtHealthyAH <- biasRand(advHealthCPT(boughtHealthyAH, advHealth))
    boughtHealthyAT <- biasRand(advTempCPT(boughtHealthyAT, advTemp))
    boughtHealthyPrec <- biasRand(precipitationCPT(boughtHealthyPrec, precipitation))
    
    if (boughtHealthyDOW
        || boughtHealthyGender
        || boughtHealthyAge
        || boughtHealthyAH
        || boughtHealthyAT
        || boughtHealthyPrec) {
      boughtHealthyCount <- boughtHealthyCount + 1
    }
    #if (boughtHealthyDOW) {
    #  boughtHealthyCount <- boughtHealthyCount + 1
    #}
    #if (boughtHealthyGender) {
    #  boughtHealthyCount <- boughtHealthyCount + 1
    #}
    #if (boughtHealthyAge) {
    #  boughtHealthyCount <- boughtHealthyCount + 1
    #}
    #if (boughtHealthyAH) {
    #  boughtHealthyCount <- boughtHealthyCount + 1
    #}
    #if (boughtHealthyAT) {
    #  boughtHealthyCount <- boughtHealthyCount + 1
    #}
    #if (boughtHealthyPrec) {
    #  boughtHealthyCount <- boughtHealthyCount + 1
    #}
  }
  
  return(boughtHealthyCount / (iterations * 6))
}

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

#hourCPT <- function(boughtHealthy, hour) {
#}

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

biasRand <- function(bias) {
  return(runif(1) <= bias)
}