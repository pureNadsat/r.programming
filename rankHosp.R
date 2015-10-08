# function that returns the rank (best or worst) of state hospitals according
# to moratlity rates of treated medical conditions (heart attack, heart failure, and pneumonia)
rankall = function(outcome, num = "best") {
    # create and trim dataset for relevant variables
    data = read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data = data[c(2, 7, 11, 17, 23)]
    names(data)[1] = "name"
    names(data)[2] = "state"
    names(data)[3] = "heart attack"
    names(data)[4] = "heart failure"
    names(data)[5] = "pneumonia"

    outcomes = c("heart attack", "heart failure", "pneumonia")
    if(outcome %in% outcomes == FALSE) stop("invalid outcome")
    if(num != "best" && num != "worst" && num%%1 != 0) stop("invalid num")

    data = data[data[outcome] != 'Not Available', ]
    
    data[outcome] = as.data.frame(sapply(data[outcome], as.numeric))
    data = data[order(data$name, decreasing = FALSE), ]
    data = data[order(data[outcome], decreasing = FALSE), ]

    hospRank = function(df, s, n) {
        df = df[df$state == s, ]
        vals = df[, outcome]
        if(n == "best") {
           rowNum = which.min(vals)
        } else if(n == "worst") {
            rowNum = which.max(vals)
        } else {
            rowNum = n
        }
        df[rowNum, ]$name
    }

    # for each state, find the hospital of the given rank
    states = data[, 2]
    states = unique(states)
    newData = data.frame("hospital" = character(), "state" = character())
    for(st in states) {
        hosp = getHospByRank(data, st, num)
        newData = rbind(newdata, data.frame(hospital = hosp, state = st))
    }

    # return a data frame with the hospital names and the (abbreviated) state name
    newData = newData[order(newData['state'], decreasing = FALSE), ]
    newData
}
