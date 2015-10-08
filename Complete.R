# part 2: Function that compiles complete observations

complete = function(directory, id = 1:332) {
  
      filesList = list.files(directory, full.names = TRUE)
      cData = data.frame()
          for(i in id) {
              cData = rbind(cData, nrow(na.omit(read.csv(filesList[i]))))
          }
              cData = data.frame(id, cData)
              names(cData) = c("id", "nobs")
              cData 
}