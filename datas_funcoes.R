converterData <- function(data){
  
      dia  <- stringr::regex("\\d{2}/")
      mes <- stringr::regex("/\\d{2}/")
      ano<- stringr::regex("/\\d{4}")
      
      datas <- cbind(stringr::str_extract(data,dia),
                     stringr::str_extract(data,mes),
                     stringr::str_extract(data,ano))
      
      datas <- cbind(stringr::str_remove(datas[,1],"/"),
                     stringr::str_remove_all(datas[,2],"/"),
                     stringr::str_remove(datas[,3],"/"))
      data <- c()
      
      linhas <- nrow(datas)
      
      for(i in 1:linhas){
        data[i] <- stringr::str_glue(datas[i,3],"-",datas[i,2],"-",datas[i,1])
      }
      
      return(as.Date(data))
}

data_ts_anual <- function(data) {
  ano <- lubridate::year(data)
  numero <- as.numeric(data - (lubridate::floor_date(data, "year") - 1))
  c(ano, numero)
}
