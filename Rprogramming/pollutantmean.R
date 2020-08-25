function (directory, pollutant, id = 1:332) 
{
    if (length(id) == 1) {
        file <- file.path(directory, sprintf("%03d.csv", id))
        result <- read.csv(file)
        print(result)
    }
    else {
        files <- list()
        for (i in id) {
            files[i] <- file.path(directory, sprintf("%03d.csv", 
                i))
        }
        csv <- lapply(files, read.csv)
        result <- do.call(rbind, csv)
    }
    print(dim(result))
    lapply(result[result$ID %in% id, ][pollutant], mean, na.rm = TRUE)
}
