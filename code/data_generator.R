setwd("~/Desktop/Ecuador_App/data")

file <- read.delim("data_ExampleTMS3_control.csv", sep = ";", header = FALSE)

# 1. Get summary stats for each column
sapply(file[, 4:7], range, na.rm = TRUE)
sapply(file[, 4:7], summary)
sapply(file[, 4:7], sd, na.rm = TRUE)

# 2. Visualize distributions to see what you're working with
par(mfrow = c(2, 2))
for (i in 4:7) {
  hist(file[[i]], main = colnames(file)[i], breaks = 30, col = "steelblue")
}

# 3. Once you know the shape, simulate fake data
# For example, if column 1 looks roughly normal:
n <- nrow(file)

for (i in 1:2) {
fake <- data.frame(
  V1 = file$V1,
  V2 = file$V2,
  V3 = file$V3,
  V4 = sample(file$V4, n, replace = TRUE),
  V5 = sample(file$V5, n, replace = TRUE),
  V6 = sample(file$V6, n, replace = TRUE),
  V7 = sample(file$V7, n, replace = TRUE),
  V8 = file$V8,
  V9 = file$V9
)
write.table(fake, file = paste0("fake", i, ".csv"), quote = FALSE, sep = ";", row.names = FALSE, col.names = FALSE)
}

offset.data <- function(dataframe, offset, name) {
   offset_dataframe <- dataframe %>%
    mutate(V4 = (V4 + offset),
           V5 = (V5 + offset),
           V6 = (V6 + offset),
           V7 = (V7 + offset))
   write.table(offset_dataframe, file = paste0("fake", name, ".csv"), quote = FALSE, sep = ";",  row.names = FALSE, col.names = FALSE)
}

offset.data(file, 5, "mowed")
offset.data(file, -5, "burned")


