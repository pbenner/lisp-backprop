t <- read.table("features.txt")
t[1:1024] <- scale(t[1:1024])
write.table(t,"features.scaled.txt", quote = FALSE, sep = '\t', row.names = FALSE, col.names = FALSE)
