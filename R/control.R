index_file <- "index.html"
pagecryptr::pagecryptr(file, "r", out_file = "index.html")
contents <- paste(readLines("index.html"), collapse = "\n")
writeLines(gsub('<div id=\"attribution\">\n        Protected by <a href=\"https://www.maxlaumeister.com/pagecrypt/\">PageCrypt</a>\n    </div>', "", contents), "index.html")
for (i in list.dirs('posts')[-1]) {
    posts <- list.files(i)
    for (j in posts) {
        file <- file.path(i, j)
        pagecryptr::pagecryptr(file, "r", out_file = file)
        contents <- paste(readLines(file), collapse = "\n")
        writeLines(gsub('<div id=\"attribution\">\n        Protected by <a href=\"https://www.maxlaumeister.com/pagecrypt/\">PageCrypt</a>\n    </div>', "", contents), file)
    }
    
}
