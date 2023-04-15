index_file <- "_site/index.html"
pagecryptr::pagecryptr(file, "r", out_file = "_site/index.html")
contents <- paste(readLines("_site/index.html"), collapse = "\n")
writeLines(gsub('<div id=\"attribution\">\n        Protected by <a href=\"https://www.maxlaumeister.com/pagecrypt/\">PageCrypt</a>\n    </div>', "", contents), "_site/index.html")
for (i in list.dirs('_site/posts')[-1]) {
    posts <- list.files(i)
    for (j in posts) {
        file <- file.path(i, j)
        pagecryptr::pagecryptr(file, "r", out_file = file)
        contents <- paste(readLines(file), collapse = "\n")
        writeLines(gsub('<div id=\"attribution\">\n        Protected by <a href=\"https://www.maxlaumeister.com/pagecrypt/\">PageCrypt</a>\n    </div>', "", contents), file)
    }
    
}
