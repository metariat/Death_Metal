library(data.table)
library(plotly)

#####################################################################
#                      Data preparation                             #
#####################################################################
setwd("C:/documents/xq.do/Desktop/Kaggle/Death_Metal/")

albums = fread("./data/albums.csv", stringsAsFactors = T)
bands = fread("./data/bands.csv", stringsAsFactors = T)
reviews = fread("./data/reviews.csv", stringsAsFactors = T)

print(paste0("There are ", dim(bands)[1], " death metal bands in the world"))
print(paste0("There are ", dim(albums)[1], " death metal albums in the world"))

#-----      formatting     -----#
bands[, formed_in := as.numeric(as.character(formed_in))]

#First Death Metal bands?
first_band = bands[ , .SD[which.min(formed_in)]]
print(paste0("The first deth metal band is: ", first_band$name))
print(paste0("They were found in ", first_band$formed_in, " in ", first_band$country))

#First metal album?
first_album = albums[, .SD[which.min(year)]]
print(paste0("World's first death metal album is: ", first_album$title, " released in ", first_album$year))

#Cumulative number of bands and album though the year?
bands[, nb.of.band := .N, by = formed_in]
band.count = unique(bands[, c("nb.of.band", "formed_in")])
band.count = band.count[order(formed_in)]
band.count = band.count[!is.na(formed_in), ]
band.count[, nb.of.band.cum := cumsum(nb.of.band)]

albums[, nb.of.album := .N, by = year]
album.count = unique(albums[, c("nb.of.album", "year")])
album.count = album.count[order(year)]
album.count = album.count[!is.na(year), ]
album.count[, nb.of.album.cum := cumsum(nb.of.album)]

band.album = merge(album.count, band.count, by.x = "year", by.y = "formed_in")

p1 = plot_ly(band.album, x = ~year, y = ~nb.of.band.cum, name = 'number of bands', type = "scatter", mode = 'lines+markers') %>%
  add_trace(y = ~nb.of.album.cum, name = 'number of albums', mode = 'lines+markers') %>%
  layout(yaxis = list(title = FALSE), legend = list(x = 0.1, y = 0.9))
p1

#number of new bands & albums?
p2 = plot_ly(band.album, x = ~year, y = ~nb.of.band, name = 'number of new bands', type = "scatter", mode = 'lines+markers') %>%
  add_trace(y = ~nb.of.album, name = 'number of new albums', mode = 'lines+markers') %>%
  layout(yaxis = list(title = FALSE), legend = list(x = 0.1, y = 0.9))
p2


#Number of bands without albums per year
cover.band = merge(bands, albums, by.x = "id", by.y = "band", all.x = T)
cover.band = cover.band[!is.na(year)][, nb.of.cover.band := .N, by = formed_in]
cover.band = unique(cover.band[order(formed_in), c("nb.of.cover.band", "formed_in")])

p3 = plot_ly(cover.band, x = ~formed_in, y = ~nb.of.cover.band, type = "scatter", mode = 'lines+markers') %>%
  layout(yaxis = list(title = FALSE), legend = list(x = 0.1, y = 0.9),
         title = 'number of new cover bands')
p3


#Number of distinct genres of bands formed in each year.




#Distribution of main genre?
bands[, genre := as.character(genre)]
bands[, genre1 := sapply(genre, function(x) strsplit(x, '|', fixed = T)[[1]][1])]
bands[, genre2 := sapply(genre, function(x) strsplit(x, '|', fixed = T)[[1]][2])]
bands[, genre3 := sapply(genre, function(x) strsplit(x, '|', fixed = T)[[1]][3])]


genre.count = bands[, list('count' = sum(genre != '')), by = genre1]
genre.count = genre.count[order(-count)]
genre.count[, genre1 := ifelse(count <= genre.count$count[11], "others", genre1)]


p5 = plot_ly(labels = ~genre.count$genre1, values = ~genre.count$count) %>%
     add_pie(hole = 0.6) %>%
     layout(title = "Distribution of death metal subgenre",  showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p5


#Which subgenre develops faster?
main.genre = unique(genre.count$genre1)
main.genre = main.genre[1:8]


main.genre.bands = bands[genre1 %in% main.genre, c("id", "genre1")]
main.genre.albums = merge(albums, main.genre.bands, 
                          by.x = "band", by.y = "id",
                          all.x = T)
main.genre.albums = main.genre.albums[!is.na(genre1),]

main.genre.dev = main.genre.albums[, list("count.main.genre.album" = .N),
                                    by = c("year", "genre1")]

main.genre.dev = main.genre.dev[order(genre1, year)]
main.genre.dev = main.genre.dev[year <= 2015, ]

p6 = plot_ly(x     = ~main.genre.dev$year, 
             y     = ~main.genre.dev$count.main.genre.album, 
             split = ~main.genre.dev$genre1,
             type  = "scatter",
             color = ~main.genre.dev$genre1, 
             mode  = "lines+markers") %>%
     layout(yaxis = list(title = FALSE), legend = list(x = 0.1, y = 1),
            title = "number of new albums by genre")

p6

#Which bands are most productive in terms of number of albums?


#which albums are most popular? (by number of reviews)

#which countries have the most death metal bands?
# - world map & bar plot

#best album?

#lyrics word cloud of each genre
  