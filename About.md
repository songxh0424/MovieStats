## Motivation

This is an interactive web application made with Shiny by Xinghui Song, a Stats Master's student at University of Michigan. 
The idea is to create a dashboard that showcases interesting statistics and visualizations about movies, directors and actors.
Hopefully, by looking at these analytics, movie fans can uncover mildly interesting insights that they normally wouldn't realize
just by seeing the raw information on websites like IMDb. 

## Data Sources

The collection of movies in this web app is from [MovieLens 20M Dataset](https://grouplens.org/datasets/movielens/20m/). The dataset
contains the titles, genres, and IMDb IDs of close to 30,000 movies.

Another data source is the [OMDb API](http://www.omdbapi.com/). By supplying the IMDb ID of a movie, the API can return a wide range
of information about that movie, including its year of release, director, actors, ratings and box office (though often missing).

Finally, some of the general information on **Directors** page and **Actors** page are scraped from [IMDb](http://www.imdb.com/) using
package `rvest`

## Issues and TODOs

 + Some pages can take a while to load. Especially the rankings.
 + The OMDb API only lists the top 4 actors for each movie. Need to get more actors from IMDb.
 + The "Best Seller" movie in director/actor pages may not be accurate since the box office of many movies are not available in the dataset. May need to scrape this information from IMDb movie pages.
 + Consider switch from using DT datatable to formattable, at least the summary table should use formattable.
