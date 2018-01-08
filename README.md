## Overview

This is an interactive web application made with Shiny by Xinghui Song, a Stats Master's student at University of Michigan. 
The idea is to create a dashboard that showcases interesting statistics and visualizations about movies, directors and actors.
Hopefully, by looking at these analytics, movie fans can uncover mildly interesting insights that they normally wouldn't realize
just by seeing the raw information on websites like IMDb. 

Aside from this page, this web app has three main components. 

The **Statistics and Rankings** page has two tabs. The **Visualizations** tab displays some visualizations on the distribution
of ratings of all movies from the data sources. Different plots are created for three rating systems: IMDb rating, Metascore from
Metacritics and Tomatometer from Rotten Tomatoes. The **Director/Actor Rankings** tab shows top actors and directors based on their
average movie ratings. The three rating systems are also available for these rankings. On the left side of this page is a few filtering
criteria determining which movies, actors or directors to be included in the output.

The **Directors** page contains general information and statistics of over 10,000 directors. You can find a director by type in and select
his/her name in the search box right under the "Directors" tab in the side menu. The **Actors** page functions in the exact same way,
only that it displays information of individual actors.

## Data Sources

The collection of movies in this web app is from [MovieLens 20M Dataset](https://grouplens.org/datasets/movielens/20m/). The dataset
contains the titles, genres, and IMDb IDs of close to 30,000 movies.

Another data source is the [OMDb API](http://www.omdbapi.com/). By supplying the IMDb ID of a movie, the API can return a wide range
of information about that movie, including its year of release, director, actors, ratings and box office (though often missing).

Finally, some of the general information on **Directors** page and **Actors** page are scraped from [IMDb](http://www.imdb.com/) using
package `rvest`

## Issues and TODOs

 + The OMDb API only lists the top 4 actors for each movie. Need to get more actors from IMDb.
 + The "Best Seller" movie in director/actor pages may not be accurate since the box office of many movies are not available in the dataset. May need to scrape this information from IMDb movie pages.
 + The "Oscars Won" and "Oscars Nominated" value boxes are merely placeholders at this moment.
 + The layout of Directors/Actors pages need to be reworked. 
 + Director images vary. Can collide with text on the right.
