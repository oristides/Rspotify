% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPlaylistSongs.R
\name{getPlaylistSongs}
\alias{getPlaylistSongs}
\title{Get the songs of an specific playlist}
\usage{
getPlaylistSongs(ownerid, playlistid, offset = 0, token)
}
\arguments{
\item{ownerid}{Owner ID}

\item{playlistid}{Playlist ID}

\item{offset}{The index of the first songs to return. Default: 0 (the first object). Maximum offset: 100.000.}

\item{token}{An OAuth token created with \code{spotifyOAuth}.}
}
\description{
function to get songs about a specifc playlist
}

