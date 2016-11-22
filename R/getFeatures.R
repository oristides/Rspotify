'Get audio feature information for a single (or multiple) tracks identified by its unique Spotify ID.
#'
#'
#'function to get audio feature information for a single or multiple tracks identified by its unique Spotify ID's but also from a list of Ids or even a playlist with GetPlaylist function
#'@param spotify_ID The Spotify ID for the track.
#'@param token An OAuth token created with \code{spotifyOAuth}.
#'@export

GetFeatures <-function(songs, token) {
  if(class(songs)=="data.frame") { songsnames <-songs$id } else { songsnames<-songs}
  req <- httr::GET(paste0("https://api.spotify.com/v1/audio-features//?ids=",paste0(songsnames, collapse=",")), httr::config(token = keys))
  json1<-httr::content(req)
  json2<-jsonlite::fromJSON(jsonlite::toJSON(json1))
  
  #create columns
  id<-unlist(json2$audio_features$id)
  danceability <-unlist(json2$audio_features$danceability)
  energy<-unlist(json2$audio_features$energy)
  key<-unlist(json2$audio_features$key)
  loudness<-unlist(json2$audio_features$loudness)
  mode<-unlist(json2$audio_features$mode)
  speechiness<-unlist(json2$audio_features$speechiness)
  acousticness<-unlist(json2$audio_features$acousticness)
  instrumentalness  <-unlist(json2$audio_features$instrumentalness)
  liveness <-unlist(json2$audio_features$liveness)
  valence<-unlist(json2$audio_features$valence)
  tempo<-unlist(json2$audio_features$tempo)
  duration_ms<-unlist(json2$audio_features$duration_ms)
  time_signature<-unlist(json2$audio_features$time_signature)
  uri<-unlist(json2$audio_features$uri)
  analysis_url  <-unlist(json2$audio_features$analysis_url)
  
  #columns to data.frame
  dados<-data.frame(id,danceability,energy,key,loudness, 
                    mode, speechiness, acousticness, instrumentalness, liveness, valence,
                    tempo, duration_ms,time_signature, uri, analysis_url, 
                    stringsAsFactors = F) 
  return(dados) }




