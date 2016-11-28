#install.packages("httr")
#install.packages("jsonlite")
#clear workspace if you want
rm(list = ls())

library(httr)
library(jsonlite)

#specifies the confidence interval that must be exceed for inclusion
tag_confidence_threshold <- .4999
caption_confidence_threshold <- .4999

#set working dir
setwd("C:\\Users\\pshea\\OneDrive\\r\\MSFT Cognitive\\vision")

# get api key
fileName <- 'key.txt'
mykey <- readChar(fileName, file.info(fileName)$size)

#create empty data frame
my_tags <- data.frame(name=character(0),confidence=character(0),hint=character(0), stringsAsFactors = FALSE)

#override photo filename
#my_pic <- "IMG_0414.jpg"

pic_list <- list.files(pattern = "*.jpg$",ignore.case = TRUE)

for (pic_count in 1:length(pic_list)){
  paste0("Working on Pic #",pic_count)
  my_pic <- pic_list[pic_count]
  request_body <-upload_file(my_pic)
  result <- POST("https://api.projectoxford.ai/vision/v1.0/analyze?visualFeatures=Categories,faces,tags,adult,Description&language=en",
                 body = list(x = request_body),
                 add_headers(.headers = c("Content-Type"="multipart/form-data","Ocp-Apim-Subscription-Key"=mykey)))
  myresults<- content(result)
  
  #build tag data.frame
  for (loop_count in 1:length(myresults[3]$tags)){
      temp_frame <- data.frame(myresults[3]$tags[loop_count], stringsAsFactors = FALSE)
      if (ncol(temp_frame) == 3) temp_frame$hint <- NULL
      my_tags <- rbind(my_tags,data.frame(temp_frame, stringsAsFactors = FALSE))
  }
  
  
  #if nsfw then add as tags
  if(myresults$adult$isAdultContent==TRUE) {
    my_tags <- rbind(my_tags, "Adult", stringsAsFactors = FALSE)  
    my_tags$confidence[loop_count+1] = myresults$adult$adultScore
  }
  
  if(myresults$adult$isRacyContent==TRUE) {
    my_tags <- rbind(my_tags, "NSFW", stringsAsFactors = FALSE)  
    my_tags$confidence[loop_count+1] = myresults$adult$racyScore
  }
  
  #get caption
  my_caption <- data.frame(myresults[4]$description$captions, stringsAsFactors = FALSE)
  my_caption$text
  my_caption$confidence
  
  #build faces data.frame
  for (loop_count in 1:length(myresults$faces)){
    if (loop_count==1) {
      ur_face <- data.frame(myresults$faces[1], stringsAsFactors = FALSE)
    } else
      ur_face  <- rbind(ur_face ,data.frame(myresults$faces[loop_count], stringsAsFactors = FALSE))
  }
  
  #build tag string
  for (loop_count in 1:nrow(my_tags)){
    if (my_tags$confidence[loop_count]> tag_confidence_threshold) {
      if (loop_count==1) {
        tag_string <- my_tags$name[loop_count]
      } else tag_string <- paste0(tag_string, ", ", my_tags$name[loop_count])
    }
  }
  
  
  #build out command for to update the file
  if(my_caption$confidence>caption_confidence_threshold) {
    cmd_string <- paste0('exiftool -sep ", " -keywords="',tag_string, '" ', '-title="',my_caption$text, '" ', my_pic)
  } else cmd_string <- paste0('exiftool -sep ", " -keywords="',tag_string, '" ',my_pic)
  
  #update metadate of photo
  info <- system(cmd_string,inter=TRUE)
}  


