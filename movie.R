library(lsa)#latent semantic analysis

  u.data <- read.delim("u.data", sep = "\t", header = FALSE)
  colnames(u.data) <- c("User.ID","Movie.ID","Ratings","Time")
  u.item <- read.delim("u.item", sep = "|", header = FALSE)

# Creating a User X Movies Matrix
  cm <- matrix(NA, max(u.data$User.ID), max(u.data$Movie.ID))         
  colnames(cm) <- u.item[,2]             # load movie names
for (i in 1:length(u.data[,1]))
{
  uid <- u.data[i,1]                  # loading user id frm u.data 
  mid <- u.data[i,2]                  #loading movie id frm u.data
  cm[uid,mid] <- u.data[i,3]          # loading Rating
}

  cm[is.na(cm)] <- 0                    # Transforming the NA in the User X Movies Matrix to Zero's

  #top 3 recommended movie based the movies which that user has rated.

mov_recom <- function(x)
 {
   x_user <- cm[x,]  
   user_similarity <- matrix(NA,max(u.data$User.ID),1)        
   for(i in 1:length(cm[,1]))
  {
   #cosine func calculates the ratings of all users cm with target_user eg:x[0 0 3 0 5] & cm 
    user_similarity[i,1] <- cosine(cm[i,],x_user)
  }
  # this stores the indexes of the movie which that x has not rated
  notrated_index <- which(x_user == 0)
  # Creating a matrix of all the unrated movies of that x 
  notrated_matrix <- cm[,notrated_index]
  
  # Creating a new weighted matrix done by multiplying user similarities and movie ratings
  weighted_matrix <- matrix(NA, nrow(notrated_matrix), ncol(notrated_matrix))
  for(i in 1:nrow(notrated_matrix))
  {
    weighted_matrix[i,] <- user_similarity[i,1] * notrated_matrix[i,]
  }
  
  # This is the sum of ratings of each movie
  total_rating <- colSums(weighted_matrix)
  
  # This is the sum of similarity measures of users who have rated a particular movies
  sum_rated <- data.frame()
  for(i in 1:ncol(weighted_matrix))
  {
    # For each movies extracting indices where the movie is actually rated
    temp_index <- which(weighted_matrix[,i] >0)
    sum_rated[1,i] <- sum(user_similarity[temp_index])
  }
  
  # The predicted movie rating
  predicted_ratings <- total_rating/sum_rated
  colnames(predicted_ratings) <- colnames(cm[,notrated_index])
  ordered_index <- order(predicted_ratings, decreasing =T)
  top_index <- ordered_index[1:3]
  print(paste("Top 3 Recommended Movie are:"))
  final <- predicted_ratings[,top_index]
  for (i in 1:3)
  {
    print(paste(colnames(final[i])))
  }
}

mov_recom(11)


