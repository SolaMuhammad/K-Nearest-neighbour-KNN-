Classifiing<-function(k)
{
  train_data = read.csv("C:\\Users\\Souhila\\Desktop\\4th year\\SecondTerm\\MachineLearning\\TrainData.csv");
  test_data = read.csv("C:\\Users\\Souhila\\Desktop\\4th year\\SecondTerm\\MachineLearning\\TestData.csv");
  print(test_data);
  print(train_data);
  fvs = train_data[,1:8];
  print(fvs);
  minDistance = 1000 ;

  rowsTrain <- dim(train_data)[1];
  print(dim(train_data)[1]);
  rowsTest  <- dim(test_data)[1];
  
  #The first column is for the eucludian distance
  #The second one is for the index and the third one 
  #is for the class
  distances <- data.frame(matrix(ncol = 3, nrow = rowsTrain));
  dim(distances);
  correctlyClassified = 0 ;
  for (i in 1:rowsTest)
  {
    for (j in 1:rowsTrain)
    {
      #print (train_data[j,9]);
      distances[j,2] = j ;
      distances[j,3] <- as.character(train_data[j,9]);
      distances[j,1] = 0 ;
      for (lenFV in 1:8)
      {
        
        distances[j,1] = distances[j,1] + ((test_data[i,lenFV]-train_data[j,lenFV])^2);
        
      }
      distances[j,1] = sqrt(distances[j,1]);
      
    }
    
    distances = distances[order(distances[,1], distances[,2]),];
    print(distances);
    
    #The first column for classes, second for index and the third for number of
    #k that has a minimum distance with this class
    classes <- data.frame(matrix(ncol = 3, nrow = 10));
    classes[1,1] = 'CYT';
    classes[2,1] = 'ERL';
    classes[3,1] = 'EXC';
    classes[4,1] = 'ME1';
    classes[5,1] = 'ME2';
    classes[6,1] = 'ME3';
    classes[7,1] = 'MIT';
    classes[8,1] = 'NUC';
    classes[9,1] = 'POX';
    classes[10,1] = 'VAC';
    classes[1:10,2:3] = 0;
    
    for(c in 1:k)
    {
      if(distances[c,3] == classes[1,1])
      {
        
        classes[1,3] = classes[1,3] + 1 ;
        classes[1,2] = distances[c,2];
      }
      else if(distances[c,3] == classes[2,1])
      {
        classes[2,3] = classes[2,3] + 1 ;
        classes[2,2] = distances[c,2];
      }
      else if(distances[c,3] == classes[3,1])
      {
        classes[3,3] = classes[3,3] + 1 ;
        classes[3,2] = distances[c,2];
      }
      else if(distances[c,3] == classes[4,1])
      {
        classes[4,3] = classes[4,3] + 1 ;
        classes[4,2] = distances[c,2];
      }
      else if(distances[c,3] == classes[5,1])
      {
        classes[5,3] = classes[5,3] + 1 ;
        classes[5,2] = distances[c,2];
      }
      else if(distances[c,3] == classes[6,1])
      {
        classes[6,3] = classes[6,3] + 1 ;
        classes[6,2] = distances[c,2];
      }
      else if(distances[c,3] == classes[7,1])
      {
        classes[7,3] = classes[7,3] + 1 ;
        classes[7,2] = distances[c,2];
      }
      else if(distances[c,3] == classes[8,1])
      {
        classes[8,3] = classes[8,3] + 1 ;
        classes[8,2] = distances[c,2];
      }
      else if(distances[c,3] == classes[9,1])
      {
        classes[9,3] = classes[9,3] + 1 ;
        classes[9,2] = distances[c,2];
      }
      else if(distances[c,3] == classes[10,1])
      {
        classes[10,3] = classes[10,3] + 1 ;
        classes[10,2] = distances[c,2];
      }
    }
    
    classes = classes[order(-classes[,3], classes[,2]),];
    print(classes);
    
    #Calculate accuracy 
    if(test_data[i,9] == classes[1,1]){
      correctlyClassified = correctlyClassified +1;
    }
      
    print(paste0("k value : ",k)) ;
    print(paste0("Predicted class : ", test_data[i,9] , " Actual class : " , classes[1,1] ))
  }
  
  print(paste0("Number of correctly classified instances : ",correctlyClassified)) ;
  print(paste0("Total number of instances : ",rowsTest)) ;
  print(paste0("Accuracy :",correctlyClassified/rowsTest)) ;
  
  
  

  # 
}




