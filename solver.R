classify <- function(x){
  x[grep("a",x)] <- 4
  x[grep("b|c",x)] <- 3
  x[grep("d|e|f",x)] <- 2
  x[grep("g|h|i|j",x)] <- 1
  x
}

visualise <- function(x){
  matrix(as.numeric(x),5,4,byrow=TRUE)
}

visualise2 <- function(x){
  x <- strtrim(x,1)
  for(i in 1:10){
    x <- gsub(letters[i],i,x)
  }
  matrix(as.numeric(x),5,4,byrow=TRUE)
}

# initial setup
init.puzzle <- c("b1","a1","a2","c1","b2","a3","a4","c2","g","d1","d2","h","e1","e2","f1","f2","i",0,0,"j")
puzzle <- list()
puzzle[[1]] <- init.puzzle
puzzle.save <- classify(init.puzzle)

hash <- list()
hash[[1]] <- puzzle.save
hash.id <- 1
hash.load <- 1
hash.front <- 0
hash.goal <- 0
musume <- c("#ffffff","#e5007f","#adff2f","#fff33f","#0233CB","#00b379","#dda0dd","#56bed9","#800080","#f1d1d4","#f5b48c")

# solve
while(hash.goal == 0){
  # hash load
  puzzle.load <- puzzle[[hash.load]]
  empty <- which(puzzle[[hash.load]]==0)
  move <- c(NA,puzzle.load[empty[1]+1],NA,puzzle.load[empty[1]+4],
            NA,puzzle.load[empty[2]+1],NA,puzzle.load[empty[2]+4])
  for(i in 1:2){
    if(empty[i]>1) move[i*4-3] <- puzzle.load[empty[i]-1]
    if(empty[i]>4) move[i*4-1] <- puzzle.load[empty[i]-4]
    if(mod(empty[i],4)==0) move[i*4-2] <- NA
    if(mod(empty[i]+3,4)==0) move[i*4-3] <- NA
  }
  move[is.na(move)] <- FALSE
  
  if(length(grep("a",move))==2){
    if(length(grep("a1|a2",move))==2 & length(grep(0,move))==2){
      puzzle.move <- puzzle.load
      puzzle.move[puzzle.move=="a3"] <- 0
      puzzle.move[puzzle.move=="a4"] <- 0
      puzzle.move[puzzle.move=="a1"] <- "a3"
      puzzle.move[puzzle.move=="a2"] <- "a4"
      puzzle.move[empty[1]] <-"a1"
      puzzle.move[empty[2]] <-"a2"
    }
    if(length(grep("a1|a3",move))==2 & length(grep(0,move))==2){
      puzzle.move <- puzzle.load
      puzzle.move[puzzle.move=="a2"] <- 0
      puzzle.move[puzzle.move=="a4"] <- 0
      puzzle.move[puzzle.move=="a1"] <- "a2"
      puzzle.move[puzzle.move=="a3"] <- "a4"
      puzzle.move[empty[1]] <-"a1"
      puzzle.move[empty[2]] <-"a3"
    }
    if(length(grep("a2|a4",move))==2 & length(grep(0,move))==2){
      puzzle.move <- puzzle.load
      puzzle.move[puzzle.move=="a1"] <- 0
      puzzle.move[puzzle.move=="a3"] <- 0
      puzzle.move[puzzle.move=="a2"] <- "a1"
      puzzle.move[puzzle.move=="a4"] <- "a3"
      puzzle.move[empty[1]] <-"a2"
      puzzle.move[empty[2]] <-"a4"
    }
    if(length(grep("a3|a4",move))==2 & length(grep(0,move))==2){
      puzzle.move <- puzzle.load
      puzzle.move[puzzle.move=="a1"] <- 0
      puzzle.move[puzzle.move=="a2"] <- 0
      puzzle.move[puzzle.move=="a3"] <- "a1"
      puzzle.move[puzzle.move=="a4"] <- "a2"
      puzzle.move[empty[1]] <-"a3"
      puzzle.move[empty[2]] <-"a4"
    }
    hash.save <- classify(puzzle.move)
    for(j in hash.id:1){
      if(sum(hash[[j]] == hash.save)==20) break
      if(j==1){
        hash.id <- hash.id + 1
        hash[[hash.id]] <- hash.save
        puzzle[[hash.id]] <- puzzle.move
        hash.front[[hash.id]] <- hash.load
        if(which(puzzle.move=="a1")==14){
          hash.goal <- hash.id
        }
      }
    }
  }
  
  for(i in c("b","c")){
    if(length(grep(i,move))==2 & length(grep(0,move))==2){
      puzzle.move <- puzzle.load
      puzzle.move[grep(i,puzzle.move)] <- 0
      puzzle.move[empty[1]] <- paste0(i,1)
      puzzle.move[empty[2]] <- paste0(i,2)
      
      hash.save <- classify(puzzle.move)
      for(j in hash.id:1){
        if(sum(hash[[j]] == hash.save)==20) break
        if(j==1){
          hash.id <- hash.id + 1
          hash[[hash.id]] <- hash.save
          puzzle[[hash.id]] <- puzzle.move
          hash.front[[hash.id]] <- hash.load
        }
      }
    }else{
      for(k in 1:2){
        if(move[k*4-1] == paste0(i,2)){
          puzzle.move <- puzzle.load
          puzzle.move[puzzle.move==paste0(i,1)] <- 0
          puzzle.move[puzzle.move==paste0(i,2)] <- paste0(i,1)
          puzzle.move[empty[k]] <- paste0(i,2)
          
          hash.save <- classify(puzzle.move)
          for(j in hash.id:1){
            if(sum(hash[[j]] == hash.save)==20) break
            if(j==1){
              hash.id <- hash.id + 1
              hash[[hash.id]] <- hash.save
              puzzle[[hash.id]] <- puzzle.move
              hash.front[[hash.id]] <- hash.load
            }
          }
        }
        if(move[k*4] == paste0(i,1)){
          puzzle.move <- puzzle.load
          puzzle.move[puzzle.move==paste0(i,2)] <- 0
          puzzle.move[puzzle.move==paste0(i,1)] <- paste0(i,2)
          puzzle.move[empty[k]] <- paste0(i,1)
          
          hash.save <- classify(puzzle.move)
          for(j in hash.id:1){
            if(sum(hash[[j]] == hash.save)==20) break
            if(j==1){
              hash.id <- hash.id + 1
              hash[[hash.id]] <- hash.save
              puzzle[[hash.id]] <- puzzle.move
              hash.front[[hash.id]] <- hash.load
            }
          }
        }
      }
    }
  }
  
  for(i in c("d","e","f")){
    if(length(grep(i,move))==2 & length(grep(0,move))==2){
      puzzle.move <- puzzle.load
      puzzle.move[grep(i,puzzle.move)] <- 0
      puzzle.move[empty[1]] <- paste0(i,1)
      puzzle.move[empty[2]] <- paste0(i,2)
      
      hash.save <- classify(puzzle.move)
      for(j in hash.id:1){
        if(sum(hash[[j]] == hash.save)==20) break
        if(j==1){
          hash.id <- hash.id + 1
          hash[[hash.id]] <- hash.save
          puzzle[[hash.id]] <- puzzle.move
          hash.front[[hash.id]] <- hash.load
        }
      }
    }else{
      for(k in 1:2){
        if(move[k*4-3] == paste0(i,2)){
          puzzle.move <- puzzle.load
          puzzle.move[puzzle.move==paste0(i,1)] <- 0
          puzzle.move[puzzle.move==paste0(i,2)] <- paste0(i,1)
          puzzle.move[empty[k]] <- paste0(i,2)
          
          hash.save <- classify(puzzle.move)
          for(j in hash.id:1){
            if(sum(hash[[j]] == hash.save)==20) break
            if(j==1){
              hash.id <- hash.id + 1
              hash[[hash.id]] <- hash.save
              puzzle[[hash.id]] <- puzzle.move
              hash.front[[hash.id]] <- hash.load
            }
          }
        }
        if(move[k*4-2] == paste0(i,1)){
          puzzle.move <- puzzle.load
          puzzle.move[puzzle.move==paste0(i,2)] <- 0
          puzzle.move[puzzle.move==paste0(i,1)] <- paste0(i,2)
          puzzle.move[empty[k]] <- paste0(i,1)
          
          hash.save <- classify(puzzle.move)
          for(j in hash.id:1){
            if(sum(hash[[j]] == hash.save)==20) break
            if(j==1){
              hash.id <- hash.id + 1
              hash[[hash.id]] <- hash.save
              puzzle[[hash.id]] <- puzzle.move
              hash.front[[hash.id]] <- hash.load
            }
          }
        }
      }
    }
  }
  
  for(i in c("g","h","i","j")){
    if(length(grep(i,move))>0){
      for(k in 1:2){
        if(k==1 & min(grep(i,move))>4) next
        if(k==2 & max(grep(i,move))<5) break
        puzzle.move <- puzzle.load
        puzzle.move[grep(i,puzzle.move)] <- 0
        puzzle.move[empty[k]] <- i
        
        hash.save <- classify(puzzle.move)
        for(j in hash.id:1){
          if(sum(hash[[j]] == hash.save)==20) break
          if(j==1){
            hash.id <- hash.id + 1
            hash[[hash.id]] <- hash.save
            puzzle[[hash.id]] <- puzzle.move
            hash.front[[hash.id]] <- hash.load
          }
        }
      }
    }
  }
  hash.load <- hash.load + 1
  print(hash.load)
}

# calculate shortest path
shortest.path <- hash.goal
shortest.id <- 0
while(shortest.path[shortest.id+1]>1){
  shortest.id <- shortest.id +1
  shortest.path[shortest.id+1] <- hash.front[shortest.path[shortest.id]]
}
shortest.path <- rev(shortest.path)

# plot shortest path
for(i in shortest.path){
  image(-t(visualise(hash[[i]]))[,5:1],col=heat.colors(50),xaxt="n", yaxt="n", asp=1)
  # image(t(visualise2(puzzle[[i]]))[,5:1],col=musume,xaxt="n", yaxt="n", asp=1)
  Sys.sleep(0.1)
}

#answer
print(paste0("Å“K‰ð‚Í",shortest.id,"Žè‚Å‚·B"))
