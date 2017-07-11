# Copyright (C) Kevin R. Coombes, 2007-2012

###
### NEEDLES.R
###

defaultNeedleParams <- list(MATCH=1,
                            MISMATCH=-1,
                            GAP=-1,
                            GAPCHAR="*")


##-----------------------------------------------------------------------------
needles <- function(pattern, subject, params=defaultNeedleParams) {
  MATCH <- params$MATCH
  MISMATCH <- params$MISMATCH
  GAP <- params$GAP
  GAPCHAR <- params$GAPCHAR
  
  patt <- strsplit(pattern, "")[[1]]
  subj <- strsplit(subject, "")[[1]]
  ## Initialize
  scoreMatrix <- matrix(NA,     ncol=1+length(patt), nrow=1+length(subj))
  direcMatrix <- matrix("none", ncol=1+length(patt), nrow=1+length(subj))
  scoreMatrix[1,1] <- 0
  for (j in 1:length(patt)) {
    scoreMatrix[1, j+1] <- GAP*j
    direcMatrix[1, j+1] <- "left"
  }
  for (i in 1:length(subj)) {
    scoreMatrix[i+1,1] <- GAP*i
    direcMatrix[i+1,1] <- "up"
  }

  ## Fill
  for (i in 1:length(subj)) {
    for (j in 1:length(patt)) {
      ## Translating from 0-based arrays and vectors to 1-based
      I <- i + 1
      J <- j + 1
      ## Calculate (mis)match scores
      letter1 <- patt[J-1]
      letter2 <- subj[I-1]
      if(letter1 == letter2) {
        diagonalScore <- scoreMatrix[I-1, J-1] + MATCH
      } else {
        diagonalScore <- scoreMatrix[I-1, J-1] + MISMATCH
      }
      ## Calculate gap scores
      upScore   <- scoreMatrix[I-1, J] + GAP
      leftScore <- scoreMatrix[I, J-1] + GAP
      ## Choose best score
      if (diagonalScore >= upScore) {
        if (diagonalScore >= leftScore) {
          scoreMatrix[I, J] <- diagonalScore
          direcMatrix[I, J] <- "diagonal";
        } else {
          scoreMatrix[I, J] <- leftScore
          direcMatrix[I, J] <- "left";          
        }
      } else {
        if (upScore >= leftScore) {
          scoreMatrix[I, J] <- upScore
          direcMatrix[I, J] <- "up";          
        } else {
          scoreMatrix[I, J] <- leftScore
          direcMatrix[I, J] <- "left";          
        }
      }
    }
  }
  theScore <- scoreMatrix[I, J]

  ## backtrace
  J <- length(patt)+1
  I <- length(subj)+1
  align1 <- align2 <- c()
  while(1) {
    direc <- direcMatrix[I, J]
    if (direc == 'none') {
      break
    }
    if (direc == 'diagonal') {
      align1 <- c(patt[J-1], align1)
      align2 <- c(subj[I-1], align2)
      I <- I-1
      J <- J-1
    } else if (direc == 'left') {
      align1 <- c(patt[J-1], align1)
      align2 <- c(GAPCHAR, align2)
      J <- J-1
    } else if (direc == 'up') {
      align1 <- c(GAPCHAR, align1)
      align2 <- c(subj[I-1], align2)
      I <- I-1
    } else {
      stop("This is not supposed to happen.")
    }
  }
  list(score=theScore,
       align1=paste(align1, collapse=''),
       align2=paste(align2, collapse=''),
       sm=scoreMatrix,
       dm=direcMatrix)
}


##-----------------------------------------------------------------------------
needleScores <- function(pattern, subjects, params=defaultNeedleParams) {
    scores <- sapply(subjects,
                     function(x, y, p) {
                         needles(x, y, p)$score
                     },
                     y=pattern,
                     p=params)
    scores
}

