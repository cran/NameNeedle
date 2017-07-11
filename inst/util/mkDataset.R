extdata.dir <- file.path("..", "extdata")

sf2Names.datafile <- file.path(extdata.dir, "sf2Names.tsv")
sf2Names <- read.delim(sf2Names.datafile, row.names=1, as.is=TRUE)
sf2Names <- sf2Names$x

rppaNames.datafile <- file.path(extdata.dir, "rppaNames.tsv")
rppaNames <- read.delim(rppaNames.datafile, row.names=1, as.is=TRUE)
rppaNames <- rppaNames$x

illuNames.datafile <- file.path(extdata.dir, "IlluminaNames.tsv")
illuNames <- read.delim(illuNames.datafile, row.names=1, as.is=TRUE)
illuType <- factor(illuNames$Type)
illuNames <- illuNames$illuNames

data.dir <- file.path("..", "..", "data")
dataset <- file.path(data.dir, "cellLineNames.RData")
save(sf2Names, rppaNames, illuNames, illuType, file=dataset)

