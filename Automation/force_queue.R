# 


linux.path <- "/data/Dagobah/sequoia/spain/level1/"
win.path   <- "n:/sequoia/spain/level1/"

dry <- T

#--------------------------------------------------

force_update_queue <- function(win.path, linux.path, dry=T) {
  
  queue_file <- file.path(win.path, "queue.txt")
  
  if (dry) message(paste('Dry run-------'))
  
  ds <- read.table(queue_file)
  message(paste('Queue entries:', nrow(ds)))
  
  fns <- list.files(win.path, "*.tar", recursive=T)
  fns_notar <- gsub(".tar", "", fns)
  message(paste('File system  :', length(fns)))
  
  queue.fn <- gsub(linux.path, "", ds[,1])
  queue.fn <- gsub(".tar", "", queue.fn)
  
  new <- fns[!fns_notar %in% queue.fn]
  message(paste('New files    :', length(new)))
  
  if (length(new)>1) {
    ds.new <- data.frame(V1=paste0(linux.path, new), V2="QUEUED")
    ds <- rbind(ds, ds.new)
    
    if (!dry) {
      
      queue_bak_file <- file.path(win.path, paste0("queue_", format(Sys.time(), "%Y%m%d%H%M%s"), ".txt."))
      
      ok <- file.copy(queue_file, queue_bak_file)
      
      output.file <- file(queue_file, "wb")
      write.table(ds, file=output.file, quote=F, row.names=F, col.names =F)
      close(output.file)
    }
    
  }
  
  message(paste('Queued files :', sum(ds[,2]=="QUEUED")))
  
}

force_redo <- function(win.path, images, dry=T) {
  
  queue_file <- file.path(win.path, "queue.txt")
  
  if (dry) message(paste('Dry run-------'))
  
  ds <- read.table(queue_file)
  message(paste('Queue entries:', nrow(ds)))
  
  i <- which(basename(ds$V1) %in% images)
  message(paste('Images defined :', length(images)))
  message(paste('Matches found  :', length(i)))
  
  if (length(i) > 0) {
    ds$V2[i] <- "QUEUED"
  }

  message(paste('Queued files   :', sum(ds[,2]=="QUEUED")))
  
  if (!dry) {
    output.file <- file(queue_file, "wb")
    write.table(ds, file=output.file, quote=F, row.names=F, col.names =F)
    close(output.file)
  }
}

force_redo_byline <- function(win.path, dry=T) {
  
  queue_file <- file.path(win.path, "queue.txt")
  
  if (dry) message(paste('Dry run-------'))
  
  ds <- read.table(queue_file)
  message(paste('Queue entries:', nrow(ds)))
  
  i <- which(basename(ds$V1) %in% images)
  message(paste('Images defined :', length(images)))
  message(paste('Matches found  :', length(i)))
  
  if (length(i) > 0) {
    ds$V2[i] <- "QUEUED"
  }
  
  message(paste('Queued files   :', sum(ds[,2]=="QUEUED")))
  
  if (!dry) {
    output.file <- file(queue_file, "wb")
    write.table(ds, file=output.file, quote=F, row.names=F, col.names =F)
    close(output.file)
  }
}


force_update_queue(win.path, linux.path, dry=dry)


imgs <- c("LE07_L1TP_199034_20200612_20200823_02_T1.tar",
          "LE07_L1TP_199034_20200916_20201013_02_T1.tar",
          "LT05_L1TP_199032_20110916_20200820_02_T1.tar",
          "LT05_L1TP_199033_20110916_20200820_02_T1.tar",
          "LT05_L1TP_200033_20111009_20200820_02_T1.tar")

# force_redo(win.path, imgs, dry=dry)
