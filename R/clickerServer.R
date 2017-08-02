examples.click.server = function() {
  clicker.dir = "D:/libraries/shinyEventsClicker/apps/clickerapp"
  app = clickerServerApp(clicker.dir=clicker.dir,userid="skranz")
  viewApp(app)

}

clicker.remove.running.task = function(clicker.dir=ct$clicker.dir, task.id = ct$task.id, ct = NULL) {
  restore.point("remove.running.task")
  # remove existing running files
  running.dir = file.path(clicker.dir, "running_task")
  files = list.files(running.dir,full.names = TRUE)
  if (!is.null(task.id)) {
    files = files[has.substr(files, task.id)]
  }
  file.remove(files)

}

write.clicker.task = function(ct,clicker.dir=ct$clicker.dir, clicker.tag=first.non.null(ct[["clicker.tag"]],make.clicker.tag(clicker.dir=clicker.dir, ct=ct))) {
  restore.point("write.clicker.task")

  task.id = ct$task.id
  ct$clicker.tag = clicker.tag

  task.dir = ct$task.dir = file.path(clicker.dir, "tasks", task.id)
  if (!dir.exists(task.dir)) {
    dir.create(task.dir,recursive = TRUE)
    Sys.chmod(task.dir, mode="777", use_umask = FALSE)
  }
  tag.dir = ct$tag.dir = file.path(task.dir,"tags",clicker.tag)
  if (!dir.exists(tag.dir)) {
    dir.create(tag.dir,recursive = TRUE)
    Sys.chmod(tag.dir, mode="777", use_umask = FALSE)
  }
  long.file = file.path(task.dir, "ct.Rds")
  ct = as.list(ct)
  saveRDS(ct, long.file, compress=FALSE)

  # write file that specifies task as running
  running.dir = file.path(clicker.dir, "running_task")


  # remove existing running files
  files = list.files(running.dir,full.names = TRUE)
  file.remove(files)

  running.file = file.path(running.dir,paste0(ct$task.id,"---", as.integer(Sys.time())))
  writeLines("",running.file)

  # write a file with the last task
  # we are mainly interested in the creation time
  writeLines(ct$task.id, file.path(clicker.dir,"LAST_TASK.txt"))

  invisible(ct)
}

import.yaml.with.source = function(txt=readLines(file, warn=FALSE), file=NULL, source.field = "source_text", add.head=TRUE, tab.len=2) {
  restore.point("import.yaml.with.source")

  txt = sep.lines(mark_utf8(txt))
  char = substring(txt,1,1)
  start.rows = which(!char %in% c(" ","\t","#",""))

  if (length(start.rows)==0) return(list())

  pos = cbind(start.rows,c(start.rows[-1]-1,length(txt)))
  blocks = lapply(seq.int(NROW(pos)), function(i) {
    merge.lines(txt[pos[i,1]:pos[i,2]])
  })

  li = lapply(blocks,function(txt) {
    el = import.yaml(text = txt)
    if (!add.head) {
      txt = merge.lines(substring(sep.lines(txt)[-1],1+tab.len))
    }
    el[[1]][[source.field]] = txt
    el
  })
  li = do.call(c,li)
  li
}


get.clicker.tags = function(clicker.dir=ct$clicker.dir, ct=NULL, task.id = ct$task.id) {
  restore.point("get.clicker.tags")


  tag.dir = file.path(clicker.dir, "tasks", task.id,"tags")
  if (!dir.exists(tag.dir)) return(NULL)

  dirs = list.dirs(tag.dir,full.names = FALSE,recursive = FALSE)
  return(dirs)
}


make.clicker.tag = function(clicker.dir=ct$clicker.dir, task.id = ct$task.id,ct=NULL) {
  restore.point("make.clicker.tag")

  dirs = get.clicker.tags(clicker.dir, ct, task.id)
  if (is.null(dirs)) return("1")
  return(as.character(length(dirs)+1))
}
