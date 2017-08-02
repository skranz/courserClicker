


normalize.clicker.tag = function(ct, clicker.tag) {
  restore.point("normalize.clicker.tag")

  if (length(clicker.tag)==0) return(NULL)

  if ("none" %in% clicker.tag) return(NULL)

  if ("all" %in% clicker.tag | "latest" %in% clicker.tag) {
    dirs = get.clicker.tags(ct=ct)
    if ("all" %in% clicker.tag) return(dirs)
    if ("latest" %in% clicker.tag) {
      nums = na.omit(as.numeric(dirs))
      if (length(nums)>0) {
        clicker.tag = union(clicker.tag, as.character(max(nums)))
      } else {
        return(NULL)
      }
    }
  }
  clicker.tag

}

load.sub.data = function(ct, clicker.tag = ct$clicker.tag, app=getApp(), type= first.non.null(ct[["type"]],"quiz"),...) {
  restore.point("load.sub.data")
  clicker.tag = normalize.clicker.tag(ct=ct, clicker.tag=clicker.tag)

  if (length(clicker.tag)==0) return(NULL)

  dirs = file.path(ct$clicker.dir, "tasks", ct$task.id, "tags", clicker.tag)
  files = unlist(lapply(dirs, function(dir) list.files(dir,pattern = glob2rx("*.sub"),full.names = TRUE)))
  if (length(files)==0) return(NULL)

  header.file = file.path(ct$clicker.dir, "tasks", ct$task.id,"colnames.csv")
  txt = readLines(header.file,warn = FALSE)
  li = unlist(lapply(files, readLines,warn=FALSE))
  txt = c(txt, li)

  dat = readr::read_csv(merge.lines(txt))

  Wid = get.clicker.Widget(type)
  if (!is.null(Wid$server$transform.sub.data)) {
    dat = call.fun(Wid$server$transform.sub.data,dat,ct)
  }
  dat
}


clicker.highscore.example = function() {
  restore.point.options(display.restore.point = TRUE)
  clicker.dir = "D:/libraries/courser/courses/vwl/course/clicker"
  hs = compute.clicker.highscore(clicker.dir)
  hs
}

compute.clicker.highscore = function(clicker.dir, multi.tag.action = c("sum", "latest")[1]) {
  restore.point("compute.clicker.highscore")
  df = update.all.aggregate.task.data(clicker.dir,return.data = TRUE)

  if (multi.tag.action == "latest") {
    df = df %>%
      group_by(userid, task.id) %>%
      filter(tag == tag[n()])
  }

  hs = df %>%
    group_by(userid) %>%
    summarize(points = sum(points)) %>%
    arrange(-points) %>%
    mutate(rank = rank(-points, ties.method="min"), rank.max = rank(-points, ties.method="max")) %>%
    select(rank, userid, points, rank.max)

  hs
}




load.aggregate.task.data = function(task.dir = file.path(clicker.dir, "tasks", task.id), clicker.dir = ct$clicker.dir, task.id = ct$task.id, ct=NULL) {
  data.file = file.path(task.dir, "aggregate.csv")
  if (!file.exists(data.file)) return(NULL)
  read_csv(data.file,col_names = TRUE)
}

update.all.aggregate.task.data = function(clicker.dir, return.data=FALSE,...) {
  restore.point("update.all.aggregate.task.data")

  task.dirs = list.files(file.path(clicker.dir,"tasks"), full.names=TRUE)


  li = lapply(task.dirs, ..., function(task.dir,...) {
    update.aggregate.task.data(task.dir = task.dir,return.data=return.data,...)
  })

  if (!return.data) return(NULL)

  li = li[!sapply(li, is.null)]
  df = bind_rows(li)
  df
}

# update the aggregate
# TO DO: May need to load ct, once we use clicker for other types than quiz
update.aggregate.task.data = function(clicker.dir = ct$clicker.dir, task.id = ct$task.id,task.dir = file.path(clicker.dir, "tasks", task.id), ct=NULL, force=FALSE, return.data=TRUE) {
  restore.point("update.aggregate.task.data")

  # check if data is already up-to-date
  if (!force) {
    up.to.date = is.aggregate.task.data.up.to.date(task.dir=task.dir)
    if (up.to.date) {
      if (return.data)
        return(load.aggregate.task.data(task.dir = task.dir))
      return(invisible(NULL))
    }
  }



  dirs = list.files(file.path(task.dir, "tags"), full.names = TRUE)

  files = unlist(lapply(dirs, function(dir) list.files(dir,pattern = glob2rx("*.sub"),full.names = TRUE)))
  if (length(files)==0)
    return(NULL)

  header.file = file.path(task.dir,"colnames.csv")
  txt = readLines(header.file,warn = FALSE)
  li = unlist(lapply(files, readLines,warn=FALSE))
  txt = c(txt, li)

  dat = readr::read_csv(merge.lines(txt))

  if (is.null(ct))
    ct = readRDS(file.path(task.dir, "ct.Rds"))

  Wid = get.clicker.Widget(ct$type)
  if (!is.null(Wid$server$transform.sub.data)) {
    dat = call.fun(Wid$server$transform.sub.data,dat,ct)
  }

  write_csv(dat, file.path(task.dir,"aggregate.csv"))

  if (return.data) {
    return(dat)
  }

  return(invisible())
}


is.aggregate.task.data.up.to.date = function(clicker.dir = ct$clicker.dir, task.id = ct$task.id, task.dir = file.path(clicker.dir, "tasks", task.id), ct=NULL) {
  data.file = file.path(task.dir, "aggregate.csv")
  if (!file.exists(data.file)) return(FALSE)

  tag.files = list.files(file.path(task.dir,"tags"), full.names=TRUE)
  tag.date = file.mtime(tag.files)

  data.date = file.mtime(data.file)

  any(tag.date > data.date)
}

count.choices = function(values, choices) {
  counts = rep(0, length(choices))
  names(counts) = choices
  cc = table(values)
  counts[names(cc)] = cc

  counts
}

html.result.table = function(df,colnames=colnames(df), bg.color="#fff", font.size=14, align=NULL) {
  restore.point("html.table")
  n = NROW(df)
  row.bgcolor = rep(bg.color,length=n)

  if (is.null(align)) align="left"
  align=rep(align, length=NCOL(df))

  head = paste0('<th class="result-table-th">',colnames,'</th>', collapse="")
  head = paste0('<tr>', head, '</tr>')

  td.class = rep("result-table-td", NROW(df))

  cols = 1:NCOL(df)
  code = paste0('"<td style=\\"text-align: ",align[[',cols,']] ,"\\" class=\\"",td.class,"\\" nowrap bgcolor=\\"",row.bgcolor,"\\">", df[[',cols,']],"</td>"', collapse=",")
  code = paste0('paste0("<tr>",',code,',"</tr>", collapse="\\n")')
  call = parse(text=code)
  main = eval(parse(text=code))

  tab = paste0('<table>\n', head, main, "\n</table>")

  th.style='font-weight: bold; margin: 3px; padding: 3px; text-align: center; border-bottom: solid;'
  td.style='font-family: Verdana,Geneva,sans-serif; margin: 0px 3px 1px 3px; padding: 1px 3px 1px 3px; text-align: center; border-bottom: solid;'

  if (!is.null(font.size)) {
    th.style = paste0(th.style, "font-size: ", font.size,";")
    td.style = paste0(td.style, "font-size: ", font.size,";")
  }

  tab = paste0("<style>",
    " table.result-table-table {	border-collapse: collapse;  display: block; overflow-x: auto;}\n",
    " td.result-table-td {", td.style,"}\n",
    " th.result-table-th {", th.style,"}\n",
     "</style>",tab
  )
  return(tab)
}
