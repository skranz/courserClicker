# clicker app modes
#
# login: show.course.list, show.course.code
# running:


clicker.client.example = function() {
  restore.point.options(display.restore.point = TRUE)

  clicker.dir = "D:/libraries/courser/courses/vwl/course/clicker"
  token.dir = "D:/libraries/courser/courses/vwl/course/stud_tokens"

  setwd(clicker.dir)
  port = 4634
  app.url = "127.0.0.1:4634"
  app.url = "http://localhost:4634"
  opts = clicker.client.opts(courseid="vwl", clicker.dir=clicker.dir, app.url=app.url, app.title="Vorlesungsquiz VWL", lang="de", email.domain="uni-ulm.de", token.dir=token.dir, auto.guest.login = TRUE, make.guest.init.userid = TRUE, use.cookies=FALSE)
  app = clickerClientApp(opts)
  viewApp(app, port=port, launch.browser = TRUE)
}



clickerClientApp = function(opts=clicker.client.opts()) {
  restore.point("clickerClientApp")
  app = eventsApp()
  glob = app$glob
  copy.into.env(source=opts, dest=glob)

  glob$clicker.dir = glob$clicker.dir
  glob$cur.task.file = NULL
  glob$mainUI = "mainUI"
  glob$running.task.file = NULL
  glob$clicker.pag
  glob$page.params = make.clicker.page.params(opts$page.params, lang=opts[["lang"]], clicker.dir=glob$clicker.dir)


  lop = clicker.client.lop(glob)
  set.lop(lop)

  if (!dir.exists(glob$token.dir)) (
    stop(paste0("token.directory ", glob$token.dir, " does not exist."))
  )

  # list of reactive nounces that monitor updates
  # for given courses
  glob$task.update.rv = reactiveValues(nonce=0)

  obs = observe({
    clicker.update.task(clicker.dir = clicker.dir,glob = glob)
  })
  set.global.observer("myobs",obs=obs)

  app$ui = fluidPage(
    mathjaxHeader(),
    uiOutput("titleUI"),
    uiOutput("mainUI"),
    uiOutput("useridUI")
  )
  appInitHandler(function(...,session=app$session,app=getApp()) {
    initLoginDispatch(lop)
  })
  app
}


clicker.client.opts = function(
  courseid=NULL,
  clicker.dir=getwd(),
  db.dir = file.path(clicker.dir,"db"),
  running.dir = file.path(clicker.dir,"running"),
  token.dir = file.path(clicker.dir, "tokens"),
  need.token = FALSE,
  auto.guest.login = FALSE,
  use.token = TRUE,
  app.url=NULL, clicker.app.url = app.url,
  init.userid="", init.password="", init.code="",
  app.title="Quiz",
  token.valid.min = 180,
  open.app.in.new.tab=TRUE,
  show.course.list=FALSE, show.course.code=FALSE,
  lang="en",
  make.guest.init.userid = FALSE,
  ...)
{
  c(as.list(environment()),list(...))
}


.glob.obs.env = new.env()

clicker.client.start.task.observer = function(tok,app=getApp()) {
  restore.point("clicker.client.start.task.observer")
  app$tok = tok
  app$userid = tok$userid

  # destroy old observer
  if (!is.null(app[["task.obs"]]))
    app$task.obs$destroy()

  setUI("titleUI", clicker.title.ui(app$glob$page.params))

  glob=app$glob
  app$task.obs = observe({
    glob$task.update.rv$nonce
    restore.point("app.task.observer")
    clicker.update.client.task()
    cat("task.nonce changed...")
  })
  clicker.update.client.task()
}

clicker.update.client.task = function(ct = app$glob[["ct"]], app=getApp()) {
  restore.point("clicker.update.client.task")

  glob = app$glob
  # currently no task
  if (is.null(ct)) {

    # task was submitted. we keep the current ui
    if (isTRUE(app$has.submitted)) {
      return()

    # task was stopped before user could submit anything
    } else if (isTRUE(app$has.seen.task)) {
      # Show wait message and disable
      # all inputs and buttons

      ct = app$glob$prev.ct
      qu = first.non.null(ct[["wid"]],ct[["qu"]])

      app$has.submitted = TRUE
      ns = NS(ct$task.id)
      msg = first.non.null(glob$page.params$timeout,"The time for submitting an answer has runned out Please wait until the next quiz starts.")
      setUI(paste0(qu$id,"-msgUI"), p(msg))
      # disable input
      js = '$(":input").prop("disabled", true); $("button:not(#loginNewBtn)").hide(); $("#loginNewBtn").prop("disabled", false);'
      evalJS(js)

    # just show wait page
    } else {
      app$no.clicker.task = TRUE
      ui = clicker.client.wait.page(app$glob$page.params)
      setUI(app$glob$mainUI, ui)
    }
    return()
  }
  app$has.seen.task = TRUE
  app$has.submitted = FALSE

  if (is.function(ct$client.init.handlers)) {
    ct$client.init.handlers(ct)
  } else if (is.character(ct$client.init.handlers)) {
    do.call(ct$client.init.handlers, list(ct=ct))
  }
  setUI(app$glob$mainUI, ct$client.ui)
}

clicker.update.task = function(clicker.dir, glob=app$glob, app=getApp(), millis=1000) {
  #restore.point("clicker.update.task")
  cat(".")
  #cat("\nI am observed...", sample.int(1000,1))

  files = list.files(file.path(clicker.dir, "running_task"),full.names = FALSE)

  # running task has been stopped
  if (length(files) == 0 & !is.null(glob$running.task.file)) {
    # update reactive values
    glob$task.update.rv$nonce = runif(1)
    glob$running.task.file = NULL
    glob$prev.ct = glob$ct
    glob$ct = NULL
    invalidateLater(millis)
    return()
  }

  files = setdiff(files, glob$running.task.file)
  if (length(files)==0) {
    invalidateLater(millis)
    return()
  }

  long.files = paste0(clicker.dir, "/running_task", files)

  # remove older task files
  if (length(files)>1) {
    ord = order(file.mtime(long.files))
    files = files[ord]
    long.files = long.files[ord]
    file.remove(long.files[-1])
  }

  restore.point("clicker.update.task.2")

  file = files[1]
  task.id = str.left.of(file,"---")

  task.file = file.path(clicker.dir,"tasks",task.id,"ct.Rds")

  ct = readRDS(task.file)
  ct$num.sub = 0

  glob$ct = ct
  # update reactive values
  glob$task.update.rv$nonce = runif(1)
  glob$running.task.file = file
  invalidateLater(millis)
}

string.match.answers = function(answers, choices, reverse=TRUE) {
  restore.point("string.match.answers")
  res = match(answers, choices)
  return(res)
  if (any(is.na(res))) {
    uni = unique(answers)
    amatch = agrep(uni, choices, fixed = TRUE)

  }

}

clicker.make.submit.data = function(values, qu, task.id=qu$task.id, tag=0, userid=app$userid, cookie = getCourserAllCookie(), app=getApp()) {
  restore.point("clicker.make.submit.data")
  submit.time = Sys.time()

  li = vector("list", length(qu$parts))
  value.i = 1
  i = 1
  for (i in seq_along(li)) {
    part = qu$parts[[i]]
    if (part$type == "grid_mc" | part$type == "grid_sc") {
      value.i = (value.i:(value.i+length(part$rows)-1))
      answers = unlist(values[value.i])
      answer.ind = string.match.answers(answers, part$cols)
      li[[i]] = data_frame(submit.time=submit.time,task.id=task.id, tag=tag, part.ind=i*1000+seq_along(answers), userid=userid,cookie=cookie$key, answer.ind=answer.ind, answer =  answers, checked=TRUE)
    } else if (part$type=="mc") {
      choices = unlist(part$choices)
      answers = unlist(values[[value.i]])
      checked = !is.na(string.match.answers(choices, answers, reverse=TRUE))

      answer.ind = seq_along(choices)
      li[[i]] = data_frame(submit.time=submit.time,task.id=task.id, tag=tag, part.ind=i, userid=userid, cookie=cookie$key, answer.ind=answer.ind, answer =  choices, checked=checked)
    } else {
      answers = unlist(values[[value.i]])
      answer.ind = string.match.answers(answers, part$choices)
      li[[i]] = data_frame(submit.time=submit.time,task.id=task.id, tag=tag, part.ind=i, userid=userid,cookie=cookie$key, answer.ind=answer.ind, answer =  answers, checked=TRUE)
    }
  }
  if (length(li)>1) {
    df = bind_rows(li)
  } else {
    df = li[[1]]
  }

  df
}

clicker.client.submit = function(values, app=getApp(), ct = app$glob[["ct"]]) {
  restore.point("clicker.client.submit")

  cat("\nclicker.client.submit")
  glob = app$glob
  userid = app$userid

  tag = basename(ct$tag.dir)

  # Values is a list with length equal to
  # the number of quiz parts
  # For multiple choice quizes each
  # list element is itself a list
  # containing the checked answers


  qu = first.non.null(ct[["wid"]],ct[["qu"]])

  df = clicker.make.submit.data(values=values,qu=qu,tag=tag, task.id=ct$task.id, userid=app$userid)

  if (!file.exists(file.path(ct$task.dir, "colnames.csv")))
    writeLines(paste0(colnames(df), collapse=","),file.path(ct$task.dir,"colnames.csv"))

  userhash = digest(userid)
  # we write as a csv table for security reasons
  # R binary files seem generaly more risky when loaded
  sub.file = file.path(ct$tag.dir, paste0(userhash,"_",sample.int(100000,1), ".sub"))
  write.table(df, file=sub.file, sep=",", row.names=FALSE, col.names= FALSE)
  glob$glob[["ct"]]$num.sub = ct$num.sub+1


  # Show wait message and disable
  # all inputs and buttons

  app$has.submitted = TRUE
  ns = NS(ct$task.id)
  wait = first.non.null(glob$page.params$submitted,"You have submitted your answer. Please wait until the next quiz starts.")
  setUI(paste0(qu$id,"-msgUI"), p(wait))

  # disable input
  js = '$(":input").prop("disabled", true); $("button:not(#loginNewBtn)").hide(); $("#loginNewBtn").prop("disabled", false);'
  evalJS(js)

}

set.global.observer = function(id, ..., obs=observe(...)) {
  li = getOption("global.shiny.observer", default=list())
  if (!is.null(li[[id]])) {
    try(li[[id]]$destroy())
  }
  li[[id]] = obs
  options(global.shiny.observer=li)
}

destroy.global.observers = function() {
  # destroy.global.observers()
  li = getOption("global.shiny.observer", default=list())
  for (obs in li) {
    try(obs$destroy())
  }
}




clicker.client.failed.login.ui = function(app=getApp()) {
  html="<h2>Login failed</h2>"
  HTML(html)
}
