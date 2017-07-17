# clicker app modes
#
# login: show.course.list, show.course.code
# running:


clicker.client.example = function() {
  restore.point.options(display.restore.point = TRUE)

  clicker.dir = "D:/libraries/courser/courses/vwl/course/clicker"
  setwd(clicker.dir)
  port = 4634
  app.url = "127.0.0.1:4634"
  app.url = "http://localhost:4634"
  opts = clicker.client.opts(clicker.dir=clicker.dir, app.url=app.url, show.course.list = FALSE, show.course.code=FALSE, use.token=FALSE, use.login.db=FALSE, app.title="Vorlesungsquiz VWL", lang="de")
  app = clickerClientApp(opts)
  viewApp(app, port=port)
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
  glob$page.params = make.clicker.page.params(opts$page.params, lang=opts[["lang"]])

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
    uiOutput("titleUI"),
    uiOutput("mainUI")
  )
  appInitHandler(function(...,session=app$session,app=getApp()) {
    glob = app$glob
    task.update.rv = glob$task.update.rv
    # we must observe the query string to perform initialization
    app$init.observer = observe(priority = -100,x = {
      if (isTRUE(app$is.initialized)) {
        app$init.observer$destroy()
        return()
      }
      app$is.initialized = TRUE
      query <- parseQueryString(session$clientData$url_search)
      restore.point("clicker.client.observe.query")
      clicker.client.init.app.with.query(query)
    })

  })
  app
}

clicker.client.init.app.with.query = function(query, app=getApp()) {
  restore.point("clicker.client.init.app.with.query")
  glob = app$glob
  mainUI = glob$mainUI

  # check if we have a token
  key = query$token_key
  if (!is.null(query$token_key)) {
    clicker.client.init.app.with.token(token_key=query$token_key, token_file=query$token_file)
    return()
  }

  # use normal login dispatch
  lop = get.lop(app=app)
  initLoginDispatch(lop)
}

clicker.client.init.app.with.token = function(token_key, token_file, app=getApp()) {
  restore.point("clicker.client.init.app.with.token")

  # check if token file exists
  failed.ui = clicker.client.failed.login.ui()
  file = file.path(glob$token.dir,token_file)
  if (!file.exists(file)) {
    setUI(mainUI, failed.ui)
    return()
  }

  tok = read.login.token(file=file)

  now = as.numeric(Sys.time())
  if (!isTRUE(now <= tok$expire)) {
    html="<h2>Timout. Your login token is not active anymore. Please login again.</h2>"
    setUI(mainUI, HTML(html))
    return()
  }
  clicker.client.start.task.observer(tok=tok)
}

clicker.client.opts = function(
  clicker.dir=getwd(),
  db.dir = file.path(clicker.dir,"db"),
  running.dir = file.path(clicker.dir,"running"),
  token.dir = file.path(clicker.dir, "tokens"),
  need.token = FALSE,
  just.login = FALSE,
  use.token = TRUE,
  use.login.db=TRUE,
  app.url=NULL, clicker.app.url = app.url,
  init.userid="", init.password="", init.code="",
  app.title="Quiz",
  email.domain = NULL, check.email.fun = NULL,
  email.text.fun=default.email.text.fun,
  smtp = NULL,
  token.valid.min = 180,
  allow.guest.login=TRUE, open.app.in.new.tab=TRUE,
  show.course.list=FALSE, show.course.code=FALSE,
  lang="en",
  page.params=list(title=app.title),
  ...)
{
  c(as.list(environment()),list(...))
}


.glob.obs.env = new.env()

clicker.client.start.task.observer = function(tok=app$glob$default.token,app=getApp()) {
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

  if (is.null(ct)) {
    if (!isTRUE(app$no.clicker.task)) {
      app$no.clicker.task = TRUE
      ui = clicker.client.wait.page(app$glob$page.params)
      setUI(app$glob$mainUI, ui)
    }
    return()
  }
  app$no.clicker.task = FALSE

  if (is.function(ct$client.init.handlers)) {
    ct$client.init.handlers(ct)
  } else if (is.character(ct$client.init.handlers)) {
    do.call(ct$client.init.handlers, list(ct=ct))
  }
  setUI(app$glob$mainUI, ct$client.ui)
}

clicker.update.task = function(clicker.dir, glob=app$glob, app=getApp(), millis=1000) {
  restore.point("clicker.update.task")
  cat(".")
  #cat("\nI am observed...", sample.int(1000,1))

  files = list.files(file.path(clicker.dir, "running_task"),full.names = FALSE)

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


clicker.client.submit = function(values, app=getApp(), ct = app$glob[["ct"]]) {
  restore.point("clicker.client.submit")

  cat("\nclicker.client.submit")
  glob = app$glob
  userid = app$userid

  ct = glob[["ct"]]
  tag = basename(ct$tag.dir)

  # Values is a list with length equal to
  # the number of quiz parts
  # For multiple choice quizes each
  # list element is itself a list
  # containing the checked answers

  submit.time = Sys.time()

  qu = ct$wid
  li = vector("list", length(qu$parts))
  value.i = 1
  i = 1
  for (i in seq_along(li)) {
    part = qu$parts[[i]]
    if (part$type == "grid_mc" | part$type == "grid_sc") {
      value.i = (value.i:(value.i+length(part$rows)-1))
      answers = unlist(values[value.i])
      answer.ind = match(answers, part$cols)
      li[[i]] = data_frame(submit.time=submit.time,task.id=ct$task.id, tag=tag, part.ind=i*1000+seq_along(answers), userid=app$userid, answer.ind=answer.ind, answer =  answers)

    } else {
      answers = unlist(values[[value.i]])
      answer.ind = match(answers, part$answer)
      li[[i]] = data_frame(submit.time=submit.time,task.id=ct$task.id, tag=tag, part.ind=i, userid=app$userid, answer.ind=answer.ind, answer =  answers)
    }
  }
  if (length(li)>1) {
    df = bind_rows(li)
  } else {
    df = li[[1]]
  }

  if (!file.exists(file.path(ct$task.dir, "colnames.csv")))
    writeLines(paste0(colnames(df), collapse=","),file.path(ct$task.dir,"colnames.csv"))

  # we write as a csv table for security reasons
  # R binary files seem generaly more risky when loaded
  sub.file = file.path(ct$tag.dir, paste0(userid,".sub"))
  write.table(df, file=sub.file, sep=",", row.names=FALSE, col.names= FALSE)
  glob$glob[["ct"]]$num.sub = ct$num.sub+1

  ui= clicker.client.submitted.page(glob$page.params, answer=values$answer)
  setUI(glob$mainUI,ui)
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
