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
  opts = clicker.client.opts(clicker.dir=clicker.dir, app.url=app.url, show.course.list = TRUE, show.course.code=FALSE, use.token=FALSE, use.login.db=FALSE)
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
  glob$courses = list()
  glob$running.task.file = NULL

  lop = clicker.client.lop(glob)
  set.lop(lop)

  if (!dir.exists(glob$token.dir)) (
    stop(paste0("token.directory ", glob$token.dir, " does not exist."))
  )

  # list of reactive nounces that monitor updates
  # for given courses
  glob$update.task.rv = reactiveValues(nonce=0)

  obs = observe({
    clicker.update.task(clicker.dir = clicker.dir,glob = glob)
  })
  set.global.observer("myobs",obs=obs)

  app$ui = bootstrapPage(
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
  app.title="RClicker",
  email.domain = NULL, check.email.fun = NULL,
  email.text.fun=default.email.text.fun,
  smtp = NULL,
  token.valid.min = 180,
  allow.guest.login=TRUE, open.app.in.new.tab=TRUE,
  show.course.list=FALSE, show.course.code=TRUE,
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

  glob=app$glob
  app$task.obs = observe({
    glob$update.task.rv$nonce
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
      ui = tagList(
        h4("Clicker - ", app$userid),
        p("Currently no task.")
      )
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

  ui = tagList(
    h4("Clicker - ", app$userid),
    ct$client.ui
  )
  setUI(app$glob$mainUI, ui)
}

clicker.update.task = function(clicker.dir, glob=app$glob, app=getApp(), millis=1000) {
  restore.point("clicker.update.task")
  cat(".")
  #cat("\nI am observed...", sample.int(1000,1))

  files = list.files(file.path(clicker.dir, "running_task"),full.names = TRUE)

  files = setdiff(files, glob$running.task.file)
  if (length(files)==0) {
    invalidateLater(millis)
    return()
  }
  # sort filest by date, newest first
  files = files[order(file.mtime(file.path(clicker.dir,"tasks",files)))]
  if (length(files)>1) {
    file.remove(files[-1])
  }

  restore.point("clicker.update.task.2")

  file = basename(files[1])
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


clicker.client.submit = function(values, app=getApp()) {
  restore.point("clicker.client.submit")

  cat("\nclicker.submit")
  glob = app$glob
  userid = app$userid

  ct = glob[["ct"]]
  vals = c(list(submitTime=Sys.time(), userid=app$userid), as.list(values))

  if (!file.exists(file.path(ct$task.dir, "colnames.csv")))
    writeLines(paste0(names(vals), collapse=","),file.path(ct$task.dir,"colnames.csv"))

  sub.file = file.path(ct$tag.dir, paste0(userid,".sub"))
  write.table(as.data.frame(vals), file=sub.file, sep=",", row.names=FALSE, col.names= FALSE)
  glob$glob[["ct"]]$num.sub = ct$num.sub+1

  setUI(glob$mainUI,p("Your answer is submitted."))
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
