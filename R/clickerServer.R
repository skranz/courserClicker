examples.click.server = function() {
  clicker.dir = "D:/libraries/shinyEventsClicker/apps/clickerapp"
  app = clickerServerApp(clicker.dir=clicker.dir,userid="skranz")
  viewApp(app)

}


# clickerServerApp = function(clicker.dir, template.dir=file.path(clicker.dir, "templates"), userid="default") {
#   restore.point("clickerServerApp")
#
#   library(shinyAce)
#   app = eventsApp()
#
#   cs = as.environment(nlist(
#     clicker.dir, template.dir
#   ))
#   app$cs = cs
#   cs$templ = load.clicker.quiz.templates(cs=cs)
#
#   headerBox = tagList(
#
#   )
#
#   mainBox = div(
#     aceEditor("quizText", value=cs$templ[[1]]$source_text,showLineNumbers = FALSE,wordWrap = TRUE,mode = "yaml",height = "10em"),
#     uiOutput("msgUI"),
#     HTML("<table><tr><td>"),
#     smallButton("sendBtn",label="Send"),
#     HTML("</td><td>"),
#     smallButton("stopBtn",label="Stop in "),
#     HTML("</td><td>"),
#     tags$input(id = "stopSecInput",type = "text", class = "form-control", value = "2",style="width: 4em; padding-left: 5px; padding-right: 5px; padding-top: 0; padding-bottom: 0; margin-left: 2px; margin-top:0; margin-bottom: 0;"),
#     HTML("</td></tr></table>")
#   )
#
#   resultBox = tagList(
#     uiOutput("taskNumSubUI"),
#     uiOutput("resultsUI")
#   )
#
#   manageBox = tagList(
#     tabsetPanel(
#       tabPanel("Send Links",
#         textAreaInput("linkEmailArea",label="Emails",cols = 8),
#         smallButton("sendLinksBtn","Send Links")
#       )
#     )
#   )
#
#   app$ui =dashboardPage(
#     dashboardHeader(title = "ShinyClicker"),
#     dashboardSidebar(
#       selectInput("selCourse", label="Course",choices=NULL),
#       textInput("newCourse",label="New Course",value = ""),
#       actionButton("addCourseBtn","Add course"),
#       selectInput("selTempl", label="Template", choices = names(cs$templ))
# ),
#     dashboardBody(
#       fluidRow(
#         box(mainBox),
#         box(resultBox),
#         box(manageBox)
#       )
#     )
#   )
#
#
#
#   selectChangeHandler("selTempl",function(app,...) {
#     cs = app$cs
#     te = getInputValue("selTempl")
#     if (nchar(te)==0) return()
#     templ = cs$templ[[te]]
#     updateAceEditor(app$session, "quizText",value=templ$source_text)
#   })
#   buttonHandler("sendBtn", function(app,...) {
#     yaml = getInputValue("quizText")
#     parse.and.send.quiz.task(yaml)
#   })
#   buttonHandler("stopBtn",function(cs=app$cs, app,...) {
#     stop.in.sec = as.integer(getInputValue("stopSecInput"))
#     restore.point("stopBtnHandler")
#
#     if (is.na(stop.in.sec)) stop.in.sec=3
#     cs$stop.time = as.integer(Sys.time()) + stop.in.sec
#   })
#
#   appInitHandler(function(...,app=getApp()) {
#     cs = as.environment(as.list(app$cs))
#     app$cs = cs
#     init.app.presenter(userid=userid,cs=cs)
#
#   })
#   app
# }
#
#
# init.app.presenter = function(userid, cs = app$cs, app=getApp()) {
#   restore.point("init.app.presenter")
#   cs$userid = userid
#   cs$user.dir = file.path(cs$clicker.dir, "presenters",userid)
#   if (!dir.exists(cs$user.dir))
#     try(dir.create(cs$user.dir))
#
# }
#
# parse.and.send.quiz.task = function(yaml, app=getApp()) {
#   restore.point("parse.and.send.quiz.task")
#
#   # for security don't offer all fields
#   qu = yaml.load(yaml)
#   fields = c("question","sc","mc","answer")
#   qu = qu[fields]
#
#   cs = app$cs
#   ct = try(as.environment(clickerQuiz(qu=qu)))
#   if (is(ct,"try-error")) {
#     setUI("msgUI",p(as.character(ct)))
#     return()
#   } else {
#     setUI("msgUI",p(paste0("Task sent to users. TaskId: ", ct$task.id)))
#   }
#
#   cs$ct = ct
#   write.clicker.task(ct=ct, clicker.dir=app$cs$clicker.dir,cs=cs)
#   start.server.task.observer(cs=cs)
# }
#
# load.clicker.quiz.templates = function(template.dir = cs$template.dir, cs=NULL) {
#   restore.point("load.clicker.quiz.templates")
#
#   dir = template.dir[[1]]
#   for (dir in template.dir) {
#     files = list.files(path = dir, pattern = glob2rx("*.yaml"),full.names = TRUE)
#     li = lapply(files, function(file) {
#       res = NULL
#       try(res<-import.yaml.with.source(file=file, add.head=FALSE))
#       res
#     })
#     li = do.call(c, li)
#   }
#   li
# }

# start.server.task.observer = function(ct=cs$ct,cs=app$cs,app=getApp()) {
#   restore.point("server.task.observer")
#
#   cs$start.time = as.integer(Sys.time())
#   cs$stop.time = NULL
#   cs$stopped = FALSE
#
#   if (!is.null(cs[["task.obs"]])) {
#     try(cs$task.obs$destroy())
#   }
#
#   cs$task.obs = observe({
#     app=getApp()
#     cs = app$cs
#     restore.point("task.observer")
#
#     tag.dir = file.path(ct$clicker.dir, "tags",ct$task.id,"tags", ct$clicker.tag)
#     files = list.files(dir)
#     cs$num.sub = max(0,length(files)-1)
#     if (!is.null(cs$stop.time)) {
#       cs$stop.in.sec = round(cs$stop.time - as.integer(Sys.time()))
#       cs$stopped = cs$stop.in.sec < 0
#       if (!cs$stopped) {
#         stop.tag = p(style="color: #d00",paste0("Stop in ", cs$stop.in.sec, " sec."))
#       } else {
#         stop.tag = p("Submission has stopped.")
#       }
#     } else {
#       stop.tag = NULL
#     }
#     setUI("taskNumSubUI",tagList(
#       stop.tag,
#       h4(paste0("Running: ", round(as.integer(Sys.time())-cs$start.time))),
#       h4(paste0("Replies: ", cs$num.sub))
#     ))
#     if (!cs$stopped) {
#       invalidateLater(1000)
#     } else {
#       show.task.results()
#     }
#   })
# }

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
