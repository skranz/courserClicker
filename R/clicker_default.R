
get.server.cs = function(app=getApp()) {
  cs = app[["cs"]]
  if (is.null(cs)) {
    cs = as.environment(list())
    app$cs = cs
  }
  cs
}

# current clicker task for a certain task.id
get.server.ct = function(cs=get.server.cs(), app=getApp()) {
  cs$ct
}

set.server.ct = function(ct,cs=get.server.cs(), app=getApp()) {
  cs$ct = ct
}

clicker.server.ui.fun = function(...,wid,Wid=get.Widget(wid$type)) {
  fun = first.non.null(Wid$server$ui.fun,default.clicker.server.ui.fun)
  call.fun(fun,wid=wid, Wid=Wid,...)
}

clicker.server.init.ct = function(...,wid,Wid=get.Widget(wid$type)) {
  fun = first.non.null(Wid$server$init.ct,default.clicker.server.init.ct)
  call.fun(fun,wid=wid, Wid=Wid,...)
}


clicker.server.show.results = function(...,ct=NULL,wid=ct$wid,Wid=get.Widget(wid$type)) {
  restore.point("clicker.server.show.results")
  if (is.null(Wid$server$show.results)) {
    stop(paste0("Your clicker widget ", wid$type," has not implemented the function server$show.results!"))
  }

  call.fun(Wid$server$show.results,ct=ct, wid=wid,...)

  if (!is.null(wid$explain.html)) {
    ns = clicker.wid.ns(wid)
    ui = slimCollapsePanel("Explanation", HTML(wid$explain.html))
    setUI(ns("quizExplainUI"),ui)
    dsetUI(ns("quizExplainUI"),ui)
  }
}


clicker.server.start.ct = function(...,wid,Wid=get.Widget(wid$type)) {
  fun = first.non.null(Wid$server$start.ct,default.clicker.server.start.ct)
  call.fun(fun,wid=wid, Wid=Wid,...)
}

clicker.server.stop.ct = function(...,wid,Wid=get.Widget(wid$type)) {
  fun = first.non.null(Wid$server$stop.ct,default.clicker.server.stop.ct)
  call.fun(fun,wid=wid, Wid=Wid,...)
}


clicker.server.init.handlers = function(...,wid,Wid=get.Widget(wid$type)) {
  restore.point("clicker.server.init.handlers")

  fun = first.non.null(Wid$server$init.handler,default.clicker.server.init.handlers)
  call.fun(fun,wid=wid, Wid=Wid,...)
}



default.clicker.server.init.handlers = function(wid,ps=get.ps(), app=getApp(),opts=ps$opts, Wid = get.Widget(wid$type),...) {
  restore.point("default.clicker.server.init.handlers")

  ns = clicker.wid.ns(wid)
  if (!isTRUE(opts$use.clicker)) {
    cat("\nDon't use clicker.\n")
    return()
  }
  if (length(opts$clicker.dir)==0) {
    warning("No clicker.dir specified.\n")
    return()
  }
  if (nchar(opts$clicker.dir)==0) {
    warning("No clicker.dir specified.\n")
    return()
  }

  clicker.dir = opts$clicker.dir

  buttonHandler(ns("startClickerBtn"),function(...) {
    restore.point("startClickerBtn")
    clicker.server.start.ct(wid=wid,opts=opts)
  })
  buttonHandler(ns("stopClickerBtn"),function(...) {
    restore.point("stopClickerBtn")
    clicker.server.stop.ct(wid=wid, ct=get.server.ct(),...)
  })

  clicker.server.update.result.tags(wid=wid)

  selectChangeHandler(id = ns("resultsRunSelect"),fun=function(id,value,...,app=getApp()) {
    args = list(...)
    restore.point("resultsRunSelectChange")

    ct = wid$ct
    ct$clicker.tag = value
    ct$clicker.dir = clicker.dir
    ct$task.dir = file.path(clicker.dir, "tasks", ct$task.id)
    ct$tag.dir = file.path(ct$task.dir,"tags",ct$clicker.tag)

    clicker.server.show.results(wid = wid, ct=ct, clicker.tag=value, Wid=Wid)

    cat("\nresultsSelectClick")
  })
}

default.clicker.server.ui.fun = function(wid=NULL,task.id=wid$task.id,above.ui=wid$ui,ns=clicker.wid.ns(wid)
, stop.in=5) {


  tagList(
    above.ui,
    HTML("<table><tr><td>"),
    smallButton(ns("startClickerBtn"),label="Start", extra.style="margin-bottom: 2px;"),
    HTML("</td><td>"),
    smallButton(ns("stopClickerBtn"),label="Stop in ",extra.style="margin-bottom: 2px;", form.ids=ns("stopInInput")),
    HTML("</td><td>"),
    tags$input(id = ns("stopInInput"),type = "text", class = "form-control", value = stop.in,style="width: 4em; padding-left: 10px; padding-right: 5px; padding-top: 0; padding-bottom: 0; margin-left: 5px; margin-top:0; margin-bottom: 0; height: 100%;"),
    HTML("</td></tr></table>"),
    uiOutput(ns("numSubUI")),
    bsCollapse(open="Results",
      slimCollapsePanel("Results",
        uiOutput(ns("resultsUI")),
        tagList(div(class="StopClickPropagation",
          selectInput(ns("resultsRunSelect"), label="Results of run", choices=list("latest"="latest", "all"="all"),multiple=FALSE)
        )),
        uiOutput(ns("quizExplainUI"))
      )
    )
  )

}

default.clicker.server.stop.ct = function(wid,clicker.tag=NULL, cs=get.server.cs(),formValues=list(),...) {
  restore.point("default.clicker.server.stop.ct")

  ns = clicker.wid.ns(wid)
  stop.in.sec = as.integer(formValues[[ns("stopInInput")]])

  if (length(stop.in.sec)==0 | isTRUE(is.na(stop.in.sec))) stop.in.sec=3
  cs$stop.time = as.integer(Sys.time()) + stop.in.sec
}


default.clicker.server.start.ct = function(wid,clicker.tag=NULL, app=getApp(), opts = rt.opts(), Wid = get.Widget(wid$type), clicker.dir = opts$clicker.dir, cs=app$cs) {
  restore.point("default.clicker.server.start.ct")

  ns = clicker.wid.ns(wid)

  ct = clicker.server.init.ct(wid=wid,Wid=Wid, clicker.dir=clicker.dir, clicker.tag=clicker.tag)


  write.ct = as.list(ct)
  write.ct = write.ct[setdiff(names(write.ct),Wid$server.ct.nowrite.fields)]
  write.ct = write.clicker.task(ct=write.ct,clicker.dir=clicker.dir)

  ct = as.environment(ct)
  # set current clicker task
  set.server.ct(ct=ct)

  # start task observer
  cs = get.server.cs()

  cs$start.time = as.integer(Sys.time())
  cs$stop.time = NULL
  cs$stopped = FALSE

  if (!is.null(cs[["task.obs"]])) {
    try(cs$task.obs$destroy())
  }

  cs$task.obs = observe({
    app=getApp()
    restore.point("task.observer")

    dir = ct$tag.dir
    files = list.files(dir)
    cs$num.sub = max(0,length(files))
    if (!is.null(cs$stop.time)) {
      cs$stop.in.sec = round(cs$stop.time - as.integer(Sys.time()))
      cs$stopped = cs$stop.in.sec < 0
      if (!cs$stopped) {
        stop.tag = p(style="color: #d00",paste0("Stop in ", cs$stop.in.sec, " sec."))
      } else {
        stop.tag = p("Submission has stopped.")
      }
    } else {
      stop.tag = NULL
    }
    setUI(ns("numSubUI"),tagList(
      stop.tag,
      p(paste0("Running: ", round(as.integer(Sys.time())-cs$start.time))," sec."),
      p(paste0("Replies: ", cs$num.sub))
    ))
    if (!cs$stopped) {
      invalidateLater(1000)
    } else {
      setUI(ns("numSubUI"),"")
      clicker.server.update.result.tags(task.id=ct$task.id,clicker.dir=ct$clicker.dir, selected = "latest")
      clicker.server.show.results(wid=wid, ct=ct, Wid=Wid)
    }
  })

  ct
}


default.clicker.server.init.ct = function(wid, clicker.dir, clicker.tag=NULL,Wid = get.Widget(wid$type), opts=rt.opts(),...) {
  ct = wid$ct
  ct$clicker.dir = clicker.dir
  ct$client.init.handlers = Wid$client$init.handlers

  if (is.null(clicker.tag)) {
    clicker.tag = make.clicker.tag(ct = ct)
  }
  ct$clicker.tag = clicker.tag
  ct$task.dir = file.path(clicker.dir, "tasks", ct$task.id)
  ct$tag.dir = file.path(ct$task.dir,"tags",clicker.tag)

  ct

}


clicker.server.update.result.tags = function(task.id = wid$task.id,selected="none",clicker.dir=opts$clicker.dir,  app=getApp(), opts=rt.opts(),wid=NULL) {
  restore.point("clicker.server.update.result.tags")

  ns = clicker.wid.ns(wid)

  tags = get.clicker.tags(clicker.dir=clicker.dir, task.id=task.id)

  tags.li = as.list(c("none", "latest", "all",tags))
  names(tags.li) = unlist(tags.li)

  updateSelectizeInput(app$session,inputId = ns("resultsRunSelect"), choices=tags.li,selected = selected)
}

clicker.wid.ns = function(wid, task.id=wid$task.id) {
  restore.point("clicker.wid.ns")
  ns = first.non.null(wid[["ns"]],NS(wid$task.id))
  ns
}
