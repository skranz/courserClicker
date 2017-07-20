examples.manual.clicker.quiz = function() {
  restore.point.options(display.restore.point=TRUE)
  app = eventsApp()
  clicker.dir = "D:/libraries/courser/courses/vwl/course/clicker"
  cc = init.custom.clicker.quiz(clicker.dir = clicker.dir)
  cc$ui=custom.clicker.quiz.ui(cc)

  ps = list(Widgets=list(quiz=rtutor.widget.quiz()))
  set.ps(ps)

  custom.clicker.quiz.init.handlers(cc)
  app$ui = fluidPage(
    #enter.sc.mc.quiz.ui()
    cc$ui
  )
  viewApp(app)
}

init.custom.clicker.quiz = function(clicker.dir, templ.dir = if (!is.null(clicker.dir)) file.path(clicker.dir,"templates"), lang= "en") {
  restore.point("init.custom.clicker.quiz")
  cc = list(
    lang=lang,
    clicker.dir=clicker.dir,
    templ.dir = templ.dir,
    templates = read.quiz.templates(templ.dir=templ.dir, lang=lang),
    ns = NS("CustomClickerQuiz")
  )
  task.dir = file.path(clicker.dir,"tasks")
  tasks = list.dirs(task.dir,full.names = FALSE,recursive = FALSE)
  custom.tasks = tasks[str.starts.with(tasks,"custom__")]
  custom.tasks = str.right.of(custom.tasks,"custom__")
  cc$custom.tasks = custom.tasks

  # set clicker.dir
  opts = rt.opts()
  opts$clicker.dir = clicker.dir
  set.rt.opts(opts)

  as.environment(cc)
}

custom.clicker.templ.click = function(formValues,cc, ...) {
  restore.point("templBtnClick")
  ns = cc$ns
  templ.name = formValues[[1]]
  cc$qu = qu = cc$templates[[ templ.name ]]


  cc$mode = "template"

  task.dir = file.path(cc$clicker.dir,"tasks")
  cfiles = list.files(task.dir,glob2rx("custom__*"), include.dirs=TRUE)

  qu$name = paste0(templ.name,"_", length(cfiles)+1)
  ui = tagList(
    enter.sc.mc.quiz.ui(qu=qu),
    custom.quiz.clicker.server.ui.fun(cc=cc)
  )
  setUI(ns("quizUI"), ui)
  dsetUI(ns("quizUI"), ui)
}

custom.clicker.old.quiz.click = function(formValues,cc, ...) {
  restore.point("old.quiz.click")
  ns = cc$ns
  task.id = paste0("custom__", formValues[[ns("oldQuiz")]])
  task.file = file.path(cc$clicker.dir,"tasks", task.id, "ct.Rds")
  ct = readRDS(task.file)

  wid = ct$wid

  cc$qu = qu = wid
  cc$mode = "template"

  #qu$name = paste0(templ.name,"_", length(cfiles)+1)
  ui = tagList(
    enter.sc.mc.quiz.ui(qu=qu),
    custom.quiz.clicker.server.ui.fun(cc=cc)
  )
  setUI(ns("quizUI"), ui)
  dsetUI(ns("quizUI"), ui)

}


custom.clicker.start.click = function(formValues,cc, ...) {
  restore.point("custom.clicker.start.click")
  cc$wid = parse.custom.quiz.widget(formValues=formValues, cc=cc)
  clicker.server.start.ct(wid=cc$wid)
  cc$ct = get.server.ct()
  cc$ct$wid$ns = cc$ns
  cc$ct$wid$task.id = cc$ct$task.id
}


custom.clicker.update.click = function(formValues,cc, ...) {
  restore.point("custom.clicker.update.click")
  cc$wid = parse.custom.quiz.widget(formValues=formValues, cc=cc)
  clicker.server.start.ct(wid=cc$wid, just.update.ct=TRUE)
  cc$ct = get.server.ct()
  cc$ct$wid$ns = cc$ns
  cc$ct$wid$task.id = cc$ct$task.id
}


custom.clicker.stop.click = function(formValues,cc, ...) {
  restore.point("custom.clicker.stop.click")
  clicker.server.stop.ct(wid=cc$wid, ct=get.server.ct(),formValues=formValues,...)
}

custom.clicker.quiz.init.handlers = function(cc) {
  ns = cc$ns
  clicker.dir = cc$clicker.dir
  buttonHandler(ns("templBtn"), function(...) {
    custom.clicker.templ.click(..., cc=cc)
  })
  buttonHandler(ns("oldQuizBtn"), function(...) {
    custom.clicker.old.quiz.click(..., cc=cc)
  })

  buttonHandler(ns("startClickerBtn"), function(...) {
    custom.clicker.start.click(..., cc=cc)
  })

  buttonHandler(ns("updateClickerBtn"), function(...) {
    custom.clicker.update.click(..., cc=cc)
  })


  buttonHandler(ns("stopClickerBtn"), function(...) {
    custom.clicker.stop.click(..., cc=cc)
  })


  selectChangeHandler(id = ns("resultsRunSelect"),fun=function(id,value,...,app=getApp()) {
    args = list(...)
    ct = cc$ct
    restore.point("customResultsRunSelectChange")

    ct$clicker.tag = value
    ct$clicker.dir = clicker.dir
    ct$task.dir = file.path(clicker.dir, "tasks", ct$task.id)
    ct$tag.dir = file.path(ct$task.dir,"tags",ct$clicker.tag)

    clicker.server.show.results(wid = ct$wid, ct=ct, clicker.tag=value)

    cat("\nresultsSelectClick")
  })

}

custom.clicker.quiz.ui= function(cc) {
  restore.point("custom.clicker.quiz.ui")
  ns = cc$ns
  ui = tagList(
    HTML("<table><tr><td>"),
    selectInput(ns("template"),"",choices = names(cc$templates)),
    submitButton(ns("templBtn"),"New quiz from template",form.ids = ns("template")),
    HTML("</td><td style='padding-left: 20px'>"),
    selectInput(ns("oldQuiz"),"", choices=cc$custom.tasks),
    submitButton(ns("oldQuizBtn"),"Show Previous Quiz", form.ids = ns("oldQuiz")),
    HTML("</td></tr></table>"),
    uiOutput(ns("quizUI"))
  )
  ui
}

read.quiz.templates = function(lang="en", templ.dir = NULL) {
  file = paste0(lang,"_quiz_templates.yaml")
  lib.dir = system.file("templates", package = "courserClicker")


  # english is fallback language
  if (!file.exists(file.path(lib.dir, file))) {
    if (is.null(templ.dir)) {
      file = paste0("en_quiz_templates.yaml")
    } else if (!file.exists(file.path(templ.dir,file))) {
      file = paste0("en_quiz_templates.yaml")
    }

  }
  templ= list()
  lib.file = file.path(lib.dir, file)

  if (file.exists(lib.file))
    templ=read.yaml(file=lib.file)

  if (!is.null(templ.dir)) {
    man.file = file.path(templ.dir, file)
    if (file.exists(man.file)) {
      man.templ=read.yaml(file=man.file)
      templ[names(man.templ)] = man.templ
    }
  }

  qu.li = lapply(templ, function(te) {
    shinyQuiz(qu=te, lang=lang)
  })
  qu.li
}

enter.sc.mc.quiz.ui = function(qu=list(name="",parts=list(list(question="Enter your answer",choices = c("A","B","C","D"), type="sc"))),id=first.non.null(qu$id,paste0("manual_quiz_", random.string(1,8))), ns=NS("CustomClickerQuiz")) {

  restore.point("enter.sc.mc.quiz.ui")
  part = qu$parts[[1]]

  answer.ind = first.non.null(part[["answer_ind"]],"")

  typeLabels = c(sc="single choice",mc="multiple choice",numeric="free number")
  tagList(
    div(style="display: flex",
      textInput(ns("name"),"Quiz name",qu$name),
      div(style="padding-left: 20px",textAreaInput(ns("question"),"Question",part$question))
    ),
    div(style="display: flex",
      lapply(seq_along(part$choices),function(i) {
        textInput(ns(paste0("choice_",i)),paste0("Choice ",i),part$choices[[i]])
      })
    ),
    passwordInput(ns("answer_ind"),label=paste0("Solution (",typeLabels[part$type],")"),value=answer.ind)
  )
}


parse.custom.quiz.widget = function(formValues, cc,...) {
  restore.point("parse.custom.quiz.widget")
  ns = cc$ns
  qu = cc$qu
  part=qu$parts[[1]]

  vals=formValues
  choice.fields = ns(paste0("choice_",seq_along(part$choices)))
  choices = unlist(vals[choice.fields])
  answer.ind = list.string.to.vector(vals[[ns("answer_ind")]], class="integer")
  answer.ind = as.integer(na.omit(answer.ind))

  new.qu = list(
    name = vals[[ns("name")]],
    question = vals[[ns("question")]],
    answer.ind = answer.ind
  )
  # Works for sc and mc quiz
  if (part$type == "sc" | part$type == "mc")
    new.qu[[part$type]] = choices

  task.id = paste0("custom__quiz__",vals[[ns("name")]])
  qu = shinyQuiz(id = task.id,qu = new.qu, bdf = NULL,add.handler = FALSE, add.check.btn=FALSE,lang = cc$lang)
  client.ui = quiz.clicker.client.ui(qu, lang=cc$lang)

  qu$ct = list(
    type = "quiz",
    task.id = task.id,
    client.ui = client.ui,
    wid = qu,
    lang=cc$lang
  )
  qu$ns = cc$ns
  qu$type = "quiz"
  qu
}


custom.clicker.quiz.form.ids = function(part=qu$parts[[1]],qu=cc$qu, cc=NULL) {
  ns = cc$ns
  c(ns("name"),ns("question"),ns(paste0("choice_",seq_along(part$choices))),ns("answer_ind"))
}

custom.quiz.clicker.server.ui.fun = function(cc=NULL, qu=cc$qu,above.ui=NULL,ns=NS("CustomClickerQuiz"), stop.in=5) {
  restore.point("custom.quiz.clicker.server.ui.fun")
  form.ids=custom.clicker.quiz.form.ids(cc=cc,qu=qu)

  tagList(
    above.ui,
    HTML("<table><tr><td>"),
    smallButton(ns("updateClickerBtn"),label="Update", extra.style="margin-bottom: 2px;", form.ids=form.ids),
    smallButton(ns("startClickerBtn"),label="Start", extra.style="margin-bottom: 2px;", form.ids=form.ids),
    HTML("</td><td>"),
    smallButton(ns("stopClickerBtn"),label="Stop in ",extra.style="margin-bottom: 2px;",form.ids=ns("stopInInput")),
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
