rtutor.widget.quiz = function() {
  Wid = list(
    parse.fun = quiz.clicker.parse,
    server = list(
      init.handlers = "default.clicker.server.init.handlers",
      init.ct = NULL,
      start.ct = NULL,
      load.sub.data = NULL,
      transform.sub.data = "quiz.clicker.transform.sub.data",
      ui.fun = "quiz.clicker.server.ui.fun",
      show.results = "quiz.clicker.server.show.results"
    ),
    client = list(
      init.handlers = "quiz.clicker.client.init.handlers",
      init.ct = NULL,
      init.cc = NULL,
      ui.fun = NULL
    )
  )
  Wid$ui.fun = Wid$server$ui.fun
  Wid$init.handlers =Wid$server$init.handlers
  Wid
}

quiz.clicker.server.ui.fun = function(wid,ct=NULL,...,app=getApp(), opts=rt.opts()) {
  default.clicker.server.ui.fun(wid=wid, above.ui=wid$ui, stop.in=first.non.null(opts$clicker.stop.in,5))
}

quiz.clicker.server.show.results = function(ct, ...) {
  show.quiz.task.results(ct=ct,...)
}

quiz.clicker.parse = function(inner.txt,type="quiz",name="",id= paste0("quiz_",args$name),args=NULL, bdf=NULL, bi=NULL, ps=get.ps(),opts = ps$opts,...) {

  task.id = paste0(ps$name,"__",if(is.null(args[["name"]])) id else paste0("quiz_",args$name))
  restore.point("quiz.clicker.parse")
  whiskers =NULL
  if (isTRUE(opts$use.whiskers)) {
    whiskers = ps$pre.env$.whiskers
  }

  qu = shinyQuiz(id = task.id,yaml = merge.lines(inner.txt), bdf = NULL,add.handler = FALSE, whiskers=whiskers, add.check.btn=FALSE)
  client.ui = quiz.clicker.client.ui(qu, lang=opts[["lang"]])

  qu$ct = list(
    type = "quiz",
    task.id = task.id,
    client.ui = client.ui,
    wid = qu,
    lang=opts[["lang"]]
  )
  qu
}


quiz.clicker.client.ui = function(qu, lang="en") {
  restore.point("quiz.clicker.client.ui")
  pli = lapply(seq_along(qu$parts), function(i) {
    restore.point("quiz.clicker.client.ui")
    part = qu$parts[[i]]
    part.ui = quiz.clicker.client.part.ui(part)
    if (i < length(qu$parts)) {
      hr = hr()
    } else {
      hr = NULL
    }
    return(list(part.ui,hr))
  })
  if (!is.null(qu$checkBtnId)) {
    ids = unlist(lapply(qu$parts, function(part) {
      if (part$type == "grid_sc" | part$type == "grid_mc") {
        return(paste0(part$answerId, seq_len(length(part$rows))))
      }
      part$answerId
    }))

    sendLabel = clicker.client.sendBtnLabel(lang)
    pli = c(pli, list(submitButton(qu$checkBtnId,label = sendLabel,form.ids = ids),br()))
  }

  pli=tagList(pli,uiOutput(paste0(qu$id, "-msgUI")))
  withMathJaxNoHeader(pli)
}

quiz.clicker.client.part.ui = function(part) {
  restore.point("quiz.clicker.client.part.ui")

  head = list(
    HTML(part$question.html)
  )
  if (part$type=="numeric") {
    answer = textInput(part$answerId, label = NULL,value = "")
  } else if (part$type =="text") {
    answer = textInput(part$answerId, label = NULL,value = "")
  } else if (part$type=="mc") {
    answer = wellCheckboxGroupInput(part$answerId, label=NULL,part$choices)
  } else if (part$type=="sc") {
    answer = wellRadioButtons(part$answerId, label=NULL,part$choices, selected=NA)
  } else if (part$type=="grid_sc" | part$type=="grid_mc") {
    answer = part$ui[[2]]
  } else {
    stop("unknown quiz part$type ", part$type)
  }
  list(head,answer,uiOutput(part$resultId))
}

quiz.clicker.client.init.handlers = function(ct=NULL,qu=ct$wid){
  restore.point("quiz.clicker.client.init.handlers")
  buttonHandler(qu$checkBtnId, function(formValues,...) {
    restore.point("clicker.click.send.btn")
    clicker.client.submit(values=formValues)
  })
}

