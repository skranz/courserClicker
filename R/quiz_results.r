show.quiz.task.results = function(ct, app=getApp(),outputId = ns("resultsUI"),clicker.tag=ct$clicker.tag,ns=clicker.wid.ns(ct$wid,task.id = ct$task.id),...) {
  restore.point("show.quiz.task.results")
  dat = load.sub.data(ct=ct, clicker.tag=clicker.tag)

  if (is.null(dat)) {
    ui = p("No answers submitted.")
    setUI(outputId,ui)
    return()
  }

  qu = ct$wid

  num.parts = length(qu$parts)
  if (num.parts > 1) {
    parts.outputId = paste0(outputId,"-",seq_along(qu$parts))
    ui = lapply(seq_along(qu$parts), function(i) {
      uiOutput(parts.outputId[[i]])
    })
    setUI(outputId, tagList(ui))
  } else {
    parts.outputId = outputId
  }

  for (i in seq_along(qu$parts)) {
    curId = parts.outputId[[i]]
    part = qu$parts[[i]]
    if (isTRUE(part$type=="sc")) {
      show.clicker.quiz.sc.results(dat=dat,qu=qu,part.ind=i, outputId = curId)
    } else if (isTRUE(part$type=="mc")) {
      show.clicker.quiz.mc.results(dat=dat,qu=qu,part.ind = i, outputId = curId)
    } else if (isTRUE(part$type=="numeric")) {
      show.clicker.quiz.numeric.results(dat=dat,part.ind = i, qu=qu,outputId = curId)
    } else if (isTRUE(part$type=="grid_sc")) {
      show.clicker.quiz.grid.sc.results(dat=dat,part.ind = i, qu=qu,outputId = curId)
    }
  }
  return()
}

show.clicker.quiz.grid.sc.results = function(dat, qu,part.ind = 1, part = qu$parts[[part.ind]], show.sol=TRUE, outputId = NULL, opts=NULL,do.plot=TRUE, app=getApp()) {
  restore.point("show.clicker.quiz.grid.sc.results")
  outputIds = paste0(outputId,"-",seq_along(qu$rows))
  ui.li = lapply(seq_along(qu$rows), function(i) {
    tagList(
      p(paste0(part$rows[[i]],":")),
      uiOutput(outputIds[[i]])
    )
  })
  setUI(outputId, tagList(ui.li))

  for (i in seq_along(qu$rows)) {
    curId = outputIds[[i]]
    cur.part.ind = part.ind*1000+i
    show.clicker.quiz.sc.results(dat=dat,qu=qu,part.ind=cur.part.ind, outputId = curId,part=part, show.header = FALSE, answer=part$answers[[i]])
  }

}

show.clicker.quiz.sc.results = function(dat, qu,part.ind = 1, part = qu$parts[[part.ind]], show.sol=TRUE, outputId = NULL, opts=NULL,do.plot=TRUE, app=getApp(), show.header=TRUE, answer = if (show.sol) part$answer else NULL) {
  restore.point("clicker.quiz.sc.results.ui")

  dat = dat[dat$part.ind==part.ind,,drop=FALSE]

  choices = unlist(part$choices)
  has.mathjax = any(has.substr(choices,"\\("))
  if (is.na(do.plot)) do.plot = !has.mathjax

  counts = count.choices(dat$answer.ind, seq_along(choices))
  names(counts) = choices
  shares = round(100*counts / max(1,sum(counts)))



  nans = NROW(dat)

  if (do.plot) {


    choice.labels = choices
    if (show.sol) {
      rows = choices == part$answer
      choice.labels[rows] = paste0("*", choice.labels[rows])
    }

    plot = choices.barplot(values=choices[dat$answer.ind], choices, answer=answer, choice.labels=choice.labels)
    # need random string to correctly rerender plot
    plotId = paste0(outputId,"_Plot_",random.string(nchar=8))
    ui = tagList(
      if (show.header) tagList(HTML(part$question), p(paste0("(",nans," replies)"))),
      div(style="height=14em",
        highchartOutput(plotId, height="14em")
      )
    )
    setUI(outputId,ui)
    dsetUI(outputId,ui)
    app$session$output[[plotId]] = renderHighchart(plot)
  # show a table
  } else {
    n = length(choices)
    bg.color = rep("#fff",n)
    if (show.sol) {
      rows = choices == part$answer
      bg.color[rows] = "#aaf"

    }
    df = data_frame(counts, paste0("(",shares,"%)"), choices)

    html = html.result.table(df,colnames=c("Number","Share","Answer",""), font.size="110%", align=c("center","center","left"),bg.color = bg.color)

    ui = tagList(HTML(html))
    if (has.mathjax) ui = withMathJaxNoHeader(ui)
    setUI(outputId, ui)
  }
  invisible(ui)
}


show.clicker.quiz.mc.results = function(dat, qu,part.ind = 1, part = qu$parts[[part.ind]], show.sol=TRUE, outputId = NULL, opts=NULL,do.plot=TRUE, app=getApp()) {
  restore.point("clicker.quiz.mc.results.ui")

  dat = dat[dat$part.ind==part.ind,,drop=FALSE]
  choices = unlist(part$choices)

  #dat = transform.to.mc.data(dat, choices)
  dat$answer = choices[dat$answer.ind]

  sum = group_by(dat,answer) %>% summarize(yes=sum(checked),no=sum(!checked))

  ind = match(choices,sum$answer)
  sum = sum[ind,]
  yes = sum$yes
  no = sum$no

  nans = length(unique(dat$userid))
  #yes = lapply(yes, function(y) list(y=y, color="#777"))
  #no = lapply(yes, function(y) list(y=y, color="#d35400"))

  if (show.sol) {
    correct = choices %in% part$answer
    pre = post = ""
    #pre = ifelse(correct,"* ","")
    #post = ifelse(correct," (Answer: Yes)", " (Answer: No)")
    pre = ifelse(correct,"(Yes) ","(No) ")
    choices = paste0(pre, choices,post)
  }

  plot = highchart() %>%
  hc_chart(type = "column") %>%
  hc_plotOptions(
    column=list(
      dataLabels=list(enabled=TRUE)
    )
  ) %>%
  hc_xAxis(categories = choices) %>%
  hc_add_series(data = yes,name = "Yes", color="#2980b9") %>%
  hc_add_series(data = no,name = "No",color="#d35400")

  cat("\nshow mc results as plot")
  # need random string to correctly rerender plot
  plotId = paste0(outputId,"_Plot_",random.string(nchar=8))
  ui = tagList(
    HTML(part$question), p(paste0("(",nans," replies)")),
    div(style="height=14em",
      highchartOutput(plotId, height="14em")
    )
    #p("Total: ",nans," replies.")
  )
  setUI(outputId,ui)
  dsetUI(outputId,ui)
  app$session$output[[plotId]] = renderHighchart(plot)

  invisible(ui)
}



show.clicker.quiz.numeric.results = function(dat, qu,part.ind = 1, part = qu$parts[[part.ind]], show.sol=TRUE, outputId = NULL, opts=NULL,do.plot=TRUE, app=getApp()) {
  restore.point("clicker.quiz.numeric.results.ui")

  dat = dat[dat$part.ind==part.ind,,drop=FALSE]
  answer = number.answer.to.numeric(part$answer)
  nans = NROW(dat)
  var = "answer"
  vals = number.answer.to.numeric(dat[[var]])

  #plot = clicker.numeric.relative.deviation.plot(vals, answer)
  plot = clicker.numeric.quantile.plot(vals, answer)

  # need random string to correctly rerender plot
  plotId = paste0(outputId,"_Plot_",random.string(nchar=8))
  ui = tagList(
    HTML(part$question), p(paste0(if (show.sol) paste0("Correct solution: ", answer)," (",nans," replies)")),
    div(style="height=14em",
      highchartOutput(plotId, height="14em")
    )
  )
  setUI(outputId,ui)
  dsetUI(outputId,ui)
  app$session$output[[plotId]] = renderHighchart(plot)


  invisible(ui)
}


clicker.numeric.quantile.plot = function(vals, solution, max.deviation=10) {
  restore.point("clicker.numeric.quantile.plot")
  #vals = na.omit(vals)

  if (solution > 0) {
    vals[vals>solution+max.deviation*solution]=NA
    vals[vals<solution-max.deviation*solution]=NA
  }

  vals = sort(vals)
  index = seq_along(vals)
  hc <- highchart() %>%
    hc_chart(zoomType = "y") %>%
    hc_yAxis(title = list(text = "Answer")) %>%
    hc_xAxis(categories = index, title = list(text = "Index of submitted answer (sorted)")) %>%
    hc_add_series(name="Correct Solution",data = rep(solution, length(vals))) %>%
    hc_add_series(name = "Answers", data = vals)
  hc

}

clicker.numeric.relative.deviation.plot = function(vals, solution) {
  vals = na.omit(vals)
  answer = solution; val = vals

  pos.dev = (val/answer-1)
  neg.dev = (answer/val-1)
  neg = neg.dev > pos.dev

  dev = pmax(pos.dev,neg.dev) * (-1)^neg


  res = relative.deviation.breaks(size="5")
  br = res$br
  lab = res$lab


  int = findInterval(dev, br)

  choices = lab
  values = lab[int]
  counts = rep(0, length(choices))
  names(counts) = choices
  cc = table(values)
  counts[names(cc)] = cc
  names(counts) = NULL


  plot = highchart() %>%
  hc_chart(type = "column") %>%
  hc_plotOptions(
    column=list(dataLabels=list(enabled=TRUE),colorByPoint = TRUE,colors=res$colors),
    colors=res$colors,
    series=list(
        pointPadding= 0,
        groupPadding= 0,
        borderWidth= 0.5,
        borderColor= 'rgba(255,255,255,0.5)'
    )
  ) %>%
  hc_xAxis(categories = lab) %>%
  hc_add_series(data = counts,name="Counts",showInLegend=FALSE)


}

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

# transform submission data into simpler format
quiz.clicker.transform.sub.data = function(dat, ct) {
  restore.point("quiz.clicker.transform.sub.data")

  qu = first.non.null(ct[["qu"]],ct[["wid"]])

  # Add checked and points to dat
  dat$points = 0

  part.ind = 1
  for (part.ind in seq_along(qu$parts)) {
    part = qu$parts[[part.ind]]
    rows = dat$part.ind == part.ind
    dat[rows,"points"] = get.clicker.quiz.points(dat = dat[rows,], part = part)
    dat[rows,"max.points"] = part$points
  }

  return(dat)

  dat
}


transform.to.mc.data = function(dat, choices) {
  restore.point("transform.to.mc.data")
  choices = unlist(choices)

  library(tidyr)
  mc = expand.grid(answer=choices,userid=unique(dat$userid))
  jd = suppressWarnings(left_join(mc, dat,by=c("userid","answer")))
  jd$checked = !is.na(jd$submit.time)

  jd = select(jd,submit.time,userid,answer,checked)
  jd
}

get.clicker.quiz.points = function(dat, part) {
  restore.point("get.clicker.quiz.points")
  if (part$type == "sc") {
    points = first.non.null(part[["points"]], 2)

    correct = (as.character(dat$answer) == as.character(part$answer))
    return(correct * points)
  } else if (part$type == "mc") {
    restore.point("get.clicker.quiz.points.mc")
    points = first.non.null(part[["points"]], 0.5*length(part$choices))
    item.points = points / length(part$choices)
    partial.points = first.non.null(part$partial.points, TRUE)
    is.sol = dat$answer %in% part$answer
    correct = dat$checked == is.sol

    if (partial.points) {
      return(correct*item.points)
    }

    # no partial points
    # need to check for each user
    # whether all choices are correct
    dat = dat %>%
      mutate(is.sol = is.sol) %>%
      group_by(dat, userid, tag) %>%
      mutate(correct = all(checked == is.sol))

    return(dat$correct * item.points)
  } else if (part$type=="numeric") {
    sol = as.numeric(part$answer)

    points = rep(0,NROW(dat))
    # give points based on relative distance
    if (!is.null(part$distance_points)) {
      dist = abs((number.answer.to.numeric(dat$answer) - sol)/sol)
      for (dp in part$distance_points) {
        rows = is.true(dist<=dp[[1]])
        points[rows] = pmax(points[rows],dp[[2]])
      }
      return(points)
    }

    # only points if anser is exact (up to rounding)
    answer = number.answer.to.numeric(dat$answer)
    if (!is.null(part$round)) {
      sol = round(sol, part$round)
      answer = round(answer, part$round)
    }
    correct.points = points = first.non.null(part[["points"]],3)
    correct = (answer == sol)
    return(correct * correct.points)
  }
  return(rep(0, NROW(dat)))
}

number.answer.to.numeric = function(str) {
  str = gsub(",",".", str, fixed=TRUE)
  as.numeric(str)
}
