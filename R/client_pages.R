make.clicker.page.params = function(params, lang="en") {
  def.params = default.clicker.pages.params(lang=lang)
  def.params[names(params)] = params
  params = def.params
  params
}

clicker.title.ui = function(params=app$glob$page.params, nickname="", app=getApp()) {
  h3(params$title)
}

clicker.client.wait.page =function(params=app$glob$page.params, app=getApp()) {
  p = params
  tagList(
    p(p$wait)
  )
}


clicker.client.submitted.page =function(params=app$glob$page.params, answer, app=getApp()) {
  restore.point("clicker.client.submited.page")
  p = params
  armd::with.mathjax(tagList(
    p(paste0(p$answerLab," ", answer)),
    p(p$wait)
  ))
}


default.clicker.pages.params = function(lang="en") {
  restore.point("default.clicker.pages.params")
  if (lang=="de") {
    mark_utf8(list(
      title="Vorlesungsquiz",
      sendLabel="Absenden",
      wait="Bitte warten Sie, bis das nächste Quiz gestartet wird.",
      answerLab="Sie haben folgende Antworte(n) gewählt:",
      solutionLab="Richtige Lösung:"
    ))
  } else {
   list(
      title="Lecture Quiz",
      sendLabel="Send",
      wait="Please wait until the next quiz starts.",
      answerLab="You have entered the following answer(s):",
      solutionLab="Correct solution:"
    )

  }
}

clicker.client.sendBtnLabel = function(lang="en") {
  if (lang == "en") return("Send")
  if (lang == "de") return("Absenden")
  return("Send")
}
