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



default.clicker.pages.params = function(lang="en") {
  restore.point("default.clicker.pages.params")
  if (lang=="de") {
    mark_utf8(list(
      title="Vorlesungsquiz",
      sendLabel="Absenden",
      wait = "Bitte warten Sie, bis das nächste Quiz gestartet wird.",
      submitted="Sie haben Ihre Antwort abgeschickt. Bitte warten Sie, bis das nächste Quiz gestartet wird.",
      timeout="Leider ist die Zeit abgelaufen um eine Antwort abzusenden.  Bitte warten Sie, bis das nächste Quiz gestartet wird."
    ))
  } else {
   list(
      title="Lecture Quiz",
      sendLabel="Send",
      wait="Please wait until the next quiz starts.",
      submitted="You have submitted your answer. Please wait until the next quiz starts.",
      timeout="Sorry, but your time to submit an answer has run out.  Please wait until the next quiz starts."

    )

  }
}

clicker.client.sendBtnLabel = function(lang="en") {
  if (lang == "en") return("Send")
  if (lang == "de") return("Absenden")
  return("Send")
}
