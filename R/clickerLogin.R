# Sign up modes for clickerApp
# By token: query key or cookie


clicker.client.lop = function(glob){
  library(shinyEventsLogin)
  library(RSQLite)

  db.arg = list(dbname=paste0(glob$db.dir,"/userDB.sqlite"),drv=SQLite())
  login.fun = clicker.client.login.fun

  lop = loginModule(db.arg = db.arg, login.fun=login.fun, check.email.fun=glob$check.email.fun, email.domain = glob$email.domain, email.text.fun = glob$email.text.fun, app.url=glob$app.url, app.title=glob$app.title,init.userid=glob$init.userid, init.password=glob$init.password,container.id = "mainUI",login.ui.fun = client.clicker.login.ui, smtp=glob$smtp, login.by.query.key = "allow", token.dir=glob$token.dir)

  lop
}

# This function will be called after a succesful login
clicker.client.login.fun = function(app=getApp(), userid, target="_self", tok=NULL,...) {
  restore.point("clicker.client.login.fun")
  glob = app$glob

  # We don't use tokens and URL with key
  # Drawback: If app refreshes, we need to login again
  if (!isTRUE(glob$use.token)) {
    clicker.client.start.task.observer(tok = list(userid=userid))
    return()
  }

  # Already called with a token, i.e. we have the correct URL
  # or have set a cookie
  if (!is.null(tok)) {
    clicker.client.start.task.observer(tok = tok)
    return()
  }

  # We logged-in without token
  # Generate a token and open link to URL with key
  # This is more stable on a refresh action
  tok = make.login.token(userid=userid,validMinutes=glob$token.valid.min)
  write.login.token(tok=tok, token.dir=glob$token.dir)
  url = token.login.url(glob$app.url,tok = tok)
  html = paste0('<a href="', url,'" class="button" target="',target,'">Click here if clicker app does not open automatically.</a>')
  setUI("mainUI",HTML(html))
  open.app.with.login.token(url=url,target=target)
}

client.clicker.login.ui = function(lop=NULL,ns=lop$ns, init.userid=lop$init.userid, init.password=lop$init.password, title.html = lop$login.title,help.text=lop$login.help,lang = lop$lang,app=getApp(),...) {
  restore.point("client.clicker.login.ui")
  sel = ids2sel(c(ns("loginUser"),ns("loginPassword"),ns("loginCourse"),ns("loginCode")))

  glob = app$glob
  if (glob$allow.guest.login) {
    glob$guest.count = first.non.null(glob$guest.count,0)+1
    init.userid = paste0("Guest_",glob$guest.count)
    app$guestid = init.userid
  }


  if (identical(lang,"de")) {
    widgets = list(
      HTML(title.html),
      #if (glob$show.course.list)
      #  selectInput(ns("loginCourse"),"Kurs:", choices = courses),
      #if (glob$show.course.code)
      #  textInput(ns("loginCode"),"Code des Kurses:", value=""),
      textInput(ns("loginUser"), "Nutzer", value = init.userid),
      if (glob$use.login.db)
        passwordInput(ns("loginPassword"), "Passwort", value = init.password),
      actionButton(ns("loginBtn"), "Login", "data-form-selector"=sel),
      actionButton(ns("loginSignupBtn"), "Registrieren"),
      actionButton(ns("loginResetBtn"), "Passwort vergessen"),
      uiOutput(ns("loginAlert")),
      HTML(help.text)
    )
  } else {
    widgets = list(
      HTML(title.html),
      #if (glob$show.course.list)
      #  selectInput(ns("loginCourse"),"Course:", choices = courses),
      #if (glob$show.course.code)
      #  textInput(ns("loginCode"),"Course code:", value=""),
      textInput(ns("loginUser"), "User", value = init.userid),
      if (glob$use.login.db)
        passwordInput(ns("loginPassword"), "Password", value = init.password),
      actionButton(ns("loginBtn"), "log in", "data-form-selector"=sel),
      actionButton(ns("loginSignupBtn"), "sign up"),
      actionButton(ns("loginResetBtn"), "reset password"),
      uiOutput(ns("loginAlert")),
      HTML(help.text)
    )
  }
  setUI(ns("loginAlert"),"")
  ui = wellPanel(widgets)
  # manual button handler to deal with guest login
  buttonHandler(ns("loginBtn"),clicker.login.btn.click,lop=lop,no.authentication.required = TRUE)

  ui
}


clicker.login.btn.click = function(app=getApp(),lop,formValues,ns=lop$ns,...) {
  restore.point("clicker.login.btn.click")

  userid = formValues[[ns("loginUser")]]
  password = formValues[[ns("loginPassword")]]

  # currently we don't use courses
  #extract course
  #courseid = formValues[[ns("loginCourse")]]
  #coursecode = formValues[[ns("loginCode")]]

  #fc = login.find.course(courseid=courseid, coursecode=coursecode)

  #if (!fc$ok) {
  #  setUI(ns("loginAlert"),HTML(fc$msg))
  #  return()
  #}

  #courseid = fc$courseid
  # guest can login without password
  if (identical(userid,app$guestid)) {
    lop$login.fun(userid=userid, password=password, lop=lop)
  } else {
    lop.login.btn.click(app=app,lop=lop,formValues=formValues,ns=ns,...)
  }
}



