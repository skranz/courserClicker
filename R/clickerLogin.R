# Login modes for clickerApp
# Simply enter email adress: works if no password is required
# By permanent cookie:  verify with button click if that is indeed the right
# By session cookie: automatically log-in (relevant e.g. for page reload)
# By query key: Automatically
#
# By token: query key or cookie
# By username with password (if user exists)


clicker.client.lop = function(glob){
  library(shinyEventsLogin)
  library(RSQLite)

  restore.point("clicker.client.lop")
  login.fun = clicker.client.login.fun

  lop = loginModule(login.fun=login.fun, app.url=glob$app.url, app.title=glob$app.title,init.userid=glob$init.userid, container.id = "mainUI",login.ui.fun = client.clicker.login.ui, login.by.query.key = "allow", token.dir=glob$token.dir,use.signup = FALSE, lang = first.non.null(glob$lang,"en"),need.password = FALSE, need.userid = !isTRUE(glob$auto.guest.login), cookie.name="courserClickerCookie")

  lop
}


client.clicker.login.ui = function(lop=NULL,ns=lop$ns, init.userid=lop$init.userid, init.password=lop$init.password, title.html = lop$login.title,help.text=lop$login.help,lang = lop$lang,app=getApp(), ...) {
  restore.point("client.clicker.login.ui")

  sel = ids2sel(c(ns("loginUser")))

  glob = app$glob

  pp = glob$page.params

  if (glob$make.guest.init.userid) {
    glob$guest.count = first.non.null(glob$guest.count,0)+1
    init.userid = paste0("Guest_",glob$guest.count)
    app$guestid = init.userid
  }

  widgets = list(
    HTML(pp$loginHeader),
    textInput(ns("loginUser"), pp$loginUserLabel, value = init.userid),
    actionButton(ns("loginBtn"), pp$loginBtnLabel, "data-form-selector"=sel),
    uiOutput(ns("loginAlert")),
    HTML(help.text)
  )
  setUI(ns("loginAlert"),"")
  ui = wellPanel(widgets)
  # manual button handler to deal with guest login
  buttonHandler(ns("loginBtn"),clicker.login.btn.click,lop=lop,no.authentication.required = TRUE)

  ui
}


# This function will be called after a succesful login
clicker.client.login.fun = function(app=getApp(), userid, target="_self", tok=NULL,login.mode=NULL,lop=get.lop(),...) {
  restore.point("clicker.client.login.fun")
  glob = app$glob

  if (userid == "") {
    glob$guest.count = first.non.null(glob$guest.count,0)+1
    userid = paste0("User_",glob$guest.count)
  }

  courser.track.cookie(courseid=glob$courseid, token.dir=glob$token.dir,userid = userid,login.app = "clicker", login.mode = login.mode)

  # We don't use tokens and URL with key
  # Drawback: If app refreshes, we need to login again
  if (!isTRUE(glob$use.token)) {
    show.userid.ui(userid,login.mode,lop)
    clicker.client.start.task.observer(tok = list(userid=userid))
    return()
  }

  # Already called with a token, i.e. we have the correct URL
  # or have set a cookie
  if (!is.null(tok)) {
    show.userid.ui(userid,login.mode, lop)
    clicker.client.start.task.observer(tok = tok)
    return()
  }

  # Reset login by cookie (may have been turned off
  # when we want to login with new userid)
  lop$login.by.cookie = "allow"

  # We logged-in without token
  # Generate a token and open link to URL with key
  # This is more stable on a refresh action
  tok = make.login.token(userid=userid,validMinutes=glob$token.valid.min)
  write.login.token(tok=tok, token.dir=glob$token.dir)
  # Set cookie. If page refreshes, we should log in automatically
  set.login.token.cookie(tok=tok, "courserClickerCookie")
  show.userid.ui(userid,login.mode,lop)
  clicker.client.start.task.observer(tok = tok)


}


clicker.login.btn.click = function(app=getApp(),lop,formValues,ns=lop$ns,...) {
  restore.point("clicker.login.btn.click")
  glob = app$glob

  userid = formValues[[ns("loginUser")]]

  # Only user lowercase letters for userid
  userid = tolower(userid)

  # Automatically adjust userid to email domain
  if (!is.null(glob$email.domain)) {
    userid = paste0(str.left.of(userid,"@"),"@",glob$email.domain)
  }

  # For direct login check if user is not restricted
  file = file.path(app$glob$clicker.dir, "restricted_login", digest(userid))
  if (file.exists(file)) {
    msg=paste0("Login for user ", userid, " is restricted. You must login via the coursepage or via the link that was provided in the welcome email when you have registered to the coursepage.")
    timedMessage(ns("loginAlert"), msg=msg, millis = Inf)
    return()
  }


  lop$login.fun(userid=userid, lop=lop, login.mode="manual")
}

show.userid.ui = function(userid, login.mode,lop, app=getApp()) {
  restore.point("show.userid.ui")


  # With query key in url user cannot be changed
  if (isTRUE(login.mode=="query")) {
    html = paste0('<hr><span style="font-size: 8px; color: #444444">', app$glob$page.params$loginAs," ", userid,'</span>')

  # With cookie or manual login , user may be changed
  } else if (!isTRUE(app$glob$auto.guest.login)) {
    setUI("useridUI", tagList(
      hr(),
      smallButton("loginNewBtn", paste0(app$glob$page.params$loginChange, " (", userid,")")),
      tags$script(HTML('$("#loginNewBtn").click(function(){Cookies.remove("courserClickerCookie");});'))
    ))
    buttonHandler("loginNewBtn", function(..., app=getApp()) {
      restore.point("loginNewBtnClick")
      # Don't listen to any clicker tasks any more
      app$task.obs$destroy()

      removeCookie("courserClickerCookie")
      app$is.authenticated = FALSE
      ct = app$glob[["ct"]]
      if (!is.null(ct$wid))
        setUI(paste0(ct$wid$id,"-msgUI"), "")
      setUI("useridUI", HTML(""))
      setUI("titleUI", HTML(""))


      # Temporary remove login.via.cookie
      lop = as.environment(as.list(lop, all.names=TRUE))
      lop$login.by.cookie = "no"
      initLoginDispatch(lop)
    })
  }
}

