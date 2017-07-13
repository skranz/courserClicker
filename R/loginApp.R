

clickerLoginApp = function(psapps, db.dir = paste0(getwd(),"/db"), init.userid="", init.password="",loginapp.url, psapp.url, app.title="RTutor Login", email.domain = NULL, check.email.fun = NULL, email.text.fun=default.email.text.fun, use.db=TRUE, main.header=rtutor.login.main.default.header(), smtp = NULL) {
  restore.point("RTutorLoginApp")


  library(shinyjs)
  library(loginPart)
  library(RSQLite)

  app = eventsApp()

  psapps = lapply(psapps, rtutor.login.init.psa)

  app$glob$psapps = psapps
  app$glob$cur.inst = rep(NA_integer_, length(psapps))

  login.fun = function(app=getApp(),userid,...) {
    show.rtutor.login.main(userid=userid, header=main.header)
  }

  if (is.null(check.email.fun)) {
    if (!is.null(email.domain)) {
      check.email.fun = function(email,...) {
        check.email.domain(email, email.domain)
      }
    } else {
      check.email.fun = function(email,...) {
        list(ok=TRUE,msg="")
      }
    }
  }

  db.arg = list(dbname=paste0(db.dir,"/userDB.sqlite"),drv=SQLite())

  lop = loginPart(db.arg = db.arg, login.fun=login.fun, check.email.fun=check.email.fun, email.text.fun = email.text.fun, app.url=loginapp.url, app.title=app.title,init.userid=init.userid, init.password=init.password,container.id = "mainUI")
  set.lop(lop)
  lop.connect.db(lop=lop)
  lop$login$ui = lop.login.ui(lop)

  if (is.null(smtp)) smtp = lop.get.smtp()
  lop$smtp = smtp

  appInitHandler(function(session,...) {
    initLoginDispatch(lop)
  })

  jsCode <- "shinyjs.openLink = function(url){window.open(url,'_blank');}"
  app$ui = tagList(
    useShinyjs(),
    extendShinyjs(text = jsCode),
    fluidPage(
      uiOutput("mainUI")
    )
  )
  app$lop = lop
  app
}

rtutor.login.init.psa = function(psa) {
  restore.point("rtutor.login.init.psa")

  psa = copy.into.missing.fields(psa, source=list(
    sessions.dir = paste0(psa$appdir,"/sessions"),
    ups.dir = paste0(psa$appdir,"/ups")
  ))
  psa
}

rtutor.login.main.default.header = function() {
"
<h3>Welcome {{userid}}</h3>
<br>
<p>Choose your problem set...</p>
<br>
"
}


show.rtutor.login.main = function(userid="guest", psapps = app$glob$psapps, app = getApp(), header = "") {
  restore.point("show.rtutor.login.main")

  psapps = lapply(psapps, function(psa) {
    psa$session.key = paste(sample(c(0:9, letters, LETTERS),40, replace=TRUE),collapse="")
    psa
  })


  psh = lapply(seq_along(psapps), function(i) {
    psa = psapps[[i]]
    url = psa$url
    html = paste0('<a href="', url,'" class="button" target="_blank">',psa$label,'</a>')
    link = HTML(html)

    btnId = paste0("openPSAppBtn__",i)
    linkUIId = paste0("openPSAppLinkUI__",i)
    buttonHandler(id=btnId,rtutor.open.psapp.click, i=i, psa=psa, url=url, userid=userid)

    list(
      bsButton(btnId,psa$label),
      uiOutput(linkUIId),
      hr()
    )
  })

  header = replace.whisker(header, list(userid=userid))
  ui = fluidRow(column(offset = 2, width=8,
    h3("Welcome ", userid),
    br(),
    p("Choose your problem set..."),
    br(),
    psh
  ))
  setUI("mainUI", ui)
}


rtutor.open.psapp.click = function(i,psa,url,userid,app=getApp(), ...) {
  restore.point("rtutor.open.psapp.click")
  glob = app$glob

  if (isTRUE(psa$instances>0)) {
    if (is.na(glob$cur.inst[[i]])) {
      glob$cur.inst[[i]] = sample.int(psa$instances,1)
    } else {
      glob$cur.inst[[i]] = ((glob$cur.inst[[i]]+1) %% psa$instances)+1
    }
    url = paste0(url,"_inst/i",glob$cur.inst[[i]],"/")
  }
  url = paste0(url,'?key=',psa$session.key)
  rtutor.write.session.file(userid=userid, session.key = psa$session.key, sessions.dir=psa$sessions.dir)

  js$openLink(url)

  linkUIId = paste0("openPSAppLinkUI__",i)


  html = paste0('<a href="', url,'" class="button" target="_blank">Click here if problem set does not open automatically.</a>')
  setUI(linkUIId,HTML(html))
}

rtutor.write.session.file = function(userid, session.key, sessions.dir) {
  restore.point("rtutor.write.session.file")

  ses = nlist(userid, time = Sys.time())
  file = paste0(sessions.dir, "/", session.key, ".ses")
  saveRDS(ses, file=file)
}
