target.app.token.link.ui = function(app.url, tok,url = token.login.url(app.url, tok),new.tab=TRUE,target=if (new.tab) "_blank" else "") {

  tagList(
    HTML(paste0("<a href='",url,"'>Click here if the app does not open automatically</a>"))
  )
}


open.app.with.login.token = function(app.url, tok, app=getApp(), new.tab=TRUE,target=if (new.tab) "_blank" else "", url = token.login.url(app.url, tok=tok)) {
  restore.point("open.app.with.login.token")
  callJS(.fun = "window.open",list(url,target))
}



clicker.default.token = function() {
  list(userid="Guest", created=Sys.time(), validUntil=Inf)
}
