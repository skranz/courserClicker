courser.track.cookie = function(courseid, token.dir, userid, login.app, login.mode,  app=getApp(), expires=30*8) {

  if (!areAppCookiesLoaded(app=app)) {
    # Create observer that waits until cookies are loaded
    app$courser.track.cookie.observer = observe({
      if (!areAppCookiesLoaded(app=app)) {
        invalidateLater(500)
        return()
      }
      # Cookies are loaded call main function again
      courser.track.cookie(courseid, token.dir, userid, login.app, login.mode, app, expires)
    })
    return()
  }
  restore.point("courser.track.cookie")
  cookie.name = paste0("courserAllCookie_",courseid)
  cookies = getLoadedCookies(app)
  cookie = cookies[[cookie.name]]

  app$..courserAllCookie = cookie

  # No cookie yet set. Set a new cookie
  if (is.null(cookie)) {
    cookie = list(key=random.string(nchar=20))
    setCookie(cookie.name, cookie, expires=expires)
  }

  cookie.dir = file.path(token.dir, "cookies")

  # Don't store info if no directory exists
  if (!dir.exists(cookie.dir)) return()


  # Write info into a random file in cookie.dir
  str = paste0(c(cookie$key,userid, login.app, login.mode, as.integer(Sys.time())), collapse=",")
  file = file.path(cookie.dir, random.string(nchar=30))
  writeLines(str, file)
}

getCourserAllCookie = function(app=getApp()) {
  first.non.null(app$..courserAllCookie,list(key="-"))
}
