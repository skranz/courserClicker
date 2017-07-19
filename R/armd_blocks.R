# blocks specified in RTutor
courserClicker.block.types.df = function(...) {
  restore.point("courserClicker.block.types.df")

  types = c("quiz")
  n = length(types)
  bt.df = data_frame(type=types, package="courserClicker", is.widget=TRUE, parse.inner.blocks = FALSE, remove.inner.blocks= TRUE, is.parent=FALSE, is.container = TRUE, dot.level=0, arg.li = vector("list",n))

  bt.df
}

rtutor.slide.plugin.custom.clicker = function(...,ps=NULL, opts=rt.opts()) {
  restore.point("rtutor.slide.plugin.custom.clicker")
  cc = init.custom.clicker.quiz(clicker.dir = opts$clicker.dir)
  ps$custom.clicker.cc = cc
  cc$ui=custom.clicker.quiz.ui(cc)
  list(
    panel = tabPanel("Quiz",cc$ui),
    init.handlers = function(ps,...) {
      restore.point("init.custom.clicker.handler.276")
      cc = ps$custom.clicker.cc
      custom.clicker.quiz.init.handlers(cc)
    }
  )
}

