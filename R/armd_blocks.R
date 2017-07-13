# blocks specified in RTutor
courserClicker.block.types.df = function(...) {
  restore.point("courserClicker.block.types.df")

  types = c("quiz")
  n = length(types)
  bt.df = data_frame(type=types, package="courserClicker", is.widget=TRUE, parse.inner.blocks = FALSE, remove.inner.blocks= TRUE, is.parent=FALSE, is.container = TRUE, dot.level=0, arg.li = vector("list",n))

  bt.df
}

