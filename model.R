# data読み込み
input <- read.csv("test.csv")
head(input)

# モデル定義を作る(データによって異なる)
createModel <- function() {
  # 変数名 ∼ 依存変数
  # + 依存変数の合成
  return ("
    bwt ~ age + lwt + smoke
    lwt ~ age
  ")
}

# lavaanでSEM
library(lavaan)
library(semPlot)

model.l <- createModel()
res.l <- lavaan(model.l, data = input, auto.var = TRUE)
summary(res.l, standardized = TRUE)

# semPlotで描画
semPaths(res.l)

# graphvizで描画するためにフォーマット変換する
formatSem <- function(param) {
  tmp <- semPlotModel(param)
  pars <- tmp@Pars
  pars$label <- paste0("par",1:length(pars$par))
  pars$edge[pars$edge == "~>"] <- "->"

  tmp.m <- paste(paste(pars$lhs, pars$edge, pars$rhs), pars$label, pars$est, sep = ",", collapse = "\n")
  return (
    sem::specifyModel(textConnection(tmp.m))
  )
}
model.s <- formatSem(res.l)
model.s

# pathDiagramでパス図の作成
res.s <- sem::sem(model.s, data = input)
g <- sem::pathDiagram(res.s,edge.labels="both")

library(DiagrammeR)
library(htmlwidgets)
library(webshot)
# グラフのhtml作成
saveWidget(grViz(g), file = "path.html")
# phontomJSでスクリーンショット
webshot("path.html", file = "path.png")