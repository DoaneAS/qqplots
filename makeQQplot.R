############################################################################
## Ashley Stephen Doane
## Weill Cornell Medicine  asd2007@med.cornell.edu
## New York Genome Center adoane@nygenome.org

## This program is free software: you can redistribute it and/or modify it
## under the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU General Public License for more details.

## You should have received a copy of the GNU Lesser General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
###############################################################################

require(data.table)
require(fishHook)

result.df <- fread("./fishHook.results.tsv")
result.df


require(cowplot)


require(data.table)
require(fishHook)
require(cowplot)



ggqq <- function(ps, symbol, ci = 0.95, splitx=3.5, pcolor) {
  require(ggrepel)
  theme_set(theme_cowplot())
  N  <- length(ps)
  df <- data.frame(
    observed = -log10(sort(ps)),
    expected = -log10(1:N / N),
    clower   = -log10(qbeta(ci,     1:N, N - 1:N + 1)),
    cupper   = -log10(qbeta(1 - ci, 1:N, N - 1:N + 1)),
    symbol   = symbol
  )
  log10Pe <- expression(paste("Expected -log"[10], plain(P)))
  log10Po <- expression(paste("Observed -log"[10], plain(P)))
  ggplot(df, aes(expected,observed,label=symbol)) +
    geom_point(aes(expected, observed), shape = 16, 
               color = ifelse(pcolor == "", "black", "red")) +
    geom_abline(intercept = 0,
                slope = 1,
                alpha = 0.5) +
    geom_line(aes(expected, cupper), linetype = 2) +
    geom_line(aes(expected, clower),
              linetype = 2,
              color = 'red') +
    xlab(log10Pe) +
    ylab(log10Po) +
    #coord_cartesian(xlim = c(0, max(df$observed)), ylim=c(0,max(df$observed))) +
    #xlim(0, 4.5) +
    geom_text_repel(
      data = subset(df, (expected > splitx & observed > 1)),
      nudge_x      =  6 - subset(df, (expected > splitx & observed > 1))$expected,
      direction    = "y",
      hjust        = 0,
      max.time = 5, max.iter = 100000,
      segment.size = 0.2) +
    geom_text_repel(
      data = subset(df, (expected <= splitx & observed > 1)),
      nudge_x      =  1 - subset(df, (expected <= splitx & observed > 1 ))$expected,
      direction    = "y",
      nudge_y =  10,
      # nudge_y =  1 + subset(df, (expected <= splitx & observed > 1 ))$observed,
      hjust        = 1,
      max.time = 5, max.iter = 100000,
      segment.size = 0.2)
  #geom_text_repel(size=4,box.padding = 0.25,segment.size = .25,
  #   max.iter = 2000
  #)
}


result.df$plotSymbol <- ""
result.df$plotSymbol[1:20] <- result.df$name[1:20]
result.df[, plotcolor := ifelse(fdr<=0.001, name, "")]     

p = ggqq(result.df$p, symbol = result.df$plotSymbol, pcolor = result.df$plotcolor)


save_plot("qqPlot.pdf", p)
