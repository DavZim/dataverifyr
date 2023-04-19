#' Creates a simple HTML table that can have nested markdown (code, links, etc)
#'
#' Useful for RMarkdown in a code chunk with `results = "asis"`
#' @noRd
#'
#' @param d a data.frame
#' @param align how the columns should be aligned
#'
#' @return
#'
#' @examples
#' d <- data.frame(
#'   library = "[`arrow`](https://arrow.apache.org/docs/r/)",
#'   code = "```R\nlibrary(arrow)\nds <- open_dataset('myfile')\nds |>\n  dplyr::collect()\n```"
#' )
#' simple_table(d, align = "cl")
simple_table <- function(d, align = paste(rep("l", ncol(d)), collapse = "")) {
  hd <- names(d)
  a <- strsplit(align, "")[[1]]
  a <- unlist(list(l = "left", r = "right", c = "center")[a])
  has_md <- function(x) grepl("[`\\[]", x)

  s <- function(...) paste(sprintf(...), collapse = "\n")
  cat(paste(
    "<table>",
    '  <thead class="header">',
    s('    <th style="text-align:%s;">%s</th>', a, names(d)),
    "  </thead>",
    "  <tbody>",
    paste(sapply(seq(nrow(d)), function(r) {
      c(
        s('<tr class="%s">', ifelse(r %% 2 == 0, "even", "odd")),
        paste(sapply(seq(ncol(d)), function(cc) {
          v <- d[r, cc]
          s('      <td style="text-align:%s;">%s%s%s</td>',
            a[cc],
            if (has_md(v)) "\n\n" else "",
            v,
            if (has_md(v)) "\n\n" else ""
          )
        }), collapse = "\n"),
        "</tr>"
      )
    }), collapse = "\n"),
    "  </tbody>",
    "</table>",
    collapse = "\n", sep = "\n"
  ))
  invisible(d)
}
