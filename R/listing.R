#' List objects in an environment
#'
#' @param pos position on the search list for the environment to be listed
#' @param pattern an optional regular expression; only names matching `pattern` are listed
#' @param order.by optional name of column by which to order output
#' @param decreasing if TRUE, order by decreasing value of sort column
#' @param head if FALSE, return all rows. If true, return `n` rows
#' @param n the number of rows to return if `head` is true
#'
#' @return a data.frame
#' @export
#' @importFrom utils object.size
#'
ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5)
{
  names <- ls(pos = pos, pattern = pattern)
  if (length(names) == 0) return(data.frame("Type" = character(), "Size" = character(), "Rows" = numeric(), "Columns" = numeric()))
  napply <-
    function(names, fn)
      sapply(names, function(x)
        fn(get(x, pos = pos)))
  obj.class <- napply(names, function(x)
    as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  obj.prettysize <-
    sapply(obj.size, function(r)
      prettyNum(r, big.mark = ","))
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim, stringsAsFactors = FALSE)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing = decreasing),]
  out <- out[c("Type", "PrettySize", "Rows", "Columns")]
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (head)
    out <- head(out, n)
  out
}

#' List objects in an environment
#'
#' @param ... arguments passed to `ls.objects`
#' @param n number of rows to return
#'
#' @return a data.frame
#' @export
#'
lsos <- function(..., n=10) {
  ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

