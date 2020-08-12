#' Wrapper for stargazer that saves latex output and displays html table
#'
regtable <- function(res, landscape=FALSE, texfile=NULL,
                     add.lines=NULL, ...) {
  if (sgtype=="latex" & !is.null(add.lines)) {
    tbl  <- capture.output(stargazer(res, type=sgtype, style=sgstyle,
                                     df=FALSE, header=FALSE,
                                     add.lines=add.lines,
                                     no.space=TRUE,
                                     p=c(0.1,0.05,0.01), ...))
  } else {
    # add.lines doesn't work for other output types ...
    tbl <- capture.output(stargazer(res, type=sgtype, style=sgstyle,
                                    df=FALSE, header=FALSE,
                                    p=c(0.1,0.05,0.01),
                                    ...))
  }
  if (landscape && knitr::is_latex_output()) {
    cat("\\afterpage{
    \\clearpage
    \\thispagestyle{empty}
    \\begin{landscape}
")
  }
  tbl <- relabel(tbl)
  tbl <- gsub("Model (\\d)", "\\(\\1\\)", tbl)
  cat(paste(relabel(tbl),collapse="\n"))
  if (landscape && knitr::is_latex_output()) {
    cat("    \\end{landscape}
    \\clearpage
}")
  }
  if (!is.null(texfile)) {
    tbl  <- capture.output(stargazer(res, type="latex", style=sgstyle,
                                     df=FALSE, header=FALSE,
                                     add.lines=add.lines,
                                     no.space=TRUE,
                                     #p=c(0.1,0.05,0.01),
                                     ...))
    tbl <- relabel(tbl)
    tbl <- gsub("Model (\\d)", "\\(\\1\\)", tbl)
    tbl <- gsub("No","Yes", tbl)
    cat(paste(tbl[c(-1,-2,-3,-4, -length(tbl))], collapse="\n"), file=texfile)
  }
  return(tbl)
};

#' Create formula for use with felm
#'
createfmla <- function(yvar, xvars, interactions, fe = "0", iv="0", cluster="state") {
  rhs <- paste(xvars, collapse=" + ")
  rhs <- paste(rhs, " + (", paste(interactions[[1]], collapse = " + "), ")*(",
               paste(interactions[[2]], collapse = " + "), ")")
  if (!is.null(fe)) {
    rhs <- paste(rhs," | ", fe, " | ", iv , " | ", paste(cluster, collapse=" + "))
  }
  return(list(as.formula(paste(yvar, "~", rhs, sep=" ")), rhs))
}

policyreg <- function(df, # data
                      yvar, # outcome
                      pols, # policies
                      bvars, # behavior (NULL if not behavior)
                      x, # controls
                      interactions,
                      iv,
                      L=14) {

  if (L==0) {
    p <- pols
    b <- bvars
  } else {
    p <- sprintf("lag(%s, %d)", pols, L)
    b <- sprintf("lag(%s, %d)", bvars, L)
  }
  fmla <- createfmla(yvar, c(p, b, x), interactions, iv=iv)
  m <- felm(fmla[[1]], data=df)
  w <- rep(1, length(p))
  peff <- c(sum(coef(m)[p]*w), sqrt(t(w) %*% (vcov(m)[p,p]) %*% w ))
  if (!is.null(bvars)) {
    w <- colMeans(subset(df, df$date>=as.Date("2020-04-01") &
                             df$date<=as.Date("2020-04-10"))[,bvars])
    beff <- c(sum(coef(m)[b]*w), sqrt(t(w) %*% (vcov(m)[b,b]) %*% w ))
  } else {
    beff <- NULL
  }
  return(list(reg=m, peff=peff, beff=beff))
}


mainregressions <- function(df, # data
                       yvar,
                       pols, # policy variables
                       bvars, # behavior variables
                       infovars,
                       tvars,
                       xlist,
                       interactions,
                       ivlist,
                       L=14) {


  plist <- list(pols, c("pmask.april","pmask.may", pols[-1]))
  plist <- list(pols,c("pmaskbus","pk12","pindex"))



  stopifnot(length(xlist)==length(interactions))
  stopifnot(length(xlist)==length(ivlist))
  ijs <- expand.grid(1:length(plist), 1:length(xlist))

  pbiy <- apply(ijs, 1, function(ij) {
    policyreg(df, yvar, plist[[ij[1]]], bvars,
              c(sprintf("lag(%s, %d)", infovars[[ij[2]]], L),
                tvars,
                xlist[[ij[2]]]),
              interactions[[ij[2]]], iv[[ij[2]]], L=L)
  })

  ## # Get subset included in regressions
  ## df$idx <- 1:nrow(df)
  ## tmp <- policyreg(df, "idx", plist[[1]], bvars,
  ##                  c(sprintf("lag(%s, %d)", infovars, L),
  ##                    xlist[[1]]),
  ##                  interactions[[1]], iv[[ij[2]]], L=L)
  ## df$insample <- NA
  ## df$insample[tmp$reg$response] <- 1


  piy <- apply(ijs, 1, function(ij) {
    policyreg(df, yvar, plist[[ij[1]]], NULL,
              c(sprintf("lag(%s, %d)", infovars[[ij[2]]], L),
                tvars,
                xlist[[ij[2]]]),
              interactions[[ij[2]]], iv[[ij[2]]], L=L)
  })

  ijs <- expand.grid(1:length(bvars), 1:length(plist))
  pib <- list()
  for (k in 1:length(xlist)) {
    pib[[k]] <- apply(ijs, 1, function(ij) {
      policyreg(df, bvars[ij[1]], plist[[ij[2]]], NULL,
                c(infovars[[k]],
                  xlist[[k]]),
                interactions[[k]], "0", L=0)
    })
  }


  ijs <- expand.grid(1:length(pols), 1:length(xlist))
  ip <- apply(ijs, 1, function(ij) {
    policyreg(df, pols[[ij[1]]],  NULL,  bvars,
              c(infovars[[ij[2]]],
                xlist[[ij[2]]]),
              interactions[[ij[2]]], "0", L=L)
  })

  return(list(pib=pib, pbiy=pbiy, piy=piy, ip=ip))
}


statevarregexp <- gsub("\\)","\\\\)",gsub("\\(","\\\\(",statevars))
statevarregexp <- paste("^(",paste(c("Constant",statevarregexp), collapse="|"),")",sep="")

printstars <- function(est, se, starp=c(0.1, 0.05, 0.01)) {
  stars <- paste(rep("*", sum(pnorm(-abs(est), sd=se)<starp/2)), collapse="")
  if (length(stars)==0 || stars=="") return(sprintf("%.3f",est))
  return(sprintf("%.3f$^{%s}$", est, stars))
}

showhtmltables <- function(pib, pbiy, piy, ip=NULL) {

  omit <- c(statevarregexp,
            "^month") #"testratedc:",)
  omit.labels <- c("state variables",
                   "Month : state variables")

  if (!(is.null(pib))) {
    cat("\n### Policies and Behavior\n")

    peff <- sapply(pib, function(x) x$peff[1])
    sep <- sapply(pib, function(x) x$peff[2])
    cnames <- sapply(pib, function(x) colnames(x$reg$response))
    stargazer(lapply(pib, function(x) x$reg),
              type="html",
              title="Policies and Behavior",
              #dep.var.labels=cnames,
              dep.var.labels.include=FALSE,
              column.labels=cnames,
              omit=omit,
              omit.labels=omit.labels,
              omit.stat=c("f", "ser"), model.names=FALSE,
              model.numbers=TRUE,
              add.lines = list("", "","",
                               "\\hline")
              # add.lines = list(c("sum Policy",sprintf("%.3f",peff)),
              #                  c("",sprintf("(%.3f)",sep)),
              #                  "\\hline")
              )

  }


  if (!(is.null(ip))) {
    cat("\n### Policies and Information\n")

    peff <- sapply(ip, function(x) x$peff[1])
    sep <- sapply(ip, function(x) x$peff[2])
    cnames <- sapply(ip, function(x) colnames(x$reg$response))
    stargazer(lapply(ip, function(x) x$reg),
              type="html",
              title="Policies and Information",
              #dep.var.labels=cnames,
              dep.var.labels.include=FALSE,
              column.labels=cnames,
              omit=omit,
              omit.labels=omit.labels,
              omit.stat=c("f", "ser"), model.names=FALSE,
              model.numbers=TRUE,
              # add.lines = list(c("sum Policy",sprintf("%.3f",peff)),
              #                  c("",sprintf("(%.3f)",sep)),
              #                  "\\hline")
              add.lines=list(c("sum behavior",
                               sprintf("%.3f",sapply(ip, function(x) x$beff[1]))),
                             c("",
                               sprintf("(%.3f)",sapply(ip, function(x) x$beff[2]))),
                             "\\hline")
    )
  }

  if (!(is.null(pbiy))) {
    ylbl <- colnames(pbiy[[1]]$reg$response)
    if (ylbl=="dlogdd") ylbl <- "Death Growth"
    if (ylbl=="dlogdc") ylbl <- "Case Growth"

    cat(sprintf("\n### Policy, Behavior, and %s\n",ylbl))

    stargazer(lapply(pbiy, function(x) x$reg),
              type="html", dep.var.labels.include=FALSE,
              title=ylbl,
              omit=omit,
              omit.labels=omit.labels,
              #column.labels=c("OLS","IV"),
              #column.separate=c(2,2),
              add.lines=list(c("sum policies",
                               sprintf("%.3f",sapply(pbiy, function(x) x$peff[1]))),
                             c("",
                               sprintf("(%.3f)",sapply(pbiy, function(x) x$peff[2]))),
                             c("sum behavior",
                               sprintf("%.3f",sapply(pbiy, function(x) x$beff[1]))),
                             c("",
                               sprintf("(%.3f)",sapply(pbiy, function(x) x$beff[2])))
                             ),
              omit.stat=c("f", "ser"), model.names=FALSE,
              model.numbers=TRUE)
  }

  if (!(is.null(piy))) {
    ylbl <- colnames(piy[[1]]$reg$response)
    if (ylbl=="dlogdd") ylbl <- "Death Growth"
    if (ylbl=="dlogdc") ylbl <- "Case Growth"

    cat(sprintf("\n### Policy and %s\n",ylbl))

    stargazer(lapply(piy, function(x) x$reg),
              type="html", dep.var.labels.include=FALSE,
              title=ylbl,
              omit=omit,
              omit.labels=omit.labels,
              #column.labels=c("OLS","IV"),
              #column.separate=c(2,2),
              add.lines=list(c("sum policies",
                               sprintf("%.3f",sapply(piy, function(x) x$peff[1]))),
                             c("",
                               sprintf("(%.3f)",sapply(piy, function(x) x$peff[2])))
                             ),
              omit.stat=c("f", "ser"), model.names=FALSE,
              model.numbers=TRUE)

  }




  return(NULL)
}


savetextables <- function(pib, pbiy, piy, prefix, ip = NULL,
                          rootdir=system("git rev-parse --show-toplevel", intern=TRUE)[1])
{
  omit <- c(statevarregexp,
            "^month")
  omit.labels <- c("state variables",
                   "Month $\\times$ state variables")

  if (!(is.null(pib))) {
    peff <- sapply(pib, function(x) x$peff[1])
    sep <- sapply(pib, function(x) x$peff[2])
    cnames <- sapply(pib, function(x) colnames(x$reg$response))
    tbl <- capture.output(stargazer(
        lapply(pib, function(x) x$reg),
        type="latex",
        title="Policies and Behavior",
        #dep.var.labels=c("Trend Information","Lag Cases Information"), #c(bvars,bvars),
        dep.var.labels.include=FALSE,
        column.labels=cnames,
        omit=omit,
        omit.labels=omit.labels,
        omit.stat=c("f", "ser"), model.names=FALSE,
        model.numbers=TRUE,
        df=FALSE, header=FALSE,
        no.space=TRUE,
        column.sep.width="1pt",
        add.lines = list(c("$\\sum_j \\mathrm{Policy}_j$",
                           sapply(1:length(peff), function(i) printstars(peff[i], sep[i]))),
                         c("",sprintf("(%.3f)",sep))))
        )
    tbl <- relabel(tbl)
    tbl <- gsub("Model (\\d)", "\\(\\1\\)", tbl)
    #tbl <- gsub("No","Yes", tbl)
    texfile <- sprintf("%s/tex/tables_and_figures/%s-pib.tex",rootdir,prefix)
    cat(paste(tbl[c(-1,-2,-3,-4, -length(tbl))], collapse="\n"), file=texfile)
  }

  if (!(is.null(pbiy))) {
    yvar <- colnames(pbiy[[1]]$reg$response)
    if (yvar=="dlogdd") ylbl <- "Death Growth"
    if (yvar=="dlogdc") ylbl <- "Case Growth"


    tbl <- capture.output(stargazer(
        lapply(pbiy, function(x) x$reg),
        type="latex", dep.var.labels.include=FALSE,
        title=ylbl,
        omit=omit,
        omit.labels=omit.labels,
        column.labels=c(yvar),
        column.separate=c(length(pbiy)),
        add.lines=list(c("$\\sum_j \\mathrm{Policy}_j$",
                         sapply(pbiy, function(x) printstars(x$peff[1], x$peff[2]))),
                       c("",
                         sprintf("(%.3f)",sapply(pbiy, function(x) x$peff[2]))),
                       c("$\\sum_k w_k \\mathrm{Behavior}_k$",
                         sapply(pbiy, function(x) printstars(x$beff[1], x$beff[2]))),
                       c("",
                         sprintf("(%.3f)",sapply(pbiy, function(x) x$beff[2])))
                       ),
        omit.stat=c("f", "ser"), model.names=FALSE,
        df=FALSE, header=FALSE,
        no.space=TRUE,
        column.sep.width="1pt",
        model.numbers=TRUE))
    tbl <- relabel(tbl)
    tbl <- gsub("Model (\\d)", "\\(\\1\\)", tbl)
    #tbl <- gsub("No","Yes", tbl)
    texfile <- sprintf("%s/tex/tables_and_figures/%s-pbiy.tex",rootdir,prefix)
    cat(paste(tbl[c(-1,-2,-3,-4, -length(tbl))], collapse="\n"), file=texfile)

  }

  if (!(is.null(piy))) {

   #  yvar <- colnames(pbiy[[1]]$reg$response)
    ylbl <- colnames(piy[[1]]$reg$response)
    if (ylbl=="dlogdd") ylbl <- "Death Growth"
    if (ylbl=="dlogdc") ylbl <- "Case Growth"

    tbl <- capture.output(stargazer(
        lapply(piy, function(x) x$reg),
        type="latex",
        dep.var.labels.include=FALSE,
        title=ylbl,
        omit=omit,
        omit.labels=omit.labels,
        column.labels=c(yvar),
        column.separate=c(length(piy)),
        add.lines=list(c("$\\sum_j \\mathrm{Policy}_j$",
                         sapply(piy, function(x) printstars(x$peff[1], x$peff[2]))),
                       c("",
                         sprintf("(%.3f)",sapply(piy, function(x) x$peff[2])))
                       ),
        omit.stat=c("f", "ser"), model.names=FALSE,
        no.space=TRUE,
        column.sep.width="1pt",
        df=FALSE, header=FALSE,
        model.numbers=TRUE))
    tbl <- relabel(tbl)
    tbl <- gsub("Model (\\d)", "\\(\\1\\)", tbl)
    #tbl <- gsub("No","Yes", tbl)
    texfile <- sprintf("%s/tex/tables_and_figures/%s-piy.tex",rootdir,prefix)
    cat(paste(tbl[c(-1,-2,-3,-4, -length(tbl))], collapse="\n"), file=texfile)
  }


  if (!(is.null(ip))) {

  beff <- sapply(ip, function(x) x$beff[1])
  sep <- sapply(ip, function(x) x$beff[2])
  cnames <- sapply(ip, function(x) colnames(x$reg$response))
  tbl <- capture.output(stargazer(
    lapply(ip, function(x) x$reg),
    type="latex",
    title="Inofrmation and Policies",
    #dep.var.labels=c("Trend Information","Lag Cases Information"), #c(bvars,bvars),
    dep.var.labels.include=FALSE,
    column.labels=cnames,
    omit=omit,
    omit.labels=omit.labels,
    omit.stat=c("f", "ser"), model.names=FALSE,
    model.numbers=TRUE,
    df=FALSE, header=FALSE,
    no.space=TRUE,
    column.sep.width="1pt",
    add.lines = list(c("$\\sum_j \\mathrm{Behavior}_j$",
                       sapply(1:length(beff), function(i) printstars(beff[i], sep[i]))),
                     c("",sprintf("(%.3f)",sep))))
  )
  tbl <- relabel(tbl)
  tbl <- gsub("Model (\\d)", "\\(\\1\\)", tbl)
  #tbl <- gsub("No","Yes", tbl)
  texfile <- sprintf("%s/tex/tables_and_figures/%s-ip.tex",rootdir,prefix)
  cat(paste(tbl[c(-1,-2,-3,-4, -length(tbl))], collapse="\n"), file=texfile)

  }

  return(NULL)
}


figdev <- function(filename, width=8, height=6, pointsize=12) {
  cairo_pdf(filename, width=width, height=height, pointsize=pointsize,
            family="serif")
}

escapeforregexp <- function(string) {
  string <- gsub("\\(", "\\\\(",string)
  string <- gsub("\\)", "\\\\)",string)
  string <- gsub("\\.", "\\\\.",string)
  return(string)
}

dieff_table <- function(pib, pbiy,  piy, policies=pols, behaviors=bvars, nsum=length(policies)) {
  stopifnot(length(pib)==length(behaviors))
  names(pib) <- behaviors
  pi <- rep(NA, length(policies))
  alpha <- rep(NA, length(behaviors))
  beta <- matrix(NA, nrow=length(policies), ncol=length(behaviors))
  names(pi) <- rownames(beta) <- policies
  colnames(beta) <- names(alpha) <- behaviors
  pi <- sapply(policies,
               function(p) pbiy[grep(sprintf("(^|lag\\()%s(, |$)",escapeforregexp(p)),names(pbiy))])
  alpha <- sapply(behaviors,
                  function(b) pbiy[grep(sprintf("(^|lag\\()%s(, |$)",escapeforregexp(b)),names(pbiy))])
  for (b in behaviors) {
    beta[,b] <- sapply(policies, function(p) {
      i <- grep(sprintf("(^|lag\\()%s(, |$)",escapeforregexp(p)),names(pib[[b]]))
      if (length(i)==0) return(0)
      else return( pib[[b]][i])
    })
  }

  tot <- sapply(policies,
                function(p) piy[grep(sprintf("(^|lag\\()%s(, |$)",escapeforregexp(p)),names(piy))])


  tbl <- cbind(pi, beta %*% alpha, pi + beta %*% alpha, tot,
  (pi + beta %*% alpha + tot)/2, pi + beta %*% alpha - tot)
  if (nsum==nrow(tbl)) {
    tbl <- rbind(tbl[1:nsum,], colSums(tbl[1:nsum,]))
    rownames(tbl) <- c(policies, "$\\sum_j \\mathrm{Policy}_j$")
  } else {
    tbl <- rbind(tbl[1:nsum,], colSums(tbl[1:nsum,]), tbl[(nsum+1):nrow(tbl),])
    rownames(tbl) <- c(policies[1:nsum], "$\\sum_j \\mathrm{Policy}_j$",
                       policies[(nsum+1):length(policies)])
  }
  colnames(tbl) <- c("Direct","Indirect", "Total", "PI$\\to$Y Coef.",
                     "Average", "Difference")
  return(tbl)
}
