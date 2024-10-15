plotCI <- function(df,
                   y,
                   ci_l,
                   ci_u,
                   variable,
                   domain=NULL,
                   ylim=NULL,
                   specs=NULL) {
  eval(parse(text=paste0("y_vals <- df$",y)))
  eval(parse(text=paste0("ci_l <- df$",ci_l)))
  eval(parse(text=paste0("ci_u <- df$",ci_u)))
  plot(y_vals,
       ylim = c(min(ci_l),max(ci_u)),
       type="b",
       ylab = "Estimate",
       xlab = variable,
       xaxt = 'n',
       col="blue")
  title(paste0(y," by ",variable," - ",domain," ",specs),cex.main = 1,font.main= 1,col.main="blue")
  axis(side = 1, at = c(1:nrow(df)), 
       labels = c(1:nrow(df)), las=1, cex.axis=0.7)
  x_points <- 1:nrow(df)
  polygon(
    x = c(x_points,rev(x_points)),
    y = c(ci_l[1:nrow(df)],rev(ci_u[1:nrow(df)])), 
    col = rgb(0, 0, 1, 0.2), border = NA)
}


