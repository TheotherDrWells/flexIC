#' @title Compare Pre- and Post-flexIC Marginals
#' @name plot_marginals_grid
#' @title Run Iman–Conover Rank-Preserving Transform
#' @param original Original (pre-transformed) data.
#' @param flex_out Output from flexIC() or a data.frame.
#' @param bins Number of histogram bins.
#' @param after_lab Label to display for the post-flexIC panel.
#' @export
plot_marginals_grid<-function(original,flex_out,bins=40,after_lab='FlexIC epsilon = 0.04')
  {if(!requireNamespace('ggplot2',quietly=TRUE))stop('install ggplot2');
  if(is.list(flex_out)&&!is.null(flex_out$data))flex_out<-flex_out$data;original<-as.matrix(original);
  flex_out<-as.matrix(flex_out);if(ncol(original)>4){message('Showing first 4 of ',ncol(original),' variables.');
    original<-original[,1:4,drop=FALSE];flex_out<-flex_out[,1:4,drop=FALSE]};vnames<-colnames(original);
    if(is.null(vnames))vnames<-paste0('V',seq_len(ncol(original)));
    make_long<-function(mat,lbl)data.frame(value=as.vector(mat),var=factor(rep(vnames,each=nrow(mat)),levels=vnames),stage=lbl);
    long<-rbind(make_long(flex_out,after_lab),make_long(original,'Original'));
ggplot2::ggplot(long,ggplot2::aes(value))+ggplot2::facet_grid(stage~var,scales='free_x')+
      ggplot2::geom_histogram(bins=bins,fill='grey80',colour='black',linewidth=.25)+
      ggplot2::theme_bw(base_size=9)+
      ggplot2::theme(strip.text.y=ggplot2::element_text(angle=0),panel.spacing=ggplot2::unit(0.8,'lines'))+
      ggplot2::labs(title='Marginals Before & After FlexIC',x=NULL,y='Count')}
