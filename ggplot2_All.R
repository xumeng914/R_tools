library(ggplot2)
require(ggplot2)
data(diamonds)
set.seed(42)
small <- diamonds[sample(nrow(diamonds), 1000), ]
head(small)


############################  散点图  #################################

p <- ggplot(data = small, mapping = aes(x = carat, y = price))
p + geom_point()

p <- ggplot(data=small, mapping=aes(x=carat, y=price, shape=cut))
p+geom_point()

p <- ggplot(data=small, mapping=aes(x=carat, y=price, shape=cut, colour=color))
p+geom_point()


############################  直方图  ################################

ggplot(small)+geom_histogram(aes(x=price))

ggplot(small)+geom_histogram(aes(x=price, fill=cut))

ggplot(small)+geom_histogram(aes(x=price, fill=cut), position="dodge")

ggplot(small)+geom_histogram(aes(x=price, fill=cut), position="fill")

##############################  柱状图 #########################################
ggplot(small)+geom_bar(aes(x=clarity))

ggplot()+geom_bar(aes(x=c(LETTERS[1:3]),y=1:3), stat="identity")

######################################################################
ggplot(small)+geom_density(aes(x=price, colour=cut))
ggplot(small)+geom_density(aes(x=price,fill=clarity))
ggplot(small)+geom_boxplot(aes(x=cut, y=price,fill=color))

#######################################################################
geom_abline 	geom_area 	
geom_bar 		geom_bin2d
geom_blank 		geom_boxplot 	
geom_contour 	geom_crossbar
geom_density 	geom_density2d 	
geom_dotplot 	geom_errorbar
geom_errorbarh 	geom_freqpoly 	
geom_hex 		geom_histogram
geom_hline 		geom_jitter 	
geom_line 		geom_linerange
geom_map 		geom_path 	
geom_point 		geom_pointrange
geom_polygon 	geom_quantile 	
geom_raster 	geom_rect
geom_ribbon 	geom_rug 	
geom_segment 	geom_smooth
geom_step 		geom_text 	
geom_tile 		geom_violin
geom_vline

#############################  标尺  #############################################
ggplot(small)+
  geom_point(aes(x=carat, y=price, shape=cut, colour=color))+
  scale_y_log10()+scale_colour_manual(values=rainbow(7))

#################################  回归线  #############################
ggplot(small, aes(x=carat, y=price))+
  geom_point()+scale_y_log10()+stat_smooth()
######################################################################
stat_abline       stat_contour      stat_identity     stat_summary
stat_bin          stat_density      stat_qq           stat_summary2d
stat_bin2d        stat_density2d    stat_quantile     stat_summary_hex
stat_bindot       stat_ecdf         stat_smooth       stat_unique
stat_binhex       stat_function     stat_spoke        stat_vline
stat_boxplot      stat_hline        stat_sum          stat_ydensity

###############################  坐标变换  ##############################################
ggplot(small)+geom_bar(aes(x=cut, fill=cut))+coord_flip()


##################################  极坐标  ############################
ggplot(small)+geom_bar(aes(x=factor(1), fill=cut))+coord_polar(theta="y")
ggplot(small)+geom_bar(aes(x=factor(1), fill=cut))+coord_polar()
ggplot(small)+geom_bar(aes(x=clarity, fill=cut))+coord_polar()

################################ 分面  #######################################
ggplot(small, aes(x=carat, y=price))+
  geom_point(aes(colour=cut))+
  scale_y_log10() +facet_wrap(~cut)+stat_smooth()








