#' gg_fp
#'
#' make a forest plot wiht ggplot2.
#' @param data data used in `gg_fp()`. A `data.frame`. 
#' @param x_axis variable showed in `x` axis. A column in `data` using Non-standard evaluation. 
#' @param point point value showed in forest polt. A column in `data` using Non-standard evaluation.
#' @param low the lower 95% CI for point value. A column in `data` using Non-standard evaluation.
#' @param up the upper 95% CI for point value. A column in `data` using Non-standard evaluation.
#' @param group_var variable used for identify the group. A column in `data` using Non-standard evaluation.
#' @param facet_var variable used for identify the facet. A column in `data` using Non-standard evaluation.
#' @param zero_line A dotted line in forest plot. The default value is 0. A `1` length numeric vector.
#' @param point_space The spacing between points in the same group. A `1` length numeric vector.
#' @param font_size Font size in forest plot. A `1` length numeric vector. 
#' @param facet_color facet color in forest plot. A character vector. `facet_color` length must same as `unique(facet_var)`.
#' @param group_color group color in forest plot. A character vector. `group_color` length must same as `unique(group_var)`.
#' @param point_shape point shape in forest plot. An integer vector. `point_shape` length must same as `unique(group_var)`.
#' @param label.x x label in in forest plot. The default is 'x axis'. A `1` length character vector. 
#' @param label.y y label in in forest plot. The default is 'y axis'. A `1` length character vector. 
#' @param point.digit The decimal number in forest plot. The default is `2` decimal digits `(point.digit = 0.01)`. 
#'                    A `1` length character vector. You can set it to 0.1 to display 1 decimal digit. Set 0.001 to display 3 decimal digits, etc.
#'
#' @return a plot with the class of `gg` and `ggplot`.
#' @export

gg_fp <- function(data, x_axis, point, low, up,
                  group_var, facet_var,
                  zero_line = 0, point_space = 0.6, font_size = 4,
                  facet_color, group_color,
                  point_shape,
                  label.x = 'x axis', label.y = 'y axis',
                  point.digit = 0.01) {
  
  # ???????? ==========================================
  group_number = data %>% select({{group_var}}) %>% distinct() %>% nrow() # ??????
  group_color_number = length(group_color) # ??????????   
  
  if(!(group_number == group_color_number)) # ??????????????????????????
    stop(glue('There are {group_number} elements in group_var, 
               but only {group_color_number} colors in group_color are provided.'))
  
  group_shape_number = length(point_shape)
  
  if(!(group_number == group_shape_number)) 
    stop(glue('There are {group_number} elements in group_var, 
               but only {group_shape_number} shapes in point_shape are provided.'))
  
  facet_number = data %>% select({{facet_var}}) %>% distinct() %>% nrow()
  facet_color_number = length(facet_color) 
  
  if(!(facet_number == facet_color_number)) 
    stop(glue('There are {facet_number} facets in facet_var, 
               but only {facet_color_number} colors in facet_color are provided.'))
  
  
  # ???????????? ======================================
  pd = position_dodge(point_space)
  
  # main plot =========================================
  p1 = 
    data %>% 
    ggplot(aes(x               = {{x_axis}}, 
               y               = {{point}}, 
               group           = {{group_var}}, 
               color           = {{group_var}}, 
               shape           = {{group_var}})) + 
    
    geom_hline(yintercept      = zero_line,
               linetype        = "dashed",
               colour          = "black",
               size            = 0.5)+
    
    geom_point(position        = pd,
               size            = 3)+
    
    geom_errorbar(aes(ymin     = {{low}},
                      ymax     = {{up}}), 
                  position = pd,
                  width    = 0,
                  size     = 0.8) +
    theme_bw()
  
  # legend set =======================================
  p2 = 
    p1 + 
    theme(legend.position   = "top")+
    
    theme(legend.key        = element_blank())+
    
    theme(legend.background = element_rect(fill   = "white", 
                                           colour = "black"))+
    
    theme(legend.text       = element_text(size   = 10 + font_size, 
                                           color  = 'black'))+
    
    theme(legend.key.height = unit(0.3, "cm"))+
    
    theme(legend.title      = element_blank())
  
  # axis and title set ===============================
  p3 = 
    p2 +
    
    xlab(label.x)+
    
    theme(axis.text.y = element_text(size  = 12 + font_size, 
                                     angle = 0,
                                     color = 'black'))+
    
    theme(axis.text.x = element_text(size  = 12 + font_size, 
                                     color = 'black'))+
    
    theme(axis.title  = element_text(size  = 14 + font_size)) +
    
    scale_y_continuous(label.y, 
                       labels              = scales::number_format(accuracy = point.digit))
  
  # color set ========================================
  p4 = 
    p3 +
    
    scale_shape_manual(values  = point_shape)+
    
    scale_colour_manual(values = c(group_color))
  
  
  # other set ========================================
  p5 = 
    p4 +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  # facet set ========================================
  
  ridiculous_strips <- 
    
    ggh4x::strip_themed(
      
      background_x =  
        ggh4x::elem_list_rect(fill = c(facet_color)))
  
  
  
  
  p6 = p5 + 
    ggh4x::facet_wrap2(vars({{facet_var}}), 
                       strip = ridiculous_strips, 
                       ncol = 2, 
                       scales = 'free_y', 
                       labeller = label_parsed) +
    
    theme(strip.text = element_text(size = 14 + font_size, 
                                    face = 'bold'))
  
  p6 
}


