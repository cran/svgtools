### svgtools
### Paket zum Anpassen von SVG-Vorlagen

### DATEIMANAGEMENT ----

#' Read SVG file and return XML document
#' @param file A string, a connection, or a raw vector. See \code{\link[xml2]{read_xml}}.
#' @param enc Encoding (default 'UTF-8').
#' @param summary Show summary of SVG file (\code{\link{summary_svg}})? (default FALSE)
#' @param display Display SVG on standard display port (\code{\link{display_svg}})? (default FALSE)
#' @return XML document with SVG content
#' @examples
#' fpath <- system.file("extdata", "fig1.svg", package="svgtools")
#' svg <- read_svg(file = fpath, summary = TRUE)
#' @export
read_svg <- function(file, enc = "UTF-8", summary = FALSE, display = FALSE) {
  
  # read xml
  svg_in <- xml2::read_xml(x = file, encoding = enc, options = c("PEDANTIC","NOBLANKS","NSCLEAN"))
  svg_in <- xml2::xml_ns_strip(svg_in)
  
  # print summary
  if (summary) {summary_svg(svg_in)}
  
  # print svg
  if (display) {display_svg(svg_in)}
  
  # set svg_obj class
  class(svg_in) <- c(class(svg_in),"svg_obj")
  
  # return
  return(svg_in)
  
}

#' Print summary of SVG file structure in console
#' @param svg XML document with SVG content.
#' @details Prints helpful information to verify the content of the SVG file:
#' \itemize{
#' \item Named groups (XML elements 'g' with attribute 'id') and number of their child elements
#' \item Available frames (XML elements 'rect' with attribute 'id')
#' \item Used fonts, font sizes and font colors (in any XML elements with attributes 'font-family', 'font-size', 'fill' and 'stroke')
#' }
#' @examples
#' #read SVG file
#' fpath <- system.file("extdata", "fig1.svg", package="svgtools")
#' svg <- read_svg(file = fpath)
#' 
#' #show a summary of SVG file
#' summary_svg(svg = svg)
#' @export
summary_svg <- function(svg) {
  
  # Available Frames
  rects <- xml2::xml_find_all(svg, "/svg/rect")
  tlr <- character()
  for (rect in rects) if (xml2::xml_has_attr(rect, "id")) tlr <- c(tlr,xml2::xml_attr(rect, "id"))
  print(paste0("-- Top Level Named Rects (Frames): ",ifelse(length(tlr)>0,paste(tlr,collapse="; "),"NONE")))

  # Named Groups
  groups <- xml2::xml_find_all(svg, "/svg/g")
  tlg <- character()
  for (group in groups)
  {
    if (xml2::xml_has_attr(group, "id"))
    {
      group_name <- xml2::xml_attr(group, "id")
      possibleSymbols <- linesSymbols_guess(group)
      has_lines <- any(xml2::xml_name(xml2::xml_children(group))=="line")
      has_text <- any(xml2::xml_name(xml2::xml_children(group))=="text")
      has_groups <- any(xml2::xml_name(xml2::xml_children(group))=="g")
      found_type <- FALSE
      if (is.null(possibleSymbols) && has_lines && !has_groups) { #keine Symbole oder Gruppen, aber Linien
        tlg <- c(tlg,paste0(group_name, " (lines)"))
        found_type <- TRUE
        next
      }
      if (is.null(possibleSymbols) && !has_lines && has_groups) { #vermutlich Gruppe von Balken
        childElements <- xml2::xml_children(group)
        is_barchart <- TRUE
        is_barchart_with_text <- NA
        for (childElement in childElements)
        {
          if (!any(xml2::xml_name(xml2::xml_children(childElement))=="rect")) is_barchart <- FALSE #sobald eine Gruppe ohne Rect-Elemente gefunden wird, ist es wohl doch kein Balkendiagramm
          if (is.na(is_barchart_with_text)) is_barchart_with_text <- any(xml2::xml_name(xml2::xml_children(childElement))=="text") #entscheide aus der ersten Gruppe, ob Texte vorliegen
          if (is_barchart_with_text != any(xml2::xml_name(xml2::xml_children(childElement))=="text")) is_barchart <- FALSE #sollte sich das aendern, ist es wohl doch kein Balkendiagramm
        }
        if (is_barchart)
        {
          tlg <- c(tlg,paste0(group_name," (",sum(xml2::xml_name(xml2::xml_children(group))=="g")," bar chart",ifelse(is_barchart_with_text," with text"," without text"),")"))
          found_type <- TRUE
          next
        }
      }
      if (!is.null(possibleSymbols) && possibleSymbols != "rect") { #alle Symbole ausser rect sind eindeutig, rect koennten auch Balken sein
        tlg <- c(tlg,paste0(group_name, " (symbols of type '",possibleSymbols,"'",ifelse(has_lines," with lines"," without lines"),")"))
        found_type <- FALSE
        next
      }
      if (!is.null(possibleSymbols) && possibleSymbols == "rect") { #alle Symbole ausser rect sind eindeutig, rect koennten auch Balken sein
        tlg <- c(tlg,paste0(group_name, " (symbols of type '",possibleSymbols,"'",ifelse(has_lines," with lines"," without lines")," OR single bar chart",ifelse(has_text," with text"," without text"),")"))
        found_type <- FALSE
        next
      }
      if (!found_type) tlg <- c(tlg,paste0(group_name, " (UNKNOWN)"))
    }
  }
  if (length(tlg)==0) print("-- Top Level Named Groups: NONE")
  if (length(tlg)>0)
  {
    print("-- Top Level Named Groups (Available Elements):")
    for (tlg1 in tlg) print(tlg1)
  }
  
  # Used Fonts
  text_elements <- xml2::xml_find_all(svg, "text")
  used_fonts <- character()
  for (font in text_elements) {
    used_fonts <- c(used_fonts, xml2::xml_attr(font, "font-family"))
  }
  used_fonts <- unique(used_fonts)
  print(paste0("-- Used Fonts: ",ifelse(length(used_fonts)>0,paste(used_fonts,collapse="; "),"NONE")))
  
  # Used Font Sizes
  used_sizes <- character()
  for (size in text_elements) {
    used_sizes <- c(used_sizes, xml2::xml_attr(size, "font-size"))
  }
  used_sizes <- unique(used_sizes)
  print(paste0("-- Used Font Sizes: ",ifelse(length(used_sizes)>0,paste(used_sizes,collapse="; "),"NONE")))
  
  # Colors
  used_colors <- character()
  for (element in xml2::xml_find_all(svg,xpath="//*")) {
    used_colors <- c(used_colors, xml2::xml_attr(element, "fill"))
    used_colors <- c(used_colors, xml2::xml_attr(element, "stroke"))
  }
  used_colors <- unique(setdiff(stats::na.omit(used_colors),"none"))
  print(paste0("-- Used Colors: ",ifelse(length(used_colors)>0,paste(used_colors,collapse="; "),"NONE")))
  
}

#' Display SVG on standard graphic display port
#' @param svg XML document with SVG content.
#' @param width Desired width (in px) of image (default NULL).
#' @param height Desired height (in px) of image (default NULL).
#' @details Viewport depends on system and IDE. In RStudio the image is displayed under 'Viewer'.\cr
#' If neither width nor height are specified the image will have its size depending on DPI settings. If only one of these is specified, the other one is scaled accordingly.
#' @examples
#' #read SVG file
#' fpath <- system.file("extdata", "fig1.svg", package="svgtools")
#' svg <- read_svg(file = fpath)
#' 
#' #display SVG file in standard viewport
#' display_svg(svg = svg, width = 500)
#' @export
display_svg <- function(svg, width = NULL, height = NULL) {
  rsvg <- rsvg::rsvg(charToRaw(toString(svg)),width = width,height = height) #wandelt das XML-Objekt zunaechst in einen String und dann in Bytes um; wird von von rsvg zu bitmap gerendert
  print(magick::image_read(rsvg))
}

#' Writes SVG to file
#' @param svg XML document with SVG content.
#' @param file Path to file or connection to write to (see \code{\link[xml2]{write_xml}}).
#' @param remove_hidden Should hidden elements (with XML attribute display="none") be removed? (default TRUE)
#' @param flatten Should grouping of SVG elements be removed? (default FALSE)
#' @details Both \code{remove_hidden=TRUE} and \code{flatten=TRUE} do not alter the XML document object itself. Therefore, subsequent calls to \code{\link{stackedBar}} and other functions remain possible.
#' @examples
#' #read SVG file
#' fpath <- system.file("extdata", "fig3.svg", package="svgtools")
#' svg <- read_svg(file = fpath)
#' 
#' #adjust some elements of SVG
#' svg <- stackedBar(svg = svg, frame_name = "frame", group_name = "overall", 
#'                   scale_real = c(0,160), values = c(10,42,106), 
#'                   alignment = "vertical")
#' 
#' \dontrun{
#' #write SVG file to disk and remove all groupings
#' write_svg(svg = svg, file = "myChart.svg", flatten = TRUE)
#' }
#' @export
write_svg <- function(svg, file, remove_hidden = TRUE, flatten = FALSE) {
  
  save_svg <- xml2::xml_new_root(xml2::xml_dtd("svg","-//W3C//DTD SVG 1.1//EN","http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"))
  xml2::xml_add_child(save_svg,svg,copy=TRUE)
  save_svg <- xml2::xml_root(save_svg)
  
  if (remove_hidden)
  {
    hidden <- xml2::xml_find_all(save_svg,"//*[@display='none']")
    if (length(hidden)>0) for (ee in hidden) xml2::xml_remove(ee)
  }
  
  if (flatten)
  {
    while (length(xml2::xml_find_all(save_svg,"//g"))>0)
    {
      group <- xml2::xml_find_first(save_svg,"//g")
      children <- xml2::xml_find_all(group,"./*") 
      if (length(children)>0)
      {
        for (child in children) xml2::xml_add_sibling(group,child,.where="before")
      }
      xml2::xml_remove(group)
    }
  }
  
  # add default namespace
  xml2::xml_set_attr(xml2::xml_find_all(save_svg, "/svg"), "xmlns", "http://www.w3.org/2000/svg")
  
  # write svg
  xml2::write_xml(x = save_svg, file = file)
  #cat("svg gespeichert als: ", file, "\n")
  rm(save_svg)
}

### ALLGEMEINE HILFSFUNKTIONEN ----

#gibt einen Vektor mit SVG-Pfad-Kommandos zurueck
getPathCommands <- function() {
  return(c("M","m","L","l","H","h","V","v","C","c","S","s","Q","q","T","t","A","a","Z","z"))
}

#gibt einen neuen Vektor (min_x,max_x,min_y,max_y) zurueck, nachdem mit neuen Werten verglichen wurde
setNewMinMax <- function(minmax,x,y)
{
  if (x < minmax[1]) minmax[1] <- x
  if (x > minmax[2]) minmax[2] <- x
  if (y < minmax[3]) minmax[3] <- y
  if (y > minmax[4]) minmax[4] <- y
  return(minmax)
}

#setzt einen neuen Startpunkt eines Pfad-Objekts
setNewPathBegin <- function(path,x,y)
{
  path.string <- xml2::xml_attr(path,"d")
  pos1 <- regexpr(pattern = paste(setdiff(getPathCommands(),c("M","m")),collapse = "|"),text = path.string)
  if (pos1>1)
  {
    path.string <- substr(path.string,pos1,nchar(path.string))
    path.string <- paste0("M",x," ",y,path.string)
    xml2::xml_set_attr(x = path,attr = "d",value = path.string)
  }
}

#berechnet die minimale und maximale Ausbreitung der Kurve bis hin zur 10. Kommastelle - die gute Version :-)
getMinMaxCurve <- function(controlpoints)
{
  sequence <- seq(0,1,length=10)
  pointsOnCurve <- bezier::bezier(t = sequence,p = controlpoints[2:3,],start = controlpoints[1,],end = controlpoints[4,],deg = nrow(controlpoints)-1)
  checks <- list(
    "min_x" = list(column = 1, FUNcriterion = min),
    "max_x" = list(column = 1, FUNcriterion = max),
    "min_y" = list(column = 2, FUNcriterion = min),
    "max_y" = list(column = 2, FUNcriterion = max)
  )
  results <- list()
  for (cc in names(checks))
  {
    check <- checks[[cc]]
    old.value <- NA
    seq1 <- sequence
    points1 <- pointsOnCurve
    new.value <- do.call(check$FUNcriterion,list(points1[,check$column]))
    while (is.na(old.value) || abs(old.value-new.value)>10e-10)
    {
      old.value <- new.value
      pos1 <- which(points1[,check$column]==old.value)
      pos1.min <- ifelse(1 %in% pos1,1,min(pos1)-1)
      pos1.max <- ifelse(nrow(points1) %in% pos1,nrow(points1),max(pos1)+1)
      if (pos1.min==1 && pos1.max==nrow(points1)) break #das Minimum bzw. Maximum liegt sowohl am Anfang als auch Ende der Kurve. In dem Fall ist eine weitere Suche hinfaellig.
      seq1 <- seq(seq1[pos1.min],seq1[pos1.max],length=10)
      points1 <- bezier::bezier(t = seq1,p = controlpoints[2:3,],start = controlpoints[1,],end = controlpoints[4,],deg = 3)
      new.value <- do.call(check$FUNcriterion,list(points1[,check$column]))
    }
    results[[cc]] <- new.value
  }
  return(results)
}

#berechnet die minimale und maximale Ausbreitung von Arcs bis hin zur 10. Kommastelle
getMinMaxArc <- function(arcparams)
{
  stop("Error: currently, svgtools does not support A- or a-command in path definition!") #TODO
  return(NULL)
}

# Liest Infos des genannten Rahmens aus und berechnet Skalierung
# TODO: Check, ob sinnvolle Skala eingegeben wurde (?)
frame_and_scaling <- function(svg_in, frame_name, scale_minToMax) {
  
  # frame information
  frame_current <- data.frame("name" = frame_name,
                                    "min_x" = NA,
                                    "max_x" = NA,
                                    "min_y" = NA,
                                    "max_y" = NA,
                                    "diff_x" = NA,
                                    "diff_y" = NA,
                                    "scale_min" = min(scale_minToMax),
                                    "scale_max" = max(scale_minToMax),
                                    "scale_diff" = NA,
                                    "scaling_x" = NA,
                                    "scaling_y" = NA)
  
  frame_index <- which(xml2::xml_attr(xml2::xml_find_all(svg_in, "/svg/rect"), "id") == frame_name)
  fr <- xml2::xml_find_all(svg_in, "/svg/rect")[frame_index]
  
  # Check if frame is available and unique
  if (length(fr) != 1) {
    
    rects <- xml2::xml_find_all(svg_in, "/svg/rect")
    frames_av <- character()
    
    for (rect in rects)
    {
      if (xml2::xml_has_attr(rect, "id"))
      {
        frames_av <- c(frames_av, xml2::xml_attr(rect, "id"))
      }
    }
    stop(paste0("Error: Frame not found or more than one frames with the same name. Available frames:", paste(frames_av, collapse=", ")))
    
  }
  
  # calculate scaling
  frame_current$min_x <- as.numeric(xml2::xml_attr(fr, "x"))
  frame_current$max_x <- frame_current$min_x + as.numeric(xml2::xml_attr(fr, "width"))
  frame_current$min_y <- as.numeric(xml2::xml_attr(fr, "y"))
  frame_current$max_y <- frame_current$min_y + as.numeric(xml2::xml_attr(fr, "height"))
  frame_current$diff_x <- frame_current$max_x - frame_current$min_x
  frame_current$diff_y <- frame_current$max_y - frame_current$min_y
  frame_current$scale_diff <- frame_current$scale_max - frame_current$scale_min
  frame_current$scaling_x <- abs(frame_current$diff_x / frame_current$scale_diff)
  frame_current$scaling_y <- abs(frame_current$diff_y / frame_current$scale_diff)
  
  # return
  return(frame_current)
  
}

# get x-y-coordinates of text element (independent of attributes used)
get_text_coords <- function(text) {
  xyattr <- FALSE
  x <- y <- numeric()
  if (xml2::xml_has_attr(text,"x"))
  {
    xyattr <- TRUE
    x <- as.numeric(xml2::xml_attr(text,"x"))
    y <- as.numeric(xml2::xml_attr(text,"y"))
  } else {
    transformattr <- xml2::xml_attr(text, "transform")
    matrix_values_start <- stringr::str_locate(transformattr, "matrix\\(")
    matrix_values <- stringr::str_sub(transformattr, (matrix_values_start[2] + 1), (nchar(transformattr) - 1))
    matrix_values <- as.numeric(unlist(strsplit(matrix_values, split = " ")))
    x <- matrix_values[5]
    y <- matrix_values[6]
  }
  return(data.frame(x,y,xyattr))
}

# set x-y-coordinates of text element
set_text_coords <- function(text,x,y,xyattr=FALSE) {
  if (xyattr)
  {
    xml2::xml_set_attr(text,"x",x)
    xml2::xml_set_attr(text,"y",y)
  } else {
    transformattr <- xml2::xml_attr(text, "transform")
    matrix_values_start <- stringr::str_locate(transformattr, "matrix\\(")
    matrix_values <- stringr::str_sub(transformattr, (matrix_values_start[2] + 1), (nchar(transformattr) - 1))
    matrix_values <- as.numeric(unlist(strsplit(matrix_values, split = " ")))
    matrix_values[5] <- x
    matrix_values[6] <- y
    transformattr <- paste0("matrix(",paste(matrix_values,collapse=" "), ")")
    xml2::xml_set_attr(text,"transform",transformattr)
  }
}

# extract coordinates (points) from polygon into matrix
get_polygon_coords <- function(polygon) {
  points <- xml2::xml_attr(polygon,"points")
  points <- strsplit(stringr::str_squish(points)," ",fixed = TRUE)[[1]]
  points <- strsplit(points,",",fixed = TRUE)
  points <- do.call(rbind,points)
  points <- apply(points,2,as.numeric)
  return(points)
}

# coerce coordinates from matrix and set them as point attribute
set_polygon_coords <- function(polygon,coords) {
  points <- apply(coords,1,paste,collapse=",")
  points <- paste(points,collapse=" ")
  xml2::xml_set_attr(polygon,"points",points)
}

# recalculates transform matrix of rotated rects and returns string
recalc_transformMatrix <- function(element)
{
  oldmatrix <- xml2::xml_attr(element, "transform")
  x <- as.numeric(xml2::xml_attr(element, "x"))
  y <- as.numeric(xml2::xml_attr(element, "y"))
  width <- as.numeric(xml2::xml_attr(element, "width"))
  height <- as.numeric(xml2::xml_attr(element, "height"))
  cx <- x+width/2.0
  cy <- y+height/2.0
  matrixvalues <- substr(oldmatrix,8,nchar(oldmatrix)-1)
  matrixvalues <- as.numeric(strsplit(matrixvalues,"\\s")[[1]])
  matrixvalues[5] <- -cx*matrixvalues[1] + cy*matrixvalues[2] + cx
  matrixvalues[6] <- -cx*matrixvalues[2] - cy*matrixvalues[1] + cy
  newmatrix <- paste0("matrix(",paste(matrixvalues,collapse=" "),")")
  return(newmatrix)
}

# converts NAs in a logical vector to FALSE
na.as.false <- function(vect)
{
  vect[is.na(vect)] <- FALSE
  return(vect)
}

### BALKENDIAGRAMME ----

# liest angefuehrte Gruppe ein, Check ob eindeutig vorhanden ist. gibt stackedBar Group aus
stackedBar_in <- function(svg_in, group_name) {
  
  # get called group
  named_groups <- xml2::xml_find_all(svg_in, "/svg/g")
  index_group <- which(xml2::xml_attr(named_groups, "id") == group_name)
  if (length(index_group) != 1)
  {
    groups <- xml2::xml_find_all(svg_in, "/svg/g")
    groups_av <- character()
    for (group in groups)
    {
      if (xml2::xml_has_attr(group, "id"))
      {
        groups_av <- c(groups_av, xml2::xml_attr(group, "id"))
      }
    }
    stop(paste0("Error: Group not found or more than one groups with the same name. Available groups:", paste(groups_av, collapse=", ")))
  }
  stackedBarGroup <- named_groups[index_group]
  return(stackedBarGroup)
  
}

# liest n subgruppen aus, checkt ob n subgruppen == n Werte (rows), gibt n subgruppen aus
stackedBar_checkSub <- function(stackedBarGroup, values) {
  
  n_subgroups <- length(xml2::xml_find_all(stackedBarGroup, "g"))
  n_subgroups <- ifelse (n_subgroups == 0, 1, n_subgroups)
  if (n_subgroups != nrow(values)) {
    stop ("Error: Number of (sub)groups not identical to number of rows of values.")
  }
  return(n_subgroups)
  
}

# returns the order of the subgroups depending on the x and y values so that the input values can be mapped correctly to the
# right stacked bar groups. (order can be mixed up in the svg textfile such that it doesn't corresponds to the displayed svg!!)
stackedBar_order_groups <- function(stackedBarGroup, n_subgroups) {
  
  rects_value_y <- rects_value_x <- NULL
  
  if (n_subgroups > 1) {
    
    for (n_children in 1:length(xml2::xml_find_all(stackedBarGroup,"./g"))) {
      
      # get y and x value of rects (min)
      barSet <- xml2::xml_find_all(stackedBarGroup,"./g")[n_children]
      rects <- xml2::xml_find_all(barSet, "./rect")
      rects_value_y <- c(rects_value_y, min(as.numeric(xml2::xml_attr(rects, "y"))))
      rects_value_x <- c(rects_value_x, min(as.numeric(xml2::xml_attr(rects, "x"))))
      
    }
    
    stackedBars_order_y <- order(rects_value_y)
    stackedBars_order_x <- order(rects_value_x)
    
  } else {
    
    stackedBars_order_y <- 1
    stackedBars_order_x <- 1
    
  }
  
  return(data.frame(stackedBars_order_x, stackedBars_order_y))
  
}  

# Rechtecke eines stackedBar bearbeiten, startpos und Breite anpassen
stackedBar_edit_rects <- function(rects, frame_info, value_set, order_rects, alignment, offset=0) {
  
  # check: n values == n bars
  if (length(value_set) != length(rects)) {
    stop ("Error: Number of data values differs from number of bar segments (XML elements 'rect').")
  }
  
  pos_next <- NULL
  
  for (rr in 1:length(value_set))
  {
    rect <- rects[order_rects[rr]]
    if (is.na(value_set[rr]))
    {
      xml2::xml_set_attr(rect, "display", "none")
      next
    } else {
      xml2::xml_set_attr(rect, "display", NULL)
    }
    if (is.null(pos_next)) #erstes Rechteck
    {
      if (alignment=="horizontal")
      {
        xml2::xml_set_attr(rect, "x", frame_info$min_x + offset * frame_info$scaling_x)
        xml2::xml_set_attr(rect, "width", abs(value_set[rr]) * frame_info$scaling_x)
        pos_next <- as.numeric(xml2::xml_attr(rect, "x")) + (value_set[rr] * frame_info$scaling_x)
      }
      if (alignment=="vertical")
      {
        xml2::xml_set_attr(rect, "y", frame_info$max_y - value_set[rr] * frame_info$scaling_y - offset * frame_info$scaling_y)
        xml2::xml_set_attr(rect, "height", abs(value_set[rr]) * frame_info$scaling_y)
        pos_next <- as.numeric(xml2::xml_attr(rect, "y"))
      }
    } else { #weitere Rechtecke
      if (alignment=="horizontal")
      {
        xml2::xml_set_attr(rect, "x", pos_next)
        xml2::xml_set_attr(rect, "width", abs(value_set[rr]) * frame_info$scaling_x)
        pos_next <- pos_next + value_set[rr] * frame_info$scaling_x
      }
      if (alignment=="vertical")
      {
        xml2::xml_set_attr(rect, "y", pos_next - value_set[rr] * frame_info$scaling_y)
        xml2::xml_set_attr(rect, "height", abs(value_set[rr]) * frame_info$scaling_y)
        pos_next <- as.numeric(xml2::xml_attr(rect, "y"))
      }
    }
  }
}

# Textelemente eines stackedBar: richtige Reihenfolge herausfinden
stackedBar_order_text <- function(barLabels) {
  
  labels_value_y <- labels_value_x <- numeric()
  
  for (lb in 1:length(barLabels)) {
    
    # get y value of rects (min)
    text_label <- barLabels[lb]
    text_matrix <- xml2::xml_attr(text_label, "transform")
    matrix_values_start <- stringr::str_locate(text_matrix, "matrix\\(")
    matrix_values <- stringr::str_sub(text_matrix, (matrix_values_start[2] + 1), (nchar(text_matrix) - 1))
    matrix_values <- as.numeric(unlist(strsplit(matrix_values, split = " ")))
    labels_value_y <- c(labels_value_y, matrix_values[length(matrix_values)])
    labels_value_x <- c(labels_value_x, matrix_values[length(matrix_values) - 1])
    
  }
  
  order_labels_x <- order(labels_value_x)
  order_labels_y <- order(-labels_value_y)
  
  return(data.frame(order_labels_x, order_labels_y))
  
}

# Text eines stackedBars bearbeiten: Text tauschen, Position anpassen
stackedBar_edit_text <- function(barLabels, order_labels, value_set, rects, order_rects, decimals, displayLimits, labelPosition, alignment) {
  
  # check: n values == n texts
  if (length(value_set) != length(barLabels)) {
    stop ("Error: Number of data values differs from number of value labels (XML elements 'text').")
  }
  
  for (rr in 1:length(value_set))
  {
    text_toChange <- barLabels[order_labels[rr]]
    
    # change value
    if (!getOption("svgtools.roundAwayFromZero", default = FALSE)) {
      rounded_value <- base::round(value_set[rr],decimals)
    } else {
      rounded_value <- roundAwayFromZero(value_set[rr],decimals)
    }
    xml2::xml_text(text_toChange) <- format(rounded_value,nsmall=decimals,decimal.mark=",",big.mark="",small.mark="")
    
    # comply with displayLimits and completely ignore NA
    xml2::xml_set_attr(text_toChange, "display", NULL)
    if (is.na(value_set[rr])) {
      xml2::xml_set_attr(text_toChange, "display", "none")
      next
    }
    if ((value_set[rr] > min(displayLimits) && value_set[rr] < max(displayLimits)) | (value_set[rr] == 0)) {
      xml2::xml_set_attr(text_toChange, "display", "none")
    }
    
    # change position
    coords <- get_text_coords(text_toChange)
    rect <- rects[order_rects[rr]]
    rectinfo_pos_x <- as.numeric(xml2::xml_attr(rect, "x"))
    rectinfo_pos_y <- as.numeric(xml2::xml_attr(rect, "y"))
    rectinfo_pos_width <- as.numeric(xml2::xml_attr(rect, "width"))
    rectinfo_pos_height <- as.numeric(xml2::xml_attr(rect, "height"))
    if (alignment == "horizontal")
    {
      if (labelPosition == "center") text_pos <- rectinfo_pos_x + (rectinfo_pos_width/2)
      if (labelPosition == "start") text_pos <- ifelse (value_set[rr] >= 0, rectinfo_pos_x + 10, rectinfo_pos_x + rectinfo_pos_width - 10)
      if (labelPosition == "end") text_pos <- ifelse (value_set[rr] >= 0, rectinfo_pos_x + rectinfo_pos_width - 10, rectinfo_pos_x + 10)
      set_text_coords(text_toChange,x=text_pos,y=coords$y,xyattr=coords$xyattr)
      xml2::xml_set_attr(text_toChange, "text-anchor", "middle")
    } else {
      if (labelPosition == "center")
      {
        xml2::xml_set_attr(text_toChange, "dy", ".5em")
        text_pos <- rectinfo_pos_y + (rectinfo_pos_height/2)
      }
      if ((labelPosition == "start" && value_set[rr] >= 0) || (labelPosition == "end" && value_set[rr] < 0))
      {
        xml2::xml_set_attr(text_toChange, "dy", NULL)
        text_pos <- rectinfo_pos_y + rectinfo_pos_height - 10
      }
      if ((labelPosition == "end" && value_set[rr] >= 0) || (labelPosition == "start" && value_set[rr] < 0))
      {
        xml2::xml_set_attr(text_toChange, "dy", "1em")
        text_pos <- rectinfo_pos_y + 10
      }
      set_text_coords(text_toChange,x=coords$x,y=text_pos,xyattr=coords$xyattr)
    }
  }

}

#' Adjust (stacked) bar chart to values on a given scale
#' @description Adjusts the horizontal (XML attribute 'x') or vertical (XML attribute 'y') position as well as width/height of bar segments (XML elements of type 'rect') and optionally value labels (XML elements of type 'text'). Positions are calculated relative to a given frame (XML element of type 'rect') and the position of a data value within the minimum and maximum of a given scale. This process is called scaling.\cr
#' In preparation, it is necessary to name a group (set attribute 'id' of XML element of type 'g') of bar segments (and value labels). Bar segments (and value labels) need to be of the same amount as there are data values for adjustment.\cr
#' It is possible to group several such groups together. Only the outer group needs to be named in that case, for convenience.
#' @param svg XML document with SVG content
#' @param frame_name Name (attribute 'id') of frame (XML element 'rect') for positioning bar segments (and value labels).
#' @param group_name Name (attribute 'id') of group (XML element 'g') containing either bar segments (and value labels) or further groups, containing bar segments (and value labels) themselves.
#' @param scale_real Numeric vector (e.g. \code{c(0,100)}) of arbitrary length. Only minimum and maximum are used for scaling of values.
#' @param values Either a numeric vector, a numeric matrix or a dataframe with only numeric columns.\cr
#' If a vector is given, it corresponds to one bar (group of bar segments and, optionally, value labels).\cr
#' If a matrix or dataframe is given, rows define the value set for several (stacked) bars grouped together.
#' @param alignment Character value. Accepts 'horizontal' (default) or 'vertical'. See details.
#' @param has_labels Are there value labels (of XML type 'text') to adjust? (default TRUE)
#' @param label_position Character value. Accepts 'start', 'center' (default) and 'end'. This refers to the underlying bar segments.
#' @param decimals Integer value defining the number of decimal digits of value labels (default 0). It is possible to set the rounding of the labels to rounding away from zero by \code{options("svgtools.roundAwayFromZero" = TRUE)}.
#' @param display_limits Interval for (small) values, that lead to suppression of the corresponding value labels. If only one value x is given, it is turned into the interval c(-x,x). (default 0 = no suppression) 
#' @param ... Further arguments used internally by \code{\link{referenceBar}}, \code{\link{diffBar}} and \code{\link{percentileBar}}.
#' @return XML document with SVG content
#' @details 'Horizontal' alignment refers to adjustment of the x-coordinates of elements, 'vertical' alignment to adjustment of the y-coordinates.\cr
#' Bar segments and, optionally, value labels may be grouped together in any order in the SVG file. The function will automatically use XML elements from left to right (with \code{alignment='horizontal'}) or bottom to top (with \code{alignment='vertical'}) according to their x/y-coordinates.\cr
#' Furthermore, the SVG file order of several bars grouped together in an outer group is irrelevant. The function will automatically use bars (that is, groups of bar segments and, optionally, value labels) from top to bottom (with \code{alignment='horizontal'}) or left to right (with \code{alignment='vertical'}) according to the lowest x/y-coordinate of any element.\cr
#' Bar segments and value labels (if any) are automatically hidden (XML attribute 'diplay' is set to 'none'), when a value of 0 or NA is provided. Subsequent calls to the function with non-zero or non-NA values make such elements reappear in the output.
#' @examples
#' #read SVG file
#' fpath <- system.file("extdata", "fig3.svg", package="svgtools")
#' svg <- read_svg(file = fpath)
#' 
#' #adjust bars
#' svg <- stackedBar(svg = svg, frame_name = "frame", group_name = "overall",
#'                   scale_real = c(0,160), values = c(9.97,42.42,105.71), 
#'                   alignment = "vertical", has_labels = TRUE, 
#'                   label_position = "end", decimals = 0, display_limits = 10)
#' df.subgroups <- matrix(1:9*8, nrow=3)
#' svg <- stackedBar(svg = svg, frame_name = "frame", group_name = "subgroups", 
#'                   scale_real = c(0,160), values = df.subgroups, 
#'                   alignment = "vertical", display_limits = 10)
#' @export
stackedBar <- function(svg, frame_name, group_name, scale_real, values, alignment = "horizontal", has_labels = TRUE, label_position = "center", decimals = 0, display_limits = 0, ...) {
  
  # check alignment string
  if (!(alignment %in% c("horizontal","vertical"))) stop("Error: Alignment has to be either 'horizontal' or 'vertical'.")
  
  # get frame info and scaling
  frame_info <- frame_and_scaling(svg, frame_name, scale_real)
  
  # if input-values == vector: transform to data.frame to get 1 row
  if (is.null(nrow(values))) {values <- t(data.frame(values))}
  
  # get called group
  stackedBarGroup <- stackedBar_in(svg, group_name)
  
  # get n subgroups
  n_subgroups <- stackedBar_checkSub(stackedBarGroup, values)

  # get order of (sub)groups in xml depending on x-value and depending on y-value
  order_groups <- stackedBar_order_groups(stackedBarGroup, n_subgroups)
  stackedBars_order_x <- order_groups$stackedBars_order_x
  stackedBars_order_y <- order_groups$stackedBars_order_y
  
  # get offsets (if any)
  dotargs <- list(...)
  offset <- rep(0,n_subgroups)
  if (!is.null(dotargs[["offset"]]))
  {
    if (length(dotargs[["offset"]])!=n_subgroups) stop("Internal error: Something went wrong with offsets for stackedBar.")
    offset <- dotargs[["offset"]]
  }
  
  # adjust all rect-elements and text-elements of all groups
  for (bar_nr in 1:n_subgroups) {
    
    # values for barSet
    value_set <- values[bar_nr, ]
    
    ## - RECTS
    # get: barSet, rects of barSet, right ordering
    if (length(xml2::xml_find_all(stackedBarGroup, "./g"))!=0)
    { 
      if (alignment=="horizontal") barSet <- xml2::xml_find_all(stackedBarGroup, "./g")[stackedBars_order_y[bar_nr]]
      if (alignment=="vertical") barSet <- xml2::xml_find_all(stackedBarGroup, "./g")[stackedBars_order_x[bar_nr]]
    }
    if (length(xml2::xml_find_all(stackedBarGroup, "./g"))==0) barSet <- stackedBarGroup #wenn es keine Untergruppen gibt, nimm direkt die übergebene Gruppe
    
    rects <- xml2::xml_find_all(barSet, "./rect")
    order_rects_x <- order(as.numeric(xml2::xml_attr(rects, "x")))
    order_rects_y <- order(-as.numeric(xml2::xml_attr(rects, "y")))
    if (alignment == "horizontal") {order_rects <- order_rects_x}
    if (alignment == "vertical") {order_rects <- order_rects_y}
    
    # edit rects
    stackedBar_edit_rects(rects, frame_info, value_set, order_rects, alignment, offset[bar_nr])

    ## -- TEXT/LABELS
    # get text elements of group and right ordering
    if (has_labels) {
      
      # turn display_limits into an interval
      if (length(display_limits)==1) display_limits <- c(-display_limits,display_limits)
      
      # find texts and get order
      barLabels <- xml2::xml_find_all(barSet, "./text")
      if (length(barLabels)<1) stop("Error: Labels requested but no text elements in (sub)group.") 
      order_textOut <- stackedBar_order_text(barLabels)
      if (alignment == "horizontal") {order_labels <- order_textOut$order_labels_x}
      if (alignment == "vertical") {order_labels <- order_textOut$order_labels_y}
      
      # adjust values and position of text elements
      stackedBar_edit_text(barLabels, order_labels, value_set, rects, order_rects, decimals, display_limits, label_position, alignment)
    
      # z-position of labels (in front of text)
      for (bb in 1:length(barLabels))
      {
        xml2::xml_add_child(barSet,barLabels[bb])
        xml2::xml_remove(barLabels[bb])
      }
    }

  }
  
  # return
  return(svg)
  
}

#' Adjust (stacked) bar chart that is aligned around a reference category
#' @description Adjusts the horizontal (XML attribute 'x') or vertical (XML attribute 'y') position as well as width/height of bar segments (XML elements of type 'rect') and optionally value labels (XML elements of type 'text'). Positions are calculated relative to a given frame (XML element of type 'rect'), a nullvalue and the position of a data value within the minimum and maximum of a given scale. The first n bar segments and, optionally, value labels of each bar (n is called the \code{reference} category) are position to the left (in horizontal alignment) or bottom (in vertical alignment) of the nullvalue, while the others are positioned to the right or top.\cr
#' For further description see \code{\link{stackedBar}}.
#' @param svg XML document with SVG content
#' @param frame_name Name (attribute 'id') of frame (XML element 'rect') for positioning bar segments (and value labels).
#' @param group_name Name (attribute 'id') of group (XML element 'g') containing either bar segments (and value labels) or further groups, containing bar segments (and value labels) themselves.
#' @param scale_real Numeric vector (e.g. \code{c(0,100)}) of arbitrary length. Only minimum and maximum are used for scaling of values.
#' @param values Either a numeric vector, a numeric matrix or a dataframe with only numeric columns.\cr
#' If a vector is given, it corresponds to one bar (group of bar segments and, optionally, value labels).\cr
#' If a matrix or dataframe is given, rows define the value set for several (stacked) bars grouped together.
#' @param reference Reference category (=column number of values). Bar segments up to this category lie to the left (horizontal) or to the bottom (vertical), bar segments above this category lie to the right (horizontal) or the top (vertical) of the bar chart.
#' @param nullvalue Value that defines the "center" of the bar segments (for left/right or bottom/top positioning)
#' @param alignment Character value. Accepts 'horizontal' (default) or 'vertical'. See details.
#' @param has_labels Are there value labels (of XML type 'text') to adjust? (default TRUE)
#' @param label_position Character value. Accepts 'start', 'center' (default) and 'end'. This refers to the underlying bar segments.
#' @param decimals Integer value defining the number of decimal digits of value labels (default 0). It is possible to set the rounding of the labels to rounding away from zero by \code{options("svgtools.roundAwayFromZero" = TRUE)}.
#' @param display_limits Interval for (small) values, that lead to suppression of the corresponding value labels. If only one value x is given, it is turned into the interval c(-x,x). (default 0 = no suppression) 
#' @return XML document with SVG content
#' @details See \code{\link{stackedBar}}.
#' @examples
#' #read SVG file
#' fpath <- system.file("extdata", "fig5.svg", package="svgtools")
#' svg <- read_svg(file = fpath)
#' 
#' #adjust bars
#' values <- matrix(c(1,2,3,4,2,3,4,1,3,4,1,2,4,1,2,3,1,2,3,4)*10, 
#'                  nrow = 5, byrow = TRUE)
#' svg <- referenceBar(svg = svg, frame_name = "frame", group_name = "group",
#'                     scale_real = c(-100,100), values = values, 
#'                     reference = 2, nullvalue = 0)
#' @export
referenceBar <- function(svg, frame_name, group_name, scale_real, values, reference, nullvalue=0, alignment = "horizontal", has_labels = TRUE, label_position = "center", decimals = 0, display_limits = 0) {
  if (nullvalue < min(scale_real) || nullvalue > max(scale_real)) stop("Error: nullvalue has to be in [min(scale_real),max(scale_real)].")
  if (length(dim(values))==1)
  {
    if (length(values)<reference) stop("Error: reference category not found, length of values too small.")
    offset <- nullvalue - min(scale_real) - sum(values[1:reference])
  } else {
    if (ncol(values)<reference) stop("Error: reference category not found, number of columns too small.")
    offset <- rep(nullvalue - min(scale_real),nrow(values))
    offset <- offset - rowSums(values[,1:reference])
  }
  return(stackedBar(svg = svg,
                    frame_name = frame_name,
                    group_name = group_name,
                    scale_real = scale_real,
                    values = values,
                    alignment = alignment,
                    has_labels = has_labels,
                    label_position = label_position,
                    decimals = decimals,
                    display_limits = display_limits,
                    offset = offset))
}

#' Adjust bar chart where bars lie to the left/right or bottom/top of a given nullvalue
#' @description Adjusts the horizontal (XML attribute 'x') or vertical (XML attribute 'y') position as well as width/height of bar segments (XML elements of type 'rect') and optionally value labels (XML elements of type 'text'). Positions are calculated relative to a given frame (XML element of type 'rect'), a nullvalue and the position of a data value within the minimum and maximum of a given scale. Bar segments and, optionally, value labels with values lower than the nullvalue are positioned to the left (in horizontal alignment) or to the bottom (in vertical alignment) of the scaled nullvalue, higher values on the opposite side.\cr
#' For further description see \code{\link{stackedBar}}.
#' @param svg XML document with SVG content
#' @param frame_name Name (attribute 'id') of frame (XML element 'rect') for positioning bar segments (and value labels).
#' @param group_name Name (attribute 'id') of group (XML element 'g') containing either bar segments (and value labels) or further groups, containing bar segments (and value labels) themselves.
#' @param scale_real Numeric vector (e.g. \code{c(0,100)}) of arbitrary length. Only minimum and maximum are used for scaling of values.
#' @param values Either a numeric vector, a numeric matrix or a dataframe with only numeric columns.\cr
#' If a vector is given, it corresponds to one bar (group of bar segments and, optionally, value labels).\cr
#' If a matrix or dataframe is given, rows define the value set for several (stacked) bars grouped together.
#' @param nullvalue Value that defines the "center" of the bar segments (for left/right or bottom/top positioning)
#' @param alignment Character value. Accepts 'horizontal' (default) or 'vertical'. See details.
#' @param has_labels Are there value labels (of XML type 'text') to adjust? (default TRUE)
#' @param label_position Character value. Accepts 'start', 'center' (default) and 'end'. This refers to the underlying bar segments.
#' @param decimals Integer value defining the number of decimal digits of value labels (default 0). It is possible to set the rounding of the labels to rounding away from zero by \code{options("svgtools.roundAwayFromZero" = TRUE)}.
#' @param display_limits Interval for (small) values, that lead to suppression of the corresponding value labels. If only one value x is given, it is turned into the interval c(-x,x). (default 0 = no suppression) 
#' @return XML document with SVG content
#' @details See \code{\link{stackedBar}}.
#' @examples
#' #read SVG file
#' fpath <- system.file("extdata", "fig7.svg", package="svgtools")
#' svg <- read_svg(file = fpath)
#' 
#' #adjust bars
#' df <- data.frame(diff_negative = c(NA,NA,-0.7,NA,-0.33), 
#'                  diff_positive = c(0.4,0.55,NA,0.02,NA))
#' svg <- diffBar(svg = svg, frame_name = "frame", group_name = "group", 
#'                scale_real = c(-1,1), values = df, nullvalue = 0, 
#'                label_position = "end", decimals = 1, display_limits = 0.1)
#' @export
diffBar <- function(svg, frame_name, group_name, scale_real, values, nullvalue=0, alignment = "horizontal", has_labels = TRUE, label_position = "center", decimals = 0, display_limits = c(0,0)) {
  if (nullvalue < min(scale_real) || nullvalue > max(scale_real)) stop("Error: nullvalue has to be in [min(scale_real),max(scale_real)].")
  if (length(dim(values))==1)
  {
    offset <- nullvalue - min(scale_real)
    if (any(na.as.false(values<nullvalue))) offset <- offset - abs(sum(values[na.as.false(values<nullvalue)]))
  } else {
    offset <- rep(nullvalue - min(scale_real),nrow(values))
    for (rr in 1:nrow(values))
    {
      rowvalues <- as.numeric(values[rr,,drop=TRUE])
      if (any(na.as.false(rowvalues<nullvalue))) offset[rr] <- offset[rr] - abs(sum(rowvalues[na.as.false(rowvalues<nullvalue)]))
    }
  }
  return(stackedBar(svg = svg,
                    frame_name = frame_name,
                    group_name = group_name,
                    scale_real = scale_real,
                    values = values,
                    alignment = alignment,
                    has_labels = has_labels,
                    label_position = label_position,
                    decimals = decimals,
                    display_limits = display_limits,
                    offset = offset))
}

#' Adjust (stacked) bar chart representing percentiles
#' @description Adjusts the horizontal (XML attribute 'x') or vertical (XML attribute 'y') position as well as width/height of bar segments (XML elements of type 'rect') and optionally value labels (XML elements of type 'text'). First, values are ordered and transformed to differences between percentiles, providing a starting point and widths/heights for bar segments. Then, positions are calculated relative to a given frame (XML element of type 'rect') and the position of a value within the minimum and maximum of a given scale.\cr
#' No value labels are possible for this type of bar chart.\cr
#' For further description see \code{\link{stackedBar}}.
#' @param svg XML document with SVG content
#' @param frame_name Name (attribute 'id') of frame (XML element 'rect') for positioning bar segments.
#' @param group_name Name (attribute 'id') of group (XML element 'g') containing either bar segments or further groups, containing bar segments themselves.
#' @param scale_real Numeric vector (e.g. \code{c(0,100)}) of arbitrary length. Only minimum and maximum are used for scaling of values.
#' @param values Either a numeric vector, a numeric matrix or a dataframe with only numeric columns.\cr
#' If a vector is given, it corresponds to one bar (group of bar segments).\cr
#' If a matrix or dataframe is given, rows define the value set for several (stacked) bars grouped together.
#' @param alignment Character value. Accepts 'horizontal' (default) or 'vertical'. See details.
#' @return XML document with SVG content
#' @details In contrast to \code{\link{stackedBar}} the value set(s) need(s) to have one more element than bar segments in each group.\cr
#' For everything else, see \code{\link{stackedBar}}.
#' @examples
#' #read SVG file
#' fpath <- system.file("extdata", "fig9.svg", package="svgtools")
#' svg <- read_svg(file = fpath)
#' 
#' #adjust bars
#' set.seed(12345)
#' yy <- -2:2*15
#' percentiles <- data.frame(p5=rnorm(5,350,10)+yy, p25=rnorm(5,450,10)+yy, 
#'                           p50low=rnorm(5,510,10)+yy-1.5, p75=rnorm(5,560,10)+yy, 
#'                           p95=rnorm(5,640,10)+yy)
#' percentiles$p50upp <- percentiles$p50low+3
#' svg <- percentileBar(svg = svg, frame_name = "frame", group_name = "group",
#'                      scale_real = c(250,750), values = percentiles)
#' @export
percentileBar <- function(svg, frame_name, group_name, scale_real, values, alignment = "horizontal") {
  if (is.numeric(values))
  {
    if (length(values)<2) stop("Error: at least two values are needed for percentile bars.")
    values <- values[order(values)]
    offset <- (min(values) - min(scale_real))
    for (vv in 1:(length(values)-1)) values[vv] <- (values[vv+1] - values[vv])
    values <- values[-length(values)]
  } else {
    if (!("data.frame" %in% class(values))) stop("Error: value has to be either a numeric vector or a dataframe.")
    if (ncol(values)<2) stop("Error: at least two values are needed for percentile bars.")
    values <- as.matrix(values)
    for (rr in 1:nrow(values))
    {
      values1 <- values[rr,]
      values1 <- values1[order(values1)]
      values[rr,] <- values1
    }
    offset <- (values[,1] - min(scale_real))
    for (vv in 1:(ncol(values)-1)) values[,vv] <- (values[,vv+1] - values[,vv])
    values <- values[,-ncol(values)]
  }
  return(stackedBar(svg = svg,
                    frame_name = frame_name,
                    group_name = group_name,
                    scale_real = scale_real,
                    values = values,
                    alignment = alignment,
                    has_labels = FALSE,
                    offset = offset))
}

### LINIEN- UND SYMBOLDIAGRAMME ----

linesSymbols_in <- function (svg_in, group_name) {
  
  named_groups <- xml2::xml_find_all(svg_in, "/svg/g")
  index_group <- which(xml2::xml_attr(named_groups, "id") == group_name)
  if (length(index_group) != 1)
  {
    groups <- xml2::xml_find_all(svg_in, "/svg/g")
    groups_av <- character()
    for (group in groups)
    {
      if (xml2::xml_has_attr(group, "id"))
      {
        groups_av <- c(groups_av, xml2::xml_attr(group, "id"))
      }
    }
    stop(paste0("Error: Group not found or more than one groups with the same name. Available groups:", paste(groups_av, collapse=", ")))
  }
  lineGroup <- named_groups[index_group]
}

linesSymbols_guess <- function(group)
{
  deferedType <- NULL
  childElements <- xml2::xml_children(group)
  for (childElement in childElements)
  {
    if (xml2::xml_name(childElement)=="line") next
    for (type1 in c("rect","circle","polygon","path")) #alle ausser linegroup
    {
      if (xml2::xml_name(childElement)==type1)
      {
        if (!is.null(deferedType) && deferedType != type1) return(NA)
        if (is.null(deferedType)) deferedType <- type1
      }
    }
    if (xml2::xml_name(childElement)=="g") #linegroup
    {
      grandchildElements <- xml2::xml_children(childElement)
      if (all(xml2::xml_name(grandchildElements)=="line"))
      {
        if (!is.null(deferedType) && deferedType != "linegroup") return(NA)
        if (is.null(deferedType)) deferedType <- "linegroup"
      }
    }
  }
  return(deferedType)
}

linesSymbols_duplicate_lines <- function (lineGroup, alignment, number) {
  
  newline <- xml2::xml_find_first(lineGroup[[1]], "./line")
  if (alignment=="vertical") width <- abs(as.numeric(xml2::xml_attr(newline, "x2"))-as.numeric(xml2::xml_attr(newline, "x1")))
  if (alignment=="horizontal") width <- abs(as.numeric(xml2::xml_attr(newline, "y2"))-as.numeric(xml2::xml_attr(newline, "y1")))
  for (ll in 2:number)
  {
    newline <- xml2::xml_add_sibling(.x = newline,.value=newline)
    if (alignment=="vertical")
    {
      xml2::xml_set_attr(newline,"x1",as.numeric(xml2::xml_attr(newline, "x1"))+width)
      xml2::xml_set_attr(newline,"x2",as.numeric(xml2::xml_attr(newline, "x2"))+width)
    }
    if (alignment=="horizontal")
    {
      xml2::xml_set_attr(newline,"y1",as.numeric(xml2::xml_attr(newline, "y1"))+width)
      xml2::xml_set_attr(newline,"y2",as.numeric(xml2::xml_attr(newline, "y2"))+width)
    }
  }
  lines_inGroup <- xml2::xml_find_all(lineGroup, "./line")
  return(lines_inGroup)
  
}

linesSymbols_order_lines <- function (lines_inGroup, alignment) {
  
  lines_x1_values <- lines_y1_values <- lines_x2_values <- lines_y2_values <- numeric()
  
  #Korrektur falsch gedrehter Linien (x2<x1 oder y2<y1)
  for (n_lines in 1:length(lines_inGroup)) {
    x1 <- as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "x1"))
    y1 <- as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "y1"))
    x2 <- as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "x2"))
    y2 <- as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "y2"))
    if (alignment=="vertical" && x2<x1)
    {
      xml2::xml_set_attr(x = lines_inGroup[n_lines],attr = "x1",value=x2)
      xml2::xml_set_attr(x = lines_inGroup[n_lines],attr = "x2",value=x1)
    }
    if (alignment=="horizontal" && y2<y1)
    {
      xml2::xml_set_attr(x = lines_inGroup[n_lines],attr = "y1",value=y2)
      xml2::xml_set_attr(x = lines_inGroup[n_lines],attr = "y2",value=y1)
    }
  }
  
  for (n_lines in 1:length(lines_inGroup)) {
    lines_x1_values <- c(lines_x1_values, as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "x1")))
    lines_y1_values <- c(lines_y1_values, as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "y1")))
    lines_x2_values <- c(lines_x2_values, as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "x2")))
    lines_y2_values <- c(lines_y2_values, as.numeric(xml2::xml_attr(lines_inGroup[n_lines], "y2")))
  }
  
  order_lines_x <- order(lines_x1_values)
  order_lines_y <- order(lines_y1_values)
  
  if (alignment == "vertical") {order_lines <- order_lines_x}
  if (alignment == "horizontal") {order_lines <- order_lines_y}
  
  return(order_lines)
  
}

linesSymbols_edit_lines <- function (lines_inGroup, order_lines, frame_info, value_set, alignment) {
  
  if (alignment=="vertical")
  {
    for (n_lines in 1:length(lines_inGroup)) {
      
      line_toChange <- lines_inGroup[order_lines[n_lines]]
      pos <- n_lines
      if (any(is.na(value_set[pos:(pos+1)])))
      {
        xml2::xml_set_attr(line_toChange, "display", "none")
      } else  {
        xml2::xml_set_attr(line_toChange, "display", "NULL")
        xml2::xml_set_attr(line_toChange, "y1", frame_info$max_y - (value_set[pos] - frame_info$scale_min) * frame_info$scaling_y)
        xml2::xml_set_attr(line_toChange, "y2", frame_info$max_y - (value_set[pos + 1] - frame_info$scale_min) * frame_info$scaling_y)
      }
    }
  }
  if (alignment=="horizontal")
  {
    for (n_lines in 1:length(lines_inGroup)) {
      
      line_toChange <- lines_inGroup[order_lines[n_lines]]
      pos <- n_lines
      if (any(is.na(value_set[pos:(pos+1)])))
      {
        xml2::xml_set_attr(line_toChange, "display", "none")
      } else  {
        xml2::xml_set_attr(line_toChange, "display", "NULL")
        xml2::xml_set_attr(line_toChange, "x1", frame_info$min_x + (value_set[pos] - frame_info$scale_min) * frame_info$scaling_x)
        xml2::xml_set_attr(line_toChange, "x2", frame_info$min_x + (value_set[pos + 1] - frame_info$scale_min) * frame_info$scaling_x)
      }
    }
  }
  
}

# pull information about paths in group (start_x, start_y, order, ...) and adjust paths for repositioning
linesSymbols_info_paths <- function (paths_inGroup) {
  
  dat_paths <- data.frame(Index = numeric(),start_x = numeric(),start_y = numeric(),offset_x = numeric(),offset_y = numeric(),min_x = numeric(),max_x = numeric(),min_y = numeric(),max_y = numeric())
  
  #zeichne alle Pfade virtuell nach, um ihre Ausdehnungen zu eruieren
  for (pp in 1:length(paths_inGroup))
  {
    dat_paths[pp,"Index"] <- pp
    minmax <- c(Inf,-Inf,Inf,-Inf)
    start_x <- start_y <- as.numeric(NA)
    subpath_x <- subpath_y <- as.numeric(NA)
    cur_x <- cur_y <- as.numeric(NA)
    path <- paths_inGroup[pp]
    path.str <- xml2::xml_attr(x = path,attr = "d")
    path.str <- gsub(pattern = "-",replacement = " -",x = path.str,ignore.case = TRUE) #bereinige vorab die komprimierten Strings
    path.str <- gsub(pattern = ",",replacement = " ",x = path.str,ignore.case = TRUE)
    while (regexpr(pattern = "\\.[0-9]*\\.[0-9]*",text = path.str)!=-1) #das ist tricky: Eine Angabe wie 2.5.6 ist tatsaechlich moeglich und bedeutet 2,5 und 0,6 :-(
    {
      pos1 <- regexpr(pattern = "\\.[0-9]*\\.[0-9]*",text = path.str)
      dot2 <- gregexpr(pattern = "\\.",text = substr(path.str,pos1,nchar(path.str)))[[1]][2]
      path.str <- paste0(substr(path.str,1,pos1+dot2-2)," ",substr(path.str,pos1+dot2-1,nchar(path.str)))
    }
    for (pc in getPathCommands()) path.str <- gsub(pattern = pc,replacement = paste0("\\$",pc),x = path.str)
    path.split <- strsplit(x = path.str,split = "\\$")[[1]] #teile nach Kommandos auf
    ecp_x <- ecp_y <- qcp_x <- qcp_y <- NA #zum Mitspeichern der alten End Control Points fuer S/s und Quadratic Control Points fuer T/t
    #damit im Nachhinein eine Verschiebung nur mit dem ersten moveto-Kommando moeglich ist, muessen hier alle absoluten Angaben in relative umgebaut werden
    for (cc in 1:length(path.split))
    {
      pc <- path.split[cc]
      handled.command <- FALSE
      pc <- gsub(pattern = "^ *",replacement = "",x = pc,ignore.case = TRUE)
      if (nzchar(pc)==0) next
      if (!substr(pc,1,1) %in% getPathCommands()) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
      suppressWarnings(coords <- as.numeric(strsplit(x = substr(pc,2,nchar(pc)),split = " ")[[1]]))
      coords <- stats::na.omit(coords)
      if (substr(pc,1,1) == "M") {
        if (length(coords) < 2 || length(coords)%%2 != 0) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        newpc <- ""
        for (ii in 1:(length(coords)/2))
        {
          if (is.na(start_x)) { #Am Beginn ist eine absolute Angabe erwuenscht.
            start_x <- coords[ii*2-1]
            start_y <- coords[ii*2]
            newpc <- paste0("M",round(start_x,10)," ",round(start_y,10))
          } else newpc <- paste0(newpc,"m",round(coords[ii*2-1]-cur_x,10)," ",round(coords[ii*2]-cur_y,10)) #weitere absolute MoveTos muessen relativiert werden
          cur_x <- subpath_x <- coords[ii*2-1]
          cur_y <- subpath_y <- coords[ii*2]
          minmax <- setNewMinMax(minmax,cur_x,cur_y)
        }
        path.split[cc] <- newpc
        ecp_x <- ecp_y <- qcp_x <- qcp_y <- NA
        handled.command <- TRUE
      }
      if (substr(pc,1,1) == "m") {
        if (length(coords) < 2 || length(coords)%%2 != 0) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        for (ii in 1:(length(coords)/2))
        {
          cur_x <- subpath_x <- (cur_x + coords[ii*2-1])
          cur_y <- subpath_y <- (cur_y + coords[ii*2])
          minmax <- setNewMinMax(minmax,cur_x,cur_y)
        }
        ecp_x <- ecp_y <- qcp_x <- qcp_y <- NA
        handled.command <- TRUE
      }
      if (substr(pc,1,1) == "L") {
        if (length(coords) < 2 || length(coords)%%2 != 0) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        newpc <- ""
        for (ii in 1:(length(coords)/2))
        {
          newpc <- paste0(newpc,"l",round(coords[ii*2-1]-cur_x,10)," ",round(coords[ii*2]-cur_y,10))
          cur_x <- coords[ii*2-1]
          cur_y <- coords[ii*2]
          minmax <- setNewMinMax(minmax,cur_x,cur_y)
        }
        path.split[cc] <- newpc
        ecp_x <- ecp_y <- qcp_x <- qcp_y <- NA
        handled.command <- TRUE
      }
      if (substr(pc,1,1) == "l") {
        if (length(coords) < 2 || length(coords)%%2 != 0) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        for (ii in 1:(length(coords)/2))
        {
          cur_x <- (cur_x + coords[ii*2-1])
          cur_y <- (cur_y + coords[ii*2])
          minmax <- setNewMinMax(minmax,cur_x,cur_y)
        }
        ecp_x <- ecp_y <- qcp_x <- qcp_y <- NA
        handled.command <- TRUE
      }
      if (substr(pc,1,1) == "H") {
        if (length(coords) < 1) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        newpc <- ""
        for (ii in 1:length(coords))
        {
          newpc <- paste0(newpc,"h",round(coords[ii]-cur_x,10))
          cur_x <- coords[ii]
          minmax <- setNewMinMax(minmax,cur_x,cur_y)
        }
        path.split[cc] <- newpc
        ecp_x <- ecp_y <- qcp_x <- qcp_y <- NA
        handled.command <- TRUE
      }
      if (substr(pc,1,1) == "h") {
        if (length(coords) < 1) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        for (ii in 1:length(coords))
        {
          cur_x <- (cur_x + coords[ii])
          minmax <- setNewMinMax(minmax,cur_x,cur_y)
        }
        ecp_x <- ecp_y <- qcp_x <- qcp_y <- NA
        handled.command <- TRUE
      }
      if (substr(pc,1,1) == "V") {
        if (length(coords) < 1) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        newpc <- ""
        for (ii in 1:length(coords))
        {
          newpc <- paste0(newpc,"v",round(coords[ii]-cur_y,10))
          cur_y <- coords[ii]
          minmax <- setNewMinMax(minmax,cur_x,cur_y)
        }
        path.split[cc] <- newpc
        ecp_x <- ecp_y <- qcp_x <- qcp_y <- NA
        handled.command <- TRUE
      }
      if (substr(pc,1,1) == "v") {
        if (length(coords) < 1) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        for (ii in 1:length(coords))
        {
          cur_y <- (cur_y + coords[ii])
          minmax <- setNewMinMax(minmax,cur_x,cur_y)
        }
        ecp_x <- ecp_y <- qcp_x <- qcp_y <- NA
        handled.command <- TRUE
      }
      if ((substr(pc,1,1) == "C" || substr(pc,1,1) == "c") && !requireNamespace("bezier")) stop(paste0("Error: package 'bezier' required for handling curves in path ",paths_inGroup[pp],"!"))
      if (substr(pc,1,1) == "C") {
        if (length(coords) < 6 || length(coords)%%6 != 0) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        newpc <- ""
        for (ii in 1:(length(coords)/6))
        {
          newpc <- paste0(newpc,"c",
                          round(coords[ii*6-5]-cur_x,10)," ",
                          round(coords[ii*6-4]-cur_y,10)," ",
                          round(coords[ii*6-3]-cur_x,10)," ",
                          round(coords[ii*6-2]-cur_y,10)," ",
                          round(coords[ii*6-1]-cur_x,10)," ",
                          round(coords[ii*6]-cur_y,10))
          points <- matrix(c(cur_x,cur_y,coords[ii*6-5:0]),nrow=4,ncol=2,byrow=TRUE)
          enclosing.rect <- getMinMaxCurve(points)
          minmax <- setNewMinMax(minmax,enclosing.rect$min_x,enclosing.rect$min_y)
          minmax <- setNewMinMax(minmax,enclosing.rect$max_x,enclosing.rect$max_y)
          ecp_x <- coords[ii*6-3]
          ecp_y <- coords[ii*6-2]
          qcp_x <- qcp_y <- NA
          cur_x <- coords[ii*6-1]
          cur_y <- coords[ii*6]
        }
        path.split[cc] <- newpc
        handled.command <- TRUE
      }
      if (substr(pc,1,1) == "c") {
        if (length(coords) < 6 || length(coords)%%6 != 0) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        for (ii in 1:(length(coords)/6))
        {
          points <- matrix(c(cur_x,cur_y,rep(c(cur_x,cur_y),3)+coords[ii*6-5:0]),nrow=4,ncol=2,byrow=TRUE)
          enclosing.rect <- getMinMaxCurve(points)
          minmax <- setNewMinMax(minmax,enclosing.rect$min_x,enclosing.rect$min_y)
          minmax <- setNewMinMax(minmax,enclosing.rect$max_x,enclosing.rect$max_y)
          ecp_x <- (cur_x + coords[ii*6-3])
          ecp_y <- (cur_y + coords[ii*6-2])
          qcp_x <- qcp_y <- NA
          cur_x <- (cur_x + coords[ii*6-1])
          cur_y <- (cur_y + coords[ii*6])
        }
        handled.command <- TRUE
      }
      if (substr(pc,1,1) == "S") {
        if (length(coords) < 4 || length(coords)%%4 != 0) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        newpc <- ""
        for (ii in 1:(length(coords)/4))
        {
          newpc <- paste0(newpc,"s",
                          round(coords[ii*4-3]-cur_x,10)," ",
                          round(coords[ii*4-2]-cur_y,10)," ",
                          round(coords[ii*4-1]-cur_x,10)," ",
                          round(coords[ii*4]-cur_y,10))
          if (is.na(ecp_x)) points <- matrix(c(cur_x,cur_y,cur_x,cur_y,coords[ii*4-3:0]),nrow=4,ncol=2,byrow=TRUE)
          if (!is.na(ecp_x)) points <- matrix(c(cur_x,cur_y,ecp_x,ecp_y,coords[ii*4-3:0]),nrow=4,ncol=2,byrow=TRUE)
          enclosing.rect <- getMinMaxCurve(points)
          minmax <- setNewMinMax(minmax,enclosing.rect$min_x,enclosing.rect$min_y)
          minmax <- setNewMinMax(minmax,enclosing.rect$max_x,enclosing.rect$max_y)
          ecp_x <- coords[ii*4-3]
          ecp_y <- coords[ii*4-2]
          qcp_x <- qcp_y <- NA
          cur_x <- coords[ii*4-1]
          cur_y <- coords[ii*4]
        }
        path.split[cc] <- newpc
        handled.command <- TRUE
      }
      if (substr(pc,1,1) == "s") {
        if (length(coords) < 4 || length(coords)%%4 != 0) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        for (ii in 1:(length(coords)/4))
        {
          if (is.na(ecp_x)) points <- matrix(c(cur_x,cur_y,cur_x,cur_y,rep(c(cur_x,cur_y),2)+coords[ii*4-3:0]),nrow=4,ncol=2,byrow=TRUE)
          if (!is.na(ecp_x)) points <- matrix(c(cur_x,cur_y,ecp_x,ecp_y,rep(c(cur_x,cur_y),2)+coords[ii*4-3:0]),nrow=4,ncol=2,byrow=TRUE)
          enclosing.rect <- getMinMaxCurve(points)
          minmax <- setNewMinMax(minmax,enclosing.rect$min_x,enclosing.rect$min_y)
          minmax <- setNewMinMax(minmax,enclosing.rect$max_x,enclosing.rect$max_y)
          ecp_x <- (cur_x + coords[ii*4-3])
          ecp_y <- (cur_y + coords[ii*4-2])
          qcp_x <- qcp_y <- NA
          cur_x <- (cur_x + coords[ii*4-1])
          cur_y <- (cur_y + coords[ii*4])
        }
        handled.command <- TRUE
      }
      if (substr(pc,1,1) == "Q") {
        if (length(coords) < 4 || length(coords)%%4 != 0) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        newpc <- ""
        for (ii in 1:(length(coords)/4))
        {
          newpc <- paste0(newpc,"q",
                          round(coords[ii*4-3]-cur_x,10)," ",
                          round(coords[ii*4-2]-cur_y,10)," ",
                          round(coords[ii*4-1]-cur_x,10)," ",
                          round(coords[ii*4]-cur_y,10))
          points <- matrix(c(cur_x,cur_y,coords[ii*4-3:0]),nrow=3,ncol=2,byrow=TRUE)
          enclosing.rect <- getMinMaxCurve(points)
          minmax <- setNewMinMax(minmax,enclosing.rect$min_x,enclosing.rect$min_y)
          minmax <- setNewMinMax(minmax,enclosing.rect$max_x,enclosing.rect$max_y)
          qcp_x <- coords[ii*4-3]
          qcp_y <- coords[ii*4-2]
          ecp_x <- ecp_y <- NA
          cur_x <- coords[ii*4-1]
          cur_y <- coords[ii*4]
        }
        path.split[cc] <- newpc
        handled.command <- TRUE
      }
      if (substr(pc,1,1) == "q") {
        if (length(coords) < 4 || length(coords)%%4 != 0) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        for (ii in 1:(length(coords)/4))
        {
          points <- matrix(c(cur_x,cur_y,rep(c(cur_x,cur_y),2)+coords[ii*4-3:0]),nrow=4,ncol=2,byrow=TRUE)
          enclosing.rect <- getMinMaxCurve(points)
          minmax <- setNewMinMax(minmax,enclosing.rect$min_x,enclosing.rect$min_y)
          minmax <- setNewMinMax(minmax,enclosing.rect$max_x,enclosing.rect$max_y)
          qcp_x <- (cur_x + coords[ii*4-3])
          qcp_y <- (cur_y + coords[ii*4-2])
          ecp_x <- ecp_y <- NA
          cur_x <- (cur_x + coords[ii*4-1])
          cur_y <- (cur_y + coords[ii*4])
        }
        handled.command <- TRUE
      }
      if (substr(pc,1,1) == "T") {
        if (length(coords) < 2 || length(coords)%%2 != 0) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        newpc <- ""
        for (ii in 1:(length(coords)/2))
        {
          newpc <- paste0(newpc,"t",
                          round(coords[ii*2-1]-cur_x,10)," ",
                          round(coords[ii*2]-cur_y,10))
          if (is.na(qcp_x)) points <- matrix(c(cur_x,cur_y,cur_x,cur_y,coords[ii*2-1:0]),nrow=4,ncol=2,byrow=TRUE)
          if (!is.na(qcp_x)) points <- matrix(c(cur_x,cur_y,qcp_x,qcp_y,coords[ii*2-1:0]),nrow=4,ncol=2,byrow=TRUE)
          enclosing.rect <- getMinMaxCurve(points)
          minmax <- setNewMinMax(minmax,enclosing.rect$min_x,enclosing.rect$min_y)
          minmax <- setNewMinMax(minmax,enclosing.rect$max_x,enclosing.rect$max_y)
          ecp_x <- ecp_y <- NA
          cur_x <- coords[ii*2-1]
          cur_y <- coords[ii*2]
        }
        path.split[cc] <- newpc
        handled.command <- TRUE
      }
      if (substr(pc,1,1) == "t") {
        if (length(coords) < 2 || length(coords)%%2 != 0) stop(paste0("Error: Invalid path command '",pc,"' in path ",paths_inGroup[pp],"!"))
        for (ii in 1:(length(coords)/2))
        {
          if (is.na(ecp_x)) points <- matrix(c(cur_x,cur_y,cur_x,cur_y,c(cur_x,cur_y)+coords[ii*2-1:0]),nrow=4,ncol=2,byrow=TRUE)
          if (!is.na(ecp_x)) points <- matrix(c(cur_x,cur_y,ecp_x,ecp_y,c(cur_x,cur_y)+coords[ii*2-1:0]),nrow=4,ncol=2,byrow=TRUE)
          enclosing.rect <- getMinMaxCurve(points)
          minmax <- setNewMinMax(minmax,enclosing.rect$min_x,enclosing.rect$min_y)
          minmax <- setNewMinMax(minmax,enclosing.rect$max_x,enclosing.rect$max_y)
          ecp_x <- ecp_y <- NA
          cur_x <- (cur_x + coords[ii*2-1])
          cur_y <- (cur_y + coords[ii*2])
        }
        handled.command <- TRUE
      }
      if (substr(pc,1,1) == "A") {
        #TODO WIP
        getMinMaxArc(NULL)
      }
      if (substr(pc,1,1) == "a") {
        #TODO WIP
        getMinMaxArc(NULL)
      }
      if (substr(pc,1,1) == "Z" || substr(pc,1,1) == "z")
      {
        cur_x <- subpath_x
        cur_y <- subpath_y
        handled.command <- TRUE
      }
      #fail-safe, wenn ein Pfad-Kommando noch nicht angelegt wurde
      if (!handled.command) stop(paste0("Error: Can't handle command '",substr(pc,1,1),"' in path ",paths_inGroup[pp],"!"))
    }
    xml2::xml_set_attr(x = path,attr = "d",value = paste(path.split,collapse = ""))
    dat_paths[pp,"start_x"] <- start_x
    dat_paths[pp,"start_y"] <- start_y
    dat_paths[pp,"offset_x"] <- (start_x - (minmax[1]+minmax[2])/2.0)
    dat_paths[pp,"offset_y"] <- (start_y - (minmax[3]+minmax[4])/2.0)
    dat_paths[pp,"min_x"] <- minmax[1]
    dat_paths[pp,"max_x"] <- minmax[2]
    dat_paths[pp,"min_y"] <- minmax[3]
    dat_paths[pp,"max_y"] <- minmax[4]
  }
  
  # order of paths
  dat_paths$order_x <- order(order(dat_paths$start_x))
  dat_paths$order_y <- order(order(dat_paths$start_y))
  
  # return
  return(dat_paths)
  
}

linesSymbols_edit_paths <- function (svg_in, group, frame_info, value_set, alignment, scatter = FALSE) {
  
  # available paths in group
  paths_inGroup <- xml2::xml_find_all(group, "./path")
  if (length(paths_inGroup)!=length(value_set))
  {
    if (length(paths_inGroup)>=1 && scatter)
    {
      while (length(paths_inGroup)!=length(value_set))
      {
        if (length(paths_inGroup)<length(value_set)) { element <- xml2::xml_find_first(group, "./path"); xml2::xml_add_sibling(element,element) }
        if (length(paths_inGroup)>length(value_set)) { element <- xml2::xml_find_first(group, "./path"); xml2::xml_remove(element) }
        paths_inGroup <- xml2::xml_find_all(group, "./path")
      }
    } else if (length(paths_inGroup)==2)
    {
      warning("Warning: Only two path-elements in group. Duplicating with assumption of fixed shape and constant distances.")
      dat_paths <- linesSymbols_info_paths(paths_inGroup)
      diff_x <- abs(dat_paths[1,"start_x"]-dat_paths[2,"start_x"])
      diff_y <- abs(dat_paths[1,"start_y"]-dat_paths[2,"start_y"])
      firstpath <- xml2::xml_find_first(group, "./path")[[1]]
      for (ee in 3:length(value_set))
      {
        newpath <- xml2::xml_add_sibling(.x = firstpath,.value=firstpath)
        if (alignment == "vertical")
        {
          dat_paths <- dat_paths[order(dat_paths$order_x),]
          setNewPathBegin(newpath,(dat_paths[1,"start_x"]+(ee-1)*diff_x),dat_paths[1,"start_y"])
        }
        if (alignment == "horizontal")
        {
          dat_paths <- dat_paths[order(dat_paths$order_y),]
          setNewPathBegin(newpath,dat_paths[1,"start_x"],(dat_paths[1,"start_y"]+(ee-1)*diff_y))
        }
      }
    } else if (length(paths_inGroup)==1 && length(xml2::xml_find_all(group, "./line"))>0)
    {
      warning("Warning: Only one path-element in group. Duplicating with assumption of fixed shape and constant distances (depending on the line-element).")
      line <- xml2::xml_find_first(group, "./line")
      height <- abs(as.numeric(xml2::xml_attr(line, "y1")) - as.numeric(xml2::xml_attr(line, "y2")))
      width <- abs(as.numeric(xml2::xml_attr(line, "x1")) - as.numeric(xml2::xml_attr(line, "x2")))
      dat_paths <- linesSymbols_info_paths(paths_inGroup)
      firstpath <- xml2::xml_find_first(group, "./path")[[1]]
      for (ee in 2:length(value_set))
      {
        newpath <- xml2::xml_add_sibling(.x = firstpath,.value=firstpath)
        if (alignment == "vertical") setNewPathBegin(newpath,(dat_paths[1,"start_x"]+(ee-1)*width),dat_paths[1,"start_y"])
        if (alignment == "horizontal") setNewPathBegin(newpath,dat_paths[1,"start_x"],(dat_paths[1,"start_y"]+(ee-1)*height))
      }
    } else stop(paste0("Error: Wrong number of path-elements in group (",length(paths_inGroup)," paths, ",length(value_set)," expected)."))
    paths_inGroup <- xml2::xml_find_all(group, "./path")
  }
  
  # information about paths
  dat_paths <- linesSymbols_info_paths(paths_inGroup)
  if (scatter)
  {
    dat_paths$order_x <- 1:nrow(dat_paths)
    dat_paths$order_y <- 1:nrow(dat_paths)
  }
  
  switch(alignment,
         
         vertical = {
           
           for (pp in 1:length(paths_inGroup)) {
             dat_path <- dat_paths[pp,,drop=FALSE]
             path_toChange <- paths_inGroup[pp]
             value <- value_set[dat_path$order_x]
             if (is.na(value))
             {
               xml2::xml_set_attr(path_toChange,"display","none")
             } else {
               location_y <- (frame_info$max_y - (value - frame_info$scale_min) * frame_info$scaling_y)
               xml2::xml_set_attr(path_toChange,"display",NULL)
               setNewPathBegin(path_toChange,dat_path$start_x,location_y+dat_path$offset_y)
               if (TRUE && pp==1) #Visueller Check des einschliessendes Rechtecks
               {
                 newrect <- xml2::xml_add_child(.x = group[[1]],.value = path_toChange[[1]])
                 xml2::xml_set_name(newrect,"rect")
                 attribs <- xml2::xml_attrs(newrect)
                 for (attrib in names(attribs)) xml2::xml_set_attr(newrect,attrib,NULL)
                 xml2::xml_set_attr(newrect,"fill","none")
                 xml2::xml_set_attr(newrect,"stroke","#000000")
                 xml2::xml_set_attr(newrect,"x",dat_path$min_x)
                 xml2::xml_set_attr(newrect,"y",location_y-(dat_path$max_y-dat_path$min_y)/2.0)
                 xml2::xml_set_attr(newrect,"width",dat_path$max_x-dat_path$min_x)
                 xml2::xml_set_attr(newrect,"height",dat_path$max_y-dat_path$min_y)
               }
             }
           }
           
         },
         
         horizontal = {
           
           for (pp in 1:length(paths_inGroup)) {
             dat_path <- dat_paths[pp,,drop=FALSE]
             path_toChange <- paths_inGroup[pp]
             value <- value_set[dat_path$order_y]
             if (is.na(value))
             {
               xml2::xml_set_attr(path_toChange,"display","none")
             } else {
               location_x <- (frame_info$min_x + (value - frame_info$scale_min) * frame_info$scaling_x)
               xml2::xml_set_attr(path_toChange,"display",NULL)
               setNewPathBegin(path_toChange,location_x+dat_path$offset_x,dat_path$start_y)
             }
           }
           
         })
  
}

# pull information about polygons in group (x, y, order, ...)
linesSymbols_info_polygons <- function (polygons_inGroup) {
  
  dat_polygons <- data.frame(Index = numeric(),num_points = numeric(),min_x = numeric(),max_x = numeric(),min_y = numeric(),max_y = numeric())
  
  for (pp in 1:length(polygons_inGroup))
  {
    dat_polygons[pp,"Index"] <- pp
    min_x <- min_y <- Inf
    max_x <- max_y <- -Inf
    points <- get_polygon_coords(polygons_inGroup[pp])
    dat_polygons[pp,]$num_points <- nrow(points)
    for (qq in 1:nrow(points))
    {
      if (!(paste0("p",qq,"_x") %in% colnames(dat_polygons)))
      {
        dat_polygons[,paste0("p",qq,"_x")] <- as.numeric(NA)
        dat_polygons[,paste0("p",qq,"_y")] <- as.numeric(NA)
      }
      x <- points[qq,1]
      y <- points[qq,2]
      dat_polygons[pp,paste0("p",qq,"_x")] <- x
      dat_polygons[pp,paste0("p",qq,"_y")] <- y
      if (min_x > x) min_x <- x
      if (min_y > y) min_y <- y
      if (max_x < x) max_x <- x
      if (max_y < y) max_y <- y
    }
    dat_polygons[pp,]$min_x <- min_x
    dat_polygons[pp,]$max_x <- max_x
    dat_polygons[pp,]$min_y <- min_y
    dat_polygons[pp,]$max_y <- max_y
  }
  
  # center, gravity, width and height of polygons
  dat_polygons$center_x <- (dat_polygons$min_x + dat_polygons$max_x)/2
  dat_polygons$center_y <- (dat_polygons$min_y + dat_polygons$max_y)/2
  dat_polygons$gravity_x <- rowMeans(dat_polygons[,grep("^p[0-9]+\\_x$",names(dat_polygons))],na.rm=TRUE)
  dat_polygons$gravity_y <- rowMeans(dat_polygons[,grep("^p[0-9]+\\_y$",names(dat_polygons))],na.rm=TRUE)
  dat_polygons$width <- (dat_polygons$max_x - dat_polygons$min_x)
  dat_polygons$height <- (dat_polygons$max_y - dat_polygons$min_y)
  
  # order of polygons
  dat_polygons$order_x <- order(order(dat_polygons$min_x))
  dat_polygons$order_y <- order(order(dat_polygons$min_y))
  
  # return
  return(dat_polygons)

}

linesSymbols_edit_polygons <- function (svg_in, group, frame_info, value_set, alignment, scatter = FALSE) {
  
  # available polygons in group
  polygons_inGroup <- xml2::xml_find_all(group, "./polygon")
  if (length(polygons_inGroup)!=length(value_set))
  {
    if (length(polygons_inGroup)>=1 && scatter)
    {
      while (length(polygons_inGroup)!=length(value_set))
      {
        if (length(polygons_inGroup)<length(value_set)) { element <- xml2::xml_find_first(group, "./polygon"); xml2::xml_add_sibling(element,element) }
        if (length(polygons_inGroup)>length(value_set)) { element <- xml2::xml_find_first(group, "./polygon"); xml2::xml_remove(element) }
        polygons_inGroup <- xml2::xml_find_all(group, "./polygon")
      }
    } else if (length(polygons_inGroup)==2)
    {
      warning("Warning: Only two polygon-elements in group. Duplicating with assumption of fixed shape and constant distances.")
      points1 <- get_polygon_coords(polygons_inGroup[1])
      points2 <- get_polygon_coords(polygons_inGroup[2])
      diff_x <- abs(points1[1,1]-points2[1,1])
      diff_y <- abs(points1[1,2]-points2[1,2])
      firstpolygon <- xml2::xml_find_first(group, "./polygon")[[1]]
      for (ee in 3:length(value_set))
      {
        newpolygon <- xml2::xml_add_sibling(.x = firstpolygon,.value=firstpolygon)
        if (alignment == "vertical")
        {
          if (points1[1,1]<=points2[1,1]) newpoints <- points1
          if (points1[1,1]> points2[1,1]) newpoints <- points2
          newpoints[,1] <- (newpoints[,1]+(ee-1)*diff_x)
        }
        if (alignment == "horizontal")
        {
          if (points1[1,2]<=points2[1,2]) newpoints <- points1
          if (points1[1,2]> points2[1,2]) newpoints <- points2
          newpoints[,2] <- (newpoints[,2]+(ee-1)*diff_y)
        }
        set_polygon_coords(newpolygon,newpoints)
      }
    } else if (length(polygons_inGroup)==1 && length(xml2::xml_find_all(group, "./line"))>0)
    {
      warning("Warning: Only one polygon-element in group. Duplicating with assumption of fixed shape and constant distances (depending on the line-element).")
      line <- xml2::xml_find_first(group, "./line")
      height <- abs(as.numeric(xml2::xml_attr(line, "y1")) - as.numeric(xml2::xml_attr(line, "y2")))
      width <- abs(as.numeric(xml2::xml_attr(line, "x1")) - as.numeric(xml2::xml_attr(line, "x2")))
      firstpolygon <- xml2::xml_find_first(group, "./polygon")[[1]]
      points <- get_polygon_coords(firstpolygon)
      for (ee in 2:length(value_set))
      {
        newpolygon <- xml2::xml_add_sibling(.x = firstpolygon,.value=firstpolygon)
        newpoints <- points
        if (alignment == "vertical") newpoints[,1] <- (newpoints[,1]+(ee-1)*width)
        if (alignment == "horizontal") newpoints[,2] <- (newpoints[,2]+(ee-1)*height)
        set_polygon_coords(newpolygon,newpoints)
      }
    } else stop(paste0("Error: Wrong number of polygon-elements in group (",length(polygons_inGroup)," polygons, ",length(value_set)," expected)."))
    polygons_inGroup <- xml2::xml_find_all(group, "./polygon")
  }
  
  # information about polygons
  dat_polygons <- linesSymbols_info_polygons(polygons_inGroup)
  if (scatter)
  {
    dat_polygons$order_x <- 1:nrow(dat_polygons)
    dat_polygons$order_y <- 1:nrow(dat_polygons)
  }
  
  switch(alignment,
         
         vertical = {
           
           for (pp in 1:length(polygons_inGroup)) {
             dat_polygon <- dat_polygons[pp,,drop=FALSE]
             polygon_toChange <- polygons_inGroup[pp]
             value <- value_set[dat_polygon$order_x]
             if (is.na(value))
             {
               xml2::xml_set_attr(polygon_toChange,"display","none")
             } else {
               location_y <- (frame_info$max_y - (value - frame_info$scale_min) * frame_info$scaling_y)
               diff_y <- (dat_polygon$gravity_y - location_y)
               points_new <- get_polygon_coords(polygon_toChange)
               points_new[,2] <- (points_new[,2] - diff_y)
               xml2::xml_set_attr(polygon_toChange,"display",NULL)
               set_polygon_coords(polygon_toChange, points_new)
             }
           }
           
         },
         
         horizontal = {
           
           for (pp in 1:length(polygons_inGroup)) {
             dat_polygon <- dat_polygons[pp,,drop=FALSE]
             polygon_toChange <- polygons_inGroup[pp]
             value <- value_set[dat_polygon$order_y]
             if (is.na(value))
             {
               xml2::xml_set_attr(polygon_toChange,"display","none")
             } else {
               location_x <- (frame_info$min_x + (value - frame_info$scale_min) * frame_info$scaling_x)
               diff_x <- (dat_polygon$gravity_x - location_x)
               points_new <- get_polygon_coords(polygon_toChange)
               points_new[,1] <- (points_new[,1] - diff_x)
               xml2::xml_set_attr(polygon_toChange,"display",NULL)
               set_polygon_coords(polygon_toChange, points_new)
             }
           }
           
         })
  
}

linesSymbols_info_linegroups <- function (linegroups_inGroup) {
  
  dat_linegroups <- data.frame(Index = numeric(),num_lines = numeric(),min_x = numeric(),max_x = numeric(),min_y = numeric(),max_y = numeric())
  
  for (pp in 1:length(linegroups_inGroup))
  {
    dat_linegroups[pp,"Index"] <- pp
    min_x <- min_y <- Inf
    max_x <- max_y <- -Inf
    lines <- xml2::xml_find_all(linegroups_inGroup[pp], "./line")
    if (length(lines)==0) stop("Error: Group without line-elements found.")
    dat_linegroups[pp,]$num_lines <- length(lines)
    for (qq in 1:length(lines))
    {
      if (!(paste0("l",qq,"_x1") %in% colnames(dat_linegroups)))
      {
        dat_linegroups[,paste0("l",qq,"_x1")] <- as.numeric(NA)
        dat_linegroups[,paste0("l",qq,"_x2")] <- as.numeric(NA)
        dat_linegroups[,paste0("l",qq,"_y1")] <- as.numeric(NA)
        dat_linegroups[,paste0("l",qq,"_y2")] <- as.numeric(NA)
      }
      x1 <- as.numeric(xml2::xml_attr(lines[qq],"x1"))
      x2 <- as.numeric(xml2::xml_attr(lines[qq],"x2"))
      y1 <- as.numeric(xml2::xml_attr(lines[qq],"y1"))
      y2 <- as.numeric(xml2::xml_attr(lines[qq],"y2"))
      dat_linegroups[pp,paste0("l",qq,"_x1")] <- x1
      dat_linegroups[pp,paste0("l",qq,"_x2")] <- x2
      dat_linegroups[pp,paste0("l",qq,"_y1")] <- y1
      dat_linegroups[pp,paste0("l",qq,"_y2")] <- y2
      if (min_x > x1) min_x <- x1
      if (min_x > x2) min_x <- x2
      if (min_y > y1) min_y <- y1
      if (min_y > y2) min_y <- y2
      if (max_x < x1) max_x <- x1
      if (max_x < x2) max_x <- x2
      if (max_y < y1) max_y <- y1
      if (max_y < y2) max_y <- y2
    }
    dat_linegroups[pp,]$min_x <- min_x
    dat_linegroups[pp,]$max_x <- max_x
    dat_linegroups[pp,]$min_y <- min_y
    dat_linegroups[pp,]$max_y <- max_y
  }
  
  # center, gravity, width and height of polygons
  dat_linegroups$center_x <- (dat_linegroups$min_x + dat_linegroups$max_x)/2
  dat_linegroups$center_y <- (dat_linegroups$min_y + dat_linegroups$max_y)/2
  dat_linegroups$gravity_x <- rowMeans(dat_linegroups[,grep("^l[0-9]+\\_x(1|2)$",names(dat_linegroups))],na.rm=TRUE)
  dat_linegroups$gravity_y <- rowMeans(dat_linegroups[,grep("^l[0-9]+\\_y(1|2)$",names(dat_linegroups))],na.rm=TRUE)
  dat_linegroups$width <- (dat_linegroups$max_x - dat_linegroups$min_x)
  dat_linegroups$height <- (dat_linegroups$max_y - dat_linegroups$min_y)
  
  # order of polygons
  dat_linegroups$order_x <- order(order(dat_linegroups$min_x))
  dat_linegroups$order_y <- order(order(dat_linegroups$min_y))
  
  # return
  return(dat_linegroups)
  
}

linesSymbols_edit_linegroups <- function (svg_in, group, frame_info, value_set, alignment, scatter = FALSE) {
  
  #available groups
  linegroups_inGroup <- xml2::xml_find_all(group, "./g")
  if (length(linegroups_inGroup)!=length(value_set))
  {
    if (length(linegroups_inGroup)>=1 && scatter)
    {
      while (length(linegroups_inGroup)!=length(value_set))
      {
        if (length(linegroups_inGroup)<length(value_set)) { element <- xml2::xml_find_first(group, "./g"); xml2::xml_add_sibling(element,element) }
        if (length(linegroups_inGroup)>length(value_set)) { element <- xml2::xml_find_first(group, "./g"); xml2::xml_remove(element) }
        linegroups_inGroup <- xml2::xml_find_all(group, "./g")
      }
    } else if (length(linegroups_inGroup)==2)
    {
      warning("Warning: Only two linegroups in group. Duplicating with assumption of fixed shape and constant distances.")
      line1 <- xml2::xml_find_first(linegroups_inGroup[1],"./line")[[1]]
      line2 <- xml2::xml_find_first(linegroups_inGroup[2],"./line")[[1]]
      height <- abs(as.numeric(xml2::xml_attr(line1, "y1")) - as.numeric(xml2::xml_attr(line2, "y1")))
      width <- abs(as.numeric(xml2::xml_attr(line1, "x1")) - as.numeric(xml2::xml_attr(line2, "x1")))
      if (alignment=="vertical")
      {
        if (as.numeric(xml2::xml_attr(line1, "x1"))<=as.numeric(xml2::xml_attr(line2, "x1"))) firstgroup <- linegroups_inGroup[1][[1]]
        if (as.numeric(xml2::xml_attr(line1, "x1"))> as.numeric(xml2::xml_attr(line2, "x1"))) firstgroup <- linegroups_inGroup[2][[1]]
      }
      if (alignment=="horizontal")
      {
        if (as.numeric(xml2::xml_attr(line1, "y1"))<=as.numeric(xml2::xml_attr(line2, "y1"))) firstgroup <- linegroups_inGroup[1][[1]]
        if (as.numeric(xml2::xml_attr(line1, "y1"))> as.numeric(xml2::xml_attr(line2, "y1"))) firstgroup <- linegroups_inGroup[2][[1]]
      }
      for (ee in 3:length(value_set))
      {
        newgroup <- xml2::xml_add_sibling(.x = firstgroup,.value=firstgroup)
        lines <- xml2::xml_find_all(newgroup, "./line")
        for (ll in 1:length(lines))
        {
          if (alignment=="vertical")
          {
            newx1 <- as.numeric(xml2::xml_attr(lines[ll],"x1")) + (ee-1)*width
            newx2 <- as.numeric(xml2::xml_attr(lines[ll],"x2")) + (ee-1)*width
            xml2::xml_set_attr(lines[ll],"x1",newx1)
            xml2::xml_set_attr(lines[ll],"x2",newx2)
          }
          if (alignment=="horizontal")
          {
            newy1 <- as.numeric(xml2::xml_attr(lines[ll],"y1")) + (ee-1)*height
            newy2 <- as.numeric(xml2::xml_attr(lines[ll],"y2")) + (ee-1)*height
            xml2::xml_set_attr(lines[ll],"y1",newy1)
            xml2::xml_set_attr(lines[ll],"y2",newy2)
          }
        }
      }
    } else if (length(linegroups_inGroup)==1 && length(xml2::xml_find_all(group, "./line"))>0)
    {
      warning("Warning: Only one linegroup in group. Duplicating with assumption of fixed shape and constant distances (depending on the line-element of the outer group).")
      line <- xml2::xml_find_first(group, "./line")
      height <- abs(as.numeric(xml2::xml_attr(line, "y1")) - as.numeric(xml2::xml_attr(line, "y2")))
      width <- abs(as.numeric(xml2::xml_attr(line, "x1")) - as.numeric(xml2::xml_attr(line, "x2")))
      firstgroup <- xml2::xml_find_first(group, "./g")[[1]]
      for (ee in 2:length(value_set))
      {
        newgroup <- xml2::xml_add_sibling(.x = firstgroup,.value=firstgroup)
        lines <- xml2::xml_find_all(newgroup, "./line")
        for (ll in 1:length(lines))
        {
          if (alignment=="vertical")
          {
            newx1 <- as.numeric(xml2::xml_attr(lines[ll],"x1")) + (ee-1)*width
            newx2 <- as.numeric(xml2::xml_attr(lines[ll],"x2")) + (ee-1)*width
            xml2::xml_set_attr(lines[ll],"x1",newx1)
            xml2::xml_set_attr(lines[ll],"x2",newx2)
          }
          if (alignment=="horizontal")
          {
            newy1 <- as.numeric(xml2::xml_attr(lines[ll],"y1")) + (ee-1)*height
            newy2 <- as.numeric(xml2::xml_attr(lines[ll],"y2")) + (ee-1)*height
            xml2::xml_set_attr(lines[ll],"y1",newy1)
            xml2::xml_set_attr(lines[ll],"y2",newy2)
          }
        }
      }
    } else stop(paste0("Error: Wrong number of linegroups in group (",length(linegroups_inGroup)," linegroups, ",length(value_set)," expected)."))
    linegroups_inGroup <- xml2::xml_find_all(group, "./g")
  }
  
  # symbols info
  dat_linegroups <- linesSymbols_info_linegroups(linegroups_inGroup)
  if (scatter)
  {
    dat_linegroups$order_x <- 1:nrow(dat_linegroups)
    dat_linegroups$order_y <- 1:nrow(dat_linegroups)
  }
  
  switch(alignment,
               
               vertical = {
                 
                 for (pp in 1:length(linegroups_inGroup)) {
                   dat_linegroup <- dat_linegroups[pp,,drop=FALSE]
                   value <- value_set[dat_linegroup$order_x]
                   if (is.na(value))
                   {
                     lines <- xml2::xml_find_all(linegroups_inGroup[pp], "./line")
                     for (qq in 1:dat_linegroup$num_lines) xml2::xml_set_attr(lines[qq], "display", "none")
                   } else {
                     location_y <- (frame_info$max_y - (value - frame_info$scale_min) * frame_info$scaling_y)
                     diff_y <- (dat_linegroup$gravity_y - location_y)
                     lines <- xml2::xml_find_all(linegroups_inGroup[pp], "./line")
                     for (qq in 1:dat_linegroup$num_lines)
                     {
                       new_y1 <- (dat_linegroup[,paste0("l",qq,"_y1")] - diff_y)
                       new_y2 <- (dat_linegroup[,paste0("l",qq,"_y2")] - diff_y)
                       xml2::xml_set_attr(lines[qq], "display", NULL)
                       xml2::xml_set_attr(lines[qq], "y1", new_y1)
                       xml2::xml_set_attr(lines[qq], "y2", new_y2)
                     }
                   }
                 }
                 
               },
               
               horizontal = {
                 
                 for (pp in 1:length(linegroups_inGroup)) {
                   dat_linegroup <- dat_linegroups[pp,,drop=FALSE]
                   value <- value_set[dat_linegroup$order_y]
                   if (is.na(value))
                   {
                     lines <- xml2::xml_find_all(linegroups_inGroup[pp], "./line")
                     for (qq in 1:dat_linegroup$num_lines) xml2::xml_set_attr(lines[qq], "display", "none")
                   } else {
                     location_x <- (frame_info$min_x + (value - frame_info$scale_min) * frame_info$scaling_x)
                     diff_x <- (dat_linegroup$gravity_x - location_x)
                     lines <- xml2::xml_find_all(linegroups_inGroup[pp], "./line")
                     for (qq in 1:dat_linegroup$num_lines)
                     {
                       new_x1 <- (dat_linegroup[,paste0("l",qq,"_x1")] - diff_x)
                       new_x2 <- (dat_linegroup[,paste0("l",qq,"_x2")] - diff_x)
                       xml2::xml_set_attr(lines[qq], "display", NULL)
                       xml2::xml_set_attr(lines[qq], "x1", new_x1)
                       xml2::xml_set_attr(lines[qq], "x2", new_x2)
                     }
                   }
                 }
                 
               })
  
}

linesSymbols_edit_circles <- function (svg, group, frame_info, value_set, alignment, scatter = FALSE) {
  
  # available circles in group
  symbols_inGroup <- xml2::xml_find_all(group, "./circle")
  if (length(symbols_inGroup)!=length(value_set))
  {
    if (length(symbols_inGroup)>=1 && scatter)
    {
      while (length(symbols_inGroup)!=length(value_set))
      {
        if (length(symbols_inGroup)<length(value_set)) { element <- xml2::xml_find_first(group, "./circle"); xml2::xml_add_sibling(element,element) }
        if (length(symbols_inGroup)>length(value_set)) { element <- xml2::xml_find_first(group, "./circle"); xml2::xml_remove(element) }
        symbols_inGroup <- xml2::xml_find_all(group, "./circle")
      }
    } else if (length(symbols_inGroup)==2)
    {
      warning("Warning: Only two circle elements in group. Duplicating with assumption of fixed shape and constant distances.")
      x <- as.numeric(xml2::xml_attr(symbols_inGroup, "cx"))
      y <- as.numeric(xml2::xml_attr(symbols_inGroup, "cy"))
      firstcircle <- xml2::xml_find_first(group, "./circle")[[1]]
      for (ee in 3:length(value_set))
      {
        newcircle <- xml2::xml_add_sibling(.x = firstcircle,.value=firstcircle)
        if (alignment == "vertical") xml2::xml_set_attr(newcircle,"cx",min(x)+(ee-1)*abs(x[1]-x[2]))
        if (alignment == "horizontal") xml2::xml_set_attr(newcircle,"cy",min(y)+(ee-1)*abs(y[1]-y[2]))
      }
    } else if (length(symbols_inGroup)==1 && length(xml2::xml_find_all(group, "./line"))>0)
    {
      warning("Warning: Only one circle element in group. Duplicating with assumption of fixed shape and constant distances (depending on the line element).")
      x <- as.numeric(xml2::xml_attr(symbols_inGroup, "cx"))
      y <- as.numeric(xml2::xml_attr(symbols_inGroup, "cy"))
      line <- xml2::xml_find_first(group, "./line")
      height <- abs(as.numeric(xml2::xml_attr(line, "y1")) - as.numeric(xml2::xml_attr(line, "y2")))
      width <- abs(as.numeric(xml2::xml_attr(line, "x1")) - as.numeric(xml2::xml_attr(line, "x2")))
      firstcircle <- xml2::xml_find_first(group, "./circle")[[1]]
      for (ee in 2:length(value_set))
      {
        newcircle <- xml2::xml_add_sibling(.x = firstcircle,.value=firstcircle)
        if (alignment == "vertical") xml2::xml_set_attr(newcircle,"cx",x+(ee-1)*width)
        if (alignment == "horizontal") xml2::xml_set_attr(newcircle,"cy",y+(ee-1)*height)
      }
    } else stop(paste0("Error: Wrong number of circle elements in group (",length(symbols_inGroup)," circles, ",length(value_set)," expected)."))
    symbols_inGroup <- xml2::xml_find_all(group, "./circle")
  }
  
  # order of circles
  if (!scatter)
  {
    symbols_order_x <- order(as.numeric(xml2::xml_attr(symbols_inGroup, "cx")))
    symbols_order_y <- order(as.numeric(xml2::xml_attr(symbols_inGroup, "cy")))
  }
  if (scatter)
  {
    symbols_order_x <- 1:length(symbols_inGroup)
    symbols_order_y <- 1:length(symbols_inGroup)
  }
  
  switch (alignment,
                
                vertical = {
                  
                  for (n_symbols in 1:length(symbols_inGroup)) {
                    symbol_toEdit <- symbols_inGroup[symbols_order_x[n_symbols]]
                    if (is.na(value_set[n_symbols]))
                    {
                      xml2::xml_set_attr(symbol_toEdit, "display", "none")
                    } else {
                      cy_new <- frame_info$max_y - (value_set[n_symbols] - frame_info$scale_min) * frame_info$scaling_y 
                      xml2::xml_set_attr(symbol_toEdit, "display", NULL)
                      xml2::xml_set_attr(symbol_toEdit, "cy", cy_new)
                    }
                  }
                  
                },
                
                horizontal = {
                  
                  for (n_symbols in 1:length(symbols_inGroup)) {
                    symbol_toEdit <- symbols_inGroup[symbols_order_y[n_symbols]]
                    if (is.na(value_set[n_symbols]))
                    {
                      xml2::xml_set_attr(symbol_toEdit, "display", "none")
                    } else {
                      cx_new <- frame_info$min_x + (value_set[n_symbols] - frame_info$scale_min) * frame_info$scaling_x 
                      xml2::xml_set_attr(symbol_toEdit, "display", NULL)
                      xml2::xml_set_attr(symbol_toEdit, "cx", cx_new)
                    }
                  }
                  
                })
  
  return(svg)
}

linesSymbols_edit_rects <- function (svg, group, frame_info, value_set, alignment, scatter = FALSE) {
  
  # available rects in group
  symbols_inGroup <- xml2::xml_find_all(group, "./rect")
  if (length(symbols_inGroup)!=length(value_set))
  {
    if (length(symbols_inGroup)>=1 && scatter)
    {
      while (length(symbols_inGroup)!=length(value_set))
      {
        if (length(symbols_inGroup)<length(value_set)) { element <- xml2::xml_find_first(group, "./rect"); xml2::xml_add_sibling(element,element) }
        if (length(symbols_inGroup)>length(value_set)) { element <- xml2::xml_find_first(group, "./rect"); xml2::xml_remove(element) }
        symbols_inGroup <- xml2::xml_find_all(group, "./rect")
      }
    } else if (length(symbols_inGroup)==2)
    {
      warning("Warning: Only two rectangle elements in group. Duplicating with assumption of fixed shape and constant distances.")
      x <- as.numeric(xml2::xml_attr(symbols_inGroup, "x"))
      y <- as.numeric(xml2::xml_attr(symbols_inGroup, "y"))
      firstrect <- xml2::xml_find_first(group, "./rect")[[1]]
      for (ee in 3:length(value_set))
      {
        newrect <- xml2::xml_add_sibling(.x = firstrect,.value=firstrect)
        if (alignment == "vertical") xml2::xml_set_attr(newrect,"x",min(x)+(ee-1)*abs(x[1]-x[2]))
        if (alignment == "horizontal") xml2::xml_set_attr(newrect,"y",min(y)+(ee-1)*abs(y[1]-y[2]))
      }
    } else if (length(symbols_inGroup)==1 && length(xml2::xml_find_all(group, "./line"))>0)
    {
      warning("Warning: Only two rectangle elements in group. Duplicating with assumption of fixed shape and constant distances (depending on the line element).")
      x <- as.numeric(xml2::xml_attr(symbols_inGroup, "x"))
      y <- as.numeric(xml2::xml_attr(symbols_inGroup, "y"))
      line <- xml2::xml_find_first(group, "./line")
      height <- abs(as.numeric(xml2::xml_attr(line, "y1")) - as.numeric(xml2::xml_attr(line, "y2")))
      width <- abs(as.numeric(xml2::xml_attr(line, "x1")) - as.numeric(xml2::xml_attr(line, "x2")))
      firstrect <- xml2::xml_find_first(group, "./rect")[[1]]
      for (ee in 2:length(value_set))
      {
        newrect <- xml2::xml_add_sibling(.x = firstrect,.value=firstrect)
        if (alignment == "vertical") xml2::xml_set_attr(newrect,"x",x+(ee-1)*width)
        if (alignment == "horizontal") xml2::xml_set_attr(newrect,"y",y+(ee-1)*height)
      }
    } else stop(paste0("Error: Wrong number of rectangle elements in group (",length(symbols_inGroup)," rectangles, ",length(value_set)," expected)."))
    symbols_inGroup <- xml2::xml_find_all(group, "./rect")
  }
  
  # order of rects
  if (!scatter)
  {
    symbols_order_x <- order(as.numeric(xml2::xml_attr(symbols_inGroup, "x")))
    symbols_order_y <- order(as.numeric(xml2::xml_attr(symbols_inGroup, "y")))
  }
  if (scatter)
  {
    symbols_order_x <- 1:length(symbols_inGroup)
    symbols_order_y <- 1:length(symbols_inGroup)
  }

  switch (alignment,
                
                vertical = {
                  
                  for (n_symbols in 1:length(symbols_inGroup)) {
                    symbol_toEdit <- symbols_inGroup[symbols_order_x[n_symbols]]
                    if (is.na(value_set[n_symbols]))
                    {
                      xml2::xml_set_attr(symbol_toEdit, "display", "none")
                    } else {
                      height_half <- as.numeric(xml2::xml_attr(symbol_toEdit, "height")) / 2
                      y_new <- frame_info$max_y - ((value_set[n_symbols] - frame_info$scale_min) * frame_info$scaling_y + height_half)
                      xml2::xml_set_attr(symbol_toEdit, "display", NULL)
                      xml2::xml_set_attr(symbol_toEdit, "y", y_new)
                      if (xml2::xml_has_attr(symbol_toEdit, "transform") && grepl("^matrix",xml2::xml_attr(symbol_toEdit, "transform")))
                        xml2::xml_set_attr(symbol_toEdit, "transform", recalc_transformMatrix(symbol_toEdit))
                    }
                  }
                  
                },
                
                horizontal = {
                  
                  for (n_symbols in 1:length(symbols_inGroup)) {
                    symbol_toEdit <- symbols_inGroup[symbols_order_y[n_symbols]]
                    if (is.na(value_set[n_symbols]))
                    {
                      xml2::xml_set_attr(symbol_toEdit, "display", "none")
                    } else {
                      width_half <- as.numeric(xml2::xml_attr(symbol_toEdit, "width")) / 2
                      x_new <- frame_info$min_x + (value_set[n_symbols] - frame_info$scale_min) * frame_info$scaling_x - width_half
                      xml2::xml_set_attr(symbol_toEdit, "display", NULL)
                      xml2::xml_set_attr(symbol_toEdit, "x", x_new)
                      if (xml2::xml_has_attr(symbol_toEdit, "transform") && grepl("^matrix",xml2::xml_attr(symbol_toEdit, "transform")))
                        xml2::xml_set_attr(symbol_toEdit, "transform", recalc_transformMatrix(symbol_toEdit))
                    }
                  }
                  
                })
  
  return(svg)
}


#' Adjust line and/or symbol charts
#' @description Adjusts the horizontal (XML attributes 'x', 'x1', 'x2', 'cx' etc.) or vertical (XML attributes 'y', 'y1', 'y2', 'cy' etc.) position of lines (XML elements of type 'line') and/or symbols (see details). Positions are calculated relative to a given frame (XML element of type 'rect') and the position of a data value within the minimum and maximum of a given scale. This process is called scaling.\cr
#' In preparation, it is necessary to name a group (set attribute 'id' of XML element of type 'g') of lines and/or symbols.
#' @param svg XML document with SVG content
#' @param frame_name Name (attribute 'id') of frame (XML element 'rect') for positioning of elements.
#' @param group_name Name (attribute 'id') of group (XML element 'g') with lines and/or symbols.
#' @param scale_real Numeric vector (e.g. \code{c(0,100)}) of arbitrary length. Only minimum and maximum are used for scaling of values.
#' @param values Numeric vector. If a dataframe or matrix is provided, only the first row will be used.
#' @param alignment Character value. Accepts 'horizontal' or 'vertical' (default). See details.
#' @param has_lines Are there lines? (default TRUE)
#' @param symbol_type Character value. Accepts 'circle', 'rect', 'polygon', 'linegroup', 'path', or 'guess' for guessing of type; see details. (default NULL = no symbols)
#' @param ... Further arguments used internally by \code{\link{scatterSymbols}}.
#' @return XML document with SVG content
#' @details Note: 'Horizontal' alignment refers to adjustment of the x-coordinates of elements, 'vertical' alignment to adjustment of the y-coordinates. This is not to be confused with the orientation of the resulting polyline (and/or the sequence of symbols) that goes from left to right (with \code{alignment='vertical'}) or from top to bottom (with \code{alignment='horizontal'}).\cr
#' Line elements and/or symbols may be grouped together in any order in the SVG file. The function will automatically use XML elements from left to right (with \code{alignment='vertical'}) or top to bottom (with \code{alignment='horizontal'}) according to their x/y-coordinates.\cr
#' Line elements and/or symbols must be prepared in the SVG file in one of the following amounts: a) same amount as data values (or one element less in case of lines), b) one line and/or or two symbols or c) one line \emph{and} one symbol. In case of b) and c) the function will automatically duplicate line elements and/or symbols with the assumption of fixed shapes (that is, all lines and/or all symbols will look the same as the 'template' that is provided) and constant distance between the elements on the coordinate that will not be adjusted by \code{values} ('x' with \code{alignment='vertical'} or 'y' with \code{alignment='horizontal'}).\cr
#' The function currently supports the following \code{symbol_type}s:
#' \itemize{
#' \item circle: XML elements of type 'circle'. Attributes 'cx' or 'cy' are adjusted.
#' \item rect: XML elements of type 'rect'. Attributes 'x' or 'y' are adjusted.
#' \item polygon: XML elements of type 'polygon'. Attribute 'points' is adjusted so that the centroid of the shape matches the scaled value position on the chart.
#' \item linegroup: XML elements of type 'g' that contain elements of type 'line'. Attributes 'x1' and 'x2' or 'y1' and 'y2' of those lines are adjusted so that the mean x- or y-coordinate of all lines in the group matches the scaled value position on the chart.
#' \item path: XML elements of type 'path'. The first command of attribute 'd' is adjusted. The center of path is defined as the midpoint between minimum and maximum x- and y-coordinates of the shape.
#' }
#' @examples
#' #read SVG file
#' fpath <- system.file("extdata", "fig11.svg", package="svgtools")
#' svg <- read_svg(file = fpath)
#' 
#' #adjust lines and/or symbols
#' set.seed(12345)
#' values <- matrix(c(rnorm(10,0.95,0.03), rnorm(10,0.75,0.05), 
#'                    rnorm(10,0.55,0.07), rnorm(10,0.35,0.05), 
#'                    rnorm(10,0.15,0.03)), nrow = 5, byrow = TRUE)
#' values[2,8] <- as.numeric(NA)
#' svg <- linesSymbols(svg = svg, frame_name = "frame", group_name = "gA", 
#'                     scale_real = c(0,1), values = values[1,], 
#'                     symbol_type = "rect")
#' svg <- linesSymbols(svg = svg, frame_name = "frame", group_name = "gB", 
#'                     scale_real = c(0,1), values = values[2,], 
#'                     symbol_type = "circle")
#' svg <- linesSymbols(svg = svg, frame_name = "frame", group_name = "gC", 
#'                     scale_real = c(0,1), values = values[3,], 
#'                     has_lines = FALSE, symbol_type = "polygon")
#' svg <- linesSymbols(svg = svg, frame_name = "frame", group_name = "gD", 
#'                     scale_real = c(0,1), values = values[4,], 
#'                     symbol_type = "linegroup")
#' svg <- linesSymbols(svg = svg, frame_name = "frame", group_name = "gE", 
#'                     scale_real = c(0,1), values = values[5,], 
#'                     symbol_type = NULL)
#' @export
linesSymbols <- function (svg, frame_name, group_name, scale_real, values, alignment = "vertical", has_lines = TRUE, symbol_type = NULL, ...) {
  
  # input check
  if (!is.null(symbol_type)) {
    if (!symbol_type %in% c("circle","rect","polygon","linegroup","path","guess")) { stop("Error: Invalid symbol_type. Must be one of 'circle', 'rect', 'polygon', 'linegroup', 'path', or 'guess'.") }
  }
  if (!alignment %in% c("horizontal","vertical")) { stop("Error: Alignment has to be either 'horizontal' or 'vertical'.") }
  # if input-values == data.frame: transform first row to vector
  if (is.data.frame(values))
  {
    if (nrow(values)>1) warning("Warning: Value is a dataframe with several rows. Using only first row.")
    values <- as.numeric(values[1,])
  }
  if (!is.numeric(values)) { stop("Error: Non-numerical values.") }
  
  # get frame info, scaling and group
  frame_info <- frame_and_scaling(svg, frame_name, scale_real)
  group <- linesSymbols_in(svg, group_name)
  
  # guess symbol type
  if (!is.null(symbol_type) && symbol_type == "guess")
  {
    symbol_type <- linesSymbols_guess(group)
    if (!is.null(symbol_type) && is.na(symbol_type)) stop("Error: Could not guess symbol_type from group content.")
  }
  
  # 1 - Lines
  if (has_lines)
  {
    lines_inGroup <- xml2::xml_find_all(group, "./line")
    if (length(lines_inGroup) == 0) { stop("Error: No elements of type line in group.") }
    if (length(lines_inGroup) == 1 && length(values)!=2)
    {
      warning("Warning: Only one line element in group. Duplicating with assumption of fixed shape and constant distances.")
      lines_inGroup <- linesSymbols_duplicate_lines(group, alignment, length(values)-1)
    }
    if (length(lines_inGroup) != length(values)-1) { stop(paste0("Error: wrong number of lines in group (",length(lines_inGroup)," lines, ",(length(values)-1)," expected).")) }
    order_lines <- linesSymbols_order_lines(lines_inGroup, alignment)
    linesSymbols_edit_lines(lines_inGroup, order_lines, frame_info, values, alignment)
  }
  
  # 2 - Symbols
  if (!is.null(symbol_type))
  {
    args <- list(...)
    if (is.null(args[['scatter']])) scatter <- FALSE
    if (!is.null(args[['scatter']])) scatter <- args[['scatter']]
    if (symbol_type == "circle")    linesSymbols_edit_circles   (svg, group, frame_info, values, alignment, scatter = scatter)
    if (symbol_type == "rect")      linesSymbols_edit_rects     (svg, group, frame_info, values, alignment, scatter = scatter)
    if (symbol_type == "polygon")   linesSymbols_edit_polygons  (svg, group, frame_info, values, alignment, scatter = scatter)
    if (symbol_type == "linegroup") linesSymbols_edit_linegroups(svg, group, frame_info, values, alignment, scatter = scatter)
    if (symbol_type == "path")      linesSymbols_edit_paths     (svg, group, frame_info, values, alignment, scatter = scatter)
  }
  
  return(svg)
}

### STREUDIAGRAMM ----

#' Adjust symbols of a scatter plot
#' @description Adjusts the horizontal (XML attributes 'x', 'x1', 'x2', 'cx' etc.) and vertical (XML attributes 'y', 'y1', 'y2', 'cy' etc.) positions of symbols (see details). Positions are calculated relative to a given frame (XML element of type 'rect') and the position of a data value within the minimum and maximum of two given scales for x- and y-axis. This process is called scaling.\cr
#' In preparation, it is necessary to name a group (set attribute 'id' of XML element of type 'g') of symbols. Symbols are automatically duplicated or removed to match the amount of data values.
#' @param svg XML document with SVG content
#' @param frame_name Name (attribute 'id') of frame (XML element 'rect') for positioning of elements.
#' @param group_name Name (attribute 'id') of group (XML element 'g') with symbols.
#' @param scale_real_x Numeric vector (e.g. \code{c(0,100)}) of arbitrary length for x-axis. Only minimum and maximum are used for scaling of values.
#' @param scale_real_y Numeric vector (e.g. \code{c(0,100)}) of arbitrary length for y-axis. Only minimum and maximum are used for scaling of values.
#' @param values Dataframe or matrix with numeric vectors. First column corresponds to x-axis. Second column corresponds to y-axis.
#' @param symbol_type Character value. Accepts 'circle', 'rect', 'polygon', 'linegroup', 'path', or 'guess' for guessing of type; see details.
#' @return XML document with SVG content
#' @details Symbols may be prepared in the SVG file in any amount. But be aware that the function will simply duplicate the first one (in the group) or remove the last ones to match the amount of data values. When, for example, you need to have different colors for different subgroups of cases, prepare several groups of symbols and call this function for each of them.\cr
#' The function currently supports the following \code{symbol_type}s:
#' \itemize{
#' \item circle: XML elements of type 'circle'. Attributes 'cx' and 'cy' are adjusted.
#' \item rect: XML elements of type 'rect'. Attributes 'x' and 'y' are adjusted.
#' \item polygon: XML elements of type 'polygon'. Attribute 'points' is adjusted so that the centroid of the shape matches the scaled value positions on the chart.
#' \item linegroup: XML elements of type 'g' that contain elements of type 'line'. Attributes 'x1', 'x2', 'y1' and 'y2' of those lines are adjusted so that the mean x- and y-coordinate of all lines in the group matches the scaled value positions on the chart.
#' \item path: XML elements of type 'path'. The first command of attribute 'd' is adjusted. The center of path is defined as the midpoint between minimum and maximum x- and y-coordinates of the shape.
#' }
#' @examples
#' #read SVG file
#' fpath <- system.file("extdata", "fig13.svg", package="svgtools")
#' svg <- read_svg(file = fpath)
#' 
#' #scatter symbols
#' set.seed(12345)
#' df <- data.frame(g=rep(1:4,10), x=rnorm(40,500,75), y=rnorm(40,500,75))
#' df[df$g==1,]$x <- df[df$g==1,]$x-35
#' df[df$g==2,]$y <- df[df$g==2,]$y-35
#' df[df$g==3,]$x <- df[df$g==3,]$x+35
#' df[df$g==4,]$y <- df[df$g==4,]$y+35
#' svg <- scatterSymbols(svg = svg, frame_name = "frame", group_name = "gA", 
#'                       scale_real_x = c(250,750), scale_real_y = c(250,750), 
#'                       values = df[df$g==1,2:3], symbol_type = "rect")
#' svg <- scatterSymbols(svg = svg, frame_name = "frame", group_name = "gB", 
#'                       scale_real_x = c(250,750), scale_real_y = c(250,750), 
#'                       values = df[df$g==2,2:3], symbol_type = "circle")
#' svg <- scatterSymbols(svg = svg, frame_name = "frame", group_name = "gC", 
#'                       scale_real_x = c(250,750), scale_real_y = c(250,750), 
#'                       values = df[df$g==3,2:3], symbol_type = "polygon")
#' svg <- scatterSymbols(svg = svg, frame_name = "frame", group_name = "gD", 
#'                       scale_real_x = c(250,750), scale_real_y = c(250,750), 
#'                       values = df[df$g==4,2:3], symbol_type = "linegroup")
#' @export
scatterSymbols <- function(svg, frame_name, group_name, scale_real_x, scale_real_y, values, symbol_type) {
  # input check
  if (length(dim(values))!=2) stop("Error: Wrong object for argument values. Expecting 2-dimensional object (dataframe, matrix) with two columns.")
  if (!is.numeric(values[,1]) || !is.numeric(values[,2])) { stop("Error: Non-numerical values.") }
  # Anpassung mittels linesSymbols
  svg <- linesSymbols(svg = svg,frame_name = frame_name,group_name = group_name,scale_real = scale_real_x,values = values[,1],alignment = "horizontal",has_lines = FALSE,symbol_type = symbol_type,scatter = TRUE)
  svg <- linesSymbols(svg = svg,frame_name = frame_name,group_name = group_name,scale_real = scale_real_y,values = values[,2],alignment = "vertical",has_lines = FALSE,symbol_type = symbol_type,scatter = TRUE)
}

### TEXT ----

svg_setElementText <- function(svg, element_name, text_new, alignment = NULL, inGroup = NULL, hide_blank = FALSE) {
  
  if (!is.null(inGroup)) {
    availableGroups <- xml2::xml_find_all(svg, "g")
    searchGroup <- availableGroups[which(xml2::xml_attr(availableGroups, "id") == inGroup)]
    elements <- xml2::xml_find_all(searchGroup, "./text")
  } else {
    elements <- xml2::xml_find_all(svg, "./text")
  }
  
  for (element_nr in 1:length(element_name)) {
    
    checkElementWithId <- (length(which(xml2::xml_attr(elements, "id") == element_name[element_nr])) > 0)
    checkElementWithText <- (length(which(xml2::xml_text(elements) == element_name[element_nr])) > 0)
    
    if (!checkElementWithId && !checkElementWithText) {
      stop(paste0("Error: No text element with id or text '", element_name[element_nr], "' was found."))
    }
    
    if (checkElementWithId) {
      text_element <- elements[which(xml2::xml_attr(elements, "id") == element_name[element_nr])]
    } else {
      text_element <- elements[which(xml2::xml_text(elements) == element_name[element_nr])]
    }
    
    # edit text
    xml2::xml_set_text(text_element, text_new[element_nr])
    
    # edit alignment
    if (!is.null(alignment)) {
      xml2::xml_set_attr(text_element, "text-anchor", alignment)
    }
    
    # hide if blank
    if (nchar(text_new) == 0 && hide_blank) xml2::xml_set_attr(text_element, "display", "none")
    if (nchar(text_new) > 0 || !hide_blank) xml2::xml_set_attr(text_element, "display", NULL)
  }
  
  return(svg)
}

#' Change text of text elements
#' @description Changes the text entry of XML element of type 'text'. The XML element may be found by its name (XML attribute 'id') or based on its current text entry.
#' @param svg XML document with SVG content
#' @param element_name Name (attribute 'id') of text (XML element 'text') or current text entry of text (XML element 'text').
#' @param text New text entry.
#' @param alignment Character value for text alignment. Accepts 'start', 'middle', and 'end' (default NULL = no change).
#' @param in_group Name (attribute 'id') of group (XML element 'g') that contains the text element (default NULL = no group, search the entire SVG).
#' @param hide_blank Should text elements with empty strings be hidden (set attribute 'display' to 'none')? (default FALSE)
#' @return XML document with SVG content
#' @examples
#' #read SVG file
#' fpath <- system.file("extdata", "fig1.svg", package="svgtools")
#' svg <- read_svg(file = fpath)
#' 
#' #change a text
#' svg <- changeText(svg = svg, element_name = "Category A", text = "low")
#' svg <- changeText(svg = svg, element_name = "Category B", text = "medium")
#' svg <- changeText(svg = svg, element_name = "Category C", text = "high")
#' @export
changeText <- function(svg, element_name, text, alignment = NULL, in_group = NULL, hide_blank = FALSE) {
  return(svg_setElementText(svg = svg,element_name = element_name,text_new = text,alignment = alignment,inGroup = in_group, hide_blank = hide_blank))
}

### FORMATIERUNGSFUNKTIONEN ----

# TODO
#' #' Do some formatting on SVG
#' #' @description With + one can apply formatting functions on SVG charts.
#' #' @param svg XML document with SVG content
#' #' @param format_function A formatting function (see details)
#' #' @return XML document with SVG content
#' #' @details BLABLABLA
#' #' @examples
#' #' @export
#' `+.svg_obj` <- function(svg,format_function) {
#'   if (!"svg_format_function" %in% names(format_function)) stop("Error: Right-hand argument of + operator is no function of type 'svg_format_function'!")
#'   args <- format_function
#'   args$svg_format_function <- NULL
#'   args$svg <- svg
#'   do.call(what = format_function$svg_format_function,args = args)
#' }
#' 
#' centerTextHorizontal_ <- function(svg,group_name) {
#'   print(paste0("Zentriere fuer ",group_name))
#'   #TODO: eigentlicher Programmcode
#'   return(svg)
#' }
#' 
#' #' @export
#' centerTextHorizontal <- function(group_name) {
#'   return(list(svg_format_function="centerTextHorizontal_",group_name=group_name))
#' }
