# R Setup -----------------------------------------------------------------
library(here) # much better way to manage working paths in R across multiple instances
library(ragg) # better video device, more accurate and faster rendering, esp. on macos. Also should enable system fonts for display
library(tidyverse)
library(tidytext) # used for reorder_within() for factor and likert plots
require(devtools)
require(usethis)
require(likert)       # Used for likert data visualisation
require(RColorBrewer)
require(scales)       # Used for adding percentages to bar charts
library(ggthemes)
library(hrbrthemes) # Used for ipsum theme etc.
library(ggeasy) # Used for centring titles
library(reshape2) # Added for use with centred stacked bar charts
library(viridis) # Used for gradient colour schemes, as with violin plots
library("readxl")
library("htmlwidgets")  
library("ggstatsplot")
library(showtext)
library(glue) # used for clean_postcodes
library(magrittr) # used for clean_postcodes
require(RCurl) # used for fetching reproducible datasets
require(sf) # new simplefeature data class, supercedes sp in many ways
require(tmap) 
library(sp) # used to define CRS below only
library(flextable) # pretty tables
library(officer)
library(ggrepel)
library(patchwork) # used for side-by-side plotting (q19 etc.)
library(gridExtra)
library('foreign')
library(regions)
library(countrycode)
library(haven) # used for importing SPSS .sav files
# Establish formatting for visualisations -------------------------------------------

# Commenting out font loading as ragg uses system fonts
# Other users may need to install fonts used below (e.g. Abril Fatface and Roboto) which are included in this repository under `fonts`
## Serif font for titles and headings
# font_add_google(name = "Abril Fatface") 
## Sans serif font for body text
# font_add_google(name = "Roboto") 

# Define colour palettes
# TODO: confirm final colour scheme for charts and normalise across usage of different themes
coul3 <- brewer.pal(3, "RdYlBu") # Using RdYlBu range to generate 3 colour palette: https://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=5
coul4 <- brewer.pal(4, "RdYlBu")
coul5 <- brewer.pal(5, "RdYlBu")
coul6 <- brewer.pal(6, "RdYlBu")
coul7 <- brewer.pal(7, "RdYlBu")
coul4_reversed <- c("#2C7BB6", "#ABD9E9", "#FDAE61", "#D7191C")
coul6_reversed <- c("#4575B4", "#91BFDB" , "#E0F3F8" , "#FEE090", "#FC8D59", "#D73027")
white <- "#ffffff"
purple <- "#590048"
ochre <- "#B18839"
ochre_12 <- wheel(ochre, num = 12)
purple_12 <- wheel(purple, num = 12)

# Tweak themes

theme_ipsum_rc(
  plot_title_family = "Abril Fatface"
)

# Set flextable styling
set_flextable_defaults(font.size = 10, 
                       digits = 0, 
                       border.color = "#000000",
                       padding.bottom = 1,
                       padding.top = 1,
                       padding.left = 3,
                       padding.right = 1)

# Set up local workspace, as needed: --------------------------------------

# Set up paths in case they haven't already been created

if (dir.exists(here("gits", "spotlight-report", "data")) == FALSE) {
  dir.create(here("gits", "spotlight-report", "data")) 
}

# Note, these two paths are excluded from github as it is best practice in reproducible research for end-user to generate their own

if (dir.exists(here("gits", "spotlight-report", "figures")) == FALSE) {
  dir.create(here("gits", "spotlight-report", "figures"))
}

if (dir.exists(here("gits", "spotlight-report", "derivedData")) == FALSE) {
  dir.create(here("gits", "spotlight-report", "derivedData"))
}

## Reusable Functions ------------------------------------------------------

# Importing code for colortools() now deprecated and removed from CRAN here. Some minor modifications to update code, but generally all credit here goes to Gaston Sanchez

setColors <- function(color, num) {
  # convert to RGB
  rgb_col = col2rgb(color)
  # convert to HSV
  hsv_col = rgb2hsv(rgb_col)[,1]
  # get degree
  hue = hsv_col[1]
  sat = hsv_col[2]
  val = hsv_col[3]
  cols = seq(hue, hue + 1, by=1/num)
  cols = cols[1:num]
  cols[cols > 1] <- cols[cols > 1] - 1
  # get colors with hsv
  colors = hsv(cols, sat, val)
  # transparency
  if (substr(color, 1, 1) == "#" && nchar(color) == 9)
    ({
      alpha = substr(color, 8, 9)
      colors = paste(colors, alpha, sep="")
    })
  colors
}

complementary <- function(color, plot=TRUE, bg="white", labcol=NULL, cex=0.8, title=TRUE) {	
  tmp_cols = setColors(color, 12)
  comp_colors <- tmp_cols[c(1, 7)]
  
  # plot
  if (plot)
    ({
      # labels color
      if (is.null(labcol)) 
        ({
          lab_col = rep("", 12)
          if (mean(col2rgb(bg)) > 127)
            ({
              lab_col[c(1, 7)] <- "black"
              lab_col[c(2:6,8:12)] <- col2HSV(bg)
            }) else ({
              lab_col[c(1, 7)] <- "white"
              lab_col[c(2:6,8:12)] <- col2HSV(bg)
            })
        }) else ({
          lab_col = rep(labcol, 12)
          if (mean(col2rgb(bg)) > 127)
            ({
              lab_col[c(1, 7)] <- labcol
              lab_col[c(2:6,8:12)] <- col2HSV(bg)
            }) else ({
              lab_col[c(1, 7)] <- labcol
              lab_col[c(2:6,8:12)] <- col2HSV(bg)
            })
        })	
      # hide non-adjacent colors
      tmp_cols[c(2:6,8:12)] <- paste(substr(tmp_cols[c(2:6,8:12)],1,7), "0D", sep="")
      pizza(tmp_cols, labcol=lab_col, bg=bg, cex=cex)
      # title
      if (title)
        title(paste("Complementary (opposite) color of: ", tmp_cols[1]), 
              col.main=lab_col[1], cex.main=0.8)
    })
  # result
  comp_colors
}

sequential <- function(color, percentage=5, what="saturation", s=NULL, v=NULL, alpha=NULL, fun="linear", plot=TRUE, verbose=TRUE)  {
  # convert to HSV
  col_hsv = rgb2hsv(col2rgb(color))[,1]
  # transparency
  if (is.null(alpha))
    alpha = 1
  if (substr(color, 1, 1) == "#" && nchar(color) == 9)
    alpha = substr(color, 8, 9)
  # get hue, saturation, and value
  hue = col_hsv[1]
  if (is.null(s)) s = col_hsv[2]
  if (is.null(v)) v = col_hsv[3]
  # sequence function
  getseq = switch(fun, 
                  linear = seq(0, 1, by=percentage/100),
                  sqrt = sqrt(seq(0, 1, by=percentage/100)),
                  log = log1p(seq(0, 1, by=percentage/100)),
                  log10 = log10(seq(0, 1, by=percentage/100))
  )
  # what type of sequence?
  if (what == "saturation") ({
    sat = getseq
    fixed = paste("v=", round(v,2), " and alpha=", alpha, sep="")
    if (is.numeric(alpha))
      seq_col = hsv(hue, s=sat, v=v, alpha=alpha)
    if (is.character(alpha)) ({
      seq_col = hsv(hue, s=sat, v=v)
      seq_col = paste(seq_col, alpha, sep="")
    })
  })
  if (what == "value") ({
    val = getseq
    fixed = paste("s=", round(s,2), " and alpha=", alpha, sep="")
    if (is.numeric(alpha))
      seq_col = hsv(hue, s=s, v=val, alpha=alpha)
    if (is.character(alpha)) ({
      seq_col = hsv(hue, s=s, v=val)
      seq_col = paste(seq_col, alpha, sep="")
    })
  })
  if (what == "alpha") ({
    alpha = getseq
    fixed = paste("s=", round(s,2), " and v=", round(v,2), sep="")
    seq_col = hsv(hue, s=s, v=v, alpha=alpha)
  })
  # if plot TRUE
  if (plot)
    ({
      n = length(seq(0, 1, by=percentage/100))
      fx = unlist(fixed)
      #dev.new()
      plot(0, 0, type="n", xlim=c(0,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="")
      rect(0:(n-1)/n, 0, 1:n/n, 1, col=seq_col, border="lightgray")
      mtext(seq_col, side=1, at=0.5:(n)/n, cex=0.8, las=2)
      title(paste("Sequential colors based on ", what, "\n with fixed ", fx, sep=""),
            cex.main=0.9)
    })
  # result
  if (verbose)
    seq_col
}

wheel <- function(color, num=12, bg="gray95", border=NULL, init.angle=105, cex=1, lty=NULL, main=NULL, verbose=TRUE, ...) {
  if (!is.numeric(num) || any(is.na(num) | num < 0)) 
    stop("\n'num' must be positive")
  x <- rep(1, num)
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  # set colors
  col = setColors(color, num)
  labels = col
  # labels color
  labcol = ifelse( mean(col2rgb(bg)) > 127, "black", "white")
  # prepare plot window
  par(bg = bg)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L]) 
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  # get ready to plot
  if (is.null(border[1])) ({
    border <- rep(bg, length.out = nx)    
  }) else ({
    border <- rep(border, length.out = nx)    
  })
  if (!is.null(lty))
    lty <- rep(NULL, length.out = nx)
  angle <- rep(45, length.out = nx)
  radius = seq(1, 0, by=-1/num)[1:num]
  twopi <- -2 * pi
  t2xy <- function(t, rad) ({
    t2p <- twopi * t + init.angle * pi/180
    list(x = rad * cos(t2p), y = rad * sin(t2p))
  })
  # plot colored segments
  for (i in 1L:nx)
    ({
      n <- max(2, floor(200 * dx[i]))
      P <- t2xy(seq.int(x[i], x[i + 1], length.out = n), rad=radius[1])
      polygon(c(P$x, 0), c(P$y, 0), angle = angle[i], 
              border = border[i], col = col[i], lty = lty[i])
      P <- t2xy(mean(x[i + 0:1]), rad=radius[1])
      lab <- labels[i]
      if (!is.na(lab) && nzchar(lab)) ({
        adjs = 0.5
        if (P$x > 1e-08) adjs <- 0
        if (P$x < -1e-08) adjs <- 1
        lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
        text(1.1 * P$x, 1.1 * P$y, labels[i], xpd = TRUE, 
             adj = adjs, cex=cex, col=labcol, ...)
      })
    })
  # add title
  title(main = main, ...)
  # return color names
  if (verbose)
    col
}


# Create function which can be repeated for importing a column, sorting, and totalling data

# A function that puts a title text object centered above a ggplot object "p"
# h/t to Sam Firke: https://stackoverflow.com/questions/10975518/how-to-align-ggplot-title-with-window-rather-than-plot-grid
add_centered_title <- function(p, text){
  grid.arrange(p, ncol = 1, top = text)
}
  
qualtrics_process_single_multiple_choice <- function(x) {
  # create separate data frame
  df <- as.data.frame(x)
  # make column names coherent and simplified
  names(df) <- c("response")
  # filter out NA values
  df <- filter(df, !is.na(response))
  # generate new dataframe with sums per category and sort in descending order
  sums <- df %>%
    dplyr::count(response, sort = TRUE) %>% 
    dplyr::mutate(
      response = forcats::fct_rev(forcats::fct_inorder(response))
    )    
  # add new column with percentages for each sum
  sums <- sums %>% 
    dplyr::mutate(perc = scales::percent(n / sum(n), accuracy = 1, trim = FALSE))
}

qualtrics_process_single_multiple_choice_unsorted <- function(x) {
  # create separate data frame
  df <- as.data.frame(x)
  # make column names coherent and simplified
  names(df) <- c("response")
  # filter out NA values
  df <- filter(df, !is.na(response))
  # generate new dataframe with sums per category and sort in descending order
  sums <- df %>%
    dplyr::count(response, sort = FALSE) %>% 
    dplyr::mutate(
      response = forcats::fct_rev(forcats::fct_inorder(response))
    )    
  # add new column with percentages for each sum
  sums <- sums %>% 
    dplyr::mutate(perc = scales::percent(n / sum(n), accuracy = 1, trim = FALSE))
}

# function to produce horizontal bar chart, colours drawn from "ochre" colour wheel defined above to match report
plot_horizontal_bar <- function(x) {
  # simplified approach to colour generation
  fill = wheel(ochre, num = as.integer(count(x[1])))
  # make plot
  ggplot(x, aes(x = n, y = response, fill = fill)) +
    geom_col(colour = "white") +
    ## add percentage labels
    geom_text(aes(label = perc),
              ## make labels left-aligned and white
              hjust = 1, nudge_x = -.5, colour = "black", size=3) +
    ## reduce spacing between labels and bars
    scale_fill_identity(guide = "none") +
    ## get rid of all elements except y axis labels + adjust plot margin
    theme_ipsum_rc() +
    theme(plot.margin = margin(rep(15, 4))) +
    easy_center_title()
}

# function to produce a summary table of results for a single column using flextable

chart_single_result_flextable <- function(.data, var) {
  table <- table(.data)
  # add calculations and convert to a flextable object
  table %>%
    prop.table %>% # turn this into a table of proportions
    # flextable requires a dataframe
    as.data.frame() %>%
    set_names(c("Variable", "Count")) %>%
    # arrange in descending order
    arrange({{ var }}) %>%
    # convert table object to a flextable()
    flextable(defaults = TRUE) %>%
    # adjust column widths automatically to fit widest values
    style(part = 'body', pr_t=fp_text(font.family='Roboto')) %>%
    style(part = 'header', pr_t=fp_text(font.family='Roboto')) %>%
    # note, likert also uses set_caption() so need to specify flextable:: here
    flextable::set_caption(caption, style = "Table Caption", autonum = run_autonum(seq_id = "tab", bkm = "figures", bkm_all = TRUE)) %>%
    autofit() %>%
    theme_vanilla() %>%
    # format numbers in count column as rounded percentages
    set_formatter( table, Count = function(x) sprintf( "%.1f%%", x*100 ))
}

plot_single_result_with_facets <- function(x) {
    x %>% 
    # we need to get the data including facet info in long format, so we use pivot_longer()
    pivot_longer(!response, names_to = "bin_name", values_to = "b") %>% 
    # add counts for plot below
    count(response, bin_name, b) %>%
    filter(!is.na(response)) %>%
    group_by(bin_name,b) %>%
    mutate(perc=paste0(round(n*100/sum(n),0),"%")) %>% 
    # run ggplot
    ggplot(aes(x = n, y = "", fill = response, label = perc)) +
    geom_col(position=position_fill(), aes(fill=response)) +
    coord_cartesian(clip = "off") +
    geom_vline(xintercept = x_limits, linetype = 3) +
    geom_label_repel(
      # important to make sure grouping of data matches grouping of labels so they aren't backwards
      aes(group = response, label = perc),
      # justify text using center = 0.5, left = 0 and right = 1
      hjust = 0.5,
      direction = "y",
      force = 2,
      fill = "white", 
      # font size in the text labels
      size = 2.5,
      # allow labels to overlap
      max.overlaps = Inf,
      # make sure that bars are included
      position = position_fill(),
      # hide points
      segment.size = 0.2,
      point.size = NA,
      # reduce padding around each text label
      box.padding = 0.01
    ) +
    scale_fill_brewer(palette="YlOrBr") +
    scale_x_continuous(labels = scales::percent_format(), expand = c(0.05, 0.05)) +
    facet_grid(vars(b), vars(bin_name), labeller=as_labeller(facet_names)) + 
    labs(title = title, caption = caption, x = "", y = "") + 
    guides(fill = guide_legend(title = NULL)) +
    theme_classic()
}

# All credit for the following code chunk goes to https://github.com/shanwilkinson2/cleaning_postcodes

clean_postcodes <- function(pcodes) {  
  
  # regex to match postcode format  
  pcode_regex <- "^[A-Z]{1,2}\\d[A-Z\\d]? {1}\\d[A-Z]{2}$"
  
  # create a dataframe to hold input postcode, whether postcode is valid as it is, 
  # output postcode, whether pcode is finally valid
  output <- data.frame(input_pcode = as.character(pcodes), 
                       input_valid = str_detect(pcodes, pcode_regex),
                       output_pcode = as.character(pcodes),
                       stringsAsFactors = FALSE)
  output$output_valid <- output$input_valid
  
  # trim trailing & leading whitespace, convert to uppercase, replace double spaces, check if valid now   
  output$output_pcode <- ifelse(output$output_valid == FALSE, 
                                yes = output$output_pcode %>%
                                  str_trim() %>%
                                  str_to_upper() %>%
                                  str_replace_all("  ", " "), 
                                no = output$output_pcode)
  
  # check for postcode validity 
  # <<- assigns from parent environments 
  # <- doesn't work as it only uses a local version within the check_again function
  check_again <- function() {
    output$output_valid <<- ifelse(str_detect(output$output_pcode, 
                                              pcode_regex), 
                                   yes = TRUE, no = FALSE)
  }
  check_again()
  
  # get rid of any special characters & check again
  # [[:punct:]] = punctuation
  output$output_pcode <- ifelse(output$output_valid == FALSE, 
                                yes = output$output_pcode %>%
                                  str_replace_all("!", "1") %>%
                                  str_replace_all('\"', '2') %>% 
                                  str_replace_all("\\$", "4") %>%
                                  str_replace_all("£", "3") %>%
                                  str_replace_all("%", "5") %>%
                                  str_replace_all('\\^', '6') %>% 
                                  str_replace_all("&", "7") %>%
                                  str_replace_all("\\*", "8") %>%
                                  str_replace_all("\\(", "9") %>%
                                  str_replace_all("\\)", "0") %>%
                                  str_replace_all("[[:punct:]]", ""),
                                no = output$output_pcode)
  check_again()
  
  # strip postcode out of other text
  output$output_pcode <- ifelse(output$output_valid == FALSE, 
                                yes = ifelse(str_detect(output$output_pcode, "[A-Z]{1,2}\\d[A-Z\\d]? {1}\\d[A-Z]{2}") == TRUE, 
                                             yes = str_extract(output$output_pcode, "[A-Z]{1,2}\\d[A-Z\\d]? {1}\\d[A-Z]{2}"),
                                             no = output$output_pcode),
                                no = output$output_pcode)
  check_again()
  
  # dodgy spacing 
  # more than one space - get rid of all spaces
  output$output_pcode <- ifelse(output$output_valid == FALSE &
                                  between(str_count(output$output_pcode, "[:alnum:]"), 5, 7) & # 5-7 letters and/or numbers 
                                  str_count(output$output_pcode, "\\s") > 1, # more than 1 space
                                yes = str_replace_all(output$output_pcode, "\\s", ""), # no spaces, can put single space in below
                                no = output$output_pcode)  
  
  # one space in the wrong place - get rid of it
  output$output_pcode <- ifelse((output$output_valid == FALSE & 
                                   str_count(output$output_pcode, "\\s") == 1 & 
                                   str_detect(output$output_pcode, "^[A-Z]{1,2}\\d[A-Z\\d]? {1}") == FALSE), # first half of postcode with space in right place
                                yes = str_replace_all(output$output_pcode, "\\s", ""), 
                                no = output$output_pcode)
  # no spaces - if postcode length is between 5 & 7, put one in 4 from the end
  output$output_pcode <- ifelse((output$output_valid == FALSE & 
                                   str_count(output$output_pcode, "\\s") == 0 & 
                                   between(nchar(output$output_pcode), 5, 7)),
                                yes = paste(str_sub(output$output_pcode, 1, -4), 
                                            str_sub(output$output_pcode, -3, -1)), 
                                no = output$output_pcode)
  check_again()
  
  # numbers to letters & vice versa (but only in the second half as where they tend to crop up)
  # o to zero
  output$output_pcode <- ifelse((output$output_valid == FALSE & 
                                   str_detect(output$output_pcode, "^[A-Z]{1,2}\\d[A-Z\\d]? {1}O")), # first half of postcode followed by letter O
                                yes = paste0(str_sub(output$output_pcode, 1, -4), "0",
                                             str_sub(output$output_pcode, -2, -1)), 
                                no = output$output_pcode)
  # check again
  check_again()  
  
  # message - summary of those that needed cleaning & how many were successfully cleaned
  
  cleaning_stats <- output[output$input_valid == FALSE, c(2,4)]
  message(glue("{length(which(cleaning_stats$output_valid == TRUE))}/{nrow(cleaning_stats)} ",
               "({round(length(which(cleaning_stats$output_valid == TRUE))/nrow(cleaning_stats)*100)}%) ",
               "of initially invalid postcodes were successfully cleaned"))
  return(output)
  
}


# This doesn't cover overseas territories and only enforces the format, NOT the existence of different areas. It is based on the following rules:
# 
# Can accept the following formats:
# 
# “GIR 0AA”
# A9 9ZZ
# A99 9ZZ
# AB9 9ZZ
# AB99 9ZZ
# A9C 9ZZ
# AD9E 9ZZ
# Where:
# 
# 9 can be any single digit number.
# A can be any letter except for Q, V or X.
# B can be any letter except for I, J or Z.
# C can be any letter except for I, L, M, N, O, P, Q, R, V, X, Y or Z.
# D can be any letter except for I, J or Z.
# E can be any of A, B, E, H, M, N, P, R, V, W, X or Y.
# Z can be any letter except for C, I, K, M, O or V.

# Load data ---------------------------------------------------------------

# TODO: upload non-embargoed data to zenodo and change code below
# TODO: drop QATTN1b, QATTN2

climate_experience_data <- read_sav(here("gits", "spotlight-report", "data", "climate_experience_data_final.sav")) %>% select(Q0:Q68)
print_labels(climate_experience_data$Age_grp)

# download local authorities data for whole UK - level 1 (least detailed)
if (file.exists(here("gits", "spotlight-report", "data", "gis", "infuse_dist_lyr_2011.shp")) == FALSE) {
  download.file("https://zenodo.org/record/6395804/files/infuse_dist_lyr_2011_simplified_100m.gpkg?download=1", destfile = here("gits", "spotlight-report", "data", "gis", "infuse_dist_lyr_2011_simplified.gpkg"))}
local_authorities <- st_read(here("gits", "spotlight-report", "data", "gis", "infuse_dist_lyr_2011.shp"))

if (file.exists(here("gits", "spotlight-report", "data", "gis", "infuse_dist_lyr_2011_simplified_100m_buildings_simplified.gpkg")) == FALSE) {
  download.file("https://zenodo.org/record/6395804/files/infuse_dist_lyr_2011_simplified_100m_buildings_overlay_simplified.gpkg?download=1", destfile = here("gits", "spotlight-report", "data", "gis", "infuse_dist_lyr_2011_simplified_100m_buildings_simplified.gpkg"))}
local_authorities_buildings_clip <- st_read(here("gits", "spotlight-report", "data", "gis", "infuse_dist_lyr_2011_simplified_100m_buildings_simplified.gpkg"))
  
# Subsetting --------------------------------------------------------------

# Add subset for income levels 

# process into hi/med/low bins based on responses
climate_experience_data <- climate_experience_data %>%
  mutate(
    income_bin = case_when(
      Q67 > 6 ~ "high",
      Q67 < 3 ~ "low",
      TRUE ~ "medium"
    ) %>% factor(levels = c("low", "medium", "high"))
  )

## we need to convert data to factors on named response data
climate_experience_data_named$Q67 <- factor(climate_experience_data_named$Q67, ordered = TRUE, levels = c("Less than £10,000 p/a", "£10,000 - £20,000 p/a", "£21,000 – £30,000 p/a", "£31,000 – £40,000 p/a", "£41,000 – £50,000 p/a", "£51,000 – £60, 000 p/a", "£61,000 - £70,000 p/a", "Over £70,000 p/a"))
climate_experience_data$Q67

climate_experience_data$Q67 <- climate_experience_data$Q67 %>%
  mutate(
    income_bin = case_when(
      as.data.frame(climate_experience_data$Q67) > 6 ~ "high",
      as.data.frame(climate_experience_data$Q67) < 3 ~ "low",
      TRUE ~ "medium"
    ) %>% factor(levels = c("low", "medium", "high"))
  )


climate_experience_data_named <- climate_experience_data_named %>%
  mutate(
    income_bin = case_when(
      as.integer(factor(climate_experience_data_named$Q67)) > 6 ~ "high",
      as.integer(factor(climate_experience_data_named$Q67)) < 3 ~ "low",
      TRUE ~ "medium"
    ) %>% factor(levels = c("low", "medium", "high"))
  )

## Q8 subsetting based on confidence on views
climate_experience_data <- climate_experience_data %>%
  mutate(
    Q8_bin = case_when(
      Q8_1 > mean(Q8_1) + sd(Q8_1) ~ "high",
      Q8_1 < mean(Q8_1) - sd(Q8_1) ~ "low",
      TRUE ~ "medium"
    ) %>% factor(levels = c("low", "medium", "high"))
  )

climate_experience_data_named <- climate_experience_data_named %>%
  mutate(
    Q8_bin = case_when(
      Q8_1 > mean(Q8_1) + sd(Q8_1) ~ "high",
      Q8_1 < mean(Q8_1) - sd(Q8_1) ~ "low",
      TRUE ~ "medium"
    ) %>% factor(levels = c("low", "medium", "high"))
  )

# Q53 subsetting based on Political LR orientation:
# Generate low/med/high bins based on Mean and SD
climate_experience_data <- climate_experience_data %>%
  mutate(
    Q53_bin = case_when(
      Q53_1 > mean(Q53_1) + sd(Q53_1) ~ "high",
      Q53_1 < mean(Q53_1) - sd(Q53_1) ~ "low",
      TRUE ~ "medium"
    ) %>% factor(levels = c("low", "medium", "high"))
  )

climate_experience_data_named <- climate_experience_data_named %>%
  mutate(
    Q53_bin = case_when(
      Q53_1 > mean(Q53_1) + sd(Q53_1) ~ "high",
      Q53_1 < mean(Q53_1) - sd(Q53_1) ~ "low",
      TRUE ~ "medium"
    ) %>% factor(levels = c("low", "medium", "high"))
  )

## Q57 subsetting based on Religiosity
climate_experience_data <- climate_experience_data %>%
  mutate(
    Q57_bin = case_when(
      Q57_1 > mean(Q57_1) + sd(Q57_1) ~ "high",
      Q57_1 < mean(Q57_1) - sd(Q57_1) ~ "low",
      TRUE ~ "medium"
    ) %>% factor(levels = c("low", "medium", "high"))
  )

climate_experience_data_named <- climate_experience_data_named %>%
  mutate(
    Q57_bin = case_when(
      Q57_1 > mean(Q57_1) + sd(Q57_1) ~ "high",
      Q57_1 < mean(Q57_1) - sd(Q57_1) ~ "low",
      TRUE ~ "medium"
    ) %>% factor(levels = c("low", "medium", "high"))
  )

## Subsetting based on Spirituality
# Calculate overall mean nature-relatedness score based on six questions:
climate_experience_data$Q51_score <- rowMeans(select(climate_experience_data, Q51_remote_vacation:Q51_heritage))

# Create low/med/high bins based on Mean and +1/-1 Standard Deviation
climate_experience_data <- climate_experience_data %>%
  mutate(
    Q51_bin = case_when(
      Q51_score > mean(Q51_score) + sd(Q51_score) ~ "high",
      Q51_score < mean(Q51_score) - sd(Q51_score) ~ "low",
      TRUE ~ "medium"
    ) %>% factor(levels = c("low", "medium", "high"))
  )

# TODO: fix issues with rowMeans by converting to factors, code below still incomplete 
climate_experience_data_named$Q51_remote_vacation <- factor(climate_experience_data_named$Q51_remote_vacation, ordered = TRUE, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
climate_experience_data_named$Q51_impact_awareness <- factor(climate_experience_data_named$Q51_impact_awareness, ordered = TRUE, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
climate_experience_data_named$Q51_spirituality <- factor(climate_experience_data_named$Q51_spirituality, ordered = TRUE, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
climate_experience_data_named$Q51_wildlife <- factor(climate_experience_data_named$Q51_wildlife, ordered = TRUE, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
climate_experience_data_named$Q51_relationship <- factor(climate_experience_data_named$Q51_relationship, ordered = TRUE, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
climate_experience_data_named$Q51_heritage <- factor(climate_experience_data_named$Q51_heritage, ordered = TRUE, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))

climate_experience_data_named$Q51_score <- rowMeans(as.integer(factor(select(climate_experience_data_named, Q51_remote_vacation:Q51_heritage))))

# Calculate overall mean spirituality score based on six questions:
climate_experience_data$Q52_score <- rowMeans(select(climate_experience_data, Q52a_1:Q52f_1))
climate_experience_data_named$Q52_score <- rowMeans(select(climate_experience_data_named, Q52a_1:Q52f_1))

# Create low/med/high bins based on Mean and +1/-1 Standard Deviation
climate_experience_data <- climate_experience_data %>%
  mutate(
    Q52_bin = case_when(
      Q52_score > mean(Q52_score) + sd(Q52_score) ~ "high",
      Q52_score < mean(Q52_score) - sd(Q52_score) ~ "low",
      TRUE ~ "medium"
    ) %>% factor(levels = c("low", "medium", "high"))
  )

climate_experience_data_named <- climate_experience_data_named %>%
  mutate(
    Q52_bin = case_when(
      Q52_score > mean(Q52_score) + sd(Q52_score) ~ "high",
      Q52_score < mean(Q52_score) - sd(Q52_score) ~ "low",
      TRUE ~ "medium"
    ) %>% factor(levels = c("low", "medium", "high"))
  )


# Generate new column with simplified postcode area data (only initial alpha characters)
climate_experience_data_named$postcode_area <- str_to_upper(str_extract(climate_experience_data_named$Q68, "^([A-Z,a-z]){1,}"))
climate_experience_data$postcode_area <- str_to_upper(str_extract(climate_experience_data_named$Q68, "^([A-Z,a-z]){1,}"))

# Add identifier flagging London residents
climate_experience_data_named <- climate_experience_data_named %>%
  rowwise() %>%
  mutate(
    isLondon = case_when(
      postcode_area == "E" ~ "1",
      postcode_area == "EC" ~ "1",
      postcode_area == "N" ~ "1",
      postcode_area == "NW" ~ "1",
      postcode_area == "SE" ~ "1",
      postcode_area == "SW" ~ "1",
      postcode_area == "W" ~ "1",
      postcode_area == "WC" ~ "1",
      TRUE ~ "0"
    )
  )

climate_experience_data <- climate_experience_data %>%
  rowwise() %>%
  mutate(
    isLondon = case_when(
      postcode_area == "E" ~ "1",
      postcode_area == "EC" ~ "1",
      postcode_area == "N" ~ "1",
      postcode_area == "NW" ~ "1",
      postcode_area == "SE" ~ "1",
      postcode_area == "SW" ~ "1",
      postcode_area == "W" ~ "1",
      postcode_area == "WC" ~ "1",
      TRUE ~ "0"
    )
  )

# Beginning of data visualisations ----------------------------------------

## Figure 1. Where are our respondents based? ----------------------------------------

# Summary of approach to this analysis:
# Use cbind to connect matching columns from climate_experience_data$Q68 to uk_postcodes$pcd 
# and draw in uk_postcodes$oa11 for census data analysis, as well as uk_postcodes$oseast1m and
# uk_postcodes$osnrth1m for coordinates to match onto UR8
# Abstract data to postcode region and drop columns with identifiable information

# 1. Import and clean postcodes non-geocoded data
# uk_postcodes <- read.csv(here("gits", "spotlight-report", "data", "gis", "ONSPD_FEB_2020_UK.csv"))
## TODO: add lsoa for deprivation lookups
#uk_postcodes_simple <- select(uk_postcodes, pcd, oa11, lsoa11, lat, long)
#write.csv(uk_postcodes_simple, here("gits", "spotlight-report", "data", "gis", "uk_postcodes_simple.csv"), row.names=FALSE)

uk_postcodes_simple <- read.csv(here("gits", "spotlight-report", "data", "gis", "uk_postcodes_simple.csv"))
imd2019 <- read.csv(here("gits", "spotlight-report", "data", "gis", "File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv"))

# Previous work to clean dataset
# urban_rural_2fold <- read_excel(here("gits", "spotlight-report", "data", "gis", "rural_urban2_uk_simplified.xlsx"))
# urban_rural_2fold$ur2 <- gsub("1", "Urban", urban_rural_2fold$ur2)
# urban_rural_2fold$ur2 <- gsub("2", "Rural", urban_rural_2fold$ur2)
# write.csv(urban_rural_2fold, here("gits", "spotlight-report", "derivedData", "urban_rural_2fold.csv"))

uk_ur2 <- read.csv(here("gits", "spotlight-report", "derivedData", "urban_rural_2fold.csv"))

postcode_data <- climate_experience_data$Q68
postcode_data_cleaned <- clean_postcodes(postcode_data)
postcode_data_cleaned <- select(postcode_data_cleaned, input_pcode, output_pcode)
names(postcode_data_cleaned) <- c("original_pcd", "pcd")

# https://dplyr.tidyverse.org/reference/mutate-joins.html
postcode_data_merged1 <- list(postcode_data_cleaned, uk_postcodes_simple) %>% reduce(inner_join, by='pcd')
# https://stackoverflow.com/questions/10655438/rename-one-named-column-in-r
# rename column so they match for inner join
imd2019_renamed <- rename(imd2019, lsoa11 = LSOA.code..2011.)

# inner join on datasets to merge in imd data
postcode_data_merged2 <- list(postcode_data_merged1, imd2019_renamed) %>% reduce(inner_join, by='lsoa11')

# additional inner join on datasets to merge in urban_rural data
uk_ur2 <- rename(uk_ur2, lsoa11 = lsoa)
postcode_data_merged_ur2 <- list(postcode_data_merged2, uk_ur2) %>% reduce(inner_join, by='lsoa11')

ur2_data <- as.data.frame(postcode_data_merged_ur2$ur2)
# make column names coherent and simplified
names(ur2_data) <- c("response")
# generate new dataframe with sums per category and sort in descending order
ur2_data_sums <- ur2_data %>% 
  dplyr::count(response, sort = TRUE) %>% 
  dplyr::mutate(
    response = forcats::fct_rev(forcats::fct_inorder(response))
  )    
# add new column with percentages for each sum
sums <- sums %>% 
  dplyr::mutate(perc = scales::percent(n / sum(n), accuracy = 1, trim = FALSE))

# merge joined subset with entire dataset
postcode_data_merged2 <- rename(postcode_data_merged2, Q68 = original_pcd)
complete_data_merge <- list(postcode_data_merged2, climate_experience_data) %>% reduce(inner_join, by='Q68')

## Calculate quantity of respondents in each admin area:
# All data up until this point has not been geocoded, now we need to convert some dataframes to be spatialpoints dataframes (geocoded objects in R) and import other already geocoded datasets

# convert surveys data to geocoded dataframe
postcode_data_merged2_sf_wgs <- st_as_sf(postcode_data_merged2, coords = c("long", "lat"))
st_crs(postcode_data_merged2_sf_wgs) <- 4326

# Original data is in wgs, so need to convert from wgs to bng to work with analysis across data sets as everything from ONS comes as 27700 projection
postcode_data_merged2_sf <- st_transform(postcode_data_merged2_sf_wgs, 27700)

## Generate a table of frequencies for each set of points in admin_lev1
## calculate count of respondents for fields in admin and provide percentages
# oa11$surveys_count <- lengths(st_covers(oa11, postcode_data_merged2_sf))
# oa11$surveys_percent<- prop.table(oa11$surveys_count)

# lsoa11$surveys_count <- lengths(st_covers(lsoa11, postcode_data_merged2_sf))
# lsoa11$surveys_percent<- prop.table(lsoa11$surveys_count)

local_authorities$surveys_count <- lengths(st_covers(local_authorities, postcode_data_merged2_sf))
local_authorities$surveys_percent <- prop.table(local_authorities$surveys_count)

# using temporary palette here so that 0s are white
palette <- c(white, "#a8ddb5", "#43a2ca")
map1 <- tm_shape(local_authorities) + 
  tm_fill(col = "surveys_count", , palette = palette, title = "Concentration of survey respondents") +
  tm_borders(alpha=.5, lwd=0.1) +
  # for intermediate polygon geometries
  # tm_shape(local_authorities) +
  # tm_borders(lwd=0.6) +
  # for dots from original dataset
  # tm_dots("red", size = .05, alpha = .4) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_style("gray") +
  tm_credits("Data: UK Data Service (OGL)\n& Jeremy H. Kidwell,\nGraphic is CC-by-SA 4.0", 
             size = 0.4, 
             position = c("left", "bottom"),
             just = c("left", "bottom"),
             align = "left") +
  tm_layout(asp = NA,
            frame = FALSE, 
            title = "Figure 1a", 
            title.size = .7,
            legend.title.size = .7,
            inner.margins = c(0.1, 0.1, 0.05, 0.05)
  )
map1

# save image
tmap_save(map1, here("gits", "spotlight-report", "figures", "map.png"), width=1920, height=1080, asp=0)
tmap_save(map1, here("gits", "spotlight-report", "figures", "map.html"))
# st_write(local_authorities_clipped, "./derivedData/", "test_clipping2.shp", driver="ESRI Shapefile")

## Working with country codes ----------------------------------------------

climate_experience_data$Q62_iso <- countrycode(climate_experience_data$Q62_1_TEXT, origin = 'country.name', destination = 'iso3c', origin_regex = TRUE)
climate_experience_data$country_name <- countrycode(climate_experience_data$Q62_iso, origin = 'iso3c', destination = 'country.name', origin_regex = TRUE)
climate_experience_data$Q62_region <- countrycode(climate_experience_data$Q62_iso, origin = 'iso3c', destination = 'un.region.name')


## Generate choropleth map of the countries of non-UK born respondents--------

q62_countries_table <- as.data.frame(table(climate_experience_data_named$country_name))
names(q62_countries_table) <- c("region", "value")

# Using ggplot for this plot just for fun:

map.world <- map_data("world") 
map.world_joined <- left_join(map.world, q62_countries_table, by = c('region' = 'region'))
map.world_joined <- map.world_joined %>% mutate(fill_flg = ifelse(is.na(value),F,T))
head(map.world_joined)

ggplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = fill_flg)) +
  scale_fill_manual(values = c("#CCCCCC","#e60000")) +
  labs(title = 'In what country were you born?') +
  theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
        ,panel.background = element_rect(fill = "#444444")
        ,plot.background = element_rect(fill = "#444444")
        ,panel.grid = element_blank()
        ,plot.title = element_text(size = 20)
        ,plot.subtitle = element_text(size = 10)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none"
  )
ggsave(here("gits", "spotlight-report", "figures", "q62_country_born.png"), width = 20, height = 10, units = "cm")


## Figure 2. Beliefs about reality and cause of climate change ----------------------------------------

# Q6

# Basic summary plot of Q6 responses
title <- "Do you think the climate is changing?"
# reverting to dataframe to use existing libraries with new haven() spss sav import of data
q6_data <- qualtrics_process_single_multiple_choice(as.data.frame(as_factor(climate_experience_data$Q6)))
q6_data_plot <- plot_horizontal_bar(q6_data)
q6_data_plot <- q6_data_plot + labs(title = title, x = "", y = "") + theme(plot.title = element_text(size =12, hjust = 0))
q6_data_plot 
ggsave("figures/q6.png", width = 15, height = 10, units = "cm")

# Faceted plot working with 3x3 grid
df <- select(climate_experience_data, Q52_bin, Q53_bin, Q57_bin, Q6)
names(df) <- c("Q52_bin", "Q53_bin", "Q57_bin", "response")
facet_names <- c(`Q52_bin` = "Spirituality", `Q53_bin` = "Politics L/R", `Q57_bin` = "Religiosity", `low`="low", `medium`="medium", `high`="high")
facet_labeller <- function(variable,value){return(facet_names[value])}
q6_levels = c("Definitely changing", "Probably changing", "Probably not changing", "Definitely not changing")
df$response <- factor(df$response, ordered = TRUE, levels = c("4", "3", "2", "1"))
df$response <- fct_recode(df$response, "Definitely not changing" = "1", "Probably not changing" = "2", "Probably changing" = "3", "Definitely changing" = "4")
# TODO: not working with function, need to sort out why
df %>% 
  # we need to get the data including facet info in long format, so we use pivot_longer()
  pivot_longer(!response, names_to = "bin_name", values_to = "b") %>% 
  # add counts for plot below
  count(response, bin_name, b) %>%
  group_by(bin_name,b) %>%
  mutate(perc=paste0(round(n*100/sum(n),1),"%")) %>% 
  # run ggplot
  ggplot(aes(x = n, y = "", fill = response)) +
  geom_col(position=position_fill(), aes(fill=response)) +
  geom_text(aes(label = perc), position = position_fill(vjust=.5), size=2) +
  scale_fill_brewer(palette="YlOrBr") +
  scale_x_continuous(labels = scales::percent_format()) +
  facet_grid(vars(b), vars(bin_name), labeller=as_labeller(facet_names)) + 
  labs(caption = caption, x = "", y = "") + 
  guides(fill = guide_legend(title = NULL))
ggsave("figures/q6_faceted.png", width = 20, height = 10, units = "cm")

# Q7

# Basic summary plot of Q7 responses
title <- "Which, if any, of the following best describes your \nopinion about the causes of climate change?"
q7_data <- qualtrics_process_single_multiple_choice_unsorted(climate_experience_data_named$Q7)
q7_levels = c("Other", "don't know", "no such thing", "entirely natural processes", "mainly natural processes", "partly natural processes partly human activity", "mainly human activity", "entirely human activity")
q7_data$response <- factor(q7_data$response, levels = q7_levels)
q7_data_plot <- plot_horizontal_bar(q7_data)
q7_data_plot <- q7_data_plot + labs(title = title, x = "", y = "") + theme(plot.title = element_text(size =12, hjust = 0))
q7_data_plot 
ggsave("figures/q7.png", width = 20, height = 10, units = "cm")

# Faceted plot working with 3x3 grid
df <- select(climate_experience_data, Q52_bin, Q53_bin, Q57_bin, Q8_bin, Q7)
names(df) <- c("Q52_bin", "Q53_bin", "Q57_bin", "Q8_bin", "response")
facet_names <- c(`Q8_bin` = "Confidence", `Q52_bin` = "Spirituality", `Q53_bin` = "Politics L/R", `Q57_bin` = "Religiosity", `low`="low", `medium`="medium", `high`="high")
facet_labeller <- function(variable,value){return(facet_names[value])}
df$response <- factor(df$response, ordered = TRUE, levels = c("1", "2", "3", "4", "5", "97", "98", "99"))
df$response <- fct_recode(df$response, "It is caused by other factors not mentioned here" = "97", "Don’t know" = "99", "There is no such thing as climate change" = "98", "It is entirely caused by natural processes" = "1", "It is mainly caused by natural processes" = "2", "It is partly caused by natural processes \nand partly caused by human activity" = "3", "It is mainly caused by human activity" = "4", "It is entirely caused by human activity" = "5")
df %>% 
  # we need to get the data including facet info in long format, so we use pivot_longer()
  pivot_longer(!response, names_to = "bin_name", values_to = "b") %>% 
  # add counts for plot below
  count(response, bin_name, b) %>%
  group_by(bin_name,b) %>%
  mutate(perc=paste0(round(n*100/sum(n),1),"%")) %>% 
  # run ggplot
  ggplot(aes(x = n, y = "", fill = response)) +
  geom_col(position=position_fill(), aes(fill=response)) +
  geom_text(aes(label = perc), position = position_fill(vjust=.5), size=2) +
  scale_fill_brewer(palette = "Dark2", type = "qual") +
  scale_x_continuous(labels = scales::percent_format()) +
  facet_grid(vars(b), vars(bin_name), labeller=as_labeller(facet_names)) + 
  labs(caption = caption, x = "", y = "") + 
  guides(fill = guide_legend(title = NULL))
ggsave("figures/q7_faceted.png", width = 30, height = 10, units = "cm")


## Figure 5. Perceived severity of threat posed by climate change ----------------------------------------

# Q25 - generate diverging stacked bar chart using likert()
title <- "How serious a threat do you think \nclimate change poses to the following?"
caption <- "Jeremy H. Kidwell and Charles Ogunbode,\nGraphic is CC-by-SA 4.0"
q25_data <- select(climate_experience_data, Q25_self_and_family:Q25_outside_uk)
names(q25_data) <- c("You and your family in the UK", "People in your local area or city", "The UK as a whole", "Your family and/or friends living outside the UK")
# Set up levels text for question responses
q25_levels <- paste(c("not at all", "somewhat", "moderately", "very", "extremely"),  
                    "serious")
q25_likert_table <- q25_data %>% 
  mutate(across(everything(), 
                factor, ordered = TRUE, levels = 1:5, labels=q25_levels)) %>% 
  as.data.frame %>% 
  likert

plot(q25_likert_table, centered=FALSE, wrap=20, text.size=3, ordered=TRUE, low.color='#B18839', high.color='#590048') + 
  ggtitle(title) +
  labs(title = title, caption = NULL, y="") + 
  guides(fill = guide_legend(title = NULL)) + 
  theme_ipsum_rc() +
  theme(axis.text.y = element_text(size = 9))

# save to png file for reports
ggsave("figures/figure5.png", width = 20, height = 10, units = "cm")


## Figure 6. Worry about effects of climate change in places outside the UK ----------------------------------------

# Q28

title <- "Are you worried about the effects of climate change \nin places other than the UK?"
caption <- "Jeremy H. Kidwell and Charles Ogunbode,\nGraphic is CC-by-SA 4.0"

# using ggplot
worry_elsewhere_data <- qualtrics_process_single_multiple_choice_unsorted(climate_experience_data_named$Q28)
# add percentages for plot
worry_elsewhere_data <- worry_elsewhere_data %>% 
  dplyr::mutate(perc = scales::percent(n / sum(n), accuracy = 1, trim = FALSE))
worry_elsewhere_ggpie <- ggplot(worry_elsewhere_data, aes(x = "", y = n, fill = response)) +
  geom_col() +
  geom_text(aes(label = perc),
            color = "white",
            position = position_stack(vjust = 0.5),
            show.legend = FALSE) +
  coord_polar(theta = "y") +
  # colours for slices
  scale_fill_manual(values = c("#3962B1", "#B1399E")) + 
  # remove labels and add caption
  labs(title = title, x = "", y = "") + 
  # remove extra lines
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = white),
        panel.background = element_rect(fill = white)) +
  # remove legend title
  guides(fill = guide_legend(title = NULL))
worry_elsewhere_ggpie

ggsave("figures/figure6.png", width = 20, height = 10, units = "cm")



## Figure 7. Support for climate policies ----------------------------------------

# Q34

caption <- "Jeremy H. Kidwell and Charles Ogunbode,\nGraphic is CC-by-SA 4.0"
title <- "To what extent do you support or oppose the following policies?"
q34_data <- select(climate_experience_data, Q34_ev:Q34_loss_and_damage)
names(q34_data) <- c("Subsidies for electric (emission-free) vehicles", "Improving public transport", "Increasing taxes on any use of fossil fuels", "Increasing the price of electricity to reduce our consumption", "Increasing taxes on carbon-intensive foods like meat and dairy products", "Additional charges for people who fly more than twice a year", "Increasing taxes on companies and industries that emit large amounts of carbon", "Using public money to subsidise renewable energy (such as wind and solar power", "Including nuclear power in the energy mix", "Spending public money now to prepare the UK for the impacts of climate change", "Spending public money to help people in developing countries adapt to harmful climate change impacts", "Using public money to compensate developing countries for loss and damage")
q34_levels <- c("strongly oppose", "tend to oppose", "tend to support", "strongly support")
q34_likert_table <- q34_data %>% 
  mutate(across(everything(), 
                factor, ordered = TRUE, levels = 1:4, labels=q34_levels)) %>% 
  as.data.frame %>% 
  likert
plot(q34_likert_table, wrap=45, text.size=3, ordered=TRUE, low.color='#B18839', high.color='#590048') + 
  labs(title = title, caption = NULL, y="") + 
  guides(fill = guide_legend(title = NULL)) + 
  theme_ipsum() +
  theme(plot.title = element_text(lineheight = 0.9, size =12, hjust = 0)) +
  scale_y_continuous(labels = abs, limits = c(-115, 115))
# save to png file for reports
ggsave("figures/figure7.png", width = 20, height = 20, units = "cm")

# Grouped likert plot using income_bin data
q34_data <- select(climate_experience_data, Q34_ev:Q34_loss_and_damage, income_bin)
q34_levels <- c("strongly oppose", "tend to oppose", "tend to support", "strongly support")
names(q34_data) <- c("Subsidies for electric (emission-free) vehicles", "Improving public transport", "Increasing taxes on any use of fossil fuels", "Increasing the price of electricity to reduce our consumption", "Increasing taxes on carbon-intensive foods like meat and dairy products", "Additional charges for people who fly more than twice a year", "Increasing taxes on companies and industries that emit large amounts of carbon", "Using public money to subsidise renewable energy (such as wind and solar power", "Including nuclear power in the energy mix", "Spending public money now to prepare the UK for the impacts of climate change", "Spending public money to help people in developing countries adapt to harmful climate change impacts", "Using public money to compensate developing countries for loss and damage", "income_bin")
q34_likert_table <- q34_data %>% 
  mutate(across(everything(), 
                factor, ordered = TRUE, levels = 1:4, labels=q34_levels)) %>% 
  as.data.frame %>% 
  likert(grouping=q34_data$income_bin)
q34_likert_table %>% 
  select(-c(income_bin))
plot(q34_likert_table, wrap = 120, ordered=TRUE, low.color='#B18839', high.color='#590048') + 
  labs(title = title, caption = caption, y="") + 
  guides(fill = guide_legend(title = NULL))
ggsave("figures/q34_faceted_income.png", width = 20, height = 25, units = "cm")

# Grouped likert plot using political_lr data
q34_data <- select(climate_experience_data, Q34_ev:Q34_loss_and_damage, Q53_bin)
q34_levels <- c("strongly oppose", "tend to oppose", "tend to support", "strongly support")
names(q34_data) <- c("Subsidies for electric (emission-free) vehicles", "Improving public transport", "Increasing taxes on any use of fossil fuels", "Increasing the price of electricity to reduce our consumption", "Increasing taxes on carbon-intensive foods like meat and dairy products", "Additional charges for people who fly more than twice a year", "Increasing taxes on companies and industries that emit large amounts of carbon", "Using public money to subsidise renewable energy (such as wind and solar power", "Including nuclear power in the energy mix", "Spending public money now to prepare the UK for the impacts of climate change", "Spending public money to help people in developing countries adapt to harmful climate change impacts", "Using public money to compensate developing countries for loss and damage", "Q53_bin")
q34_data$Q53_bin <- fct_recode(q34_data$Q53_bin, "Right" = "high", "Centre" = "medium", "Left" = "low")
q34_likert_table <- q34_data %>% 
  mutate(across(everything(), 
                factor, ordered = TRUE, levels = 1:4, labels=q34_levels)) %>% 
  as.data.frame %>% 
  likert(grouping=q34_data$Q53_bin)
plot(q34_likert_table, wrap = 120, ordered=TRUE, low.color='#B18839', high.color='#590048') + 
  labs(title = title, caption = caption, y="") + 
  guides(fill = guide_legend(title = NULL))
ggsave("figures/q34_faceted_politics.png", width = 20, height = 25, units = "cm")


## Figure 8. Conceptual awareness of climate justice ----------------------------------------

# Q45 - simple bar chart

# TODO: facet 45/46/48 by age

title <- "Have you ever heard of the phrase “Climate Justice”?"
caption <- "Jeremy H. Kidwell and Charles Ogunbode,\nGraphic is CC-by-SA 4.0"
q45_data <- qualtrics_process_single_multiple_choice(climate_experience_data_named$Q45)
q45_plot <- plot_horizontal_bar(q45_data) + labs(title = title, caption = NULL, x = "", y = "")
q45_plot
ggsave("figures/figure8.png", width = 20, height = 5, units = "cm")


## Figure 9. Agreement with climate (in)justice statements  ----------------------------------------

# TODO: Note to self: fix clipping on RH side

# Q48
title <- "To what extent do you agree or disagree \nwith the following statements"
caption <- "Jeremy H. Kidwell and Charles Ogunbode,\nGraphic is CC-by-SA 4.0"
q48_data <- select(climate_experience_data, Q48_poor_suffer:Q48_colonialism)
names(q48_data) <- c("People living in poverty suffer worse effects from climate change", "Around the world, people who are least responsible suffer the worst negative impacts from climate change", "Climate change affects women around the world worse than men", "Climate change will increase existing inequalities", "The effects of climate change are worse for people of colour", "Solving climate change requires redistribution of resources from the wealthy to those who have less", "People from communities most affected by climate change should have more of a say in decisions about solutions to climate change than they currently do", "Climate change is driven and exacerbated by exploitative systems like colonialism and capitalism")
q48_levels <- c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")
q48_likert_table <- q48_data %>% 
  mutate(across(everything(), 
                factor, ordered = TRUE, levels = 1:5, labels=q48_levels)) %>% 
  as.data.frame %>% 
  likert
plot(q48_likert_table, wrap=40, text.size=3, ordered=TRUE, low.color='#B18839', high.color='#590048') + 
  labs(title = title, caption = NULL, y="") + 
  guides(fill = guide_legend(title = NULL)) + 
  theme_ipsum() +
  theme(axis.text.y = element_text(size = 9)) +
  scale_y_continuous(labels = abs, limits = c(-105, 115))

# save to png file for reports
ggsave("figures/figure9.png", width = 20, height = 18, units = "cm")

# TODO: facet by ethnicity (in 3 bins), age, gender, political orientation bins


## Figure 10. Participation in sustainability activities  ----------------------------------------

# Q40
title <- "Have you engaged in the following \nactivities in the past year?"
q40_data <- select(climate_experience_data, Q39_protest:Q39_active_member)
names(q40_data) <- c("Attended a climate protest", "Donated money to an environmental organisation tackling climate change", "Changed your lifestyle to reduce impact on the environment or climate", "Volunteered in an organisation tackling climate issues", "Signed a petition or contacted a politician regarding climate change", "Attended a public lecture, webinar or workshop about climate change", "Participation as an active member of an environmental organisation")
q40_levels <- c("No", "Yes")
q40_likert_table <- q40_data %>% 
  mutate(across(everything(),
                factor, ordered = TRUE, levels = 0:1, labels=q40_levels)) %>% 
  as.data.frame %>% 
  likert
plot(q40_likert_table, wrap=25, text.size=3, ordered=TRUE, low.color='#B18839', high.color='#590048') + 
  labs(title = title, caption = NULL, y="") + 
  guides(fill = guide_legend(title = NULL)) + 
  theme_ipsum() +
  theme(plot.title = element_text(lineheight = 0.9, size =12, hjust = 0))

# save to png file for reports
ggsave("figures/figure10.png", width = 20, height = 20, units = "cm")


## Figure 11. Individual sustainability behaviours ----------------------------------------

# Q41 - likert style centered stacked bar viz:
title <- "How often do you do the following?"
caption <- "Jeremy H. Kidwell and Charles Ogunbode,\nGraphic is CC-by-SA 4.0"
q41_data <- select(climate_experience_data, Q41_avoid_car:Q41_discuss)
names(q41_data) <- c("Cycle, walk or use public transport instead of using your car", "Have meat-free meals", "Try to influence family and friends to act in a pro-environmental way", "Shop in second-hand or ‘antique’ shops instead of buying new things", "Choose not to fly", "Save energy at home", "Try to avoid food-waste at home", "Discuss climate change with family and friends")
q41_levels <- c("Never", "Rarely", "Sometimes", "Often", "Almost Always")
q41_likert_table <- q41_data %>% 
  mutate(across(everything(),
                factor, ordered = TRUE, levels = 1:5, labels=q41_levels)) %>% 
  as.data.frame %>% 
  likert
plot(q41_likert_table, wrap=25, text.size=3, ordered=TRUE, low.color='#B18839', high.color='#590048') + 
  labs(title = title, caption = NULL, y="") + guides(fill = guide_legend(title = NULL)) + 
  theme_ipsum() +
  theme(plot.title = element_text(lineheight = 0.9, size =12, hjust = 0))
# save to png file for reports
ggsave("figures/figure11.png", width = 20, height = 20, units = "cm")

# Q41 - stacked percent bar viz:
title <- "To what extent do you agree or disagree \nwith the following statements"
q41_data <- select(climate_experience_data, Q41_avoid_car:Q41_discuss)
q41_levels <- c("yes, happened in the UK to me", "yes, happened outside the UK to me", "yes, happened in the UK to someone I know", "yes, happened outside the UK to someone I know", "no, has neither happened to me nor someone I know")  
q41_data <- q41_data %>%
  # we need to get the data including facet info in long format, so we use pivot_longer()
  pivot_longer(cols = everything()) %>%
  # need to split multiple answers into separate rows for summarising
  separate_rows(value, sep = ",") %>%
  mutate(across(value, factor, ordered = TRUE, levels = 1:5, labels=q41_levels)) %>%
  group_by(name) %>%
  ## count totals
  count(value, name)

# names_q41_data <- c("Heatwave [discomfort/being unable to sleep]", "Heatwave [experiencing disruption to travel or working]", "Heatwave [Health significantly affected]", "Flood damage to your home [not including water leaking through roof or burst pipes]", "Flooding in your local area [e.g., experiencing disruption to travel]", "Relocation due to flood risk or erosion", "Extreme snow [damage to personal property]", "Extreme snow [experiencing disruption to travel or working]", "Water restrictions/shortages due to low rainfall", "Restrictions to food supplies due to extreme weather", "Wildfires during drought periods [disruption to travel, loss of natural habitat]")
# q41_data %>% 
#   mutate(across(everything(), factor, ordered = TRUE, levels = 1:5, labels=q41_levels)) %>% 

ggplot(q41_data, aes(fill=value, y=n, x=name)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette = "Dark2", type = "qual") +
  labs(title = title, x = "", y="") + 
  guides(fill = guide_legend(title = NULL)) +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size = 9), axis.text.y = element_text(size = 9))

# save to png file for reports
ggsave("figures/q41_weather_groupedbar.png", width = 20, height = 18, units = "cm")

# Grouped likert plot using income_bin data
q41_data <- select(climate_experience_data, Q41_avoid_car:Q41_discuss, income_bin)
names(q41_data) <- c("Cycle, walk or use public transport instead of using your car", "Have meat-free meals", "Try to influence family and friends to act in a pro-environmental way", "Shop in second-hand or ‘antique’ shops instead of buying new things", "Choose not to fly", "Save energy at home", "Try to avoid food-waste at home", "Discuss climate change with family and friends", "income_bin")
q41_levels <- c("Never", "Rarely", "Sometimes", "Often", "Almost Always")
q41_likert_table <- q41_data %>% 
  mutate(across(everything(), 
                factor, ordered = TRUE, levels = 1:5, labels=q41_levels)) %>% 
  as.data.frame %>% 
  likert(grouping=q41_data$income_bin)
plot(q41_likert_table, wrap = 120, ordered=TRUE, low.color='#B18839', high.color='#590048') + 
  labs(title = title, caption = caption, y="") + 
  guides(fill = guide_legend(title = NULL))
ggsave("figures/q41_faceted_income.png", width = 20, height = 25, units = "cm")

# Grouped likert plot using political_lr data
title <- "How often do you do the following?"
q41_data <- select(climate_experience_data, Q41_avoid_car:Q41_discuss, Q53_bin)
q41_levels <- c("Never", "Rarely", "Sometimes", "Often", "Almost Always")
names(q41_data) <- c("Cycle, walk or use public transport instead of using your car", "Have meat-free meals", "Try to influence family and friends to act in a pro-environmental way", "Shop in second-hand or ‘antique’ shops instead of buying new things", "Choose not to fly", "Save energy at home", "Try to avoid food-waste at home", "Discuss climate change with family and friends", "Q53_bin")
q41_data$Q53_bin <- fct_recode(q41_data$Q53_bin, "Right" = "high", "Centre" = "medium", "Left" = "low")
q41_likert_table <- q41_data %>% 
  mutate(across(everything(), 
                factor, ordered = TRUE, levels = 1:5, labels=q41_levels)) %>% 
  as.data.frame %>% 
  likert(grouping=q41_data$Q53_bin)
plot(q41_likert_table, wrap = 120, ordered=TRUE, low.color='#B18839', high.color='#590048') + 
  labs(title = title, caption = caption, y="") + 
  guides(fill = guide_legend(title = NULL))
ggsave("figures/q41_faceted_politics.png", width = 20, height = 25, units = "cm")


## Figure 12. Barriers to action on climate change  ----------------------------------------

# TODO - change to un-stacked horizontal bar chart, centre line to be inbetween not at all and a little, with grey/purple bars to the right stacked together

# Q42
title <- "To what degree would you say each of the following stops \nyou or acts as a barrier to you taking more \naction on climate change?"
caption <- "Jeremy H. Kidwell and Charles Ogunbode,\nGraphic is CC-by-SA 4.0"
q42_data <- select(climate_experience_data, Q42_lacking_knowledge:Q42_other)
names(q42_data) <- c("Feeling that you have  insufficient awareness or knowledge of climate change", "Feeling uncertain or skeptical about climate change", "Lacking trust in information sources", "Difficulty or inconvenience of climate actions or climate-friendly lifestyle changes", "Feeling that individual action to address climate change is ineffective", "Feeling is too late to slow or reverse the negative effects of climate change", "Other things taking up your time and energy", "Other barriers not included in this list")
q42_levels <- c("Not at all", "A little", "A great deal")
q42_likert_table <- q42_data %>% 
  mutate(across(everything(), 
                factor, ordered = TRUE, levels = 1:3, labels=q42_levels)) %>% 
  as.data.frame %>% 
  likert
plot(q42_likert_table, center=1.5, centered=TRUE, wrap=45, text.size=3, ordered=TRUE, low.color='#B18839', high.color='#590048') + 
  labs(title = title, y="") + 
  guides(fill = guide_legend(title = NULL)) + 
  theme_ipsum() +
  theme(plot.title = element_text(lineheight = 0.9, size =12, hjust = 0)) +
  scale_y_continuous(labels = abs, limits = c(-110, 115))

# save to png file for reports
ggsave("figures/figure12.png", width = 20, height = 15, units = "cm")