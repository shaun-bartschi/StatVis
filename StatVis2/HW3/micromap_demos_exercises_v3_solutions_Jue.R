#Linked Micromap Demos and Exercises - Solutions
#Program:  micromap_demos_exercises_v3_solutions_Jue.R
#Programmer:  Michael McManus

#Date:  2013-10-21
#Date Modified:  2014-04-04 using micromap version 1.8
#Updated: 2019-03-21 by Juergen Symanzik

setwd("C://JUE//Teaching//Stat5810_Sp2018_StatVisualizationII//Micromaps//Micromap_NWQMC_Workshop_students")
#The setwd statement above applies to my computer.

#Code and data installations
#In RStudio set a working directory pointing to the Micromap_NMC_Workshop folder containing
#the R code micromap_demos_exercises_v3_Jue.R,
#the workspaces of NLA_pH_ds2.Rdata, WSA9.Rdata, nass_cotton_1977.csv, and  
#RandomWatershed2_stats_smooth, and the power point nmc_20140502_session1_micromap.pptx

#I. Overview of Four Steps to Make a Linked Micromap

### I. A. Geoprocessing of Spatial Data
library(micromap)
sessionInfo()
getwd()

library(labeling)

data("USstates")
#Here is the spatial data
plot(USstates)
head(USstates@data)
dev.off()

### I. B. Structure:  Create a Map Table to Link to the Statistical Data
statePolys <- create_map_table(USstates, 'ST')
#The spatial data is now in a simpler form that the micromap package can use
head(statePolys)

data("edPov")
#Here is the statistical data.
head(edPov)
#The spatial and statistical data frames must have a common linking variable.

### I. C. Draft Micromap Plot
mmplot(stat.data=edPov,
       map.data=statePolys,
       map.link=c('StateAb','ID'),
       panel.types=c('dot_legend','labels', 'dot', 'dot','map'),
       panel.data=list(NA, 'state','pov','ed', NA),
       ord.by='pov',   
       grouping=5, median.row=TRUE)

### I. D. Basic Modification Exercises of Draft Micromap Plot

### I. D. a) Reorder the previous plot in decreasing order of poverty
mmplot(stat.data=edPov,
       map.data=statePolys,
       map.link=c('StateAb','ID'),
       panel.types=c('dot_legend','labels', 'dot', 'dot','map'),
       panel.data=list(NA, 'state','pov','ed', NA),
       ord.by='pov',   
       rev.ord=TRUE,
       grouping=5, median.row=TRUE)

### I. D. b) Change the order of the education and poverty panels and sort by decreasing education
mmplot(stat.data=edPov,
       map.data=statePolys,
       map.link=c('StateAb','ID'),
       panel.types=c('dot_legend','labels', 'dot', 'dot','map'),
       panel.data=list(NA, 'state','ed','pov', NA),
       ord.by='ed',   
       rev.ord=TRUE,
       grouping=5, median.row=TRUE)

### I. D. c) Place the maps on the left side
mmplot(stat.data=edPov,
       map.data=statePolys,
       map.link=c('StateAb','ID'),
       panel.types=c('map','dot_legend','labels', 'dot', 'dot'),
       panel.data=list(NA, NA, 'state','ed','pov'),
       ord.by='ed',   
       rev.ord=TRUE,
       grouping=5, median.row=TRUE)

### I. D. d) Try some different groupings, such as 6, 6, 6, 6, 3, 6, 6, 6, 6 (and no median.row)
mmplot(stat.data=edPov,
       map.data=statePolys,
       map.link=c('StateAb','ID'),
       panel.types=c('map','dot_legend','labels', 'dot', 'dot'),
       panel.data=list(NA, NA, 'state','ed','pov'),
       ord.by='ed',   
       rev.ord=TRUE,
       grouping=c(6,6,6,6,3,6,6,6,6), median.row=FALSE)

### I. D. e) Align the state names to the left, see panel.att page 5-6 User guide
mmplot(stat.data=edPov,
       map.data=statePolys,
       map.link=c('StateAb','ID'),
       panel.types=c('map','dot_legend','labels', 'dot', 'dot'),
       panel.data=list(NA, NA, 'state','ed','pov'),
       ord.by='ed',   
       rev.ord=TRUE,
       grouping=c(6,6,6,6,3,6,6,6,6), median.row=FALSE,
       panel.att=list(list(3,align='left')))

### I. D. f) Vertically align the state names to the center, see vertical.align page 17 User guide
mmplot(stat.data=edPov,
       map.data=statePolys,
       map.link=c('StateAb','ID'),
       panel.types=c('map','dot_legend','labels', 'dot', 'dot'),
       panel.data=list(NA, NA, 'state','ed','pov'),
       ord.by='ed',   
       rev.ord=TRUE,
       grouping=c(6,6,6,6,3,6,6,6,6), median.row=FALSE,
       vertical.align='center',
       panel.att=list(list(3,align='left')))

### I. D. g) Use a divergent 6-class BrBG color scheme from RColorBrewer (which is colorblind safe)
mmplot(stat.data=edPov,
       map.data=statePolys,
       map.link=c('StateAb','ID'),
       panel.types=c('map','dot_legend','labels', 'dot', 'dot'),
       panel.data=list(NA, NA, 'state','ed','pov'),
       ord.by='ed',   
       rev.ord=TRUE,
       grouping=c(6,6,6,6,3,6,6,6,6), median.row=FALSE,
       vertical.align='center',
       colors=brewer.pal(6, "BrBG"),
       panel.att=list(list(3,align='left')))


### I. E. Refine the Micromap Plot
mmplot(stat.data=edPov, map.data=statePolys,
        panel.types=c('dot_legend',  'labels', 'dot', 'dot', 'map'),
        panel.data=list(NA, 'state', 'pov', 'ed', NA),
        map.link=c('StateAb','ID'),
        ord.by='pov', 
        grouping=5, 
        median.row=TRUE, 
        
        plot.height=9, 
        
        colors=c('red','orange','green','blue','purple'),
        
        panel.att=list(list(1, point.type=20, point.border=TRUE, point.size=2),
                       
                       list(2, header='States', panel.width=.8, 
                            align='left', text.size=.9),
                       
                       list(3, header='Percent Living Below\nPoverty Level',
                            graph.bgcolor='lightgray', point.size=1.5,
                            xaxis.ticks=list(10,15,20), xaxis.labels=list(10,15,20),
                            xaxis.title='Percent'),
                       
                       list(4, header='Percent Adults With\n4+ Years of College',
                            graph.bgcolor='lightgray', point.size=1.5,
                            xaxis.ticks=list(20,30,40), xaxis.labels=list(20,30,40), 
                            xaxis.title='Percent'),
                       
                       list(5, header='Light Gray Means\nPreviously Displayed',
                            map.all=TRUE, fill.regions='aggregate',
                            active.border.color='black', active.border.size=1.5,
                            inactive.border.color=gray(.7), inactive.border.size=1, 
                            panel.width=.8)))

### I. F. Refine the Micromap Plot with adjusted ticmarks & ticmark labels
mmplot(stat.data=edPov, map.data=statePolys,
       panel.types=c('dot_legend',  'labels', 'dot', 'dot', 'map'),
       panel.data=list(NA, 'state', 'pov', 'ed', NA),
       map.link=c('StateAb','ID'),
       ord.by='pov', 
       grouping=5, 
       median.row=TRUE, 
       
       plot.height=9, 
       
       colors=c('red','orange','green','blue','purple'),
       
       panel.att=list(list(1, point.type=20, point.border=TRUE, point.size=2),
                      
                      list(2, header='States', panel.width=.8, 
                           align='left', text.size=.9),
                      
                      list(3, header='Percent Living Below\nPoverty Level',
                           graph.bgcolor='lightgray', point.size=1.5,
                           xaxis.ticks=as.list(extended(min(edPov$pov), max(edPov$pov), 3)), 
                           xaxis.labels=as.list(extended(min(edPov$pov), max(edPov$pov), 3)),
                           xaxis.title='Percent'),
                      
                      list(4, header='Percent Adults With\n4+ Years of College',
                           graph.bgcolor='lightgray', point.size=1.5,
                           xaxis.ticks=as.list(extended(min(edPov$ed), max(edPov$ed), 4)), 
                           xaxis.labels=as.list(extended(min(edPov$ed), max(edPov$ed), 4)), 
                           xaxis.title='Percent'),
                      
                      list(5, header='Light Gray Means\nPreviously Displayed',
                           map.all=TRUE, fill.regions='aggregate',
                           active.border.color='black', active.border.size=1.5,
                           inactive.border.color=gray(.7), inactive.border.size=1, 
                           panel.width=.8)))

#II. Data Structure and Micromap Plot Syntax

### II. A. Read in Spatial Data and Create a Map Table 
data("USstates")
class(USstates)
str(USstates, max.level=2) #shows complexity of SpatialPolygonDataFrame
summary(USstates)
head(USstates@data)
dim(USstates@data)
plot(USstates)
dev.off()

#Compare that to the Statespoly dataframe.
class(statePolys)
head(statePolys, 15) #recall polygon is a set of vertices, or nodes, connected by lines

#See Introduction Guide Section 3 "Preparing data of the for use with the library"
#p. 11 on examples for simplifying spatial polygons

### II. B. Read in Statistical Data
#The components of the statistical data include a linking variable, columns
#of statistical summaries, and labels associated with the names of the polygons
#or areal units.
data("edPov")
head(edPov)

### II. C. Draft Syntax
mmplot(stat.data=edPov,
       map.data=statePolys,
       panel.types=c('dot_legend','labels', 'dot', 'dot','map'),
       panel.data=list(NA, 'state','pov','ed', NA),
       ord.by='pov',   
       grouping=5, median.row=TRUE,
       map.link=c('StateAb','ID'))

#Note the sequential and parallel construction of the syntax for panel.types
#and panel.data list.  If a change is made to panel.type then a corresponding 
#change is needed for the panel.data list.  Panels for Linked micromap plots
#are laid out from left to right.
#How will the linked micromap plot made from the code below differ the one above?

mmplot(stat.data=edPov,
       map.data=statePolys,
       panel.types=c('map','dot_legend','labels', 'dot', 'dot'),
       panel.data=list(NA, NA, 'state','pov','ed'),
       ord.by='pov',   
       grouping=5, median.row=TRUE,
       map.link=c('StateAb','ID'))

### II. D. Syntax for Refining the Plot
mmplot(stat.data=edPov, map.data=statePolys,
        panel.types=c('dot_legend',  'labels', 'dot', 'dot', 'map'),
        panel.data=list(NA, 'state', 'pov', 'ed', NA),
        map.link=c('StateAb','ID'),
        ord.by='pov', 
        grouping=5, 
        median.row=TRUE, 
        
        plot.height=9, 
        
        colors=c('red','orange','green','blue','purple'),
        
        panel.att=list(list(1, point.type=15, point.border=TRUE),
                       
                       list(2, header='States', panel.width=.8, 
                            align='left', text.size=.9),
                       
                       list(3, header='Percent Living Below\nPoverty Level',
                            graph.bgcolor='lightgray', point.size=1.5,
                            xaxis.ticks=list(10,15,20), xaxis.labels=list(10,15,20),
                            xaxis.title='Percent'),
                       
                       list(4, header='Percent Adults With\n4+ Years of College',
                            graph.bgcolor='lightgray', point.size=1.5,
                            xaxis.ticks=list(20,30,40), xaxis.labels=list(20,30,40), 
                            xaxis.title='Percent'),
                       
                       list(5, header='Tan Means\nPreviously Displayed',
                            map.all=TRUE, fill.regions='aggregate',
                            active.border.color='black', active.border.size=1.5,
                            inactive.fill='tan',
                            inactive.border.color=gray(.7),inactive.border.size=.5, 
                            panel.width=.8)))

#Colors refers to the colors that will be used in the perceptual groups for the dot
#legends, symbols in the statistical panels, and polygons in the map panel.  Note 
#this is a five panel linked micromap plot so we have five lists, one for each panel,
#and we can adjust options within each list.  Compared to the first time you saw this plot,
#I adjusted the point.type and point.border=TRUE in list 1, the first panel.  Also, I adjusted
#the inactive.fill color,and inactive.border.size in list 5, the fifth panel.
#Active refers to the polygons being displayed in the current perceptual
#group or row.  Inactive refers to polygons that have already been displayed.

# III.  Advanced Exercises (30-45 minutes)
#   A. Making a Draft Dot Micromap
#   B. Dot with Confidence Limits Micromap, see dot_cl pages 20 & 31 User guide
#   C. 5-Number Summary & Dot with CL Micromap

#I showed you a linked micromap earlier on pH from the NLA.  Below are the two data
#sets you can use to make a four panel draft micromap plot of the estimated median pH.
#The four panels include a dot legend, names of ecoregions, estimated median pH, and maps.

load("NLA_pH.ds2.RData")#statistical data from National Lakes Assessment
load("WSA9.RData")#spatial data from National Lakes Assessment

NLA_pH.ds2
#Column "Estimate" contains the pH estimates
#Columns "LCB95Pct" and "UCB95Pct" are the lower and upper confidence bounds
#Last five columns, beginning with Min  are the 5-number summaries of pH by Ecoregions

head(WSA9@data, 9)

# Code for three exercises
####################################################################################
wsa.polys<-create_map_table(WSA9, 'WSA_9_NM')
head(wsa.polys)

###III A. Dot
mmplot(stat.data=NLA_pH.ds2,
       map.data=wsa.polys,
       panel.types=c('dot_legend', 'labels', 'dot', 'map'),
       panel.data=list(
         NA,
         'Ecoregions',
         'Estimate',
         NA),
       ord.by='Estimate', grouping=3,
       median.row=FALSE,
       map.link=c("Ecoregions", "ID")) 


###III B. Dot with Confidence Limits, see dot_cl pages 20 & 31 Introduction Guide
mmplot(stat.data=NLA_pH.ds2,
       map.data=wsa.polys,
       panel.types=c('dot_legend', 'labels', 'dot_cl', 'map'),
       panel.data=list(
         NA,
         'Ecoregions',
         list('Estimate', 'LCB95Pct', 'UCB95Pct'),
         NA),
       ord.by='Estimate', grouping=3,
       median.row=FALSE,
       map.link=c("Ecoregions", "ID"))

###III C.  Box Plot and Dot with Confidence Limits, see box_summary page 21 Introduction Guide
mmplot(stat.data=NLA_pH.ds2,
       map.data=wsa.polys,
       panel.types=c('dot_legend', 'labels', 'box_summary', 'dot_cl', 'map'),
       panel.data=list(
         NA,
         'Ecoregions',
         list('Min', 'Q1.25.', 'Median.50.', 'Q3.75.', 'Max'),
         list('Estimate', 'LCB95Pct', 'UCB95Pct'),
         NA),
       ord.by='Estimate', grouping=3,
       median.row=FALSE,
       map.link=c("Ecoregions", "ID"),
       
       plot.height=7, plot.width=7, #for poster use plot.height=6, plot.width=9,
       colors=brewer.pal(5, "Spectral"), #for journal use plot.height=9, plot.width=6.8
       
       panel.att=list(list(1, point.type=20, point.size=2,
                           graph.border.color='white',
                           xaxis.text.display=FALSE, xaxis.line.display=FALSE,
                           graph.grid.major=FALSE),
                      
                      list(2, header='NARS Reporting\n Regions', panel.width=1.25,
                           align='left', text.size=.8),
                      
                      list(3, header='Sampled Field pH\n from Lakes',
                           graph.bgcolor='lightgray',
                           xaxis.ticks=list(4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0),
                           xaxis.labels=list(4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0),
                           xaxis.title='pH', graph.bar.size = .4, panel.width=1),
                      
                      list(4, header='Estimated Median pH\n with 95% CL',
                           graph.bgcolor='lightgray',
                           xaxis.ticks=list(6.5, 7.0, 7.5, 8.0, 8.5, 9.0),
                           xaxis.labels=list(6.5, 7.0, 7.5, 8.0, 8.5, 9.0),
                           xaxis.title='pH',panel.width=1),              
                      
                      list(5, header='Light Gray Means\n Highlighted Above',
                           map.all=TRUE, fill.regions='aggregate',
                           active.border.color='black', active.border.size=1.5,
                           inactive.border.color=gray(.7), inactive.border.size=1,
                           panel.width=2)))

### IV. Demonstration of adding an overall statistic or criteria line
#Chunk 1. Linked micromap of conductivity descriptive statistics and estimates
#spatial and statistical data brought in together as shapefile

wv.ds1<-readOGR(".", "RandomWatershed2_stats_smooth")
plot(wv.ds1)
head(wv.ds1@data)
wv.data.ds1<-wv.ds1@data #Pull out statistical data from spatial polygon dataframe
wv.data.ds1
names(wv.data.ds1)

#The variable names beginning with an uppercase letter, Cond_med, Cond_LCB95.
#Cond_UCB95, Wvsci_med, etc., are the population estimates.  Variable names that
#start with a lowercase letter, cond_min, condq1, cond_med1, cond_q3, cond_max, etc.,
#are the descriptive statistics.

#shorten names of Random_Watersheds that combine 7 smaller HUC08s with larger 
# neighboring HUC08s in WV Sampling Frame
wv.data.ds1$alt.name <- wv.data.ds1$Random_Wat
levels(wv.data.ds1$alt.name) <- list(
  "S. Br. Potomac"="South Branch Potomac",  
  "N. Br. Potomac"="North Branch Potomac",
  "Cacapon/Shen."="Cacapon/Shenandoah Hardy", 
  "Potomac/Shen."="Potomac Direct Drains/Shenandoah Jefferson", "U. New/James"="Upper New/James",
  "Tygart Valley"="Tygart Valley", "West Fork"="West Fork",
  "Mononga./Dunk."="Monongahela/Dunkard",
  "Cheat/Youg."="Cheat/Youghiogheny",
  "U. Ohio N./S."="Upper Ohio North/Upper Ohio South",
  "M. Ohio N."="Middle Ohio North",
  "M. Ohio S."="Middle Ohio South",
  "Little Kanawha"="Little Kanawha", "Greenbrier"="Greenbrier",
  "L. New"="Lower New", "Gauley"="Gauley", "U. Kanawha"="Upper Kanawha",
  "Elk"="Elk", "L. Kanawha"="Lower Kanawha", "Coal"="Coal", 
  "U. Guyandotte"="Upper Guyandotte", "L. Guyandotte"="Lower Guyandotte",
  "Tug Fork"="Tug Fork", "Big Sandy/L. Ohio"="Big Sandy/Lower Ohio",
  "Twelvepole"="Twelvepole")

wv.map.table<-create_map_table(wv.ds1,'Random_Wat')#ID variable is Random_Wat
head(wv.map.table)


#Chunk 2. code below for publication plot of conductivity descriptive statistics
#and estimates together
###add.line=129 for estimated median conductvity
#Draft plot
mmplot(stat.data=wv.data.ds1,
       map.data=wv.map.table,
       panel.types=c('dot_legend', 'labels', 'box_summary', 'dot_cl', 'map'),
       panel.data=list(NA,
                       'alt.name',
                       list('cond_min', 'condq1', 'cond_med1', 'condq3', 'cond_max'),
                       list('Cond_med', 'Cond_LCB95', 'Cond_UCB95'),                       
                       NA),
       ord.by='Cond_med', rev.ord=TRUE,
       grouping=5, plot.pGrp.spacing=1,
       median.row=FALSE,
       map.link=c('Random_Wat', 'ID'))

#Refined Plot
mmplot(stat.data=wv.data.ds1,
       map.data=wv.map.table,
       panel.types=c('dot_legend', 'labels', 'box_summary', 'dot_cl', 'map'),
       panel.data=list(NA,
                       'alt.name',
                       list('cond_min', 'condq1', 'cond_med1', 'condq3', 'cond_max'),
                       list('Cond_med', 'Cond_LCB95', 'Cond_UCB95'),                       
                       NA),
       ord.by='Cond_med', rev.ord=TRUE,
       grouping=5, plot.pGrp.spacing=1,
       median.row=FALSE,
       map.link=c('Random_Wat', 'ID'),
       
       plot.height=9, plot.width=6.8, #for poster use plot.height=6, plot.width=9,
       colors=brewer.pal(5, "Spectral"), #for journal use plot.height=9, plot.width=6.8
              
       panel.att=list(list(1, panel.width=1, point.type=20, point.size=2,
                           point.border=TRUE), 
                      
                      list(2, header='WVDEP\n Subbasins', panel.width=1.25,#increased panel.width to 1.1 from .8
                           align='left', text.size=.8),
                      
                      list(3, header='Observed Conductivity \n 4th Order Streams or Less',
                           graph.bgcolor='lightgray',
                           xaxis.ticks=c(0, 1000, 2000, 3000, 4000, 5000),
                           xaxis.labels=c(0, 1, 2, 3, 4, 5),
                           xaxis.labels.size=1.25,
                           #xaxis.labels.angle=90,
                           xaxis.title='Conductivity (x1000)', xaxis.title.size=1.25, panel.width=2),
                      
                      list(4, header='Estimated Conductivity \n 4th Order Streams or Less',
                           graph.bgcolor='lightgray',
                           xaxis.ticks=list(0, 150, 300, 450, 600, 750),
                           xaxis.labels=list(0, 150, 300, 450, 600, 750),
                           xaxis.labels.size=1.25,
                           xaxis.title='Conductivity',
                           add.line=129, add.line.col="black", add.line.typ="dashed",
                           xaxis.title.size=1.25,panel.width=2),
                      
                      list(5, header='Light Gray Means\n Highlighted Above',
                           inactive.border.color=gray(.7), inactive.border.size=2,
                            panel.width=1.5)))

#V. Demonstration of map options
#see p. 21 of Introduction Guide
#source of data:  http://www.nass.usda.gov/research/gmctnapy.htm
#use statePolys map table previously created
cotton<-read.csv("nass_cotton_1997.csv")
names(cotton)
str(cotton)
head(cotton, 16)
#A.
mmplot(stat.data=cotton,
       map.data=statePolys,
       map.link=c('StateAb','ID'),
       panel.types=c('dot_legend','labels', 'dot', 'dot', 'dot', 'map'),
       panel.data=list(NA, 'state','Acres','Bales', 'Yield', NA),
       ord.by='Acres',
       grouping=4)

#Four refinements
#a. rev.ord so sorted from highest acreage to lowest
#b. change from default colors, see RColorBrewer package
#c. map panel option outer.hull=TRUE so outline of US border drawn
#d. B/c maps appear small, adjust panel.width of map panel

#B.
mmplot(stat.data=cotton,
       map.data=statePolys,
       map.link=c('StateAb','ID'),
       panel.types=c('dot_legend','labels', 'dot', 'dot', 'dot', 'map'),
       panel.data=list(NA, 'state','Acres','Bales', 'Yield', NA),
       ord.by='Acres',
       rev.ord=TRUE,
       grouping=4,
       
       colors=brewer.pal(4, "Set1"),
       plot.height=9,plot.width=6.8,
       panel.att=list(list(1, point.type=20, point.border=TRUE, point.size=1),
               
               list(2, header='States Producing \n Cotton', panel.width=1.2, 
                    align='left', text.size=.9),
               
               list(3, header='Acres',
                    graph.bgcolor='lightgray', point.size=1,
                    xaxis.ticks=list(0,1,2,3,4,5), xaxis.labels=list(0,1,2,3,4,5),
                    xaxis.title='Millions', panel.width=1.1),
               
               list(4, header='Bales',
                    graph.bgcolor='lightgray', point.size=1,
                    xaxis.ticks=list(0,1,2,3,4,5), xaxis.labels=list(0,1,2,3,4,5), 
                    xaxis.title='Millions', panel.width=1.1),
               
               list(5, header='Yield',
                    graph.bgcolor='lightgray', point.size=1,
                    xaxis.ticks=list(1.0,1.5,2.0,2.5), xaxis.labels=list(1.0,1.5,2.0,2.5), 
                    xaxis.title='Ratio Bales:Acres',panel.width=1.1),
               
               list(6, header='Light Gray Means \n Previously Displayed',
                    map.all=TRUE, fill.regions='aggregate',
                    active.border.color='black', active.border.size=1.5,
                    outer.hull=TRUE, outer.hull.color='brown', outer.hull.size=1,
                    inactive.border.color=gray(.7), inactive.border.size=2, 
                    panel.width=2))) 

#C. Exercise to Make the Map Panel be First
#First run the code below, then modify it so the map panel comes first
mmplot(stat.data=cotton,
       map.data=statePolys,
       map.link=c('StateAb','ID'),
       panel.types=c('map', 'dot_legend','labels', 'dot'),
       panel.data=list(NA, NA, 'state', 'Yield'),
       ord.by='Yield',
       rev.ord=TRUE,
       grouping=4,
       
       colors=brewer.pal(4, "Set1"),
       plot.height=9,plot.width=6.8,
       panel.att=list(list(2, point.type=20, point.border=TRUE, point.size=2,
                           panel.header.size=1.25),
                      
                      list(3, header='States Producing \n Cotton',  
                           align='left', panel.header.size=1.25),
                      
                      list(4, header='Yield',
                           graph.bgcolor='lightgray', point.size=1.5,
                           xaxis.ticks=list(1.0,1.5,2.0,2.5), xaxis.labels=list(1.0,1.5,2.0,2.5), 
                           xaxis.title='Ratio Bales:Acres', xaxis.title.size=1.25,
                           panel.header.size=1.25),
                      
                      list(1, header='Light Gray Means \n Previously Displayed',
                           map.all=TRUE, fill.regions='aggregate',
                           active.border.color='black', active.border.size=1.5,
                           outer.hull=TRUE, outer.hull.color='brown', outer.hull.size=1,
                           inactive.border.color=gray(.7), inactive.border.size=2, 
                           panel.width=2, panel.header.size=1.25))) 


#VI. Use of external shapefiles
library(sp)
library(rgeos)
library(rgdal)
library(maptools)

library(micromap)

### Read in the shapefile and simplify the polygons (run this part of the code only once!)

ChinaShapefile <- readOGR("China_Shapefiles", "export", verbose = TRUE)
ChinaShapefile1 <- readOGR("China_New_Shapefiles", "gadm36_CHN_1", verbose = TRUE)
plot(ChinaShapefile)
plot(ChinaShapefile1)
summary(ChinaShapefile)
summary(ChinaShapefile1)

gIsValid(ChinaShapefile1, byid = TRUE, reason = TRUE)
ChinaShapefileThin <- thinnedSpatialPoly(ChinaShapefile, 
                                         tolerance = 30000, # try 100, 300, 3000, 30000
                                         minarea = 10000000000,
                                         topologyPreserve = TRUE, 
                                         avoidGEOS = TRUE)
ChinaShapefileThin <- gBuffer(ChinaShapefileThin, width = 0, byid = TRUE)
gIsValid(ChinaShapefileThin, byid = TRUE, reason = TRUE)
plot(ChinaShapefileThin)
# ChinaShapefile2 <- spTransform( ChinaShapefile1, CRS( "+init=epsg:3347" ) ) 
# ChinaShapefileThin2 <- thinnedSpatialPoly(ChinaShapefile2, 
#                                          tolerance = 30000, # try 100, 300, 3000, 30000
#                                          minarea = 10000000000,
#                                          topologyPreserve = TRUE, 
#                                          avoidGEOS = TRUE)
# ChinaShapefileThin2 <- gBuffer(ChinaShapefileThin2, width = 0, byid = TRUE)
# gIsValid(ChinaShapefileThin2, byid = TRUE, reason = TRUE)
# plot(ChinaShapefileThin2)

### Change tolerance to 100, 3000, and 30000 and run the the entire code again, starting with readOGR.
### Which of these is the best choice for tolerance? You may have to enlarge the map.

### If you plan to use LM plots for a research project, you may also want to 
### enlarge small regions or shift far-away regions closer to the main region.
### Talk to me for further information.

### Read in the data

religion <- read.csv("Main Document-ReligionData.csv", header = TRUE)

ChinaPolys <- create_map_table(ChinaShapefileThin, "ename")

### Basic micromap plot

ChinaPlot <- mmplot(stat.data = religion,
                    map.data = ChinaPolys,
                    panel.types = c("labels", "dot", "map"),
                    panel.data = list("Province", "Christianity", NA),
                    ord.by = "Christianity",
                    grouping = 5,
                    median.row = TRUE, 
                    map.link = c("Province", "ID"))

### Advanced micromap plot

ChinaReligion <- mmplot(stat.data = religion, 
                        map.data = ChinaPolys,
                        panel.types = c("map", "dot_legend", "labels", 
                                        "dot", "dot", "dot", "dot"),
                        panel.data = list(NA, NA, "Province", 
                                          "Christianity", "Buddhism", "Daoism", "Islam"),
                        map.link = c("Province", "ID"),
                        ord.by = "Christianity",
                        rev.ord = TRUE,
                        
                        grouping = 5,
                        median.row = TRUE,
                        plot.height = 9,
                        plot.width = 9,
                        colors = c("red", "orange", "green", "blue", "purple"),
                        two.ended.maps = TRUE,
                        map.all = TRUE,
                        map.color2 = "lightgray",
                        
                        plot.header = "Religion in China",
                        plot.header.size = 2,
                        plot.header.color = "black",
                        main = "Test"
,                        
                        plot.panel.spacing = 0,
                        panel.att = list(
                          list(1, header = "Two-ended\nCumulative Maps",
                               inactive.border.color = gray(.7), 
                               inactive.border.size = 1,
                               panel.width = 1.2),
                          list(2, point.type = 20,
                               point.size = 1.4),
                          list(3, header = "Provinces", 
                               panel.width = .9,
                               align = "left", 
                               text.size = .8),
                          list(4, header = "Christianity",
                               header.color = "red",
                               graph.bgcolor = "lightgray", 
                               point.size = 1,
                               xaxis.ticks = seq(0, 3000, by = 1000), 
                               xaxis.labels = seq(0, 3, by = 1),
                               xaxis.title = "Number (Thousand)"
                          ),
                          list(5, header = "Buddhism",
                               graph.bgcolor = "lightgray", 
                               point.size = 1,
                               xaxis.ticks = seq(0, 3000, by = 1000), 
                               xaxis.labels = seq(0, 3, by = 1),
                               xaxis.title = "Number (Thousand)"
                          ),
                          list(6, header = "Daoism",
                               graph.bgcolor = "lightgray", 
                               point.size = 1,
                               xaxis.ticks = seq(0, 2000, by = 1000), 
                               xaxis.labels = seq(0, 2, by = 1),
                               xaxis.title = "Number (Thousand)"
                          ),
                          list(7, header = "Islam",
                               graph.bgcolor = "lightgray", 
                               point.size = 1,
                               xaxis.ticks = seq(0, 25000, by = 12500), 
                               xaxis.labels = seq(0, 25, by = 12.5),
                               xaxis.title = "Number (Thousand)"
                          )
                        )
)


### Final advanced micromap plot with reduced margins

ChinaReligion <- mmplot(stat.data = religion, 
                        map.data = ChinaPolys,
                        panel.types = c("map", "dot_legend", "labels", 
                                        "dot", "dot", "dot", "dot"),
                        panel.data = list(NA, NA, "Province", 
                                          "Christianity", "Buddhism", "Daoism", "Islam"),
                        map.link = c("Province", "ID"),
                        ord.by = "Christianity",
                        rev.ord = TRUE,
                        
                        grouping = 5,
                        median.row = TRUE,
                        plot.height = 9,
                        plot.width = 9,
                        colors = c("red", "orange", "green", "blue", "purple"),
                        two.ended.maps = TRUE,
                        map.all = TRUE,
                        map.color2 = "lightgray",
                        
                        plot.header = "Religion in China",
                        plot.header.size = 2,
                        plot.header.color = "black",
                        
                        plot.panel.spacing = 0,
                        panel.att = list(
                          list(1, header = "Two-ended\nCumulative Maps",
                               inactive.border.color = gray(.7), 
                               inactive.border.size = 1,
                               panel.width = 1.2),
                          list(2, point.type = 20,
                               point.size = 1.4),
                          list(3, header = "Provinces", 
                               panel.width = .9,
                               align = "left", 
                               text.size = .8),
                          list(4, header = "Christianity",
                               header.color = "red",
                               graph.bgcolor = "lightgray", 
                               point.size = 1,
                               right.margin = 0, 
                               left.margin = -0.5,
                               xaxis.ticks = seq(0, 3000, by = 1000), 
                               xaxis.labels = seq(0, 3, by = 1),
                               xaxis.title = "Number (Thousand)"
                          ),
                          list(5, header = "Buddhism",
                               graph.bgcolor = "lightgray", 
                               point.size = 1,
                               right.margin = 0, 
                               left.margin = -0.5,
                               xaxis.ticks = seq(0, 3000, by = 1000), 
                               xaxis.labels = seq(0, 3, by = 1),
                               xaxis.title = "Number (Thousand)"
                          ),
                          list(6, header = "Daoism",
                               graph.bgcolor = "lightgray", 
                               point.size = 1,
                               right.margin = 0, 
                               left.margin = -0.5,
                               xaxis.ticks = seq(0, 2000, by = 1000), 
                               xaxis.labels = seq(0, 2, by = 1),
                               xaxis.title = "Number (Thousand)"
                          ),
                          list(7, header = "Islam",
                               graph.bgcolor = "lightgray", 
                               point.size = 1,
                               right.margin = 0.25, 
                               left.margin = -0.5,
                               xaxis.ticks = seq(0, 25000, by = 12500), 
                               xaxis.labels = seq(0, 25, by = 12.5),
                               xaxis.title = "Number (Thousand)"
                          )
                        )
)
