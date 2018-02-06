# first things first (i eat your brains)
#   call permian data
# then (i start rocking gold teeth and fangs)

library(stringr)
library(ggplot2)
library(scales)
theme_set(theme_bw())  # graph setting
# color blind palette
cb <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
        "#0072B2", "#D55E00", "#CC79A7", "#999999")

# macrostrat strat data from the orodovician
strat <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Permian&response=long&format=csv', 
                  stringsAsFactors = FALSE)

# order by unit start age
strat <- strat[order(strat$b_age), ]
# factor ordered by time
strat$unit_id <- factor(strat$unit_id, levels = unique(strat$unit_id))  

# plot
sg <- ggplot(strat, aes(x = b_age, y = unit_id)) 
sg <- sg + geom_segment(mapping = aes(xend = t_age, yend = unit_id))
sg <- sg + labs(x = 'Time (Mya)', y = 'macrostrat unit')
sg <- sg + theme(axis.text.y = element_blank(),
                 axis.ticks.y = element_blank())
ggsave(plot = sg, filename = 'units_basic.png', height = 5.5, width = 8)

# zoom in on Permian
sg <- sg + coord_cartesian(xlim = c(298.9, 251.9))
sg <- sg + geom_vline(xintercept = c(298.9, 251.9), colour = 'blue')
ggsave(plot = sg, filename = 'units_zoom.png', height = 5.5, width = 8)

# color units based on fossil/no fossil
strat$fossil <- ifelse(strat$pbdb_collections != 0, 'fossils', 'no fossils')

sg <- ggplot(strat, aes(x = b_age, y = unit_id, colour = fossil)) 
sg <- sg + geom_segment(mapping = aes(xend = t_age, yend = unit_id))
sg <- sg + labs(x = 'Time (Mya)', y = 'macrostrat unit')
sg <- sg + scale_colour_manual(values = cb, name = 'fossil bearing?')
sg <- sg + theme(axis.text.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 legend.position = 'bottom')
sg <- sg + coord_cartesian(xlim = c(298.9, 251.9))
sg <- sg + geom_vline(xintercept = c(298.9, 251.9), colour = 'blue')
ggsave(plot = sg, filename = 'units_color.png', height = 5.5, width = 8)


# compare duration between fossil bearing and non-fossil bearing
strat$duration <- abs(strat$b_age - strat$t_age)
strat$log_duration <- log(strat$duration)

dg <- ggplot(strat, aes(x = log_duration, fill = fossil))
dg <- dg + geom_histogram(alpha = 0.5, position = 'identity')
dg <- dg + scale_fill_manual(values = cb, name = 'fossil bearing?')
dg <- dg + labs(x = 'geologic unit duration', y = 'frequency')
dg <- dg + theme(legend.position = 'bottom')
ggsave(plot = dg, filename = 'units_duration.png', height = 5.5, width = 6)
