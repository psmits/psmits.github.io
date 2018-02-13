library(stringr)
library(dplyr)
library(ggplot2)
library(scales)
theme_set(theme_bw())  # graph setting
# color blind palette
cb <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
        "#0072B2", "#D55E00", "#CC79A7", "#999999")
permian <- c(298.9, 251.9)

# macrostrat strat data from the permian
strat <- read.csv('https://macrostrat.org/api/v2/units?interval_name=Permian&response=long&format=csv', 
                  stringsAsFactors = FALSE)

# fossil information from macrostrat based on units
furl <- paste0('https://macrostrat.org/api/v2/fossils?unit_id=', 
               paste0(strat$unit_id, collapse = ','), 
               '&response=long&format=csv')
fossil <- read.csv(furl, stringsAsFactors = FALSE)

# pbdb collection match to macrostrat unit
cypher <- unique(cbind(fossil$cltn_id, fossil$unit_id))

# get fossil information from pbdb
furl <- paste0('https://paleobiodb.org/data1.2/occs/list.txt?coll_id=', 
               paste0(fossil$cltn_id, collapse = ','), 
               '&show=full')
taxon <- read.csv(furl, stringsAsFactors = FALSE)

# give taxon occurrences their macrostrat unit id
taxon$unit_id <- cypher[match(taxon$collection_no, cypher[, 1]), 2]

# unique genera per unit
byunit <- group_by(taxon, unit_id)
unitdiv <- summarise(byunit, count = length(unique(genus)))
fossil$unitdiv <- 0
fossil$unitdiv[match(unitdiv$unit_id, cypher[, 2])] <- unitdiv$count
fossil <- mutate(fossil, length = abs(t_age - b_age))

# basic plot of duration X diversity
fd <- ggplot(fossil, aes(x = length, y = unitdiv)) + geom_point(alpha = 0.5)
fd <- fd + geom_smooth(se = FALSE, alpha = 0.5)
fd <- fd + labs(x = 'duration (My)', y = 'unit diveristy')
ggsave(plot = fd, filename = 'unit_div_age.png', height = 5.5, width = 6)

# log transform x axis
fd <- fd + coord_trans(x = 'log') + labs(x = 'duration (My)')
ggsave(plot = fd, filename = 'unit_div_logage.png', height = 5.5, width = 6)


# how about areal extent and diveristy
fossil$col_area <- strat$col_area[match(fossil$unit_id, strat$unit_id)]
ad <- ggplot(fossil, aes(x = col_area, unitdiv)) + geom_point(alpha = 0.5)
ad <- ad + coord_trans(x = 'log')
ad <- ad + geom_smooth(se = FALSE, alpha = 0.5)
ad <- ad + labs(x = 'column area', y = 'unit diversity')
ggsave(plot = ad, filename = 'unit_div_logarea.png', height = 5.5, width = 6)


# lets do both those plots excluding units with 0 fossils
musthavefossil <- filter(fossil, unitdiv > 0)
nfd <- fd %+% musthavefossil
ggsave(plot = nfd, filename = 'unit_div_age_gr0.png', height = 5.5, width = 6)
nad <- ad %+% musthavefossil
ggsave(plot = nad, filename = 'unit_div_area_gr0.png', height = 5.5, width = 6)


# summarize more about fossils in permian units
# lets just get units that have age midpoints in the actual permian
fossil$m_age <- apply(cbind(fossil$t_age, fossil$b_age), 1, mean)
nfossil <- filter(fossil, m_age <= permian[1], m_age > permian[2])
# unit diversity through time, plotted at midpoints
td <- ggplot(nfossil, aes(x = m_age, y = unitdiv)) + geom_point(alpha = 0.5)
td <- td + geom_smooth(se = FALSE, alpha = 0.5)
td <- td + labs(x = 'Time (Mya)', y = 'unit diveristy')
td <- td + scale_x_reverse()
ggsave(plot = td, filename = 'unit_div_time.png', height = 5.5, width = 6)

musthavefossil2 <- filter(nfossil, unitdiv > 0)
ntd <- td %+% musthavefossil2
ggsave(plot = ntd, filename = 'unit_div_time_gr0.png', height = 5.5, width = 6)
