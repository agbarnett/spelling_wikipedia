# 99_dag.R
# plot DAGs for errors -> citations/retractions
# see https://cran.r-project.org/web/packages/ggdag/vignettes/intro-to-dags.html
# December 2024
library(ggdag)
library(ggplot2)
theme_set(theme_dag())
TeachingDemos::char2seed('rovers')

# DAG
dag_e <- dagify(spelling ~ type, # expect fewer errors for editorials
                spelling ~ word_count, # longer abstracts will have more errors 
                spelling ~ english, # fewer errors for native speakers
                #spelling ~ time, # errors will change over time; assume this effect is fully via quality
                spelling ~ n_authors, # more authors means more checks
                spelling ~ quality, # researchers with no care for quality will make spelling errors
                spelling ~ software, # spelling improves with software
                #spelling ~ country, # English speakers will have fewer errors (happens via native English speaker)
                spelling ~ editorial, # Spelling depends on editorial controls
                english ~ country, # language skills depend on country
                #english ~ time, # more non-English speakers over time; assume fully explained by country
                publisher ~ country, # Some countries will more heavily use some publishers
                publisher ~ time, # Publishers have changed over time
                software ~ time, # more software over time
                oa ~ publisher, # open access varies by publisher
                oa ~ time, # open access has increased over time
                n_authors ~ time, # number of authors per paper is increasing over time
                editorial ~ publisher, # editorial controls depend on publisher
                word_count ~ type, # word count depends on article type
                n_authors ~ type, # number of authors depends on article type
                country ~ time, # definite changes in country patterns over time
                quality ~ time, # quality is declining over time
                # outcomes
                citations ~ quality, # citations depend on latent quality
                labels = c(
                  "citations" = "Citations",
                  "english" = "Native\nEnglish\nspeaker",
                  "country" = "Country",
                  "editorial" = "Editorial\neffort",
                  "quality" = "Research\nquality",
                  "n_authors" = "Number of\nauthors",
                  "spelling" = "Spelling\nerrors",
                  "software" = "Spell-check\nsoftware",
                  "publisher" = "Publisher",
                  "oa" = "Open\naccess",
                  "time" = "Time\ntrend",
                  "word_count" = "Word\ncount",
                  "type" = 'Article\ntype'
                ),
                latent = c("quality","editorial",'english','software'),
                exposure = "quality",
                outcome = "spelling"
)

# get data and plot to allow unmeasured variables in different shape
for_plot = tidy_dagitty(dag_e) %>%
  mutate(dependent = str_detect(name, pattern='spelling'),
         latent = str_detect(name, pattern='quality|editorial|english|software'))
# adjustments to coordinates to make DAG look better
for_plot$data = mutate(for_plot$data,
                       yend = ifelse(name=='country' & to == 'english', -0.1, yend),
                       yend = ifelse(name=='time' & to == 'quality', -1.2, yend),
                       yend = ifelse(name=='quality' & to == 'citations', -1.2, yend),
                  xend = ifelse(name=='quality' & to == 'citations', 4.7, xend),
                  x = ifelse(name=='citations' & is.na(to), 4.7, x),
                  y = ifelse(name=='citations' & is.na(to), -1.2, y),
                  y = ifelse(name=='english', -0.1, y),
                  y = ifelse(name=='quality', -1.2, y),
                  y = ifelse(name=='english' & is.na(to), -0.0, y))

# plot
dag_plot = ggplot(for_plot, aes(x = x, y = y, xend = xend, yend = yend, col = dependent, shape=latent)) +
  geom_dag_point(show.legend = FALSE) + # stops both legends
  geom_dag_edges() +
  geom_dag_text(col='grey44', aes(x = x, y = y, label= label), cex=2)+
  scale_shape_manual('Measured', values=c(1,0), labels=c('Yes','No'))+
  scale_color_manual(NULL, values=c('darkorchid2','darkseagreen4'))+
  theme_dag() 
dag_plot

# export
ggsave(filename = 'figures/dag.jpg', plot = dag_plot, width=7, height=6, units='in', dpi=400)

# more complex dag with institution and field of research
#dag_c = dagify(spelling ~ institute)
#for_plot = tidy_dagitty(dag_e, dag_c)

#
ggdag_collider(dag_e) # find colliders
control_for(dag_e, var = "spelling")
ggdag_adjust(dag_e, var = "spelling")
ggdag_adjustment_set(dag_e)
