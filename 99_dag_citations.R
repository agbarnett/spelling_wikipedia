# 99_dag_citations.R
# plot DAGs for errors -> citations/retractions
# see https://cran.r-project.org/web/packages/ggdag/vignettes/intro-to-dags.html
# December 2024
library(ggdag)
library(ggplot2)
theme_set(theme_dag())

# 'simpler' DAG
dag_e <- dagify(citations ~ type, # more citations for reviews
                citations ~ age, # more citations for older article age
                citations ~ country, # bias in citations by country
                citations ~ publisher, # bias in citations by publisher
                citations ~ quality, # more citations for better quality
                citations ~ time, # more citations over time
                citations ~ n_authors, # more citations for more authors
                #spelling ~ english, # fewer errors for native speakers, happens via country
                #spelling ~ time, # errors will change over time; assume this effect is fully via quality
                spelling ~ n_authors, # more authors means more checks
                spelling ~ quality, # researchers with no care for quality will make spelling errors
                spelling ~ country, # English speakers will have fewer errors
                spelling ~ editorial, # Spelling depends on editorial controls
                english ~ country, # language skills depend on country
                #english ~ time, # more non-English speakers over time; assume done fully by country
                publisher ~ country, # Some countries will more heavily use some publishers
                publisher ~ time, # Publishers have changed over time
                citations ~ oa, # more citations for Open Access papers
                oa ~ publisher, # open access varies by publisher
                oa ~ time, # open access has increased over time
                quality ~ time, # quality is declining over time
                n_authors ~ time, # number of authors per paper is increasing over time
                editorial ~ publisher, # editorial controls depend on publisher
                n_authors ~ type, # number of authors depends on article type
                country ~ time, # definite changes in country patterns over time
                labels = c(
                  "age" = "Article\nage",
                  "citations" = "Citations",
                  "english" = "Native English\nspeaker",
                  "country" = "Country",
                  "editorial" = "Editorial\neffort",
                  "quality" = "Research\nquality",
                  "n_authors" = "Number of\nauthors",
                  "spelling" = "Spelling\nerrors",
                  "publisher" = "Publisher",
                  "oa" = "Open\naccess",
                  "time" = "Time\ntrend",
                  "word_count" = "Word\ncount",
                  "type" = 'Article\ntype'
                ),
                latent = c("quality","editorial",'english'),
                exposure = "country",
                outcome = "spelling"
)

# get data and plot to allow unmeasured variables in different shape
for_plot = tidy_dagitty(dag_e) %>%
  mutate(dependent = str_detect(name, pattern='citations'),
         latent = str_detect(name, pattern='quality|editorial|english'))
dag_plot = ggplot(for_plot, aes(x = x, y = y, xend = xend, yend = yend, col = dependent, shape=latent)) +
  geom_dag_point(show.legend = FALSE) + # stops both legends
  geom_dag_edges() +
  geom_dag_text(col='grey44', aes(x = x, y = y, label= label), cex=2)+
  scale_shape_manual('Measured', values=c(1,0), labels=c('Yes','No'))+
  scale_color_manual(NULL, values=c('darkorchid2','darkseagreen4'))+
  theme_dag() 
dag_plot

# export
ggsave(filename = 'figures/dag_citations.jpg', plot = dag_plot, width=7, height=6, units='in', dpi=400)


#
ggdag_collider(dag_e) # find colliders
ggdag_adjust(dag_e, var = "citations")
control_for(dag_e, var = "citations")

