# Spelling errors over time

This project examines trends in spelling errors over time and the potential predictors of spelling errors. The errors were examined in abstracts which were extracted from [PubMed](https://pubmed.ncbi.nlm.nih.gov/). The list of spelling errors was sourced from _Wikipedia_. 

The files are ordered in their execution order, which is:
* 0_ read the error data
* 1_ search for errors on _PubMed_
* 2_ concatenate the errors
* 3_ refine the errors to exclude acronyms and names
* 4_ randomly select control abstracts without errors
* 5_ put cases and controls together
* 6_ use _OpenAlex_ to get the author's country and publisher
* 7_ create the analysis-ready data
* 8_ run the statistical models; create the trend data
* 9_ present the trend results
* 10_ present the results of the regression models
* 99_ Miscellaneous files and functions

Subfolders:
* `figures` for figures
* `data` for the analysis data at the individual-level (abstract), mostly in _R_ format with main data also in _Excel_ format
* `checks` for results of the manual checks
* `reports` for the reports in _Word_ format


<details><summary>#### R version and packages</summary>
<p>

```
R version 4.4.1 (2024-06-14 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 22631)

Matrix products: default

locale:
[1] LC_COLLATE=English_Australia.utf8  LC_CTYPE=English_Australia.utf8   
[3] LC_MONETARY=English_Australia.utf8 LC_NUMERIC=C                      
[5] LC_TIME=English_Australia.utf8    

time zone: Australia/Brisbane
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] nimble_1.2.1      gridExtra_2.3     ggplot2_3.5.1     ggtext_0.1.2     
 [5] countrycode_1.6.0 stringr_1.5.1     tidyr_1.3.1       flextable_0.9.6  
 [9] janitor_2.2.0     dplyr_1.1.4      

loaded via a namespace (and not attached):
 [1] gtable_0.3.5            xfun_0.47               lattice_0.22-6         
 [4] numDeriv_2016.8-1.1     vctrs_0.6.5             tools_4.4.1            
 [7] generics_0.1.3          parallel_4.4.1          curl_5.2.3             
[10] tibble_3.2.1            fansi_1.0.6             pkgconfig_2.0.3        
[13] data.table_1.15.4       uuid_1.2-1              lifecycle_1.0.4        
[16] compiler_4.4.1          textshaping_0.4.0       munsell_0.5.1          
[19] snakecase_0.11.1        httpuv_1.6.15           fontquiver_0.2.1       
[22] fontLiberation_0.1.0    htmltools_0.5.8.1       yaml_2.3.10            
[25] pracma_2.4.4            later_1.3.2             pillar_1.9.0           
[28] crayon_1.5.3            gfonts_0.2.0            openssl_2.2.1          
[31] rsconnect_1.3.1         mime_0.12               fontBitstreamVera_0.1.1
[34] tidyselect_1.2.1        zip_2.3.1               digest_0.6.36          
[37] stringi_1.8.4           purrr_1.0.2             fastmap_1.2.0          
[40] grid_4.4.1              colorspace_2.1-1        cli_3.6.3              
[43] magrittr_2.0.3          crul_1.5.0              utf8_1.2.4             
[46] withr_3.0.1             gdtools_0.3.7           scales_1.3.0           
[49] promises_1.3.0          lubridate_1.9.3         timechange_0.3.0       
[52] rmarkdown_2.28          officer_0.6.6           igraph_2.0.3           
[55] askpass_1.2.0           ragg_1.3.2              coda_0.19-4.1          
[58] shiny_1.9.1             evaluate_0.24.0         knitr_1.48             
[61] rlang_1.1.4             gridtext_0.1.5          Rcpp_1.0.13            
[64] xtable_1.8-4            glue_1.7.0              httpcode_0.3.0         
[67] xml2_1.3.6              rstudioapi_0.16.0       jsonlite_1.8.8         
[70] R6_2.6.1                systemfonts_1.1.0       
```

</p>
</details>
