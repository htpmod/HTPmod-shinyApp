library(shiny)
library(shinyjs)
library(parallel)

workers <- max(2, trunc(sqrt(detectCores()))) 
## enableBookmarking(store = "url")

APP_NAME <- "HTPmod"
APP_BASE <- "http://www.epiplant.hu-berlin.de/shiny/app/"
APP_URL <- paste0(APP_BASE, APP_NAME, "/")
APPs <-
    list(
        module1 = list(
            name = "Growth Modeling",
            short = "growMod", 
            link = paste0(APP_BASE, "growmod/"),
            icon = "line-chart",
            image = "module1.png",
            ref="The Plant Cell. 2016 Dec; doi: 10.1105/tpc.114.129601",
            href="https://doi.org/10.1105/tpc.114.129601",
            desc = "Modeling of plant growth based on time-series data."
        ),
        module2 = list(
            name = "Prediction Models",
            short = "predMod", 
            link = paste0(APP_BASE, "predmod/"),
            icon = "puzzle-piece",
            image = "module2.png",
            ref="bioRxiv. 2016 March; doi: 10.1101/046656",
            href="https://doi.org/10.1101/046656",
            desc = "Predicting important traits based on high-dimensional data."
        ),
        module3 = list(
            name = "HT Data Visualization",
            short = "htpdVis", 
            link = paste0(APP_BASE, "htpdvis/"),
            icon = "qrcode",
            image = "module3.png",
            ref="The Plant Cell. 2016 Dec; doi: 10.1105/tpc.114.129601",
            href="https://doi.org/10.1105/tpc.114.129601",
            desc = "Visualization of high-throughput data."
        )
    )

HTML_FOOTER <-
    "&copy; 2017. Author: Dijun Chen (chendijun2012 at gmail.com). All right reserved.
This web application was built with the <a href=\"http://shiny.rstudio.com/\" target=\"_blank\"><i class=\"fa fa-external-link\" aria-hidden=\"true\"></i> Shiny framework</a>. "

my.legend <- function(..., inset=0.01, pch=19, box.col=NA, title.col='black', bg="#EFEFEF"){
    legend(..., inset=inset, pch=pch, box.col=box.col, bg=bg, title.col=title.col)
}

## is.na + is.null + length==0 + ""
is.nothing <- function(x, false.triggers = FALSE) {
    if (is.function(x))
        return(FALSE)
    return(is.null(x) ||
               length(x) == 0 ||
               all(is.na(x)) ||
               all(x == "") || (false.triggers && all(!x)))
}

msgPlot <- function(msg = "Plot shown here", add = F) {
    if (!add) {
        plot(1, type="n", xaxt="n", yaxt="n", 
             frame=F, xlab="", ylab="")
    }
    text(1, 1, msg, cex=2, col=2)
}

getTmpString <- function() {
    a <- do.call(paste0, replicate(5, sample(c(LETTERS, letters), 1, TRUE), FALSE))
    paste0(a, sprintf("%04d", sample(9999, 1, TRUE)), sample(LETTERS, 1, TRUE))
}


reference <- list(
    "Chen_2014" = c("https://doi.org/10.1105/tpc.114.129601", "Chen et al., 2014 the Plant Cell."),
    "Chen_2018" = c("https://doi.org/10.1093/gigascience/giy001", "Chen et al., 2018 GigaScience."),
    "Chen_unpublished" = c("#", "Chen et al., unpublished results."),
    "Fahlgren_2015" = c("https://doi.org/10.1016/j.molp.2015.06.005", "Fahlgren et al., 2015 Molecular Plant."),
    "Jiao_2010" = c("https://doi.org/10.1038/msb.2010.76", "Jiao et al., 2010 Molecular Systems Biology."),
    "Smaczniak_2016" = c("https://doi.org/10.1105/tpc.17.00145", "Smaczniak et al., 2017 the Plant Cell."),
    "Song_2016" = c("https://doi.org/10.1126/science.aag1550", "Song et al., 2016 Science."),
    "Wang_2015" = c("https://doi.org/10.1101/gr.170332.113", "Wang et al., Genome Research."), 
    "Zhan_2015" = c("https://doi.org/10.1105/tpc.114.135657", "Zhan et al., 2015 the Plant Cell."), 
    "Zhu_2018" = c("https://doi.org/10.1016/j.cell.2017.12.019", "Zhu et al., 2018 Cell.")
) 

growmod_example_data <- list(
    'HTP: Barley' = c(`Chen et al., 2014; Normal-growth plants` = "Chen_2014_control",
                      `Chen et al., 2014; Stressed-growth plants` = "Chen_2014_stressed"),
    'HTP: Setaria' = c(`Fahlgren et al., 2015; Group by genotype` = "Fahlgren_2015_genotype",
                       `Fahlgren et al., 2015; Group by treatment` = "Fahlgren_2015_treatment")
)

predmod_example_data <- list(
    'HTP: Image-derived features' = c(`Chen et al., 2014; Regression or Classification` = "Chen_2014_biomass_prediction",
                                           `Chen et al., 2018; Regression or Classification` = "Chen_2018_biomass_prediction"), 
    'HTS: RNA-seq + ChIP-seq' = c(`Chen et al., 2018; Classification` = "Chen_unpublished_TFBS",
                                  `Song et al., 2016; Regression` = "Song_2016_DE_regression"),
    'HTS: SELEX-seq' = c(`Smaczniak et al., 2017; Classification` = "Smaczniak_2017_SELEX")
)

htpdvis_example_data <- list(
    'HTP: Phenome' = c(`Chen et al., 2014; Barley HTP data` = "Chen_2014_htpvis",
                       `Chen et al., 2018; Consecutive HTP experiments` = "Chen_2018_htpvis"), 
    'HTP: Metabolome' = c(`Zhu et al., 2018; Tomato metabolomes` = "Zhu_2018_metabolites"), 
    'HTS: Transcriptome' = c(`Zhan et al., 2015; Maize RNA-seq` = "Zhan_2015_RNAseq", 
                             `Jiao et al., 2010; Cell-type specific transcriptome` = "Jiao_2010_TRAPseq"),
    'HTS: Epigenome' = c(`Wang et al., 2015; Chromatin states` = "Wang_2015_chromatin_state",
                         `Chen et al., 2018; TFBSs of floral regulators` = "Chen_unpublished_TFBS",
                         `Smaczniak et al., 2017; DNA binding specificity of MADS proteins` = "Smaczniak_2017_SELEX")
)

