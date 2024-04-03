rule targets:
    input:
        "figures/Regresion_poly.png",
        "figures/Regresion_log.png",
        "figures/Regresion_log_outliers.png",
        "Length_Weight.html"

rule length_weight:
    input:
        r_script = "Length_Weight.R"
    output:
        "figures/Regresion_poly.png",
        "figures/Regresion_log.png",
        "figures/Regresion_log_outliers.png"
    conda:
        "environment.yaml"
    shell:
        """
        Rscript {input.r_script}
        """

rule render_length_weight:
    input:
        rmd = "Length_Weight.Rmd",
        png1 = "figures/Regresion_poly.png",
        png2 = "figures/Regresion_log.png",
        png3 = "figures/Regresion_log_outliers.png"
    output:
        "Length_Weight.html"
    conda:
        "environment.yaml"
    shell:
        """
        R -e "library(rmarkdown); render('{input.rmd}')"
        """      