args <- commandArgs(trailingOnly=TRUE)

if (length(args) == 0) {
	stop("At least one argument must be supplied (plate id number).n", call.=FALSE)
} else {
	message("Running regression for: ", args[1], " directory")
}

library(qqplotr)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(reshape2)
library(nortest)
library(ggpubr)
library(grid)
library(pdftools)


ORDER = 1 # For linear model
CI = 0.95
THRESHOLD_MFI = 75
DATA_PT_SIZE = 10
FONT_SIZE = 22
EQN_SIZE = 10
datadump = "Z://data"
args = "20210319-RB-FY21w5p8"
######################## FROM GLOBAL.R USED IN REGRESSION APP ############################

## Calculate the % of 4C Reference MFI data from the uploaded stats
calculate_perct_4C_MFI <- function(df){
    
    calc_vect <- c() # Initialize % of 4C MFI data
    for(i in unique(df$Concentration)){
        for(row in c(1:nrow(df[df$Concentration == i,]))){
            ref_for_each_conc <- df$`MFI+`[df$Concentration == i & df$Condition == 0]
            MFI <- df$`MFI+`[df$Concentration == i][[row]]
            calc <- round((as.numeric(MFI)/as.numeric(ref_for_each_conc))*100,0)
            calc_vect <- append(calc_vect, calc)
        }
        
    }
    df <- bind_cols(df, "% 4C Reference MFI"=calc_vect)
    return(df)
}

## Step 3a
create_raw_reference_MFI_table_wide <- function(df){
    
    # Take only subset of raw stats table
    df_selected <- select(df, c(Condition, Concentration, `% 4C Reference MFI`))
    
    df_wide <- df_selected %>% pivot_wider(names_from = Concentration, values_from = `% 4C Reference MFI`)
    colnames(df_wide)[1] <- "Time"
    for(i in c(2:length(colnames(df_wide)))){
        colnames(df_wide)[i] <- paste0(colnames(df_wide)[i], " ng/test")
    }
    
    return(df_wide)
}

rounded_shelf_life <- function(shelf_life){
    ## Rounding rules:
    ## 1. Round down to nearest half integer
    ## 2. If a half or whole integer, still round down to next half integer
    ## 3. If 1.5yrs, don't round down
    
    # If shelf-life is greater than 5 years, round down to 5 years to avoid extrapolation
    if(shelf_life > 5){
        shelf_life <- 5 # max timepoint tested
    }
    # If shelf-life is equal to or below 1.5y, don't round down
    else if(shelf_life <= 1.5){
        shelf_life <- shelf_life
    }
    # If shelf-life is a whole # or half integer (ex. 4.0 or 3.5), round down to next nearest half integer
    else if(shelf_life %% 0.5 == 0){
        shelf_life <- floor((shelf_life - 0.1) / 0.5) * 0.5
    }
    # Otherwise, round down to nearest half integer
    else{
        shelf_life <- floor(shelf_life / 0.5) * 0.5
    }
    
    return(round(shelf_life,1))
}

melt_reference_mfi_table <- function(df_full=template_data){
    # Melt Columns by Time
    dataMelt <- melt(df_full, "Time", variable='Concentrations')
    dataMelt <- cbind(dataMelt, 'Labels'=paste0(parse_number(as.character(dataMelt$Concentrations)), ' ng/test'))
    
    return(na.omit(dataMelt))
}

find_confidence_bands <- function(df_melt, order, CI, threshold_y) {
    
    df_melt <- na.omit(df_melt)
    y <- na.omit(df_melt$value)
    x <- na.omit(df_melt$Time)
    n <- length(y) # Find length of y to use as sample size
    
    fit <- lm(y ~ poly(x,order,raw=TRUE),data=df_melt)
    
    summary_regression <- summary(fit)
    
    a <- as.numeric(format(round(summary_regression$coefficients[[1]],2))) # y-intercept
    b <- as.numeric(format(round(summary_regression$coefficients[[2]],2))) # 1st order coeff
    c <- ifelse(order > 1, as.numeric(format(round(summary_regression$coefficients[[3]],2))), 0) # 2nd order coeff
    d <- ifelse(order > 2, as.numeric(format(round(summary_regression$coefficients[[4]],2))), 0) # 3rd order coeff
    
    x_new <- seq(min(x), max(x), length.out = length(x))
    y_fit <- a + b*x_new + c*x_new^2 + d*x_new^3
    
    pre_predict <- predict(fit, data.frame(x_new))
    p1 <- ggplot(data=df_melt) +
        geom_point(data=df_melt, aes(x=x_new, y=y), col='blue')
    conf_df <- predict(fit, data.frame('x_new'=x_new), interval='confidence',level=CI, se.fit=TRUE)
    predict_df <- data.frame('x_new'=x_new, 'fit'=y_fit, 'lwr'=conf_df$fit[,2], 'upr'=conf_df$fit[,3], 'se_fit'=conf_df$se.fit)
    slope.upper <- predict_df$upr
    slope.lower <- predict_df$lwr
    
    fit_lower <- lm(slope.lower ~ poly(x,order, raw=TRUE))
    summary_regression_lower <- summary(fit_lower)
    
    f1 <- function(x) a + b*x + c*x^2 + d*x^3
    f2 <- function(x) threshold_y
    
    a_lower <- as.numeric(format(round(summary_regression_lower$coefficients[[1]],2))) # y-intercept
    b_lower <- as.numeric(format(round(summary_regression_lower$coefficients[[2]],2))) # 1st order coeff
    c_lower <- ifelse(order > 1, as.numeric(format(round(summary_regression_lower$coefficients[[3]],2))), 0) # 2nd order coeff
    d_lower <- ifelse(order > 2, as.numeric(format(round(summary_regression_lower$coefficients[[4]],2))), 0) # 3rd order coeff
    
    f1_lower <- function(x) a_lower + b_lower*x + c_lower*x^2 + d_lower*x^3
    f2_lower <- function(x) threshold_y
    
    bands <- data.frame(cbind(predict_df$x_new, predict_df$lwr, predict_df$fit, predict_df$upr))
    colnames(bands) <- c('X Values', 'Lower Confidence Band', 'Y Values', 'Upper Confidence Band')
    
    
    return(bands)
}

solve_for_lower_shelf_life <- function(df_melt, order, CI, threshold_y){
    df_melt <- na.omit(df_melt)
    
    x <- na.omit(df_melt$Time)
    y <- na.omit(df_melt$value)
    
    n <- length(y) # Find length of y to use as sample size
    
    fit <- lm(y ~ poly(x,order,raw=TRUE),data=df_melt)
    summary_regression <- summary(fit)
    a <- as.numeric(format(round(summary_regression$coefficients[[1]],2))) # y-intercept
    b <- as.numeric(format(round(summary_regression$coefficients[[2]],2))) # 1st order coeff
    c <- ifelse(order > 1, as.numeric(format(round(summary_regression$coefficients[[3]],2))), 0) # 2nd order coeff
    d <- ifelse(order > 2, as.numeric(format(round(summary_regression$coefficients[[4]],2))), 0) # 3rd order coeff
    
    x_new <- seq(min(x), max(x), length.out = length(x))
    y_fit <- a + b*x_new + c*x_new^2 + d*x_new^3
    
    # pre_predict <- predict(fit, data.frame(x_new))
    pre_predict <- predict(fit, data.frame('x_new'=x_new), interval='confidence',level=CI, se.fit=TRUE)
    pre_predict_df <- data.frame('x_new'=x_new, 'fit'=y_fit, 'lwr'=pre_predict$fit[,2], 'upr'=pre_predict$fit[,3])
    
    slope.upper <- pre_predict_df$upr
    slope.lower <- pre_predict_df$lwr
    
    fit_lower <- lm(slope.lower ~ poly(x,order, raw=TRUE))
    summary_regression_lower <- summary(fit_lower)
    
    f1 <- function(x) a + b*x + c*x^2 + d*x^3
    f2 <- function(x) threshold_y
    
    # shelf_life <- uniroot(function(x) f1(x)-f2(x),c(0,5), extendInt="yes")$root
    
    a_lower <- as.numeric(format(round(summary_regression_lower$coefficients[[1]],2))) # y-intercept
    b_lower <- as.numeric(format(round(summary_regression_lower$coefficients[[2]],2))) # 1st order coeff
    c_lower <- ifelse(order > 1, as.numeric(format(round(summary_regression_lower$coefficients[[3]],2))), 0) # 2nd order coeff
    d_lower <- ifelse(order > 2, as.numeric(format(round(summary_regression_lower$coefficients[[4]],2))), 0) # 3rd order coeff
    
    f1_lower <- function(x) a_lower + b_lower*x + c_lower*x^2 + d_lower*x^3
    f2_lower <- function(x) threshold_y
    
    # UPDATE: ADDING TRYCATCH STATEMENT TO CATCH getChannel() ERROR
    shelf_life_lower <- tryCatch(uniroot(function(x) f1_lower(x)-f2_lower(x),c(0,5), extendInt="yes")$root, error = function(c){
        cat("Never crosses MFI Threshold - no shelf-life can be found \n")
        shelf_life_lower = NULL
        return(shelf_life_lower)
    })
    return(shelf_life_lower)
}

polynomial_evaluation_of_linearity <- function(df_melt, order){
    fit <- lm(value ~ poly(Time,order, raw=TRUE), data=df_melt)
    summary_regression <- summary(fit)
    p_values <- summary_regression$coefficients[,4]
    
    a_pvalue <- p_values[1]
    b_pvalue <- p_values[2]
    c_pvalue <- p_values[3]
    d_pvalue <- p_values[4]
    
    pvalue_df <- data.frame('a_pvalue'=a_pvalue,
                            'b_pvalue'=b_pvalue,
                            'c_pvalue'=c_pvalue,
                            'd_pvalue'=d_pvalue)
    
    return(pvalue_df)
}

R_sq <- function(df_melt, order){
    summary_regression <- summary(lm(value ~ poly(Time,order, raw=TRUE), data=df_melt))
    r_sq <- format(round(summary_regression$r.squared,2)) # R^2 value
    adj_r_sq <- format(round(summary_regression$adj.r.squared,2)) # Adjusted R^2 value
    
    return(as.numeric(r_sq))
    
}

regression_plot_global <- function(text_size, data_point_size, eqn_size, df, confidence_bands, order, CI, eqn_location, eqn_location2){
    
    p <- ggplot(df, aes(x=Time, y=value, color=Concentrations)) + 
        geom_ribbon(data=df, 
                    aes(x=Time, y=value, 
                        ymin=confidence_bands[[2]], 
                        ymax=confidence_bands[[4]]), 
                    formula = y ~ poly(x,order, raw=TRUE), method="lm",col = "red", 
                    level=as.numeric(CI), alpha=0.2) +
        geom_line(data=confidence_bands, 
                  aes(x=`X Values`, y=`Y Values`), 
                  formula = y ~ poly(x,order, raw=TRUE), method="lm", col = "red") +
        geom_point(size=data_point_size) + 
        labs(x = "Time (years)",
             y = "% of 4C Reference MFI") +
        theme_minimal() +
        scale_color_brewer(palette = 'Dark2', na.translate = F,
                           labels = unique(df$Labels)) +
        theme(text=element_text(size = text_size),
              legend.position = "bottom")
    return(suppressWarnings(p))
}

residual_histogram <- function(df_melt, order, font_size){
    df_melt <- na.omit(df_melt)
    
    x <- na.omit(df_melt$Time)
    y <- na.omit(df_melt$value)
    n <- length(x)
    fit <- lm(y ~ poly(x,order, raw=TRUE), data=df_melt)
    fit_residuals <- resid(fit)
    
    p <- ggplot(df_melt,aes(x=fit_residuals, label=Time)) + 
        geom_histogram(binwidth=sd(fit_residuals), boundary=0, fill = '#eb6864') +
        labs(title = 'Histogram of Residuals',
             x = 'Residuals', 
             y = '# of Residuals') +
        theme(text=element_text(size = font_size))
    
    return(p)
}

find_residuals <- function(df_melt, order){
    df_melt <- na.omit(df_melt)
    
    x <- na.omit(df_melt$Time)
    y <- na.omit(df_melt$value)
    n <- length(x)
    fit <- lm(y ~ poly(x,order, raw=TRUE), data=df_melt)
    fit_residuals <- resid(fit)
    df <- df_melt %>% add_column("Residuals"=fit_residuals)
    
    return(df)
}

normal_probability_plot <- function(df_melt, order, residuals, font_size, data_point_size){
    
    # And adding line with proper properties
    p <-
        ggplot(residuals, mapping = aes(sample = Residuals)) +
        qqplotr::stat_qq_point(size = data_point_size,color = "#eb6864") +
        qqplotr::stat_qq_line(color="black") +
        # geom_abline(aes(slope=1, intercept=0), color="black") +
        # geom_qq(color = "#eb6864") +
        # geom_qq_line(colour = "black") +
        labs(title = 'Normal Probability Plot of Residuals',
             x = 'Theoretical Quantiles',
             y = 'Residuals') +
        theme(text=element_text(size = font_size))
    return(p)
}

residual_vs_fit_plot <- function(df_melt, order, font_size, data_point_size){
    df_melt <- na.omit(df_melt)
    
    x <- na.omit(df_melt$Time)
    y <- na.omit(df_melt$value)
    n <- length(x)
    fit <- lm(y ~ poly(x,order, raw=TRUE), data=df_melt)
    fitted_y <- data.frame("y-fit"=fitted(fit))
    fit_residuals <- resid(fit)
    
    p <- ggplot(df_melt,aes(x=fitted_y$y.fit, y=fit_residuals, label=Time, 
                            text = sprintf("Time: %s yrs<br>Concentration: %s<br>Actual %% of 4C Ref. MFI: %.0f<br>Predicted %% of 4C Ref. MFI: %.0f<br>Residuals: %.2f", 
                                           Time, Concentrations, value, fitted_y$y.fit, fit_residuals))) + 
        geom_point(size=data_point_size, color = '#eb6864') +
        geom_hline(aes(yintercept=0)) +
        ylim(0-max(abs(fit_residuals)), 0+max(abs(fit_residuals))) +
        labs(title = 'Residuals vs. Fit Plot',
             x = 'Predicted % of 4C MFI', 
             y = 'Residuals') +
        theme(text=element_text(size = font_size))
    # p <- ggplotly(p, tooltip=c("text"))
    return(p)
}

anderson_darling_normality_test <- function(residuals){
    ad <- nortest::ad.test(residuals$Residuals)
    p_value <- ad$p.value
    
    # null hypothesis is that data DOES follow normal distribution
    # can reject null hypothesis if p-value < 0.05 --> meaning we can say with sufficient evidence that data does NOT follow normal distribution
    return(p_value)
}

######################## FUNCTIONS SPECIFIC TO REGRESSION REPORT GENERATION ONLY ############################

## Step 1: Upload raw stats ##
read_stats <- function(stats_file, pop){
    stats <- read_csv(stats_file, col_types = cols())
    if (!"Concentration" %in% colnames(stats)) {
	    names(stats)[names(stats) == "ug.test"] <- "Concentration"
    }
    print(head(stats))
    stats <- stats[!is.na(stats$SI),] %>% arrange(Concentration)
    
    stats_pop <- stats[stats$pop == pop,]
    
    if(dim(stats_pop)[[1]] == 0){
        return(NULL)
    }
    else{
        stats_pop <- stats_pop %>% select(Stability.Time.point, Concentration, `%+`, `MFI+`, `MFI-`, `rSD-`)
        colnames(stats_pop)[1] <- c("Condition")
        
        return(stats_pop)
    }
    
    
}


get_stats_table_for_mfi_table <- function(stats_file, cell_pop){
    
    ## Step 1: Upload raw stats ##
    df <- read_stats(stats_file, cell_pop)
    if(!is.null(df)){
        ## Step 2: Calculate % of 4C Reference MFI ##
        df <- calculate_perct_4C_MFI(df)
        
        ## Step 3a: Transform % of 4C Reference MFI data to wide ##
        df <- create_raw_reference_MFI_table_wide(df)
        
        return(df)
    }
    else{
        return(NULL)
    }
    
}

create_shelf_life_summary_table <- function(lower_shelf_life, r_sq, p_value){
    
    rounded_lower_shelf_life <- rounded_shelf_life(lower_shelf_life)
    
    df <- tibble(
        "Raw Shelf-Life" = c(paste0(round(lower_shelf_life,1), " yrs (", round(lower_shelf_life*365,0), " days)")),
        "Rounded Shelf-Life" = c(paste0(rounded_lower_shelf_life, " yrs (", round(rounded_lower_shelf_life*365,0), " days)")),
        "R-squared" = round(r_sq, 2),
        "Model p-value"=format(round(p_value$b_pvalue, 3), nsmall = 3)
    )
    return(df)
}

perc_4C_mfi_table_png <- function(df){
    tab = ggpubr::ggtexttable(df, rows = NULL,
                              theme=ggpubr::ttheme("classic",
                                                   base_size=28,
                                                   padding = unit(c(16, 5), "mm"),
                                                   colnames.style = ggpubr::colnames_style(fill = "lightgray",
                                                                                           size = 28,
                                                                                           linecolor = "black")))
    tab = ggpubr::tab_add_title(tab,text = "% of 0 Reference MFI", face = "bold", size = 28, hjust = -2)
    
    
    for (r in 3:(NROW(df)+2)){ # add bold first column and gray bg
        tab = ggpubr::table_cell_font(tab, row=r, column=1, face="bold", size=28)
        tab = ggpubr::table_cell_bg(tab, row=r, column=1, fill="lightgray")
    }
    fp <- file.path(stats_file_path,"StatsTable.png")
    png(fp, width=1600, height=800, units="px")
    print(tab)
    dev.off()
}

shelf_life_estimates_table_png <- function(stats_file, shelf_life_df, p_value, shelf_life, r_sq){
    
    shelf_life_df_tab = ggpubr::ggtexttable(shelf_life_df, rows = NULL,
                                            theme=ggpubr::ttheme("classic",
                                                                 base_size=28,
                                                                 padding = unit(c(16, 5), "mm"),
                                                                 colnames.style = ggpubr::colnames_style(fill = "lightgray",
                                                                                                         size = 28,
                                                                                                         linecolor = "black")))
    shelf_life_df_tab = ggpubr::tab_add_title(shelf_life_df_tab,text = "Shelf-Life Estimates", face = "bold", size = 28, hjust = -1.2)
    
    if(p_value >= 0.05){
        shelf_life_df_tab <- table_cell_bg(
            shelf_life_df_tab,
            row = 3,
            column = 4,
            linewidth = 5,
            fill = "#f69494",
            color = "#ee0000"
        )
    }
    if(p_value < 0.05){
        shelf_life_df_tab <- table_cell_bg(
            shelf_life_df_tab,
            row = 3,
            column = 4,
            linewidth = 5,
            fill = "#a5eba5",
            color = "#2DC62D"
        )
        
    }
    if(shelf_life < 0){
        shelf_life_df_tab <- table_cell_bg(
            shelf_life_df_tab,
            row = 3,
            column = c(1:2),
            linewidth = 5,
            fill = "#ffec8b",
            color = "#ffd700"
        )
    }
    if(unique(stats_file$Fluorochrome) == "Ab-E" & shelf_life > 0 & shelf_life < 1){
        shelf_life_df_tab <- table_cell_bg(
            shelf_life_df_tab,
            row = 3,
            column = c(1:2),
            linewidth = 5,
            fill = "#f69494",
            color = "#ee0000"
        )
    }
    else if(unique(stats_file$Fluorochrome) == "Purified" & shelf_life > 0 & shelf_life < 2){
        shelf_life_df_tab <- table_cell_bg(
            shelf_life_df_tab,
            row = 3,
            column = c(1:2),
            linewidth = 5,
            fill = "#f69494",
            color = "#ee0000"
        )
    }
    else if(shelf_life < 1.5 & shelf_life > 0){
        shelf_life_df_tab <- table_cell_bg(
            shelf_life_df_tab,
            row = 3,
            column = c(1:2),
            linewidth = 5,
            fill = "#f69494",
            color = "#ee0000"
        )
    }
    if(r_sq < 0.80){
        shelf_life_df_tab <- table_cell_bg(
            shelf_life_df_tab,
            row = 3,
            column = 3,
            linewidth = 5,
            fill = "#ffec8b",
            color = "#ffd700"
        )
    }
    fp <- file.path(stats_file_path, "ShelfLifeEstimates.png")
    png(fp, width=1200, height=800, units="px")
    print(shelf_life_df_tab)
}

anderson_darling_p_value_png <- function(p_value){
    
    ad_p_value_df = ggpubr::ggtexttable(tibble("Anderson-Darling\nNormality Test p-value" = round(p_value, 3)), rows = NULL,
                                        theme=ggpubr::ttheme("classic",
                                                             base_size=28,
                                                             padding = unit(c(16, 5), "mm"),
                                                             colnames.style = ggpubr::colnames_style(fill = "lightgray",
                                                                                                     size = 28,
                                                                                                     linecolor = "black")))
    
    # Reject null hypothesis - not normally distributed - flag as red
    if(p_value < 0.05){
        ad_p_value_df <- table_cell_bg(
            ad_p_value_df,
            row = 2,
            column = 1,
            linewidth = 5,
            fill = "#f69494",
            color = "#ee0000"
        )
    }
    # normally distributed - color green
    else{
        ad_p_value_df <- table_cell_bg(
            ad_p_value_df,
            row = 2,
            column = 1,
            linewidth = 5,
            fill = "#a5eba5",
            color = "#2DC62D"
        )
    }
    fp <- file.path(stats_file_path, "anderson_darling_p_value.png")
    png(fp, width=1200, height=800, units="px")
    print(ad_p_value_df)
}

regression_pdf <- function(regress_plot, reference_mfi_table, shelf_life_summary_table, cell_pop, marker_name, optimal){
    
    # Create regression plot png
    fp <- file.path(stats_file_path, "regression_plot.png")
    png(fp, width = 1200, height = 800, units = "px")
    print(regress_plot)
    dev.off()
    
    # Create % of 4C Reference MFI Table png
    reference_mfi_table
    dev.off()
    
    # Create shelf-life estimates png
    shelf_life_summary_table
    dev.off()
    
    
    
    png_plots = list()
    for (i in c("regression_plot.png","StatsTable.png", "ShelfLifeEstimates.png")) {
	fp <- file.path(stats_file_path, i)
        png_plots[[i]] = grid::rasterGrob(png::readPNG(fp))
    }
    
    figure = ggarrange(
        png_plots[[1]], ggarrange(png_plots[[2]], png_plots[[3]], ncol = 2),
        labels = c(paste0(cell_pop,"\n",marker_name, "\n", optimal)),
        nrow = 2
    )
    
    return(figure)
}

quality_checks_pdf <- function(resid_plot, resid_histogram, normal_prob_plot, normality_table){
    
    # Create residuals vs fit plot png
    fp <- file.path(stats_file_path, "residuals_vs_fit_plot.png")
    png(fp, width = 1200, height = 800, units = "px")
    print(resid_plot)
    dev.off()
    
    # Create histogram of residuals png
    fp <- file.path(stats_file_path, "residual_histogram.png")
    png(fp, width = 1200, height = 800, units = "px")
    print(resid_histogram)
    dev.off()
    
    # Create normal probability plot of residuals png
    fp <- file.path(stats_file_path, "normal_prob_plot_of_residuals.png")
    png(fp, width = 1200, height = 800, units = "px")
    print(normal_prob_plot)
    dev.off()
    
    # Generate p-value for Anderson-Darling Normality Test
    fp <- file.path(stats_file_path, "anderson_darling_p_value.png")
    png(fp, width = 1200, height = 800, units = "px")
    print(normality_table)
    dev.off()
    
    # Loop through each png and print to pdf
    png_plots = list()
    for (i in c("residuals_vs_fit_plot.png", "residual_histogram.png", "normal_prob_plot_of_residuals.png", "anderson_darling_p_value.png")) {
	fp <- file.path(stats_file_path, i)
        png_plots[[i]] = grid::rasterGrob(png::readPNG(fp))
    }
    figure = ggpubr::ggarrange(
        plotlist=png_plots,
        ncol=2, nrow=2
    )
    
    return(figure)
}

######################## BUILD REGRESSION REPORT ############################

build_regression_report <- function(data_path, stats_file, cell_pop, marker_name, optimal){
    
    ## Step 1: Upload raw stats ##
    df <- get_stats_table_for_mfi_table(stats_file, cell_pop)
    
    if(!is.null(df)){
        ## Step 3b: Transform % of 4C Reference MFI data to long ##
        df_melt <- melt_reference_mfi_table(df)
        
        ## Step 5: Add lower 95% Confidence Intervals ##
        bands <- find_confidence_bands(df_melt, ORDER, CI, THRESHOLD_MFI)
        
        ## Step 6: Calculate predicted shelf-life ##
        lower_shelf_life <- solve_for_lower_shelf_life(df_melt, ORDER, CI, THRESHOLD_MFI)
        
        ## Step 7: Calculate model p-value ##
        p_value <- polynomial_evaluation_of_linearity(df_melt, ORDER)
        
        ## Step 8: Calculated R^2 value ##
        r_sq <- R_sq(df_melt, ORDER)
        
        # Step 9: Create plot of regression model ##
        regress_plot <- regression_plot_global(FONT_SIZE, DATA_PT_SIZE, EQN_SIZE, df_melt, bands, ORDER, CI, 0.1, 0.2)  +
            coord_cartesian(ylim=c(0, NA)) + 
            scale_y_continuous(breaks=seq(0, 120, 20)) +
            stat_regline_equation(data=df_melt,
                                  aes(x=Time, y=value,
                                      label=paste(..eq.label..)),
                                  formula = y ~ poly(x,ORDER,raw=TRUE), method="lm", col="red",
                                  label.x=0,label.y=10,size=12) +
            stat_regline_equation(data=df_melt,
                                  aes(x=Time, y=value,
                                      label=paste(..rr.label..)),
                                  formula = y ~ poly(x,ORDER,raw=TRUE), method="lm", col="red",
                                  label.x=0,label.y=5,size=12)
        
        
        residuals <- find_residuals(df_melt, ORDER)
        
        ## Step 10: Create residual vs fit plot ##
        resid_plot <- residual_vs_fit_plot(df_melt, ORDER, FONT_SIZE, DATA_PT_SIZE)
        
        ## Step 11: Create normal probability plot of residuals ##
        normal_prob_plot <- normal_probability_plot(df_melt, ORDER, residuals, FONT_SIZE, DATA_PT_SIZE)
        
        ## Step 12: Create histogram of residuals ##
        resid_histogram <- residual_histogram(df_melt, ORDER, FONT_SIZE)
        
        normality_p_value <- anderson_darling_normality_test(residuals)
        
        shelf_life_df <- create_shelf_life_summary_table(lower_shelf_life, r_sq, p_value)
        
        shelf_life_summary_png <- shelf_life_estimates_table_png(read_csv(stats_file, col_types = cols()), shelf_life_df, p_value$b_pvalue, lower_shelf_life, r_sq)
        
        reference_mfi_table_png <- perc_4C_mfi_table_png(df)
        
        normality_pvalue_png <- anderson_darling_p_value_png(normality_p_value)
        
        pdf(file.path(data_path, paste0("regression_report_",cell_pop, ".pdf")), title="Regression for Stability", width = 16, height = 10, onefile = TRUE)
        print(regression_pdf(regress_plot, reference_mfi_table_png, shelf_life_summary_png, cell_pop, marker_name, optimal))
        print(quality_checks_pdf(resid_plot, resid_histogram, normal_prob_plot, normality_pvalue_png))
        
        dev.off()
    }
    else{
        return(cat(paste0("Cannot create regression report for ", cell_pop, ", see all_stats.csv file.\n")))
    }
    
    
}
# build_regression_report("all_stats_new_wellids.csv", "Lymph")

merge_full_stability_report <- function(data_path, report_filename_list, all_regression_reports){
    
    omiq_stability_report <- file.path(data_path, "full_stability_report.pdf")
    title_page_output <- file.path(data_path, "title_page.pdf")
    omiq_stability_subset_output <- file.path(data_path, "omiq_stability.pdf")
    
    ## Subset title page so we can add regression reports after title page
    title_page <- pdf_subset(omiq_stability_report, 
                             pages = 1, 
                             output = title_page_output)
    
    ## Subset omiq stability report without title page
    omiq_stability_pages <- pdf_subset(omiq_stability_report, 
                                       pages = 2:pdf_info(omiq_stability_report)$pages, 
                                       output = omiq_stability_subset_output)
    
    ## Combine title page, regression reports for each cell pop, and omiq stability report
    pdf_combine(c(title_page,
                  all_regression_reports,
                  omiq_stability_pages
                  
    ),
    output = file.path(data_path, "final_stability_report_with_regression.pdf"))
    
    ## Remove subsetted files from directory
    invisible(file.remove(c(title_page_output, omiq_stability_subset_output, all_regression_reports, report_filename_list)))
}

build_regression_report_per_cell_pop <- function(data_path, stats_file){
    stats_file <- file.path(data_path, stats_file)
    
    stats_df <- read_csv(stats_file, col_types = cols())
    marker_name <- paste(unique(stats_df$Target.Species), 
                         unique(stats_df$Specificity..CD.), 
                         unique(stats_df$Clone), 
                         unique(stats_df$Fluorochrome))
    
    cell_pop_list <- unique(stats_df$pop)
    regression_report_filename_list <- c()
    for(pop in cell_pop_list){
        cat(paste0("Building regression report for ", pop, "\n"))
        optimal <- unique(stats_df$Optimal.with.units[stats_df$pop == pop])
        report <- build_regression_report(data_path, stats_file, pop, marker_name, optimal)
        report
        
        if(!is.null(report)){
            regression_report_filename_list <- append(regression_report_filename_list, 
                                                      file.path(data_path, paste0("regression_report_", pop, ".pdf")))
        }
        else{
            regression_report_filename_list <- regression_report_filename_list
        }
        
        
    }
    
    all_regression_reports <- pdf_combine(regression_report_filename_list, output = "all_reports.pdf")
    merge_full_stability_report(data_path, regression_report_filename_list, all_regression_reports)
    cat("Regression reports complete.\n")
}

stats_file_path = file.path(datadump, args[1])
print(paste0("stats file path:", stats_file_path, "/all_stats.csv"))
build_regression_report_per_cell_pop(stats_file_path, "all_stats.csv")
