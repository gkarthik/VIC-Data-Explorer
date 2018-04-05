library(dunn.test)

source("./filterUI.R")

## Numerical Features
generate_annotations <- function(t, ymax, diff){
    start = c()
    end = c()
    ypos = c()
    label=c()
    for(i in rownames(t)){
        if(t[i ,"P.adjusted"] <= 0.05){
            ymax = ymax + (diff/length(names(t)))
            v <- unlist(strsplit(as.character(t[i, "comparisons"]), " - "))
            start <- c(start, v[1])
            end <- c(end, v[2])
            ypos <- c(ypos, ymax)
            label <- c(label, signif(as.numeric(t[i, "P.adjusted"]), 3))
        }
    }
    return(data.frame(start, end, ypos, label))
}

is_outlier <- function(x) {
    return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

fun_mean <- function(x){
    return(data.frame(y=mean(x),label=signif(mean(x,na.rm=T), 2)))
}

shinyServer(function(input, output, session) {

    filterVal <- reactiveValues();
    filterVal$el <- c()
    filterVal$val <- c()

    observeEvent(input$addFilter, {
        btn <- input$addFilter
        id <- paste0('filter-element-', btn)
        filterID <- paste0('filter-', btn)
        insertUI(
            selector = '#filter-wrapper',
            ui = tags$div(
                          filterUI(filterID),
                          id = id
                      ),
            where="beforeBegin"
        )
        filterVal$val <- c(filterVal$val, callModule(filter, filterID))
        filterVal$el <<- c(filterVal$el,id)
    });

    observeEvent(input$removeFilter, {
        removeUI(
            selector = paste0('#', filterVal$el[length(filterVal$el)])
        )
        filterVal$el <<- filterVal$el[-length(filterVal$el)]
        filterVal$val <- filterVal$val[-length(filterVal$val)]
    });

    output$dfStatus <- renderText({
        if(generateCondExp()!=""){
            paste("df[with(df,",generateCondExp(),"),]")
        }
    })

    generateCondExp <- function(){
        ## df <- filterDataset()
        cond <- sapply(filterVal$val, function(x){
            l <- x()
            req(l$col, l$cond);
            if(class(subl[,l$col]) == "numeric"){
                paste(l$col, ">=", min(l$cond), "&", l$col, "<=", max(l$cond), sep=" ")
            } else {
                paste0(l$col, " %in% ", "c(\"", paste(l$cond, collapse="\",\"") ,"\")")
            }
        });
        return(paste(cond, collapse=" & "));
    }

    filterDataset <- reactive({
        if(generateCondExp() != ""){
            subl[with(subl, eval(parse(text=generateCondExp()))),]
        } else {
            subl
        }
    })

    output$detailsText <- renderText({
        df <- filterDataset()
        if(length(df) == 0){
            ""
        }
        if(class(df[,input$x]) == "numeric" && class(df[,input$y]) == "numeric"){
            c <- cor.test(df[,input$x], df[,input$y], method = "spearman")
            paste("Spearman's Rho = ", round(c$estimate, 4), "<br>P-value = ", c$p.value)
        } else if(class(df[,input$x]) == "factor" && class(df[,input$y]) == "numeric") {
            temp <- dunn.test(df[,input$y], df[,input$x], method="bh")
            temp <- as.data.frame(temp)
            temp <- temp[with(temp, order(comparisons)),]
            anndf <- generate_annotations(temp, max(df[,input$y]), (range(df[,input$y])[2] - range(df[,input$y])[1]))
            if(nrow(anndf) == 0){
                "None of the tests are statistically significant at a cutoff of 0.05."
            } else {
                t <- apply(anndf[,c("start", "end", "label")], 1, function(x){paste(c("<tr><td>", x, "</td></tr>"), collapse="</td><td>")})
                t <- paste(c("<h3>Dunn's test(FDR by BH)</h3><table>", t, "</table>"))
                t
            }
        } else if ((class(df[,input$x]) == "numeric" && class(df[,input$y]) == "factor")){
            temp <- dunn.test(df[,input$x], df[,input$y], method="bh")
            temp <- as.data.frame(temp)
            temp <- temp[with(temp, order(comparisons)),]
            anndf <- generate_annotations(temp, max(df[,input$x]), (range(df[,input$x])[2] - range(df[,input$x])[1]))
            if(nrow(anndf) == 0){
                "None of the tests are statistically significant at a cutoff of 0.05."
            } else {
                t <- apply(anndf[,c("start", "end", "label")], 1, function(x){paste(c("<tr><td>", x, "</td></tr>"), collapse="</td><td>")})
                t <- paste(c("<h3>Dunn's test(FDR by BH)</h3><table>", t, "</table>"))
                t
            }
        } else if (class(df[,input$x]) == "factor" && class(df[,input$x]) == "factor"){

        }
    });

    output$trendPlot <- renderPlotly({
        df <- filterDataset();
        if(class(df[,input$x]) == "numeric" && class(df[,input$y]) == "numeric"){
            ggplot(df, aes_string(x = input$x, y = input$y, color=input$color)) + geom_point() + theme_bw()
        } else if(class(df[,input$x]) == "factor" && class(df[,input$y]) == "numeric") {
            medians <- aggregate(df[,input$y], by=list(df[,input$x]), FUN=median)
            colnames(medians) <- c("Group", "x")
            medians[,"Group"] <- factor(medians$Group)
            ggplot(df, aes_string(x = input$x, y = input$y)) + geom_jitter(aes_string(color=input$color), stroke=0, position=position_jitter(width = 0.1, height=0), size=4, alpha=0.7) + stat_summary(fun.data="median_hilow", geom="linerange", size=0.2, colour="black") + theme_bw() + geom_segment(aes(x=as.numeric(Group)-0.3, xend=as.numeric(Group)+0.3, y=x, yend=x), data=medians)
        } else if((class(df[,input$x]) == "numeric" && class(df[,input$y]) == "factor")){
            updateSelectInput(session, "y", selected = input$x);
            updateSelectInput(session, "x", selected = input$y);
            medians <- aggregate(df[,input$x], by=list(df[,input$y]), FUN=median)
            colnames(medians) <- c("Group", "x")
            medians[,"Group"] <- factor(medians$Group)
            ggplot(df, aes_string(x = input$y, y = input$x)) + geom_jitter(aes_string(color=input$color), stroke=0, position=position_jitter(width = 0.1, height=0), size=4, alpha=0.7) + stat_summary(fun.data="median_hilow", geom="linerange", size=0.2, colour="black") + theme_bw() + geom_segment(aes(x=as.numeric(Group)-0.3, xend=as.numeric(Group)+0.3, y=x, yend=x), data=medians)
        } else if (class(df[,input$x]) == "factor" && class(df[,input$x]) == "factor"){
            ## updateSelectInput(session, "color", selected = input$y)
            t <- data.frame(table(df[,input$x],df[,input$y]));
            names(t) <- c(input$x,input$y,"Count");
            ggplot(t, aes_string(x=input$x, y="Count", fill=input$y)) + geom_bar(stat="identity")+ theme_bw()
        }
    })
})
