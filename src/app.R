library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(rjson)
library(plotly)
library(GGally)
library(ggiraph)
library(ggplot2)
library(tidyverse)



##### Data
df <- read.csv(file = 'data/processed/bank.csv', stringsAsFactors = FALSE)
df.g <- read.csv(file = 'data/processed/bank_group.csv', stringsAsFactors = FALSE)
df.n <- read.csv(file = 'data/processed/bank_numeric.csv', stringsAsFactors = FALSE)
df.c <- read.csv(file = 'data/processed/bank_categorical.csv', stringsAsFactors = FALSE)



app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
      list(
        
        htmlBr(),
        htmlH1('Term Deposit Subscription Analysis Dashboard'),
        htmlBr(),
        htmlP('Try with the first three charts to gain more information about the clients and marketing campaign'),
        htmlP('Try with the last two charts to see prediction analysis for the direct marketing campaign results'),
        
        
        
        # PLOTS 1-5
        dbcTabs(
          list(
            
          
            # TAB 1
            dbcTab(
              list(
                
                # DROPDOWN MENU - GROUP DISTRIBUTION
                htmlBr(),
                htmlLabel("Select variable to see the corresponding subgroup percentages"),
                htmlBr(),
                htmlLabel("* Hover over the plot to see the group names,
                          number of records for each subgroup, and the percentages"),
                dccDropdown(
                  id='donut-menu',
                  options = df.g %>%
                    colnames %>%
                    purrr::map(function(col) list(label = col, value = col)), 
                  value='Age.Group'),
                htmlBr(),
                
                
                # PLOT 3 - DISTRIBUTION DONUT CHART
                dccGraph(id = 'donut-plot'),
                htmlBr()
              ), label = 'Data Distribution (Ratio)'
            ),  
            

            
            
            # TAB 2
            dbcTab(
              list(
                
                # DROPDOWN MENU - GROUP DISTRIBUTION
                htmlBr(),
                htmlLabel("Select variable to see the corresponding subgroups"),
                htmlBr(),
                htmlLabel("* Ordered by count of records descending"),
                htmlBr(),
                htmlLabel("* Hover over the plot to see count of each subgroup"),
                htmlBr(),
                dccDropdown(
                  id='bar-menu',
                  options = df.g %>%
                    colnames %>%
                    purrr::map(function(col) list(label = col, value = col)), 
                  value='Age.Group'),
                
                
                # PLOT 2 - DISTRIBUTION BAR CHART
                dccGraph(id = 'bar-plot'),
                htmlBr()
              ), label = 'Data Distribution (Count)'
            ),
            
            
            
            
            # TAB 3
            dbcTab(
              list(
                
                # DROPDOWN MENU - X AXIS - CATEGORICAL
                htmlBr(),
                htmlLabel("Select values for x and y axes to see data distrubutions and relationships between attributes"),
                htmlBr(),
                htmlLabel("* Hover over the plot to see details of data distributions"),
                htmlBr(),
                htmlLabel('x-axis'),
                dccDropdown(
                  id='box-x',
                  options = df.c %>%
                    colnames %>%
                    purrr::map(function(col) list(label = col, value = col)), 
                  value='Type.of.Job'),
                
                
                # DROPDOWN MENU - Y AXIS - NUMERICAL
                htmlLabel('y-axis'),
                dccDropdown(
                  id='box-y',
                  options = df.n %>%
                    colnames %>%
                    purrr::map(function(col) list(label = col, value = col)), 
                  value='Age'),
                
                
                # PLOT 1 - CORRELATION BOX PLOT
                dccGraph(id = 'box-plot'),
                htmlBr()
                
              ), label = 'Correlation Distribution'
            ),
            
            
            # TAB 4
            dbcTab(
              list(
                # DROPDOWN MENU - X AXIS - CATEGORICAL
                htmlBr(),
                htmlLabel("Select values for numerical variables to see their performance in prediction"),
                htmlBr(),
                htmlLabel("* Hover over the plot to see counts in different categories"),
                htmlBr(),
                htmlLabel('Categorical Variables'),
                dccDropdown(
                  id='bar-x',
                  options = df.c %>%
                    colnames %>%
                    purrr::map(function(col) list(label = col, value = col)), 
                  value='Type.of.Job'),
                
                # PLOT 4 - numerical prediction plot
                dccGraph(id = 'barplot_predict'),
                htmlBr()

              ), label = 'Categorical Value Prediction'
            ),
            
            
            
            
            
            # TAB 5
            dbcTab(
              list(
                htmlBr(),
                htmlLabel("Select values from numerical values to see the distribution in subscribe or non-subscribe group"),
                htmlBr(),
                htmlLabel("* Hover over the plot to see counts in different categories"),
                htmlBr(),
                # DROPDOWN MENU - Y AXIS - NUMERICAL
                htmlLabel('Numerical Variables'),
                dccDropdown(
                  id='line-x',
                  options = df.n %>%
                    colnames %>%
                    purrr::map(function(col) list(label = col, value = col)), 
                  value='Age'),
                
                # PLOT 4 - numerical prediction plot
                dccGraph(id = 'lineplot_predict'),
                htmlBr()
                
                
                
                
                
              ), label = 'Numerical Value Prediction'
            )
          
            
            
            
            
            
            
            
        )
      )
    )
  )
)



##### PLOT 1 - Donut Charts

app$callback(
  output('donut-plot', 'figure'),
  list(input('donut-menu', 'value')),
  function(ycol) {
    
    data <- df.g %>%
      group_by(!!sym(ycol)) %>%
      summarize(count = n())
    
    if(!!sym(ycol) == "Age.Group"){
      donut <- plot_ly(data, values = ~count, text = ~Age.Group) %>%
        add_pie(hole = 0.6) %>%
        layout(showlegend = F, 
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
      
    }else if(!!sym(ycol) == "Type.of.Job"){
      donut <- plot_ly(data, values = ~count, text = ~Type.of.Job) %>%
        add_pie(hole = 0.6) %>%
        layout(showlegend = F, 
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
      
    }else if(!!sym(ycol) == "Marital.Status"){
      donut <- plot_ly(data, values = ~count, text = ~Marital.Status) %>%
        add_pie(hole = 0.6) %>%
        layout(showlegend = F, 
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
      
    }else if(!!sym(ycol) == "Education.Level"){
      donut <- plot_ly(data, values = ~count, text = ~Education.Level) %>%
        add_pie(hole = 0.6) %>%
        layout(showlegend = F, 
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
      
    }else if(!!sym(ycol) == "Credit.in.Default"){
      donut <- plot_ly(data, values = ~count, text = ~Credit.in.Default) %>%
        add_pie(hole = 0.6) %>%
        layout(showlegend = F, 
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
      
    }else if(!!sym(ycol) == "Annual.Balance..Euros."){
      donut <- plot_ly(data, values = ~count, text = ~Annual.Balance..Euros.) %>%
        add_pie(hole = 0.6) %>%
        layout(showlegend = F, 
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
      
    }else if(!!sym(ycol) == "Loan..Housing...Personal."){
      donut <- plot_ly(data, values = ~count, text = ~Loan..Housing...Personal.) %>%
        add_pie(hole = 0.6) %>%
        layout(showlegend = F, 
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
      
    }else if(!!sym(ycol) == "Last.Contact.Duration..sec."){
      donut <- plot_ly(data, values = ~count, text = ~(Last.Contact.Duration..sec.)) %>%
        add_pie(hole = 0.6) %>%
        layout(showlegend = F, 
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
      
    }else if(!!sym(ycol) == "Contacts.Performed..current."){
      donut <- plot_ly(data, values = ~count, text = ~(Contacts.Performed..current.)) %>%
        add_pie(hole = 0.6) %>%
        layout(showlegend = F, 
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
      
    }else if(!!sym(ycol) == "Contacts.Performed..previous."){
      donut <- plot_ly(data, values = ~count, text = ~(Contacts.Performed..previous.)) %>%
        add_pie(hole = 0.6) %>%
        layout(showlegend = F, 
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
      
    }else if(!!sym(ycol) == "Month.Since.Last.Contacted"){
      donut <- plot_ly(data, values = ~count, text = ~Month.Since.Last.Contacted) %>%
        add_pie(hole = 0.6) %>%
        layout(showlegend = F, 
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
      
    }else if(!!sym(ycol) == "Previous.Outcomes"){
      donut <- plot_ly(data, values = ~count, text = ~Previous.Outcomes) %>%
        add_pie(hole = 0.6) %>%
        layout(showlegend = F, 
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
      
    }else if(!!sym(ycol) == "Predicted.Subscription..current."){
      donut <- plot_ly(data, values = ~count, text = ~(Predicted.Subscription..current.)) %>%
        add_pie(hole = 0.6) %>%
        layout(showlegend = F, 
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
    }
    
    ggthemes::scale_color_tableau()
    ggplotly(donut, width=1000, height=400)
    
  }
)





##### PLOT 2 - Bar Chart

app$callback(
  output('bar-plot', 'figure'),
  list(input('bar-menu', 'value')),
  function(ycol) {
    
    p <- ggplot(df.g) +
      aes(y = fct_rev(fct_infreq(!!sym(ycol))), fill = !!sym(ycol)) +
      geom_bar(stat = 'count', position = 'dodge') +
      theme(legend.position = "none", 
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      xlab("Count of Records")
    ggthemes::scale_color_tableau()
    ggplotly(p, width=1000, height=400)
    
  }
)






##### PLOT 3 - Box Plot

app$callback(
  output('box-plot', 'figure'),
  list(input('box-x', 'value'),
       input('box-y', 'value')),
  function(xcol, ycol) {
    
    f <- ggplot(df) +
      aes(y = !!sym(ycol), x = !!sym(xcol), fill = !!sym(xcol)) +
      geom_boxplot(outlier.colour="black", outlier.shape = 4, outlier.size=1)+
      theme_bw() +
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    ggthemes::scale_color_tableau()
    ggplotly(f, width=1000, height=400)
  }
)






##### PLOT 4 - Bar Plot

app$callback(
  output('barplot_predict','figure'),
  list(input('bar-x', 'value')),
  function(xcol){
    select_df_yes <- df %>% filter(Predicted.Subscription..current.=="yes")
    select_df_no <- df %>% filter(Predicted.Subscription..current.=="no")
    
    p2 <- ggplot(select_df_yes,
                aes(x = !!sym(xcol),fill = !!sym(xcol))) +
      geom_bar(stat='count')+
      xlab("Subscribe the Term Deposite")+
      ylim(0, 1000)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none") +
      ggthemes::scale_color_tableau()
    
    p1 <- ggplot(select_df_no,
                aes(x = !!sym(xcol),fill = !!sym(xcol))) +
      geom_bar(stat='count')+
      xlab("NOT Subscribe the Term Deposite")+
      ylim(0, 1000)+
      ggthemes::scale_color_tableau()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none")
    
   
    subplot(ggplotly(p1,width=1000, height=400), ggplotly(p2,width=1000, height=400), shareY = TRUE,titleX=TRUE)
  }
)




##### PLOT 5 - Donut Charts

app$callback(
  output('lineplot_predict','figure'),
  list(input('line-x', 'value')),
  function(xcol){
    select_df_yes <- df %>% filter(Predicted.Subscription..current.=="yes")
    select_df_no <- df %>% filter(Predicted.Subscription..current.=="no")
    p2 <- ggplot(select_df_yes,
                 aes(x = !!sym(xcol))) +
      geom_bar()+
      xlab("Subscribe the Term Deposite")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none") +
      ggthemes::scale_color_tableau()
    
    p1 <- ggplot(select_df_no,
                 aes(x = !!sym(xcol))) +
      geom_bar()+
      xlab("NOT Subscribe the Term Deposite")+
      ggthemes::scale_color_tableau()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none")
    
    
    subplot(ggplotly(p1,width=1000, height=400), ggplotly(p2,width=1000, height=400), shareY = TRUE,titleX=TRUE)
  }
)






app$run_server(debug = T)














