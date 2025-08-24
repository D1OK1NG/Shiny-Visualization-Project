library(shiny)
library(ggplot2)

ui <- fluidPage(
  tags$head(tags$style(HTML('body {font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, "Noto Sans", "Apple Color Emoji", "Segoe UI Emoji";} .small-note{color:#666;font-size:12px}'))),
  titlePanel("Visualizing Discrete Probability Distributions"),
  sidebarLayout(
    sidebarPanel(
      withMathJax(
        HTML("
  <h4>Discrete Probability Distributions</h4>
  <p>
    A discrete random variable \\(X\\) is called <em>discrete</em> if its set of possible values is finite or countably infinite.
    For a discrete random variable, there exist unique nonnegative functions: the
    <span style='color:#2171b5;font-weight:bold;'>probability mass function</span> (PMF) \\(f_X(x)\\) and the
    <span style='color:#238b45;font-weight:bold;'>cumulative distribution function</span> (CDF) \\(F_X(x)\\), defined by
  </p>
  <ul>
    <li>\\( f_X(x) = P(X = x) \\)</li>
    <li>\\( F_X(x) = P(X \\le x) ; -\\infty < x < \\infty \\)</li>
  </ul>
  <p>
    Choose one of the following major discrete distributions to visualize. The <span style='color:#2171b5;font-weight:bold;'>PMF</span> is shown in 
    <span style='color:#2171b5;font-weight:bold;'>blue</span> and the <span style='color:#238b45;font-weight:bold;'>CDF</span> in 
    <span style='color:#238b45;font-weight:bold;'>green</span>.
  </p>
  ")
      ),
      
      selectInput("dist", "Choose discrete distribution", c(
        "Bernoulli" = "bern",
        "Binomial" = "binom",
        "Negative Binomial" = "nbinom",
        "Geometric" = "geom",
        "Poisson" = "pois",
        "Hypergeometric" = "hyper",
        "Discrete Uniform" = "dunifd"
      ), selected = "bern"),
      
      conditionalPanel(
        condition = "input.dist == 'bern'",
        sliderInput("p_bern", "p", min = 0, max = 1, value = 0.50, step = 0.01)
      ),
      conditionalPanel(
        condition = "input.dist == 'binom'",
        sliderInput("n", "n", min = 1, max = 20, value = 5, step = 1),
        sliderInput("p", "p", min = 0, max = 1, value = 0.5, step = 0.01)
      ),
      conditionalPanel(
        condition = "input.dist == 'nbinom'",
        sliderInput("r_nb", "r", min = 1, max = 20, value = 5, step = 1),
        sliderInput("p_nb", "p", min = 0.01, max = 1, value = 0.5, step = 0.01),
      ),
      conditionalPanel(
        condition = "input.dist == 'geom'",
        sliderInput("pg", "p", min = 0.01, max = 1, value = 0.50, step = 0.01),
      ),
      conditionalPanel(
        condition = "input.dist == 'pois'",
        sliderInput("lambda", "λ", min = 0.01, max = 10, value = 5, step = 0.1)
      ),
      conditionalPanel(
        condition = "input.dist == 'hyper'",
        sliderInput("N_hy", "N", min = 10, max = 30, value = 10, step = 1),
        sliderInput("K_hy", "K", min = 1, max = 30, value = 10, step = 1),
        sliderInput("n_hy", "n", min = 1, max = 30, value = 10, step = 1),
      ),
      conditionalPanel(
        condition = "input.dist == 'dunifd'",
        sliderInput("a_du", "a", min = -10, max = 10, value = 0, step = 1),
        sliderInput("b_du", "b", min = -10, max = 10, value = 0, step = 1),
      ),
      
      
      
      hr(),
      
      uiOutput("dist_desc"),
      
      h4("Formula"),
      withMathJax(uiOutput("formula")),
      hr(),
      helpText("Click on the graph to highlight a value k, and the panel will display P(X = k) and F(k) = P(X ≤ k)")
    ),
    
    mainPanel(
      fluidRow(
        column(6, plotOutput("pmf_plot", height = 360, click = "click_pmf")),
        column(6, plotOutput("cdf_plot", height = 360, click = "click_cdf"))
      ),
      fluidRow(
        column(12, h4("Current Selection"), verbatimTextOutput("sel_text"))
      )
    )
  )
)

server <- function(input, output, session){
  default_range <- reactive({
    switch(input$dist,
           bern = c(0, 1),
           binom = c(0, input$n),
           nbinom = {
             r <- input$r_nb
             p <- max(0.001, min(0.999, input$p_nb))
             mu <- r * (1 - p) / p
             sigma <- sqrt(r * (1 - p) / (p^2))     # Var = r(1-p)/p^2
             lo <- max(0, floor(mu - 6 * sigma))
             hi <- ceiling(mu + 6 * sigma)
             c(lo, max(lo + 10, hi))
           },
           geom = {
             p <- max(0.001, min(0.999, input$pg))
             kmax <- ceiling(log(1 - 0.999) / log(1 - p)-1)
             c(0, max(10, kmax))
           },
           pois  = {
             mu <- input$lambda
             lo <- max(0, floor(mu - 6 * sqrt(mu)))
             hi <- ceiling(mu + 6 * sqrt(mu))
             c(lo, hi)
           },
           hyper = {
             N <- input$N_hy; K <- input$K_hy; n <- input$n_hy
             K <- max(0, min(K, N)); n <- max(0, min(n, N))   # 先夹紧
             lo <- max(0, n + K - N)
             hi <- min(n, K)
             c(lo, hi)
           },
           dunifd = {
             a <- min(input$a_du, input$b_du)
             b <- max(input$a_du, input$b_du)
             c(a, b)
           }
    )
  })
  
  observeEvent(input$N_hy, {
    req(input$N_hy)
    N <- input$N_hy
    
    updateSliderInput(session, "K_hy", max = N)
    updateSliderInput(session, "n_hy", max = N)
    
    K <- isolate(input$K_hy); n <- isolate(input$n_hy)
    if (!is.null(K)) updateSliderInput(session, "K_hy", value = max(0, min(K, N)))
    if (!is.null(n)) updateSliderInput(session, "n_hy", value = max(0, min(n, N)))
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$K_hy, input$n_hy, input$N_hy), {
    req(input$N_hy, input$K_hy, input$n_hy)
    N <- input$N_hy; K <- input$K_hy; n <- input$n_hy
    if (K < 0 || K > N) updateSliderInput(session, "K_hy", value = max(0, min(K, N)))
    if (n < 0 || n > N) updateSliderInput(session, "n_hy", value = max(0, min(n, N)))
  }, ignoreInit = TRUE)
  
  dist_df <- reactive({
    rng <- default_range()
    
    if (input$dist == "bern") {
      k <- 0:1
      p <- input$p_bern
      pmf <- ifelse(k == 1, p, 1 - p)
      cdf <- cumsum(pmf)
      mean_val <- p
      
    } else if (input$dist == "binom") {
      k <- seq(rng[1], rng[2])
      pmf <- dbinom(k, size = input$n, prob = input$p)
      cdf <- pbinom(k, size = input$n, prob = input$p)
      mean_val <- input$n * input$p
      
    } else if (input$dist == "nbinom") {
      k <- seq(max(0, rng[1]), max(0, rng[2]))
      p <- max(0.001, min(0.999, input$p_nb))
      pmf <- dnbinom(k, size = input$r_nb, prob = p)
      cdf <- pnbinom(k, size = input$r_nb, prob = p)
      mean_val <- input$r_nb * (1 - p) / p
    
    } else if (input$dist == "geom") {
      k <- seq(max(0, rng[1]), max(0, rng[2]))
      p <- max(0.001, min(0.999, input$pg))
      pmf <- dgeom(k, prob = p)
      cdf <- pgeom(k, prob = p)
      mean_val <- (1 - p) / p
      
    } else if (input$dist == "pois") {
      k <- seq(rng[1], rng[2])
      pmf <- dpois(k, lambda = input$lambda)
      cdf <- ppois(k, lambda = input$lambda)
      mean_val <- input$lambda
      
    } else if (input$dist == "hyper") {
      N <- input$N_hy; K <- input$K_hy; n <- input$n_hy
      K <- max(0, min(K, N)); n <- max(0, min(n, N))
      lo <- max(0, n + K - N); hi <- min(n, K)
      k <- if (lo <= hi) seq(lo, hi) else integer(0)
      pmf <- if (length(k)) dhyper(k, m = K, n = N - K, k = n) else numeric(0)
      cdf <- if (length(k)) cumsum(pmf) else numeric(0)
      mean_val <- n * (K / N)
    
    } else { # dunifd
      a <- min(input$a_du, input$b_du); b <- max(input$a_du, input$b_du)
      k <- seq(a, b)
      pmf <- rep(1 / (b - a + 1), length(k))
      cdf <- cumsum(pmf)
      mean_val <- (a + b) / 2
    }
    
    df <- data.frame(k = k, pmf = pmf, cdf = pmin(1, cdf), mean = mean_val)
    validate(need(nrow(df) > 0, "There is no valid support set for the parameter combination. Please adjust the parameters."))
    df
  })
  
  output$dist_desc <- renderUI({
    if (input$dist == "bern") {
      HTML("
      <b>Bernoulli Distribution</b><br>
          A random variable Y is called a <b>Bernoulli</b> random variable if it has only two possible values, 0 and 1.<br>
          Y ~ Bernoulli(p) means P(Y = 1) = p and P(Y = 0) = 1 − p, where p is the parameter of the distribution.")
    } else if (input$dist == "binom") {
      HTML("
    <b>Binomial Distribution</b><br>
    Let <em>X</em> be the number of successes in <em>n</em> independent Bernoulli trials,
    each with success probability <em>p</em>. Then <em>X</em> has the <b>Binomial</b> distribution
    with parameters <em>n</em> and <em>p</em>. We write
    <em>X ~ Binomial(n, p)</em> (or <em>X ~ Bin(n, p)</em>).<br>
    Thus, the binomial distribution counts the number of successes in a fixed number of Bernoulli trials.
  ")
    }else if (input$dist == "nbinom") {HTML("
    <b>Negative Binomial Distribution</b><br>
    Let <em>X</em> be the number of failures before the <em>k</em>-th success occurs 
    in a sequence of independent Bernoulli trials, each with success probability <em>p</em>. 
    Then <em>X</em> has the <b>Negative Binomial</b> distribution with parameters <em>k</em> and <em>p</em>, 
    and we write <em>X ~ NegBin(k, p)</em>.<br>
  ")
    }else if (input$dist == "geom") {HTML("
    <b>Geometric Distribution</b><br>
    Let <em>X</em> be the number of failures before the first success occurs 
    in a sequence of independent Bernoulli trials, each with probability of success <em>p</em> on a single trial. 
    Then <em>X</em> has the <b>Geometric</b> distribution with parameter <em>p</em>, 
    and we write <em>X ~ Geometric(p)</em>.
  ")
    }else if (input$dist == "pois") {HTML("
    <b>Poisson Distribution</b><br>
    A discrete random variable <em>X</em> is said to have a <b>Poisson</b> distribution with parameter 
    <em>&lambda; &gt; 0</em>. The parameter <em>&lambda;</em> represents the average rate of occurrence of events 
    in a fixed interval of time or space. The Poisson distribution is commonly used to model the number of 
    arrivals, occurrences, or events happening independently at a constant rate. For this distribution, the 
    mean and variance are both equal to <em>&lambda;</em>.

  ")
    }else if (input$dist == "hyper") {HTML("
    <b>Hypergeometric Distribution</b><br>
    Let <em>X</em> be the number of “successes” in <em>n</em> draws without replacement 
    from a finite population of size <em>N</em>, of which <em>M</em> are “successes”. 
    Then <em>X</em> has the <b>hypergeometric</b> distribution with parameters 
    <em>n</em>, <em>M</em>, and <em>N</em>, and we write <em>X ~ HYP(n, M, N)</em> 
    (or <em>X ~ Hypergeometric(n, M, N)</em>).<br><br>
    The support is <em>x = max(0, n - N + M), ..., min(n, M)</em>.
  ")
    }else if (input$dist == "dunifd") {HTML("
    <b>Discrete Uniform Distribution</b><br>
    Suppose <em>X</em> is equally likely to take any integer value from 
    <em>a</em> to <em>b</em>. Then <em>X</em> has a 
    <b>discrete uniform</b> distribution on {a, a+1, …, b}, 
    and we write <em>X ~ DU(a, b)</em>.<br><br>
    Each outcome in the set has the same probability, 
    and the support is {a, a+1, …, b}.
  ")
    }
  })
  
  
  selected_k <- reactiveVal(NULL)
  observeEvent(dist_df(), ignoreInit = TRUE, {
    df <- dist_df()
    selected_k(df$k[which.max(df$pmf)])
  })
  
  nearest_k <- function(x_click, ks){
    if (is.null(x_click)) return(NULL)
    ks[which.min(abs(ks - x_click))]
  }
  
  observeEvent(input$click_pmf, {
    k_new <- nearest_k(input$click_pmf$x, dist_df()$k)
    if (!is.null(k_new)) selected_k(k_new)
  })
  observeEvent(input$click_cdf, {
    k_new <- nearest_k(input$click_cdf$x, dist_df()$k)
    if (!is.null(k_new)) selected_k(k_new)
  })
  
 
  output$pmf_plot <- renderPlot({
    df <- dist_df()
    k_sel <- selected_k()
    p_sel <- if (!is.null(k_sel) && k_sel %in% df$k) df$pmf[df$k == k_sel] else NA_real_
    
    rng <- default_range()
    
    g <- ggplot(df, aes(k, pmf)) +
      geom_col(width = 0.9, fill = "#9ecae1") +
      scale_x_continuous(limits = c(rng[1] - 0.5, rng[2] + 0.5),
                         breaks = seq(rng[1], rng[2], by = 1)) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = "PMF：P(X = k)", x = "k", y = "Probability") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
    
    if (!is.null(k_sel) && k_sel %in% df$k) {
      g <- g +
        geom_col(data = subset(df, k == k_sel), aes(k, pmf), fill = "#2171b5") +
        annotate("text", x = k_sel, y = p_sel,
                 label = sprintf("k=%d\nP=%.4f", k_sel, p_sel),
                 vjust = -0.6, size = 4)
    }
    g
  })
  
  output$cdf_plot <- renderPlot({
    df <- dist_df()
    k_sel <- selected_k()
    F_sel <- if (!is.null(k_sel) && k_sel %in% df$k) df$cdf[df$k == k_sel] else NA_real_
    
    rng <- default_range()
    
    g <- ggplot(df, aes(k, cdf)) +
      geom_step(linewidth = 1, direction = "vh", color = "#238b45") +
      geom_point(size = 1.6, color = "#238b45") +
      scale_x_continuous(limits = c(rng[1] - 0.5, rng[2] + 0.5),
                         breaks = seq(rng[1], rng[2], by = 1)) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(title = "CDF：F(k) = P(X ≤ k)", x = "k", y = "Cumulative probability") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
    
    if (!is.null(k_sel) && k_sel %in% df$k) {
      g <- g +
        geom_point(data = subset(df, k == k_sel), aes(k, cdf),
                   color = "#238b45", size = 3) +
        annotate("text", x = k_sel, y = F_sel,
                 label = sprintf("F(%d)=%.4f", k_sel, F_sel),
                 vjust = -0.8, size = 4, color = "#238b45")
    }
    g
  })
  
  
  output$sel_text <- renderText({
    df <- dist_df()
    k_sel <- selected_k()
    if (is.null(k_sel) || !(k_sel %in% df$k)) return("You can select any k in the figure to highlight it")
    p <- df$pmf[df$k == k_sel]
    Fk <- df$cdf[df$k == k_sel]
    sprintf("k = %d\nP(X = %d) = %.6f\nF(%d) = P(X ≤ %d) = %.6f", k_sel, k_sel, p, k_sel, k_sel, Fk)
  })
  
  output$formula <- renderUI({
    spec <- list(
      bern = "<ul>
  <li><b>PMF</b>: $$f_Y(y)=\\begin{cases}
      p, & y=1 \\\\
      1-p, & y=0 \\\\
      0, & \\text{otherwise}
    \\end{cases}$$</li>

  <li><b>CDF</b>: $$F_Y(y)=\\begin{cases}
      0, & y<0 \\\\
      1-p, & 0 \\le y < 1 \\\\
      1, & y \\ge 1
    \\end{cases}$$</li>

  <li><b>Mean</b>: $$\\mathbb{E}[Y]=0\\times(1-p)+1\\times p = p$$</li>

  <li><b>Variance</b>: $$\\text{Var}(Y)=\\mathbb{E}[Y^2]-(\\mathbb{E}[Y])^2
      =0^2\\times(1-p)+1^2\\times p - p^2 = p - p^2 = p(1-p)$$</li>
</ul>"
      ,
      binom = "<ul>
  <li><b>PMF</b>:
    $$ f_X(x) = P(X = x) = \\binom{n}{x} \\, p^{x} (1-p)^{\\,n-x}, \\quad x = 0,1,\\ldots,n. $$ 
  </li>

  <li><b>CDF</b>:
    $$ F_X(x) = P(X \\le x) =
      \\begin{cases}
        0, & x < 0, \\\\
        \\displaystyle \\sum_{y=0}^{\\lfloor x \\rfloor} \\binom{n}{y} \\, p^{y} (1-p)^{\\,n-y}, & 0 \\le x < n, \\\\
        1, & x \\ge n. 
      \\end{cases}
    $$
  </li>

  <li><b>Mean</b>:
    $$ \\mathbb{E}[X] = np. $$
  </li>

  <li><b>Variance</b>:
    $$ \\mathrm{Var}(X) = np(1-p) = npq, \\quad \\text{where } q = 1-p. $$
  </li>
</ul>"
      ,
      nbinom = "<ul>
  <li><b>PMF</b>:
    $$ f_X(x) = P(X = x) = \\binom{k+x-1}{x} p^k (1-p)^x, \\quad x=0,1,2,\\ldots $$
  </li>

  <li><b>CDF</b>:
    $$ F_X(x) = P(X \\le x) = \\sum_{i=0}^{x} 
       \\binom{i+k-1}{i} (1-p)^i p^k, \\quad x=0,1,2,\\ldots $$
  </li>
  
  <li><b>Mean</b>:
    $$ \\mathbb{E}[X] = \\frac{k(1-p)}{p} = \\frac{kq}{p}, \\quad q=1-p. $$
  </li>

  <li><b>Variance</b>:
    $$ \\mathrm{Var}(X) = \\frac{k(1-p)}{p^2} = \\frac{kq}{p^2}, \\quad q=1-p. $$
  </li>
</ul>"
      ,
      geom = "<ul>
  <li><b>PMF</b>:
    $$ f_X(x) = P(X = x) = (1-p)^{x} p, \\quad x = 0,1,2,\\ldots $$
  </li>

  <li><b>CDF</b>:
    $$ F_X(x) = P(X \\le x) = 1 - (1-p)^{x+1}, \\quad x = 0,1,2,\\ldots $$
  </li>

  <li><b>Mean</b>:
    $$ \\mathbb{E}[X] = \\frac{1-p}{p} = \\frac{q}{p}, \\quad q = 1-p. $$
  </li>

  <li><b>Variance</b>:
    $$ \\mathrm{Var}(X) = \\frac{1-p}{p^2} = \\frac{q}{p^2}, \\quad q = 1-p. $$
  </li>
</ul>"
      ,
      pois = "<ul>
        <li><b>PMF</b>:
    $$ f_X(x) = P(X = x) = \\dfrac{\\lambda^{x} e^{-\\lambda}}{x!}, 
       \\quad x=0,1,2,\\ldots $$
  </li>

  <li><b>CDF</b>:
    $$ F_X(x) = P(X \\le x) =
      \\sum_{y=0}^{\\lfloor x \\rfloor} \\dfrac{\\lambda^{y} e^{-\\lambda}}{y!}
      = e^{-\\lambda} \\sum_{y=0}^{\\lfloor x \\rfloor} \\dfrac{\\lambda^{y}}{y!}. $$
  </li>

  <li><b>Mean</b>:
    $$ \\mathbb{E}[X] = \\lambda. $$
  </li>

  <li><b>Variance</b>:
    $$ \\mathrm{Var}(X) = \\lambda. $$
  </li>
      </ul>",
      hyper = "<ul>
       <li><b>PMF</b>:
    $$ f_X(x) = P(X = x) = 
      \\dfrac{\\binom{M}{x} \\, \\binom{N-M}{\\,n-x\\,}}{\\binom{N}{n}}, 
      \\quad x = \\max(0, n-N+M),\\ldots,\\min(n,M). $$
  </li>

  <li><b>CDF</b>:
    $$ F_X(x) = P(X \\le x) =
      \\sum_{y=\\max(0, n-N+M)}^{x} 
      \\dfrac{\\binom{M}{y}\\binom{N-M}{n-y}}{\\binom{N}{n}}. $$
  </li>
  
  <li><b>Mean</b>:
    $$ \\mathbb{E}[X] = \\dfrac{nM}{N}. $$
  </li>

  <li><b>Variance</b>:
    $$ \\mathrm{Var}(X) = 
      n\\,\\dfrac{M}{N}\\Bigl(1 - \\dfrac{M}{N}\\Bigr)\\,
      \\dfrac{N-n}{N-1}. $$
  </li>
      </ul>",
      dunifd = "<ul>
        <li><b>PMF</b>:
    $$ f_X(x) = P(X = x) = \\dfrac{1}{b-a+1}, 
       \\quad x = a,a+1,\\ldots,b. $$
  </li>

  <li><b>CDF</b>:
    $$ F_X(x) = P(X \\le x) = 
      \\begin{cases}
        0, & x < a \\\\
        \\dfrac{\\lfloor x \\rfloor - a + 1}{b-a+1}, & a \\le x \\le b \\\\
        1, & x > b
      \\end{cases} $$
  </li>

  <li><b>Mean</b>:
    $$ \\mathbb{E}[X] = \\dfrac{a+b}{2}. $$
  </li>

  <li><b>Variance</b>:
    $$ \\mathrm{Var}(X) = \\dfrac{(b-a+1)^2 - 1}{12}. $$
  </li>
      </ul>"
    )
    withMathJax(HTML(spec[[input$dist]]))
  })
}

shinyApp(ui, server)
