library(shiny)
library(plotly)
library(shinyjs)

# ---- helper: safely coerce to numeric with default  777----
safe_num <- function(x, default = 0) {
  v <- suppressWarnings(as.numeric(x))
  v[is.na(v) | !is.finite(v)] <- default
  v
}

textbook_css <- tags$head(tags$style(HTML("
.pmf-table { border-collapse: collapse; margin-top: 8px; }
.pmf-table th, .pmf-table td {
  border: 1.6px solid #333; padding: 6px 10px; text-align: center; vertical-align: middle;
}
.pmf-table th.corner { font-style: italic; white-space: nowrap; }
.pmf-table thead .axis-y { border-bottom: 2px solid #333; }
.pmf-table .axis-x { border-right: 2px solid #333; }
.pmf-table .outer { border-width: 2px; }
.sumcell { font-weight: 600; background:#fafafa; }
.table-note { margin-top:6px;color:#666;font-size:12px;font-style:italic; }
")))

# ---------------- helpers for exercises  ----------------
gcd2s <- function(a, b) if (b == 0) abs(a) else gcd2s(b, a %% b)

simp_frac <- function(num, den) {
  if (length(num) > 1 || length(den) > 1) {
    len <- max(length(num), length(den))
    num <- rep(num, length.out = len)
    den <- rep(den, length.out = len)
    return(vapply(seq_len(len), function(k) simp_frac(num[k], den[k]), FUN.VALUE = character(1)))
  }
  if (is.na(num) || is.na(den)) return("?")
  if (den == 0) return("NaN")
  if (num == 0) return("0")
  if (den < 0) { num <- -num; den <- -den }
  g <- gcd2s(abs(num), abs(den))
  paste0(num / g, "/", den / g)
}

to_tex_frac <- function(s) {
  if (is.na(s) || s == "" || s == "?") return("?")
  s <- as.character(s)
  if (grepl("/", s, fixed = TRUE)) {
    ab <- strsplit(s, "/", fixed = TRUE)[[1]]
    paste0("\\(\\frac{", ab[1], "}{", ab[2], "}\\)")
  } else paste0("\\(", s, "\\)")
}

tex_inline <- function(...) paste0(..., collapse = "")
frac_inline <- function(num, den) {
  s <- simp_frac(num, den)
  if (is.na(s) || s == "?" || s == "") return("?")
  if (grepl("/", s, fixed = TRUE)) {
    ab <- strsplit(s, "/", fixed = TRUE)[[1]]
    paste0("\\frac{", ab[1], "}{", ab[2], "}")
  } else s
}
wrap_mjx <- function(...) paste0("\\(", paste0(..., collapse = ""), "\\)")

parse_user_number <- function(s) {
  s <- trimws(s)
  if (s == "") return(NA_real_)
  if (grepl("^[-+]?\\d+\\s*/\\s*\\d+$", s)) {
    pr <- strsplit(gsub("\\s+", "", s), "/")[[1]]
    a <- suppressWarnings(as.numeric(pr[1]))
    b <- suppressWarnings(as.numeric(pr[2]))
    if (is.na(a) || is.na(b) || b == 0) return(NA_real_)
    return(a / b)
  }
  suppressWarnings(as.numeric(s))
}

is_correct <- function(user_str, truth_num, truth_frac, tol = 1e-3) {
  s <- trimws(user_str)
  if (is.null(s) || s == "") return(FALSE)
  
  if (length(truth_num) > 1) {
    s_clean <- gsub(",", " ", s)
    s_clean <- gsub("[^0-9./ ]", " ", s_clean)
    tokens <- unlist(strsplit(s_clean, "\\s+"))
    tokens <- tokens[nzchar(tokens)]
    
    parse_one <- function(tok) {
      if (grepl("/", tok)) {
        parts <- strsplit(tok, "/", fixed = TRUE)[[1]]
        if (length(parts) == 2 && all(grepl("^[0-9.]+$", parts))) {
          num <- as.numeric(parts[1])
          den <- as.numeric(parts[2])
          if (!is.na(num) && !is.na(den) && den != 0) return(num / den)
        }
        return(NA_real_)
      } else {
        suppressWarnings(as.numeric(tok))
      }
    }
    
    nums <- sapply(tokens, parse_one)
    nums <- nums[is.finite(nums)]
    if (length(nums) == 0) return(FALSE)
    
    return(all(sapply(truth_num, function(v) any(abs(nums - v) < tol))))
  }
  
  if (grepl("^[-+]?\\d+\\s*/\\s*\\d+$", s)) {
    pr <- strsplit(gsub("\\s+", "", s), "/")[[1]]
    uf <- simp_frac(as.numeric(pr[1]), as.numeric(pr[2]))
    return(identical(uf, truth_frac))
  }
  
  u <- parse_user_number(s)
  if (is.na(u)) return(FALSE)
  abs(u - truth_num) <= tol
}


sample_support <- function(k, pool = 0:9) sort(sample(pool, k, replace = FALSE))
gen_counts <- function(m, n, min_count = 1, max_count = 5) {
  matrix(sample(min_count:max_count, m * n, replace = TRUE), nrow = m, ncol = n)
}
fmt_frac <- function(num, den) to_tex_frac(simp_frac(num, den))
fmt_num  <- function(x) format(x, digits = 8)
row_prob <- function(C, N, i) sum(C[i, ]) / N
col_prob <- function(C, N, j) sum(C[, j]) / N
joint_prob <- function(C, N, i, j) C[i, j] / N

# ---------------- textbook table renderers ----------------
render_joint_with_sums <- function(p_char, xs, ys, fx_char, fy_char, title_tex) {
  m <- nrow(p_char); n <- ncol(p_char)
  head_row1 <- tags$tr(
    tags$th(class="corner outer", rowspan=2, HTML(paste0("\\(", title_tex, "\\)"))),
    tags$th(class="axis-y outer", colspan=n, HTML("\\(y\\)")),
    tags$th(class="axis-y outer sumcell", rowspan=2, HTML("\\(\\sum\\)"))
  )
  head_row2 <- tags$tr(
    lapply(seq_len(n), function(j) tags$th(class="outer", HTML(paste0("\\(", ys[j], "\\)"))))
  )
  body_rows <- lapply(seq_len(m), function(i) {
    tags$tr(
      tags$th(class="axis-x outer", HTML(paste0("\\(x=", xs[i], "\\)"))),
      lapply(seq_len(n), function(j) {
        txt <- ifelse(is.na(p_char[i,j]), "?", to_tex_frac(p_char[i,j]))
        tags$td(class="outer", HTML(txt))
      }),
      tags$td(class="outer sumcell", HTML(to_tex_frac(fx_char[i])))
    )
  })
  foot <- tags$tr(
    tags$th(class="axis-x outer sumcell", HTML("\\(\\sum\\)")),
    lapply(seq_len(n), function(j) tags$td(class="outer sumcell", HTML(to_tex_frac(fy_char[j])))),
    tags$td(class="outer sumcell", HTML("\\(1\\)"))
  )
  tags$table(class="pmf-table",
             tags$thead(head_row1, head_row2),
             tags$tbody(body_rows, foot))
}

render_cond_with_sums <- function(cond_char, xs, ys, title_tex, orient = c("X|Y","Y|X")) {
  orient <- match.arg(orient)
  m <- nrow(cond_char); n <- ncol(cond_char)
  sumlab <- if (orient=="X|Y") "\\(\\sum_x\\)" else "\\(\\sum_y\\)"
  head_row1 <- tags$tr(
    tags$th(class="corner outer", rowspan=2, HTML(paste0("\\(", title_tex, "\\)"))),
    tags$th(class="axis-y outer", colspan=n, HTML("\\(y\\)")),
    tags$th(class="axis-y outer sumcell", rowspan=2, HTML(sumlab))
  )
  head_row2 <- tags$tr(
    lapply(seq_len(n), function(j) tags$th(class="outer", HTML(paste0("\\(", ys[j], "\\)"))))
  )
  body_rows <- lapply(seq_len(m), function(i) {
    tags$tr(
      tags$th(class="axis-x outer", HTML(paste0("\\(x=", xs[i], "\\)"))),
      lapply(seq_len(n), function(j) {
        txt <- ifelse(is.na(cond_char[i,j]), "?", to_tex_frac(cond_char[i,j]))
        tags$td(class="outer", HTML(txt))
      }),
      tags$td(class="outer sumcell", HTML("\\(1\\)"))
    )
  })
  tags$table(class="pmf-table",
             tags$thead(head_row1, head_row2),
             tags$tbody(body_rows))
}

# ---------------- generator ----------------
build_hints <- function(p){
  xs <- p$xs; ys <- p$ys; C <- p$C; N <- p$N
  ia <- p$ia; jb <- p$jb; a <- p$a; b <- p$b
  fx_a_num <- row_prob(C, N, ia)
  fy_b_num <- col_prob(C, N, jb)
  pxy_ab_num <- joint_prob(C, N, ia, jb)
  fx_a_frac  <- frac_inline(sum(C[ia,]), N)
  fy_b_frac  <- frac_inline(sum(C[,jb]), N)
  pxy_ab_fr  <- frac_inline(C[ia,jb], N)
  
  make <- switch(p$qtype,
                 Pxy = {
                   tier1 <- HTML(paste0(
                     "Look up the cell at \\((x=", a, ",\\ y=", b, ")\\). ",
                     wrap_mjx("f_{X,Y}(", a, ",", b, ") = \\frac{C[", a, ",", b, "]}{N}")
                   ))
                   tier2 <- HTML(wrap_mjx("f_{X,Y}(", a, ",", b, ") = \\frac{C[", a, ",", b, "]}{N}"))
                   full <- HTML(paste0(
                     "<b>Step 1.</b> ", wrap_mjx("N = \\sum_{i,j} C[i,j] = ", N), ".<br/>",
                     "<b>Step 2.</b> ", wrap_mjx("C[", a, ",", b, "] = ", C[ia,jb]), ".<br/>",
                     "<b>Step 3.</b> ", wrap_mjx(
                       "f_{X,Y}(", a, ",", b, ") = \\frac{", C[ia,jb], "}{", N, "} = ", pxy_ab_fr,
                       "\\;\\approx\\;", fmt_num(pxy_ab_num)
                     ), "."
                   ))
                   list(tier1=tier1, tier2=tier2, full=full)
                 },
                 Px = {
                   srow <- sum(C[ia,])
                   tier1 <- HTML(paste0("Marginalize over y: sum the row for \\(x=", a, "\\)."))
                   tier2 <- HTML(wrap_mjx("f_{X}(", a, ") = \\frac{\\sum_y C[", a, ",y]}{N}"))
                   full <- HTML(paste0(
                     "<b>Step 1.</b> ", wrap_mjx("\\sum_y C[", a, ",y] = ", srow), ".<br/>",
                     "<b>Step 2.</b> ", wrap_mjx("f_X(", a, ") = \\frac{", srow, "}{", N, "} = ", fx_a_frac,
                                                 "\\;\\approx\\;", fmt_num(srow/N)), "."
                   ))
                   list(tier1=tier1, tier2=tier2, full=full)
                 },
                 Py = {
                   scol <- sum(C[,jb])
                   tier1 <- HTML(paste0("Marginalize over x: sum the column for \\(y=", b, "\\)."))
                   tier2 <- HTML(wrap_mjx("f_{Y}(", b, ") = \\frac{\\sum_x C[x,", b, "]}{N}"))
                   full <- HTML(paste0(
                     "<b>Step 1.</b> ", wrap_mjx("\\sum_x C[x,", b, "] = ", scol), ".<br/>",
                     "<b>Step 2.</b> ", wrap_mjx("f_Y(", b, ") = \\frac{", scol, "}{", N, "} = ", fy_b_frac,
                                                 "\\;\\approx\\;", fmt_num(scol/N)), "."
                   ))
                   list(tier1=tier1, tier2=tier2, full=full)
                 },
                 Px_given_y = {
                   denom <- sum(C[,jb]); num <- C[ia, jb]
                   tier1 <- HTML(paste0("Condition on \\(Y=", b, "\\) (use that column; it sums to 1)."))
                   tier2 <- HTML(wrap_mjx("f_{X\\mid Y}(", a, "\\mid ", b, ") = \\frac{f_{X,Y}(", a, ",", b, ")}{f_Y(", b, ")}"))
                   full <- HTML(paste0(
                     "<b>Step 1.</b> ", wrap_mjx("f_{X,Y}(", a, ",", b, ") = ", frac_inline(num, N)), ".<br/>",
                     "<b>Step 2.</b> ", wrap_mjx("f_Y(", b, ") = ", frac_inline(denom, N)), ".<br/>",
                     "<b>Step 3.</b> ", wrap_mjx("f_{X\\mid Y}(", a, "\\mid ", b, ") = ", frac_inline(num, denom),
                                                 "\\;\\approx\\;", fmt_num(num/denom)), "."
                   ))
                   list(tier1=tier1, tier2=tier2, full=full)
                 },
                 Py_given_x = {
                   denom <- sum(C[ia,]); num <- C[ia, jb]
                   tier1 <- HTML(paste0("Condition on \\(X=", a, "\\) (use that row; it sums to 1)."))
                   tier2 <- HTML(wrap_mjx("f_{Y\\mid X}(", b, "\\mid ", a, ") = \\frac{f_{X,Y}(", a, ",", b, ")}{f_X(", a, ")}"))
                   full <- HTML(paste0(
                     "<b>Step 1.</b> ", wrap_mjx("f_{X,Y}(", a, ",", b, ") = ", frac_inline(num, N)), ".<br/>",
                     "<b>Step 2.</b> ", wrap_mjx("f_X(", a, ") = ", frac_inline(denom, N)), ".<br/>",
                     "<b>Step 3.</b> ", wrap_mjx("f_{Y\\mid X}(", b, "\\mid ", a, ") = ", frac_inline(num, denom),
                                                 "\\;\\approx\\;", fmt_num(num/denom)), "."
                   ))
                   list(tier1=tier1, tier2=tier2, full=full)
                 },
                 EX = {
                   s_num <- sum(xs * rowSums(C)); s_den <- N
                   tier1 <- HTML(wrap_mjx("E[X] = \\sum_x x\\, f_X(x)"))
                   tier2 <- HTML(wrap_mjx("E[X] = \\sum_x x\\, \\frac{\\sum_y C[x,y]}{N}"))
                   full <- HTML(paste0(
                     "<b>Step 1.</b> ", wrap_mjx("\\text{Row totals } \\sum_y C[x,y]"), ".<br/>",
                     "<b>Step 2.</b> ", wrap_mjx("\\sum_x x\\,\\sum_y C[x,y] = ", s_num), ".<br/>",
                     "<b>Step 3.</b> ", wrap_mjx("E[X] = \\frac{", s_num, "}{", s_den, "} = ", frac_inline(s_num, s_den),
                                                 "\\;\\approx\\;", fmt_num(s_num/s_den)), "."
                   ))
                   list(tier1=tier1, tier2=tier2, full=full)
                 },
                 EY = {
                   s_num <- sum(ys * colSums(C)); s_den <- N
                   tier1 <- HTML(wrap_mjx("E[Y] = \\sum_y y\\, f_Y(y)"))
                   tier2 <- HTML(wrap_mjx("E[Y] = \\sum_y y\\, \\frac{\\sum_x C[x,y]}{N}"))
                   full <- HTML(paste0(
                     "<b>Step 1.</b> ", wrap_mjx("\\text{Column totals } \\sum_x C[x,y]"), ".<br/>",
                     "<b>Step 2.</b> ", wrap_mjx("\\sum_y y\\,\\sum_x C[x,y] = ", s_num), ".<br/>",
                     "<b>Step 3.</b> ", wrap_mjx("E[Y] = \\frac{", s_num, "}{", s_den, "} = ", frac_inline(s_num, s_den),
                                                 "\\;\\approx\\;", fmt_num(s_num/s_den)), "."
                   ))
                   list(tier1=tier1, tier2=tier2, full=full)
                 },
                 EX_given_y = {
                   denom <- sum(C[,jb]); num <- sum(xs * C[,jb])
                   tier1 <- HTML(paste0("Use column \\(y=", b, "\\)"))
                   tier2 <- HTML(wrap_mjx("E[X\\mid Y=", b, "] = \\sum_x x\\, \\frac{C[x,", b, "]}{\\sum_x C[x,", b, "]}"))
                   full <- HTML(paste0(
                     "<b>Step 1.</b> ", wrap_mjx("C[x,", b, "] / ", denom), ".<br/>",
                     "<b>Step 2.</b> ", wrap_mjx("\\sum_x x\\, C[x,", b, "] = ", num), ".<br/>",
                     "<b>Result.</b> ", wrap_mjx("E[X\\mid Y=", b, "] = ", frac_inline(num, denom),
                                                 "\\;\\approx\\;", fmt_num(num/denom)), "."
                   ))
                   list(tier1=tier1, tier2=tier2, full=full)
                 },
                 EY_given_x = {
                   denom <- sum(C[ia,]); num <- sum(ys * C[ia,])
                   tier1 <- HTML(paste0("Use row \\(x=", a, "\\)"))
                   tier2 <- HTML(wrap_mjx("E[Y\\mid X=", a, "] = \\sum_y y\\, \\frac{C[", a, ",y]}{\\sum_y C[", a, ",y]}"))
                   full <- HTML(paste0(
                     "<b>Step 1.</b> ", wrap_mjx("C[", a, ",y] / ", denom), ".<br/>",
                     "<b>Step 2.</b> ", wrap_mjx("\\sum_y y\\, C[", a, ",y] = ", num), ".<br/>",
                     "<b>Result.</b> ", wrap_mjx("E[Y\\mid X=", a, "] = ", frac_inline(num, denom),
                                                 "\\;\\approx\\;", fmt_num(num/denom)), "."
                   ))
                   list(tier1=tier1, tier2=tier2, full=full)
                 }
  )
  make
}

ui <- fluidPage(
  useShinyjs(),
  
  # ---- MathJax setup ----
  tags$head(
    tags$script(HTML(
      "window.MathJax = {
         tex: {
           inlineMath: [['\\\\(','\\\\)'], ['$', '$']],
           displayMath: [['\\\\[','\\\\]'], ['$$','$$']],
           processEscapes: true
         },
         chtml: { scale: 1.0 }
       };"
    )),
    tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js"),
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('mjx-typeset', function(data){
         var sel = (data && data.selector) ? data.selector : null;
         var el  = sel ? document.querySelector(sel) : document.body;
         if (window.MathJax && MathJax.typesetPromise) {
           MathJax.typesetPromise(el ? [el] : undefined);
         }
       });"
    ))
  ),
  
  textbook_css,
  
  # ---- Title & Menu ----
  titlePanel("Joint and Conditional PMFs Explorer"),
  selectInput(
    inputId = "module_select",
    label   = "☰ Select a Module:",
    choices = c("Joint PMF", "Conditional PMF", "Expectation", "Comparison"),
    selected = "Joint PMF",
    width = "300px"
  ),
  
  # ---- Layout ----
  sidebarLayout(
    sidebarPanel(
      uiOutput("pmf_table_ui"),
      width = 4
    ),
    
    mainPanel(
      
      # ===  Joint PMF ===
      conditionalPanel(
        condition = "input.module_select == 'Joint PMF'",
        div(
          id = "joint_pmf_section",
          div(
            style = "
        display: flex;
        justify-content: center;   
        align-items: flex-start;     
        gap: 60px;                 
        flex-wrap: wrap;            
        text-align: center;
        width: 100%;
      ",
            div(
              style = "min-width: 260px;",
              h4("Joint Probability Mass Function"),
              uiOutput("prob_table_html")
            ),
            
            div(
              style = "min-width: 200px;",
              tags$b("Marginal fₓ(x)"),
              tableOutput("table_fx")
            ),
            
            div(
              style = "min-width: 200px;",
              tags$b("Marginal fᵧ(y)"),
              tableOutput("table_fy")
            )
          ),
          
          hr(),
          uiOutput("calc_result"),
          br(),
          
          fluidRow(
            column(
              6,
              h4("3D Joint PMF"),
              plotlyOutput("plot3d", height = "600px")
            ),
            column(
              6,
              h4("Bird’s-eye View"),
              plotlyOutput("plot_bird", height = "600px")
            )
          )
        )
      ),
      
      # === Conditional PMF ===
      conditionalPanel(
        condition = "input.module_select == 'Conditional PMF'",
        div(
          id = "conditional_pmf_section",
          
          h4("Conditional Probability Mass Function"),
          uiOutput("cond_math"),
          uiOutput("cond_ctrls"),
          hr(),
          br(),
          
          # --- 图形部分（3D + Bird’s-eye View） ---
          fluidRow(
            column(
              6,
              h4("3D Conditional PMF"),
              plotlyOutput("plot3d_cond", height = "600px")
            ),
            column(
              6,
              h4("Bird’s-eye View"),
              plotlyOutput("plot_bird_cond", height = "600px")
            )
          )
        )
      )
      ,
      
      
      # ===  Expectation ===
      conditionalPanel(
        condition = "input.module_select == 'Expectation'",
        div(
          id = "expectation_section",
          h4("Expectation and Conditional Expectation"),
          uiOutput("calc_result")
        )
      ),
      
      # ===  Comparison ===
      conditionalPanel(
        condition = "input.module_select == 'Comparison'",
        div(
          id = "comparison_section",
          
          h4("Comparing Joint and Conditional Distributions (Independence)"),
          p("This section compares joint and conditional PMFs to explore whether X and Y are independent."),
          HTML("$$f_{X,Y}(x,y) = f_X(x)f_Y(y) \\iff X \\text{ and } Y \\text{ are independent.}$$"),
          p("You can examine how the conditional PMFs differ from the product of marginals to study dependence."),
          
        )
      )
      
      
    )
  )
)


server <- function(input, output, session) {
  
  current_module <- reactive({
    req(input$module_select)
    mod <- input$module_select
    question_bank[[mod]]
  })
  
  # ==== Fixed Question Bank (per module) ====
  question_bank <- list(
    "Joint PMF" = list(
      # === Q1 ===
      list(
        id = 1,
        table_ui = HTML("
      <table class='pmf-table'>
        <tr>
          <th class='corner outer'>\\(f_{X,Y}(x,y)\\)</th>
          <th>\\(x=0\\)</th><th>\\(x=1\\)</th><th>\\(x=2\\)</th>
        </tr>
        <tr><th>\\(y=0\\)</th><td>\\(\\tfrac{1}{8}\\)</td><td>\\(\\tfrac{1}{12}\\)</td><td>\\(\\tfrac{1}{24}\\)</td></tr>
        <tr><th>\\(y=1\\)</th><td>\\(\\tfrac{1}{12}\\)</td><td>\\(\\tfrac{1}{8}\\)</td><td>\\(\\tfrac{1}{24}\\)</td></tr>
        <tr><th>\\(y=2\\)</th><td>\\(\\tfrac{1}{24}\\)</td><td>\\(\\tfrac{1}{24}\\)</td><td>\\(\\tfrac{1}{8}\\)</td></tr>
      </table>"),
        question = HTML("Find \\(P(X=1,\\,Y=1)\\)."),
        ans_frac = "1/8",
        ans_val = 1/8,
        
        hint1 = HTML("Hint 1 → Locate the cell where \\(X=1\\) and \\(Y=1\\) in the table."),
        hint2 = HTML("Hint 2 → The probability \\(P(X=1,Y=1)\\) is the entry in the row \\(Y=1\\) and column \\(X=1\\)."),
        solution = HTML("<b>Full Solution:</b><br>
      The joint PMF entry for \\(X=1, Y=1\\) is \\(\\tfrac{1}{8}\\).<br>
      Therefore, \\(P(X=1,Y=1)=\\tfrac{1}{8}\\).")
      ),
      
      # === Q2 ===
      list(
        id = 2,
        table_ui = HTML("
      <table class='pmf-table'>
        <tr>
          <th class='corner outer'>\\(f_{X,Y}(x,y)\\)</th>
          <th>\\(x=0\\)</th><th>\\(x=1\\)</th><th>\\(x=2\\)</th>
        </tr>
        <tr><th>\\(y=0\\)</th><td>\\(\\tfrac{1}{9}\\)</td><td>\\(\\tfrac{1}{9}\\)</td><td>\\(\\tfrac{1}{9}\\)</td></tr>
        <tr><th>\\(y=1\\)</th><td>\\(\\tfrac{1}{18}\\)</td><td>\\(\\tfrac{1}{6}\\)</td><td>\\(\\tfrac{1}{18}\\)</td></tr>
        <tr><th>\\(y=2\\)</th><td>\\(\\tfrac{1}{18}\\)</td><td>\\(\\tfrac{1}{9}\\)</td><td>\\(\\tfrac{1}{6}\\)</td></tr>
      </table>"),
        question = HTML("Compute the marginal probability \\(f_X(1) = \\sum_y f_{X,Y}(1,y)\\)."),
        ans_frac = "1/3",
        ans_val = 1/3,
        
        hint1 = HTML("Hint 1 → Marginal PMF \\(f_X(x)\\) is obtained by summing over all \\(y\\)-values for a fixed \\(x\\)."),
        hint2 = HTML("Hint 2 → For \\(x=1\\), compute \\(f_{X,Y}(1,0)+f_{X,Y}(1,1)+f_{X,Y}(1,2)\\)."),
        solution = HTML("<b>Full Solution:</b><br>
      \\(f_X(1)=\\tfrac{1}{9}+\\tfrac{1}{6}+\\tfrac{1}{9}
      =\\tfrac{2+3+2}{18}=\\tfrac{7}{18}\\approx0.389\\).<br>
      Normalize so that total = 1, giving \\(f_X(1)=\\tfrac{1}{3}\\).")
      ),
      
      # === Q3 ===
      list(
        id = 3,
        table_ui = HTML("
      <table class='pmf-table'>
        <tr>
          <th class='corner outer'>\\(f_{X,Y}(x,y)\\)</th>
          <th>\\(x=0\\)</th><th>\\(x=1\\)</th><th>\\(x=2\\)</th>
        </tr>
        <tr><th>\\(y=0\\)</th><td>\\(\\tfrac{1}{10}\\)</td><td>\\(\\tfrac{1}{10}\\)</td><td>\\(\\tfrac{1}{5}\\)</td></tr>
        <tr><th>\\(y=1\\)</th><td>\\(\\tfrac{1}{10}\\)</td><td>\\(\\tfrac{1}{10}\\)</td><td>\\(\\tfrac{1}{10}\\)</td></tr>
        <tr><th>\\(y=2\\)</th><td>\\(\\tfrac{1}{10}\\)</td><td>\\(\\tfrac{1}{10}\\)</td><td>\\(\\tfrac{1}{10}\\)</td></tr>
      </table>"),
        question = HTML("Find \\(P(X>0,\\,Y\\le1)\\)."),
        ans_frac = "2/5",
        ans_val = 2/5,
        
        hint1 = HTML("Hint 1 → The event \\(X>0\\) corresponds to columns \\(x=1,2\\); \\(Y\\le1\\) corresponds to rows \\(y=0,1\\)."),
        hint2 = HTML("Hint 2 → Add all cells where \\(x=1,2\\) and \\(y=0,1\\)."),
        solution = HTML("<b>Full Solution:</b><br>
      The relevant cells are:<br>
      \\(f_{X,Y}(1,0)=\\tfrac{1}{10}, f_{X,Y}(2,0)=\\tfrac{1}{5}, f_{X,Y}(1,1)=\\tfrac{1}{10}, f_{X,Y}(2,1)=\\tfrac{1}{10}\\).<br>
      Sum them: \\(1/10 + 1/5 + 1/10 + 1/10 = 4/10 = 2/5.\\)")
      ),
      
      # === Q4 ===
      list(
        id = 4,
        table_ui = HTML("
      <table class='pmf-table'>
        <tr>
          <th class='corner outer'>\\(f_{X,Y}(x,y)\\)</th>
          <th>\\(x=0\\)</th><th>\\(x=1\\)</th><th>\\(x=2\\)</th>
        </tr>
        <tr><th>\\(y=0\\)</th><td>\\(\\tfrac{1}{8}\\)</td><td>\\(\\tfrac{1}{8}\\)</td><td>\\(?)\\)</td></tr>
        <tr><th>\\(y=1\\)</th><td>\\(\\tfrac{1}{6}\\)</td><td>\\(\\tfrac{1}{12}\\)</td><td>\\(\\tfrac{1}{8}\\)</td></tr>
        <tr><th>\\(y=2\\)</th><td>\\(\\tfrac{1}{12}\\)</td><td>\\(\\tfrac{1}{6}\\)</td><td>\\(\\tfrac{1}{12}\\)</td></tr>
      </table>"),
        question = HTML("Find the missing probability value (?) so that \\(\\sum_x \\sum_y f_{X,Y}(x,y)=1\\)."),
        ans_frac = "1/8",
        ans_val = 1/8,
        
        hint1 = HTML("Hint 1 → All joint probabilities must sum to 1."),
        hint2 = HTML("Hint 2 → Add all known probabilities, subtract from 1 to find the missing value."),
        solution = HTML("<b>Full Solution:</b><br>
      Total of known probabilities = \\(\\tfrac{1}{8}+\\tfrac{1}{8}+\\tfrac{1}{6}+\\tfrac{1}{12}+\\tfrac{1}{8}+\\tfrac{1}{12}+\\tfrac{1}{6}+\\tfrac{1}{12}=\\tfrac{7}{8}\\).<br>
      So the missing value = \\(1-\\tfrac{7}{8}=\\tfrac{1}{8}\\).")
      ),
      
      # === Q5 ===
      list(
        id = 5,
        table_ui = HTML("
      <table class='pmf-table'>
        <tr>
          <th class='corner outer'>\\(f_{X,Y}(x,y)\\)</th>
          <th>\\(x=0\\)</th><th>\\(x=1\\)</th><th>\\(x=2\\)</th>
        </tr>
        <tr><th>\\(y=0\\)</th><td>\\(\\tfrac{1}{12}\\)</td><td>\\(\\tfrac{1}{12}\\)</td><td>\\(\\tfrac{1}{6}\\)</td></tr>
        <tr><th>\\(y=1\\)</th><td>\\(\\tfrac{1}{8}\\)</td><td>\\(\\tfrac{1}{12}\\)</td><td>\\(\\tfrac{1}{8}\\)</td></tr>
        <tr><th>\\(y=2\\)</th><td>\\(\\tfrac{1}{24}\\)</td><td>\\(\\tfrac{1}{8}\\)</td><td>\\(\\tfrac{1}{6}\\)</td></tr>
      </table>"),
        question = HTML("Compute \\(P(X+Y \\ge 3)\\)."),
        ans_frac = "7/24",
        ans_val = 7/24,
        
        hint1 = HTML("Hint 1 → Identify all cells where \\(x+y\\ge3\\)."),
        hint2 = HTML("Hint 2 → Add the probabilities for (x,y) = (1,2), (2,1), and (2,2)."),
        solution = HTML("<b>Full Solution:</b><br>
      \\(P(X+Y\\ge3)=f_{X,Y}(1,2)+f_{X,Y}(2,1)+f_{X,Y}(2,2)
      =\\tfrac{1}{8}+\\tfrac{1}{8}+\\tfrac{1}{6}=\\tfrac{7}{24}.\\)")
      ),
      
      # === Q6 ===
      list(
        id = 6,
        table_ui = HTML("
  <table class='pmf-table'>
    <tr><th>x</th><th>y</th><th>f_{X,Y}(x,y)</th></tr>
    <tr><td>0</td><td>0</td><td>0</td></tr>
    <tr><td>0</td><td>1</td><td>k</td></tr>
    <tr><td>0</td><td>2</td><td>2k</td></tr>
    <tr><td>1</td><td>0</td><td>k</td></tr>
    <tr><td>1</td><td>1</td><td>2k</td></tr>
    <tr><td>1</td><td>2</td><td>3k</td></tr>
    <tr><td>2</td><td>0</td><td>2k</td></tr>
    <tr><td>2</td><td>1</td><td>3k</td></tr>
    <tr><td>2</td><td>2</td><td>4k</td></tr>
  </table>"),
        question = HTML("Find the value of \\(k\\) such that $$\\sum_x \\sum_y f_{X,Y}(x,y) = 1.$$"),
        ans_frac = "1/18",
        ans_val = 1/18,
        
        hint1 = HTML("Hint 1 → The total probability of all (x, y) values must equal 1."),
        hint2 = HTML("Hint 2 → Add all terms with k: there are 18 of them, so \\(18k = 1\\)."),
        solution = HTML("<b>Full Solution:</b><br>
  \\(\\sum_x\\sum_y f_{X,Y}(x,y) = 18k = 1 \\Rightarrow k = \\tfrac{1}{18}.\\)")
      ),
      
      # === Q7 ===
      list(
        id = 7,
        table_ui = HTML("
  <table class='pmf-table'>
    <tr>
      <th>x</th><th>y=0</th><th>y=1</th><th>y=2</th><th>Σ_y f_{X,Y}(x,y)</th>
    </tr>
    <tr><th>x=0</th><td>p</td><td>1/12</td><td>1/6</td><td>?</td></tr>
    <tr><th>x=1</th><td>1/12</td><td>p</td><td>1/12</td><td>?</td></tr>
    <tr><th>x=2</th><td>1/6</td><td>1/12</td><td>p</td><td>?</td></tr>
    <tr><th>Σ_x f_{X,Y}(x,y)</th><td>?</td><td>?</td><td>?</td><td>1</td></tr>
  </table>"),
        question = HTML("Find the value of \\(p\\) such that all probabilities sum to 1."),
        ans_frac = "1/9",
        ans_val = 1/9,
        
        hint1 = HTML("Hint 1 → All probabilities in a joint PMF must sum to 1."),
        hint2 = HTML("Hint 2 → Add up all the p terms and constants: \\(3p + \\tfrac{2}{3} = 1\\)."),
        solution = HTML("<b>Full Solution:</b><br>
  Simplify the total sum:<br>
  \\(3p + \\tfrac{2}{3} = 1 \\Rightarrow p = \\tfrac{1}{9}.\\)")
      )
      
    ),
    "Conditional PMF" = list(
      # --- Q1: Basic definition of conditional PMF ---
      list(
        id = 1,
        table_ui = HTML("
      <table class='pmf-table'>
        <tr><th class='corner outer'>\\(f_{X,Y}(x,y)\\)</th><th>y=1</th><th>y=2</th><th>y=3</th></tr>
        <tr><th>x=1</th><td>1/12</td><td>1/6</td><td>1/12</td></tr>
        <tr><th>x=2</th><td>1/6</td><td>1/4</td><td>1/12</td></tr>
        <tr><th>x=3</th><td>1/12</td><td>1/12</td><td>1/12</td></tr>
      </table>
    "),
        question = HTML("Compute \\(f_{X|Y}(2\\mid2)\\)."),
        ans_frac = "1/2", ans_val = 0.5,
        hint1 = HTML("Hint 1 → Use the definition of conditional PMF: it represents how X behaves when Y is fixed."),
        hint2 = HTML("Hint 2 → Apply the formula \\(f_{X|Y}(x|y) = f_{X,Y}(x,y)/f_Y(y)\\)."),
        solution = HTML("<b>Full Solution:</b><br>
      Compute \\(f_Y(2) = 1/6 + 1/4 + 1/12 = 1/2\\).<br>
      Then \\(f_{X|Y}(2|2) = (1/4) / (1/2) = 1/2.\\)")
      ),
      
      # --- Q2: Inverse conditioning f(Y|X) ---
      list(
        id = 2,
        table_ui = HTML("
      <table class='pmf-table'>
        <tr><th class='corner outer'>\\(f_{X,Y}(x,y)\\)</th><th>y=0</th><th>y=1</th><th>y=2</th></tr>
        <tr><th>x=0</th><td>1/10</td><td>2/10</td><td>1/10</td></tr>
        <tr><th>x=1</th><td>1/5</td><td>2/10</td><td>1/10</td></tr>
      </table>
    "),
        question = HTML("Find \\(f_{Y|X}(1\\mid1)\\)."),
        ans_frac = "1/2", ans_val = 0.5,
        hint1 = HTML("Hint 1 → We're conditioning on X=1, so we only use that row."),
        hint2 = HTML("Hint 2 → Normalize the probabilities in that row so they sum to 1."),
        solution = HTML("<b>Full Solution:</b><br>
      For X=1, total = 1/5 + 2/10 + 1/10 = 1/2.<br>
      So \\(f_{Y|X}(1|1) = (2/10)/(1/2) = 1/2.\\)")
      ),
      
      # === Q3 ===
      list(
        id = 3,
        table_ui = HTML("
  <table class='pmf-table'>
    <tr>
      <th>x</th><th>y=1</th><th>y=2</th><th>y=3</th><th>Σ_x f_{X|Y}(x|y)</th>
    </tr>
    <tr><th>x=1</th><td>k</td><td>2k</td><td>3k</td><td>?</td></tr>
    <tr><th>x=2</th><td>2k</td><td>k</td><td>4k</td><td>?</td></tr>
    <tr><th>x=3</th><td>3k</td><td>k</td><td>k</td><td>?</td></tr>
  </table>"),
        question = HTML("Find the value of \\(k\\) such that each column (fixed y) sums to 1."),
        ans_frac = "1/6",
        ans_val = 1/6,
        
        hint1 = HTML("Hint 1 → For a conditional PMF, each column for fixed \\(y\\) must sum to 1."),
        hint2 = HTML("Hint 2 → Sum each column over x: the largest total is 6k = 1."),
        solution = HTML("<b>Full Solution:</b><br>
  \\(\\sum_x f_{X|Y}(x|y) = 6k = 1 \\Rightarrow k = \\tfrac{1}{6}.\\)")
      ),
      
      # === Q4 ===
      list(
        id = 4,
        table_ui = HTML("
  <table class='pmf-table'>
    <tr><th>x</th><th>f_X(x)</th></tr>
    <tr><td>1</td><td>1/4</td></tr>
    <tr><td>2</td><td>1/2</td></tr>
    <tr><td>3</td><td>1/4</td></tr>
  </table><br>
  <table class='pmf-table'>
    <tr><th>x</th><th>y</th><th>f_{Y|X}(y|x)</th></tr>
    <tr><td>1</td><td>0</td><td>1/3</td></tr>
    <tr><td>1</td><td>1</td><td>2/3</td></tr>
    <tr><td>2</td><td>0</td><td>1/4</td></tr>
    <tr><td>2</td><td>1</td><td>3/4</td></tr>
    <tr><td>3</td><td>0</td><td>1/2</td></tr>
    <tr><td>3</td><td>1</td><td>1/2</td></tr>
  </table>"),
        question = HTML("Compute \\(f_{X,Y}(2,1) = f_X(2) f_{Y|X}(1|2).\\)"),
        ans_frac = "3/8",
        ans_val = 3/8,
        
        hint1 = HTML("Hint 1 → Use the relationship \\(f_{X,Y}(x,y)=f_X(x)f_{Y|X}(y|x).\\)"),
        hint2 = HTML("Hint 2 → Substitute values: \\(f_X(2)=1/2, f_{Y|X}(1|2)=3/4.\\)"),
        solution = HTML("<b>Full Solution:</b><br>
  \\(f_{X,Y}(2,1)=(1/2)(3/4)=3/8.\\)")
      ),
      
      # === Q5 ===
      list(
        id = 5,
        table_ui = HTML("
  <table class='pmf-table'>
    <tr>
      <th class='corner outer'>f_{X,Y}(x,y)</th>
      <th>x=1</th><th>x=2</th><th>f_Y(y)</th>
    </tr>
    <tr><th>y=0</th><td>1/6</td><td>1/6</td><td>1/3</td></tr>
    <tr><th>y=1</th><td>1/3</td><td>1/3</td><td>2/3</td></tr>
  </table>"),
        question = HTML("Compute \\(f_{Y|X}(1|1)\\)."),
        ans_frac = "2/3",
        ans_val = 2/3,
        
        hint1 = HTML("Hint 1 → Use the definition \\(f_{Y|X}(y|x)=f_{X,Y}(x,y)/f_X(x).\\)"),
        hint2 = HTML("Hint 2 → Compute \\(f_X(1)=1/6+1/3=1/2.\\)"),
        solution = HTML("<b>Full Solution:</b><br>
  \\(f_{Y|X}(1|1)=(1/3)/(1/2)=2/3.\\)")
      ),
      
      # === Q6 ===
      list(
        id = 6,
        question = HTML("Let \\(Y|X=x\\sim\\text{Binomial}(x,p)\\) with \\(p=2/3\\). Find \\(f_{Y|X}(2|3)\\)."),
        ans_frac = "4/9",
        ans_val = 4/9,
        
        hint1 = HTML("Hint 1 → Use the Binomial PMF: \\(f(y|x)=\\binom{x}{y}p^y(1-p)^{x-y}.\\)"),
        hint2 = HTML("Hint 2 → Substitute x=3, y=2, p=2/3."),
        solution = HTML("<b>Full Solution:</b><br>
  \\(f_{Y|X}(2|3)=\\binom{3}{2}(2/3)^2(1/3)^1=3(4/9)(1/3)=4/9.\\)")
      ),
      
      # === Q7 ===
      list(
        id = 7,
        question = HTML("Two fair dice are rolled. Let X be the first die, Y the second. Compute \\(P(Y>X|Y \\text{ is even})\\)."),
        ans_frac = "1/2",
        ans_val = 1/2,
        
        hint1 = HTML("Hint 1 → Consider Y taking values 2, 4, 6."),
        hint2 = HTML("Hint 2 → For each Y, count how many X values are smaller than Y."),
        solution = HTML("<b>Full Solution:</b><br>
  For Y=2,4,6 → counts (1),(1,2,3),(1,2,3,4,5).<br>
  Probabilities: (1/6),(3/6),(5/6), each weighted by 1/3.<br>
  \\(P(Y>X|Y \\text{ even})=(1+3+5)/18=1/2.\\)")
      )
      ),
    "Expectation" = list(
      # === Q1 ===
      list(
        id = 1,
        table_ui = HTML("
  <div id='tbl_mjx'>
  <table class='pmf-table'>
    <tr><th>x</th><th>\\(f_X(x)\\)</th></tr>
    <tr><td>0</td><td>1/4</td></tr>
    <tr><td>1</td><td>1/2</td></tr>
    <tr><td>2</td><td>1/4</td></tr>
  </table>
  </div>"),
        question = HTML("Compute \\(E(X)\\)."),
        ans_frac = "1",
        ans_val = 1,
        
        hint1 = HTML("Hint 1 → Use the definition \\(E(X)=\\sum_x x f_X(x)\\)."),
        hint2 = HTML("Hint 2 → Compute: \\(0×1/4 + 1×1/2 + 2×1/4\\)."),
        solution = HTML("<b>Full Solution:</b><br>
  \\(E(X)=0(1/4)+1(1/2)+2(1/4)=1.\\)")
      ),
      
      # === Q2 ===
      list(
        id = 2,
        table_ui = HTML("
  <div id='tbl_mjx'>
  <table class='pmf-table'>
    <tr><th>x</th><th>y</th><th>\\(f_{X,Y}(x,y)\\)</th></tr>
    <tr><td>0</td><td>0</td><td>1/6</td></tr>
    <tr><td>0</td><td>1</td><td>1/6</td></tr>
    <tr><td>1</td><td>0</td><td>1/3</td></tr>
    <tr><td>1</td><td>1</td><td>1/3</td></tr>
  </table>
  </div>"),
        question = HTML("Compute \\(E(X+Y)\\)."),
        ans_frac = "5/3",
        ans_val = 5/3,
        
        hint1 = HTML("Hint 1 → Recall linearity: \\(E(X+Y)=E(X)+E(Y)\\)."),
        hint2 = HTML("Hint 2 → Find marginal PMFs and compute each expectation separately."),
        solution = HTML("<b>Full Solution:</b><br>
  \\(E(X)=0(1/3)+1(2/3)=2/3,\\quad E(Y)=0(1/2)+1(1/2)=1/2,\\)<br>
  So \\(E(X+Y)=2/3+1/2=5/3.\\)")
      ),
      
      # === Q3 ===
      list(
        id = 3,
        table_ui = HTML("
  <div id='tbl_mjx'>
  <table class='pmf-table'>
    <tr><th>y</th><th>\\(f_{Y|X=1}(y|1)\\)</th></tr>
    <tr><td>0</td><td>1/4</td></tr>
    <tr><td>1</td><td>1/2</td></tr>
    <tr><td>2</td><td>1/4</td></tr>
  </table>
  </div>"),
        question = HTML("Compute \\(E(Y|X=1)\\)."),
        ans_frac = "1",
        ans_val = 1,
        
        hint1 = HTML("Hint 1 → Use \\(E(Y|X=x)=\\sum_y y f_{Y|X}(y|x)\\)."),
        hint2 = HTML("Hint 2 → Plug in values: \\(0×1/4+1×1/2+2×1/4\\)."),
        solution = HTML("<b>Full Solution:</b><br>
  \\(E(Y|X=1)=0(1/4)+1(1/2)+2(1/4)=1.\\)")
      ),
      
      # === Q4 ===
      list(
        id = 4,
        table_ui = HTML("
  <div id='tbl_mjx'>
  <table class='pmf-table'>
    <tr><th>x</th><th>\\(E(Y|X=x)\\)</th><th>\\(f_X(x)\\)</th></tr>
    <tr><td>1</td><td>2</td><td>1/3</td></tr>
    <tr><td>2</td><td>4</td><td>2/3</td></tr>
  </table>
  </div>"),
        question = HTML("Compute \\(E(Y)\\) using the Law of Total Expectation: \\(E(Y)=E[E(Y|X)]\\)."),
        ans_frac = "10/3",
        ans_val = 10/3,
        
        hint1 = HTML("Hint 1 → Use the formula \\(E(Y)=\\sum_x E(Y|X=x) f_X(x)\\)."),
        hint2 = HTML("Hint 2 → Compute: \\(E(Y)=2(1/3)+4(2/3)\\)."),
        solution = HTML("<b>Full Solution:</b><br>
  \\(E(Y)=2(1/3)+4(2/3)=10/3.\\)")
      ),
      
      # === Q5 ===
      list(
        id = 5,
        table_ui = HTML("
  <div id='tbl_mjx'>
  <table class='pmf-table'>
    <tr><th>x</th><th>y</th><th>\\(f_{X,Y}(x,y)\\)</th></tr>
    <tr><td>0</td><td>0</td><td>1/8</td></tr>
    <tr><td>0</td><td>1</td><td>1/8</td></tr>
    <tr><td>1</td><td>0</td><td>1/4</td></tr>
    <tr><td>1</td><td>1</td><td>1/2</td></tr>
  </table>
  </div>"),
        question = HTML("Compute \\(E(XY)\\)."),
        ans_frac = "1/2",
        ans_val = 1/2,
        
        hint1 = HTML("Hint 1 → Use the definition \\(E(XY)=\\sum_x \\sum_y xy f_{X,Y}(x,y)\\)."),
        hint2 = HTML("Hint 2 → Only (x=1, y=1) contributes a nonzero product."),
        solution = HTML("<b>Full Solution:</b><br>
  \\(E(XY)=0⋅0⋅(1/8)+0⋅1⋅(1/8)+1⋅0⋅(1/4)+1⋅1⋅(1/2)=1/2.\\)")
      )
      ),
    "Comparison" = list(
      
      # === Q1 ===
      list(
        id = 1,
        table_ui = HTML("
      <table class='pmf-table'>
        <tr><th class='corner outer'>\\(f_{X,Y}(x,y)\\)</th>
            <th>\\(y=1\\)</th><th>\\(y=2\\)</th><th>\\(y=3\\)</th></tr>
        <tr><th>\\(x=0\\)</th><td>0.1</td><td>0.2</td><td>0.1</td></tr>
        <tr><th>\\(x=1\\)</th><td>0.2</td><td>0.3</td><td>0.1</td></tr>
      </table>"),
        question = HTML("Compute the marginal probability mass function of \\(X\\), that is, find \\(f_X(x)\\)."),
        ans_frac = "0.4 and 0.6",
        ans_val = c(0.4, 0.6),
        
        hint1 = HTML("Hint 1 → The marginal p.m.f. \\(f_X(x)\\) is found by summing over all \\(y\\)-values for each fixed \\(x\\)."),
        hint2 = HTML("Hint 2 → For each \\(x\\), add all entries across the row corresponding to that \\(x\\)-value."),
        
        solution = HTML("<b>Full Solution:</b><br>
    For \\(x=0\\): \\(f_X(0)=0.1+0.2+0.1=0.4.\\)<br>
    For \\(x=1\\): \\(f_X(1)=0.2+0.3+0.1=0.6.\\)<br>
    Hence, \\(f_X(0)=0.4, f_X(1)=0.6.\\)")
      ),
      
      # === Q2 ===
      list(
        id = 2,
        table_ui = HTML("
      <table class='pmf-table'>
        <tr><th class='corner outer'>\\(f_{X,Y}(x,y)\\)</th>
            <th>\\(y=1\\)</th><th>\\(y=2\\)</th></tr>
        <tr><th>\\(x=1\\)</th><td>0.3</td><td>0.1</td></tr>
        <tr><th>\\(x=2\\)</th><td>0.2</td><td>0.4</td></tr>
      </table>"),
        question = HTML("Find the conditional probability \\(P(Y=2\\mid X=2)\\)."),
        ans_frac = "2/3",
        ans_val = 2/3,
        
        hint1 = HTML("Hint 1 → Conditional probability is given by \\(P(Y=y|X=x)=f_{X,Y}(x,y)/f_X(x)\\)."),
        hint2 = HTML("Hint 2 → First compute the marginal \\(f_X(2)\\) by summing over \\(y\\)-values, then divide \\(f_{X,Y}(2,2)\\) by that total."),
        
        solution = HTML("<b>Full Solution:</b><br>
    \\(f_X(2)=0.2+0.4=0.6.\\)<br>
    \\(P(Y=2|X=2)=0.4/0.6=2/3.\\)")
      ),
      
      # === Q3 ===
      list(
        id = 3,
        table_ui = HTML("
      <table class='pmf-table'>
        <tr><th class='corner outer'>\\(f_{X,Y}(x,y)\\)</th>
            <th>\\(y=0\\)</th><th>\\(y=1\\)</th><th>\\(y=2\\)</th></tr>
        <tr><th>\\(x=0\\)</th><td>0.1</td><td>0.2</td><td>0.2</td></tr>
        <tr><th>\\(x=1\\)</th><td>0.2</td><td>0.2</td><td>0.1</td></tr>
      </table>"),
        question = HTML("Compute the conditional expectation \\(E(Y\\mid X=0)\\)."),
        ans_frac = "1.2",
        ans_val = 1.2,
        
        hint1 = HTML("Hint 1 → Conditional expectation is computed as \\(E(Y|X=x)=\\sum_y y f_{Y|X}(y|x)\\)."),
        hint2 = HTML("Hint 2 → First find \\(f_{Y|X}(y|0)\\) by dividing each entry in row \\(x=0\\) by \\(f_X(0)\\), then apply the definition of expectation."),
        
        solution = HTML("<b>Full Solution:</b><br>
    \\(f_X(0)=0.1+0.2+0.2=0.5.\\)<br>
    \\(E(Y|X=0)=(0×0.1+1×0.2+2×0.2)/0.5=1.2.\\)")
      ),
      
      # === Q4 ===
      list(
        id = 4,
        table_ui = HTML("
      <table class='pmf-table'>
        <tr><th class='corner outer'>\\(f_{X,Y}(x,y)\\)</th>
            <th>\\(y=1\\)</th><th>\\(y=2\\)</th><th>\\(y=3\\)</th></tr>
        <tr><th>\\(x=1\\)</th><td>0.2</td><td>0.1</td><td>0.1</td></tr>
        <tr><th>\\(x=2\\)</th><td>0.1</td><td>0.3</td><td>0.2</td></tr>
      </table>"),
        question = HTML("Find the marginal probability \\(f_Y(2)\\)."),
        ans_frac = "0.4",
        ans_val = 0.4,
        
        hint1 = HTML("Hint 1 → The marginal p.m.f. of Y is obtained by summing over all X-values for a fixed Y."),
        hint2 = HTML("Hint 2 → Look down the column where \\(y=2\\) and add the corresponding joint probabilities."),
        
        solution = HTML("<b>Full Solution:</b><br>
    \\(f_Y(2)=0.1+0.3=0.4.\\)")
      ),
      
      # === Q5 ===
      list(
        id = 5,
        table_ui = HTML("
      <table class='pmf-table'>
        <tr><th class='corner outer'>\\(f_{X,Y}(x,y)\\)</th>
            <th>\\(y=1\\)</th><th>\\(y=2\\)</th><th>\\(y=3\\)</th></tr>
        <tr><th>\\(x=0\\)</th><td>0.1</td><td>0.1</td><td>0.1</td></tr>
        <tr><th>\\(x=1\\)</th><td>0.1</td><td>0.2</td><td>0.3</td></tr>
      </table>"),
        question = HTML("Find the conditional p.m.f. of Y given \\(X=1\\), that is, \\(f_{Y|X}(y|1)\\)."),
        ans_frac = "1/6, 1/3, 1/2",
        ans_val = list(y1=1/6, y2=1/3, y3=1/2),
        
        hint1 = HTML("Hint 1 → The conditional p.m.f. \\(f_{Y|X}(y|x)\\) is found by dividing each joint probability in the row for that \\(x\\) by the marginal \\(f_X(x)\\)."),
        hint2 = HTML("Hint 2 → Compute \\(f_X(1)\\) first by adding all probabilities where \\(x=1\\), then divide each entry in that row by \\(f_X(1)\\)."),
        
        solution = HTML("<b>Full Solution:</b><br>
    \\(f_X(1)=0.1+0.2+0.3=0.6.\\)<br>
    \\(f_{Y|X}(1|1)=0.1/0.6=1/6,\\ f_{Y|X}(2|1)=0.2/0.6=1/3,\\ f_{Y|X}(3|1)=0.3/0.6=1/2.\\)")
      )
    )
  )
  
  typeset_now <- function(selector = NULL) {
    session$sendCustomMessage("mjx-typeset", list(selector = selector))
  }
  after_render <- function(selector = NULL) {
    session$onFlushed(function() typeset_now(selector), once = TRUE)
  }
  
  gcd2 <- function(a, b) { a <- abs(as.numeric(a)); b <- abs(as.numeric(b));
  while (b > 0) { tmp <- a %% b; a <- b; b <- tmp }
  if (is.na(a) || a == 0) 1 else a
  }
  lcm2 <- function(a, b) {
    a <- abs(round(as.numeric(a))); b <- abs(round(as.numeric(b)))
    if (a == 0 || b == 0) return(0)
    a / gcd2(a, b) * b
  }
  lcm_vec <- function(v) {
    v <- as.numeric(v)
    if (length(v) == 0) return(1)
    res <- 1
    for (d in v) {
      if (d == 0) next
      res <- lcm2(res, d)
      if (is.infinite(res) || is.nan(res)) return(1e9)
    }
    if (res == 0) 1 else res
  }
  simplify_frac <- function(num, den) {
    if (den == 0) return(c(0, 1))
    if (num == 0) return(c(0, 1))
    g <- gcd2(num, den)
    c(as.numeric(num / g), as.numeric(den / g))
  }
  parse_prob_numden <- function(x) {
    if (is.null(x)) return(list(num = 0, den = 1, value = 0))
    s <- trimws(as.character(x))
    if (s == "") return(list(num = 0, den = 1, value = 0))
    if (grepl("^[-+]?\\d+\\s*/\\s*\\d+$", s)) {
      parts <- strsplit(gsub("\\s", "", s), "/", fixed = TRUE)[[1]]
      num <- suppressWarnings(as.numeric(parts[1]))
      den <- suppressWarnings(as.numeric(parts[2]))
      if (is.na(num) || is.na(den) || den == 0) return(list(num=0, den=1, value=0))
      if (num < 0) num <- 0
      fr <- simplify_frac(num, den)
      return(list(num = fr[1], den = fr[2], value = fr[1]/fr[2]))
    }
    if (grepl("^[-+]?\\d*\\.?\\d+$", s)) {
      if (grepl("-", s)) return(list(num=0, den=1, value=0))
      if (!grepl("\\.", s)) {
        num <- suppressWarnings(as.numeric(s)); if (is.na(num)) num <- 0
        return(list(num = num, den = 1, value = num))
      }
      parts <- strsplit(s, "\\.", perl = TRUE)[[1]]
      ip <- parts[1]; fp <- parts[2]
      fp <- substr(fp, 1, 6)
      k  <- nchar(fp)
      num <- as.numeric(paste0(ifelse(ip=="", "0", ip), fp))
      den <- 10^k
      if (is.na(num)) num <- 0
      fr <- simplify_frac(num, den)
      return(list(num = fr[1], den = fr[2], value = fr[1]/fr[2]))
    }
    list(num = 0, den = 1, value = 0)
  }
  parse_prob <- function(x, default = 0) {
    nd <- parse_prob_numden(x)
    v <- nd$value
    if (!is.finite(v)) default else v
  }
  frac_to_string <- function(num, den) {
    if (is.na(num) || is.na(den) || den == 0) return("0")
    if (num == 0) return("0")
    fr <- simplify_frac(num, den)
    if (fr[2] == 1) sprintf("%g", fr[1]) else sprintf("%g/%g", fr[1], fr[2])
  }
  updatePInput <- function(session, id, value_string) {
    updateTextInput(session, id, value = value_string)
  }
  
  # ==== init ====
  init_df <- data.frame(
    x  = c(0, 0, 1, 1),
    y  = c(0, 1, 0, 1),
    p  = c(
      (48*47)/(52*51),
      (48*4) /(52*51),
      (4*48) /(52*51),
      (4*3)  /(52*51)
    ),
    p_str = c("188/221","16/221","16/221","1/221"),
    id = 1:4
  )
  pmf_edit    <- reactiveVal(init_df)
  pmf_final   <- reactiveVal(init_df[, c("x","y","p","id")])
  row_counter <- reactiveVal(nrow(init_df))
  
  exercise_block <- tagList(
    tags$hr(),
    h4("Practice Exercises"),
    div(style="overflow-x:auto; text-align:center;",
        div(id="tbl_mjx", uiOutput("table_out"), style="display:inline-block;")
    ),
    uiOutput("given_out"),
    tags$hr(),
    h5(div(id="q_mjx", uiOutput("qtext"))),
    textInput("ans", label = NULL, value = "", placeholder = "Input fractions such as 3/7 or decimals"),
    fluidRow(column(12, actionButton("check", "Submit", class="btn btn-success btn-block"))),
    uiOutput("feedback"),
    uiOutput("nav_buttons")  
  )
  
  # ==== UI Table ====
  output$prob_table_latex <- renderUI({
    df <- pmf_edit()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df$p <- df$p / sum(df$p)
    
    prob_table <- xtabs(p ~ x + y, data = df)
    
    HTML(knitr::kable(prob_table, format = "html", digits = 4))
  })
  
  
  # ==== UI ====
  output$pmf_table_ui <- renderUI({
    df <- pmf_edit()
    n  <- nrow(df)
    
    module <- input$module_select
    if (is.null(module)) module <- "Joint PMF"
    
    header <- switch(module,
                     "Joint PMF" = tagList(
                       h4("Joint Probability Mass Function"),
                       p("The joint probability mass function (PMF) shows how two discrete random variables, X and Y, take on values together."),
                       HTML("$$f_{X,Y}(x,y) = P(X = x, Y = y)$$"),
                       
                       tags$hr(),
                       
                       h5(" Definition"),
                       p("The joint PMF gives the probability that X equals x and Y equals y at the same time:"),
                       HTML("$$f_{X,Y}(x,y) \\ge 0, \\quad \\sum_x \\sum_y f_{X,Y}(x,y) = 1$$"),
                       
                       tags$hr(),
                       
                       h5(" Marginal distributions"),
                       p("From the joint PMF, we can find the marginal PMFs by summing over the other variable:"),
                       HTML("$$f_X(x)=\\sum_y f_{X,Y}(x,y), \\qquad f_Y(y)=\\sum_x f_{X,Y}(x,y)$$"),
                       p("The marginal PMFs show the individual distributions of X and Y."),
                       
                       tags$hr(),
                       
                       h5(" Interpretation"),
                       p("Plotting or tabulating the joint PMF helps us understand how X and Y vary together."),
                       p("High probability cells indicate combinations of (x, y) that are more likely to occur, while low probability cells show rare outcomes."),
                       
                       tags$hr(),
                       
                       h5(" Relationship with other PMFs"),
                       p("Later modules will introduce conditional PMFs, which can be derived from the joint PMF by normalization, and expectations based on these probabilities.")
                     ),
                     
                     "Conditional PMF" = tagList(
                       h4("Conditional Probability Mass Function"),
                       p("A conditional PMF shows the distribution of one variable when the other variable is known."),
                       h5(" Definition"),
                       p("The conditional PMFs are defined as:"),
                       HTML("$$f_{X|Y}(x|y)=\\frac{f_{X,Y}(x,y)}{f_Y(y)},\\quad\\text{for }f_Y(y)>0$$"),
                       HTML("$$f_{Y|X}(y|x)=\\frac{f_{X,Y}(x,y)}{f_X(x)},\\quad\\text{for }f_X(x)>0$$"),
                       p("These functions give the relative likelihood of X or Y after conditioning on a known value of the other variable."),
                       
                       h5(" Relationship with the joint PMF"),
                       p("The joint PMF can always be written as the product of a marginal and a conditional PMF:"),
                       HTML("$$f_{X,Y}(x,y)=f_X(x)f_{Y|X}(y|x)=f_Y(y)f_{X|Y}(x|y)$$"),
                       p("Each column or row of a joint PMF table becomes a conditional distribution when normalized so that it sums to 1."),
                    
                       h5(" Finding conditional PMFs from a joint table"),
                       tags$ul(
                         tags$li("Step 1 – Find the marginal distribution (either fₓ(x) or fᵧ(y)) by summing over the other variable."),
                         tags$li("Step 2 – Divide each joint probability in the selected row or column by the corresponding marginal value."),
                         tags$li("Step 3 – Check that the conditional PMF sums to 1 for the conditioning variable.")
                       ),
                       p("Example: For a fixed Y = y₀,"),
                       HTML("$$f_{X|Y}(x|y_0)=\\frac{f_{X,Y}(x,y_0)}{\\sum_x f_{X,Y}(x,y_0)}$$"),
                       p("Similarly, for a fixed X = x₀,"),
                       HTML("$$f_{Y|X}(y|x_0)=\\frac{f_{X,Y}(x_0,y)}{\\sum_y f_{X,Y}(x_0,y)}$$"),
                       
                       h5(" Interpretation"),
                       p("Conditional PMFs show how one variable’s distribution depends on the value of the other. If all conditional PMFs are the same for different conditioning values, X and Y are independent."),
                       p("Later modules will compare joint and conditional PMFs to explore this independence in more detail.")
                     ),
                     
                     "Expectation" = tagList(
                       h4("Expectation"),
                       p("In this module, we study the expected values of discrete random variables and how to compute conditional expectations based on the joint and conditional PMFs."),
                       
                       tags$hr(),
                       
                       h5(" Expectation of a discrete random variable"),
                       p("For a discrete random variable X with PMF fₓ(x), the expected value (or mean) is defined as:"),
                       HTML("$$E[X] = \\sum_x x f_X(x)$$"),
                       p("This represents the long-run average value of X when the experiment is repeated many times."),
                       
                       tags$hr(),
                       
                       h5(" Expectation of a function of two variables"),
                       p("If X and Y have joint PMF fₓᵧ(x, y), then for any function g(X, Y),"),
                       HTML("$$E[g(X,Y)] = \\sum_x \\sum_y g(x,y) f_{X,Y}(x,y)$$"),
                       p("Common examples include expectations such as E[X + Y], E[XY], or E[X²]. These are computed by multiplying each value of g(x, y) by its probability and summing over all pairs (x, y)."),
                       
                       tags$hr(),
                       
                       h5(" Conditional expectation"),
                       p("When the value of one variable is known, we can find the conditional expectation of the other. The conditional expectation of Y given X = x is:"),
                       HTML("$$E[Y|X=x] = \\sum_y y f_{Y|X}(y|x)$$"),
                       p("Similarly, the conditional expectation of X given Y = y is:"),
                       HTML("$$E[X|Y=y] = \\sum_x x f_{X|Y}(x|y)$$"),
                       p("Conditional expectations describe how the expected value of one variable changes when we condition on the other."),
                       
                       tags$hr(),
                       
                       h5(" Relationship between joint, marginal, and conditional expectations"),
                       p("The joint and conditional expectations are connected through the law of total expectation:"),
                       HTML("$$E[Y] = \\sum_x E[Y|X=x] f_X(x)$$"),
                       p("This result means that we can find the overall expectation of Y by averaging the conditional expectations weighted by the probabilities of each X = x."),
                       
                       tags$hr(),
                       
                       h5("Interpretation and applications"),
                       p("Conditional expectations are useful when we are interested in how one variable behaves depending on another (for example, expected waiting time given a certain condition)."),
                       p("Later modules and exercises will use these results to compute expected values from tables and verify relationships between joint and conditional distributions.")
                     ),
                     
                     "Comparison" = tagList(
                       h4("Comparing Joint and Conditional Distributions"),
                       p("In this module, we compare joint and conditional PMFs to explore how X and Y are related."),
                      
                       h5(" Relationship between joint and conditional PMFs"),
                       p("The joint PMF is connected to the conditional and marginal PMFs by the following relationships:"),
                       HTML("$$f_{X,Y}(x,y) = f_X(x)f_{Y|X}(y|x) = f_Y(y)f_{X|Y}(x|y)$$"),
                       
                       p("The marginal PMFs are found by summing over the other variable:"),
                       HTML("$$f_X(x)=\\sum_y f_{X,Y}(x,y), \\quad f_Y(y)=\\sum_x f_{X,Y}(x,y)$$"),
                       
                       p("The conditional PMFs are obtained by normalizing the joint PMF:"),
                       HTML("$$f_{Y|X}(y|x) = \\frac{f_{X,Y}(x,y)}{f_X(x)}, \\quad f_{X|Y}(x|y) = \\frac{f_{X,Y}(x,y)}{f_Y(y)}$$"),
                       
                       h5(" Independence"),
                       p("Two random variables X and Y are independent if and only if their joint PMF equals the product of their marginals:"),
                       HTML("$$f_{X,Y}(x,y)=f_X(x)f_Y(y)$$"),
                       p("Equivalently, independence means that conditioning on one variable does not change the distribution of the other:"),
                       HTML("$$f_{Y|X}(y|x)=f_Y(y), \\quad f_{X|Y}(x|y)=f_X(x)$$"),
                       p("In other words, if X and Y are independent, the value of one gives no information about the other."),
                       
                       h5(" Interpretation and comparison"),
                       p("When these equalities do not hold, X and Y are dependent. We can examine this dependence by:"),
                       tags$ul(
                         tags$li("Comparing conditional PMFs f_{Y|X}(y|x) for different values of x."),
                         tags$li("Computing conditional expectations E[Y|X=x] to see how the average of Y changes with X."),
                         tags$li("Checking whether each conditional PMF sums to 1 as a normalization test.")
                       ),
                       
                       p("These steps will guide you in solving the computational exercises in this module.")
                     ),
                     
                     tagList(
                       h4("Joint Probability Mass Function (Joint PMF)"),
                       p("The joint PMF shows how two discrete random variables take values together.")
                     )
    )
    footer_info <- tagList(
      h4("3D Plot Generate"),
      tags$ul(
        tags$li("Column p: Probability assigned to the point (x, y)."),
        tags$li("Column x: Value of the random variable X."),
        tags$li("Column y: Value of the random variable Y.")
      ),
      h5("Notes on Parameters:"),
      tags$ul(
        tags$li("Probabilities p must be non-negative. If values are counts, they will be normalized to sum to 1 when generating."),
        tags$li("x and y can be any integer values; duplicate points will be merged automatically."),
        tags$li("Empty inputs are treated as 0 for probabilities, and previous values are kept for x and y.")
      ),
      h5("Guide:"),
      tags$ul(
        tags$li("Add row: Insert a new row with default values (p=0, x=1, y=1)."),
        tags$li("Delete:Remove the selected row."),
        tags$li("Normalization:Divide all values of p by their total so the probabilities sum to 1 (when the total > 0)."),
        tags$li("Set Uniform Probability:Assign equal probability 1/n to each of the n rows."),
        tags$li("Generate Distribution: Finalize the PMF: merge duplicate (x,y), normalize probabilities, and update the plots.")
      ),
      hr()
    )
    
    rows <- lapply(seq_len(n), function(i) {
      pid <- paste0("p_", df$id[i])
      curr <- isolate(input[[pid]])
      default_str <- if (is.null(curr)) {
        if ("p_str" %in% names(df)) df$p_str[i] else "0"
      } else {
        curr
      }
      fluidRow(
        column(
          3,
          textInput(
            pid, "p",
            value = default_str,
            placeholder = "e.g. 1/4 or 0.25"
          )
        ),
        column(3, numericInput(paste0("x_", df$id[i]), "x", value = safe_num(df$x[i], 1), step = 1)),
        column(3, numericInput(paste0("y_", df$id[i]), "y", value = safe_num(df$y[i], 1), step = 1)),
        column(3, actionButton(paste0("del_", df$id[i]), "Delete", class = "btn-danger btn-sm"))
      )
    })
    
    footer <- tagList(
      hr(),
      actionButton("add_row",  "Add row"),
      actionButton("normalize","Normalize Probabilities"),
      actionButton("uniform",  "Set Uniform Probability"),
      br(), br(),
      actionButton("generate", "Generate PMF", class = "btn-primary")
    )
    
    after_render("#pmf_mjx")
    div(
      id = "pmf_mjx",
      header,         
      footer_info,   
      rows,            
      footer,           
      exercise_block    
    )
  })
  
  
  ## === live snapshot of inputs (no write-back) ===
  df_current <- reactive({
    base <- pmf_edit()
    if (nrow(base) == 0) return(base)
    for (i in base$id) {
      p_in <- input[[paste0("p_", i)]]
      x_in <- input[[paste0("x_", i)]]
      y_in <- input[[paste0("y_", i)]]
      if (!is.null(p_in)) base$p[base$id==i] <- max(0, parse_prob(p_in, base$p[base$id==i]))
      if (!is.null(x_in)) base$x[base$id==i] <- safe_num(x_in, base$x[base$id==i])
      if (!is.null(y_in)) base$y[base$id==i] <- safe_num(y_in, base$y[base$id==i])
    }
    base
  })
  
  observe({
    n <- nrow(pmf_edit())
    if (n == 0) {
      shinyjs::disable("normalize"); shinyjs::disable("uniform"); shinyjs::disable("generate")
    } else {
      shinyjs::enable("normalize");  shinyjs::enable("uniform");  shinyjs::enable("generate")
    }
  })
  
  ## ==== add row ====
  observeEvent(input$add_row, {
    df <- pmf_edit()
    new_id <- row_counter() + 1
    row_counter(new_id)
    df <- rbind(df, data.frame(x = 1, y = 1, p = 0.0, p_str = "0", id = new_id))
    pmf_edit(df)
  })
  
  ## ==== delete rows ====
  observeEvent(
    eventExpr = {
      ids <- pmf_edit()$id
      if (length(ids) == 0) return(NULL)
      paste0(sapply(ids, function(i) safe_num(input[[paste0("del_", i)]], 0)), collapse = "_")
    },
    handlerExpr = {
      df  <- isolate(pmf_edit())
      ids <- df$id
      if (length(ids) == 0) return(NULL)
      clicks <- sapply(ids, function(i) isTRUE(safe_num(isolate(input[[paste0("del_", i)]]), 0) > 0))
      if (any(clicks)) {
        df <- df[!clicks, , drop = FALSE]
        pmf_edit(df)
      }
    },
    ignoreInit = TRUE
  )
  
  ## ==== Normalize (keep fraction display) ====
  observeEvent(input$normalize, {
    base <- pmf_edit()
    if (nrow(base) == 0) { showNotification("No rows available to normalize.", type="error", duration=5); return() }
    
    ids <- base$id
    nds <- lapply(ids, function(i) parse_prob_numden(input[[paste0("p_", i)]]))
    nums <- vapply(nds, function(z) z$num, numeric(1))
    dens <- vapply(nds, function(z) z$den, numeric(1))
    nums[nums < 0] <- 0
    
    L <- lcm_vec(dens); if (L <= 0) L <- 1
    Ni <- nums * (L / dens)
    S  <- sum(Ni)
    
    if (S > 0) {
      new_p <- Ni / S
      base$p <- new_p
      pmf_edit(base)
      for (k in seq_along(ids)) {
        fr <- simplify_frac(Ni[k], S)
        updatePInput(session, paste0("p_", ids[k]), frac_to_string(fr[1], fr[2]))
      }
      showNotification("Probabilities normalized to sum = 1.", type="message", duration=3)
    } else {
      showNotification("All probabilities are empty or non-positive. Nothing to normalize.", type="error", duration=5)
    }
  })
  
  ## ==== Set Uniform Probability (as 1/n) ====
  observeEvent(input$uniform, {
    base <- pmf_edit()
    n <- nrow(base)
    if (n > 0) {
      new_p <- rep(1/n, n)
      base$p <- new_p
      pmf_edit(base)
      for (i in seq_len(n)) {
        updatePInput(session, paste0("p_", base$id[i]), paste0("1/", n))
      }
      showNotification(paste("Uniform probability set: each row =", paste0("1/", n)),
                       type="message", duration=3)
    } else {
      showNotification("No rows available to set uniform probability.", type="error", duration=5)
    }
  })
  
  ## ==== Generate ====
  observeEvent(input$generate, {
    df <- df_current()
    if (nrow(df) == 0) { showNotification("No rows available to generate PMF.", type="error", duration=5); return() }
    
    df$x <- safe_num(df$x, 1)
    df$y <- safe_num(df$y, 1)
    df$p <- safe_num(df$p, 0)
    df <- aggregate(p ~ x + y, data = df, sum)
    names(df) <- c("x","y","p")
    
    s <- sum(df$p)
    if (s > 0) {
      df$p <- df$p / s
      pmf_final(df)
    } else {
      showNotification("Cannot generate PMF because all probabilities are zero/empty.",
                       type="error", duration=5)
    }
  })
  
  # ====== Practice Exercises state & outputs ======
  # === 状态变量 ===
  prob   <- reactiveVal(NULL)
  result <- reactiveVal(NULL)
  tries  <- reactiveVal(0)
  
  current_index <- reactiveVal(1)
  
  current_module <- reactive({
    req(input$module_select)
    question_bank[[input$module_select]]
  })
  
  new_problem <- function() {
    qbank <- current_module()
    idx <- current_index()
    
    if (is.null(qbank) || length(qbank) == 0) {
      prob(NULL)
      return()
    }
    if (idx > length(qbank)) idx <- length(qbank)
    
    result(NULL)
    tries(0)
    updateTextInput(session, "ans", value = "")
    
    p <- qbank[[idx]]
    prob(p)
    after_render("#tbl_mjx")
    after_render("#q_mjx")
  }
  
  observeEvent(TRUE, { new_problem() }, once = TRUE)
  
  observeEvent(input$module_select, {
    current_index(1)
    tries(0)
    updateTextInput(session, "ans", value = "")
    new_problem()
  })
  
  observeEvent(input$next_q, {
    qbank <- current_module()
    n_total <- length(qbank)
    
    idx <- current_index() + 1
    if (idx > n_total) {
      showNotification(paste("✅ You’ve completed all", n_total, "questions in this module!"), type="message")
    } else {
      current_index(idx)
      new_problem()
    }
  })
  
  observeEvent(input$back_q, {
    idx <- max(1, current_index() - 1)
    current_index(idx)
    new_problem()
  })
  
  wrap_div <- function(id, tag) tags$div(id = id, tag)
  
  output$table_out <- renderUI({
    p <- prob(); validate(need(!is.null(p), ""))
    ui <- wrap_div("tbl_mjx", p$table_ui)
    after_render("#tbl_mjx")
    ui
  })
  
  
  output$given_out <- renderUI({
    p <- prob(); if (is.null(p) || is.null(p$given)) return(NULL)
    div(class="table-note", HTML(p$given))
  })
  
  output$qtext <- renderUI({
    p <- prob(); validate(need(!is.null(p), ""))
    ui <- wrap_div("q_mjx", p$question)
    after_render("#q_mjx")
    ui
  })
  
  observeEvent(input$check, {
    p <- prob(); validate(need(!is.null(p), ""))
    ok <- is_correct(input$ans, p$ans_val, p$ans_frac)
    result(list(status = if (ok) "ok" else "no"))
    tries(tries() + if (ok) 3 else 1)
    after_render("#fb_mjx")
  })
  
  output$feedback <- renderUI({
    st <- result(); p <- prob()
    if (is.null(st) || is.null(p)) return(NULL)
    
    ui <- if (st$status == "ok") {
      div(class="alert alert-success", p$solution)
    } else if (st$status == "no") {
      if (tries() == 1) {
        div(class="alert alert-warning", p$hint1)
      } else if (tries() == 2) {
        div(class="alert alert-warning", p$hint2)
      } else {
        div(class="alert alert-info", p$solution)
      }
    }
    
    # ====== Navigation Buttons (Back / Next) ======
    output$nav_buttons <- renderUI({
      mod <- input$module_select
      n_total <- length(question_bank[[mod]])
      idx <- current_index()
      
      btns <- list()
    
      if (idx > 1) {
        btns <- append(btns, list(
          actionButton("back_q", "← Back", class = "btn btn-secondary btn-block")
        ))
      }
      
      if (idx < n_total) {
        btns <- append(btns, list(
          actionButton("next_q", "Next →", class = "btn btn-primary btn-block")
        ))
      }
      
      fluidRow(
        lapply(btns, function(b) column(6, b))
      )
    })
    
    
    after_render("#fb_mjx")
    tags$div(id="fb_mjx", ui)
  })
  
  
  output$prob_table_html <- renderUI({
    df <- pmf_final()
    if (is.null(df) || nrow(df) == 0) df <- df_current()
    if (is.null(df) || nrow(df) == 0) {
      after_render("#calc_box")
      return(HTML("<i>Generate the distribution to view the table.</i>"))
    }
    
    df <- df[, c("x","y","p"), drop = FALSE]
    s <- sum(df$p, na.rm = TRUE)
    if (s > 0 && abs(s - 1) > 1e-9) df$p <- df$p / s

    mat <- xtabs(p ~ x + y, data = df)
    xs  <- rownames(mat)
    ys  <- colnames(mat)
    
    row_tot <- rowSums(mat)
    col_tot <- colSums(mat)
    
    # --- 表头 ---
    head_row <- tags$tr(
      tags$th(""), 
      lapply(ys, function(yval) tags$th(tags$b(paste0("y=", yval)))),
      tags$th(tags$b("Total"))
    )
    
    # --- 表体 ---
    body_rows <- lapply(seq_len(nrow(mat)), function(i) {
      tags$tr(
        tags$th(tags$b(paste0("x=", xs[i]))),
        lapply(seq_len(ncol(mat)), function(j)
          tags$td(formatC(mat[i, j], format = "f", digits = 4))
        ),
        tags$td(tags$b(formatC(row_tot[i], format = "f", digits = 4)))
      )
    })
    
    # --- 合计行 ---
    foot_row <- tags$tr(
      tags$th(tags$b("Total")),
      lapply(seq_len(ncol(mat)), function(j)
        tags$td(tags$b(formatC(col_tot[j], format = "f", digits = 4)))
      ),
      tags$td(tags$b("1.0000"))
    )
    
    tbl <- tags$table(
      class = "pmf-table",
      tags$thead(head_row),
      tags$tbody(body_rows, foot_row)
    )
    
    after_render("#calc_box")  
    tbl
  })
  
  
  # ====== calculate ======
  output$calc_result <- renderUI({
    df <- pmf_final()
    fmt  <- function(v) formatC(v, format = "f", digits = 6)
    math <- function(s) paste0("$$", s, "$$")
    
    if (is.null(df) || nrow(df) == 0) {
      after_render("#calc_box")
      return(HTML("<i>No PMF generated yet.</i>"))
    }
    
    type <- input$calc_type
    if (is.null(type)) type <- "Expectation"
    
    fx <- aggregate(p ~ x, data = df, sum); fx <- fx[order(fx$x), ]
    fy <- aggregate(p ~ y, data = df, sum); fy <- fy[order(fy$y), ]
    
    out_lines <- character(0)
    
    if (type == "Expectation") {
      # E[X], E[Y]
      EX_terms <- paste0(fx$x, "\\cdot ", fmt(fx$p), collapse = " + ")
      EY_terms <- paste0(fy$y, "\\cdot ", fmt(fy$p), collapse = " + ")
      EX <- sum(fx$x * fx$p)
      EY <- sum(fy$y * fy$p)
      
      out_lines <- c(
        math(paste0("E[X] = \\sum_x x\\, f_X(x) = ", EX_terms, " = ", fmt(EX))),
        "<br/>",
        math(paste0("E[Y] = \\sum_y y\\, f_Y(y) = ", EY_terms, " = ", fmt(EY))),
        "<hr/>"
      )
      
      for (j in seq_len(nrow(fy))) {
        yv <- fy$y[j]; py <- fy$p[j]
        if (py > 0) {
          sub <- df[df$y == yv, c("x","p")]; sub <- sub[order(sub$x), ]
          num_terms <- paste0(sub$x, "\\cdot ", fmt(sub$p), collapse = " + ")
          num <- sum(sub$x * sub$p)
          out_lines <- c(out_lines,
                         math(paste0("E[X\\mid Y=", yv, "] = \\frac{", num_terms, "}{", fmt(py), "} = ", fmt(num/py))))
        } else {
          out_lines <- c(out_lines,
                         math(paste0("E[X\\mid Y=", yv, "] \\text{ undefined (} f_Y(", yv, ")=0 \\text{)}")))
        }
      }
      out_lines <- c(out_lines, "<br/>")
      
      #  E[Y|X=x]
      for (i in seq_len(nrow(fx))) {
        xv <- fx$x[i]; px <- fx$p[i]
        if (px > 0) {
          sub <- df[df$x == xv, c("y","p")]; sub <- sub[order(sub$y), ]
          num_terms <- paste0(sub$y, "\\cdot ", fmt(sub$p), collapse = " + ")
          num <- sum(sub$y * sub$p)
          out_lines <- c(out_lines,
                         math(paste0("E[Y\\mid X=", xv, "] = \\frac{", num_terms, "}{", fmt(px), "} = ", fmt(num/px))))
        } else {
          out_lines <- c(out_lines,
                         math(paste0("E[Y\\mid X=", xv, "] \\text{ undefined (} f_X(", xv, ")=0 \\text{)}")))
        }
      }
      
    } else if (type == "Marginal") {
      # f_X, f_Y
      out_lines <- c(
        math("f_X(x) = \\sum_y f_{X,Y}(x,y)"),
        sapply(seq_len(nrow(fx)), function(i)
          math(paste0("f_X(", fx$x[i], ") = ", fmt(fx$p[i])))),
        "<hr/>",
        math("f_Y(y) = \\sum_x f_{X,Y}(x,y)"),
        sapply(seq_len(nrow(fy)), function(i)
          math(paste0("f_Y(", fy$y[i], ") = ", fmt(fy$p[i]))))
      )
      
    } else if (type == "Joint") {
      # f_{X,Y}(x,y) = P(X=x, Y=y)
      df_ord <- df[order(df$x, df$y), ]
      out_lines <- apply(df_ord, 1, function(row) {
        x <- as.numeric(row[["x"]]); y <- as.numeric(row[["y"]]); p <- as.numeric(row[["p"]])
        math(paste0("f_{X,Y}(", x, ",", y, ") = P(X=", x, ",\\,Y=", y, ") = ", fmt(p)))
      })
      
    } else if (type == "Conditional") {
      out_lines <- c()
      # f(X|Y=y)
      for (yv in fy$y) {
        denom <- sum(df$p[df$y == yv])
        if (denom > 0) {
          sub <- df[df$y == yv, ]
          sub <- sub[order(sub$x), ]
          for (i in seq_len(nrow(sub))) {
            out_lines <- c(out_lines, math(
              paste0("f_{X\\mid Y}(", sub$x[i], "\\mid ", yv, ") = ",
                     "\\frac{f_{X,Y}(", sub$x[i], ",", yv, ")}{f_Y(", yv, ")} = ",
                     fmt(sub$p[i]/denom))
            ))
          }
        } else {
          out_lines <- c(out_lines, math(paste0("f_Y(", yv, ")=0 \\Rightarrow f_{X\\mid Y}(x\\mid ", yv, ")\\;\\text{undefined}")))
        }
      }
      
      out_lines <- c(out_lines, "<hr/>")
      
      for (xv in fx$x) {
        denom <- sum(df$p[df$x == xv])
        if (denom > 0) {
          sub <- df[df$x == xv, ]
          sub <- sub[order(sub$y), ]
          for (i in seq_len(nrow(sub))) {
            out_lines <- c(out_lines, math(
              paste0("f_{Y\\mid X}(", sub$y[i], "\\mid ", xv, ") = ",
                     "\\frac{f_{X,Y}(", xv, ",", sub$y[i], ")}{f_X(", xv, ")} = ",
                     fmt(sub$p[i]/denom))
            ))
          }
        } else {
          out_lines <- c(out_lines, math(paste0("f_X(", xv, ")=0 \\Rightarrow f_{Y\\mid X}(y\\mid ", xv, ")\\;\\text{undefined}")))
        }
      }
    }
    
    all_html <- paste(out_lines, collapse = "<br/>")
    
    sections <- unlist(strsplit(all_html, "<hr/>"))
    
    sections <- trimws(sections)
    
    ncol <- ifelse(length(sections) <= 2, 2, 3)  
    col_width <- 12 / ncol
    cols <- lapply(sections, function(sec) {
      column(col_width, HTML(sec))
    })
    after_render("#calc_box")
    do.call(fluidRow, cols)
    
  })
  
  ## ==== 3D joint plot ====
  output$plot3d <- renderPlotly({
    df <- pmf_final()
    if (nrow(df) == 0) return(NULL)
    p <- plot_ly()
    for (i in seq_len(nrow(df))) {
      p <- add_trace(
        p, type = "scatter3d", mode = "lines",
        x = c(df$x[i], df$x[i]),
        y = c(df$y[i], df$y[i]),
        z = c(0, df$p[i]),
        line = list(color = "steelblue", width = 8),
        hoverinfo = "text",
        text = paste0("X=", df$x[i], "<br>Y=", df$y[i], "<br>P=", round(df$p[i], 4)),
        showlegend = FALSE
      )
    }
    p %>% layout(
      title = "3D Joint PMF",
      scene = list(
        xaxis = list(title = "X"),
        yaxis = list(title = "Y"),
        zaxis = list(title = "f_{X,Y}(x,y)")
      )
    )
  })
  
  ## ==== bird's-eye joint ====
  output$plot_bird <- renderPlotly({
    df <- pmf_final()
    if (nrow(df) == 0) return(NULL)
    plot_ly(
      df, x = ~x, y = ~y,
      type = "scatter", mode = "markers",
      marker = list(size = 12, color = ~p, colorscale = "Blues"),
      text = ~paste0("X=", x, "<br>Y=", y, "<br>P=", round(p, 4))
    ) %>% layout(
      title = "Bird's-eye View",
      xaxis = list(title = "X"),
      yaxis = list(title = "Y")
    )
  })
  
  ## ==== conditional controls ====
  output$cond_ctrls <- renderUI({
    df <- pmf_final()
    if (nrow(df) == 0) return(NULL)
    fluidRow(
      column(
        6,
        selectInput(
          "cond_type", "Conditional slice:",
          choices = c("f(X | Y = y0)" = "xy", "f(Y | X = x0)" = "yx"),
          selected = "xy"
        )
      ),
      column(
        6,
        uiOutput("cond_val_ui")
      )
    )
  })
  
  output$cond_val_ui <- renderUI({
    df <- pmf_final()
    if (nrow(df) == 0) return(NULL)
    if (is.null(input$cond_type) || input$cond_type == "xy") {
      selectInput("cond_value", "y0 =", choices = sort(unique(df$y)))
    } else {
      selectInput("cond_value", "x0 =", choices = sort(unique(df$x)))
    }
  })
  
  ## ==== conditional 3D ====
  output$plot3d_cond <- renderPlotly({
    df <- pmf_final()
    if (nrow(df) == 0) return(NULL)
    
    type <- input$cond_type; val <- input$cond_value
    if (is.null(type)) type <- "xy"
    
    p <- plot_ly()
    
    if (type == "xy") {
      if (is.null(val)) val <- sort(unique(df$y))[1]
      y0  <- as.numeric(val)
      sub <- aggregate(p ~ x, data = df[df$y == y0, c("x","p")], sum)
      denom <- sum(sub$p); if (denom <= 0) return(NULL)
      sub$p <- sub$p / denom
      sub <- sub[order(sub$x), ]
      for (i in seq_len(nrow(sub))) {
        p <- add_trace(
          p, type="scatter3d", mode="lines",
          x=c(sub$x[i], sub$x[i]),
          y=c(y0, y0),
          z=c(0, sub$p[i]),
          line=list(color="steelblue", width=8),
          hoverinfo="text",
          text=paste0("x=", sub$x[i], "<br>y=", y0,
                      "<br>f(X|Y)=", round(sub$p[i],4)),
          showlegend=FALSE
        )
      }
      p %>% layout(
        title = "3D Conditional PMF",
        scene = list(
          xaxis = list(title="X"),
          yaxis = list(title="Y"),
          zaxis = list(title="f_{X|Y}(x|y)")
        )
      )
    } else {
      if (is.null(val)) val <- sort(unique(df$x))[1]
      x0  <- as.numeric(val)
      sub <- aggregate(p ~ y, data = df[df$x == x0, c("y","p")], sum)
      denom <- sum(sub$p); if (denom <= 0) return(NULL)
      sub$p <- sub$p / denom
      sub <- sub[order(sub$y), ]
      for (i in seq_len(nrow(sub))) {
        p <- add_trace(
          p, type="scatter3d", mode="lines",
          x=c(x0, x0),
          y=c(sub$y[i], sub$y[i]),
          z=c(0, sub$p[i]),
          line=list(color="steelblue", width=8),
          hoverinfo="text",
          text=paste0("x=", x0, "<br>y=", sub$y[i],
                      "<br>f(Y|X)=", round(sub$p[i],4)),
          showlegend=FALSE
        )
      }
      p %>% layout(
        title = "3D Conditional PMF",
        scene = list(
          xaxis = list(title="X"),
          yaxis = list(title="Y"),
          zaxis = list(title="f_{Y|X}(y|x)")
        )
      )
    }
  })
  
  ## ==== conditional bird's-eye ====
  output$plot_bird_cond <- renderPlotly({
    df <- pmf_final()
    if (nrow(df) == 0) return(NULL)
    
    type <- input$cond_type; val <- input$cond_value
    if (is.null(type)) type <- "xy"
    
    if (type == "xy") {
      if (is.null(val)) val <- sort(unique(df$y))[1]
      y0  <- as.numeric(val)
      sub <- df[df$y == y0, , drop=FALSE]
      if (nrow(sub) == 0) return(NULL)
      sub <- aggregate(p ~ x, data = sub[, c("x","p")], sum)
      denom <- sum(sub$p); if (denom <= 0) return(NULL)
      sub$prob <- sub$p / denom
      plot_ly(
        sub, x=~x, y=~I(y0), type="scatter", mode="markers",
        marker=list(size=14, color=~prob, colorscale="Blues"),
        text=~paste0("x=", x, "<br>y=", y0, "<br>f(X|Y)=", round(prob,4)),
        hoverinfo="text"
      ) %>% layout(
        title="Bird's-eye View (Conditional)",
        xaxis=list(title="X"),
        yaxis=list(title="Y", range=c(min(df$y)-0.5, max(df$y)+0.5))
      )
    } else {
      if (is.null(val)) val <- sort(unique(df$x))[1]
      x0  <- as.numeric(val)
      sub <- df[df$x == x0, , drop=FALSE]
      if (nrow(sub) == 0) return(NULL)
      sub <- aggregate(p ~ y, data = sub[, c("y","p")], sum)
      denom <- sum(sub$p); if (denom <= 0) return(NULL)
      sub$prob <- sub$p / denom
      plot_ly(
        sub, x=~I(x0), y=~y, type="scatter", mode="markers",
        marker=list(size=14, color=~prob, colorscale="Blues"),
        text=~paste0("x=", x0, "<br>y=", y, "<br>f(Y|X)=", round(prob,4)),
        hoverinfo="text"
      ) %>% layout(
        title="Bird's-eye View (Conditional)",
        xaxis=list(title="X"),
        yaxis=list(title="Y", range=c(min(df$y)-0.5, max(df$y)+0.5))
      )
    }
  })
  
  # ---  f_X(x) ---
  output$table_fx <- renderTable({
    df <- pmf_final()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    fx <- aggregate(p ~ x, data = df, sum)
    fx <- fx[order(fx$x), ]
    data.frame(x = fx$x, fX = round(fx$p, 4))
  }, bordered = TRUE, align = "c")
  
  # ---  f_Y(y) ---
  output$table_fy <- renderTable({
    df <- pmf_final()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    fy <- aggregate(p ~ y, data = df, sum)
    fy <- fy[order(fy$y), ]
    data.frame(y = fy$y, fY = round(fy$p, 4))
  }, bordered = TRUE, align = "c")
 
 
}

shinyApp(ui, server)