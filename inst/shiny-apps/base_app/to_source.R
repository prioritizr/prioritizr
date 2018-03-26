about_panel <- tabPanel("About", 
                        h4("Systematic conservation prioritization in R"),
                        p("Prioritizr is an R package for solving systematic conservation prioritization problems 
                        using integer linear programming (ILP) techniques. The package offers a flexible interface 
                        for creating conservation problems using a range of different objectives and constraints that 
                        can be tailored to the specific needs of the conservation planner. Conservation problems can 
                        be solved using a variety of commercial and open-source exact algorithm solvers. In contrast 
                        to the algorithms conventionally used to solve conservation problems, such as greedy heuristics 
                        or simulated annealing, the exact algorithms used by prioritizr are guaranteed to find optimal 
                        solutions. This package also has the functionality to read Marxan input data and find much 
                        cheaper solutions in a much shorter period of time than Marxan (Beyer et al. 2016). Check out the 
                        prioritizrshiny R package to interactively build and customize conservation planning problems."),
                        h4("Overview"),
                        tags$div(HTML('<p>This package largely consists of seven main types of functions. These functions are used to:</p>
                            <ul>
                            <li>create a new reserve design <a href="https://prioritizr.github.io/prioritizr/reference/problem.html">problem</a> by specifying the planning units and features of conservation interest (e.g. species, ecosystems).</li>
                            <li>add an <a href="https://prioritizr.github.io/prioritizr/reference/objectives.html">objective</a> to a reserve design problem. For example, the <a href="https://prioritizr.github.io/prioritizr/reference/add_min_set_objective.html"><code>add_min_set_objective</code></a> function can used to specify that the overall goal of the prioritization is to adequately represent each feature for minimal cost.</li>
                            <li>add <a href="https://prioritizr.github.io/prioritizr/reference/targets.html">targets</a> to a problem to identify how much of each feature is desired in solutions</li>
                            <li>add <a href="https://prioritizr.github.io/prioritizr/reference/constraints.html">constraints</a> to a problem to obtain better solutions. For example, the <a href="https://prioritizr.github.io/prioritizr/reference/add_locked_in_constraints.html"><code>add_locked_in_constraints</code></a> function can be used to ensure that specific planning units will be prioritized. This can be useful when identifying new places to add to an existing reserve network.</li>
                            <li>add <a href="https://prioritizr.github.io/prioritizr/reference/penalties.html">penalties</a> to a problem to penalize ineffective solutions. For example, the <a href="https://prioritizr.github.io/prioritizr/reference/add_boundary_penalties.html"><code>add_boundary_penalties</code></a> function can be used to add penalties to the problem that result in solutions being clumped into contiguous reserves.</li>
                            <li>add <a href="https://prioritizr.github.io/prioritizr/reference/decisions.html">decisions</a> to a problem to specify the nature of the conservation decision on the planning units. For example, the <a href="https://prioritizr.github.io/prioritizr/reference/add_binary_decisions.html"><code>add_binary_decisions</code></a> function specifies that planning units are either prioritized or not. Whereas, the <a href="https://prioritizr.github.io/prioritizr/reference/add_proportion_decisions.html"><code>add_proportion_decisions</code></a> can be used to specify that a proportion of each planning unit can be prioritized.</li>
                            <li>add methods to generate a <a href="https://prioritizr.github.io/prioritizr/reference/portfolios.html">portfolio</a> of solutions. For instance, use <a href="https://prioritizr.github.io/prioritizr/reference/add_cuts_portfolio.html"><code>add_cuts_portfolio</code></a> to find a given number of solutions that are closest to optimality.</li>
                            <li>add a <a href="https://prioritizr.github.io/prioritizr/reference/solve.html">solver</a> to customize the methods used to solve the problem.</li>
                            </ul>
                            <p>The currently supported solvers are listed below. Each must be installed separately from this package. The details of the solvers are intentionally abstracted away so that minimal knowledge is required to use a given solver.</p>
                            <ul>
                            <li>
                            <a href="http://gurobi.com">Gurobi:</a> Install the <a href="http://www.gurobi.com/products/modeling-languages/r"><em>gurobi</em> <em>R</em> package</a> to use this solver.</li>
                            <li>
                            <a href="https://projects.coin-or.org/SYMPHONY">SYMPHONY:</a> Install either the <a href="https://CRAN.R-project.org/package=Rsymphony"><em>Rsymphony</em></a> or <a href="https://bioconductor.riken.jp/packages/3.3/bioc/html/lpsymphony.html"><em>lpsymphony</em></a> <em>R</em> packages to use this solver.</li>
                            </ul>
                            <p>To cite package <em>prioritizr</em> in publications please use:</p>
                            <pre><code>Hanson JO, Schuster R, Morrell N, Strimas-Mackey M, Watts ME, Arcese P, Bennett J, Possingham HP (2017). prioritizr: Systematic Conservation Prioritization in R. R package version 3.0.3.3. https://github.com/prioritizr/prioritizr.</code></pre>
                            <p>Additionally, we keep a <a href="https://prioritizr.github.io/prioritizr/articles/publication_record.html">record of publications</a> that use <em>prioritizr</em>, so please <a href="https://github.com/prioritizr/prioritizr/issues/new">file an issue on GitHub</a> so we can add it to the list.</p>')
                                 ),
                        
                        #a("Key assumptions", class = "btn btn-primary btn-md", href="Key-Assumptions.pdf", target="_blank"),
                        #a("Technical information", class = "btn btn-primary btn-md", href="Technical Information.pdf", target="_blank"),
                        a("Contact us", class = "btn btn-primary btn-md", href="mailto:mail@richard-schuster.com?Subject=prioritizrshiny"),
                        br()#,
                        #br(),
                        #tags$a(href="https://www.sfu.ca/", target="_blank", tags$img(src='sfulogo.jpg')),
                        #tags$a(href="http://www.wilburforce.org/", target="_blank", tags$img(src='sop1.png'))
                        )

howto_panel <- tabPanel("Instructions", 
                        h4("How to use this tool?"),
                        p("This "))
