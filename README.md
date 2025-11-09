# Shiny Visualization Project

This repository contains the Shiny-based interactive visualization system developed for the dissertation project on **Probability and Statistics Education**.  
It provides an intuitive platform for exploring probability distributions through visualization, interaction, and storytelling.

---

## Project Overview

The Shiny application helps students learn key probability concepts through simulation and dynamic visualization.  
Users can interactively manipulate parameters, visualize how probability functions change, and explore statistical relationships in real time.

### Extended Work: Java-Based Markov Chain Learning Tool

In addition to the main Shiny-based platform, this project also includes an enhanced version of a **Java-based Markov Chain interactive learning tool**, originally developed by previous research students in the Department of Statistics.  
The upgraded version introduces a refined parameter adjustment range and a **dynamic color-tracking system** within the transition table, allowing users to visually follow the evolution of state probabilities during chain iterations.  
These improvements significantly enhance the pedagogical clarity and interactivity of the tool while preserving its computational robustness.  

The updated Java source code is also included in this repository, within the same GitHub project, to ensure **transparency, reproducibility, and open access** for future academic and teaching use.  
This side work complements the Shiny platform by extending the interactive visualization philosophy to **stochastic process education**, supporting deeper conceptual understanding of Markov chains and state transitions.

---

## Core Modules

| Module | Description |
|:--------|:-------------|
| **Main Application (`main.R`)** | Integrates all modules into a unified Shiny dashboard. |
| **Probability Distribution (`module_probability.R`)** | Visualizes major discrete and continuous distributions, including Bernoulli, Binomial, Poisson, and Normal. |
| **Game: Discrete (`module_game_discrete.r`)** | An interactive detective-style probability game that strengthens conceptual understanding through contextual tasks. |
| **Joint Distribution (`module_joint.R`)** | Enables users to construct and explore joint PMFs, marginal, and conditional distributions. |
| **Conditional Distribution (`module_conditional.R`)** | Demonstrates how conditional probabilities evolve based on different given conditions. |
| **Expectation (`module_expectation.R`)** | Allows learners to compute and visualize expectations of random variables using table-based PMFs. |
| **Extended Work (Java: Markov Chain Tool)** | A supplementary interactive program for visualizing stochastic transitions, with improved parameter range control and dynamic color-tracking effects. |

---

## Included Resources

The packaged file `Project_PPeek1ng.zip` includes:
- All R source files listed above  
- Image and visualization assets used across modules  
- The enhanced Java Markov Chain learning tool (source and compiled version)  
- A structured directory ready for deployment on RStudio or Shiny Server

---

## How to Run

1. Download or clone this repository:
   git clone https://github.com/D1OK1NG/Shiny-Visualization-Project.git
Unzip Project_PPeek1ng.zip

Open the project folder in RStudio

Run the app:
shiny::runApp("Project_PPeek1ng")
The dashboard will open in your default browser.

For the Markov Chain Tool, open the MarkovChainTool/ subfolder and run:

java -jar MarkovChainVisualizer.jar
Academic Context
This project was developed as part of a Master’s dissertation in the Department of Statistics, University of Auckland, under the academic supervision of Dr. Azam Asanjarani and Dr. Heti Afimeimounga.
Their guidance and expertise were invaluable in shaping both the educational framework and technical implementation of this system.

Alongside the Shiny platform, a supplementary enhancement of a Java-based Markov Chain learning tool was also undertaken as part of the same research initiative.
This side work modernized the interface and improved the pedagogical visualization of stochastic transitions, aligning with the dissertation’s overarching goal of fostering interactive, visually supported probability learning.

As this work forms part of a larger ongoing project led by the supervisors, the repository has also been forked to their GitHub account.
Upon completion, it will be included in the department’s official GitHub repository to support future research and teaching initiatives in probability education.

For detailed methodology, educational design, and system architecture, please refer to the dissertation document.

Repository Structure
Shiny-Visualization-Project/
│
├── Project_PPeek1ng.zip              # Full Shiny project archive
├── README.md                         # Project documentation
├── MarkovChainTool/                  # Java-based side project
│   ├── src/
│   ├── bin/
│   ├── MarkovChainVisualizer.jar
│   └── README_Markov.md
└── (unzipped Shiny folder contents)
    ├── main.R
    ├── module_probability.R
    ├── module_game_discrete.r
    ├── module_joint.R
    ├── module_conditional.R
    ├── module_expectation.R
    └── images/
License & Citation
This project is shared under the MIT License for academic and educational purposes.
If you reference or reuse this work, please cite:

Wu, Chunyi (2025). Shiny-Based Probability Visualization Project and Java Markov Chain Learning Tool. Department of Statistics, University of Auckland.
GitHub: https://github.com/D1OK1NG/Shiny-Visualization-Project

Contact
Author: Chunyi Wu
Institution: University of Auckland — Master of Professional Studies in Data Science
Department: Statistics
Supervisors: Dr. Azam Asanjarani, Dr. Heti Afimeimounga
Email: cwu112@aucklanduni.ac.nz
