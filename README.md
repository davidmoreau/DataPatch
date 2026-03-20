### DataPatch: Interactive Missing Data Simulation and Imputation

DataPatch is a comprehensive Shiny application designed to facilitate transparent and rigorous handling of missing data in psychological and behavioral research. This tool enables researchers to simulate missing data under different mechanisms, apply various imputation methods, and generate publication-ready documentation for methods sections, all within an intuitive graphical interface.

#### Core Functionalities

Data Management and Simulation
You can upload your own CSV datasets or use built-in sample data to begin analysis immediately. For experimental designs, the application provides sophisticated missing data simulation capabilities, allowing you to introduce missing values under three well-established mechanisms: Missing Completely at Random (MCAR), Missing at Random (MAR), and Missing Not at Random (MNAR). You control the percentage of missingness, enabling systematic investigation of how different amounts of missing data affect analytical outcomes.

Advanced Imputation Methods
DataPatch implements a comprehensive suite of missing data handling techniques suitable for various research contexts and statistical assumptions. Available methods include traditional approaches like listwise and pairwise deletion, mean substitution, and regression imputation, alongside modern computational techniques such as Multiple Imputation using Chained Equations (MICE), Bayesian Multiple Imputation via the Amelia package, Bayesian Data Augmentation using Gibbs sampling, K-Nearest Neighbor imputation, Random Forest imputation, Maximum Likelihood estimation, and Pattern Mixture Models. This range ensures appropriate methodological choices for different data structures and research questions.

Interactive Visualization and Diagnostics
The application provides multiple visualization panels to assess data quality and method performance. A dedicated missingness pattern visualization displays the distribution of missing values across variables, while interactive plots compare original and imputed values for most methods. For pairwise deletion analyses, an interactive correlation matrix heatmap reveals relationships between variables using available data. All visualizations include statistical annotations such as R-squared values where applicable, facilitating quantitative assessment of imputation accuracy.

Methods Report Generation
A distinctive feature of DataPatch is its automated methods report generation, which produces transparent documentation suitable for publication methods sections. The report includes study metadata, dataset characteristics, detailed descriptions of missing data handling procedures, quantitative results of the processing, methodological justification, and proper citation to established guidelines in the field. These reports can be downloaded as plain text files for direct inclusion in manuscripts or supplementary materials.

Enhanced Analytical Workflow
The application supports two primary workflows: simulating missing data to test methodological robustness, and imputing missing values in existing incomplete datasets. Each workflow provides appropriate controls and outputs, with the imputation workflow including data export functionality. The interface incorporates progress indicators for computationally intensive operations and comprehensive error handling with informative messages, ensuring a smooth analytical experience.

#### How to Use DataPatch

Initial Setup
Begin by uploading your CSV dataset using the file upload control or loading the provided sample data with a single click. The dataset will immediately display in tabular format with missing values visually highlighted for quick assessment.

Workflow Selection
Choose between "Simulate Missing Data" to introduce controlled missingness into your complete dataset, or "Impute Missing Data" to handle existing missing values in an incomplete dataset. For simulation workflows, specify the percentage of missing data and the missingness mechanism (MCAR, MAR, or MNAR) according to your research design.

Method Application
Select your preferred missing data handling technique from the comprehensive dropdown menu. Click "Process Data" to execute the selected method. The application will display the processed dataset, relevant visualizations, and statistical summaries across dedicated tabs.

Results Documentation
Navigate to the Methods Report tab and complete the study information fields (study name, author, dataset name) to contextualize your analysis. Click "Generate Methods Report" to create a comprehensive documentation of your missing data handling procedures. Download this report as a text file for manuscript submission.

Data Export
When working in the imputation workflow, use the download button to export your processed dataset as a CSV file for further analysis in external statistical software.

#### Scientific Applications

Research Transparency
DataPatch facilitates transparent reporting of missing data handling, a critical aspect of open science practices and methodological rigor in quantitative research. The automated reporting ensures consistent documentation across studies and research teams.

Methodological Education
The tool serves as an educational platform for students and early-career researchers to understand missing data mechanisms, explore consequences of different handling approaches, and develop methodological decision-making skills through hands-on experimentation.

Analytical Preparation
Researchers and analysts can efficiently prepare datasets for downstream analysis by comparing multiple imputation strategies, assessing their impact on data distributions and relationships, and selecting optimal approaches for their specific analytical needs.

Simulation Studies
Investigators conducting methodological research can use the simulation features to systematically examine how different missing data mechanisms and imputation methods affect statistical estimates and inferences under controlled conditions.

#### Methodological Foundation

DataPatch implements established missing data handling procedures aligned with current methodological standards in psychological research. The application references and supports compliance with reporting guidelines outlined in Moreau (2026), which emphasizes transparent documentation of missing data procedures as essential for research reproducibility and validity.

#### Technical Implementation

Built using the R Shiny framework, DataPatch integrates several specialized packages including mice for multiple imputation, Amelia for Bayesian imputation, VIM for K-nearest neighbor imputation, missForest for random forest imputation, and plotly for interactive visualizations. The application employs reactive programming to ensure immediate visual feedback as parameters change, creating an engaging analytical environment that balances computational power with user accessibility.

#### Access and Usage

DataPatch is designed for researchers across experience levels, from students learning about missing data concepts to experienced investigators preparing data for publication. The intuitive interface requires no programming knowledge, while the methodological sophistication supports rigorous research applications. Via the integration of simulation, analysis, visualization, and documentation in a single platform, DataPatch streamlines the missing data handling process from exploratory investigation to manuscript-ready reporting.
