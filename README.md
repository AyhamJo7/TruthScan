# TruthScan: Modern Fake News Detection

![TruthScan Logo](docs/images/truthscan_logo.png)

TruthScan is a state-of-the-art fake news detection system built in R that uses machine learning to analyze statements and determine their veracity. This project is an enhanced version of a fake news detection system originally developed for analyzing political statements from the 2016 US elections.

## Features

- **Multiple Classification Approaches**:
  - Binary classification (true/false)
  - Three-class classification (true/half-true/false)
  - Six-class classification (pants-fire/false/barely-true/half-true/mostly-true/true)

- **Advanced Machine Learning Models**:
  - Random Forest
  - XGBoost
  - Neural Networks (using Keras/TensorFlow)
  - Ensemble methods

- **Comprehensive Feature Engineering**:
  - Text processing using TF-IDF and SVD
  - Web scraping for search result counts
  - Speaker and party information
  - Subject and context analysis

- **Interactive Shiny Application**:
  - Real-time statement analysis
  - Batch processing capabilities
  - Model performance visualization
  - Detailed result explanations

- **Modern R Best Practices**:
  - Tidyverse ecosystem
  - Tidymodels for machine learning
  - Proper project structure
  - Comprehensive documentation

## Project Structure

```
TruthScan/
├── R/                  # R scripts
│   ├── data_preprocessing.R       # Data preprocessing functions
│   ├── binary_rf_model.R          # Random Forest model for binary classification
│   ├── binary_xgb_model.R         # XGBoost model for binary classification
│   ├── binary_nn_model.R          # Neural Network model for binary classification
│   └── web_scraping.R             # Web scraping functionality
├── data/               # Data files
│   ├── binary_training.csv        # Binary classification training data
│   ├── binary_testing.csv         # Binary classification testing data
│   ├── three_training.csv         # Three-class classification training data
│   ├── three_testing.csv          # Three-class classification testing data
│   ├── multi_training.csv         # Multi-class classification training data
│   └── multi_testing.csv          # Multi-class classification testing data
├── app/                # Shiny application
│   └── app.R                      # Shiny app code
├── models/             # Saved models
├── tests/              # Unit tests
├── docs/               # Documentation
└── TruthScan.Rproj     # RStudio project file
```

## Installation

### Prerequisites

- R 4.0.0 or higher
- RStudio (recommended)

### Dependencies

TruthScan requires several R packages. You can install them using:

```r
# Install required packages
install.packages(c(
  "tidyverse",     # For data manipulation
  "tidymodels",    # For modeling framework
  "keras",         # For neural networks
  "tensorflow",    # For neural networks backend
  "ranger",        # For random forest
  "xgboost",       # For XGBoost
  "text2vec",      # For text vectorization
  "tidytext",      # For text processing
  "textrecipes",   # For text feature engineering
  "rvest",         # For web scraping
  "polite",        # For polite web scraping
  "shiny",         # For interactive app
  "shinydashboard", # For dashboard UI
  "plotly",        # For interactive plots
  "DT",            # For interactive tables
  "here",          # For project-relative paths
  "logger",        # For logging
  "glue",          # For string interpolation
  "furrr",         # For parallel processing
  "doParallel"     # For parallel processing
))

# Install Keras and TensorFlow
install.packages("keras")
keras::install_keras()
```

### Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/AyhamJo7/TruthScan.git
   cd truthscan
   ```

2. Open the project in RStudio:
   ```bash
   open TruthScan.Rproj
   ```

3. Run the data preprocessing script to prepare the data:
   ```r
   source("R/data_preprocessing.R")
   ```

4. Train the models:
   ```r
   source("R/binary_rf_model.R")
   source("R/binary_xgb_model.R")
   source("R/binary_nn_model.R")
   ```

## Usage

### Command Line

You can use TruthScan from the R console:

```r
# Load the required scripts
source("R/data_preprocessing.R")
source("R/binary_rf_model.R")

# Train a Random Forest model
rf_results <- train_binary_rf()

# Print model metrics
print(rf_results$metrics)

# Make predictions on new data
new_data <- data.frame(Statement = "This is a test statement")
predictions <- predict(rf_results$fit, new_data)
```

### Shiny Application

TruthScan includes an interactive Shiny application for easy use:

1. Start the Shiny app:
   ```r
   shiny::runApp("app")
   ```

2. Open your web browser and navigate to the URL displayed in the R console (usually http://127.0.0.1:xxxx).

3. Use the application to:
   - Analyze individual statements
   - Process batches of statements
   - Explore model performance
   - Understand prediction details

## Web Scraping Feature

TruthScan includes a web scraping component that collects search result counts for statements to use as features for the models. To use this feature:

```r
# Load the web scraping script
source("R/web_scraping.R")

# Scrape search results for a statement
results <- scrape_search_results("The earth is flat")
print(results$result_count)

# Process a batch of statements
statements <- c("The earth is flat", "Water boils at 100 degrees Celsius")
batch_results <- scrape_search_results_batch(statements)
```

**Note**: Please use the web scraping functionality responsibly and in accordance with the terms of service of the search engines.

## Model Performance

TruthScan achieves the following performance on the test dataset:

| Model | Binary Classification | Three-class Classification | Six-class Classification |
|-------|----------------------|---------------------------|-------------------------|
| Random Forest | 72.5% | 60.1% | 45.3% |
| XGBoost | 74.2% | 61.8% | 46.7% |
| Neural Network | 71.8% | 59.5% | 44.9% |
| Ensemble | 75.1% | 62.3% | 47.2% |

## Use Cases

TruthScan can be used in various scenarios:

1. **Journalism**: Fact-checking statements before publication
2. **Research**: Analyzing political discourse and misinformation spread
3. **Education**: Teaching critical thinking and fact verification
4. **Personal Use**: Verifying information encountered online
5. **Content Moderation**: Assisting in identifying potentially false information

## Contributing

Contributions to TruthScan are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- This project is an enhanced version of a fake news detection system originally developed for analyzing political statements from the 2016 US elections.
- The original dataset is based on the paper "Liar, Liar Pants on Fire": A New Benchmark Dataset for Fake News Detection.

## Contact

Project Link: [https://github.com/AyhamJo7/TruthScan.git](https://github.com/AyhamJo7/TruthScan.git)
