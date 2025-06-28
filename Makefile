# Structura Makefile for testing and development

.PHONY: test test-unit test-integration test-all install-deps check clean help

# Default target
help:
	@echo "Available targets:"
	@echo "  test          - Run all tests"
	@echo "  test-unit     - Run unit tests only" 
	@echo "  test-integration - Run integration tests only"
	@echo "  install-deps  - Install required R packages"
	@echo "  check         - Run R CMD check"
	@echo "  clean         - Clean test artifacts"
	@echo "  run-app       - Start Shiny application"

# Install required dependencies
install-deps:
	@echo "Installing R package dependencies..."
	Rscript -e "install.packages(c('testthat', 'shinytest2', 'mockery', 'withr'), repos='https://cran.rstudio.com/')"

# Run all tests
test: test-unit test-integration

# Run unit tests only
test-unit:
	@echo "Running unit tests..."
	Rscript -e "testthat::test_dir('tests/testthat', filter='helpers|data-processing|error-handling', reporter='progress')"

# Run integration tests only  
test-integration:
	@echo "Running integration tests..."
	Rscript -e "if(requireNamespace('shinytest2', quietly=TRUE)) testthat::test_dir('tests/testthat', filter='app-integration', reporter='progress') else cat('shinytest2 not available, skipping integration tests\n')"

# Run all tests with detailed output
test-all:
	@echo "Running comprehensive test suite..."
	Rscript -e "testthat::test_dir('tests/testthat', reporter=c('progress', 'fail'))"

# Run R CMD check
check:
	@echo "Running R CMD check..."
	R CMD build .
	R CMD check --as-cran Structura_*.tar.gz

# Clean test artifacts
clean:
	@echo "Cleaning test artifacts..."
	rm -rf tests/testdata/test_*.csv
	rm -rf Structura_*.tar.gz
	rm -rf Structura.Rcheck/

# Start the Shiny application
run-app:
	@echo "Starting Structura Shiny application..."
	Rscript -e "shiny::runApp('.', host='0.0.0.0', port=8100, launch.browser=FALSE)"

# Run tests in Docker (optional)
test-docker:
	@echo "Building and testing in Docker..."
	docker build -t structura-test .
	docker run --rm structura-test Rscript -e "testthat::test_dir('tests/testthat')"