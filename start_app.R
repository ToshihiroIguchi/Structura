# Structura Application Startup Script
# Automatically uses port 8100 for development

# Set environment
Sys.setenv(SHINY_ENV = "development")

# Start application
shiny::runApp(host = "0.0.0.0", port = 8100)