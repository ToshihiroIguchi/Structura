library(testthat)
library(shiny)
library(shinytest2)

describe("Shiny App Integration Tests", {
  
  setup({
    if (!dir.exists("../../tests/testdata")) {
      dir.create("../../tests/testdata", recursive = TRUE)
    }
    
    test_data <- data.frame(
      x1 = rnorm(100, 5, 1),
      x2 = rnorm(100, 3, 0.8),
      x3 = rnorm(100, 4, 1.2),
      y1 = rnorm(100, 6, 1.1),
      y2 = rnorm(100, 5, 0.9),
      stringsAsFactors = FALSE
    )
    
    write.csv(test_data, "../../tests/testdata/sample_data.csv", row.names = FALSE)
  })
  
  describe("App Initialization", {
    
    it("launches successfully and shows initial modal", {
      # skip_on_cran()
      # skip_if_not_installed("shinytest2")
      
      app <- AppDriver$new(
        app_dir = "../..",
        name = "structura-test",
        timeout = 20000
      )
      
      expect_true(app$wait_for_idle())
      
      modal_visible <- app$get_js("$('.modal').is(':visible')")
      expect_true(modal_visible)
      
      app$stop()
    })
    
    it("accepts file upload and closes modal", {
      # skip_if_not_installed("shinytest2")
      
      app <- AppDriver$new(app_dir = "../..", timeout = 20000)
      
      app$upload_file(
        datafile = "tests/testdata/sample_data.csv"
      )
      app$wait_for_idle(timeout = 5000)
      
      modal_visible <- app$get_js("$('.modal').is(':visible')")
      expect_false(modal_visible)
      
      datatable_visible <- app$get_js("$('#datatable').length > 0")
      expect_true(datatable_visible)
      
      app$stop()
    })
    
    it("loads sample dataset correctly", {
      # skip_if_not_installed("shinytest2")
      
      app <- AppDriver$new(app_dir = "../..", timeout = 20000)
      
      app$set_inputs(sample_ds = "HolzingerSwineford1939")
      app$wait_for_idle(timeout = 5000)
      
      modal_visible <- app$get_js("$('.modal').is(':visible')")
      expect_false(modal_visible)
      
      datatable_rows <- app$get_js("$('#datatable tbody tr').length")
      expect_true(datatable_rows > 0)
      
      app$stop()
    })
  })
  
  describe("Data Processing Workflow", {
    
    it("filters and processes data correctly", {
      # skip_if_not_installed("shinytest2")
      
      app <- AppDriver$new(app_dir = "../..", timeout = 20000)
      
      app$upload_file(datafile = "tests/testdata/sample_data.csv")
      app$wait_for_idle()
      
      app$click("filtered-tab")
      app$wait_for_idle()
      
      display_columns_exists <- app$get_js("$('#display_columns').length > 0")
      expect_true(display_columns_exists)
      
      correlation_plot_exists <- app$get_js("$('#corr_heatmap').length > 0")
      expect_true(correlation_plot_exists)
      
      app$stop()
    })
    
    it("handles standardized vs raw analysis modes", {
      # skip_if_not_installed("shinytest2")
      
      app <- AppDriver$new(app_dir = "../..", timeout = 20000)
      
      app$set_inputs(sample_ds = "HolzingerSwineford1939")
      app$wait_for_idle()
      
      app$click("Model-tab")
      app$wait_for_idle()
      
      app$set_inputs(analysis_mode = "std")
      app$wait_for_idle()
      
      mode_value <- app$get_value(input = "analysis_mode")
      expect_equal(mode_value, "std")
      
      app$set_inputs(analysis_mode = "raw")
      app$wait_for_idle()
      
      diagram_std_visible <- app$get_js("$('#diagram_std').is(':visible')")
      expect_true(diagram_std_visible)
      
      app$stop()
    })
  })
  
  describe("Model Specification and Fitting", {
    
    it("builds and runs a complete SEM model", {
      # skip_if_not_installed("shinytest2")
      
      app <- AppDriver$new(app_dir = "../..", timeout = 30000)
      
      app$set_inputs(sample_ds = "HolzingerSwineford1939")
      app$wait_for_idle()
      
      app$click("Model-tab")
      app$wait_for_idle()
      
      app$set_inputs(analysis_mode = "std")
      app$wait_for_idle()
      
      app$click("add_row")
      app$wait_for_idle()
      
      app$click("run_model")
      app$wait_for_idle(timeout = 15000)
      
      lavaan_output_exists <- app$get_js("$('#lavaan_model').text().length > 0")
      expect_true(lavaan_output_exists)
      
      app$stop()
    })
    
    it("displays fit indices when model converges", {
      # skip_if_not_installed("shinytest2")
      
      app <- AppDriver$new(app_dir = "../..", timeout = 30000)
      
      app$set_inputs(sample_ds = "HolzingerSwineford1939")
      app$wait_for_idle()
      
      app$click("Model-tab")
      app$wait_for_idle()
      
      app$set_inputs(
        extra_eq = "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6"
      )
      app$wait_for_idle()
      
      app$click("run_model")
      app$wait_for_idle(timeout = 15000)
      
      fit_indices_exists <- app$get_js("$('#fit_indices').length > 0")
      expect_true(fit_indices_exists)
      
      app$click("right_tabs-Diagnostics-tab")
      app$wait_for_idle()
      
      diagnostics_visible <- app$get_js("$('#fit_indices').is(':visible')")
      expect_true(diagnostics_visible)
      
      app$stop()
    })
    
    it("handles model fitting errors gracefully", {
      # skip_if_not_installed("shinytest2")
      
      app <- AppDriver$new(app_dir = "../..", timeout = 20000)
      
      app$set_inputs(sample_ds = "HolzingerSwineford1939")
      app$wait_for_idle()
      
      app$click("Model-tab")
      app$wait_for_idle()
      
      app$set_inputs(extra_eq = "invalid_model ~ nonexistent_var")
      app$wait_for_idle()
      
      app$click("run_model")
      app$wait_for_idle(timeout = 10000)
      
      error_alert_visible <- app$get_js("$('#fit_alert_box').is(':visible')")
      expect_true(error_alert_visible)
      
      app$stop()
    })
  })
  
  describe("Visualization and Output", {
    
    it("generates path diagram for valid model", {
      # skip_if_not_installed("shinytest2")
      
      app <- AppDriver$new(app_dir = "../..", timeout = 30000)
      
      app$set_inputs(sample_ds = "HolzingerSwineford1939")
      app$wait_for_idle()
      
      app$click("Model-tab")
      app$wait_for_idle()
      
      app$set_inputs(
        extra_eq = "visual =~ x1 + x2\ntextual =~ x4 + x5"
      )
      app$wait_for_idle()
      
      app$click("run_model")
      app$wait_for_idle(timeout = 15000)
      
      diagram_exists <- app$get_js("$('#sem_plot').length > 0")
      expect_true(diagram_exists)
      
      app$stop()
    })
    
    it("shows detailed results in Details tab", {
      # skip_if_not_installed("shinytest2")
      
      app <- AppDriver$new(app_dir = "../..", timeout = 30000)
      
      app$set_inputs(sample_ds = "HolzingerSwineford1939")
      app$wait_for_idle()
      
      app$click("Model-tab")
      app$wait_for_idle()
      
      app$set_inputs(extra_eq = "visual =~ x1 + x2 + x3")
      app$wait_for_idle()
      
      app$click("run_model")
      app$wait_for_idle(timeout = 15000)
      
      app$click("Details-tab")
      app$wait_for_idle()
      
      param_table_exists <- app$get_js("$('#param_tbl').length > 0")
      expect_true(param_table_exists)
      
      fit_summary_exists <- app$get_js("$('#fit_summary').text().length > 0")
      expect_true(fit_summary_exists)
      
      app$stop()
    })
    
    it("updates diagram layout options correctly", {
      # skip_if_not_installed("shinytest2")
      
      app <- AppDriver$new(app_dir = "../..", timeout = 30000)
      
      app$set_inputs(sample_ds = "HolzingerSwineford1939")
      app$wait_for_idle()
      
      app$click("Model-tab")
      app$wait_for_idle()
      
      app$set_inputs(extra_eq = "visual =~ x1 + x2")
      app$wait_for_idle()
      
      app$click("run_model")
      app$wait_for_idle(timeout = 15000)
      
      app$click("right_tabs-Diagram Settings-tab")
      app$wait_for_idle()
      
      app$set_inputs(layout_style = "dot_TB")
      app$wait_for_idle()
      
      layout_value <- app$get_value(input = "layout_style")
      expect_equal(layout_value, "dot_TB")
      
      app$stop()
    })
  })
  
  teardown({
    if (file.exists("../../tests/testdata/sample_data.csv")) {
      unlink("../../tests/testdata/sample_data.csv")
    }
    if (dir.exists("../../tests/testdata")) {
      unlink("../../tests/testdata", recursive = TRUE)
    }
  })
})