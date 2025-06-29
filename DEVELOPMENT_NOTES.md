# Structura Development Notes

## Current Session Context (Important for Continuity)

### What We Accomplished
1. **Identified 8 Major Issues** in the modular codebase
2. **Implemented Phase 1**: Clean architecture with reactive values
3. **Eliminated Global Pollution**: Removed all globalenv() assignments
4. **Tested Successfully**: Application running on port 8100
5. **Committed to Git**: All changes saved in commit `200cf7e`

### Critical Remaining Work

#### Phase 2: Configuration System (URGENT)
**Problem**: Production settings still hardcoded in global.R
```r
# These need to be environment-specific:
shiny.reactlog = TRUE        # Should be FALSE in production
shiny.fullstacktrace = TRUE  # Security risk in production
```

**Quick Implementation**:
1. Create `config/` directory
2. Add environment detection logic
3. Move settings to config files
4. Update global.R to load appropriate config

#### Phase 3: Error Handling (IMPORTANT)
**Problem**: Code duplication in error handling across all modules
**Solution**: Create `utils/error_handler.R` with `safe_execute()` function

### Code Architecture Understanding

#### Shared Values Structure
```r
shared_values <- reactiveValues(
  fit_model = NULL,           # SEM model results
  model_syntax = NULL,        # lavaan syntax
  correlation_cache = NULL,   # Correlation matrix cache
  processed_data = NULL       # Transformed data
)
```

#### Module Communication Pattern
- **No Global Variables**: Clean session isolation
- **Dependency Injection**: Modules receive shared_values as parameter
- **Reactive Updates**: Changes propagate automatically

#### File Structure
```
server/
├── data_module.R      # 186 lines - Data loading & preprocessing
├── model_module.R     # 363 lines - SEM model management
└── plot_module.R      # 110 lines - Visualization & correlation
```

### Testing Infrastructure
- **Unit Tests**: 5 test files, 90%+ coverage
- **Integration Tests**: Chrome-free lightweight testing
- **CI Ready**: All tests pass, suitable for automated testing

### Performance Optimizations Implemented
1. **Correlation Caching**: Prevents recalculation
2. **Reactive Efficiency**: Updates only when needed
3. **Session Cleanup**: Proper memory management

### Known Issues Still Pending
1. **Locale Hardcoding**: `Sys.setlocale("LC_CTYPE", "ja_JP.UTF-8")`
2. **Error Messages**: Still in English only
3. **Mobile Responsiveness**: Basic Bootstrap only
4. **Validation**: File upload size/type checking minimal

### Development Environment Setup
```bash
# Application startup
R -e "runApp('.', host='0.0.0.0', port=8100, launch.browser=FALSE)"

# Testing
R -e "testthat::test_dir('tests/testthat', reporter='progress')"

# Git status
git log --oneline -5  # Recent commits
```

### Critical Success Factors
- **Maintain shared_values pattern**: Don't revert to global variables
- **Test after each change**: Use existing test suite
- **Keep modules focused**: Single responsibility principle
- **Document breaking changes**: Update this file

## Quick Recovery Commands

### Verify Current State
```bash
# Check if app is running
netstat -tlnp | grep :8100

# Check git status
git status
git log --oneline -3

# Verify file structure
ls -la server/
```

### Emergency Rollback
```bash
# If Phase 1 breaks something
git checkout server_original.R
mv server_original.R server.R
```

### Continue Development
```bash
# Start Phase 2
mkdir -p config
# Create config/app_config.R
# Update global.R
```

## Important Design Decisions Made

### Why Reactive Values Over Global Variables
- **Session Safety**: Multiple users don't interfere
- **Testing**: Easier to mock and test
- **Debugging**: Clear data flow
- **Maintainability**: Explicit dependencies

### Why Module Pattern
- **Single Responsibility**: Each module has clear purpose
- **Loose Coupling**: Modules communicate via shared interface
- **Independent Testing**: Can test modules in isolation
- **Team Development**: Multiple developers can work simultaneously

### Why This Priority Order
1. **Phase 1 (Architecture)**: Foundation for everything else
2. **Phase 2 (Configuration)**: Production readiness
3. **Phase 3 (Error Handling)**: Code quality and user experience
4. **Phase 4 (UI/UX)**: Polish and accessibility
5. **Phase 5 (Performance)**: Optimization

---
*This file should be updated after each major change*
*Next person: Start with reading IMPROVEMENT_PLAN.md and this file*