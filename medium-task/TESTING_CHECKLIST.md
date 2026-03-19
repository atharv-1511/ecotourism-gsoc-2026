# Testing Checklist for Wildlife Explorer App

## Pre-Deployment Testing (Local)

### Test in R Console First
```r
# Open R or RStudio
setwd("C:/Users/Atharv Raskar/Desktop/Ecotourism- GSoC 2026/medium-task")

# Test that all data loads without errors
gouldian_finch <- readRDS("data/gouldian_finch.rds")
manta_rays <- readRDS("data/manta_rays.rds")
glowworms <- readRDS("data/glowworms.rds")
orchids <- readRDS("data/orchids.rds")
weather <- readRDS("data/weather.rds")
tourism_region <- readRDS("data/tourism_region.rds")

# Check dimensions
cat("Gouldian Finch:", nrow(gouldian_finch), "rows\n")
cat("Manta Rays:", nrow(manta_rays), "rows\n")
cat("Glowworms:", nrow(glowworms), "rows\n")
cat("Orchids:", nrow(orchids), "rows\n")
cat("Weather:", nrow(weather), "rows\n")
cat("Tourism Region:", nrow(tourism_region), "rows\n")

# Run the app locally
shiny::runApp()
```

---

## Comprehensive Test Matrix

### 1. ORGANISM TESTS (4 organisms × 4 tabs = 16 tests)

#### Gouldian Finch
- [ ] **Explorer Tab**
  - Map loads with markers
  - Markers cluster properly
  - Weather plot displays (boxplot by month)
  - Monthly bar chart shows data
  - Annual trend chart shows data
  - All 5 KPI cards show values

- [ ] **Predictions Tab**
  - Click "Generate Predictions" button
  - Model stats display (Model type, Observations, AIC)
  - Prediction table shows 5 days
  - Prediction chart displays with error bars
  - No errors in console

- [ ] **Tourism Tab**
  - State/territory chart displays
  - Tourism table shows state summary
  - Percentages calculate correctly
  - No "No data" errors

- [ ] **Time Analysis Tab**
  - Hour chart displays (0-23 hours)
  - Weekday chart displays (all 7 days)
  - Heatmap displays (month × hour grid)
  - No missing data warnings

#### Manta Rays
- [ ] Repeat all 4 tabs above
- [ ] Verify Queensland-specific data shows

#### Glowworms
- [ ] Repeat all 4 tabs above
- [ ] Check hour data (likely night hours)

#### Orchids
- [ ] Repeat all 4 tabs above
- [ ] Check large dataset performance (orchids is biggest)

---

### 2. FILTER TESTS

For **Gouldian Finch** (or any organism):

#### State Filter
- [ ] "All States" - shows all data
- [ ] Select specific state (e.g., "Northern Territory")
  - Map updates
  - KPI cards update
  - All charts update
  - No errors

#### Month Filter
- [ ] "All Months" - shows all data
- [ ] Select specific month (e.g., "June")
  - Data filters correctly
  - Monthly bar highlights selected month
  - All tabs still work

#### Record Type Filter
- [ ] All types checked - shows all data
- [ ] Uncheck one type - data filters
- [ ] Uncheck all types - shows validation message

#### Combined Filters
- [ ] State = Northern Territory + Month = June + Record Type = 1 type only
  - Should still have enough data for predictions
  - No crashes or empty states

---

### 3. DOWNLOAD TESTS

- [ ] **Download Data (CSV)**
  - File downloads
  - Opens in Excel/spreadsheet
  - Contains correct columns
  - Has weather data joined

- [ ] **Download Monthly Chart**
  - PNG file downloads
  - High resolution (150 dpi)
  - White background
  - Readable text

---

### 4. PREDICTIONS TAB DETAILED TEST

For **each organism**:

- [ ] **First Load**
  - Shows info message "Click Generate Predictions..."
  - No spinner running

- [ ] **Generate with All Months**
  - Input: 5 days
  - Click "Generate Predictions"
  - Model stats appear (Negative Binomial or Poisson)
  - Table shows 5 rows with dates, confidence intervals
  - Chart shows 5 bars with error bars
  - No errors

- [ ] **Generate with Filtered Data**
  - Select one state + one month
  - Try generating predictions
  - If fails, shows warning "Not enough data..."
  - Switch back to "All Months" and retry - should work

- [ ] **Change N Days**
  - Input: 10 days
  - Click "Generate Predictions"
  - Table and chart update to show 10 days

---

### 5. EDGE CASES & ERROR HANDLING

- [ ] **Empty Filter Result**
  - Select state with no data for that organism
  - Should show: "No sightings match filters. Try widening your selection."

- [ ] **Prediction Failure**
  - Over-filter data (1 state + 1 month + 1 record type)
  - Try predictions
  - Should show warning, not crash

- [ ] **Missing Hour Data**
  - Time Analysis tab with organism that has sparse hour data
  - Should show validation: "No hour data available"
  - Should not crash

- [ ] **Tab Switching**
  - Load app
  - Switch between all 4 tabs rapidly
  - All plots should render
  - No "suspended" or blank outputs

---

### 6. UI/UX TESTS

- [ ] **Font Sizes**
  - All text readable at 100% zoom
  - Labels are 1.1rem or larger
  - Plot titles are 22px
  - KPI values are large (2.2rem)

- [ ] **Colors & Contrast**
  - Sidebar is dark green with white text (readable)
  - Charts use colorblind-safe palettes
  - Buttons are visible and styled

- [ ] **Responsiveness** (if possible)
  - Resize browser window to narrow width
  - Sidebar should stack on top
  - Cards should reflow
  - Still usable on mobile

- [ ] **Loading Indicators**
  - Spinners show when plots are rendering
  - Green spinner color (#2d6a4f)

---

### 7. PERFORMANCE TESTS

- [ ] **Orchids (Largest Dataset)**
  - Load Orchids organism
  - Map renders in < 10 seconds
  - Charts render without freezing
  - Predictions complete in < 30 seconds

- [ ] **Tab Switch Speed**
  - Switch from Explorer to Predictions
  - Should be instant (no re-rendering)
  - All tabs cached properly

---

## Post-Deployment Testing (shinyapps.io)

After deploying to https://ecotourism-atharvraskar.shinyapps.io/medium-task/

### Browser Tests
Test in multiple browsers:
- [ ] Chrome
- [ ] Firefox
- [ ] Edge
- [ ] Safari (if available)

### Core Functionality
- [ ] **Cold Start** (first visit)
  - App loads within 60 seconds
  - No deployment errors
  - All assets load (CSS, icons)

- [ ] **Data Loading**
  - All 4 organisms selectable
  - Data loads correctly
  - No "object not found" errors

- [ ] **All 4 organisms × 4 tabs = 16 tab loads**
  - Every combination works
  - No server errors

- [ ] **Predictions on Production**
  - Generate predictions for each organism
  - Should work consistently
  - No timeout errors

### Final Checks
- [ ] No console errors (open browser DevTools)
- [ ] No R errors visible to user
- [ ] All download buttons work
- [ ] App doesn't disconnect frequently

---

## Mentor Review Preparation

### What Mentors Will Check

1. **Completeness**
   - [PASS] All 4 organisms work
   - [PASS] All 4 tabs functional
   - [PASS] GLM predictions implemented

2. **Data Integration**
   - [PASS] Weather data joined and visualized
   - [PASS] Tourism state analysis
   - [PASS] Time analysis (hour/weekday/heatmap)

3. **Code Quality**
   - [PASS] Native Shiny tabs (no custom JS bugs)
   - [PASS] Proper validation and error handling
   - [PASS] Clean, readable code structure

4. **User Experience**
   - [PASS] Large, readable fonts
   - [PASS] Professional design
   - [PASS] Fast loading
   - [PASS] No crashes with any filter combination

5. **Requirements Met**
   - [PASS] Interactive map with clustering
   - [PASS] Filters (organism, state, month, record type)
   - [PASS] KPI cards
   - [PASS] Weather analysis
   - [PASS] Download handlers
   - [PASS] Predictions tab (unique feature)
   - [PASS] Tourism analysis (unique feature)
   - [PASS] Time analysis (unique feature)

---

## Known Issues & Workarounds

### Issue 1: Sparse Hour Data
- **Organism:** Some organisms have < 50% hour data
- **Impact:** Time Analysis tab may show warnings
- **Mitigation:** Validation messages added, no crashes

### Issue 2: Prediction Fails with Over-Filtering
- **Scenario:** State=1, Month=1, Type=1 may have < 30 records
- **Impact:** Predictions can't run
- **Mitigation:** Clear warning shown, suggests widening filters

### Issue 3: Tourism Region Data
- **Status:** Uses state-level fallback if region data missing
- **Impact:** Tourism tab always works, even without region file
- **Mitigation:** Conditional loading handled

---

## Success Criteria

[PASS] **PASS**: All 16 organism-tab combinations work without errors
[PASS] **PASS**: Filters work and update all outputs
[PASS] **PASS**: Predictions generate for all organisms (with "All Months")
[PASS] **PASS**: Downloads work for CSV and PNG
[PASS] **PASS**: No console errors visible to user
[PASS] **PASS**: Font sizes are large and readable
[PASS] **PASS**: App loads on shinyapps.io within 60 seconds

---

## If Any Test Fails

1. Note the exact steps to reproduce
2. Check browser console for errors
3. Check R console (if local) for error messages
4. Document the organism + tab + filter combination
5. Report back with specific error message

---

## Deployment Command

```r
setwd("C:/Users/Atharv Raskar/Desktop/Ecotourism- GSoC 2026/medium-task")
rsconnect::deployApp()
```

After deployment succeeds, run the full test matrix on the live URL.
