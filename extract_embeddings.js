// Google Earth Engine Script: Extract Satellite Embeddings for Electoral Analysis
// This script computes mean satellite embeddings for precinct and county boundaries
// to predict Trump vote share in the 2020 presidential election

// =============================================================================
// CONFIGURATION
// =============================================================================

// Set the scale for reduceRegion operations (meters)
var SCALE = 30;  // Adjust based on embedding resolution

// Maximum pixels for reduceRegion (prevents memory errors)
var MAX_PIXELS = 1e13;

// Export settings
var EXPORT_FOLDER = 'electoral_embeddings';  // Google Drive folder name

// =============================================================================
// LOAD SATELLITE EMBEDDINGS
// =============================================================================

// Load the satellite embeddings ImageCollection
var embeddings = ee.ImageCollection("GOOGLE/SATELLITE_EMBEDDING/V1/ANNUAL");

// Get the most recent year available (or specific year if needed)
var latestYear = embeddings.aggregate_max('system:time_start');
var embeddingsLatest = embeddings.filter(ee.Filter.eq('system:time_start', latestYear));

// Reduce the ImageCollection to a single composite image
// Using median to reduce noise, but mean() could also be used
var embeddingsComposite = embeddingsLatest.median();

// Print information about the embeddings
print('Embeddings composite bands:', embeddingsComposite.bandNames());
print('Embeddings composite:', embeddingsComposite);

// =============================================================================
// PRECINCT-LEVEL EXTRACTION
// =============================================================================

// Function to extract embeddings for precinct boundaries
function extractPrecinctEmbeddings() {
  
  // Load precinct boundaries from uploaded asset
  // NOTE: You need to upload your precinct shapefiles to GEE first
  // Replace 'users/your_username/precincts_2020' with your actual asset path
  var precincts = ee.FeatureCollection('users/your_username/precincts_2020');
  
  // Alternative: If you have individual state precinct files
  // var precincts = ee.FeatureCollection('users/your_username/precincts_ca_2020');
  
  print('Number of precincts:', precincts.size());
  
  // Function to compute mean embeddings for a single precinct
  function computePrecinctEmbeddings(feature) {
    
    // Calculate mean embedding values within the precinct geometry
    var meanValues = embeddingsComposite.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: feature.geometry(),
      scale: SCALE,
      maxPixels: MAX_PIXELS,
      tileScale: 4  // Helps with memory management
    });
    
    // Add statistics for robustness (min, max, stdDev)
    var statsValues = embeddingsComposite.reduceRegion({
      reducer: ee.Reducer.minMax().combine({
        reducer2: ee.Reducer.stdDev(),
        sharedInputs: true
      }),
      geometry: feature.geometry(),
      scale: SCALE,
      maxPixels: MAX_PIXELS,
      tileScale: 4
    });
    
    // Combine mean and statistics
    var allValues = meanValues.combine(statsValues);
    
    // Return feature with embedding properties
    return feature.set(allValues);
  }
  
  // Apply the function to all precincts
  var precinctsWithEmbeddings = precincts.map(computePrecinctEmbeddings);
  
  // Export precinct results
  Export.table.toDrive({
    collection: precinctsWithEmbeddings,
    description: 'Precinct_Embeddings_2020',
    folder: EXPORT_FOLDER,
    fileFormat: 'CSV',
    selectors: null  // Export all properties
  });
  
  print('Precinct embeddings export started');
  
  return precinctsWithEmbeddings;
}

// =============================================================================
// COUNTY-LEVEL EXTRACTION
// =============================================================================

// Function to extract embeddings for county boundaries
function extractCountyEmbeddings() {
  
  // Load US counties from TIGER dataset
  var counties = ee.FeatureCollection('TIGER/2018/Counties');
  
  print('Number of counties:', counties.size());
  
  // Function to compute mean embeddings for a single county
  function computeCountyEmbeddings(feature) {
    
    // Calculate mean embedding values within the county geometry
    var meanValues = embeddingsComposite.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: feature.geometry(),
      scale: SCALE,
      maxPixels: MAX_PIXELS,
      tileScale: 4
    });
    
    // Add statistics for robustness
    var statsValues = embeddingsComposite.reduceRegion({
      reducer: ee.Reducer.minMax().combine({
        reducer2: ee.Reducer.stdDev(),
        sharedInputs: true
      }),
      geometry: feature.geometry(),
      scale: SCALE,
      maxPixels: MAX_PIXELS,
      tileScale: 4
    });
    
    // Combine mean and statistics
    var allValues = meanValues.combine(statsValues);
    
    // Return feature with embedding properties
    return feature.set(allValues);
  }
  
  // Apply the function to all counties
  var countiesWithEmbeddings = counties.map(computeCountyEmbeddings);
  
  // Export county results
  Export.table.toDrive({
    collection: countiesWithEmbeddings,
    description: 'County_Embeddings_2020',
    folder: EXPORT_FOLDER,
    fileFormat: 'CSV',
    selectors: null  // Export all properties
  });
  
  print('County embeddings export started');
  
  return countiesWithEmbeddings;
}

// =============================================================================
// SAMPLE EXTRACTION (for testing)
// =============================================================================

// Function to extract embeddings for a sample of features (useful for testing)
function extractSampleEmbeddings(sampleSize) {
  
  var counties = ee.FeatureCollection('TIGER/2018/Counties');
  
  // Take a random sample
  var sampleCounties = counties.randomColumn('random').limit(sampleSize);
  
  print('Extracting embeddings for', sampleSize, 'random counties');
  
  // Function to compute embeddings for sample
  function computeSampleEmbeddings(feature) {
    var meanValues = embeddingsComposite.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: feature.geometry(),
      scale: SCALE,
      maxPixels: MAX_PIXELS,
      tileScale: 4
    });
    
    return feature.set(meanValues);
  }
  
  var sampleWithEmbeddings = sampleCounties.map(computeSampleEmbeddings);
  
  // Export sample results
  Export.table.toDrive({
    collection: sampleWithEmbeddings,
    description: 'Sample_County_Embeddings_2020',
    folder: EXPORT_FOLDER,
    fileFormat: 'CSV'
  });
  
  print('Sample embeddings export started');
  
  return sampleWithEmbeddings;
}

// =============================================================================
// EXECUTION
// =============================================================================

// Uncomment the functions you want to run:

// 1. Extract embeddings for all counties (recommended to start here)
extractCountyEmbeddings();

// 2. Extract embeddings for sample counties (for testing)
// extractSampleEmbeddings(100);

// 3. Extract embeddings for precincts (requires uploaded precinct data)
// extractPrecinctEmbeddings();

// =============================================================================
// VISUALIZATION (optional)
// =============================================================================

// Visualize a subset of embeddings on the map
function visualizeEmbeddings() {
  
  // Select a few embedding bands for visualization
  var visualizationBands = ['embedding_0', 'embedding_1', 'embedding_2'];
  
  // Create RGB composite from first 3 embedding bands
  var rgbComposite = embeddingsComposite.select(visualizationBands);
  
  // Add to map
  Map.addLayer(rgbComposite, {
    min: 0,
    max: 1,
    gamma: 1.5
  }, 'Satellite Embeddings RGB');
  
  // Add county boundaries
  var counties = ee.FeatureCollection('TIGER/2018/Counties');
  Map.addLayer(counties.style({color: 'white', fillColor: '00000000'}), {}, 'County Boundaries');
  
  // Center map on US
  Map.setCenter(-98, 39, 4);
}

// Uncomment to visualize embeddings
// visualizeEmbeddings();

print('Script completed. Check the Tasks tab for export progress.');
print('Exported files will appear in your Google Drive folder:', EXPORT_FOLDER);
