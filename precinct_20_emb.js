// ============================================================
// 1. CONFIGURATION
// ============================================================

// Base path to your assets
var projectPath = 'projects/embeddings-477220/assets/precinct_20/';

// List of all states to process
var states = [
  {abbr: 'ak', asset: projectPath + 'ak_2020'},
  {abbr: 'al', asset: projectPath + 'al_2020'},
  {abbr: 'ar', asset: projectPath + 'ar_2020'},
  {abbr: 'az', asset: projectPath + 'az_2020'},
  {abbr: 'ca', asset: projectPath + 'ca_2020'},
  {abbr: 'co', asset: projectPath + 'co_2020'},
  {abbr: 'ct', asset: projectPath + 'ct_2020'},
  {abbr: 'dc', asset: projectPath + 'dc_2020'},
  {abbr: 'de', asset: projectPath + 'de_2020'},
  {abbr: 'fl', asset: projectPath + 'fl_2020'},
  {abbr: 'ga', asset: projectPath + 'ga_2020'},
  {abbr: 'hi', asset: projectPath + 'hi_2020'},
  {abbr: 'ia', asset: projectPath + 'ia_2020'},
  {abbr: 'id', asset: projectPath + 'id_2020'},
  {abbr: 'il', asset: projectPath + 'il_2020'},
  {abbr: 'in', asset: projectPath + 'in_2020'},
  {abbr: 'ks', asset: projectPath + 'ks_2020'},
  {abbr: 'ky', asset: projectPath + 'ky_2020'},
  {abbr: 'la', asset: projectPath + 'la_2020'},
  {abbr: 'ma', asset: projectPath + 'ma_2020'},
  {abbr: 'md', asset: projectPath + 'md_2020'},
  {abbr: 'me', asset: projectPath + 'me_2020'},
  {abbr: 'mi', asset: projectPath + 'mi_2020'},
  {abbr: 'mn', asset: projectPath + 'mn_2020'},
  {abbr: 'mo', asset: projectPath + 'mo_2020'},
  {abbr: 'ms', asset: projectPath + 'ms_2020'},
  {abbr: 'mt', asset: projectPath + 'mt_2020'},
  {abbr: 'nc', asset: projectPath + 'nc_2020'},
  {abbr: 'nd', asset: projectPath + 'nd_2020'},
  {abbr: 'ne', asset: projectPath + 'ne_2020'},
  {abbr: 'nh', asset: projectPath + 'nh_2020'},
  {abbr: 'nj', asset: projectPath + 'nj_2020'},
  {abbr: 'nm', asset: projectPath + 'nm_2020'},
  {abbr: 'nv', asset: projectPath + 'nv_2020'},
  {abbr: 'ny', asset: projectPath + 'ny_2020'},
  {abbr: 'oh', asset: projectPath + 'oh_2020'},
  {abbr: 'ok', asset: projectPath + 'ok_2020'},
  {abbr: 'or', asset: projectPath + 'or_2020'},
  {abbr: 'pa', asset: projectPath + 'pa_2020'},
  {abbr: 'ri', asset: projectPath + 'ri_2020'},
  {abbr: 'sc', asset: projectPath + 'sc_2020'},
  {abbr: 'sd', asset: projectPath + 'sd_2020'},
  {abbr: 'tn', asset: projectPath + 'tn_2020'},
  {abbr: 'tx', asset: projectPath + 'tx_2020'},
  {abbr: 'ut', asset: projectPath + 'ut_2020'},
  {abbr: 'va', asset: projectPath + 'va_2020'},
  {abbr: 'vt', asset: projectPath + 'vt_2020'},
  {abbr: 'wa', asset: projectPath + 'wa_2020'},
  {abbr: 'wi', asset: projectPath + 'wi_2020'},
  {abbr: 'wv', asset: projectPath + 'wv_2020'},
  {abbr: 'wy', asset: projectPath + 'wy_2020'}
];

// Set Processing Parameters
// Scale 150m is a good balance for embeddings (native is ~64m, but 150m aggregates well)
var scale = 150; 
var tileScale = 8; // High tileScale prevents "Out of Memory" errors on complex calculations

// ============================================================
// 2. PREPARE IMAGE & REDUCERS
// ============================================================

// Load 2020 Embeddings
var embeddings = ee.ImageCollection('GOOGLE/SATELLITE_EMBEDDING/V1/ANNUAL');
var embeddings2020 = embeddings
  .filter(ee.Filter.calendarRange(2020, 2020, 'year'))
  .mosaic();

print('Input Image:', embeddings2020);

// Create Combined Reducer (Mean, StdDev, Skew)
// This runs efficiently by sharing inputs
var combinedReducer = ee.Reducer.mean()
  .combine({
    reducer2: ee.Reducer.stdDev(),
    sharedInputs: true
  })
  .combine({
    reducer2: ee.Reducer.skew(),
    sharedInputs: true
  });

// ============================================================
// 3. CORE PROCESSING FUNCTION
// ============================================================

function processState(st) {
  var abbr = st.abbr;
  var asset = st.asset;
  
  // Load the state's precincts
  var precincts = ee.FeatureCollection(asset);

  // A. Reduce Regions (Extract Embedding Stats)
  // This adds columns like 'embedding_0_mean', 'embedding_0_stdDev', etc.
  var reduced = embeddings2020.reduceRegions({
    collection: precincts,
    reducer: combinedReducer,
    scale: scale,
    tileScale: tileScale
  });

  // B. Post-Process (Add Area, Total Votes, Clean Up)
  var processed = reduced.map(function(f) {
    
    // 1. Calculate Area (Must be done BEFORE dropping geometry)
    //    maxError: 10 improves speed on complex polygons
    var areaSqMeters = f.area(10); 
    
    // 2. Calculate Total Votes
    //    Sum all properties starting with "G20PRE"
    var propNames = ee.List(f.propertyNames());
    var g20Props = propNames.filter(ee.Filter.stringStartsWith('item', 'G20PRE'));
    
    var totalVotes = ee.Number(
      ee.Algorithms.If(
        g20Props.size().gt(0),
        g20Props.iterate(function(p, acc) {
          return ee.Number(acc).add(ee.Number(ee.Algorithms.If(f.get(p), f.get(p), 0)));
        }, 0),
        0
      )
    );

    // 3. Return Clean Feature
    return f
      .set('state', abbr)
      .set('precinct_area', areaSqMeters)
      .set('TOTALVOTES', totalVotes)
      .setGeometry(null); // CRITICAL: Drops geometry to allow CSV export
  });

  return processed;
}

// ============================================================
// 4. EXECUTION LOOP
// ============================================================

var allEmbeddings = ee.FeatureCollection([]);

// Iterate through states and merge results
states.forEach(function(st) {
  var fc = processState(st);
  allEmbeddings = allEmbeddings.merge(fc);
});

print('Processing Complete.');
print('Total Feature Count (Approx):', allEmbeddings.size());
print('Sample Feature:', allEmbeddings.limit(1));

// ============================================================
// 5. EXPORT
// ============================================================

Export.table.toDrive({
  collection: allEmbeddings,
  description: 'raw_emb_precinct_20',
  fileFormat: 'CSV'
});