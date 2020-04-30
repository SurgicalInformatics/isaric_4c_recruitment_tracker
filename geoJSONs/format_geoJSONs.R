#format geoJSON

scotland = geojsonio::geojson_read('geoJSONs/scot_hb.geojson', what = 'sp')
wa = geojsonio::geojson_read('geoJSONs/welsh_hb.geojson', what = 'sp')
ni = geojsonio::geojson_read('geoJSONs/ni_hb.geojson', what = 'sp')
eng = geojsonio::geojson_read('geoJSONs/ccg.geojson', what = 'sp')

ni$code = ni$TrustCode
ni$name = ni$TrustName
ni$shape_length = ni$Shape_Leng
ni$shape_area = ni$Shape_Area

ni$TrustCode = NULL
ni$TrustName = NULL
ni$Shape_Leng = NULL
ni$Shape_Area = NULL

eng$code = eng$ccg17cd
eng$name = eng$ccg17nm
eng$shape_length = eng$st_lengthshape
eng$shape_area = eng$st_areashape

eng$objectid = NULL
eng$ccg17cd = NULL
eng$ccg17nm = NULL
eng$bng_e = NULL
eng$bng_n = NULL
eng$long = NULL
eng$lat = NULL
eng$st_areashape = NULL
eng$st_lengthshape = NULL

wa$code = wa$lhb16cd
wa$name = wa$lhb16nm
wa$shape_length = wa$st_lengthshape
wa$shape_area = wa$st_areashape

wa$objectid = NULL
wa$lhb16cd = NULL
wa$lhb16nm = NULL
wa$lhb16nmw = NULL
wa$bng_e = NULL
wa$bng_n = NULL
wa$long = NULL
wa$lat = NULL
wa$st_areashape = NULL
wa$st_lengthshape = NULL

names(scotland) = c('code', 'name', 'shape_length', 'shape_area')

scotland$country = 'Scotland'
ni$country = 'Northern Ireland'
eng$country = 'England'
wa$country = 'Wales'

master = rbind(eng, ni, wa, scotland)
saveRDS(master, 'geoJSONs/master.rds')
