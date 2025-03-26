ol.proj.proj4.register(proj4);
//ol.proj.get("EPSG:3857").setExtent([-13720474.184074, 6319521.293449, -13719913.596302, 6319821.814729]);
var wms_layers = [];


        var lyr_ESRISatellite_0 = new ol.layer.Tile({
            'title': 'ESRI Satellite',
            'type':'base',
            'opacity': 1.000000,
            
            
            source: new ol.source.XYZ({
            attributions: ' ',
                url: 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}'
            })
        });
var format_beaty_point_1 = new ol.format.GeoJSON();
var features_beaty_point_1 = format_beaty_point_1.readFeatures(json_beaty_point_1, 
            {dataProjection: 'EPSG:4326', featureProjection: 'EPSG:3857'});
var jsonSource_beaty_point_1 = new ol.source.Vector({
    attributions: ' ',
});
jsonSource_beaty_point_1.addFeatures(features_beaty_point_1);
var lyr_beaty_point_1 = new ol.layer.Vector({
                declutter: false,
                source:jsonSource_beaty_point_1, 
                style: style_beaty_point_1,
                popuplayertitle: 'beaty_point',
                interactive: true,
                title: '<img src="styles/legend/beaty_point_1.png" /> beaty_point'
            });
var format_uncertainty_rings_2 = new ol.format.GeoJSON();
var features_uncertainty_rings_2 = format_uncertainty_rings_2.readFeatures(json_uncertainty_rings_2, 
            {dataProjection: 'EPSG:4326', featureProjection: 'EPSG:3857'});
var jsonSource_uncertainty_rings_2 = new ol.source.Vector({
    attributions: ' ',
});
jsonSource_uncertainty_rings_2.addFeatures(features_uncertainty_rings_2);
var lyr_uncertainty_rings_2 = new ol.layer.Vector({
                declutter: false,
                source:jsonSource_uncertainty_rings_2, 
                style: style_uncertainty_rings_2,
                popuplayertitle: 'uncertainty_rings',
                interactive: true,
                title: '<img src="styles/legend/uncertainty_rings_2.png" /> uncertainty_rings'
            });
var format_reference_points_3 = new ol.format.GeoJSON();
var features_reference_points_3 = format_reference_points_3.readFeatures(json_reference_points_3, 
            {dataProjection: 'EPSG:4326', featureProjection: 'EPSG:3857'});
var jsonSource_reference_points_3 = new ol.source.Vector({
    attributions: ' ',
});
jsonSource_reference_points_3.addFeatures(features_reference_points_3);
var lyr_reference_points_3 = new ol.layer.Vector({
                declutter: false,
                source:jsonSource_reference_points_3, 
                style: style_reference_points_3,
                popuplayertitle: 'reference_points',
                interactive: true,
                title: '<img src="styles/legend/reference_points_3.png" /> reference_points'
            });

lyr_ESRISatellite_0.setVisible(true);lyr_beaty_point_1.setVisible(true);lyr_uncertainty_rings_2.setVisible(true);lyr_reference_points_3.setVisible(true);
var layersList = [lyr_ESRISatellite_0,lyr_beaty_point_1,lyr_uncertainty_rings_2,lyr_reference_points_3];
lyr_beaty_point_1.set('fieldAliases', {'id': 'id', });
lyr_uncertainty_rings_2.set('fieldAliases', {'id': 'id', 'layer': 'layer', 'path': 'path', 'distance': 'distance', });
lyr_reference_points_3.set('fieldAliases', {'id': 'id', 'name': 'name', 'distance': 'distance', 'title': 'title', 'dist_title': 'dist_title', });
lyr_beaty_point_1.set('fieldImages', {'id': 'TextEdit', });
lyr_uncertainty_rings_2.set('fieldImages', {'id': 'TextEdit', 'layer': 'TextEdit', 'path': 'TextEdit', 'distance': 'TextEdit', });
lyr_reference_points_3.set('fieldImages', {'id': 'TextEdit', 'name': 'TextEdit', 'distance': 'TextEdit', 'title': 'TextEdit', 'dist_title': 'TextEdit', });
lyr_beaty_point_1.set('fieldLabels', {'id': 'no label', });
lyr_uncertainty_rings_2.set('fieldLabels', {'id': 'hidden field', 'layer': 'hidden field', 'path': 'hidden field', 'distance': 'inline label - always visible', });
lyr_reference_points_3.set('fieldLabels', {'id': 'hidden field', 'name': 'hidden field', 'distance': 'hidden field', 'title': 'inline label - always visible', 'dist_title': 'inline label - always visible', });
lyr_reference_points_3.on('precompose', function(evt) {
    evt.context.globalCompositeOperation = 'normal';
});