
import os
import glob
import json
import fiona



project_dir = "/sciclone/aiddata10/REU/projects/afghanistan_gie"

area_path_list = glob.glob(
    "{0}/mapping/mapping_uploads/*/*_poly*.shp".format(project_dir))
canal_path_list = glob.glob(
    "{0}/mapping/mapping_uploads/*/*_line*.shp".format(project_dir))


# make sure all projects have area and canal
assert len(area_path_list) == len(canal_path_list) == 87


# -----------------------------------------------------------------------------

# combine all lines (no project information needed)


all_canal_features = []
for path in canal_path_list:
    project_id = os.path.basename(path)[:4]
    with fiona.open(path, 'r') as feat_src:
        for feature in feat_src:
            feature['properties']['project_id'] = project_id
            del feature['properties']['id']
            all_canal_features.append(feature)

canal_geo = {
  "type": "FeatureCollection",
  "features": all_canal_features
}

canal_path = os.path.expanduser(
    "~/git/afghanistan_gie/canal_data/canal_lines.geojson")
canal_file = open(canal_path, "w")
json.dump(canal_geo, canal_file)
canal_file.close()



all_area_features = []
for path in area_path_list:
    project_id = os.path.basename(path)[:4]
    with fiona.open(path, 'r') as feat_src:
        for feature in feat_src:
            feature['properties']['project_id'] = project_id
            del feature['properties']['id']
            all_area_features.append(feature)



area_geo = {
  "type": "FeatureCollection",
  "features": all_area_features
}

area_path = os.path.expanduser(
    "~/git/afghanistan_gie/canal_data/canal_polygons.geojson")
area_file = open(area_path, "w")
json.dump(area_geo, area_file)
area_file.close()





