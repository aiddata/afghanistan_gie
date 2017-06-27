

import os
import json
import math
import itertools
import fiona
import numpy as np
from shapely.geometry import shape


import pandas as pd
import geopandas as gpd

from datetime import datetime



# -----------------------------------------------------------------------------


polygon_path = os.path.expanduser(
    "~/git/afghanistan_gie/canal_data/canal_polygons.geojson")

output_path = os.path.expanduser(
        "~/git/afghanistan_gie/canal_data/canal_point_grid.geojson")

pixel_size = 0.0002695

polygon_data = fiona.open(polygon_path, 'r')


# -----------------------------------------------------------------------------



feature_list = []


# canal_feature = polygon_data[0]

for canal_feature in polygon_data:
    canal_id = canal_feature['properties']['project_id']
    canal_shape = shape(canal_feature['geometry'])
    print "Running {0}".format(canal_id)
    bounds = canal_shape.bounds
    xmin, ymin, xmax, ymax = bounds
    adj_xmin = math.floor((xmin - -180) / pixel_size) * pixel_size + -180
    adj_ymin = math.floor((ymin - -90) / pixel_size) * pixel_size + -90
    adj_xmax = math.ceil((xmax - -180) / pixel_size) * pixel_size + -180
    adj_ymax = math.ceil((ymax - -90) / pixel_size) * pixel_size + -90
    adj_bounds = (adj_xmin, adj_ymin, adj_xmax, adj_ymax)
    x_count = (adj_xmax-adj_xmin)/pixel_size
    if x_count < round(x_count):
        adj_xmax += pixel_size
    y_count = (adj_ymax-adj_ymin)/pixel_size
    if y_count < round(y_count):
        adj_ymax += pixel_size
    coords = itertools.product(
        np.arange(adj_xmin, adj_xmax, pixel_size),
        np.arange(adj_ymin, adj_ymax, pixel_size))
    point_list = [
        {
            # "type": "Feature",
            "geometry": shape({
                "type": "Point",
                "coordinates": c
            }),
            # "properties": {
            "project_id": canal_id,
            "unique": "{0}_{1}".format(round(c[0], 9), round(c[1], 9))
            # }
        }
        for c in coords
    ]
    canal_points = [feat for feat in point_list
                    if canal_shape.contains(shape(feat['geometry']))]
    feature_list += canal_points


# -----------------------------------------------------------------------------



gdf = gpd.GeoDataFrame(feature_list)


project_data_path = os.path.expanduser(
        "~/git/afghanistan_gie/canal_data/original_project_data.csv")

project_data = pd.read_csv(project_data_path, quotechar='\"',
                           na_values='', keep_default_na=False,
                           encoding='utf-8')
project_data = project_data[["OFWM Project ID", "Actual end date"]]
project_data.columns = ['project_id', 'actual_end_date']


project_data.loc[project_data['actual_end_date'].isnull()] = "Jan/01/9999"

def prep_date(datestr):
    delim = datestr[3]
    datestr = "/".join(datestr.split(delim))
    return datetime.strptime(datestr, "%b/%d/%Y").strftime('%Y-%m-%d')


project_data['actual_end_date_iso'] = project_data['actual_end_date'].apply(
    lambda z: prep_date(z))

gdf = gdf.merge(
    project_data[['project_id', 'actual_end_date_iso']], on='project_id')

grp = gdf.groupby(['unique'], as_index=False).aggregate(
    lambda x: '|'.join(x))

grp['project_list'] = grp['project_id']

gdf = gdf.merge(grp[['unique', 'project_list']], on='unique')

out_gdf = gdf.sort_values(by='actual_end_date_iso', ascending=True).groupby('unique', as_index=False).first()
out_gdf = gpd.GeoDataFrame(out_gdf)


geo_json = out_gdf.to_json()
geo_file = open(output_path, "w")
json.dump(json.loads(geo_json), geo_file, indent=4)
geo_file.close()


# -----------------------------------------------------------------------------


import tarfile

def make_tarfile(dst, src):
    with tarfile.open(dst, "w:gz") as tar:
        tar.add(src, arcname=os.path.basename(src))


make_tarfile(dst=output_path + ".tar.gz" , src=output_path)


# -----------------------------------------------------------------------------


# def create_geojson(features, path):
#     output_geo = {
#       "type": "FeatureCollection",
#       "features": features
#     }
#     output_file = open(path, "w")
#     json.dump(output_geo, output_file)
#     output_file.close()


# create_geojson(feature_list, output_path)

