


import os
import tarfile

from distancerasters import build_distance_array, rasterize, export_raster


# -----------------------------------------------------------------------------

from affine import Affine
# import numpy as np
import fiona

pixel_size = 0.0002695

canal_path = os.path.expanduser(
    "~/git/afghanistan_gie/canal_data/canal_lines.geojson")

with fiona.open(canal_path) as canal_src:
    bounds = canal_src.bounds


rv_array, affine = rasterize(canal_path, pixel_size=pixel_size, bounds=bounds)


binary_raster_path = os.path.expanduser(
    "~/git/afghanistan_gie/raster/binary_canals.tif")

export_raster(rv_array, affine, binary_raster_path)


def make_tarfile(dst, src):
    with tarfile.open(dst, "w:gz") as tar:
        tar.add(src, arcname=os.path.basename(src))


make_tarfile(dst=binary_raster_path[:-3] + "tar.gz" , src=binary_raster_path)

# -----------------------------------------------------------------------------


distance_raster_path = os.path.expanduser(
    "~/git/afghanistan_gie/raster/distance_canals.tif")

def raster_conditional(rarray):
    return (rarray == 1)

dist = build_distance_array(rv_array, affine=affine,
                            output=distance_raster_path,
                            conditional=raster_conditional)


make_tarfile(dst=distance_raster_path[:-3] + "tar.gz" , src=distance_raster_path)

