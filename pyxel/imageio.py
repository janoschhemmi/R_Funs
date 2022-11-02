import os
from datetime import datetime, timedelta
from collections import OrderedDict
from subprocess import call, check_call, CalledProcessError
import fnmatch
import sys
import numpy as np
import shutil
import gzip
from math import floor
from multiprocessing import Pool


#os_environ_path = os.environ['PATH']
#os.environ['PATH'] = r"\\141.20.140.91\SAN_Projects\Geomultisens\code\anaconda\Library\bin;" + os_environ_path
#if os.getenv('GDAL_DATA') is None:
#    os.environ['GDAL_DATA'] = r'\\141.20.140.91\SAN_Projects\Geomultisens\code\anaconda\Library\share\gdal'

# os_environ_path = os.environ['PATH']
# os.environ['PATH'] = r"c:\anaconda\Library\bin;c:\anaconda;c:\anaconda\Scripts;" + os_environ_path
# if os.getenv('GDAL_DATA') is None:
#     os.environ['GDAL_DATA'] = r'c:\anaconda\Library\share\gdal'

try:  # check if gdal is in path
    from osgeo import gdal, gdal_array
    from osgeo import ogr
    from osgeo import osr
    from osgeo.gdalconst import *

    gdal.UseExceptions()
    check_call('gdal_translate --help-general', stdout=open(os.devnull, 'wb'), shell=True)
except:  # pragma no cover
    print('Adding OSGeo4W64 to PATH variable.')

try:  # pragma no cover
    from cStringIO import StringIO
except ImportError:  # pragma no cover
    try:
        from StringIO import StringIO
    except ImportError:
        from io import StringIO


def file_search(path, pattern='*', directory=False, full_names=True):

    if directory:
        # rlist = [dirpath for dirpath, dirnames, files in os.walk(path)
        #          if fnmatch.fnmatch(os.path.basename(dirpath), pattern)]

        if full_names:
            rlist = [os.path.join(dirpath, d) for dirpath, dirnames, files in os.walk(path)
                     for d in fnmatch.filter(dirnames, pattern)]
        else:
            rlist = [d for dirpath, dirnames, files in os.walk(path)
                     for d in fnmatch.filter(dirnames, pattern)]

    else:
        if full_names:
            rlist = [os.path.join(dirpath, f)
                     for dirpath, dirnames, files in os.walk(path)
                     for f in fnmatch.filter(files, pattern)]
        else:
            rlist = [f for dirpath, dirnames, files in os.walk(path)
                     for f in fnmatch.filter(files, pattern)]

    return rlist


def read_hdr(img_file):

    hdr_files = [img_file[:-3]+'.hdr', img_file[:-4]+'.hdr', img_file+'.hdr']
    hdr_files = [hdr_file for hdr_file in hdr_files if os.path.isfile(hdr_file)]
    if len(hdr_files) != 1:
        print('Unable to find unique HDR for '+img_file)
        return None
    else:
        hdr_file = hdr_files[0]

    hdr = OrderedDict()
    cont = False
    with open(hdr_file, 'r') as f:
        for line in f:
            if cont:
                hdr[attr_name] += line.strip()
                if hdr[attr_name].endswith('}'):
                    cont = False
            else:
                items = line.split('=', 1)
                if len(items) > 1:
                    attr_name = items[0].strip()
                    attr_value = items[1].strip()
                    hdr[attr_name] = attr_value
                    if hdr[attr_name].startswith('{'):
                        cont = True
                    if hdr[attr_name].endswith('}'):
                        cont = False
    return hdr


def write_hdr(img_file, hdr):

    hdr_files = [img_file[:-3]+'.hdr', img_file[:-4]+'.hdr', img_file+'.hdr']
    hdr_files = [hdr_file for hdr_file in hdr_files if os.path.isfile(hdr_file)]
    if len(hdr_files) != 1:
        print('Unable to find unique HDR for '+img_file)
        return None
    else:
        hdr_file = hdr_files[0]

    with open(hdr_file, 'w') as f_out:
        f_out.writelines('ENVI\n')
        for key in iter(hdr.keys()):
            f_out.writelines(" = ".join([key, hdr[key]])+"\n")


def update_hdr(img_file, update_dict):
    hdr = read_hdr(img_file)
    hdr.update(update_dict)
    write_hdr(img_file, hdr)


def compress_envi_file(img_file):
    hdr = read_hdr(img_file)
    if hdr.get('file compression', '0') == '0':
        with open(img_file, 'rb') as f_in:
            with gzip.open(img_file + '.gz', 'wb') as f_out:
                f_out.writelines(f_in)
        os.remove(img_file)
        os.rename(img_file + '.gz', img_file)

        hdr.update({'file compression': '1'})
        write_hdr(img_file, hdr)


def mapToPixel(mx, my, gt):
    """ Convert map to pixel coordinates
        @param  mx    Input map x coordinate (double)
        @param  my    Input map y coordinate (double)
        @param  gt    Input geotransform (six doubles)
        @return px,py Output coordinates (two doubles)
    """
    if gt[2] + gt[4] == 0:  # Simple calc, no inversion required
        px = (mx - gt[0]) / gt[1]
        py = (my - gt[3]) / gt[5]
    else:
        px, py = ApplyGeoTransform(mx, my, InvGeoTransform(gt))
    return int(px + 0.5), int(py + 0.5)


def pixelToMap(px, py, gt):
    """ Convert pixel to map coordinates
        @param  px    Input pixel x coordinate (double)
        @param  py    Input pixel y coordinate (double)
        @param  gt    Input geotransform (six doubles)
        @return mx,my Output coordinates (two doubles)
    """
    mx, my = ApplyGeoTransform(px, py, gt)
    return mx, my


def ApplyGeoTransform(inx, iny, gt):
    """Apply a geotransform
        @param  inx       Input x coordinate (double)
        @param  iny       Input y coordinate (double)
        @param  gt        Input geotransform (six doubles)
        @return outx,outy Output coordinates (two doubles)
    """
    outx = gt[0] + inx * gt[1] + iny * gt[2]
    outy = gt[3] + inx * gt[4] + iny * gt[5]
    return outx, outy


def InvGeoTransform(gt_in):
    """
    ************************************************************************
    *                        InvGeoTransform(gt_in)
    ************************************************************************

    **
    * Invert Geotransform.
    *
    * This function will invert a standard 3x2 set of GeoTransform coefficients.
    *
    * @param  gt_in  Input geotransform (six doubles - unaltered).
    * @return gt_out Output geotransform (six doubles - updated) on success,
    *                None if the equation is uninvertable.
    """
    # ******************************************************************************
    #    * This code ported from GDALInvGeoTransform() in gdaltransformer.cpp
    #    * as it isn't exposed in the python SWIG bindings until GDAL 1.7
    #    * copyright & permission notices included below as per conditions.
    #
    # ******************************************************************************
    #    * $Id: gdaltransformer.cpp 15024 2008-07-24 19:25:06Z rouault $
    #    *
    #    * Project:  Mapinfo Image Warper
    #    * Purpose:  Implementation of one or more GDALTrasformerFunc types, including
    #    *           the GenImgProj (general image reprojector) transformer.
    #    * Author:   Frank Warmerdam, [hidden email]
    #    *
    # ******************************************************************************
    #    * Copyright (c) 2002, i3 - information integration and imaging
    #    *                          Fort Collin, CO
    #    *
    #    * Permission is hereby granted, free of charge, to any person obtaining a
    #    * copy of this software and associated documentation files (the "Software"),
    #    * to deal in the Software without restriction, including without limitation
    #    * the rights to use, copy, modify, merge, publish, distribute, sublicense,
    #    * and/or sell copies of the Software, and to permit persons to whom the
    #    * Software is furnished to do so, subject to the following conditions:
    #    *
    #    * The above copyright notice and this permission notice shall be included
    #    * in all copies or substantial portions of the Software.
    #    *
    #    * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    #    * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    #    * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
    #    * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    #    * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    #    * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    #    * DEALINGS IN THE SOFTWARE.
    # ****************************************************************************

    # we assume a 3rd row that is [1 0 0]

    # Compute determinate
    det = gt_in[1] * gt_in[5] - gt_in[2] * gt_in[4]

    if abs(det) < 0.000000000000001:
        return

    inv_det = 1.0 / det

    # compute adjoint, and divide by determinate
    gt_out = [0, 0, 0, 0, 0, 0]
    gt_out[1] = gt_in[5] * inv_det
    gt_out[4] = -gt_in[4] * inv_det

    gt_out[2] = -gt_in[2] * inv_det
    gt_out[5] = gt_in[1] * inv_det

    gt_out[0] = (gt_in[2] * gt_in[3] - gt_in[0] * gt_in[5]) * inv_det
    gt_out[3] = (-gt_in[1] * gt_in[3] + gt_in[0] * gt_in[4]) * inv_det

    return gt_out


def raster_extent(file_name, center=True):
    img = gdal.Open(file_name)
    gt = img.GetGeoTransform()

    if center:
        offset_x = gt[1] / 2.0
        offset_y = gt[5] / 2.0
    else:
        offset_x = 0
        offset_y = 0

    lx = gt[0] + offset_x
    uy = gt[3] + offset_y
    rx = gt[0] + img.RasterXSize * gt[1] - offset_x
    ly = gt[3] + img.RasterYSize * gt[5] - offset_y

    return (lx, uy), (rx, ly)


def raster_coordinates(hdr):
    gt = hdr.get('map info').split(', ')[3:7]
    ft = [float(x) for x in gt]

    offset_x = ft[2] / 2.0
    offset_y = ft[3] / 2.0

    lx = ft[0] + offset_x
    uy = ft[1] - offset_y
    rx = ft[0] + float(hdr.get('samples')) * ft[2] - offset_x
    ly = ft[1] - float(hdr.get('lines')) * ft[3] + offset_y

    return (lx, uy), (rx, ly)


def readBandAsArray(file_name, subset=None):

    src_ds = gdal.Open(file_name)
    gt = src_ds.GetGeoTransform()

    gtInverse = gdal.InvGeoTransform(gt)

    (lx, uy), (rx, ly) = subset

    x1, y1 = gdal.ApplyGeoTransform(gtInverse, lx, uy)
    x2, y2 = gdal.ApplyGeoTransform(gtInverse, rx, ly)

    band = src_ds.GetRasterBand(1)
    array = band.ReadAsArray(int(x1), int(y1), int(x2)-int(x1)+1, int(y2)-int(y1)+1)

    src_ds = None

    return array


def polygon_boundary(shp_file, attribute=None, value=None):
    pts = ogr.Open(shp_file)
    lyr = pts.GetLayer()
    if attribute is not None and value is not None:
        print("Attribute filter set to %s" % " = ".join([attribute, value]))
        lyr.SetAttributeFilter(" = ".join([attribute, value]))

    feat = lyr.GetNextFeature()
    geom = feat.GetGeometryRef()
    geom.ExportToWkt()


def extract_pixel(raster_file, shp_file, csv_file, idfield='POINT_ID', date_acquired=None, sceneid=None):
    img = gdal.Open(raster_file)
    gt = img.GetGeoTransform()

    pts = ogr.Open(shp_file)
    lyr = pts.GetLayer()

    if sceneid is None:
        sceneid = os.path.basename(raster_file)

    if date_acquired is None:
        date_acquired = datetime.today()

    # coordinate transformation
    img_srs = osr.SpatialReference(wkt=img.GetProjection())
    pts_srs = lyr.GetSpatialRef()
    trans_i2p = osr.CoordinateTransformation(img_srs, pts_srs)
    trans_p2i = osr.CoordinateTransformation(pts_srs, img_srs)

    lx = gt[0]
    uy = gt[3]
    rx = gt[0] + img.RasterXSize * gt[1]
    ly = gt[3] + img.RasterYSize * gt[5]

    ring = ogr.Geometry(ogr.wkbLinearRing)
    ring.AddPoint(lx, uy)
    ring.AddPoint(rx, uy)
    ring.AddPoint(rx, ly)
    ring.AddPoint(lx, ly)
    ring.AddPoint(lx, uy)
    bbox = ogr.Geometry(ogr.wkbPolygon)
    bbox.AddGeometry(ring)

    # transform image boundary box in shp coordinate system for subsetting
    bbox.Transform(trans_i2p)
    # print(bbox.ExportToWkt())
    # print(img.GetMetadata_Dict())

    lyr.SetSpatialFilter(bbox)
    n_points = lyr.GetFeatureCount()

    layerDefinition = lyr.GetLayerDefn()
    # for i in range(layerDefinition.GetFieldCount()):
    #    print layerDefinition.GetFieldDefn(i).GetName()

    if os.path.isfile(csv_file):
        readmode = 'a'
    else:
        readmode = 'w'

    shape_attributes = [layerDefinition.GetFieldDefn(i).GetName() for i in range(layerDefinition.GetFieldCount())]
    if idfield not in shape_attributes:
        print('idfield not valid.')
        print('Shapefile has the following attributes: %s' % ', '.join(shape_attributes))
        return

    band_names = ['band_%s' % '{0:02d}'.format(x) for x in np.arange(img.RasterCount) + 1]
    columns = ['pointid', 'x', 'y', 'date', 'sceneid'] + band_names

    bands = [img.GetRasterBand(i + 1) for i in range(img.RasterCount)]

    with open(csv_file, readmode) as f_out:
        if readmode == 'w':
            header = ','.join(columns)
            f_out.writelines(header + '\n')

        for feat in lyr:
            geom = feat.GetGeometryRef()
            x_srs = geom.GetX()
            y_srs = geom.GetY()
            geom.Transform(trans_p2i)
            pointid = feat.GetFieldAsString(idfield)
            x_coord = int((geom.GetX() - gt[0]) / gt[1])
            y_coord = int((geom.GetY() - gt[3]) / gt[5])
            xtract = [band.ReadAsArray(x_coord, y_coord, 1, 1) for band in bands]
            values = [str(x[0, 0]) for x in xtract]
            line = '%s,%s,%s,%s,%s,' % (pointid, x_srs, y_srs, date_acquired.strftime("%Y-%m-%d"), sceneid)
            line += ','.join(values)
            line += '\n'
            f_out.writelines(line)

    img = None
    # return df
    print('Extracted %s features from %s' % (n_points, raster_file))


def coord_snap(coord, ref_coord, pixel_size=30.0, refIsCenter=False, outputCenter=True):
    # returns coordinate snapped to the center coordinate of the pixel
    if refIsCenter:
        ref_coord_corner = (ref_coord[0] - (pixel_size / 2.0), ref_coord[1] + (pixel_size / 2.0))
    else:
        ref_coord_corner = ref_coord

    fx = floor((coord[0] - ref_coord_corner[0]) / pixel_size)
    fy = floor((ref_coord_corner[1] - coord[1]) / pixel_size)

    x = ref_coord_corner[0] + fx * pixel_size
    y = ref_coord_corner[1] - fy * pixel_size

    if outputCenter:
        x += pixel_size / 2.0
        y -= pixel_size / 2.0

    return x, y


def reproject_dataset(dataset, dst_pixelsize=30., dst_epsg=3575, dst_nodata=None, dst_proj=None,
                      dst_ref=None, resampling_method=None):
    """
    A sample function to reproject and resample a GDAL dataset from within
    Python. The idea here is to reproject from one system to another, as well
    as to change the pixel size. The procedure is slightly long-winded, but
    goes like this:

    1. Set up the two Spatial Reference systems.
    2. Open the original dataset, and get the geotransform
    3. Calculate bounds of new geotransform by projecting the UL corners
    4. Calculate the number of pixels with the new projection & spacing
    5. Create an in-memory raster dataset
    6. Perform the projection
    """

    # Open the file
    g = gdal.Open(dataset)
    if g is None:
        print("Could not open the file!")

    band = g.GetRasterBand(1)
    src_type = band.DataType
    src_nodata = band.GetNoDataValue()

    if resampling_method is None:
        resampling_method = gdal.GRA_Cubic

    # Define the input projection

    if dst_proj is None:
        dst_proj = osr.SpatialReference()
        dst_proj.ImportFromEPSG(dst_epsg)
    if dst_nodata is None:
        dst_nodata = src_nodata

    src_proj = osr.SpatialReference()
    src_proj.ImportFromWkt(g.GetProjectionRef())
    tx = osr.CoordinateTransformation(src_proj, dst_proj)
    # Up to here, all  the projection have been defined, as well as a
    # transformation from the from to the  to :)

    # Get the Geotransform vector
    geo_t = g.GetGeoTransform()

    # Raster xsize
    x_size = g.RasterXSize
    # Raster ysize
    y_size = g.RasterYSize

    # Work out the boundaries of the new dataset in the target projection

    (ulx, uly, ulz) = tx.TransformPoint(geo_t[0], geo_t[3])
    (urx, ury, urz) = tx.TransformPoint(geo_t[0] + geo_t[1] * x_size, geo_t[3])
    (lrx, lry, lrz) = tx.TransformPoint(geo_t[0] + geo_t[1] * x_size, geo_t[3] + geo_t[5] * y_size)
    (llx, lly, llz) = tx.TransformPoint(geo_t[0], geo_t[3] + geo_t[5] * y_size)

    if dst_ref is not None:
        ulx, uly = coord_snap([min(ulx, urx, lrx, llx), max(uly, ury, lry, lly)], [dst_ref[0], dst_ref[1]],
                              pixel_size=dst_pixelsize, outputCenter=True)
        lrx, lry = coord_snap([max(ulx, urx, lrx, llx), min(uly, ury, lry, lly)], [dst_ref[0], dst_ref[1]],
                              pixel_size=dst_pixelsize, outputCenter=True)
        ulx -= (dst_pixelsize / 2.0)
        uly += (dst_pixelsize / 2.0)
        lrx += (dst_pixelsize / 2.0)
        lry -= (dst_pixelsize / 2.0)

    # Now, we create an in-memory raster
    mem_drv = gdal.GetDriverByName('MEM')

    # The size of the raster is given the new projection and pixel spacing
    # Using the values we calculated above. Also, setting it to store one band
    dest = mem_drv.Create('', int((lrx - ulx) / dst_pixelsize), int((uly - lry) / dst_pixelsize), g.RasterCount,
                          src_type)
    dest.GetRasterBand(1).SetNoDataValue(dst_nodata)
    dest.GetRasterBand(1).Fill(dst_nodata)

    # Calculate the new geotransform
    new_geo = (ulx, dst_pixelsize, geo_t[2], uly, geo_t[4], - dst_pixelsize)

    # Set the geotransform
    dest.SetGeoTransform(new_geo)
    dest.SetProjection(dst_proj.ExportToWkt())

    # Perform the projection/resampling
    res = gdal.ReprojectImage(g, dest, src_proj.ExportToWkt(), dst_proj.ExportToWkt(), resampling_method)
    dest.GetRasterBand(1).SetNoDataValue(dst_nodata)

    return dest


def reproject(infile, outfile, dst_epsg=None, dst_proj=None, dst_ref=None, dst_nodata=None, dst_pixelsize=30.0,
              resampling_method=None):

    # Open the file
    g = gdal.Open(infile)
    if g is None:
        print("Could not open the file!")

    nodata_value = g.GetRasterBand(1).GetNoDataValue()
    g = None

    # Do in memory reprojection
    reprojected_dataset = reproject_dataset(infile, dst_epsg=dst_epsg, dst_proj=dst_proj, dst_ref=dst_ref,
                                            resampling_method=resampling_method, dst_pixelsize=dst_pixelsize,
                                            dst_nodata=dst_nodata)

    # Output driver, as before
    driver = gdal.GetDriverByName("GTiff")

    # Create a copy of the in memory dataset `reprojected_dataset`, and save it
    # os.environ['GDAL_PAM_ENABLED'] = 'NO'
    dst_ds = driver.CreateCopy(outfile, reprojected_dataset, 1, options=['COMPRESS=LZW'])
    # del os.environ['GDAL_PAM_ENABLED']

    # Flush the dataset to disk
    dst_ds = None

    # hdr_file = outfile.replace('img', 'hdr')
    # update_hdr(hdr_file, nodata_value)


def writeRaster(array, out_file, src_ds=None, ref_file=None, driver='Gtiff', nodata=None,
                bandNames=None, enviMetadata=None, co=None):

    if ref_file is not None:
        src_ds = gdal.Open(ref_file)

    if len(array.shape) == 2:
        img = array[np.newaxis, :, :]
    else:
        img = array

    if not os.path.exists(os.path.dirname(out_file)):
        os.makedirs(os.path.dirname(out_file))

    type_code = gdal_array.NumericTypeCodeToGDALTypeCode(img.dtype)

    n_bands, n_rows, n_cols = img.shape

    drv = gdal.GetDriverByName(driver)

    if co is not None:
        options=co
    else:
        options=['']

    dst_ds = drv.Create(out_file, src_ds.RasterXSize, src_ds.RasterYSize, n_bands,
                        type_code)

    dst_ds.SetGeoTransform(src_ds.GetGeoTransform())
    dst_ds.SetProjection(src_ds.GetProjectionRef())

    for i in range(n_bands):

        dst_band = dst_ds.GetRasterBand(i + 1)

        if nodata is not None:
            dst_band.SetNoDataValue(nodata)

        # write the data
        dst_band.WriteArray(img[i, :, :], 0, 0)

        if bandNames is not None:
            dst_band.SetDescription(bandNames[i])

        # flush data to disk, set the NoData value and calculate stats
        dst_band.FlushCache()

    src_ds = None
    dst_ds = None

    if (driver == 'ENVI') & (enviMetadata is not None):
        hdr = read_hdr(out_file)
        for key in enviMetadata:
            hdr[key] = enviMetadata[key]
        write_hdr(out_file, hdr)


def sieve(src_file, out_file=None, mmu=6, neighbors=4):

    src_ds = gdal.Open(src_file)
    src_band = src_ds.GetRasterBand(1)

    if out_file is None:
        out_file = src_file[:-4] + '_mmu%sn%s.tif' % (mmu, neighbors)

    drv = gdal.GetDriverByName('GTiff')
    dst_ds = drv.Create(out_file, src_ds.RasterXSize, src_ds.RasterYSize, 1,
                        src_band.DataType, options=['COMPRESS=LZW'])

    dst_ds.SetGeoTransform(src_ds.GetGeoTransform())
    dst_ds.SetProjection(src_ds.GetProjectionRef())

    dst_band = dst_ds.GetRasterBand(1)

    gdal.SieveFilter(src_band, None, dst_band, mmu, neighbors, callback=gdal.TermProgress_nocb)

    dst_ds = None
    src_ds = None


def clip(in_raster, out_raster, shp_file, ot='Byte', of='GTiff', co=None, dstnodata=0, cores=None, verbose=True):

    if os.path.exists(out_raster):
        print('%s exists.' % out_raster)
        return

    src_ds = gdal.Open(in_raster)
    gt = src_ds.GetGeoTransform()

    lx = gt[0]
    uy = gt[3]
    rx = gt[0] + src_ds.RasterXSize * gt[1]
    ly = gt[3] + src_ds.RasterYSize * gt[5]

    options = ['-ot', ot]
    options += ['-of', of]
    options += ['-tr', '%s' % gt[1], '%s' % gt[1]]
    options += ['-cutline', shp_file]
    options += ['-dstnodata', '%s' % dstnodata]
    options += ['-te', '%s' % lx, '%s' % ly, '%s' % rx, '%s' % uy]
    options += ['-multi']

    if cores is not None:
        options += ['-wo', 'NUM_THREADS=%s' % cores]

    if co is not None:
        options += ['-co', co]

    out_ds = gdal.Rasterize(out_raster, src_ds, options=' '.join(options))
    out_ds = None


def crop(in_raster, out_raster, ref_raster, ot='Byte', of='GTiff', co=None, dstnodata=0, cores=None):

    if os.path.exists(out_raster):
        print('%s exists.' % out_raster)
        return

    img = gdal.Open(ref_raster)
    gt = img.GetGeoTransform()

    lx = gt[0]
    uy = gt[3]
    rx = gt[0] + img.RasterXSize * gt[1]
    ly = gt[3] + img.RasterYSize * gt[5]

    options = ['-ot', ot]
    options += ['-of', of]
    options += ['-tr', '%s' % gt[1], '%s' % gt[1]]
    options += ['-dstnodata', '%s' % dstnodata]
    options += ['-te', '%s' % lx, '%s' % ly, '%s' % rx, '%s' % uy]
    options += ['-multi']

    if cores is not None:
        options += ['-wo', 'NUM_THREADS=%s' % cores]

    if co is not None:
        options += ['-co', co]

    src_ds = gdal.Open(in_raster)
    out_ds = gdal.Rasterize(out_raster, src_ds, options=' '.join(options))
    out_ds = None


def rasterize(shp_file, raster_file, ref_file, field='ID', ot='Byte', of='GTiff', co=None, a_nodata=0):

    if os.path.exists(raster_file):
        print('%s exists.' % raster_file)
        return

    img = gdal.Open(ref_file)
    gt = img.GetGeoTransform()

    lx = gt[0]
    uy = gt[3]
    rx = gt[0] + img.RasterXSize * gt[1]
    ly = gt[3] + img.RasterYSize * gt[5]

    options = ['-a', field]
    options += ['-tr', '%s' % gt[1], '%s' % gt[1]]
    options += ['-a_nodata', '%s' % a_nodata]
    options += ['-te', '%s' % lx, '%s' % ly, '%s' % rx, '%s' % uy]
    options += ['-ot', ot]
    options += ['-of', of]
    if co is not None:
        options += ['-co', co]

    src_ds = gdal.Open(shp_file)

    out_ds = gdal.Rasterize(raster_file, src_ds, options=' '.join(options))

    out_ds = None


def mosaic(in_files, out_file, pixel_size=None, quiet=True, epsg=None, anchor=None, srcnodata=None, bbox=None,
           r=None, compress=True, bigtiff=False, band=None, of='GTiff', overwrite=False):
    if not os.path.exists(os.path.dirname(out_file)):
        os.makedirs(os.path.dirname(out_file))

    if os.path.exists(out_file) and not overwrite:
        print('%s exists. Use overwrite keyword.' % out_file)
        return

    if epsg is not None:
        tile_in_files = []
        for in_file in in_files:
            tile_vrt_file = in_file + '.tmp.tif'
            warp(in_file, tile_vrt_file, epsg=epsg, r=r, overwrite=True, pixel_size=pixel_size,
                 anchor=anchor, of='GTiff')
            tile_in_files.append(tile_vrt_file)
    else:
        tile_in_files = in_files

    listfile = out_file + '_tmp'
    with open(listfile, 'w') as fl:
        fl.writelines(["%s\n" % tile_vrt_file for tile_vrt_file in tile_in_files])

    cmd = ['gdalbuildvrt']
    if quiet:
        cmd.append('-q')

    if bbox is not None:
        cmd += ['-te', str(bbox[0][0]), str(bbox[0][1]), str(bbox[1][0]), str(bbox[1][1])]

    if srcnodata is not None:
        cmd += ['-srcnodata', str(srcnodata)]

    if pixel_size is not None:
        cmd += ['-resolution', 'user', '-tr', str(pixel_size), str(pixel_size)]

    if band is not None:
        cmd += ['-b', str(band)]

    if overwrite:
        cmd += ['-overwrite']

    cmd += ['-input_file_list', listfile, out_file]

    if not quiet:
        print(' '.join(cmd))
    call(cmd)
    os.remove(listfile)

    if not out_file.endswith('vrt'):
        vrt_file = out_file + '.vrt'
        os.rename(out_file, vrt_file)

        cmd = ['gdal_translate', '-q', '-of', of]

        if of == 'GTiff':
            cmd += ['-co', 'tiled=yes']
            if compress:
                cmd += ['-co', 'compress=LZW']
            if bigtiff:
                cmd += ['-co', 'BIGTIFF=YES']

        cmd += [vrt_file, out_file]
        # print(' '.join(cmd))
        call(cmd)
        os.remove(vrt_file)
        if epsg is not None:
            for f in tile_in_files:
                if f.endswith('.tmp.tif'):
                    os.remove(f)

    print('Created %s' % out_file)


def warp(in_file, vrt_file, epsg=None, pixel_size=None, bbox=None, r=None, anchor=None, overwrite=False,
             dstnodata=None, of='vrt', lzw=False):

    if not os.path.exists(os.path.dirname(vrt_file)):
        os.makedirs(os.path.dirname(vrt_file))

    if not os.path.exists(in_file):
        print('%s does not exist.' % in_file)
        return

    if os.path.exists(vrt_file) and not overwrite:
        print('%s exists. Use overwrite keyword.' % vrt_file)
        return
    else:
        print('gdalwarp: %s' % vrt_file)

    options = '-q -multi -of %s -t_srs EPSG:%s' % (of, epsg)

    src_ds = gdal.Open(in_file)
    # Get the Geotransform vector
    geo_t = src_ds.GetGeoTransform()

    if anchor is not None:

        dst_proj = osr.SpatialReference()
        dst_proj.ImportFromEPSG(epsg)

        src_proj = osr.SpatialReference()
        src_proj.ImportFromWkt(src_ds.GetProjectionRef())
        tx = osr.CoordinateTransformation(src_proj, dst_proj)

        # Raster xsize
        x_size = src_ds.RasterXSize
        # Raster ysize
        y_size = src_ds.RasterYSize

        (ulx, uly, ulz) = tx.TransformPoint(geo_t[0], geo_t[3])
        (urx, ury, urz) = tx.TransformPoint(geo_t[0] + geo_t[1] * x_size, geo_t[3])
        (lrx, lry, lrz) = tx.TransformPoint(geo_t[0] + geo_t[1] * x_size, geo_t[3] + geo_t[5] * y_size)
        (llx, lly, llz) = tx.TransformPoint(geo_t[0], geo_t[3] + geo_t[5] * y_size)

        if pixel_size is None:
            dst_pixelsize = geo_t[1]
        else:
            dst_pixelsize = pixel_size

        ulx, uly = coord_snap([min(ulx, urx, lrx, llx), max(uly, ury, lry, lly)], anchor, pixel_size=dst_pixelsize,
                              outputCenter=True)
        lrx, lry = coord_snap([max(ulx, urx, lrx, llx), min(uly, ury, lry, lly)], anchor, pixel_size=dst_pixelsize,
                              outputCenter=True)
        ulx -= dst_pixelsize / 2.0
        uly += dst_pixelsize / 2.0
        lrx += dst_pixelsize / 2.0
        lry -= dst_pixelsize / 2.0

        options += ' -te %s %s %s %s' % (ulx, lry, lrx, uly)
    elif bbox is not None:
        options += ' -te %s %s %s %s' % (bbox[0][0], bbox[0][1], bbox[1][0], bbox[1][1])

    if pixel_size is None:
        pixel_size = geo_t[1]
    options += ' -tr %s %s' % (pixel_size, pixel_size)

    if overwrite:
        options += ' -overwrite'

    if r is None:
        options += ' -r near'
    else:
        options += ' -r %s' % r

    if dstnodata is not None:
        options += ' -dstnodata %s' % dstnodata

    if lzw:
        options += ' -co "COMPRESS=LZW"'

    out_ds = gdal.Translate(vrt_file, src_ds, options=' '.join(options))
    out_ds = None


def export_gtiff(in_file, out_file, overwrite=False, verbose=True):

    if os.path.isfile(out_file) and not overwrite:
        if verbose:
            print('%s file exists. Use overwrite keyword.' % out_file)
        return

    print('Writing %s' % out_file)

    if not os.path.isdir(os.path.dirname(out_file)):
        os.makedirs(os.path.dirname(out_file))

    src_ds = gdal.Open(in_file)

    options = ['-q', '-of', 'GTiff', '-co', 'tiled=yes', '-co', 'BLOCKXSIZE=256', '-co',
               'BLOCKYSIZE=256', '-co', 'PREDICTOR=2', '-co', 'compress=LZW', '-co', 'INTERLEAVE=PIXEL']

    out_ds = gdal.Translate(out_file, src_ds, options=' '.join(options))

    out_ds = None


def batch_export_gtiff(inroot, outroot, filter='vrt', overwrite=False, processes=1, verbose=True):

    if processes > 1:
        pool = Pool(processes=processes)
        for root, dirs, files in os.walk(inroot):
            for fname in files:
                if fname.endswith(filter):
                    relative_file = os.path.relpath(os.path.join(root, fname), inroot)
                    in_file = os.path.join(inroot, relative_file)
                    out_file = os.path.join(outroot, relative_file).replace(filter, 'tif')
                    pool.apply_async(export_gtiff, (in_file, out_file),
                                     kwds={'verbose': verbose})
        pool.close()
        pool.join()
    else:
        for root, dirs, files in os.walk(inroot):
            for fname in files:
                if fname.endswith(filter):
                    relative_file = os.path.relpath(os.path.join(root, fname), inroot)
                    in_file = os.path.join(inroot, relative_file)
                    out_file = os.path.join(outroot, relative_file).replace(filter, 'tif')
                    export_gtiff(in_file, out_file, verbose=verbose, overwrite=overwrite)


class Stack(object):
    def __init__(self, file_list, mask_list=[], band=None):

        self.file_list = file_list
        self.mask_list = mask_list
        self.vrt_file = ''
        self.band = band

    def export_vrt(self, outfile):

        self.vrt_file = outfile
        try:
            os.makedirs(os.path.dirname(outfile))
        except:
            pass

        vrtOptions = gdal.BuildVRTOptions(bandList=self.band, separate=True)
        vrt_ds = gdal.BuildVRT(outfile, self.file_list, options=vrtOptions)
        vrt_ds.FlushCache()

    def export(self, outfile, format='Gtiff', verbose=False):

        if self.vrt_file == '':
            self.vrt_file = outfile + '.vrt'

        if not os.path.exists(self.vrt_file):
            self.export_vrt(self.vrt_file)

        call('gdal_translate -q -of %s %s %s' % (format, self.vrt_file, outfile), shell=True)
        os.remove(self.vrt_file)
        if verbose:
            print('Stacked %s ' % outfile)

    def extract_pixel(self, point_coordinate, point_espg=None, file_coordinate=False, verbose=False):

        for i, file in enumerate(self.file_list):

            image = gdal.Open(file)
            mask = gdal.Open(self.mask_list[i])

            if point_espg is not None:
                # create coordinate transformation
                inSpatialRef = osr.SpatialReference()
                inSpatialRef.ImportFromEPSG(point_espg)

                # create a geometry from coordinates
                point = ogr.Geometry(ogr.wkbPoint)
                point.AddPoint(point_coordinate[0], point_coordinate[1])

                outSpatialRef = osr.SpatialReference()
                # outSpatialRef.ImportFromProj4("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
                outSpatialRef.ImportFromWkt(image.GetProjection())
                coordTransform = osr.CoordinateTransformation(inSpatialRef, outSpatialRef)
                point.Transform(coordTransform)
                mcx = point.GetX()
                mcy = point.GetY()
            else:
                mcx = point_coordinate[0]
                mcy = point_coordinate[1]

            if file_coordinate:
                fcx = point_coordinate[0]
                fcy = point_coordinate[1]
            else:
                # In case of north up images, the GT(2) and GT(4) coefficients are zero,
                # and the GT(1) is pixel width, and GT(5) is pixel height. The (GT(0),GT(3))
                # position is the top left corner of the top left pixel of the raster.
                gt = image.GetGeoTransform()

                fcx = int((mcx - gt[0]) / gt[1])
                fcy = int((mcy - gt[3]) / gt[5])

                if verbose and i == 0:
                    print('Map coordinates are x=%s and y=%s' % (mcx, mcy))
                    print('File coordinates are x=%s and y=%s' % (fcx, fcy))

            adate = image.GetMetadataItem('RANGEBEGINNINGDATE')
            if adate is not None:
                adate = np.datetime64(adate)

            img = image.GetRasterBand(1).ReadAsArray(fcx, fcy, 1, 1)[0, 0]
            msk = mask.GetRasterBand(1).ReadAsArray(fcx, fcy, 1, 1)[0, 0]

            if i == 0:
                mseries = np.zeros(len(self.file_list), dtype=msk.dtype)
                tseries = np.zeros(len(self.file_list), dtype=img.dtype)
                jseries = np.zeros(len(self.file_list), dtype=np.int16)
                dseries = np.empty(len(self.file_list), dtype='datetime64[D]')

            jseries[i] = (adate.astype('datetime64[D]') - adate.astype('datetime64[Y]')).astype(int) + 1
            dseries[i] = adate
            tseries[i] = img
            mseries[i] = msk

        return tseries, dseries, jseries, mseries







