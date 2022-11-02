from pyxel import imageio as io
from osgeo import gdal
from skimage.measure import label
import os
import numpy as np


def create_disturbance_patches(in_file, connectivity=2, background=0):

    out_file = in_file[:-4] + '_patch.tif'

    if os.path.exists(out_file):
        print('File exists: %s' % out_file)
        return

    src_ds = gdal.Open(in_file)
    src_img = src_ds.ReadAsArray()

    result = label(src_img, background=background, return_num=False, connectivity=connectivity)

    drv = gdal.GetDriverByName('GTiff')
    dst_ds = drv.Create(out_file, src_ds.RasterXSize, src_ds.RasterYSize, 1,
                        gdal.GDT_Int32, options=['COMPRESS=LZW'])

    dst_ds.SetGeoTransform(src_ds.GetGeoTransform())
    dst_ds.SetProjection(src_ds.GetProjectionRef())

    dst_band = dst_ds.GetRasterBand(1)
    dst_band.SetNoDataValue(0)

    dst_band.WriteArray(result, 0, 0)

    dst_ds = None
    dst_band = None

    print(out_file)


def filter_and_mask_disturbance_map(year_file, mask_file, aoi_file=None, mmu=6, neighbors=8,
                                    yearRange=None, nodata_value=0, undisturbed_value=1970,
                                    undisturbed_fillHoles=True):

    baseName = os.path.basename(year_file)[:-4]

    if yearRange is None:
        subpath = os.path.basename(mask_file)[:-4] + '_' + baseName.split('_')[3]
    else:
        subpath = os.path.basename(mask_file)[:-4] + '_' + '%s-%s' % yearRange

    outpath = os.path.join(os.path.dirname(year_file), subpath)

    out_mmu_file = os.path.join(outpath,  baseName + '_mmu%s.tif' % mmu)
    out_formask_file = os.path.join(outpath, baseName + '_forestmask.tif')
    out_masked_file = os.path.join(outpath, baseName + '.tif')

    if os.path.exists(out_masked_file):
        print('File exists: %s' % out_masked_file)
        return

    if not os.path.exists(outpath):
        os.mkdir(outpath)

    # masking

    year_ds = gdal.Open(year_file)
    year_band = year_ds.GetRasterBand(1)
    year_img = year_ds.ReadAsArray()
    # print(year_img.shape)

    subset = io.raster_extent(year_file)
    mask_img = io.readBandAsArray(mask_file, subset)
    # print(mask_img.shape)

    year_img = year_img * mask_img

    if yearRange is not None:
        year_img[year_img < yearRange[0]] = nodata_value
        year_img[year_img > yearRange[1]] = nodata_value

    if aoi_file is not None:
        aoi_ds = gdal.Open(aoi_file)
        aoi_img = aoi_ds.ReadAsArray()
        aoi_img = mask_img * aoi_img * undisturbed_value * (year_img == nodata_value)
        year_img_sav = aoi_img + year_img
    else:
        year_img_sav = year_img


    if not os.path.exists(out_masked_file):
        io.writeRaster(year_img_sav, out_masked_file, ref_file=year_file)

    if not os.path.exists(out_formask_file):
        io.writeRaster(mask_img, out_formask_file, ref_file=year_file)

    ##################################################

    if not os.path.exists(out_mmu_file):

        print('Creating %s' % os.path.basename(out_mmu_file))

        drvMem = gdal.GetDriverByName('MEM')
        mem_ds = drvMem.Create('', year_ds.RasterXSize, year_ds.RasterYSize, 1, year_band.DataType)
        mem_ds.SetGeoTransform(year_ds.GetGeoTransform())
        mem_ds.SetProjection(year_ds.GetProjectionRef())

        mem_band = mem_ds.GetRasterBand(1)
        mem_band.SetNoDataValue(nodata_value)

        # write the data
        mem_band.WriteArray(year_img, 0, 0)


        # flush data to disk, set the NoData value and calculate stats
        # mem_band.FlushCache()

        ##################################################
        drv = gdal.GetDriverByName('GTiff')
        dst_ds = drv.Create(out_mmu_file, year_ds.RasterXSize, year_ds.RasterYSize, 1,
                            year_band.DataType, options=['COMPRESS=LZW'])

        dst_ds.SetGeoTransform(year_ds.GetGeoTransform())
        dst_ds.SetProjection(year_ds.GetProjectionRef())

        dst_band = dst_ds.GetRasterBand(1)
        dst_band.SetNoDataValue(nodata_value)

        # we don't want undisturbed holes (pixel value = 0) to be filled in.
        # To ignore 0 pixels we need to do some trick because ignoring 0 pixels
        # by setting the mask also leaves small islands of disturbed pixels
        mask_band = mem_band.GetMaskBand()
        gdal.SieveFilter(mem_band, mask_band, dst_band, mmu, neighbors, callback=gdal.TermProgress_nocb)

        # rerun sieve but this time without mask to remove small islands
        gdal.SieveFilter(dst_band, None, dst_band, mmu, neighbors, callback=gdal.TermProgress_nocb)

        if undisturbed_fillHoles:
            # last step also fill-in undisturbed holes
            # remove filled-in zero holes
            dst_img = dst_band.ReadAsArray()
            dst_img = dst_img * (year_img != 0)
            dst_band.WriteArray(dst_img, 0, 0)
            # dst_band.FlushCache()

        if aoi_file is not None:
            dst_img = dst_band.ReadAsArray()

            aoi_ds = gdal.Open(aoi_file)
            aoi_img = aoi_ds.ReadAsArray()
            aoi_img = mask_img * aoi_img * undisturbed_value * (dst_img == nodata_value)

            dst_img = aoi_img + dst_img
            dst_band.WriteArray(dst_img, 0, 0)



        dst_ds = None
        dst_band = None

    dst_band = None
    dst_ds = None
    year_ds = None
    mem_ds = None


def apply_mask_from_other(src_file, aux_files):

    src_ds = gdal.Open(src_file)
    src_img = src_ds.ReadAsArray()
    src_ds = None

    if aux_files is not None:
        for aux_file in aux_files:
            out_file = os.path.join(os.path.dirname(src_file), os.path.basename(aux_file)[:-4] + '_mmu6.tif')
            if os.path.exists(out_file):
                print('File exists: %s' % out_file)
                continue
            else:
                print('Masking: %s' % out_file)
                aux_ds = gdal.Open(aux_file)
                aux_img = aux_ds.ReadAsArray()
                aux_ds = None
                aux_img[src_img == 0] = 0
                if not os.path.exists(out_file):
                    io.writeRaster(aux_img, out_file, ref_file=src_file, nodata=0)


##################################################
if __name__ == '__main__':
    path = r'p:\landtrendr_maps\landtrendr_1986-2019_v2_out'
    path_mask = r'p:\masks'

    year_file = 'bb_fdist_p27_19862019_disturbed_year.tif'

    mask_file = ['bb_forestmask_19862015_open_mmu11n4.tif']

    aoi_file = os.path.join(path_mask, 'bb_aoi.tif')

    for m in mask_file:
        filter_and_mask_disturbance_map(os.path.join(path, year_file),
                                        os.path.join(path_mask, m),
                                        aoi_file=aoi_file,
                                        mmu=6, neighbors=4, yearRange=(1986, 2019))

    # apply to partial
    if False:
        src_files = io.file_search(path, '*mmu6.tif')
        partial_file = io.file_search(path, '*partial_year.tif')
        for src_file in src_files:
            apply_mask_from_other(src_file, partial_file)

    # patches
    make_patches = True
    if make_patches:
        map_files = io.file_search(path, '*disturbed_year_mmu6*.tif')
        for map_file in map_files:
            create_disturbance_patches(map_file, connectivity=2, background=0)
