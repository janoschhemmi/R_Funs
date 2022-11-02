from osgeo import gdal
from multiprocessing import Pool
import imageio as io
import os
import numpy as np



def bfast_greatest_disturbance(in_file, out_path, mag_na_value=30000, na_value=-9999, n_dist=2, overwrite=False):

    src_ds = gdal.Open(in_file)
    src_img = src_ds.ReadAsArray().astype("int16")
    src_img[src_img == -32768] = na_value  # np.nan

    layer_names = [src_ds.GetRasterBand(x + 1).GetDescription() for x in range(src_ds.RasterCount)]

    outpath = os.path.join(out_path, os.path.basename(os.path.dirname(in_file)))

    if not os.path.exists(outpath):
        os.makedirs(outpath)

    for dist_type in ["fire", "harvest", "insect"]:
        #dist_type = "insect"
        #dist_type = "fire"
        out_file = os.path.join(outpath, os.path.basename(in_file)[:-4] + '_%s.tif' % dist_type)

        if os.path.isfile(out_file) and not overwrite:
            print('%s exists. Use overwrite keyword.' % out_file)
            return

        # create output array
        out_ras = np.ones((n_dist*4, src_img.shape[1], src_img.shape[2]), dtype=np.int16)

        l = np.array([x for x in range(len(layer_names)) if "year_%s" % dist_type in layer_names[x]])
        year_img = src_img[l, :, :]

        l = np.array([x for x in range(len(layer_names)) if "mag_%s" % dist_type in layer_names[x]])
        mag_img = src_img[l, :, :]

        l = np.array([x for x in range(len(layer_names)) if "prob_%s" % dist_type in layer_names[x]])
        prob_img = src_img[l, :, :]

        l = np.array([x for x in range(len(layer_names)) if "doy_%s" % dist_type in layer_names[x]])
        doy_fire = src_img[l, :, :]

        mag_img[year_img == na_value] = mag_na_value

        for i in range(n_dist):
            #i = 1
            inds = np.argmin(mag_img, 0)

            out_ras[(i * 4), :, :] = np.take_along_axis(year_img, inds[np.newaxis, :, :], 0).squeeze()
            out_ras[(i * 4) + 1, :, :] = np.take_along_axis(doy_fire, inds[np.newaxis, :, :], 0).squeeze()
            out_ras[(i * 4) + 2, :, :] = np.take_along_axis(mag_img, inds[np.newaxis, :, :], 0).squeeze()
            out_ras[(i * 4) + 3, :, :] = np.take_along_axis(prob_img, inds[np.newaxis, :, :], 0).squeeze()

            out_ras[(i * 4) + 2, :, :][out_ras[(i * 4) + 2, :, :] == mag_na_value] = na_value

            np.put_along_axis(mag_img, inds[np.newaxis, :, :], mag_na_value, axis=0)
            np.put_along_axis(year_img, inds[np.newaxis, :, :], na_value, axis=0)
            np.put_along_axis(doy_fire, inds[np.newaxis, :, :], na_value, axis=0)
            np.put_along_axis(prob_img, inds[np.newaxis, :, :], na_value, axis=0)

        out_names = ["greatest_year", "greatest_doy", "greatest_mag", "greatest_prob",
                     "second_year", "second_doy", "second_mag", "second_prob"]

        writeRaster(out_ras, out_file, ref_file=in_file, driver='Gtiff', bandNames=out_names)


def update_bandnames(in_file):
    src_ds = gdal.Open(in_file)

    bandnames = ["greatest_year", "greatest_doy", "greatest_mag", "greatest_prob",
                 "second_year", "second_doy", "second_mag", "second_prob"]

    [src_ds.GetRasterBand(x + 1).SetDescription(bandnames[x]) for x in range(src_ds.RasterCount)]

    src_ds.FlushCache()



if __name__ == '__main__':

    cores = 1
    do_process = True
    do_mosaic_tiles = True

    # root = r'p:\workspace\jan\fire_detection\break_detection\classified_breaks_cube\landsat-nbr-h40-o1-bp3-expansion_v3'
    root = r'p:\workspace\jan\fire_detection\break_detection\classified_breaks_cube\landsat-nbr-h40-o1-bp3-expansion'
    inpath = os.path.join(root, 'model_basic_mtry_10_redo_3')
    out_path = os.path.join(root, 'model_basic_mtry_10_redo_3_greatest')
    mosaic_path = os.path.join(root, 'model_basic_mtry_10_redo_3_greatest_disturbance_mosaic')

    if not os.path.exists(mosaic_path):
        os.makedirs(mosaic_path)

    mosaic_filebase = os.path.join(mosaic_path, 'model_basic_mtry_10_redo_3_greatest_disturbance_mosaic')


    in_files = file_search(inpath, "*.tif")

    if do_process:
        if cores == 1:
            for in_file in in_files:

                bfast_greatest_disturbance(in_file, out_path)
        else:
            pool = Pool(processes=cores)
            for in_file in in_files:
                print(in_file)
                pool.apply_async(bfast_greatest_disturbance, (in_file, out_path))
            pool.close()
            pool.join()

    if do_mosaic_tiles:
        # print(mosaic_file)
        for dist_type in ["fire", "harvest","insect"]:
            mosaic_file = mosaic_filebase + "_" + dist_type + ".tif"
            if not os.path.exists(mosaic_file):
                ln = file_search(out_path, 'X*%s.tif' % dist_type)
                if len(ln) > 1:
                    mosaic(ln, mosaic_file, compress=False)
                    update_bandnames(mosaic_file)
                    print(80 * '-')


    print("Done.")
