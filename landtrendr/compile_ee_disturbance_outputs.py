from pyxel import imageio as io
import os


# PROJ_LIB=c:\anaconda\Library\share\proj\
#if os.getenv('GDAL_DATA') is None:
#    os.environ['GDAL_DATA'] = r'\\141.20.140.91\SAN_Projects\Geomultisens\code\anaconda\Library\share\gdal'

if __name__ == '__main__':

    inpath = r'p:\landtrendr_maps\landtrendr_1986-2019_v2'
    outpath = r'p:\landtrendr_maps\landtrendr_1986-2019_v2_out'

    modus = 'fdist'
    types = ['disturbed', 'partial', 'cleared']
    timeperiod = '19862019'
    probabiltiy = 27

    if not os.path.exists(outpath):
        os.mkdir(outpath)

    for ftype in types:
        in_files = io.file_search(inpath, '*_%s_p%s_%s_%s.tif' % (modus, probabiltiy, timeperiod, ftype))
        if len(in_files) >= 1:
            for j, band in enumerate(['year', 'ddur', 'prob', 'clrp']):
                if ftype != 'disturbed' and band == 'clrp':
                    continue
                out_file = os.path.basename(in_files[0]).split('_')
                out_file[0] = 'bb'
                out_file = '%s_%s.tif' % ('_'.join(out_file).replace('.tif', ''), band)
                out_file = os.path.join(outpath, out_file)
                io.mosaic(in_files, out_file, of='GTiff', band=j+1, srcnodata=0, compress=True)
        else:
            print('Found %s files to play with.' % len(in_files))
