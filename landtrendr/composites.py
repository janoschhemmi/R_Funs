from pyxel import imageio as io
import os

inpath = r'd:\GDrive\europe_disturbances'
outpath = r'p:\landtrendr_maps\bb\composites'


for index in ['NBR', 'TCW']:
    infiles = io.file_search(inpath, 'bb_source_%s_19852019-*.tif' % index)

    if len(infiles) > 0:
        outfile = os.path.join(outpath,  'bb_source_%s_19852019.tif' % index)

        if not os.path.isfile(outfile):
            io.mosaic(infiles, outfile, of='GTiff', compress=True, bigtiff=True)