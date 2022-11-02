import os
from timesynctools import downloader as ts
from timesynctools import utils

if __name__ == '__main__':

    # utils.plotboundary(r'p:\timesync\bb\gis\tsync_plots_bb_raw.shp',
    #                    idfield='plotid', dst_ref=(10.0, 0.0), refIsCenter=False)
    # utils.plotboundary(r'p:\timesync\bb\gis\smp_fires_brandsat_4_distance_to_all.shp', idfield='UID',
    #                    dst_ref=(10.0, 0.0), refIsCenter=False)
    # utils.plotboundary(r'p:\timesync\bb\gis\tsync_fires_post_training1.gpkg', idfield='id',
    #                    dst_ref=(10.0, 0.0), refIsCenter=False)

    project_id = 8001
    path = r'p:\timesync\bb'
    plotFile = os.path.join(path, 'tsync_fires_post_training1.csv')

    # if reproject is False and crs is specified then crs is used only to
    #  interpret the sample coordinates - the chips stay in UTM
    # if reproject is True and crs is specified then crs is also used to reproject image chips

    # set cores=0 to skip process

    ts.extractPlots(plotFile, path, cores=0, reproject=True,
                    crs="EPSG:3035", crsTransform=[30, 0, 10.0, 0, -30, 0.0],
                    startYear=1985, endYear=2021,
                    startMonth=1, endMonth=12,
                    tc=True, b743=False, b432=False)

    ts.unstackChips(path, project_id=project_id, cores=4, tc=True, b743=False, b432=False)
