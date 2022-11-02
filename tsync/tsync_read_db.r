
fn <- '/Users/dirk/OneDrive - Humboldt-Universitaet zu Berlin, CMS/projects/brandsat/timesync/tsync_brandsat_bb.dat'

library(RSQLite)

?RSQLite

conn <- dbConnect(SQLite(), fn)

dbListTables(conn)

df <- dbReadTable(conn, 'plot_comments')
dv <- dbReadTable(conn, 'vertex')

dbDisconnect(conn)
