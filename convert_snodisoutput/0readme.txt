You can use the R script in this directory to convert the grads formatted .dat files from snodis to .nc and .tif

Place the .dat files in yearly folders in "recon_grads". Make sure there aren't any other folders in recon_grads/
recon_grads/
  2000/
  2001/
  ... etc

Open the convert_grads.R and change the USER INPUT to identify the domain coordinates, resolution, etc. If you are copying from swe.ctl (this is a grads control file that georeferences the .dat file) from a snodis run, just remember that the grid is fixed bottom left, and the coordinates are cell center.

Source (run) convert_grads.R and you should get recon_nc/ and recon_tif/ with the corresponding files.
