#!/bin/bash

for yr in {2000..2014}
do
echo "Getting $yr..."

mkdir $yr
scp -i ~/.ssh/snowserver_rsa snowserver.colorado.edu:/data/hydroProjects/SWE/Sierras/Spatial_SWE/SWE_SNODIS/$yr/swe.dat $yr/swe.dat


# /usr/local/bin/wget "http://cdec.water.ca.gov/cgi-progs/queryCSV?station_id=${stnID[$cnt]}&dur_code=D&sensor_num=82&start_date=1/1/1978&end_date=08/04/2017" -O "${stnID[$cnt]}.txt"
# grep -l "can't find Station ID" ${stnID[$cnt]}.txt | xargs rm -rf # Delete file if Station does not exist.
# if [ -e ${stnID[$cnt]}.txt ]; then # if file not deleted in the above step.
#    sed -i -e '1,2d' "${stnID[$cnt]}.txt" # Delete header lines 1--3
#    sed -i -e 's/m/-9999/g' "${stnID[$cnt]}.txt" # Set missing values to -9999
#    sed -i -e 's/,/ /g' "${stnID[$cnt]}.txt"
#    sed -i -e 's/^M//g' "${stnID[$cnt]}.txt" # Remove trailing special character on each line
#    line1field1=`head -n1 ${stnID[$cnt]}.txt | cut -d ' ' -f 1`
#    line1field1trick=$line1field1'trick' # to prevent a null string.
#    if [ $line1field1trick != '19970101trick' ]; then rm -rf ${stnID[$cnt]}.txt; fi # Delete file if starts later than specified.
# fi
done
