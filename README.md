
With the code supplied in this repository, we are able to forecast monthly vehicle registrations for new cars in Germany.
There are 3 R files associated with this.

1) TS_Registrations_Download.R - lets you download the relevant data from the website of the respecive government body

2) TS_Registrations_PrepareData.R - Adjust the data so that we can work with it (the excel files are pretty messy)

3) TS_Registrations_Forecast.R    - Choose the right model (ARIMA vs Exponential smoothing) and forecast the time period 06/2016-12/2016

You find the output of steps 1 and 2 in the master branch (DF_Unedited.csv / DF_Class.csv and DF_Model.csv) so you can 
investigate the data yourself.

