=========================================================================
Effective Drought Index (EDI) Fortran Program 

-  developed by Hi-Ryong Byun, 1999      
  * Byun H. R., and D. A. Wilhite, 1999
   : Objective quantification of drought severity and duration. J. Climate,12, 2747-2756.
     E-mail : hrbyun@pknu.ac.kr
=========================================================================


 How to run?
-------------

  1. Edit "NAMELIST.TXT"

  2. Excute (double click) "EDI.exe"
    OR you can compile "EDI.F95" and Excute for yourself


 NAMELIST.TXT
--------------
&IOFILE
 IFILE = 'input.txt',
 OFILE = 'output.txt' /
# Set input file (precipitation data) & output file (AWRI & EDI data)

&CalibPeriod
 Msyr = 1981,
 Meyr = 2010 /
# Set calibration period (It is recommended to set 30 years, climatological period)


 INPUT FILE
------------
    FORTRAN FORMAT :   READ(2,*) year, month, precipitaion        : for monthly calculation
                    or READ(2,*) year, month, day, precipitation  : for daily calculation

    ¡Ø A year must be 365 days (12 months).
       If a year is a leap year, add ¡°precipitation of 29 February¡± to ¡°precipitation of 28 February".
 
    ¡Ø Start date of precipitation data must be 1 January.

    EXAMPLE (for daily calculation)
    ---------------------------------------------
      2008     1       1            0
      2008     1       2            0
      2008     1       3          1.5
      2008     1       4          3.5
      2008     1       5         10.0
       .       .       .           .
       .       .       .           .
       .       .       .           .
      2008     2      27         11.0
      2008     2      28          2.0      
      2008     3       1          3.0
       .       .       .           .
       .       .       .           .
       .       .       .           .
     --------------------------------------------------     
       year   month   day   precipitation (mm)
            


     EXAMPLE (for monthly calculation)
    ---------------------------------------------
      2008     1           50
      2008     2           60
      2008     3           10
      2008     4           35
      2008     5          100
       .       .           .
       .       .           .
       .       .           .
      2008    12           20
      2009     1           40      
      2009     2           30
       .       .           .
       .       .           .
       .       .           .
     --------------------------------------------------     
       year   month   precipitation (mm)   



 OUTPUT VALUE
------------
 EDI : Effective Drought Index
     - Climatological/Meteorological drought index 
     - Measures the drought severity 
     Extreme drought < -2.0 < Severe drought < -1.5 < moderate drought < -1.0 < Near normal
     Extreme wet     >  2.0 > Severe wet     >  1.5 > moderate wet     >  1.0 > Near normal

 AWRI : Available Water Resouces Index
      - Hydrological Index (unit: mm)
      - Estimates current available water resources accumulated for 365 days
      Extreme water shortage < 50 mm < severe water shortage < 100 mm < moderate water shortage < 150 mm
      (These ranges of water shortage are based on hydrological climate in Korea. It depends on regional climate)

#### NOTICE ####
For calculating the daily (monthly) EDI value on a specific day (month), the precipitation data accumulated for previous 365 days (12 months) are required.
Missing data is not allowed. It is recommended to fill missing data by climatological mean value.

#### WEB SITE OF DROUGHT MONITORING####
 Meteorological Disaster Research (http://atmos.pknu.ac.kr)
 Korean Daily Drought Monitoring (http://atmos.pknu.ac.kr/~intra)
 East Asia Monthly Drought Monitoring (http://atmos.pknu.ac.kr/~intra2)

#### REFERENCES ####
Byun, H.R. and D.A. Wilhite, 1999: Objective quantification of drought severity and duration. J. Climate, 12, 2747-2756.
Byun, H.R. and D.K. Lee, 2002: Defining three rainy seasons and the hydrological summer monsoon in Korea using available water resources index. J.Meteor. Soc. Japan, 80, 33-44.
Kim, D.W., H.R. Byun, and K.S. Choi, 2009: Evaluation, Modification, and Application of the Effective Drought Index to 200-Year Drought Climatology of Seoul, Korea, J. Hydrol., 378, 1-12.
