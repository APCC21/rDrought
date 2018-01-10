!--------------------------------------------------------------------
!     Monthly Effective Drought Index
!     developed by Hi-Ryong Byun, 1999      
!     E-mail : hrbyun@pknu.ac.kr
!
!     References
!     Byun, H.R. and D.A. Wilhite, 1999: Objective quantification of
!       drought severity and duration. J. Climate, 12, 2747-2756.
!     Byun, H.R. and D.K. Lee, 2002: Defining three rainy seasons and 
!       the hydrological summer monsoon in Korea using available water resources index. 
!       J.Meteor. Soc. Japan, 80, 33-44.
!--------------------------------------------------------------------
      program MonthlyEffectiveDroughtIndexProgram

      character(len=100) :: IFILE,OFILE
      real,allocatable,dimension(:)    :: prec,EP,EDI,AWRI     
      integer,allocatable,dimension(:) :: jdata,CDD
      real,dimension(12)              :: Mprec,MEP,SD
      integer                          :: DD,Syear

      namelist/IOFILE/ IFILE,OFILE
      namelist/CalibPeriod/ Msyr, Meyr

      call memo(1)

      open(1,file='NAMELIST.TXT',status='old')
      read(1,IOFILE); read(1,CalibPeriod); close(1)

      write(*,*) 'Input file : '//trim(IFILE)

      open(10,file=OFILE)

      open(2,file=IFILE,status='old',position='rewind')
      numif = 2
      call getinfo(numif,Msyr,Meyr,n,Mst,Met,Syear)
      REWIND 2

      allocate(prec(n)); allocate(jdata(n))

      call readprec(numif,n,prec,jdata,Msyr,Meyr,Mprec)

      allocate(EP(n))
      allocate(AWRI(n))

      W = 3.103211

      do i=1+12,n
      EP(i) = funEP(12,prec(i-12+1:i))
      AWRI(i) = EP(i) / W
      enddo

      do j = 1,12
      MEP(j) = funMEP(12,Mprec,j)
      enddo

!-----Continued Dry Duration----------------------
      allocate(CDD(n))
      B = 0; icnt = 0; j = 0

      do i = 12+1,n
          EP(i) = funEP(12,prec(i-12+1:i))
      A = EP(i) - MEP(jdata(i))
      if( A.lt.0 .and. B.lt.0 ) then
      icnt = icnt + 1
      CDD(i) = icnt
      else
      icnt = 0
      CDD(i) = 0
      endif
      B = A
      enddo
!-------------------------------------------------

      call SD12(EP,MEP,n,Mst,Met,SD)
      allocate(EDI(n))

      do i = 12+1,n
      j = jdata(i)
      
      if(CDD(i).eq.0) then
      A = EP(i)
      B = MEP(j)
      C = SD(j)
      EDI(i) = (A-B)/C
      else
      MAXDD = (Msyr-Syear)*12 - jdata(1) + 1
      DD = MIN(CDD(i) + 12, MAXDD)
      A = funEP(DD,prec(i-DD+1:i))
      B = funMEP(DD,Mprec,j)
      C = funSD(prec,Mprec,DD,j,n,Mst,Met)
      EDI(i) = (A-B)/C
      endif

      enddo

!--------Writing----------------------------------
      write(10,101) 'Date','Prec','AWRI','EDI'
101   format(3x,a4,6x,a4,4x,a4,4x,a4)

      iyr = Syear+1
      do i = 12+1,n
      write(10,100) iyr,jdata(i),prec(i),AWRI(i),EDI(i)
      if(jdata(i).eq.12) iyr=iyr+1
      enddo

      write(*,*) '== Calculation is completed =='
      write(*,*) 'Output file : '//trim(OFILE)
      write(*,*)

100   format(i4,'-',i2.2,'-15',f7.1,f8.1,f8.2)
!-------------------------------------------------

      end program

      function funSD(prec,Mprec,DD,J,n,Mst,Met)
      real,dimension(n)   :: prec
      real,dimension(12) :: Mprec   
      integer             :: DD

      k  =  (Met - Mst + 1)/12
      A  = funMEP(DD,Mprec,J)
      C  = 0
      do iyr=1,k
      I = Mst + (iyr-1)*12 + J-1
      B = funEP(DD,prec(I-DD+1:I)) 
      C = C + (A-B)**2
      enddo
      funSD = sqrt(C/(k-1))
      return
      end function funSD

      subroutine SD12(EP,MEP,n,Mst,Met,SD)
      real,dimension(n)   :: EP
      real,dimension(12) :: MEP,SD

      k = (Met - Mst + 1)/12
      do imn=1,12
      A = 0
        do iyr=1,k
          I = Mst + (iyr-1)*12 + imn-1
        A = A + (EP(I)-MEP(imn))**2
        enddo
      SD(imn) = sqrt(A/(k-1))
      enddo
      return
      endsubroutine SD12

      function funMEP(dd,Mprec,j)
      integer             :: dd
      real,dimension(12) :: Mprec

      A = 0; B = 0
      do i = 0, dd-1
      k = j - i
10    if(k.le.0) k = k+12
      if(k.le.0) goto 10
      A = A + Mprec(k)
      B = B + A/real(i+1)
      enddo
      funMEP = B 
      end function funMEP

      function funEP(dd,p12)
      integer             :: dd
      real,dimension(dd)  :: p12

      A = 0.; B = 0.
      do i = dd,1,-1
        A = A + p12(i)
        B = B + A/real(dd+1-i)
      enddo
      funEP = B
      end function funEP

      subroutine readprec(numif,n,prec,jdata,Msyr,Meyr,Mprec)
      real,dimension(n)      :: prec
      integer,dimension(n)   :: jdata
      real,dimension(12)     :: Mprec

      do i = 1,n
      read(numif,*,err=20) iyr,imn,p
      prec(i)  = p
      jdata(i) = imn

      if(iyr.ge.Msyr .and. iyr.le.Meyr) then
        Mprec(imn) = Mprec(imn) + p
      endif

      if(p.lt.0) then
      write(*,100) 'In line ',i,': minus value of precipitation'
      write(*,100) 'Program is forced to stop.'
100   format(a,i8,a)
      stop
      endif

10    continue      
      enddo

       K = Meyr - Msyr+1
       if(K.lt.30) then
       write(*,*) 'At least 30 yrs of calbration preiod is recommended'
       endif
      Mprec = Mprec/K
      return

20    continue
      write(0,*) 'Error reading input file.'
      stop

      end subroutine readprec

      subroutine getinfo(numif,Msyr,Meyr,n,Mst,Met,Syear)
      integer :: Syear,Eyear
      Syear = -999; Eyear = -999
     

      itmp = 0; n = 0
      read(numif,*) iyr,imn
      n = n+1
      Syear = iyr

99    read(numif,*,end=999) iyr,imn
      n = n+1
      if(iyr.eq.Msyr.and.imn.eq.1) then
      Mst = n

        if(Mst.lt.13) then
        write(*,*) ' ERROR'
        write(*,*) ' Adjust start year of calibration period '
        write(*,*) ' to 1 year after start year of precipitation data.'
        write(*,*) ' Program is forced to stop.'
        stop
        endif

      endif

      if(iyr.eq.Meyr.and.imn.eq.12) Met = n
      Eyear = iyr
      goto 99
999   continue

      if(Msyr.lt.Syear) then
      write(*,*) ' ERROR in start year of calibration period'
      write(*,*) ' Program is forced to stop.'
      stop
      else if(Meyr.gt.Eyear) then
      write(*,*) ' ERROR in last year of calibration period'
      write(*,*) ' Program is forced to stop.'
      stop
      endif
 
      if(n/12. .lt. 31) then
       write(*,*) ' At least 31 yrs of data are recommended.'
      endif      

      A = (Met - Mst + 1)/12
      K = Meyr - Msyr + 1
      if( A .eq. K ) then
      write(*,100) ' Precipitation data: ',Syear,'-',Eyear
      write(*,100) ' Calibration period: ',Msyr,'-',Meyr,int(A),' years'
 100  format(a,i4,a,i4,i4,a)
        else
        write(*,*) ' Error in  calibration period'
        write(*,*) ' It should be from Jan YYYY to Dec YYYY.'
        write(*,*) ' Program is forced to stop.'
        stop
      endif

      return
      end subroutine getinfo

      subroutine memo(i)
      PRINT*, ''
      PRINT*, '  ***********************************'
      PRINT*, '  MONTHLY EDI PROGRAM         '
      PRINT*, '  MADE BY PROF. BYUN, HI-RYONG'
      PRINT*, '  DEPT. OF ENVIRONMENTAL ATMOSPHERIC'
      PRINT*, '  SCIENCES, PUKYONG NATIONAL UNIVERSITY,'
      PRINT*, '  REPUBLIC OF KOREA'
      PRINT*, ''
      PRINT*, '  PHONE: +82-51-629-6640'
      PRINT*, '  E-MAIL: hrbyun@pknu.ac.kr'
      PRINT*, '  ***********************************'
      PRINT*, ''
      end subroutine      

