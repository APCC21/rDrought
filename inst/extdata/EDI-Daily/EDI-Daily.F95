!--------------------------------------------------------------------
!     Daily Effective Drought Index
!     developed by Hi-Ryong Byun, 1999      
!     E-mail : hrbyun@pknu.ac.kr
!
!     References
!     Byun, H.R. and D.A. Wilhite, 1999: Objective quantification of
!       drought severity and duration. J. Climate, 12, 2747-2756.
!     Byun, H.R. and D.K. Lee, 2002: Defining three rainy seasons and 
!       the hydrological summer monsoon in Korea using available water
!       resources index. 
!       J.Meteor. Soc. Japan, 80, 33-44.
!--------------------------------------------------------------------
      program EffectiveDroughtIndexProgram
      character(len=100) :: IFILE,OFILE
      real,allocatable,dimension(:)    :: prec,EP,EDI,AWRI
      integer,allocatable,dimension(:) :: jdata,CDD
      real,dimension(365)              :: Mprec,MEP,SD
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

      W = 6.478484

      do i=1+365,n
      EP(i) = funEP(365,prec(i-364:i))
      AWRI(i) = EP(i) / W
      enddo

      do j = 1,365
      MEP(j) = funMEP(365,Mprec,j)
      enddo

!-----Continued Dry Duration---------------------
      allocate(CDD(n))
      B = 0; icnt = 0; j = 0

      do i = 365+1,n
          EP(i) = funEP(365,prec(i-364:i))
      A = EP(i) - MEP(jdata(i))
      if( A .lt. 0 .and. B.lt.0 ) then
      icnt = icnt + 1
      CDD(i) = icnt
      else
      icnt = 0
      CDD(i) = 0
      endif
      B = A
      enddo
!------------------------------------------------

      call SD365(EP,MEP,n,Mst,Met,SD)
      allocate(EDI(n))

      do i = 365+1,n
      j = jdata(i)
      
      if(CDD(i).eq.0) then
      A = EP(i)
      B = MEP(j)
      C = SD(j)
      EDI(i) = (A-B)/C
      else
      MAXDD = (Msyr-Syear)*365 - jdata(1) 
      DD = MIN(CDD(i) + 365, MAXDD)
      A = funEP(DD,prec(i-DD+1:i))
      B = funMEP(DD,Mprec,j)
      C = funSD(prec,Mprec,DD,j,n,Mst,Met)
      EDI(i) = (A-B)/C
      endif

      enddo

!--------Writing---------------------------------
      write(10,101) 'Date','Prec','AWRI','EDI'
101   format(3x,a4,6x,a4,4x,a4,4x,a4)


      iyr = Syear+1
      do i = 365+1,n
      call JultoDate(jdata(i),imn,idy)
      write(10,100) iyr,imn,idy,prec(i),AWRI(i),EDI(i)
      if(jdata(i).eq.365) iyr=iyr+1
      enddo
      write(*,*) '== Calculation is completed =='
      write(*,*) 'Output file : '//trim(OFILE)
      write(*,*)

100   format(i4,'-',i2.2,'-',i2.2,f7.1,f8.1,f8.2)
!-------------------------------------------------

      end program

      function funSD(prec,Mprec,DD,J,n,Mst,Met)
      real,dimension(n)   :: prec
      real,dimension(365) :: Mprec   
      integer             :: DD

      k  =  (Met - Mst + 1)/365
      A  = funMEP(DD,Mprec,J)
      C  = 0
      do iyr=1,k
      I = Mst + (iyr-1)*365 + J-1
      B = funEP(DD,prec(I-DD+1:I)) 
      C = C + (A-B)**2
      enddo
      funSD = sqrt(C/(k-1))
      return
      end function funSD

      subroutine SD365(EP,MEP,n,Mst,Met,SD)
      real,dimension(n)   :: EP
      real,dimension(365) :: MEP,SD

      k = (Met - Mst + 1)/365
      do jdy=1,365
      A = 0
        do iyr=1,k
          I = Mst + (iyr-1)*365 + jdy-1
        A = A + (EP(I)-MEP(jdy))**2
        enddo
      SD(jdy) = sqrt(A/(k-1))
      enddo
      return
      endsubroutine SD365

      function funMEP(dd,Mprec,j)
      integer             :: dd
      real,dimension(365) :: Mprec

      A = 0; B = 0
      do i = 0, dd-1
      k = j - i
10    if(k.le.0) k = k+365
      if(k.le.0) goto 10
      A = A + Mprec(k)
      B = B + A/real(i+1)
      enddo
      funMEP = B 
      end function funMEP

      function funEP(dd,p365)
      integer             :: dd
      real,dimension(dd)  :: p365

      A = 0.; B = 0.
      do i = dd,1,-1
        A = A + p365(i)
        B = B + A/real(dd+1-i)
      enddo
      funEP = B
      end function funEP

      subroutine readprec(numif,n,prec,jdata,Msyr,Meyr,Mprec)
      real,dimension(n)      :: prec
      integer,dimension(n)   :: jdata
      real,dimension(365)    :: Mprec

      do i = 1,n
      read(numif,*,err=20) iyr,imn,idy,p
         if(LEAP(iyr,imn,idy).eq.366) then
          prec(i-1) = prec(i-1) + p 
          write(*,*)'Precipitation data for Feb 29 of each leap year'
          write(*,*)'were added to data for Feb 28.'
          goto 10
         endif
      prec(i) = p
      jdata(i) = DatetoJul(imn,idy)

      if(iyr.ge.Msyr .and. iyr.le.Meyr) then
        jday = DatetoJul(imn,idy)
        Mprec(jday) = Mprec(jday) + p
      endif

      if(p.lt.0) then
      write(*,100) 'In line ',i,': minus value of precipitation'
      write(*,*) 'Program is forced to stop.'
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
      write(*,*) 'Error reading input file.'
      stop

      end subroutine readprec

      subroutine getinfo(numif,Msyr,Meyr,n,Mst,Met,Syear)
      integer :: Syear,Eyear

      itmp = 0; n = 0
      read(numif,*) iyr,imn,idy
      n = n+1
      Syear = iyr

99    read(numif,*,end=999) iyr,imn,idy
             if(LEAP(iyr,imn,idy).eq.366) goto 10  
      n = n+1
      if(iyr.eq.Msyr.and.imn.eq.1.and.idy.eq.1) then
      Mst = n

        if(Mst.lt.366) then
        write(*,*) 'ERROR'
        write(*,*) 'Adjust start year of calibration period '
        write(*,*) 'to 1 year after start year of precipitation data.'
        write(*,*) 'Program is forced to stop.'
        stop
        endif

      endif

      if(iyr.eq.Meyr.and.imn.eq.12.and.idy.eq.31) Met = n
      Eyear = iyr
10      continue
      goto 99
999   continue
 
      if(n/365. .lt. 31) then
       write(*,*) 'At least 31 yrs of data are recommended'
      endif      

      A = (Met - Mst + 1)/365
      K = Meyr - Msyr + 1
      if( A .eq. K ) then
      write(*,100) 'Precipitation data: ',Syear,'-',Eyear
      write(*,100) 'Calibration period: ',Msyr,'-',Meyr,int(A),' years'
100   format(a,i4,a,i4,i4,a)
        else
        write(*,*) 'Error in  calibration period'
        write(*,*) 'It should be from 1 Jan YYYY to 31 Dec YYYY.'
        write(*,*) 'Program is forced to stop.'
        stop
      endif

      return
      end subroutine getinfo

      FUNCTION LEAP(iyr,imn,idy)
      LEAP = 365
      M1=MOD(iyr,4); M2=MOD(iyr,100); M3 = MOD(iyr,400)
      IF(M1==0.AND.M2/= 0.OR.M3==0) then
      IF(imn==2.AND.idy==29) then
        LEAP = 366
      endif;endif
      END FUNCTION LEAP

      function DatetoJul(imn,idy)
      integer,dimension(12)::mon=(/31,28,31,30,31,30,31,31,30,31,30,31/)

      DatetoJul = 0
      do i = 1,imn-1
      do j = 1,mon(i)
        DatetoJul = DatetoJul + 1
      enddo;enddo
      DatetoJul = DatetoJul + idy
      end function DatetoJul

      subroutine JultoDate(j,month,day)
      integer,dimension(12)::mon=(/31,28,31,30,31,30,31,31,30,31,30,31/)
      integer               :: month, day

      do imn=1,12
      do idy=1,mon(imn)
      if(j.eq.DatetoJul(imn,idy)) then
      month=imn
      day  =idy
      endif
      enddo;enddo
      end subroutine JultoDate

      subroutine memo(i)
      PRINT*, ''
      PRINT*, '  ***********************************'
      PRINT*, '  DAILY EDI PROGRAM         '
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

