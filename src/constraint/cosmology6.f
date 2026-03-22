	program cosmology

c        Cosmology Predictor Code
c
c        modified 5/6/06 to do the monte carlo properly
c
c
c        the output of the BBN code, bbn_outputa.txt, at one eta:
c
c 5.998611E-10  2.664760E-05  6.563159E-07
c 5.998611E-10  1.049415E-05  2.913498E-07
c 5.998611E-10  2.453572E-01  1.237432E-04
c 5.998611E-10  4.543833E-10  2.914029E-11
c  1000  3.000000E+00  5.998611E-10
c
c        this should be an INPUT for a NEW ANALYSIS CODE
c          the OUTPUT of this code should be four files:
c
c        eta_1 d_1 d_uncertainty_1
c        eta_2 d_2 d_uncertainty_2
c        ...
c
c        eta_1 3He_1 3He_uncertainty_1
c        eta_2 3He_2 3He_uncertainty_2
c        ...
c        
c        and similar for 4He and 7Li
c
c
c        These abundances with uncertainties vs. eta from the monte carlo
c          are one INPUT for this program  [4 different files]
c          note: we should require at least 3 different eta values for input
c
c
c        Another INPUT: sets of observations
c        accept just one set of observational abundances
c        we can repeat the code when a new set is chosen
c         NOTE: uncertainties are needed as input
c         d_observ   d_observ_unc  
c         3He_observ 3He_observ_unc   
c         4He_observ 4He_observ_unc  
c         7Li_observ 7Li_observ_unc
c
c
c        OUTPUT:   
c
c        ABUNDANCE SET   SIMUL NAME      ABUNDANCE  eta_lo  eta_hi
c         set 1             latest          d          5.5    5.9
c         set 1             latest          3He        5.4    6.3
c         set 1             latest          4He        5.8    6.1
c         set 1             latest          7Li        2.4    7.3
c
c
c      OUTLINE
c         1. read in BBN code output - abundances as a function of eta
c
c         2. populate these into a matrix in the new code
c
c         3. from this generate a "lo and hi" curves of abundance vs. eta - don't need the 'middle' value
c             UNLESS the run was a non MC run [the abundances have the special value ....], in which 
c             case you just use the middle value only
c
c         4. read in abundance observation values 
c
c            
c         5. FOR each of the curves LO and HI [or mid for the non-MC case] 
c            do operations to find out what the curves "look like"...
c              monotonic ... increasing, decreasing, or
c            if it is saddle shaped (decreasing then increasing)
c
c            handle each case - decreasing, increasing, saddle shaped - 
c              differently
c 
c          for saddle shaped functions
c                find the inflection point as this is needed later




c          declare variables
c
      implicit none

      integer*4 n_obs               ! number of abundance observation 'sets' 
      parameter (n_obs=3)           !
      integer*4 n                   ! looping over observational abundances 
c                                   !n = 1: MID
c                                   !n = 1: LO
c                                   !n = 1: HI
c                                   !NOTE: asymmetric abundance uncertainties are allowed
      real*8 h2_o(n_obs)       ! observed primordial 2H abundance
      real*8 he3_o(n_obs)		! observed primordial 3He abundance
      real*8 he4_o(n_obs)		! observed primordial 4He abundance
      real*8 li7_o(n_obs)          ! observed primordial 7Li abundance


      integer*4 num_eta_max                      ! number of maximum possible eta points accepted
      parameter (num_eta_max=100)                 
      integer*4 num_eta                          ! number of eta points actually read in
      integer*4 j                              ! looping over eta values
      integer*4 j_max,j_min                    ! eta values corresponding to the max and min eta values 
      integer*4 j_inf(3)        ! index of eta values for the INFLECTION POINT OF THE MID, LO, and HI 7Li curves
	  integer*4 counter                        ! temporary count of number of eta values  
	  integer*4 error_eta                      ! 0 if eta values match, 1 if not  
      real*8 eta_max                           ! maximum eta value
      real*8 eta(num_eta_max)                           ! eta values
      real*8 etemp1(num_eta_max),etemp2(num_eta_max),etemp3(num_eta_max)      ! temporary eta values
      integer*4 j_bracket(3,6,n_obs)     ! gives the lower number of the 
                                         ! eta values 
                                         ! that bracket the observational bound

      real*8 eta_inter(3,6,n_obs)        ! interpolated eta values
      real*8 eta_inter_dummy        ! default value

	  real*8  eta_lim_h2(3)             ! limit on eta from 2H observation
      real*8  eta_lim_he3(3)           ! limit on eta from he4 observation
      real*8  eta_lim_he4(3)           ! limit on eta from he4 observation
      real*8  eta_lim_li7a(3)           ! limit on eta from li7 observation - below inflection point
      real*8  eta_lim_li7b(3)           ! limit on eta from li7 observation - above inflection point
      real*8  dummy_lim                    ! dummy limit
	  integer*4  error_lim_h2(3)              ! error on the limits
	  integer*4  error_lim_he3(3)              ! error on the limits
	  integer*4  error_lim_he4(3)              ! error on the limits
	  integer*4  error_lim_li7a(3)              ! error on the limits - below inflection point
	  integer*4  error_lim_li7b(3)              ! error on the limits - above inflection point
	  integer*4  error_lim_li7                  ! sum of errors on 7li limits

      integer*4 l                      ! loop over curves of abundance vs. eta
c                                      ! l = 1: MID  [use only this for non-MC runs]
c                                      ! l = 2: LO
c                                      ! l = 3: HI




      integer*4 k                      ! loop over isotopes
c                                      ! k = 1: eta
c                                      ! k = 2: d
c                                      ! k = 3: 3He
c                                      ! k = 4: 4He
c                                      ! k = 5: 7Li

      real*8 h2(num_eta_max)                            ! deuterium as a function of eta 
      real*8 h2_lo(num_eta_max)                            ! deuterium LOWER ENVELOPE as a function of eta 
      real*8 h2_hi(num_eta_max)                            ! deuterium UPPER ENVELOPE as a function of eta 
      real*8 unc_h2(num_eta_max)                            ! deuterium uncertainty as a function of eta 
      real*8 h2_min                                   ! min value of deuterium curve 
      real*8 h2_lo_min                                   ! min value of LOWER ENVELOPE deuterium curve 
      real*8 h2_hi_min                                   ! min value of UPPER ENVELOPE deuterium curve 

      real*8 he3(num_eta_max)                          ! 3He as a function of eta
      real*8 he3_lo(num_eta_max)                          ! 3He LOWER ENVELOPE as a function of eta
      real*8 he3_hi(num_eta_max)                          ! 3He UPPER ENVELOPE as a function of eta
      real*8 unc_he3(num_eta_max)                          ! 3He  uncertainty as a function of eta
      real*8 he3_min                                   ! min value of he3 curve 
      real*8 he3_lo_min                                   ! min value of LOWER ENVELOPE he3 curve 
      real*8 he3_hi_min                                   ! min value of UPPER ENVELOPE he3 curve 

      real*8 he4(num_eta_max)                          ! 4He as a function of eta
      real*8 he4_lo(num_eta_max)                          ! 4He LOWER ENVELOPE as a function of eta
      real*8 he4_hi(num_eta_max)                          ! 4He UPPER ENVELOPE as a function of eta
      real*8 unc_he4(num_eta_max)                          ! 4He  uncertainty as a function of eta
      real*8 he4_max                                   ! min value of he4 curve 
      real*8 he4_lo_max                                   ! max value of LOWER ENVELOPE he4 curve 
      real*8 he4_hi_max                                   ! max value of UPPER ENVELOPE he4 curve 

      real*8 li7(num_eta_max)                             ! 7Li as a function of eta
      real*8 li7_lo(num_eta_max)                             ! 7Li LOWER ENVELOPE as a function of eta
      real*8 li7_hi(num_eta_max)                             ! 7Li UPPER ENVELOPE as a function of eta
      real*8 unc_li7(num_eta_max)                             ! 7Li  uncertainty as a function of eta
      real*8 li7_min                                   ! min value of li7 curve 
      real*8 li7_lo_min                                   ! min value of LOWER ENVELOPE li7 curve 
      real*8 li7_hi_min                                   ! min value of UPPER ENVELOPE li7 curve 
      real*8 li7_max                                   ! max value of li7 curve 
      real*8 li7_lo_max                                   ! max value of LOWER ENVELOPE li7 curve 
      real*8 li7_hi_max                                   ! max value of UPPER ENVELOPE li7 curve 


      integer*4 monotonic(3,5)               ! monotonic(l,k) = 1 if curve l for isotope k is 
                                               !monotonic increasing
                                           ! monotonic(l,k) = 0  if curve l for isotope k is
                                               !saddle shaped (decr/ incr)
                                           ! monotonic(l,k) = -1  if curve l for isotope k is 
                                               ! monotonic decreasing
                                           ! monotonic(l,k) = 3  if a problem!
                                           ! monotonic(l,k) = -2  if a problem ! 

c                                      valid (l,k) combinations:
c                                      ! (1,1): eta
c                                      ! (1,2): d central value   (2,2): d LO    (3,2): d HI envelope
c                                      ! (1,3): he3 central value   (2,3): he3 LO    (3,3): he3 HI envelope
c                                      ! (1,4): he4 central value   (2,4): he4 LO    (3,4): he4 HI envelope
c                                      ! (1,5): li7 central value   (2,5): li7 LO    (3,5): li7 HI envelope

      integer*4 bounds(3,5,n_obs)           ! bounds(l,k,n) = 0 if curve l for isotope k is
                                             ! within the observational bounds in observation n
                                          ! bounds(l,k,n) = 1 if curve l for isotope k is TOO HIGH
                                             ! above the observational bounds in observation n
                                          ! bounds(l,k,n) = -1 if curve l for isotope k is TOO LOW
                                             ! below the observational bounds in observation n
                                          ! bounds(l,k,n) = -2 if curve l for isotope k is
                                             ! OK for values BELOW the 
                                             ! inflection point, but not OK 
                                             ! for values ABOVE, in observation n
                                          ! bounds(l,k,n) = +2 if curve l for isotope k is
                                             ! OK for values ABOVE the 
                                             ! inflection point, but not OK 
                                             ! for values BELOW, in observation n

c                                      valid (l,k,n) combinations:
c                                      ! (1,2,1): d central curve, D OBSERV CENTRAL VALUE   
c                                      ! (1,2,2): d central curve, D OBSERV LO VALUE   
c                                      ! (1,2,3): d central curve, D OBSERV HI VALUE   
c                                      ! (2,2,1): d LO curve, D OBSERV CENTRAL VALUE   
c                                      ! (2,2,2): d LO curve, D OBSERV LO VALUE   
c                                      ! (2,2,3): d LO curve, D OBSERV HI VALUE   
c                                      ! (3,2,1): d HI curve, D OBSERV CENTRAL VALUE   
c                                      ! (3,2,2): d HI curve, D OBSERV LO VALUE   
c                                      ! (3,2,3): d HI curve, D OBSERV HI VALUE   
c
c                                      ! (1,3,1): he3 central curve, he3 OBSERV CENTRAL VALUE   
c                                      ! (1,3,2): he3 central curve, he3 OBSERV LO VALUE   
c                                      ! (1,3,3): he3 central curve, he3 OBSERV HI VALUE   
c                                      ! (2,3,1): he3 LO curve, he3 OBSERV CENTRAL VALUE   
c                                      ! (2,3,2): he3 LO curve, he3 OBSERV LO VALUE   
c                                      ! (2,3,3): he3 LO curve, he3 OBSERV HI VALUE   
c                                      ! (3,3,1): he3 HI curve, he3 OBSERV CENTRAL VALUE   
c                                      ! (3,3,2): he3 HI curve, he3 OBSERV LO VALUE   
c                                      ! (3,3,3): he3 HI curve, he3 OBSERV HI VALUE   
c
c                                      ! (1,4,1): he4 central curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (1,4,2): he4 central curve, he4 OBSERV LO VALUE   
c                                      ! (1,4,3): he4 central curve, he4 OBSERV HI VALUE   
c                                      ! (2,4,1): he4 LO curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (2,4,2): he4 LO curve, he4 OBSERV LO VALUE   
c                                      ! (2,4,3): he4 LO curve, he4 OBSERV HI VALUE   
c                                      ! (3,4,1): he4 HI curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (3,4,2): he4 HI curve, he4 OBSERV LO VALUE   
c                                      ! (3,4,3): he4 HI curve, he4 OBSERV HI VALUE   
c
c                                      ! (1,5,1): li7 central curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (1,5,2): li7 central curve, he4 OBSERV LO VALUE   
c                                      ! (1,5,3): li7 central curve, he4 OBSERV HI VALUE   
c                                      ! (2,5,1): li7 LO curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (2,5,2): li7 LO curve, he4 OBSERV LO VALUE   
c                                      ! (2,5,3): li7 LO curve, he4 OBSERV HI VALUE   
c                                      ! (3,5,1): li7 HI curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (3,5,2): li7 HI curve, he4 OBSERV LO VALUE   
c                                      ! (3,5,3): li7 HI curve, he4 OBSERV HI VALUE   


      real*8 num,denom,temp1,temp2,temp3            ! temporary variables for interpolation

c--------------------------------------------------------------------------------------------------------------------
c          open input and output files
c
      open(unit=30,file='abund_h2_out.txt',status='old')
      open(unit=31,file='abund_he3_out.txt',status='old')
      open(unit=32,file='abund_he4_out.txt',status='old')
      open(unit=33,file='abund_li7_out.txt',status='old')
c
      open(unit=4,file='abund_observ.dat',status='old')
c
      open(unit=9,file='cosmo_out.txt',status='old')



c--------------------------------------------------------------------------------------------------------------------
c     initialize counter for eta values

      counter = 0

c--------------------------------------------------------------------------------------------------------------------
c          read in BBN code output - abundances as a function of eta
c

c--------------------------------------------------------------------------------------------------------------------
c     read in deuterium
c           
	  write(9,5)
 5	  format(' eta  deuterium  uncertainty')

	  do j=1, num_eta_max
         read(30,7,END=9)eta(j),h2(j),unc_h2(j)
         write(9,7)eta(j),h2(j),unc_h2(j)
		 counter=counter+1
 7       FORMAT(3(1pe13.6,1x))
      enddo
 9    continue
	  
	  num_eta=counter     ! actual number of eta values
	  if(num_eta.le.3) then
	     write(9,10)num_eta
 10      format(' PROBLEM: 3 or less eta values!!')
      endif
	  		
c--------------------------------------------------------------------------------------------------------------------
c     read in he3
c           
	  write(9,11)
 11	  format(' eta  He3  uncertainty')

      do j=1, num_eta_max
         read(31,12,END=13)etemp1(j),he3(j),unc_he3(j)
         write(9,12)etemp1(j),he3(j),unc_he3(j)
 12      FORMAT(3(1pe13.6,1x))
      enddo
 13   continue

c--------------------------------------------------------------------------------------------------------------------
c     read in he4
c           
	  write(9,14)
 14	  format(' eta  4He  uncertainty')

      do j=1, num_eta_max
         read(32,15,END=16)etemp2(j),he4(j),unc_he4(j)
         write(9,15)etemp2(j),he4(j),unc_he4(j)
 15      FORMAT(3(1pe13.6,1x))
      enddo
 16   continue

c--------------------------------------------------------------------------------------------------------------------
c     read in li7
c           
	  write(9,17)
 17	  format(' eta  7Li  uncertainty')

      do j=1, num_eta_max
         read(33,18,END=19)etemp3(j),li7(j),unc_li7(j)
         write(9,18)etemp3(j),li7(j),unc_li7(j),(li7(j)-unc_li7(j)),
     |           (li7(j)+unc_li7(j))
 18      FORMAT(5(1pe13.6,1x))
      enddo
 19   continue


c--------------------------------------------------------------------------------------------------------------------
c     check if all eta values match
	  
	  error_eta=0
	  
      do j=1,num_eta
        if(eta(j).ne.etemp1(j)) then
		   error_eta=1
		endif
        if(eta(j).ne.etemp2(j)) then
		   error_eta=1
		endif
        if(eta(j).ne.etemp2(j)) then
		   error_eta=1
		endif
      enddo

      if(error_eta.eq.1) then
 	     write(9,20)
 20      format(/'problem with eta values not matching'/)

	  else
	     write(9,21)
 21      format(/' eta values match'/)
 
	  endif
	   

c--------------------------------------------------------------------------------------------------------------------
c          populate LOWER ENVELOPE and UPPER ENVELOPE curves of abundance vs. eta
c

      do j=1,num_eta
        h2_lo(j) = h2(j) - unc_h2(j)                      ! deuterium LOWER ENVELOPE as a function of eta 
        h2_hi(j) = h2(j) + unc_h2(j)                      ! deuterium UPPER ENVELOPE as a function of eta 
        he3_lo(j) = he3(j) - unc_he3(j)                      ! he3 LOWER ENVELOPE as a function of eta 
        he3_hi(j) = he3(j) + unc_he3(j)                      ! he3 UPPER ENVELOPE as a function of eta 
        he4_lo(j) = he4(j) - unc_he4(j)                      ! he4 LOWER ENVELOPE as a function of eta 
        he4_hi(j) = he4(j) + unc_he4(j)                      ! he4 UPPER ENVELOPE as a function of eta 
        li7_lo(j) = li7(j) - unc_li7(j)                      ! li7 LOWER ENVELOPE as a function of eta 
        li7_hi(j) = li7(j) + unc_li7(j)                      ! li7 UPPER ENVELOPE as a function of eta 
      enddo
	  	  
c--------------------------------------------------------------------------------------------------------------------
c          read in primordial abundance observations
c          the central value is stored in array index 1, the LO in 2, the HI in 3
c          this allows for asymmetric uncertainties
c
         read(4,22)h2_o(1),h2_o(2),h2_o(3)
         read(4,22)he3_o(1),he3_o(2),he3_o(3)
         read(4,22)he4_o(1),he4_o(2),he4_o(3)
         read(4,22)li7_o(1),li7_o(2),li7_o(3)
 22      format(4(1pe13.5))
 
         write(9,23)
 23	     format(/'abundance observations'/'  central value      LO value     HI value ')
         write(9,24)h2_o(1),h2_o(2),h2_o(3)
         write(9,24)he3_o(1),he3_o(2),he3_o(3)
         write(9,24)he4_o(1),he4_o(2),he4_o(3)
         write(9,24)li7_o(1),li7_o(2),li7_o(3)
 24	     format(3(1pe13.5,1x))



c--------------------------------------------------------------------------------------------------------------------
c          do operations to look at the shapes of the curves
c              e.g., monotonic ... increasing, decreasing, or
c              if it is saddle shaped (decreasing then increasing)

c--------------------------------------------------------------------------------------------------------------------
c           initialize monotonicity results array

      do l=1,3
	    do k=1,5
          monotonic(l,k)=-2
        enddo
      enddo
	  
c--------------------------------------------------------------------------------------------------------------------
c            check if eta values are monotonic INcreasing
c

      j_max=9999                               ! choose an initial ridiculously large value
      eta_max = 1.0e-14                                ! choose an initial very small value
      do j=1,num_eta           
            if(eta(j).gt.eta_max) then         ! it better be!
                 eta_max=eta(j)
                 j_max=j
            else
                 write(9,30)
 30				 format(/'eta not monotonic increasing'/)
                 monotonic(1,1) = 3                     ! problem!!
            endif
      enddo

      if((j_max.ne.num_eta).or.(eta_max.ne.eta(num_eta))) then
	     write(9,32)
 32      format(/'problem with eta - not monotonic increasing'/)
      else
                 monotonic(1,1) = 1                ! it is monotonic increasing as expected
	             write(9,33)
 33              format(/'eta is monotonic increasing as it should be'/)
      endif


c--------------------------------------------------------------------------------------------------------------------
c        check if deuterium curves are monotonic DEcreasing
c       
c   valid values of (l,k) for monotonic(l,k)  
c                         ! (1,2): d central value   (2,2): d LO    (3,2): d HI envelope

      j_min=9999                                  ! choose an initial ridiculously large value
      h2_min = 5.0e-01                                ! choose an initial very large value
      h2_lo_min = 5.0e-01                                ! choose an initial very large value
      h2_hi_min = 5.0e-01                                ! choose an initial very large value
 
      do j=1,num_eta           
            if(h2(j).lt.h2_min) then             ! it better be!
                 h2_min=h2(j)
                 j_min=j
            else
                 write(9,34)
 34				 format(/'h2 not monotonic decreasing'/)
                 monotonic(1,2) = 3                     ! problem!!
            endif

      enddo

      if((j_min.ne.num_eta).or.(h2_min.ne.h2(num_eta))) then
                 write(9,36)
 36				 format(/'h2 not monotonic decreasing'/)
      else
                 monotonic(1,2) = -1               ! it is monotonic decreasing
      endif


      j_min=9999                                  ! choose an initial ridiculously large value
      do j=1,num_eta           
            if(h2_lo(j).lt.h2_lo_min) then             ! it better be!
                 h2_lo_min=h2_lo(j)
                 j_min=j
            else
                 write(9,38)
 38				 format(/'h2_lo not monotonic decreasing'/)
                 monotonic(2,2) = 3                     ! problem!!
            endif

      enddo

      if((j_min.ne.num_eta).or.(h2_lo_min.ne.h2_lo(num_eta))) then
                 write(9,40)
 40				 format(/'h2_lo not monotonic decreasing'/)
      else
                 monotonic(2,2) = -1               ! it is monotonic decreasing
      endif


      j_min=9999                                  ! choose an initial ridiculously large value
      do j=1,num_eta           
            if(h2_hi(j).lt.h2_hi_min) then             ! it better be!
                 h2_hi_min=h2_hi(j)
                 j_min=j
            else
                 write(9,42)
 42				 format(/'h2_hi not monotonic decreasing'/)
                 monotonic(3,2) = 3                     ! problem!!
            endif

      enddo

      if((j_min.ne.num_eta).or.(h2_hi_min.ne.h2_hi(num_eta))) then
                 write(9,44)
 44				 format(/'h2_hi not monotonic decreasing'/)
      else
                 monotonic(3,2) = -1               ! it is monotonic decreasing
      endif


c--------------------------------------------------------------------------------------------------------------------
c        check if he3 values are monotonic DEcreasing
c
c   valid values of (l,k) for monotonic(l,k)  
c                       ! (1,3): he3 central value   (2,3): he3 LO    (3,3): he3 HI envelope

      j_min=9999                                  ! choose an initial ridiculously large value
      he3_min = 5.0e-01                                ! choose an initial very large value
      he3_lo_min = 5.0e-01                                ! choose an initial very large value
      he3_hi_min = 5.0e-01                                ! choose an initial very large value
 
      do j=1,num_eta           
            if(he3(j).lt.he3_min) then             ! it better be!
                 he3_min=he3(j)
                 j_min=j
            else
                 write(9,46)
 46				 format(/'he3 not monotonic increasing'/)
                 monotonic(1,3) = 3                     ! problem!!
            endif

      enddo

      if((j_min.ne.num_eta).or.(he3_min.ne.he3(num_eta))) then
                 write(9,48)
 48				 format(/'he3 not monotonic decreasing'/)
      else
                 monotonic(1,3) = -1               ! it is monotonic decreasing
      endif



      j_min=9999                                  ! choose an initial ridiculously large value
      do j=1,num_eta           
            if(he3_lo(j).lt.he3_lo_min) then             ! it better be!
                 he3_lo_min=he3_lo(j)
                 j_min=j
            else
                 write(9,50)
 50				 format(/'he3_lo not monotonic decreasing'/)
                 monotonic(2,3) = 3                     ! problem!!
            endif

      enddo

      if((j_min.ne.num_eta).or.(he3_lo_min.ne.he3_lo(num_eta))) then
                 write(9,52)
 52				 format(/'he3_lo not monotonic increasing'/)
      else
                 monotonic(2,3) = -1               ! it is monotonic decreasing
      endif



      j_min=9999                                  ! choose an initial ridiculously large value
      do j=1,num_eta           
            if(he3_hi(j).lt.he3_hi_min) then             ! it better be!
                 he3_hi_min=he3_hi(j)
                 j_min=j
            else
                 write(9,54)
 54				 format(/'he3_hi not monotonic increasing'/)
                 monotonic(3,3) = 3                     ! problem!!
            endif

      enddo

      if((j_min.ne.num_eta).or.(he3_hi_min.ne.he3_hi(num_eta))) then
                 write(9,56)
 56				 format(/'he3_hi not monotonic increasing'/)
      else
                 monotonic(3,3) = -1               ! it is monotonic decreasing
      endif


c--------------------------------------------------------------------------------------------------------------------
c        check if he4 values are monotonic INcreasing
c
c   valid values of (l,k) for monotonic(l,k)  
c                       ! (1,4): he4 central value   (2,4): he4 LO    (3,4): he4 HI envelope

      j_max=9999                                  ! choose an initial ridiculously large value
      he4_max = 1.0e-05                           ! choose an initial very small value

      do j=1,num_eta           
            if(he4(j).gt.he4_max) then 
                 he4_max=he4(j)
                 j_max=j
            else
                 write(9,58)
 58				 format(/'he4 not monotonic increasing'/)
                 monotonic(1,4) = 3                ! problem!!
            endif
      enddo

      if((j_max.ne.num_eta).or.(he4_max.ne.he4(num_eta))) then
                 write(9,60)
 60				 format(/'he4 not monotonic increasing'/)
      else
                 monotonic(1,4) = 1               ! it is monotonic increasing
      endif




      j_max=9999                                  ! choose an initial ridiculously large value
      do j=1,num_eta           
            if(he4_lo(j).gt.he4_lo_max) then 
                 he4_lo_max=he4_lo(j)
                 j_max=j
            else
                 write(9,62)
 62				 format(/'he4_lo not monotonic increasing'/)
                 monotonic(2,4) = 3                ! problem!!
            endif
      enddo

      if((j_max.ne.num_eta).or.(he4_lo_max.ne.he4_lo(num_eta))) then
                 write(9,63)
 63				 format(/'he4_lo not monotonic increasing'/)
      else
                 monotonic(2,4) = 1               ! it is monotonic increasing
      endif



      j_max=9999                                  ! choose an initial ridiculously large value
      do j=1,num_eta           
            if(he4_hi(j).gt.he4_hi_max) then 
                 he4_hi_max=he4_hi(j)
                 j_max=j
            else
                 write(9,64)
 64				 format(/'he4_hi not monotonic increasing'/)
                 monotonic(3,4) = 3                ! problem!!
            endif
      enddo

      if((j_max.ne.num_eta).or.(he4_hi_max.ne.he4_hi(num_eta))) then
                 write(9,65)
 65				 format(/'he4_hi not monotonic increasing'/)
      else
                 monotonic(3,4) = 1               ! it is monotonic increasing
      endif

c--------------------------------------------------------------------------------------------------------------------
c            check shape of 7Li curve - assume it is saddle shaped
c
c          for saddle shaped functions
c                find the inflection point
c                 treat the side below the inflection point one way 
c                 (e.g. monotonic decreasing) and the data above the 
c              inflection point the other way (e.g. monotonic increasing).
c
c       we have to also be aware of the cases where the observations are all 
c              below the entire curve, or above the entire curve, or intersect 
c              just one portion of the curve (low eta or high eta); this will
c              be handled with the "bounds" analysis below
c
c
c   valid values of (l,k) for monotonic(l,k)  
c                        ! (1,5): li7 central value   (2,5): li7 LO    (3,5): li7 HI envelope


c     check li7 central curve

	  j_inf(1) = -1                               ! choose an initial ludicrous value
      j_min=9999                                  ! choose an initial ridiculously large value
      j_max=9999                                  ! choose an initial ridiculously large value
      li7_min = 1.0e-01                                ! choose an initial very large value
      li7_max = 1.0e-20                                ! choose an initial very small value

      do j=1,num_eta           
            if(li7(j).lt.li7_min) then 
                 li7_min=li7(j)
                 j_min=j
				 j_inf(1)=j
            else                                      !reached inflection point
                 write(9,66)j_inf(1),eta(j_inf(1))
 66				 format(/'7Li inflection point',i4,1pe13.6/)
                 goto 68    ! jump out of this loop; check other side of curve
            endif
      enddo

 68   li7_max=li7(j_inf(1))
 
      do j=j_inf(1)+1,num_eta                            ! check other side of curve
            if(li7(j).gt.li7_max) then 
                 li7_max=li7(j)
                 j_max=j
            else
                 write(9,70)
 70				 format(/'7Li is not saddle shaped'/)
                 monotonic(1,5) = 3                     ! problem!!
            endif
      enddo

      if((j_max.ne.num_eta).or.(li7_max.ne.li7(num_eta))) then
                 write(9,72)
 72				 format(/'7Li is not saddle shaped'/)
      else
                 monotonic(1,5) = 0                      ! it is saddle shaped
      endif


c     check li7 LO curve

	  j_inf(2) = -1                               ! choose an initial ludicrous value
      j_min=9999                                  ! choose an initial ridiculously large value
      j_max=9999                                  ! choose an initial ridiculously large value
      li7_lo_min = 1.0e-01                                ! choose an initial very large value
      li7_lo_max = 1.0e-20                                ! choose an initial very small value

      do j=1,num_eta           
            if(li7_lo(j).lt.li7_lo_min) then 
                 li7_lo_min=li7_lo(j)
                 j_min=j
				 j_inf(2)=j
            else                                      !reached inflection point
                 write(9,74)j_inf(2),eta(j_inf(2))
 74				 format(/'7Li LO inflection point',i4,1pe13.6/)
                 goto 76    ! jump out of this loop; check other side of curve
            endif
      enddo

 76    li7_lo_max=li7_lo(j_inf(2))

      do j=j_inf(2)+1,num_eta                            ! check other side of curve
            if(li7_lo(j).gt.li7_lo_max) then 
                 li7_lo_max=li7_lo(j)
                 j_max=j
            else
                 write(9,78)
 78				 format(/'7Li LO is not saddle shaped'/)
                 monotonic(2,5) = 3                     ! problem!!
            endif
      enddo

      if((j_max.ne.num_eta).or.(li7_lo_max.ne.li7_lo(num_eta))) then
                 write(9,80)
 80				 format(/'7Li LO is not saddle shaped'/)
      else
                 monotonic(2,5) = 0                      ! it is saddle shaped
      endif


c     check li7 HI curve

	  j_inf(3) = -1                               ! choose an initial ludicrous value
      j_min=9999                                  ! choose an initial ridiculously large value
      j_max=9999                                  ! choose an initial ridiculously large value
      li7_hi_min = 1.0e-01                                ! choose an initial very large value
      li7_hi_max = 1.0e-20                                ! choose an initial very small value

      do j=1,num_eta           
            if(li7_hi(j).lt.li7_hi_min) then 
                 li7_hi_min=li7_hi(j)
                 j_min=j
				 j_inf(3)=j
            else                                      !reached inflection point
                 write(9,82)j_inf(3),eta(j_inf(3))
 82				 format(/'7Li HI inflection point',i4,1pe13.6/)
                 goto 84    ! jump out of this loop; check other side of curve
            endif
      enddo

 84    li7_hi_max=li7_hi(j_inf(3))

      do j=j_inf(3)+1,num_eta                            ! check other side of curve
            if(li7_hi(j).gt.li7_hi_max) then 
                 li7_hi_max=li7_hi(j)
                 j_max=j
            else
                 write(9,86)
 86				 format(/'7Li HI is not saddle shaped'/)
                 monotonic(3,5) = 3                     ! problem!!
            endif
      enddo

      if((j_max.ne.num_eta).or.(li7_hi_max.ne.li7_hi(num_eta))) then
                 write(9,88)
 88				 format(/'7Li HI is not saddle shaped'/)
      else
                 monotonic(3,5) = 0                      ! it is saddle shaped
      endif


c--------------------------------------------------------------------------------------------------------------------
c     report on monotonicity
c
      if(
     |    (monotonic(1,1).eq.1).and.
     |    (monotonic(1,2).eq.-1).and.
     |    (monotonic(2,2).eq.-1).and.
     |    (monotonic(3,2).eq.-1).and.
     |    (monotonic(1,3).eq.-1).and.
     |    (monotonic(2,3).eq.-1).and.
     |    (monotonic(3,3).eq.-1).and.
     |    (monotonic(1,4).eq.1).and.
     |    (monotonic(1,4).eq.1).and.
     |    (monotonic(1,4).eq.1).and.
     |    (monotonic(1,5).eq.0).and. 
     |    (monotonic(2,5).eq.0).and. 
     |    (monotonic(3,5).eq.0) ) 
     | then
                 write(9,90)
 90				 format(/'all curves have expected shapes'/)
      else
                 write(9,92) monotonic(1,1),
     |      monotonic(1,2),monotonic(2,2),monotonic(3,2),
     |      monotonic(1,3),monotonic(2,3),monotonic(3,3),
     |      monotonic(1,4),monotonic(2,4),monotonic(3,4),
     |      monotonic(1,5),monotonic(2,5),monotonic(3,5)
 92				 format(/'at least one curve has a problem'/i4/4(3(i4,2x)/))
         goto 1000                                       ! end the program!
c                                                       ! later versions will
c                                                       ! suggest a way to 
c                                                       ! handle these cases
c
      endif

      write(9,94)
 94	  format(/'OK after monotonicity report'/)

c--------------------------------------------------------------------------------------------------------------------
c          should also find the min and max of each curve to see if 
c            the abundance constraint falls within these bounds
c            Note: this is easy to do for the monotonic cases!

                                          ! bounds(l,k,n) = 0 if curve l for isotope k is
                                             ! within the observational bounds in set n
                                          ! bounds(l,k,n) = 1 if curve l for isotope k is too high,
                                             ! above the observational bounds in set n
                                          ! bounds(l,k,n) = -1 if curve l for isotope k is too low
                                             ! below the observational bounds
                                          ! bounds(l,k,n) = -2 if curve l for isotope k is
                                             ! OK for values BELOW the 
                                             ! inflection point, but not OK 
                                             ! for values ABOVE
                                          ! bounds(l,k,n) = +2 if curve l for isotope k is
                                             ! OK for values ABOVE the 
                                             ! inflection point, but not OK 
                                             ! for values BELOW


c                                      valid (l,k) combinations:
c                                      ! (1,1): eta
c                                      ! (1,2): d central value   (2,2): d LO    (3,2): d HI envelope
c                                      ! (1,3): he3 central value   (2,3): he3 LO    (3,3): he3 HI envelope
c                                      ! (1,4): he4 central value   (2,4): he4 LO    (3,4): he4 HI envelope
c                                      ! (1,5): li7 central value   (2,5): li7 LO    (3,5): li7 HI envelope

c--------------------------------------------------------------------------------------------------------------------
c           initialize the "bounds" results array

      do l=1,3
	    do k=1,5
          do n=1,n_obs
            bounds(l,k,n)=-100
          enddo
		enddo
      enddo


c--------------------------------------------------------------------------------------------------------------------
c          check to see if deuterium values are within bounds
c
c                                      valid (l,k,n) combinations for bounds(l,k,n):
c                                      ! (1,2,1): d central curve, D OBSERV CENTRAL VALUE   
c                                      ! (1,2,2): d central curve, D OBSERV LO VALUE   
c                                      ! (1,2,3): d central curve, D OBSERV HI VALUE   
c                                      ! (2,2,1): d LO curve, D OBSERV CENTRAL VALUE   
c                                      ! (2,2,2): d LO curve, D OBSERV LO VALUE   
c                                      ! (2,2,3): d LO curve, D OBSERV HI VALUE   
c                                      ! (3,2,1): d HI curve, D OBSERV CENTRAL VALUE   
c                                      ! (3,2,2): d HI curve, D OBSERV LO VALUE   
c                                      ! (3,2,3): d HI curve, D OBSERV HI VALUE   

                                          ! bounds(l,k,n) = 1 if curve l for isotope k is TOO HIGH
                                             ! above the observational bounds in observation n

                                          ! bounds(l,k,n) = -1 if curve l for isotope k is TOO LOW
                                             ! below the observational bounds in observation n

          do j=1,num_eta
            write(9,96)j,h2(j)
 96	    format(/'h2 at ',i4,' value of eta =  ',1pe13.5)
          enddo



      do n=1,n_obs                                    ! loop over observations (MID, LO, HI)



            write(9,100)n,h2_o(n),h2(1),h2(num_eta),monotonic(1,2)
 100	    format(/'h2 observation',i4,'  =  ',1pe13.5,
     |             /'h2 CENTRAL values range from',1pe13.5,
     |               '--',1pe13.5,
     |             /'monotonicity =',i4)

         if(monotonic(1,2).eq.-1) then                  ! deuterium CENTRAL CURVE well behaved

 
            if((h2_o(n).lt.h2(1)).and.
     |         (h2_o(n).gt.h2(num_eta)) ) then

               bounds(1,2,n)=0                             ! bound is OK

            elseif(h2_o(n).gt.h2(1)) then

               bounds(1,2,n) = -1                          ! deuterium curve too LOW, BELOW observational bounds

            elseif(h2_o(n).lt.h2(num_eta)) then

               bounds(1,2,n) = 1                          ! deuterium curve too HIGH, above observational bounds

            else
               bounds(1,2,n)=-90                           ! test fails
               write(9,102)n
 102	       format(' problem with observational bound ',i4,
     |              ' on deuterium ')
            endif

         else        ! not monotonic as expected
           bounds(1,2,n)=-100

           write(9,104)n
 104	   format(' observational bound ',i4,
     |              ' on deuterium not tested - not monotonic')
         endif

      enddo                                           ! loop over observations (MID, LO, HI)
	  
c     repeat for deuterium LO curve	  

      do n=1,n_obs                                    ! loop over observations (MID, LO, HI)


            write(9,106)n,h2_o(n),h2_lo(1),h2_lo(num_eta),monotonic(2,2)
 106	    format(/'h2 observation: ',i4,' =  ',1pe13.5,
     |             /'h2 LO values range from',1pe15.5,'--',1pe13.5,
     |             /'monotonicity =',i4)

         if(monotonic(2,2).eq.-1) then                  ! deuterium LO CURVE well behaved

 
            if((h2_o(n).lt.h2_lo(1)).and.
     |         (h2_o(n).gt.h2_lo(num_eta)) ) then

               bounds(2,2,n)=0                             ! bound is OK

            elseif(h2_o(n).gt.h2_lo(1)) then

               bounds(2,2,n) = -1                          ! deuterium curve too LOW, BELOW observational bounds

            elseif(h2_o(n).lt.h2_lo(num_eta)) then

               bounds(2,2,n) = 1                          ! deuterium curve too HIGH, above observational bounds

            else
               bounds(2,2,n)=-90                           ! test fails
               write(9,108)n
 108	       format(' problem with observational bound ',i4,
     |              ' on deuterium LO curve ')
            endif

         else        ! not monotonic as expected
           bounds(2,2,n)=-100

           write(9,112)n
 112	   format(' observational bound ',i4,
     |              ' on deuterium LO curve not tested - not monotonic')
         endif

      enddo                                           ! loop over observations (MID, LO, HI)
	  
c     repeat for deuterium HI curve	  

      do n=1,n_obs                                    ! loop over observations (MID, LO, HI)


            write(9,114)n,h2_o(n),h2_hi(1),h2_hi(num_eta),monotonic(3,2)
 114	    format(/'h2 observation: ',i4,' =  ',1pe13.5,
     |             /'h2 HI values range from',1pe15.5,'--',1pe13.5,
     |             /'monotonicity =',i4)

         if(monotonic(3,2).eq.-1) then                  ! deuterium HI CURVE well behaved

 
            if((h2_o(n).lt.h2_hi(1)).and.
     |         (h2_o(n).gt.h2_hi(num_eta)) ) then

               bounds(3,2,n)=0                             ! bound is OK

            elseif(h2_o(n).gt.h2_hi(1)) then

               bounds(3,2,n) = -1                          ! deuterium curve too LOW, BELOW observational bounds

            elseif(h2_o(n).lt.h2_hi(num_eta)) then

               bounds(3,2,n) = 1                          ! deuterium curve too HIGH, above observational bounds

            else
               bounds(3,2,n)=-90                           ! test fails
               write(9,116)n
 116	       format(' problem with observational bound ',i4,
     |              ' on deuterium HI curve ')
            endif

         else        ! not monotonic as expected
           bounds(3,2,n)=-100

           write(9,118)n
 118	   format(' observational bound ',i4,
     |              ' on deuterium HI curve not tested - not monotonic')
         endif

      enddo                                           ! loop over observations (MID, LO, HI)
	  



c--------------------------------------------------------------------------------------------------------------------
c          check to see if 3He values are within bounds
c
c                                      valid (l,k,n) combinations for bounds(l,k,n):
c                                      ! (1,3,1): he3 central curve, he3 OBSERV CENTRAL VALUE   
c                                      ! (1,3,2): he3 central curve, he3 OBSERV LO VALUE   
c                                      ! (1,3,3): he3 central curve, he3 OBSERV HI VALUE   
c                                      ! (2,3,1): he3 LO curve, he3 OBSERV CENTRAL VALUE   
c                                      ! (2,3,2): he3 LO curve, he3 OBSERV LO VALUE   
c                                      ! (2,3,3): he3 LO curve, he3 OBSERV HI VALUE   
c                                      ! (3,3,1): he3 HI curve, he3 OBSERV CENTRAL VALUE   
c                                      ! (3,3,2): he3 HI curve, he3 OBSERV LO VALUE   
c                                      ! (3,3,3): he3 HI curve, he3 OBSERV HI VALUE   


      do n=1,n_obs                                    ! loop over observations (MID, LO, HI)



            write(9,120)n,he3_o(n),he3(1),he3(num_eta),monotonic(1,3)
 120	    format(/'he3 observation',i4,'  =  ',1pe13.5,
     |             /'he3 CENTRAL values range from',1pe13.5,'--',1pe13.5,
     |             /'monotonicity =',i4)

         if(monotonic(1,3).eq.-1) then                  ! he3 CENTRAL CURVE well behaved

 
            if((he3_o(n).lt.he3(1)).and.
     |         (he3_o(n).gt.he3(num_eta)) ) then

               bounds(1,3,n)=0                             ! bound is OK

            elseif(he3_o(n).gt.he3(1)) then

               bounds(1,3,n) = -1                          ! he3 too low

            elseif(he3_o(n).lt.he3(num_eta)) then

               bounds(1,3,n) = 1                          ! he3 too high

            else
               bounds(1,3,n)=-90                           ! test fails
               write(9,122)n
 122	       format(' problem with observational bound ',i4,
     |              ' on he3 ')
            endif

         else        ! not monotonic as expected
           bounds(1,3,n)=-100

           write(9,124)n
 124	   format(' observational bound ',i4,
     |              ' on he3 not tested - not monotonic')
         endif

      enddo                                           ! loop over observations (MID, LO, HI)
	  
c     repeat for he3 LO curve	  

      do n=1,n_obs                                    ! loop over observations (MID, LO, HI)


            write(9,126)n,he3_o(n),he3_lo(1),he3_lo(num_eta),
     |       monotonic(2,3)
 126	    format(/'he3 observation: ',i4,' =  ',1pe13.5,
     |             /'he3 LO values range from',1pe15.5,'--',1pe13.5,
     |             /'monotonicity =',i4)

         if(monotonic(2,3).eq.-1) then                  ! he3 LO CURVE well behaved

 
            if((he3_o(n).lt.he3_lo(1)).and.
     |         (he3_o(n).gt.he3_lo(num_eta)) ) then

               bounds(2,3,n)=0                             ! bound is OK

            elseif(he3_o(n).gt.he3_lo(1)) then

               bounds(2,3,n) = -1                          ! he3 curve too LOW, BELOW observational bounds

            elseif(he3_o(n).lt.he3_lo(num_eta)) then

               bounds(2,3,n) = 1                          ! he3 curve too HIGH, ABOVE observational bounds

            else
               bounds(2,3,n)=-90                           ! test fails
               write(9,128)n
 128	       format(' problem with observational bound ',i4,
     |              ' on he3 LO curve ')
            endif

         else        ! not monotonic as expected
           bounds(2,3,n)=-100

           write(9,132)n
 132	   format(' observational bound ',i4,
     |              ' on he3 LO curve not tested - not monotonic')
         endif

      enddo                                           ! loop over observations (MID, LO, HI)
	  
c     repeat for he3 HI curve	  

      do n=1,n_obs                                    ! loop over observations (MID, LO, HI)


            write(9,134)n,he3_o(n),he3_hi(1),he3_hi(num_eta),
     |        monotonic(3,3)
 134	    format(/'he3 observation: ',i4,' =  ',1pe13.5,
     |             /'he3 HI values range from',1pe15.5,' to ',1pe13.5,
     |             /'monotonicity =',i4)

         if(monotonic(3,3).eq.-1) then                  ! he3 HI CURVE well behaved

 
            if((he3_o(n).lt.he3_hi(1)).and.
     |         (he3_o(n).gt.he3_hi(num_eta)) ) then

               bounds(3,3,n)=0                             ! bound is OK

            elseif(he3_o(n).gt.he3_hi(1)) then

               bounds(3,3,n) = -1                          ! he3 curve too LOW, BELOW observational bounds

            elseif(he3_o(n).lt.he3_hi(num_eta)) then

               bounds(3,3,n) = 1                          ! he3 curve too HIGH, ABOVE observational bounds

            else
               bounds(3,3,n)=-90                           ! test fails
               write(9,136)n
 136	       format(' problem with observational bound ',i4,
     |              ' on he3 HI curve ')
            endif

         else        ! not monotonic as expected
           bounds(3,3,n)=-100

           write(9,138)n
 138	   format(' observational bound ',i4,
     |              ' on he3 HI curve not tested - not monotonic')
         endif

      enddo                                           ! loop over observations (MID, LO, HI)
	  

c--------------------------------------------------------------------------------------------------------------------
c          check to see if 4He values are within bounds
c
c
c                                      valid (l,k,n) combinations for bounds(l,k,n):
c                                      ! (1,4,1): he4 central curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (1,4,2): he4 central curve, he4 OBSERV LO VALUE   
c                                      ! (1,4,3): he4 central curve, he4 OBSERV HI VALUE   
c                                      ! (2,4,1): he4 LO curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (2,4,2): he4 LO curve, he4 OBSERV LO VALUE   
c                                      ! (2,4,3): he4 LO curve, he4 OBSERV HI VALUE   
c                                      ! (3,4,1): he4 HI curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (3,4,2): he4 HI curve, he4 OBSERV LO VALUE   
c                                      ! (3,4,3): he4 HI curve, he4 OBSERV HI VALUE   

                                          ! bounds(l,k,n) = 0 if curve l for isotope k is
                                             ! within the observational bounds in set n

                                          ! bounds(l,k,n) = 1 if curve l for isotope k is too high,
                                             ! above the observational bounds in set n

                                          ! bounds(l,k,n) = -1 if curve l for isotope k is too low
                                             ! below the observational bounds


      do n=1,n_obs                                    ! loop over observations (MID, LO, HI)



            write(9,140)n,he4_o(n),he4(1),he4(num_eta),monotonic(1,4)
 140	    format(/'he4 observation',i4,'  =  ',1pe13.5,
     |             /'he4 CENTRAL values range from',1pe13.5,'--',1pe13.5,
     |             /'monotonicity =',i4)

         if(monotonic(1,4).eq.1) then                  ! he4 CENTRAL CURVE well behaved

 
            if((he4_o(n).gt.he4(1)).and.
     |         (he4_o(n).lt.he4(num_eta)) ) then

               bounds(1,4,n)=0                             ! bound is OK

            elseif(he4_o(n).lt.he4(1)) then

               bounds(1,4,n) = 1                          ! he4 curve too high, above observational bounds

            elseif(he4_o(n).gt.he4(num_eta)) then

               bounds(1,4,n) = -1                          ! he4 curve too low, below observational bound

            else
               bounds(1,4,n)=-90                           ! test fails
               write(9,142)n
 142	       format(' problem with observational bound ',i4,
     |              ' on he4 ')
            endif

         else        ! not monotonic as expected
           bounds(1,4,n)=-100

           write(9,144)n
 144	   format(' observational bound ',i4,
     |              ' on he4 not tested - not monotonic')
         endif

      enddo                                           ! loop over observations (MID, LO, HI)
	  
c     repeat for he4 LO curve	  

      do n=1,n_obs                                    ! loop over observations (MID, LO, HI)


            write(9,146)n,he4_o(n),he4_lo(1),he4_lo(num_eta),
     |       monotonic(2,4)
 146	    format(/'he4 observation: ',i4,' =  ',1pe13.5,
     |             /'he4 LO values range from',1pe15.5,'--',1pe13.5,
     |             /'monotonicity =',i4)

         if(monotonic(2,4).eq.1) then                  ! he4 LO CURVE well behaved

 
            if((he4_o(n).gt.he4_lo(1)).and.
     |         (he4_o(n).lt.he4_lo(num_eta)) ) then

               bounds(2,4,n)=0                             ! bound is OK

            elseif(he4_o(n).lt.he4_lo(1)) then

               bounds(2,4,n) = 1                          ! he4 curve too high, above observational bounds

            elseif(he4_o(n).gt.he4_lo(num_eta)) then

               bounds(2,4,n) = -1                          ! he4 curve too low, below observational bound

            else
               bounds(2,4,n)=-90                           ! test fails
               write(9,148)n
 148	       format(' problem with observational bound ',i4,
     |              ' on he4 LO curve ')
            endif

         else        ! not monotonic as expected
           bounds(2,4,n)=-100

           write(9,152)n
 152	   format(' observational bound ',i4,
     |              ' on he4 LO curve not tested - not monotonic')
         endif

      enddo                                           ! loop over observations (MID, LO, HI)
	  
c     repeat for he4 HI curve	  

      do n=1,n_obs                                    ! loop over observations (MID, LO, HI)


            write(9,154)n,he4_o(n),he4_hi(1),he4_hi(num_eta),
     |         monotonic(3,4)
 154	    format(/'he4 observation: ',i4,' =  ',1pe13.5,
     |             /'he4 HI values range from',1pe15.5,'--',1pe13.5,
     |             /'monotonicity =',i4)

         if(monotonic(3,4).eq.1) then                  ! he4 HI CURVE well behaved

 
            if((he4_o(n).gt.he4_hi(1)).and.
     |         (he4_o(n).lt.he4_hi(num_eta)) ) then

               bounds(3,4,n)=0                             ! bound is OK

            elseif(he4_o(n).lt.he4_hi(1)) then

               bounds(3,4,n) = 1                          ! he4 curve too high, above observational bounds

            elseif(he4_o(n).gt.he4_hi(num_eta)) then

               bounds(3,4,n) = -1                          ! he4 curve too low, below observational bound

            else
               bounds(3,4,n)=-90                           ! test fails
               write(9,156)n
 156	       format(' problem with observational bound ',i4,
     |              ' on he4 HI curve ')
            endif

         else        ! not monotonic as expected
           bounds(3,4,n)=-100

           write(9,158)n
 158	   format(' observational bound ',i4,
     |              ' on he4 HI curve not tested - not monotonic')
         endif

      enddo                                           ! loop over observations (MID, LO, HI)
	


c--------------------------------------------------------------------------------------------------------------------
c          check to see if 7Li values are within bounds
c
c                                      valid (l,k,n) combinations for bounds(l,k,n):
c                                      ! (1,5,1): li7 central curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (1,5,2): li7 central curve, he4 OBSERV LO VALUE   
c                                      ! (1,5,3): li7 central curve, he4 OBSERV HI VALUE   
c                                      ! (2,5,1): li7 LO curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (2,5,2): li7 LO curve, he4 OBSERV LO VALUE   
c                                      ! (2,5,3): li7 LO curve, he4 OBSERV HI VALUE   
c                                      ! (3,5,1): li7 HI curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (3,5,2): li7 HI curve, he4 OBSERV LO VALUE   
c                                      ! (3,5,3): li7 HI curve, he4 OBSERV HI VALUE   
c
          ! bounds(l,k,n) = 0 if curve l for isotope k is
                                             ! within the observational bounds in observation n

                                          ! bounds(l,k,n) = 1 if curve l for isotope k is TOO HIGH
                                             ! above the observational bounds in observation n

                                          ! bounds(l,k,n) = -1 if curve l for isotope k is TOO LOW
                                             ! below the observational bounds in observation n

                                          ! bounds(l,k,n) = -2 if curve l for isotope k is
                                             ! OK for values BELOW the 
                                             ! inflection point, but not OK 
                                             ! for values ABOVE, in observation n

                                          ! bounds(l,k,n) = +2 if curve l for isotope k is
                                             ! OK for values ABOVE the 
                                             ! inflection point, but not OK 
                                             ! for values BELOW, in observation n


c     check for the CENTRAL or MID li7 curve

      do n=1,n_obs                                    ! loop over observations MID, LO, HI

         if(monotonic(1,5).eq.0) then                  ! Li7 MID CURVE well behaved

            if((li7_o(n).lt.li7(1)).and.
     |         (li7_o(n).gt.li7(j_inf(1))).and.
     |         (li7_o(n).lt.li7(num_eta)) ) then

               bounds(1,5,n)=0                             ! bound is OK


             elseif((li7_o(n).lt.li7(1)).and.
     |              (li7_o(n).gt.li7(j_inf(1))).and.
     |              (li7_o(n).gt.li7(num_eta)) ) then


               bounds(1,5,n)=-2                       ! bound is OK for low eta
                                                    ! 7Li curve too low at high eta


             elseif((li7_o(n).gt.li7(1)).and.
     |              (li7_o(n).gt.li7(j_inf(1))).and.
     |              (li7_o(n).lt.li7(num_eta)) ) then


               bounds(1,5,n)=2                       ! bound is OK for high eta
                                                   ! 7Li curve too high at low eta


             elseif((li7_o(n).gt.li7(1)).and.
     |              (li7_o(n).gt.li7(j_inf(1))).and.
     |              (li7_o(n).gt.li7(num_eta)) ) then


               bounds(1,5,n)=-1                      ! 7Li curve too low



             elseif((li7_o(n).lt.li7(1)).and.
     |              (li7_o(n).lt.li7(j_inf(1))).and.
     |              (li7_o(n).lt.li7(num_eta)) ) then


               bounds(1,5,n)=1                      ! 7Li curve too high


             else
               bounds(1,5,n)=-90                           ! test fails
               write(9,160)n
 160	       format(' problem with observational bound ',i4,
     |              ' on 7Li CENTRAL CURVE ')
            endif  ! different cases

         else  ! not well behaved case
           bounds(1,5,n)=-100

           write(9,162)n
 162	   format(' observational bound ',i4,
     |              ' on 7Li CENTRAL CURVE not tested-not well behaved')
         endif ! well behaved status

      enddo                                           ! loop over observations MID, LO, HI


c     check for the LO li7 curve

      do n=1,n_obs                                    ! loop over observations MID, LO, HI

         if(monotonic(2,5).eq.0) then                  ! Li7 LO CURVE well behaved

            if((li7_o(n).lt.li7_lo(1)).and.
     |         (li7_o(n).gt.li7_lo(j_inf(2))).and.
     |         (li7_o(n).lt.li7_lo(num_eta)) ) then

               bounds(2,5,n)=0                             ! bound is OK


             elseif((li7_o(n).lt.li7_lo(1)).and.
     |              (li7_o(n).gt.li7_lo(j_inf(2))).and.
     |              (li7_o(n).gt.li7_lo(num_eta)) ) then


               bounds(2,5,n)=-2                       ! bound is OK for low eta
                                                    ! 7Li curve too low at high eta


             elseif((li7_o(n).gt.li7_lo(1)).and.
     |              (li7_o(n).gt.li7_lo(j_inf(2))).and.
     |              (li7_o(n).lt.li7_lo(num_eta)) ) then


               bounds(2,5,n)=2                       ! bound is OK for high eta
                                                   ! 7Li curve too high at low eta


             elseif((li7_o(n).gt.li7_lo(1)).and.
     |              (li7_o(n).gt.li7_lo(j_inf(2))).and.
     |              (li7_o(n).gt.li7_lo(num_eta)) ) then


               bounds(2,5,n)=-1                      ! 7Li curve too low



             elseif((li7_o(n).lt.li7_lo(1)).and.
     |              (li7_o(n).lt.li7_lo(j_inf(2))).and.
     |              (li7_o(n).lt.li7_lo(num_eta)) ) then


               bounds(2,5,n)=1                      ! 7Li curve too high


             else
               bounds(2,5,n)=-90                           ! test fails
               write(9,164)n
 164	       format(' problem with observational bound ',i4,
     |              ' on 7Li LO CURVE ')
            endif  ! different cases

         else  ! not well behaved case
           bounds(2,5,n)=-100

           write(9,166)n
 166	   format(' observational bound ',i4,
     |              ' on 7Li LO CURVE not tested - not well behaved')

         endif ! well behaved status

      enddo                                           ! loop over observations MID, LO, HI


c     check for the HI li7 curve

      do n=1,n_obs                                    ! loop over observations MID, LO, HI

         if(monotonic(3,5).eq.0) then                  ! Li7 MID CURVE well behaved

            if((li7_o(n).lt.li7_hi(1)).and.
     |         (li7_o(n).gt.li7_hi(j_inf(3))).and.
     |         (li7_o(n).lt.li7_hi(num_eta)) ) then

               bounds(3,5,n)=0                             ! bound is OK


             elseif((li7_o(n).lt.li7_hi(1)).and.
     |              (li7_o(n).gt.li7_hi(j_inf(3))).and.
     |              (li7_o(n).gt.li7_hi(num_eta)) ) then


               bounds(3,5,n)=-2                       ! bound is OK for low eta
                                                    ! 7Li curve too low at high eta


             elseif((li7_o(n).gt.li7_hi(1)).and.
     |              (li7_o(n).gt.li7_hi(j_inf(3))).and.
     |              (li7_o(n).lt.li7_hi(num_eta)) ) then


               bounds(3,5,n)=2                       ! bound is OK for high eta
                                                   ! 7Li curve too high at low eta


             elseif((li7_o(n).gt.li7_hi(1)).and.
     |              (li7_o(n).gt.li7_hi(j_inf(3))).and.
     |              (li7_o(n).gt.li7_hi(num_eta)) ) then


               bounds(3,5,n)=-1                      ! 7Li curve too low



             elseif((li7_o(n).lt.li7_hi(1)).and.
     |              (li7_o(n).lt.li7_hi(j_inf(3))).and.
     |              (li7_o(n).lt.li7_hi(num_eta)) ) then


               bounds(3,5,n)=1                      ! 7Li curve too high


             else
               bounds(3,5,n)=-90                           ! test fails
               write(9,168)n
 168	       format(' problem with observational bound ',i4,
     |              ' on 7Li hi CURVE ')
            endif  ! different cases

         else  ! not well behaved case
           bounds(3,5,n)=-100

           write(9,170)n
 170	   format(' observational bound ',i4,
     |              ' on 7Li hi CURVE not tested - not well behaved')

         endif ! well behaved status

      enddo                                           ! loop over observations MID, LO, HI
	  

c--------------------------------------------------------------------------------------------------------------------
c     handle each case - decreasing, increasing, saddle shaped - differently
c
c          for each isotope, find the TWO ORDERED PAIRS 
c                [eta_j, abund_j] and [eta_(j+1), abund_(j+1)] that have 
c                an abundance prediction that BRACKETs 
c                the abundance observation ... 
c                abund_j < abund_observation < abund_(j+1)
c
c                set j_bracket equal to the lower j value


c--------------------------------------------------------------------------------------------------------------------
c          initialize bracketing array
c

      do l=1,3
	    do k=1,6
          do n=1,n_obs
            j_bracket(l,k,n)=9999
          enddo
		enddo
      enddo


c--------------------------------------------------------------------------------------------------------------------
c          find bracketing eta values for h2 for both sets of observations
c
c                                      valid (l,k,n) combinations for bounds(l,k,n):
c                                      ! (1,2,1): d central curve, D OBSERV CENTRAL VALUE   
c                                      ! (1,2,2): d central curve, D OBSERV LO VALUE   
c                                      ! (1,2,3): d central curve, D OBSERV HI VALUE   
c                                      ! (2,2,1): d LO curve, D OBSERV CENTRAL VALUE   
c                                      ! (2,2,2): d LO curve, D OBSERV LO VALUE   
c                                      ! (2,2,3): d LO curve, D OBSERV HI VALUE   
c                                      ! (3,2,1): d HI curve, D OBSERV CENTRAL VALUE   
c                                      ! (3,2,2): d HI curve, D OBSERV LO VALUE   
c                                      ! (3,2,3): d HI curve, D OBSERV HI VALUE   

                                          ! j_bracket(l,k,n) = the index of the lower of the two eta values
										  !   in which curve l of isotope k brackets observation n
										  

      write(9,180)j_bracket(1,2,1),bounds(1,2,1),
     |         bounds(2,2,1)
 180	format(/' OK at start of looping',i4,' bounds ',i4,i4)

c
c          only go through this loop if the observational limit 
c            does provide a bound ... bound(l,k,n) = 0   {k=2 here}
c

c     find brackets for the central curve  l = 1

      do n=1,n_obs                         ! loop over observations MID, LO, and HI values

      if(bounds(1,2,n).ne.0) then                       ! bound is NOT OK
        write(9,182)n
 182	format(/'bound ',i4,' from h2 will not work on CENTRAL curve')

      elseif(bounds(1,2,n).eq.0) then                   ! bound is OK, curve is well behaved


         do j=1,num_eta-1                               ! loop over eta values

            if((h2(j).gt.h2_o(n)).and.(h2(j+1).lt.h2_o(n))) then 
               j_bracket(1,2,n)=j
               write(9,184)n,j,h2(j),h2(j+1),h2_o(n)
 184	       format('brackets --  n = ',i4,' j = ',i4,3(1pe13.6))
               goto 190                                ! jump out of the loop

            else                 ! if there is no bracketing, keep j_bracket value at 9999

              write(9,186)j
 186	      format('no bracket   j = ',i4)
              write(9,188)j,h2(j),h2(j+1),h2_o(n)
 188	      format(i4,3(1pe13.6,1x))
            endif
 
         enddo                                        ! loop over eta values

 190     continue   ! after bracketing was successful

         write(9,192)n,eta(j_bracket(1,2,n)),h2(j_bracket(1,2,n)),
     |          eta(j_bracket(1,2,n)+1),h2(j_bracket(1,2,n)+1)
 192	 format('h2 observation set ',i4/
     |          'bracketing values CENTRAL curve (eta,h2): (',1pe13.6,
     |          ', ',1pe13.6,') and (',1pe13.6,', ',1pe13.6,')'/)

      
       else
       endif                                         ! loop on bounds

       enddo                            ! loop over observations MID, LO, and HI



c     find brackets for the LO curve  l = 2

      do n=1,n_obs                         ! loop over observations MID, LO, and HI values

      if(bounds(2,2,n).ne.0) then                       ! bound is NOT OK
        write(9,194)n
 194	format(/'bound ',i4,' from h2 will not work on LO curve')

      elseif(bounds(2,2,n).eq.0) then                   ! bound is OK, curve is well behaved


         do j=1,num_eta-1                               ! loop over eta values

            if((h2_lo(j).gt.h2_o(n)).and.(h2_lo(j+1).lt.h2_o(n))) then 
               j_bracket(2,2,n)=j
               write(9,196)n,j,h2_lo(j),h2_lo(j+1),h2_o(n)
 196	       format('brackets -- n = ',i4,' j = ',i4,3(1pe13.6))
               goto 202                                ! jump out of the loop

            else                 ! if there is no bracketing, keep j_bracket value at 9999

              write(9,198)j
 198	      format('no bracket   j = ',i4)
              write(9,200)j,h2_lo(j),h2_lo(j+1),h2_o(n)
 200	      format(i4,3(1pe13.6,1x))
            endif
 
         enddo                                        ! loop over eta values

 202     continue   ! after bracketing was successful

         write(9,204)n,eta(j_bracket(2,2,n)),h2_lo(j_bracket(2,2,n)),
     |          eta(j_bracket(2,2,n)+1),h2_lo(j_bracket(2,2,n)+1)
 204	 format('h2 observation set ',i4/
     |          'bracketing values LO curve (eta,h2): (',1pe13.6,
     |          ', ',1pe13.6,') and (',1pe13.6,', ',1pe13.6,')'/)

      
       else
       endif                                         ! loop on bounds

       enddo                            ! loop over observations MID, LO, and HI



c     find brackets for the HI curve  l = 3

      do n=1,n_obs                         ! loop over observations MID, LO, and HI values

      if(bounds(3,2,n).ne.0) then                       ! bound is NOT OK
        write(9,210)n
 210	format(/'bound ',i4,' from h2 will not work on HI curve')

      elseif(bounds(3,2,n).eq.0) then                   ! bound is OK, curve is well behaved


         do j=1,num_eta-1                               ! loop over eta values

            if((h2_hi(j).gt.h2_o(n)).and.(h2_hi(j+1).lt.h2_o(n))) then 
               j_bracket(3,2,n)=j
               write(9,212)n,j,h2_hi(j),h2_hi(j+1),h2_o(n)
 212	       format('brackets -- n = ',i4,' j = ',i4,3(1pe13.6))
               goto 220                                ! jump out of the loop

            else                 ! if there is no bracketing, keep j_bracket value at 9999

              write(9,214)j
 214	      format('no bracket   j = ',i4)
              write(9,216)j,h2_hi(j),h2_hi(j+1),h2_o(n)
 216	      format(i4,3(1pe13.6,1x))
            endif
 
         enddo                                        ! loop over eta values

 220     continue   ! after bracketing was successful

         write(9,222)n,eta(j_bracket(3,2,n)),h2_hi(j_bracket(3,2,n)),
     |          eta(j_bracket(3,2,n)+1),h2_hi(j_bracket(3,2,n)+1)
 222	 format('h2 observation set ',i4/
     |          'bracketing values HI curve (eta,h2): (',1pe13.6,', ',
     |           1pe13.6,') and (',1pe13.6,', ',1pe13.6,')'/)

      
       else
       endif                                         ! loop on bounds

       enddo                            ! loop over observations MID, LO, and HI



c--------------------------------------------------------------------------------------------------------------------
c          find bracketing eta values for he3 for both sets of observations
c
c                                      valid (l,k,n) combinations for bounds(l,k,n):
c                                      ! (1,3,1): he3 central curve, he3 OBSERV CENTRAL VALUE   
c                                      ! (1,3,2): he3 central curve, he3 OBSERV LO VALUE   
c                                      ! (1,3,3): he3 central curve, he3 OBSERV HI VALUE   
c                                      ! (2,3,1): he3 LO curve, he3 OBSERV CENTRAL VALUE   
c                                      ! (2,3,2): he3 LO curve, he3 OBSERV LO VALUE   
c                                      ! (2,3,3): he3 LO curve, he3 OBSERV HI VALUE   
c                                      ! (3,3,1): he3 HI curve, he3 OBSERV CENTRAL VALUE   
c                                      ! (3,3,2): he3 HI curve, he3 OBSERV LO VALUE   
c                                      ! (3,3,3): he3 HI curve, he3 OBSERV HI VALUE   

                                          ! j_bracket(l,k,n) = the index of the lower of the two eta values
										  !   in which curve l of isotope k brackets observation n
										  
c
c          only go through this loop if the observational limit 
c            does provide a bound ... bound(l,k,n) = 0   {k=3 here}
c

c     find brackets for the central curve  l = 1

      do n=1,n_obs                         ! loop over observations MID, LO, and HI values

      if(bounds(1,3,n).ne.0) then                       ! bound is NOT OK
        write(9,282)n
 282	format(/'bound ',i4,' from he3 will not work on CENTRAL curve')

      elseif(bounds(1,3,n).eq.0) then                   ! bound is OK, curve is well behaved


         do j=1,num_eta-1                               ! loop over eta values

            if((he3(j).gt.he3_o(n)).and.(he3(j+1).lt.he3_o(n))) then 
               j_bracket(1,3,n)=j
               write(9,284)n,j,he3(j),he3(j+1),he3_o(n)
 284	       format('brackets --  n = ',i4,' j = ',i4,3(1pe13.6))
               goto 290                                ! jump out of the loop

            else                 ! if there is no bracketing, keep j_bracket value at 9999

              write(9,286)j
 286	      format('no bracket   j = ',i4)
              write(9,288)j,he3(j),he3(j+1),he3_o(n)
 288	      format(i4,3(1pe13.6,1x))
            endif
 
         enddo                                        ! loop over eta values

 290     continue   ! after bracketing was successful

         write(9,292)n,eta(j_bracket(1,3,n)),he3(j_bracket(1,3,n)),
     |          eta(j_bracket(1,3,n)+1),he3(j_bracket(1,3,n)+1)
 292	 format('he3 observation set ',i4/
     |          'bracketing values Central curve (eta,he3): (',1pe13.6,
     |          ', ',1pe13.6,') and (',1pe13.6,', ',1pe13.6,')'/)

      
       else
       endif                                         ! loop on bounds

       enddo                            ! loop over observations MID, LO, and HI



c     find brackets for the LO curve  l = 2

      do n=1,n_obs                         ! loop over observations MID, LO, and HI values

      if(bounds(2,3,n).ne.0) then                       ! bound is NOT OK
        write(9,294)n
 294	format(/'bound ',i4,' from he3 will not work on LO curve')

      elseif(bounds(2,3,n).eq.0) then                   ! bound is OK, curve is well behaved


         do j=1,num_eta-1                               ! loop over eta values
            if(
     |          (he3_lo(j).gt.he3_o(n))
     |          .and.
     |          (he3_lo(j+1).lt.he3_o(n))
     |         ) then

               j_bracket(2,3,n)=j
               write(9,296)n,j,he3_lo(j),he3_lo(j+1),he3_o(n)
 296	       format('brackets --  n = ',i4,' j = ',i4,3(1pe13.6))
               goto 302                                ! jump out of the loop

            else                 ! if there is no bracketing, keep j_bracket value at 9999

              write(9,298)j
 298	      format('no bracket   j = ',i4)
              write(9,300)j,he3_lo(j),he3_lo(j+1),he3_o(n)
 300	      format(i4,3(1pe13.6,1x))
            endif
 
         enddo                                        ! loop over eta values

 302     continue   ! after bracketing was successful

         write(9,304)n,eta(j_bracket(2,3,n)),he3_lo(j_bracket(2,3,n)),
     |          eta(j_bracket(2,3,n)+1),he3_lo(j_bracket(2,3,n)+1)
 304	 format('he3 observation set ',i4/
     |          'bracketing values LO curve (eta,he3): (',1pe13.6,
     |          ', ',1pe13.6,') and (',1pe13.6,', ',1pe13.6,')'/)

      
       else
       endif                                         ! loop on bounds

       enddo                            ! loop over observations MID, LO, and HI



c     find brackets for the HI curve  l = 3

      do n=1,n_obs                         ! loop over observations MID, LO, and HI values

      if(bounds(3,3,n).ne.0) then                       ! bound is NOT OK
        write(9,310)n
 310	format(/'bound ',i4,' from he3 will not work on HI curve')

      elseif(bounds(3,3,n).eq.0) then                   ! bound is OK, curve is well behaved


         do j=1,num_eta-1                               ! loop over eta values

            if(
     |          (he3_hi(j).gt.he3_o(n))
     |          .and.
     |          (he3_hi(j+1).lt.he3_o(n))
     |         ) then

               j_bracket(3,3,n)=j
               write(9,312)n,j,he3_hi(j),he3_hi(j+1),he3_o(n)
 312	       format('brackets --  n = ',i4,' j = ',i4,3(1pe13.6))
               goto 320                                ! jump out of the loop

            else                 ! if there is no bracketing, keep j_bracket value at 9999

              write(9,314)j
 314	      format('no bracket   j = ',i4)
              write(9,316)j,he3_hi(j),he3_hi(j+1),he3_o(n)
 316	      format(i4,3(1pe13.6,1x))
            endif
 
         enddo                                        ! loop over eta values

 320     continue   ! after bracketing was successful

         write(9,322)n,eta(j_bracket(3,3,n)),he3_hi(j_bracket(3,3,n)),
     |          eta(j_bracket(3,3,n)+1),he3_hi(j_bracket(3,3,n)+1)
 322	 format('he3 observation set ',i4/
     |          'bracketing values HI curve (eta,he3): (',1pe13.6,
     |          ', ',1pe13.6,') and (',1pe13.6,', ',1pe13.6,')'/)

      
       else
       endif                                         ! loop on bounds

       enddo                            ! loop over observations MID, LO, and HI


c--------------------------------------------------------------------------------------------------------------------
c          find bracketing eta values for he4 for both sets of observations
c
c
c                                      valid (l,k,n) combinations for bounds(l,k,n):
c                                      ! (1,4,1): he4 central curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (1,4,2): he4 central curve, he4 OBSERV LO VALUE   
c                                      ! (1,4,3): he4 central curve, he4 OBSERV HI VALUE   
c                                      ! (2,4,1): he4 LO curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (2,4,2): he4 LO curve, he4 OBSERV LO VALUE   
c                                      ! (2,4,3): he4 LO curve, he4 OBSERV HI VALUE   
c                                      ! (3,4,1): he4 HI curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (3,4,2): he4 HI curve, he4 OBSERV LO VALUE   
c                                      ! (3,4,3): he4 HI curve, he4 OBSERV HI VALUE   

                                          ! j_bracket(l,k,n) = the index of the lower of the two eta values
										  !   in which curve l of isotope k brackets observation n										  
c
c          only go through this loop if the observational limit 
c            does provide a bound ... bound(l,k,n) = 0   {k=4 here}
c


c     find brackets for the central curve  l = 1

      do n=1,n_obs                         ! loop over observations MID, LO, and HI values

      if(bounds(1,4,n).ne.0) then                       ! bound is NOT OK
        write(9,382)n
 382	format(/'bound ',i4,' from he4 will not work on CENTRAL curve')

      elseif(bounds(1,4,n).eq.0) then                   ! bound is OK, curve is well behaved


         do j=1,num_eta-1                               ! loop over eta values

            if((he4(j).lt.he4_o(n)).and.(he4(j+1).gt.he4_o(n))) then 
               j_bracket(1,4,n)=j
               write(9,384)n,j,he4(j),he4(j+1),he4_o(n)
 384	       format('brackets ---  n = ',i4,' j = ',i4,3(1pe13.6))
               goto 390                                ! jump out of the loop

            else                 ! if there is no bracketing, keep j_bracket value at 9999

              write(9,386)j
 386	      format('no bracket   j = ',i4)
              write(9,388)j,he4(j),he4(j+1),he4_o(n)
 388	      format(i4,3(1pe13.6,1x))
            endif
 
         enddo                                        ! loop over eta values

 390     continue   ! after bracketing was successful

         write(9,392)n,eta(j_bracket(1,4,n)),he4(j_bracket(1,4,n)),
     |          eta(j_bracket(1,4,n)+1),he4(j_bracket(1,4,n)+1)
 392	 format('he4 observation set ',i4/
     |          'bracketing values Central curve (eta,he4): (',1pe13.6,
     |          ', ',1pe13.6,') and (',1pe13.6,', ',1pe13.6,')'/)

      
       else
       endif                                         ! loop on bounds

       enddo                            ! loop over observations MID, LO, and HI



c     find brackets for the LO curve  l = 2

      do n=1,n_obs                         ! loop over observations MID, LO, and HI values

      if(bounds(2,4,n).ne.0) then                       ! bound is NOT OK
        write(9,394)n
 394	format(/'bound ',i4,' from he4 will not work on LO curve')

      elseif(bounds(2,4,n).eq.0) then                   ! bound is OK, curve is well behaved


         do j=1,num_eta-1                               ! loop over eta values


            if(
     |          (he4_lo(j).lt.he4_o(n))
     |          .and.
     |          (he4_lo(j+1).gt.he4_o(n))
     |         ) then
               j_bracket(2,4,n)=j
               write(9,396)n,j,he4_lo(j),he4_lo(j+1),he4_o(n)
 396	       format('brackets --  n = ',i4,' j = ',i4,3(1pe13.6))
               goto 402                                ! jump out of the loop

            else                 ! if there is no bracketing, keep j_bracket value at 9999

              write(9,398)j
 398	      format('no bracket   j = ',i4)
              write(9,400)j,he4_lo(j),he4_lo(j+1),he4_o(n)
 400	      format(i4,3(1pe13.6,1x))
            endif
 
         enddo                                        ! loop over eta values

 402     continue   ! after bracketing was successful

         write(9,404)n,eta(j_bracket(2,4,n)),he4_lo(j_bracket(2,4,n)),
     |          eta(j_bracket(2,4,n)+1),he4_lo(j_bracket(2,4,n)+1)
 404	 format('he4 observation set ',i4/
     |          'bracketing values LO curve (eta,he4): (',1pe13.6,
     |          ', ',1pe13.6,') and (',1pe13.6,', ',1pe13.6,')'/)

      
       else
       endif                                         ! loop on bounds

       enddo                            ! loop over observations MID, LO, and HI



c     find brackets for the HI curve  l = 3

      do n=1,n_obs                         ! loop over observations MID, LO, and HI values

      if(bounds(3,4,n).ne.0) then                       ! bound is NOT OK
        write(9,410)n
 410	format(/'bound ',i4,' from he4 will not work on HI curve')

      elseif(bounds(3,4,n).eq.0) then                   ! bound is OK, curve is well behaved


         do j=1,num_eta-1                               ! loop over eta values

            if(
     |          (he4_hi(j).lt.he4_o(n))
     |          .and.
     |          (he4_hi(j+1).gt.he4_o(n))
     |         ) then

               j_bracket(3,4,n)=j
               write(9,412)n,j,he4_hi(j),he4_hi(j+1),he4_o(n)
 412           format('brackets -- n=',i4,
     |         'j=',i4,3(1pe13.6)) 
               goto 420                                ! jump out of the loop

            else                 ! if there is no bracketing, keep j_bracket value at 9999

              write(9,414)j
 414	      format('no bracket   j = ',i4)
              write(9,416)j,he4_hi(j),he4_hi(j+1),he4_o(n)
 416	      format(i4,3(1pe13.6,1x))
            endif
 
         enddo                                        ! loop over eta values

 420     continue   ! after bracketing was successful

         write(9,422)n,eta(j_bracket(3,4,n)),he4_hi(j_bracket(3,4,n)),
     |          eta(j_bracket(3,4,n)+1),he4_hi(j_bracket(3,4,n)+1)
 422	 format('he4 observation set ',i4/
     |          'bracketing values HI curve (eta,he4): (',1pe13.6,
     |          ', ',1pe13.6,') and (',1pe13.6,', ',1pe13.6,')'/)

      
       else
       endif                                         ! loop on bounds 
       enddo                            ! loop over observations MID, LO, and HI




c--------------------------------------------------------------------------------------------------------------------
c          find bracketing eta values for li7 for both sets of observations
c
c
c                                      valid (l,k,n) combinations for bounds(l,k,n):
c                                      ! (1,5,1): li7 central curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (1,5,2): li7 central curve, he4 OBSERV LO VALUE   
c                                      ! (1,5,3): li7 central curve, he4 OBSERV HI VALUE   
c                                      ! (2,5,1): li7 LO curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (2,5,2): li7 LO curve, he4 OBSERV LO VALUE   
c                                      ! (2,5,3): li7 LO curve, he4 OBSERV HI VALUE   
c                                      ! (3,5,1): li7 HI curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (3,5,2): li7 HI curve, he4 OBSERV LO VALUE   
c                                      ! (3,5,3): li7 HI curve, he4 OBSERV HI VALUE   
c
c          find bracketing eta values for Li7 for both sets of observations
c          this has to be handled a little differently with the different 
c          cases described by the "bounds" array 
c
c     bracketing values for the CENTRAL li7 curve

      do n=1,n_obs                                    ! loop over observations MID, LO, HI

              write(9,478)
 478	      format('OK at line 478')
      
      if((bounds(1,5,n).eq.0).or.(bounds(1,5,n).eq.-2)) then  
                                                   ! bound is OK at low eta
              write(9,480)
 480	      format('OK at line 480')

         do j=1,j_inf(1)-1                               ! loop over eta values

            if(
     |          (li7(j).gt.li7_o(n))
     |          .and.
     |          (li7(j+1).lt.li7_o(n))
     |         ) then

               j_bracket(1,5,n)=j
               write(9,512)j,li7(j),li7(j+1),li7_o(n)
 512	       format('brackets on low eta --   j = ',i4,3(1pe13.5))
               goto 520                                ! jump out of the loop
            else
              write(9,514)j
 514	      format('no bracket on low eta side -- j = ',i4)
              write(9,516)j,li7(j),li7(j+1),li7_o(n)
 516	      format(i4,3(1pe13.5))
            endif
 
         enddo                                        ! loop over eta values

 520      continue
              
              write(9,521)
 521	      format('OK after line 520')
			  

         write(9,522)n,eta(j_bracket(1,5,n)),li7(j_bracket(1,5,n)),
     |          eta(j_bracket(1,5,n)+1),li7(j_bracket(1,5,n)+1)
 522      format('li7 CENTRAL CURVE, observation set ',i4/
     |          'low eta bracketing values (eta,li7): (',
     |           1pe13.5,', ',1pe13.5,
     |          ') and (',1pe13.5,', ',1pe13.5,')'/)

      else                       ! bound is not OK

        write(9,524)n,bounds(1,5,n)
 524	format(/'bound ',i4,' on 7Li will not work AT LOW ETA;bound =',i4)

      endif       ! first condition on bounds

      if((bounds(1,5,n).eq.0).or.(bounds(1,5,n).eq.2)) then  
                                                   ! bound is OK at high eta

         do j=j_inf(1),num_eta-1                               ! loop over eta values

            if(
     |          (li7(j).lt.li7_o(n))
     |          .and.
     |          (li7(j+1).gt.li7_o(n))
     |         ) then
	 
               j_bracket(1,6,n)=j
               write(9,532)j,li7(j),li7(j+1),li7_o(n)
 532	       format('brackets on high eta --   j = ',i4,3(1pe13.5))
               goto 540                                ! jump out of the loop
            else
              write(9,534)j
 534	      format('no bracket on high eta side -- j = ',i4)
              write(9,536)j,li7(j),li7(j+1),li7_o(n)
 536	      format(i4,3(1pe13.6,1x))
            endif
 
         enddo                                        ! loop over eta values

 540      continue

         write(9,542)n,eta(j_bracket(1,6,n)),li7(j_bracket(1,6,n)),
     |          eta(j_bracket(1,6,n)+1),li7(j_bracket(1,6,n)+1)
 542      format('li7 observation set ',i4/
     |          'high eta bracketing values (eta,li7): (',
     |           1pe13.5,', ',1pe13.5,
     |          ') and (',1pe13.5,', ',1pe13.5,')'/)


       else                       ! bound is not OK

        write(9,544)n,bounds(1,5,n)
 544	format(/'bound ',i4,' on 7Li not work AT HIGH ETA; bound =',i4)

       endif                                         ! conditions on brackets

       enddo                                         ! loop over observations

              write(9,609)
 609	      format('OK at line 609')

c   STOP HERE!!! 

c     bracketing values for the LO li7 curve

      do n=1,n_obs                                    ! loop over observations MID, LO, HI

              write(9,610)
 610	      format('OK at line 610')
      
      if((bounds(2,5,n).eq.0).or.(bounds(2,5,n).eq.-2)) then  
                                                   ! bound is OK at low eta

              write(9,611)
 611	      format('OK at line 611')


         do j=1,j_inf(2)-1                               ! loop over eta values

            if(
     |          (li7_lo(j).gt.li7_o(n))
     |          .and.
     |          (li7_lo(j+1).lt.li7_o(n))
     |         ) then

               j_bracket(2,5,n)=j
               write(9,612)j,li7_lo(j),li7_lo(j+1),li7_o(n)
 612	       format('brackets on low eta --   j = ',i4,3(1pe13.5))
               goto 620                                ! jump out of the loop
            else
              write(9,614)j
 614	      format('no bracket on low eta side -- j = ',i4)
              write(9,616)j,li7_lo(j),li7_lo(j+1),li7_o(n)
 616	      format(i4,3(1pe13.5))
            endif
 
         enddo                                        ! loop over eta values

 620      continue

         write(9,622)n,eta(j_bracket(2,5,n)),li7_lo(j_bracket(2,5,n)),
     |          eta(j_bracket(2,5,n)+1),li7_lo(j_bracket(2,5,n)+1)
 622      format('li7 LO CURVE, observation set ',i4/
     |          'low eta bracketing values (eta,li7): (',
     |           1pe13.5,', ',1pe13.5,
     |          ') and (',1pe13.5,', ',1pe13.5,')'/)

      else                       ! bound is not OK

        write(9,624)n,bounds(2,5,n)
 624	format(/'bound ',i4,' on 7Li LO CURVE will not work AT LOW ETA;bound =',i4)

      endif       ! first condition on bounds


      if((bounds(2,5,n).eq.0).or.(bounds(2,5,n).eq.2)) then  
                                                   ! bound is OK at high eta

         do j=j_inf(2),num_eta-1                               ! loop over eta values

            if(
     |          (li7_lo(j).lt.li7_o(n))
     |          .and.
     |          (li7_lo(j+1).gt.li7_o(n))
     |         ) then

               j_bracket(2,6,n)=j
               write(9,632)j,li7_lo(j),li7_lo(j+1),li7_o(n)
 632	       format('brackets on high eta --   j = ',i4,3(1pe13.5))
               goto 640                                ! jump out of the loop
            else
              write(9,634)j
 634	      format('no bracket on high eta side -- j = ',i4)
              write(9,636)j,li7_lo(j),li7_lo(j+1),li7_o(n)
 636	      format(i4,3(1pe13.5))
            endif
 
         enddo                                        ! loop over eta values

 640      continue

         write(9,642)n,eta(j_bracket(2,6,n)),li7_lo(j_bracket(2,6,n)),
     |          eta(j_bracket(2,6,n)+1),li7_lo(j_bracket(2,6,n)+1)
 642      format('li7 observation set ',i4/
     |          'high eta bracketing values LO CURVE (eta,li7): (',
     |           1pe13.5,', ',1pe13.5,
     |          ') and (',1pe13.5,', ',1pe13.5,')'/)


       else                       ! bound is not OK

        write(9,644)n,bounds(2,5,n)
 644	format(/'bound ',i4,' on 7Li LO CURVE not work AT HIGH ETA; bound =',i4)

       endif                                         ! conditions on brackets



       enddo                                         ! loop over observations


c     bracketing values for the HI li7 curve

      do n=1,n_obs                                    ! loop over observations MID, LO, HI

      
      if((bounds(3,5,n).eq.0).or.(bounds(3,5,n).eq.-2)) then  
                                                   ! bound is OK at low eta

         do j=1,j_inf(3)-1                               ! loop over eta values

            if(
     |          (li7_hi(j).gt.li7_o(n))
     |          .and.
     |          (li7_hi(j+1).lt.li7_o(n))
     |         ) then

               j_bracket(3,5,n)=j
               write(9,712)j,li7_hi(j),li7_hi(j+1),li7_o(n)
 712	       format('brackets on low eta --   j = ',i4,3(1pe13.5))
               goto 720                                ! jump out of the loop
            else
              write(9,714)j
 714	      format('no bracket on low eta side -- j = ',i4)
              write(9,716)j,li7_hi(j),li7_hi(j+1),li7_o(n)
 716	      format(i4,3(1pe13.5))
            endif
 
         enddo                                        ! loop over eta values

 720      continue

         write(9,722)n,eta(j_bracket(3,5,n)),li7_hi(j_bracket(3,5,n)),
     |          eta(j_bracket(3,5,n)+1),li7_hi(j_bracket(3,5,n)+1)
 722      format('li7 hi CURVE, observation set ',i4/
     |          'low eta bracketing values (eta,li7): (',
     |           1pe13.5,', ',1pe13.5,
     |          ') and (',1pe13.5,', ',1pe13.5,')'/)

      else                       ! bound is not OK

        write(9,724)n,bounds(3,5,n)
 724	format(/'bound ',i4,' on 7Li hi CURVE will not work AT LOW ETA;bound =',i4)

      endif       ! first condition on bounds

      if((bounds(3,5,n).eq.0).or.(bounds(3,5,n).eq.2)) then  
                                                   ! bound is OK at high eta

         do j=j_inf(3),num_eta-1                               ! loop over eta values

            if(
     |          (li7_hi(j).lt.li7_o(n))
     |          .and.
     |          (li7_hi(j+1).gt.li7_o(n))
     |         ) then


               j_bracket(3,6,n)=j
               write(9,732)j,li7_hi(j),li7_hi(j+1),li7_o(n)
 732	       format('brackets on high eta --  j = ',i4,3(1pe13.5))
            goto 740                                ! jump out of the loop
            else
              write(9,734)j
 734	      format('no bracket on high eta side -- j = ',i4)
              write(9,736)j,li7_hi(j),li7_hi(j+1),li7_o(n)
 736	      format(i4,3(1pe13.6,1x))
            endif
 
         enddo                                        ! loop over eta values

 740      continue

         write(9,742)n,eta(j_bracket(3,6,n)),li7_hi(j_bracket(3,6,n)),
     |          eta(j_bracket(3,6,n)+1),li7_hi(j_bracket(3,6,n)+1)
 742      format('li7 observation set ',i4/
     |          'high eta bracketing values HI CURVE (eta,li7): (',
     |           1pe13.5,', ',1pe13.5,
     |          ') and (',1pe13.5,', ',1pe13.5,')'/)


       else                       ! bound is not OK

        write(9,744)n,bounds(3,5,n)
 744	format(/'bound ',i4,' on 7Li hi CURVE not work AT HIGH ETA; bound =',i4)

       endif                                         ! conditions on brackets

       enddo                                         ! loop over observations


c--------------------------------------------------------------------------------------------------------------------
c          find interpolated eta values 
c
c        perform an interpolation to find the exact eta value of interest
c        interpolation schemes:
c         linear interpolation
c         polynomial
c         spline
c         others
c
c         estimate the error of the interpolation as well
c         use a different subroutine for each of the interpolation scheme

c         compare the different interpolation schemes with a comparison 
c          subroutine

c
c         use linear interpolation first
c

c
c
c--------------------------------------------------------------------------------------------------------------------
c          initialize interpolation array
c

            eta_inter_dummy=1.660e-25
      do l=1,3
	    do k=1,6
          do n=1,n_obs
            eta_inter(l,k,n)=eta_inter_dummy
          enddo
		enddo
      enddo

c--------------------------------------------------------------------------------------------------------------------
c          find interpolated eta values for h2
c 
c                                      valid (l,k,n) combinations for bounds(l,k,n):
c                                      ! (1,2,1): d central curve, D OBSERV CENTRAL VALUE   
c                                      ! (1,2,2): d central curve, D OBSERV LO VALUE   
c                                      ! (1,2,3): d central curve, D OBSERV HI VALUE   
c                                      ! (2,2,1): d LO curve, D OBSERV CENTRAL VALUE   
c                                      ! (2,2,2): d LO curve, D OBSERV LO VALUE   
c                                      ! (2,2,3): d LO curve, D OBSERV HI VALUE   
c                                      ! (3,2,1): d HI curve, D OBSERV CENTRAL VALUE   
c                                      ! (3,2,2): d HI curve, D OBSERV LO VALUE   
c                                      ! (3,2,3): d HI curve, D OBSERV HI VALUE   

      write(9,802)eta_inter(1,2,1)
 802	format(' OK at start of H2 interpolation',1pe13.6)


c
c          only go through this loop if the observational limit 
c            does provide a bound ... bound(l,k,n) = 0  with l = 2 here
c


c     interpolation for the CENTRAL CURVE

      do n=1,n_obs                                    ! loop over observations 

      if(bounds(1,2,n).ne.0) then                       ! bound is NOT OK
        write(9,804)n
 804	format(/'interpolation ',i4,' from h2 CENTRAL curve will not work')

      elseif(bounds(1,2,n).eq.0) then                   ! bound is OK

         temp1=h2(j_bracket(1,2,n))-h2_o(n)   ! assumes monotonicity -1
         temp2=h2(j_bracket(1,2,n))-h2(j_bracket(1,2,n)+1)
         temp3=eta(j_bracket(1,2,n)+1)-eta(j_bracket(1,2,n))

         eta_inter(1,2,n)= eta(j_bracket(1,2,n)) + ((temp1/temp2)*temp3) 
      
       else  
       endif                                         ! loop on bounds

         write(9,806)n,eta_inter(1,2,n),h2_o(n)
 806	 format(/'h2 observation set ',i4/
     |       'interpolated eta value CENTRAL CURVE (eta, observation)',
     |           1pe13.5,', ',1pe13.5/)

       enddo                                         ! loop over observations



c     interpolation for the LO CURVE

      do n=1,n_obs                                    ! loop over observations 

      if(bounds(2,2,n).ne.0) then                       ! bound is NOT OK
        write(9,808)n
 808	format(/'interpolation ',i4,' from h2 LO curve will not work')

      elseif(bounds(2,2,n).eq.0) then                   ! bound is OK

         temp1=h2_lo(j_bracket(2,2,n))-h2_o(n)   ! assumes monotonicity -1
         temp2=h2_lo(j_bracket(2,2,n))-h2_lo(j_bracket(2,2,n)+1)
         temp3=eta(j_bracket(2,2,n)+1)-eta(j_bracket(2,2,n))

         eta_inter(2,2,n)= eta(j_bracket(2,2,n)) + ((temp1/temp2)*temp3) 
      
       else  
       endif                                         ! loop on bounds

         write(9,810)n,eta_inter(2,2,n),h2_o(n)
 810	 format(/'h2 observation set ',i4/
     |          'interpolated eta value LO CURVE (eta, observation)',
     |           1pe13.5,', ',1pe13.5/)

       enddo                                         ! loop over observations


c     interpolation for the HI CURVE

      do n=1,n_obs                                    ! loop over observations 

      if(bounds(3,2,n).ne.0) then                       ! bound is NOT OK
        write(9,812)n
 812	format(/'interpolation ',i4,' from h2 HI curve will not work')

      elseif(bounds(3,2,n).eq.0) then                   ! bound is OK

         temp1=h2_hi(j_bracket(3,2,n))-h2_o(n)   ! assumes monotonicity -1
         temp2=h2_hi(j_bracket(3,2,n))-h2_hi(j_bracket(3,2,n)+1)
         temp3=eta(j_bracket(3,2,n)+1)-eta(j_bracket(3,2,n))

         eta_inter(3,2,n)= eta(j_bracket(3,2,n)) + ((temp1/temp2)*temp3) 
      
       else  
       endif                                         ! loop on bounds

         write(9,814)n,eta_inter(3,2,n),h2_o(n)
 814	 format(/'h2 observation set ',i4/
     |          'interpolated eta value HI CURVE (eta, observation)',
     |           1pe13.5,', ',1pe13.5/)

       enddo                                         ! loop over observations

c--------------------------------------------------------------------------------------------------------------------
c          find interpolated eta values for he3
c 
c                                      valid (l,k,n) combinations for bounds(l,k,n):
c                                      ! (1,3,1): he3 central curve, he3 OBSERV CENTRAL VALUE   
c                                      ! (1,3,2): he3 central curve, he3 OBSERV LO VALUE   
c                                      ! (1,3,3): he3 central curve, he3 OBSERV HI VALUE   
c                                      ! (2,3,1): he3 LO curve, he3 OBSERV CENTRAL VALUE   
c                                      ! (2,3,2): he3 LO curve, he3 OBSERV LO VALUE   
c                                      ! (2,3,3): he3 LO curve, he3 OBSERV HI VALUE   
c                                      ! (3,3,1): he3 HI curve, he3 OBSERV CENTRAL VALUE   
c                                      ! (3,3,2): he3 HI curve, he3 OBSERV LO VALUE   
c                                      ! (3,3,3): he3 HI curve, he3 OBSERV HI VALUE   



      write(9,816)eta_inter(1,3,1)
 816	format(' OK at start of 3He interpolation',1pe13.6)
 

c     interpolation for the CENTRAL CURVE

      do n=1,n_obs                                    ! loop over observations 

      if(bounds(1,3,n).ne.0) then                       ! bound is NOT OK
        write(9,824)n
 824	format(/'interpolation ',i4,' from he3 CENTRAL curve will not work')

      elseif(bounds(1,3,n).eq.0) then                   ! bound is OK

         temp1=he3(j_bracket(1,3,n))-he3_o(n)   ! assumes monotonicity -1
         temp2=he3(j_bracket(1,3,n))-he3(j_bracket(1,3,n)+1)
         temp3=eta(j_bracket(1,3,n)+1)-eta(j_bracket(1,3,n))

         eta_inter(1,3,n)= eta(j_bracket(1,3,n)) + ((temp1/temp2)*temp3) 
      
       else  
       endif                                         ! loop on bounds

         write(9,826)n,eta_inter(1,3,n),he3_o(n)
 826	 format(/'he3 observation set ',i4/
     |       'interpolated eta value CENTRAL CURVE (eta, observation)',
     |           1pe13.5,', ',1pe13.5/)

       enddo                                         ! loop over observations



c     interpolation for the LO CURVE

      do n=1,n_obs                                    ! loop over observations 

      if(bounds(2,3,n).ne.0) then                       ! bound is NOT OK
        write(9,828)n
 828	format(/'interpolation ',i4,' from he3 LO curve will not work')

      elseif(bounds(2,3,n).eq.0) then                   ! bound is OK

         temp1=he3_lo(j_bracket(2,3,n))-he3_o(n)   ! assumes monotonicity -1
         temp2=he3_lo(j_bracket(2,3,n))-he3_lo(j_bracket(2,3,n)+1)
         temp3=eta(j_bracket(2,3,n)+1)-eta(j_bracket(2,3,n))

         eta_inter(2,3,n)= eta(j_bracket(2,3,n)) + ((temp1/temp2)*temp3) 
      
       else  
       endif                                         ! loop on bounds

         write(9,830)n,eta_inter(2,3,n),he3_o(n)
 830	 format(/'he3 observation set ',i4/
     |          'interpolated eta value LO CURVE (eta, observation)',
     |           1pe13.5,', ',1pe13.5/)

       enddo                                         ! loop over observations


c     interpolation for the HI CURVE

      do n=1,n_obs                                    ! loop over observations 

      if(bounds(3,3,n).ne.0) then                       ! bound is NOT OK
        write(9,832)n
 832	format(/'interpolation ',i4,' from he3 HI curve will not work')

      elseif(bounds(3,3,n).eq.0) then                   ! bound is OK

         temp1=he3_hi(j_bracket(3,3,n))-he3_o(n)   ! assumes monotonicity -1
         temp2=he3_hi(j_bracket(3,3,n))-he3_hi(j_bracket(3,3,n)+1)
         temp3=eta(j_bracket(3,3,n)+1)-eta(j_bracket(3,3,n))

         eta_inter(3,3,n)= eta(j_bracket(3,3,n)) + ((temp1/temp2)*temp3) 
      
       else  
       endif                                         ! loop on bounds

         write(9,834)n,eta_inter(3,3,n),he3_o(n)
 834	 format(/'he3 observation set ',i4/
     |          'interpolated eta value HI CURVE (eta, observation)',
     |           1pe13.5,', ',1pe13.5/)

       enddo                                         ! loop over observations

c--------------------------------------------------------------------------------------------------------------------
c          find interpolated eta values for he4
c 
c                                      valid (l,k,n) combinations for bounds(l,k,n):
c                                      ! (1,4,1): he4 central curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (1,4,2): he4 central curve, he4 OBSERV LO VALUE   
c                                      ! (1,4,3): he4 central curve, he4 OBSERV HI VALUE   
c                                      ! (2,4,1): he4 LO curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (2,4,2): he4 LO curve, he4 OBSERV LO VALUE   
c                                      ! (2,4,3): he4 LO curve, he4 OBSERV HI VALUE   
c                                      ! (3,4,1): he4 HI curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (3,4,2): he4 HI curve, he4 OBSERV LO VALUE   
c                                      ! (3,4,3): he4 HI curve, he4 OBSERV HI VALUE   
c
c 

      write(9,922)eta_inter(1,4,1)
 922	format(' OK at start of 4He interpolation',1pe13.6)


c     interpolation for the CENTRAL CURVE

      do n=1,n_obs                                    ! loop over observations 

      if(bounds(1,4,n).ne.0) then                       ! bound is NOT OK
        write(9,924)n
 924	format(/'interpolation ',i4,' from he4 CENTRAL curve will not work')

      elseif(bounds(1,4,n).eq.0) then                   ! bound is OK

         temp1=he4_o(n) - he4(j_bracket(1,4,n))   ! assumes monotonicity +1
         temp2=he4(j_bracket(1,4,n)+1) - he4(j_bracket(1,4,n))
         temp3=eta(j_bracket(1,4,n)+1) - eta(j_bracket(1,4,n))

         eta_inter(1,4,n)= eta(j_bracket(1,4,n)) + ((temp1/temp2)*temp3) 
      
       else  
       endif                                         ! loop on bounds

         write(9,926)n,eta_inter(1,4,n),he4_o(n)
 926	 format(/'he4 observation set ',i4/
     |       'interpolated eta value CENTRAL CURVE (eta, observation)',
     |           1pe13.5,', ',1pe13.5/)

       enddo                                         ! loop over observations



c     interpolation for the LO CURVE

      do n=1,n_obs                                    ! loop over observations 

      if(bounds(2,4,n).ne.0) then                       ! bound is NOT OK
        write(9,928)n
 928	format(/'interpolation ',i4,' from he4 LO curve will not work')

      elseif(bounds(2,4,n).eq.0) then                   ! bound is OK

         temp1=he4_o(n) - he4_lo(j_bracket(2,4,n))   ! assumes monotonicity +1
         temp2=he4_lo(j_bracket(2,4,n)+1) - he4_lo(j_bracket(2,4,n))
         temp3=eta(j_bracket(2,4,n)+1)-eta(j_bracket(2,4,n))

         eta_inter(2,4,n)= eta(j_bracket(2,4,n)) + ((temp1/temp2)*temp3) 
      
       else  
       endif                                         ! loop on bounds

         write(9,930)n,eta_inter(2,4,n),he4_o(n)
 930	 format(/'he4 observation set ',i4/
     |          'interpolated eta value LO CURVE (eta, observation)',
     |           1pe13.5,', ',1pe13.5/)

       enddo                                         ! loop over observations


c     interpolation for the HI CURVE

      do n=1,n_obs                                    ! loop over observations 

      if(bounds(3,4,n).ne.0) then                       ! bound is NOT OK
        write(9,932)n
 932	format(/'interpolation ',i4,' from he4 HI curve will not work')

      elseif(bounds(3,4,n).eq.0) then                   ! bound is OK

         temp1=he4_o(n) - he4_hi(j_bracket(3,4,n))   ! assumes monotonicity +1
         temp2=he4_hi(j_bracket(3,4,n)+1) - he4_hi(j_bracket(3,4,n))
         temp3=eta(j_bracket(3,4,n)+1)-eta(j_bracket(3,4,n))

         eta_inter(3,4,n)= eta(j_bracket(3,4,n)) + ((temp1/temp2)*temp3) 
      
       else  
       endif                                         ! loop on bounds

         write(9,934)n,eta_inter(3,4,n),he4_o(n)
 934	 format(/'he4 observation set ',i4/
     |          'interpolated eta value HI CURVE (eta, observation)',
     |           1pe13.5,', ',1pe13.5/)

       enddo                                         ! loop over observations




c--------------------------------------------------------------------------------------------------------------------
c          find interpolated eta values for li7
c 
c                                      valid (l,k,n) combinations for bounds(l,k,n):
c                                     ! (1,5,1): li7 central curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (1,5,2): li7 central curve, he4 OBSERV LO VALUE   
c                                      ! (1,5,3): li7 central curve, he4 OBSERV HI VALUE   
c                                      ! (2,5,1): li7 LO curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (2,5,2): li7 LO curve, he4 OBSERV LO VALUE   
c                                      ! (2,5,3): li7 LO curve, he4 OBSERV HI VALUE   
c                                      ! (3,5,1): li7 HI curve, he4 OBSERV CENTRAL VALUE   
c                                      ! (3,5,2): li7 HI curve, he4 OBSERV LO VALUE   
c                                      ! (3,5,3): li7 HI curve, he4 OBSERV HI VALUE   

      write(9,948)eta_inter(1,5,1)
 948	format(' OK at start of 7Li interpolation',1pe13.6)


c     interpolate for the 7Li CENTRAL curve

      do n=1,n_obs                                    ! loop over observations 



      if((bounds(1,5,n).eq.0).or.(bounds(1,5,n).eq.-2)) then  
                                                   ! bound is OK at low eta

         temp1=li7(j_bracket(1,5,n))-li7_o(n) ! assumes monotonicity -1 low eta
         temp2=li7(j_bracket(1,5,n))-li7(j_bracket(1,5,n)+1)
         temp3=eta(j_bracket(1,5,n)+1)-eta(j_bracket(1,5,n))

c         write(9,949)temp1,li7(j_bracket(1,5,n)),li7_o(n),temp2,temp3
c 949	 format(/'TEMP values:'/5(1pe13.6,2x))
 
         eta_inter(1,5,n)= eta(j_bracket(1,5,n)) + ((temp1/temp2)*temp3) 
      
         write(9,950)n,eta_inter(1,5,n),li7_o(n)
 950	 format(/'li7 observation set ',i4/
     |  /'interp. LOW eta value for CENTRAL 7Li CURVE (eta, observ)',
     |     1pe13.6,', ',1pe13.6/)



      else                ! bound not OK

        write(9,952)n
 952	format(/'bound ',i4,' from 7Li will not work on CENTRAL CURVE AT LOW ETA')

      endif               ! bounds


      if((bounds(1,5,n).eq.0).or.(bounds(1,5,n).eq.2)) then  
                                                   ! bound is OK at high eta

         temp1=li7_o(n)-li7(j_bracket(1,6,n)) ! assumes monotonicity +1 high eta
         temp2=li7(j_bracket(1,6,n)+1)-li7(j_bracket(1,6,n))
         temp3=eta(j_bracket(1,6,n)+1)-eta(j_bracket(1,6,n))

         eta_inter(1,6,n)= eta(j_bracket(1,6,n)) + ((temp1/temp2)*temp3) 
      
         write(9,954)n,eta_inter(1,6,n),li7_o(n)
 954	 format(/'li7 observation set ',i4/
     | /'interp. HIGH eta value for CENTRAL 7Li CURVE (eta, observ)',
     |     1pe13.6,', ',1pe13.6/)


       else

        write(9,956)n
 956	format(/'bound ',i4,' from 7Li will not work on CENTRAL CURVE AT HIGH ETA')

       endif          ! check on bounds!

       enddo                                         ! loop over observations


c     interpolate for the 7Li LO curve

      do n=1,n_obs                                    ! loop over observations 



      if((bounds(2,5,n).eq.0).or.(bounds(2,5,n).eq.-2)) then  
                                                   ! bound is OK at low eta

         temp1=li7_lo(j_bracket(2,5,n))-li7_o(n) ! assumes monotonicity -1 low eta
         temp2=li7_lo(j_bracket(2,5,n))-li7_lo(j_bracket(2,5,n)+1)
         temp3=eta(j_bracket(2,5,n)+1)-eta(j_bracket(2,5,n))

         eta_inter(2,5,n)= eta(j_bracket(2,5,n)) + ((temp1/temp2)*temp3) 
      
         write(9,958)n,eta_inter(2,5,n),li7_o(n)
 958	 format('7Li observation set ',i4/
     |    /'interpolated LOW eta value for LOW 7Li CURVE (eta, observ)',
     |     1pe13.6,', ',1pe13.6/)


      else                ! bound not OK

        write(9,960)n
 960	format(/'bound ',i4,' from 7Li will not work on LO CURVE AT LOW ETA')

      endif               ! bounds


      if((bounds(2,5,n).eq.0).or.(bounds(2,5,n).eq.2)) then  
                                                   ! bound is OK at high eta

         temp1=li7_o(n)-li7_lo(j_bracket(2,6,n)) ! assumes monotonicity +1 high eta
         temp2=li7_lo(j_bracket(2,6,n)+1)-li7_lo(j_bracket(2,6,n))
         temp3=eta(j_bracket(2,6,n)+1)-eta(j_bracket(2,6,n))

         eta_inter(2,6,n)= eta(j_bracket(2,6,n)) + ((temp1/temp2)*temp3) 
      
         write(9,962)n,eta_inter(2,6,n),li7_o(n)
 962	 format('7Li observation set ',i4/
     |    /'interpolated HIGH eta value for LO 7Li CURVE (eta, observ)',
     |     1pe13.6,', ',1pe13.6/)

       else

        write(9,964)n
 964	format(/'bound ',i4,' from 7Li will not work on CENTRAL CURVE AT HIGH ETA')

       endif          ! check on bounds!

       enddo                                         ! loop over observations

c     interpolate for the 7Li HI curve

      do n=1,n_obs                                    ! loop over observations 



      if((bounds(3,5,n).eq.0).or.(bounds(3,5,n).eq.-2)) then  
                                                   ! bound is OK at low eta

         temp1=li7_hi(j_bracket(3,5,n))-li7_o(n) ! assumes monotonicity -1 low eta
         temp2=li7_hi(j_bracket(3,5,n))-li7_hi(j_bracket(3,5,n)+1)
         temp3=eta(j_bracket(3,5,n)+1)-eta(j_bracket(3,5,n))

         eta_inter(3,5,n)= eta(j_bracket(3,5,n)) + ((temp1/temp2)*temp3) 
      
         write(9,968)n,eta_inter(3,5,n),li7_o(n)
 968	 format('7Li observation set ',i4/
     |    /'interpolated LOW eta value for HI 7Li CURVE (eta, observ)',
     |     1pe13.6,', ',1pe13.6/)


      else                ! bound not OK

        write(9,970)n
 970	format(/'bound ',i4,' from 7Li will not work on HI CURVE AT LOW ETA')

      endif               ! bounds


      if((bounds(3,5,n).eq.0).or.(bounds(3,5,n).eq.2)) then  
                                                   ! bound is OK at high eta

         temp1=li7_o(n)-li7_hi(j_bracket(3,6,n)) ! assumes monotonicity +1 high eta
         temp2=li7_hi(j_bracket(3,6,n)+1)-li7_hi(j_bracket(3,6,n))
         temp3=eta(j_bracket(3,6,n)+1)-eta(j_bracket(3,6,n))

         eta_inter(3,6,n)= eta(j_bracket(3,6,n)) + ((temp1/temp2)*temp3) 

         write(9,972)n,eta_inter(3,6,n),li7_o(n)
 972     format(/'Li7 observ set',i4
     |    /'interpolated HIGH eta value for HI 7Li CURVE (eta, observ)',
     |     1pe13.6,', ',1pe13.6/)

       else

        write(9,974)n
 974	format(/'bound ',i4,' from 7Li will not work on HI CURVE AT HIGH ETA')

       endif          ! check on bounds!

       enddo                                         ! loop over observations




 1000 continue



c--------------------------------------------------------------------------------------------------------------------
c        OUTPUT
c


c--------------------------------------------------------------------------------------------------------------------
c        XMGR:
c        write out abundances vs. eta for plotting in XMGR
c

c--------------------------------------------------------------------------------------------------------------------
c      results for h2

      do n=1,n_obs
          write(10,1010)eta(1),h2_o(n),eta(num_eta),h2_o(n)
 1010     format(2(1pe13.5)/2(1pe13.5)/'&')
      enddo
	  
c     write out h2 results CENTRAL CURVE

      do j=1, num_eta
         write(10,1012)eta(j),h2(j)
 1012	 format(2(1pe13.6,1x))
      enddo
      write(10,1014)
 1014  format('&')

      do n=1,n_obs
         write(10,1016)eta_inter(1,2,n),h2_o(n)
 1016	 format(2(1pe13.5)/'&')
      enddo

c     write out h2 results LO CURVE

      do j=1, num_eta
         write(10,1018)eta(j),h2_lo(j)
 1018	 format(2(1pe13.6,1x))
      enddo
      write(10,1020)
 1020  format('&')

      do n=1,n_obs
         write(10,1022)eta_inter(2,2,n),h2_o(n)
 1022	 format(2(1pe13.5)/'&')
      enddo

c     write out h2 results HI CURVE

      do j=1, num_eta
         write(10,1024)eta(j),h2_hi(j)
 1024	 format(2(1pe13.6,1x))
      enddo
      write(10,1026)
 1026  format('&')

      do n=1,n_obs
         write(10,1028)eta_inter(3,2,n),h2_o(n)
 1028	 format(2(1pe13.5)/'&')
      enddo


c--------------------------------------------------------------------------------------------------------------------
c      results for he3

      do n=1,n_obs
          write(11,1100)eta(1),he3_o(n),eta(num_eta),he3_o(n)
 1100     format(2(1pe13.5)/2(1pe13.5)/'&')
      enddo
	  
c     write out he3 results CENTRAL CURVE

      do j=1, num_eta
         write(11,1102)eta(j),he3(j)
 1102	 format(2(1pe13.6,1x))
      enddo
      write(11,1104)
 1104  format('&')

      do n=1,n_obs
         write(11,1106)eta_inter(1,3,n),he3_o(n)
 1106	 format(2(1pe13.5)/'&')
      enddo

c     write out he3 results LO CURVE

      do j=1, num_eta
         write(11,1108)eta(j),he3_lo(j)
 1108	 format(2(1pe13.6,1x))
      enddo
      write(11,1110)
 1110  format('&')

      do n=1,n_obs
         write(11,1112)eta_inter(2,3,n),he3_o(n)
 1112	 format(2(1pe13.5)/'&')
      enddo

c     write out he3 results HI CURVE

      do j=1, num_eta
         write(11,1114)eta(j),he3_hi(j)
 1114	 format(2(1pe13.6,1x))
      enddo
      write(11,1116)
 1116  format('&')

      do n=1,n_obs
         write(11,1118)eta_inter(3,3,n),he3_o(n)
 1118	 format(2(1pe13.5)/'&')
      enddo

c--------------------------------------------------------------------------------------------------------------------
c      results for he4

      do n=1,n_obs
          write(12,1200)eta(1),he4_o(n),eta(num_eta),he4_o(n)
 1200     format(2(1pe13.5)/2(1pe13.5)/'&')
      enddo
	  
c     write out he4 results CENTRAL CURVE

      do j=1, num_eta
         write(12,1202)eta(j),he4(j)
 1202	 format(2(1pe13.6,1x))
      enddo
      write(12,1204)
 1204  format('&')

      do n=1,n_obs
         write(12,1206)eta_inter(1,4,n),he4_o(n)
 1206	 format(2(1pe13.5)/'&')
      enddo

c     write out he4 results LO CURVE

      do j=1, num_eta
         write(12,1208)eta(j),he4_lo(j)
 1208	 format(2(1pe13.6,1x))
      enddo
      write(12,1210)
 1210  format('&')

      do n=1,n_obs
         write(12,1212)eta_inter(2,4,n),he4_o(n)
 1212	 format(2(1pe13.5)/'&')
      enddo

c     write out he4 results HI CURVE

      do j=1, num_eta
         write(12,1214)eta(j),he4_hi(j)
 1214	 format(2(1pe13.6,1x))
      enddo
      write(12,1216)
 1216  format('&')

      do n=1,n_obs
         write(12,1218)eta_inter(3,4,n),he4_o(n)
 1218	 format(2(1pe13.5)/'&')
      enddo


c--------------------------------------------------------------------------------------------------------------------
c      results for li7

      do n=1,n_obs
          write(13,1300)eta(1),li7_o(n),eta(num_eta),li7_o(n)
 1300     format(2(1pe13.5)/2(1pe13.5)/'&')
      enddo
	  
c     write out li7 results CENTRAL CURVE

      do j=1, num_eta
         write(13,1302)eta(j),li7(j)
 1302	 format(2(1pe13.6,1x))
      enddo
      write(13,1104)
 1304  format('&')

      do n=1,n_obs
         write(13,1306)eta_inter(1,5,n),li7_o(n)
 1306	 format(2(1pe13.5)/'&')
      enddo

      do n=1,n_obs
         write(13,1307)eta_inter(1,6,n),li7_o(n)
 1307	 format(2(1pe13.5)/'&')
      enddo

c     write out li7 results LO CURVE

      do j=1, num_eta
         write(13,1308)eta(j),li7_lo(j)
 1308	 format(2(1pe13.6,1x))
      enddo
      write(13,1310)
 1310  format('&')

      do n=1,n_obs
         write(13,1312)eta_inter(2,5,n),li7_o(n)
 1312	 format(2(1pe13.5)/'&')
      enddo

      do n=1,n_obs
         write(13,1313)eta_inter(2,6,n),li7_o(n)
 1313	 format(2(1pe13.5)/'&')
      enddo

c     write out li7 results HI CURVE

      do j=1, num_eta
         write(13,1314)eta(j),li7_hi(j)
 1314	 format(2(1pe13.6,1x))
      enddo
      write(13,1316)
 1316  format('&')

      do n=1,n_obs
         write(13,1318)eta_inter(3,5,n),li7_o(n)
 1318	 format(2(1pe13.5)/'&')
      enddo
      do n=1,n_obs
         write(13,1320)eta_inter(3,6,n),li7_o(n)
 1320	 format(2(1pe13.5)/'&')
      enddo

c-----------------------------------------------------------------------------

c--------------------------------------------------------------------------------------------------------------------
c      Logic for final eta ranges

c-----------------------------------------------------------------------------
c      eta range for h2
c
c          expected ordering as eta increases, for LO curve:
c          eta_inter(2,2,3)   LO h2 CURVE intersecting with HI observation        <<<--- probably the LOWEST
c          eta_inter(2,2,1)   LO h2 CURVE intersecting with central observation
c          eta_inter(2,2,2)   LO h2 CURVE intersecting with LO observation


c          expected ordering as eta increases, for CENTRAL curve:
c          eta_inter(1,2,3)   CENTRAL h2 CURVE intersecting with HI observation
c          eta_inter(1,2,1)   CENTRAL h2 CURVE intersecting with central observation  <<--- the CENTRAL
c          eta_inter(1,2,2)   CENTRAL h2 CURVE intersecting with LO observation

c          expected ordering as eta increases, for HI curve:
c          eta_inter(3,2,3)   HI h2 CURVE intersecting with HI observation
c          eta_inter(3,2,1)   HI h2 CURVE intersecting with central observation
c          eta_inter(3,2,2)   HI h2 CURVE intersecting with LO observation            <<-- likely the HIGHEST

c          if h2 curve is monotonic decreasing, then the limits that I have 
c              found so far mean:
c                 LO curve:  eta_inter(2,2,3) <  eta  < eta_inter(2,2,2) 
c                 MID curve:  eta_inter(1,2,3) <  eta  < eta_inter(1,2,2) 
c                 HI curve:  eta_inter(3,2,3) <  eta  < eta_inter(3,2,2) 

c               and the monotonicity means that the full eta range has to be
c                 full:  eta_inter(2,2,3) <  eta  < eta_inter(3,2,2) 
c                   and the central value is eta(1,2,1), as if there were no monte carlo
c                   and no uncertainty in the abundance
c
c           so the greatest range has to come from  
c              low eta:  LO curve and HI observation  eta_inter(2,2,3)  
c               to
c              high eta:  HI curve and LOW observation eta_inter(3,2,2)
c
c            
c            the FULL RANGE can be checked by FINDING the LOWER or the LOWER LIMITS and 
c                the UPPER of the UPPER LIMITS

	   dummy_lim=9.999e-25

      if((eta_inter(2,2,3).lt.eta_inter(1,2,3)).and.
     |  (eta_inter(1,2,3).lt.eta_inter(3,2,3))
     |  ) then                                          ! correct order for lower limit
          
		  error_lim_h2(1) = 0
		  eta_lim_h2(1)=eta_inter(2,2,3)

	  else

		  error_lim_h2(1) = 1
		  eta_lim_h2(1)=dummy_lim

	  endif
	  
	  
	  if(
     |  (eta_inter(2,2,2).lt.eta_inter(1,2,2)).and.
     |  (eta_inter(1,2,2).lt.eta_inter(3,2,2))
     |  ) then                                          ! correct order for upper limit

		  error_lim_h2(3) = 0
		  eta_lim_h2(3)=eta_inter(3,2,2)

	  else

		  error_lim_h2(3) = 1
		  eta_lim_h2(3)=dummy_lim

	  endif
	   
	  if(
     |  (eta_inter(2,2,3).lt.eta_inter(3,2,2))
     |  ) then                                          ! the lower is less than the upper
	      
		  error_lim_h2(2) = 0
		  eta_lim_h2(2)=eta_inter(1,2,1)

	  else

		  error_lim_h2(2) = 1
		  eta_lim_h2(2)=eta_inter(1,2,1)

	  endif


	   
	   write(20,1400)eta_lim_h2(1),eta_lim_h2(2),eta_lim_h2(3) ! the limit from deuterium
 1400  format('h2 lower limit   central value    upper limit'/
     |                  3(1pe13.5,1x))
	 

c-----------------------------------------------------------------------------
c      eta range for he3
c
c          expected ordering as eta increases, for LO curve:
c          eta_inter(2,3,3)   LO he3 CURVE intersecting with HI observation        <<<--- probably the LOWEST
c          eta_inter(2,3,1)   LO he3 CURVE intersecting with central observation
c          eta_inter(2,3,2)   LO he3 CURVE intersecting with LO observation


c          expected ordering as eta increases, for CENTRAL curve:
c          eta_inter(1,3,3)   CENTRAL he3 CURVE intersecting with HI observation
c          eta_inter(1,3,1)   CENTRAL he3 CURVE intersecting with central observation  <<--- the CENTRAL
c          eta_inter(1,3,2)   CENTRAL he3 CURVE intersecting with LO observation

c          expected ordering as eta increases, for HI curve:
c          eta_inter(3,3,3)   HI he3 CURVE intersecting with HI observation
c          eta_inter(3,3,1)   HI he3 CURVE intersecting with central observation
c          eta_inter(3,3,2)   HI he3 CURVE intersecting with LO observation            <<-- likely the HIGHEST

c          if he3 curve is monotonic decreasing, then the limits that I have 
c              found so far mean:
c                 LO curve:  eta_inter(2,3,3) <  eta  < eta_inter(2,3,2) 
c                 MID curve:  eta_inter(1,3,3) <  eta  < eta_inter(1,3,2) 
c                 HI curve:  eta_inter(3,3,3) <  eta  < eta_inter(3,3,2) 

c               and the monotonicity means that the full eta range has to be
c                 full:  eta_inter(2,3,3) <  eta  < eta_inter(3,3,2) 
c                   and the central value is eta(1,3,1), as if there were no monte carlo
c                   and no uncertainty in the abundance
c
c           so the greatest range has to come from  
c              low eta:  LO curve and HI observation  eta_inter(2,3,3)  
c               to
c              high eta:  HI curve and LOW observation eta_inter(3,3,2)
c
c            
c            the FULL RANGE can be checked by FINDING the LOWER or the LOWER LIMITS and 
c                the UPPER of the UPPER LIMITS

	   dummy_lim=9.999e-25

      if((eta_inter(2,3,3).lt.eta_inter(1,3,3)).and.
     |  (eta_inter(1,3,3).lt.eta_inter(3,3,3))
     |  ) then                                          ! correct order for lower limit
          
		  error_lim_he3(1) = 0
		  eta_lim_he3(1)=eta_inter(2,3,3)

	  else

		  error_lim_he3(1) = 1
		  eta_lim_he3(1)=dummy_lim

	  endif
	  
	  
	  if(
     |  (eta_inter(2,3,2).lt.eta_inter(1,3,2)).and.
     |  (eta_inter(1,3,2).lt.eta_inter(3,3,2))
     |  ) then                                          ! correct order for upper limit

		  error_lim_he3(3) = 0
		  eta_lim_he3(3)=eta_inter(3,3,2)

	  else

		  error_lim_he3(3) = 1
		  eta_lim_he3(3)=dummy_lim

	  endif
	   
	  if(
     |  (eta_inter(2,3,3).lt.eta_inter(3,3,2))
     |  ) then                                          ! the lower is less than the upper
	      
		  error_lim_he3(2) = 0
		  eta_lim_he3(2)=eta_inter(1,3,1)

	  else

		  error_lim_he3(2) = 1
		  eta_lim_he3(2)=eta_inter(1,3,1)

	  endif


	   
	   write(20,1500)eta_lim_he3(1),eta_lim_he3(2),eta_lim_he3(3) ! the limit from deuterium
 1500  format('he3 lower limit   central value    upper limit'/
     |                  3(1pe13.5,1x))
	 

c-----------------------------------------------------------------------------
c      eta range for he4
c
c          expected ordering as eta increases, for HI curve:
c          eta_inter(3,4,2)   HI he4 CURVE intersecting with LO observation            <<-- likely the LOWEST
c          eta_inter(3,4,1)   HI he4 CURVE intersecting with central observation
c          eta_inter(3,4,3)   HI he4 CURVE intersecting with HI observation

c          expected ordering as eta increases, for CENTRAL curve:
c          eta_inter(1,4,2)   CENTRAL he4 CURVE intersecting with LO observation
c          eta_inter(1,4,1)   CENTRAL he4 CURVE intersecting with central observation  <<--- the CENTRAL
c          eta_inter(1,4,3)   CENTRAL he4 CURVE intersecting with HI observation

c          expected ordering as eta increases, for LO curve:
c          eta_inter(2,4,2)   LO he4 CURVE intersecting with LO observation
c          eta_inter(2,4,1)   LO he4 CURVE intersecting with central observation
c          eta_inter(2,4,3)   LO he4 CURVE intersecting with HI observation        <<<--- probably the HIGHEST


c          if he4 curve is monotonic decreasing, then the limits that I have 
c              found so far mean:
c                 HI curve:  eta_inter(3,4,2) <  eta  < eta_inter(3,4,3) 
c                 MID curve:  eta_inter(1,4,2) <  eta  < eta_inter(1,4,3) 
c                 LO curve:  eta_inter(2,4,2) <  eta  < eta_inter(2,4,3) 

c               and the monotonicity means that the full eta range has to be
c                 full:  eta_inter(3,4,2) <  eta  < eta_inter(2,4,3) 
c                   and the central value is eta(1,4,1), as if there were no monte carlo
c                   and no uncertainty in the abundance
c
c           so the greatest range has to come from  
c              lo eta:  HI curve and LOW observation eta_inter(3,4,2)
c               to
c              hi eta:  LO curve and HI observation  eta_inter(2,4,3)  
c
c            
c            the FULL RANGE can be checked by FINDING the LOWER or the LOWER LIMITS and 
c                the UPPER of the UPPER LIMITS

	   dummy_lim=9.999e-25

      if((eta_inter(3,4,2).lt.eta_inter(1,4,2)).and.
     |  (eta_inter(1,4,2).lt.eta_inter(2,4,2))
     |  ) then                                          ! correct order for lower limit
          
		  error_lim_he4(1) = 0
		  eta_lim_he4(1)=eta_inter(3,4,2)

	  else

		  error_lim_he4(1) = 1
		  eta_lim_he4(1)=dummy_lim

	  endif
	  
	  
	  if(
     |  (eta_inter(3,4,3).lt.eta_inter(1,4,3)).and.
     |  (eta_inter(1,4,3).lt.eta_inter(2,4,3))
     |  ) then                                          ! correct order for upper limit

		  error_lim_he4(3) = 0
		  eta_lim_he4(3)=eta_inter(2,4,3)

	  else

		  error_lim_he4(3) = 1
		  eta_lim_he4(3)=dummy_lim

	  endif
	   
	  if(
     |  (eta_inter(3,4,2).lt.eta_inter(2,4,3))
     |  ) then                                          ! the lower is less than the upper
	      
		  error_lim_he4(2) = 0
		  eta_lim_he4(2)=eta_inter(1,4,1)

	  else

		  error_lim_he4(2) = 1
		  eta_lim_he4(2)=eta_inter(1,4,1)

	  endif

	   
	   write(20,1600)eta_lim_he4(1),eta_lim_he4(2),eta_lim_he4(3) ! the limit from deuterium
 1600  format('he4 lower limit   central value    upper limit'/
     |                  3(1pe13.5,1x))	 


c-----------------------------------------------------------------------------
c      eta range for li7 LO ETA
c
c          expected ordering as eta increases, for LO curve:
c          eta_inter(2,5,3)   LO li7 CURVE intersecting with HI observation        <<<--- probably the LOWEST
c          eta_inter(2,5,1)   LO li7 CURVE intersecting with central observation
c          eta_inter(2,5,2)   LO li7 CURVE intersecting with LO observation


c          expected ordering as eta increases, for CENTRAL curve:
c          eta_inter(1,5,3)   CENTRAL li7 CURVE intersecting with HI observation
c          eta_inter(1,5,1)   CENTRAL li7 CURVE intersecting with central observation  <<--- the CENTRAL
c          eta_inter(1,5,2)   CENTRAL li7 CURVE intersecting with LO observation

c          expected ordering as eta increases, for HI curve:
c          eta_inter(3,5,3)   HI li7 CURVE intersecting with HI observation
c          eta_inter(3,5,1)   HI li7 CURVE intersecting with central observation
c          eta_inter(3,5,2)   HI li7 CURVE intersecting with LO observation            <<-- likely the HIGHEST

c          if li7 curve is monotonic decreasing, then the limits that I have 
c              found so far mean:
c                 LO curve:  eta_inter(2,5,3) <  eta  < eta_inter(2,5,2) 
c                 MID curve:  eta_inter(1,5,3) <  eta  < eta_inter(1,5,2) 
c                 HI curve:  eta_inter(3,5,3) <  eta  < eta_inter(3,5,2) 

c               and the monotonicity means that the full eta range has to be
c                 full:  eta_inter(2,5,3) <  eta  < eta_inter(3,5,2) 
c                   and the central value is eta(1,5,1), as if there were no monte carlo
c                   and no uncertainty in the abundance
c
c           so the greatest range has to come from  
c              low eta:  LO curve and HI observation  eta_inter(2,5,3)  
c               to
c              high eta:  HI curve and LOW observation eta_inter(3,5,2)
c
c            
c            the FULL RANGE can be checked by FINDING the LOWER or the LOWER LIMITS and 
c                the UPPER of the UPPER LIMITS

c-----------------------------------------------------------------------------
c      eta range for li7 hi eta
c
c          expected ordering as eta increases, for HI curve:
c          eta_inter(3,6,2)   HI li7 CURVE intersecting with LO observation            <<-- likely the LOWEST
c          eta_inter(3,6,1)   HI li7 CURVE intersecting with central observation
c          eta_inter(3,6,3)   HI li7 CURVE intersecting with HI observation

c          expected ordering as eta increases, for CENTRAL curve:
c          eta_inter(1,6,2)   CENTRAL li7 CURVE intersecting with LO observation
c          eta_inter(1,6,1)   CENTRAL li7 CURVE intersecting with central observation  <<--- the CENTRAL
c          eta_inter(1,6,3)   CENTRAL li7 CURVE intersecting with HI observation

c          expected ordering as eta increases, for LO curve:
c          eta_inter(2,6,2)   LO li7 CURVE intersecting with LO observation
c          eta_inter(2,6,1)   LO li7 CURVE intersecting with central observation
c          eta_inter(2,6,3)   LO li7 CURVE intersecting with HI observation        <<<--- probably the HIGHEST


c          if li7 curve is monotonic decreasing, then the limits that I have 
c              found so far mean:
c                 HI curve:  eta_inter(3,6,2) <  eta  < eta_inter(3,6,3) 
c                 MID curve:  eta_inter(1,6,2) <  eta  < eta_inter(1,6,3) 
c                 LO curve:  eta_inter(2,6,2) <  eta  < eta_inter(2,6,3) 

c               and the monotonicity means that the full eta range has to be
c                 full:  eta_inter(3,6,2) <  eta  < eta_inter(2,6,3) 
c                   and the central value is eta(1,6,1), as if there were no monte carlo
c                   and no uncertainty in the abundance
c
c           so the greatest range has to come from  
c              lo eta:  HI curve and LOW observation eta_inter(3,6,2)
c               to
c              hi eta:  LO curve and HI observation  eta_inter(2,6,3)  
c
c            
c            the FULL RANGE can be checked by FINDING the LOWER or the LOWER LIMITS and 
c                the UPPER of the UPPER LIMITS


	   dummy_lim=9.999e-25

c------------

      if(eta_inter(2,5,3).ne.eta_inter_dummy) then      ! then LO curve crosses HI observation at LO ETA - lowest limit on ETA

		  error_lim_li7a(1) = 0
		  eta_lim_li7a(1)=eta_inter(2,5,3)

	  else

		  error_lim_li7a(1) = 1
		  eta_lim_li7a(1)=dummy_lim

       endif
	   


c------------


       if(eta_inter(2,6,3).ne.eta_inter_dummy) then      ! then LO curve crosses HI observation at HI ETA - highest limit on ETA


		  error_lim_li7b(3) = 0
		  eta_lim_li7b(3)=eta_inter(2,6,3)

	  else

		  error_lim_li7b(3) = 1
		  eta_lim_li7b(3)=dummy_lim


       endif


c--------------
c      FOR THE CASE WHEN the HI CURVE DOES NOT INTERSECT THE LOW ABUNDANCE, THEN THERE IS JUST ONE 
c       CONTIGIOUS RANGE IN ETA
c      the eta range is then from eta_lim_li7a(1)  to   eta_lim_li7b(3)   
c              the midpoint should be the average of these
c              note that the central curve does not necessarily have to intersect any observational curve
c      

      if((eta_inter(3,5,2).eq.eta_inter_dummy).and.      ! then HI curve DOES NOT cross LO observation at LO ETA
     |   (error_lim_li7a(1).eq.0).and.
     |   (error_lim_li7b(3).eq.0).and.
     |   (eta_inter(3,6,2).eq.eta_inter_dummy)) then     ! then HI curve DOES NOT cross LO observation at HI ETA

	     write(20,1700)eta_lim_li7a(1),((eta_lim_li7a(1)+eta_lim_li7b(3))/2.0),
     |      eta_lim_li7b(3) ! the limit from li7
 1700    format('li7 lower limit   central value    upper limit'/
     |                  3(1pe13.5,1x))


c-------------
c     two eta ranges

      elseif((eta_inter(3,5,2).ne.eta_inter_dummy).and.      ! then HI curve DOES cross LO observation at LO ETA
     |   (error_lim_li7a(1).eq.0).and.
     |   (error_lim_li7b(3).eq.0).and.
     |   (eta_inter(3,6,2).ne.eta_inter_dummy).and.      ! then HI curve DOES cross LO observation at HI ETA
     |   (eta_inter(3,5,2).le.eta_inter(3,5,2))) then    ! upper bd at low eta LESS THAN lower bd at high eta
	 
		  error_lim_li7a(3) = 0
		  eta_lim_li7a(3)=eta_inter(3,5,2)

		  error_lim_li7b(1) = 0
		  eta_lim_li7b(1)=eta_inter(3,6,2)

		  error_lim_li7a(2) = 0
		  eta_lim_li7a(2)=eta_inter(1,5,1)

		  error_lim_li7b(2) = 0
		  eta_lim_li7b(2)=eta_inter(1,6,1)

	     write(20,1800)eta_lim_li7a(1),eta_lim_li7a(2),eta_lim_li7a(3) ! the limit from li7
 1800    format('li7 lower limit   central value    upper limit'/
     |                  3(1pe13.5,1x))

	   
	     write(20,1820)eta_lim_li7b(1),eta_lim_li7b(2),eta_lim_li7b(3) ! the limit from li7
 1820    format(3(1pe13.5,1x))

c------------

	  endif	      

      stop

      end
