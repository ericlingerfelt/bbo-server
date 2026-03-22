C========================IDENTIFICATION DIVISION=============================

      SUBROUTINE check1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	modified to run on hp
c	comment out open, close statements
c	output directed to unit 6
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
C----------REMARKS.
C     This is an interface subroutine,
C     a flexible module which allows user to manipulate physical quantities
C     of interest at certain key points during the computer run.
C     Included within this subroutine is a roster of all global variables 
C     and their respective COMMON areas.
C
C     This version contains the Monte Carlo analysis for the Smith, 
C     Kawano, and Malaney paper.  The proper running of this version
C     requires a small modification in subroutine NUCCOM.FOR --
C     ** 1 ** In the common area of DERIVS, you need to insert:
C     COMMON /check/  itime                          !Computation location.
C     ** 2 ** You also need to insert the following:
C       CALL rate4                 !Forward rate for all of reactions.
C110  CONTINUE
C       CALL rate3                 !Forward rate for reactions with A < 19.
C120  CONTINUE
C       CALL rate2                 !Forward rate for reactions with A < 10.
C..........SOLVE COUPLED DIFFERENTIAL EQUATIONS.
C     itime = 5         **** insert **** 
C     CALL check        **** insert ****THIS GETS THE RATE VARIATIONS!!
C     CALL sol(loop)
C     IF (mbad.gt.0) RETURN        !Abort in case matrix not invertible.


C----------PARAMETERS.
      PARAMETER (nrec=88)          !Number of nuclear reactions.
      PARAMETER (nvar=29)          !Number of variables to be evolved.
      PARAMETER (nnuc=26)          !Number of nuclides in calculation.
      PARAMETER (itmax=40)         !Maximum # of lines to be printed.
      PARAMETER (nruns_max=10000)         ! MAXIMUM (not actual) NUMBER OF MONTE CARLO RUNS!!!

C----------COMMON AREAS.
cc      COMMON /mcrun/  n_mcrun,yes_monte             ! for monte carlo
      COMMON /mcrun/  n_mcrun,yes_monte,nruns        ! for monte carlo
      COMMON /recpr0/ reacpr                       !Reaction parameter values.
      COMMON /recpr/  iform,ii,jj,kk,ll,rev,q9       !Reaction parameter names.
      COMMON /rates/  f,r                            !Reaction rates.
      COMMON /ratecoeff/  aa                         ! Rate params - new
      COMMON /rateuncer/un1,un12,un16,un17,un20,un24,
     |                un26,un27,un28,un29,un30,un31
      COMMON /evolp1/ t9,hv,phie,y                   !Evolution parameters.
      COMMON /evolp2/ dt9,dhv,dphie,dydt             !Evolution parameters.
      COMMON /evolp3/ t90,hv0,phie0,y0               !Evolution parameters.
      COMMON /compr0/ cy0,ct0,t9i0,t9f0,ytmin0,inc0  !Default comp parameters.
      COMMON /compr/  cy,ct,t9i,t9f,ytmin,inc        !Computation parameters.
      COMMON /modpr0/ c0,cosmo0,xi0                !Default model parameters.
      COMMON /modpr/  g,tau,xnu,c,cosmo,xi           !Model parameters.
      COMMON /varpr0/ dt0,eta0                     !Default variationl params.
      COMMON /varpr/  dt1,eta1                       !Variational parameters.
      COMMON /time/   t,dt,dlt9dt                    !Time variables.
      COMMON /therm2/  thm,hubcst                     !Dynamic variables.
      COMMON /endens/ rhone0,rhob0,rhob,rnb          !Energy densities.
      COMMON /lncoef/ a,b,yx                         !Linear eqn coefficients.
      COMMON /nucdat/ am,zm,dm                       !Nuclide data.
      COMMON /bessel2/ bl1,bl2,bl3,bl4,bl5,           !Eval function bl(z).
     |                bm1,bm2,bm3,bm4,bm5,           !Eval function bm(z).
     |                bn1,bn2,bn3,bn4,bn5            !Eval function bn(z).
      COMMON /kays/   bk0,bk1,bk2,bk3,bk4            !Coefficients K.
      COMMON /flags/  ltime,is,ip,it,mbad            !Flags,counters.
      COMMON /check2/  itime                          !Computation location.
      COMMON /outdat/ xout,thmout,t9out,tout,dtout,  !Output data.
     |                etaout,hubout
      COMMON /nupar/  t9mev,tnmev,tnu,cnorm,nu,rhonu !Neutrino parameters.
      COMMON /runopt/ irun,isize,jsize               !Run options.
      COMMON /outopt/ nout,outfile                   !Output option.

      COMMON /temp/   iodd,sigma,mark,               ! all for monte carlo
     |                avg_2,avg_3,avg_4,avg_7,       ! may not need to be in common!
     |                dev_2,dev_3,dev_4,dev_7,
     |                a2,a3,a4,a7,
     |                ee01,ee12,ee16,ee17,ee20,ee24,
     |                ee26,ee27,ee28,ee29,ee30,ee31


      COMMON /switcher/ switch                     ! for new / old rates


C==========================DECLARATION DIVISION=============================

C----------REACTION PARAMETER VALUES.
      REAL    reacpr(nrec,8)       !Reaction parameters.

C----------REACTION PARAMETER NAMES.
      INTEGER iform(nrec)          !Reaction type code (1-11).
      INTEGER ii(nrec)             !Incoming nuclide type (1-26).
      INTEGER jj(nrec)             !Incoming light nuclide type (1-6).
      INTEGER kk(nrec)             !Outgoing light nuclide type (1-6).
      INTEGER ll(nrec)             !Outgoing nuclide type (1-26).
      REAL    rev(nrec)            !Reverse reaction coefficient.
      REAL    q9(nrec)             !Energy released in reaction (in 10**9 K).

C----------REACTION RATES.
      REAL    f(nrec)              !Forward reaction rate coefficients.
      REAL    r(nrec)              !Reverse reaction rate coefficients.
      REAL    aa(nrec,35)  !REACLIB RATE params - 35 per reaction only - NEW

C----------EVOLUTION PARAMETERS.
      REAL    t9                   !Temperature of photons (units of 10**9 K).
      REAL    hv                   !Defined by hv = M(atomic)n(baryon)/t9**3.
      REAL    phie                 !Chemical potential of electron.
      REAL    y(nnuc)              !Relative number abundances.

C----------EVOLUTION PARAMETERS (DERIVATIVES).
      REAL    dt9                  !Change in temperature.
      REAL    dhv                  !Change in hv.
      REAL    dphie                !Change in chemical potential.
      REAL    dydt(nnuc)           !Change in relative number abundances.

C----------EVOLUTION PARAMETERS (ORIGINAL VALUES).
      REAL    y0(nnuc)             !Rel # abundances at end of 1st R-K loop.

C----------DEFAULT COMPUTATION PARAMETERS.
      REAL    cy0                  !Default cy.
      REAL    ct0                  !Default ct.
      REAL    t9i0                 !Default t9i.
      REAL    t9f0                 !Default t9f.
      REAL    ytmin0               !Default ytmin.
      INTEGER inc0                 !Default accumilation increment.

C----------COMPUTATION PARAMETERS.
      REAL    cy                 !Time step limiting constant on abundances.
      REAL    ct                 !Time step limiting constant on temperature.
      REAL    t9i                  !Initial temperature (in 10**9 K).
      REAL    t9f                  !Final temperature (in 10**9 K).
      REAL    ytmin                !Smallest abundances allowed.
      INTEGER inc                  !Accumilation increment.

C----------DEFAULT MODEL PARAMETERS.
      REAL    c0(3)                !Default c.
      REAL    cosmo0               !Default cosmological constant.
      REAL    xi0(3)               !Default neutrino degeneracy parameters.

C----------EARLY UNIVERSE MODEL PARAMETERS.
      REAL    g                    !Gravitational constant.
      REAL    tau                  !Neutron lifetime (sec).
      REAL    xnu                  !Number of neutrino species.
      REAL    c(3)               !c(1) is variation of gravitational constant.
     |                             !c(2) is neutron half-life (min).
     |                             !c(3) is number of neutrino species.
      REAL    cosmo                !Cosmological constant.
      REAL    xi(3)                !Neutrino degeneracy parameters.
     |                             !xi(1) is e neutrino degeneracy parameter.
     |                             !xi(2) is m neutrino degeneracy parameter.
     |                             !xi(3) is t neutrino degeneracy parameter.

C----------DEFAULT VARIATIONAL PARAMETERS.
      REAL    dt0                  !Default initial time step.
      REAL    eta0                 !Default baryon-to-photon ratio.

C----------VARIATIONAL PARAMETERS.
      REAL    dt1                  !Initial time step.
      REAL    eta1                 !Baryon-to-photon ratio.

C----------TIME VARIABLES.
      REAL    t                    !Time.
      REAL    dt                   !Time step.
      REAL    dlt9dt               !(1/t9)*d(t9)/d(t).

C----------DYNAMIC VARIABLES.
      REAL    thm(14)              !Thermodynamic variables (energy densities).
      REAL    hubcst               !Expansion rate of the universe.

C----------ENERGY DENSITIES.
      REAL    rhone0               !Initial electron neutrino energy density.
      REAL    rhob0                !Initial baryon energy density.
      REAL    rhob                 !Baryon energy density.
      REAL    rnb                  !Baryon energy density (ratio to init value).

C----------MATRIX COEFFICIENTS FOR LINEAR EQUATION.
      DOUBLE PRECISION a(nnuc,nnuc)!Relates y(t+dt) to y(t).
      REAL    b(nnuc)              !Contains y0 in inverse order.
      REAL    yx(nnuc)             !yy in reverse order.

C----------NUCLIDE DATA.
      REAL    am(nnuc)             !Atomic number of nuclide.
      REAL    zm(nnuc)             !Charge of nuclide.
      REAL    dm(nnuc)             !Mass excess of nuclide.

C----------EVALUATION OF FUNCTIONS bl,bm,bn.
      REAL    bl1,bl2,bl3,bl4,bl5  !Evaluation of function bl(z).
      REAL    bm1,bm2,bm3,bm4,bm5  !Evaluation of function bm(z).
      REAL    bn1,bn2,bn3,bn4,bn5  !Evaluation of function bn(z).

C----------EVALUATION OF MODIFIED BESSEL FUNCTIONS.
      REAL    bk0,bk1,bk2,bk3,bk4  !Values k0(r),k1(r),k2(r),k3(r),k4(r).

C----------FLAGS AND COUNTERS.
      INTEGER ltime                !Indicates if output buffer printed.
      INTEGER is                   !# total iterations for particular model.
      INTEGER ip                   !# iterations after outputing a line.
      INTEGER mbad                 !Indicates if gaussian elimination failed.

C----------COMPUTATION LOCATION.
      INTEGER itime                !Time check.

C----------OUTPUT ARRAYS.
      REAL    xout(itmax,nnuc)     !Nuclide mass fractions.
      REAL    thmout(itmax,6)      !Thermodynamic variables.
      REAL    t9out(itmax)         !Temperature (in units of 10**9 K).
      REAL    tout(itmax)          !Time.
      REAL    dtout(itmax)         !Time step.
      REAL    etaout(itmax)        !Baryon to photon ratio.
      REAL    hubout(itmax)        !Expansion rate.

C----------NEUTRINO PARAMETERS.
      REAL    t9mev                !Temperature (in units of MeV).
      REAL    tnmev                !Neutrino temperature (in units of MeV).
      REAL    tnu                  !Neutrino temperature.
      REAL    cnorm                !Normalizing constant.
      REAL    rhonu                !Neutrino energy density.
      INTEGER nu                   !Type of neutrino.

C----------RUN OPTION.
      INTEGER irun                 !Run network size.
      INTEGER isize                !Number of nuclides in computation.
      INTEGER jsize                !Number of reactions in computation.

C----------OUTPUT FILE STATUS.
      INTEGER nout                 !Number of output requests.
      LOGICAL outfile              !Indicates if output file used.

C----------MONTE CARLO SIGMA GENERATOR.
      REAL a2(nruns_max),a3(nruns_max),a4(nruns_max),a7(nruns_max)
      REAL ee01(nruns_max),ee12(nruns_max),ee16(nruns_max)
      REAL ee17(nruns_max)
      REAL ee20(nruns_max),ee24(nruns_max),ee26(nruns_max)
      REAL ee27(nruns_max)
      REAL ee28(nruns_max),ee29(nruns_max),ee30(nruns_max)
      REAL ee31(nruns_max)

      REAL un1,un12,un16,un17,un20,un24,            ! rate uncertainties
     |                un26,un27,un28,un29,un30,un31

      REAL e1,e12,e16,e17
      REAL e20,e24,e26,e27
      REAL e28,e29,e30,e31

      REAL sigma(12)
      REAL point(50)
      DATA point /0.013,0.038,0.063,0.088,0.113,
     |            0.138,0.164,0.189,0.217,0.241,
     |            0.266,0.292,0.319,0.345,0.372,
     |            0.399,0.426,0.454,0.482,0.510,
     |            0.539,0.568,0.598,0.628,0.659,
     |            0.690,0.723,0.755,0.789,0.824,
     |            0.860,0.897,0.935,0.974,1.015,
     |            1.058,1.103,1.150,1.201,1.253,
     |            1.311,1.372,1.439,1.514,1.598,
     |            1.696,1.811,1.960,2.170,2.575/

      INTEGER n_mcrun            ! index of the monte carlo run
      INTEGER n_sets             ! number of param sets for each reaction
      CHARACTER*30  char_dummy   ! label for reaction
      CHARACTER*30  dummy_uncer   ! label for reaction uncertainty
      INTEGER switch(90)         ! switches for new / old reaction rates
      INTEGER yes_monte          ! 1 for Monte Carlo, 0 for no Monte Carlo
      INTEGER nruns              ! total number of actual monte carlo runs
      REAL realjunk              ! output uncertainties for non-monte carlo runs
C===========================PROCEDURE DIVISION================================

C10--------OPEN FILE----------------------------------------------------------

      IF (itime.eq.1) THEN         !Beginning of program.

cc        OPEN (unit=3, file='nucint.dat',  status='old')


C11--------INITIALIZE MC uncertainties -------------------------------
        DO j=1,12
          sigma(j)=0.
        ENDDO

C11a--------INITIALIZE switch -------------------------------
        DO i=1,90
          switch(i)=0
        ENDDO

C11b--------INITIALIZE yes_monte -------------------------------

cc        yes_monte=0
      

C11--------INITIALIZE ABUND MC MATRICES AND ERROR MATRICES ------------------

        DO j=1,nruns
          a2(j)=0.
          a3(j)=0.
          a4(j)=0.
          a7(j)=0.
        ENDDO

        DO j=1,nruns
          ee01(j)   = 0.
          ee12(j)   = 0.
          ee16(j)   = 0.
          ee17(j)   = 0.
          ee20(j)   = 0.
          ee24(j)   = 0.
          ee26(j)   = 0.
          ee27(j)   = 0.
          ee28(j)   = 0.
          ee29(j)   = 0.
          ee30(j)   = 0.
          ee31(j)   = 0.
        ENDDO

       
c       IF(yes_monte.eq.1) THEN
c      
c      
c      ELSE   !  if yes_monte is not equal to 1, skip those steps
c      ENDIF   !  if yes_monte
        
      END IF ! itime.eq.1


      IF (itime.eq.2) THEN
        iodd  = -1961
        mark  = 0

        avg_2 = 0.
        avg_3 = 0.
        avg_4 = 0.
        avg_7 = 0.
        dev_2 = 0.
        dev_3 = 0.
        dev_4 = 0.
        dev_7 = 0.

      END IF



      IF (itime.eq.3) THEN

       IF(yes_monte.eq.1) THEN
       
          DO i = 1,nruns        !  MAIN MONTE CARLO LOOP 

              n_mcrun=i   ! this needs to be passed to nuccommcb.f via common

              mark = mark + 1
              IF (mark.eq.5) THEN
                 mark = 0
              END IF

              DO j = 1,12
                ran01 = ran(iodd)
                ran01 = 100*(ran01 - 0.5)
                IF (ran01.eq.0.) THEN
                   ran01 = 0.5
                ELSE
                   ran01 = (ran01/abs(ran01))*0.5 + ran01
                END IF
                icoun = abs(nint(ran01))

                 IF (icoun.gt.50) icoun = 50
                 IF (icoun.lt.1)  icoun = 1
                 sigma(j) = (ran01/abs(ran01))*point(icoun)

              END DO   ! loop over 12 reactions in variable j

              CALL driver   ! this calls nuccommcb.f, the main solver
                        ! DRIVER in turns does set itime=5 in the 
                        ! middle, 

c           accumulate abundances

            xout(it,8) = xout(it,8) + xout(it,9)  
            xout(it,5) = xout(it,5) + xout(it,4)  
            xout(it,6) = xout(it,6)-0.0025  
            a2(i) = xout(it,3)
            a3(i) = xout(it,5)
            a4(i) = xout(it,6)
            a7(i) = xout(it,8)
            avg_2 = avg_2 + a2(i)
            avg_3 = avg_3 + a3(i)
            avg_4 = avg_4 + a4(i)
            avg_7 = avg_7 + a7(i)

              END DO  ! loop over MC runs in variable i
        
                 avg_2 = avg_2/nruns
                 avg_3 = avg_3/nruns
                 avg_4 = avg_4/nruns
                 avg_7 = avg_7/nruns
                 DO i = 1,nruns
                       dev_2 = dev_2 + (a2(i)-avg_2)**2
                       dev_3 = dev_3 + (a3(i)-avg_3)**2
                       dev_4 = dev_4 + (a4(i)-avg_4)**2
                       dev_7 = dev_7 + (a7(i)-avg_7)**2
                 END DO
                 dev_2 = sqrt(dev_2/(nruns-1))
                 dev_3 = sqrt(dev_3/(nruns-1))
                 dev_4 = sqrt(dev_4/(nruns-1))
                 dev_7 = sqrt(dev_7/(nruns-1))

                 WRITE (39,192) etaout(it),avg_2,dev_2
                 WRITE (39,192) etaout(it),avg_3,dev_3
                 WRITE (39,192) etaout(it),avg_4,dev_4
                 WRITE (39,192) etaout(it),avg_7,dev_7
                 WRITE (39,193) nruns, xnu, etaout(it)

                 WRITE (42,192) etaout(it),avg_2,dev_2
                 WRITE (42,192) etaout(it),avg_3,dev_3
                 WRITE (42,192) etaout(it),avg_4,dev_4
                 WRITE (42,192) etaout(it),avg_7,dev_7
                 WRITE (42,193) nruns, xnu, etaout(it)
 192             FORMAT(3(1pe13.6,1x))
 193             FORMAT(i6,1x,2(1pe13.6,1x))

c--------------- CORRELATION OUTPUT

c        WRITE (39,193)
c 193    FORMAT(/' REAC 1 uncertainty, Abund 2')

c        DO j=1,nruns
c          WRITE (39,194)ee01(j),a2(j)
c 194      FORMAT(2(1pe13.6,1x))
c        ENDDO


c        WRITE (39,196)
c 196    FORMAT(/' REAC 12 uncertainty, Abund 2')

c        DO j=1,nruns
c          WRITE (39,197)ee12(j),a2(j)
c 197      FORMAT(2(1pe13.6,1x))
c        ENDDO

c        WRITE (39,198)
c 198    FORMAT(/' REAC 17 uncertainty, Abund 2')

c        DO j=1,nruns
c          WRITE (39,199)ee17(j),a2(j)
c 199      FORMAT(2(1pe13.6,1x))
c        ENDDO


         CALL OUTPUT                ! write values to output file


               STOP       ! ONLY FOR THE MONTE CARLO CASE!


          ELSE  ! if yes_monte.eq.0  .. do nothing so we can return to RUN

          ENDIF  !yes_monte
         

      END IF    ! itime = 3





      IF (itime.eq.5) THEN

        IF(yes_monte.eq.1) THEN
        
cc        IF (t9.gt.10) THEN
cc          un26  = 0.5*(0.1612) 
cc          un27  = 0.5*(0.1947) 
cc        ELSE
cc          t9a   = t9 + 0.04189
cc          t9a12 = sqrt(t9a)            !t9a**(1/2)
cc          t9a32 = t9a*t9a12            !t9a**(3/2)
cc          t9b   = t9 + 0.7829
cc          t9b12 = sqrt(t9b)            !t9b**(1/2)
cc          t9b32 = t9b*t9b12            !t9b**(3/2)


cc          un26  = 0.5*(0.5724 - 0.1177*t9a12 - 0.1446*t9a +
cc     |                 7.973e-2*t9a32 - 1.114e-2*t9a*t9a)
cc          un27  = 0.5*(0.5414 - 0.3012*t9b12 + 0.08034*t9b -
cc     |                 4.954e-3*t9b32 - 3.974e-4*t9b*t9b)
cc
cc        END IF



        j=n_mcrun

cc        ee01(j)    = (3.73/.98)*sigma(1)
        ee01(j)    = un1*sigma(1)

c       un are positive
c       sigma can be positive or negative
c       ee are deviations from 1, multiplicative rate factors
        
        ee12(j)   = 1.0 + un12*sigma(2)
        ee16(j)   = 1.0 + un16*sigma(3)
        ee17(j)   = 1.0 + un17*sigma(4)
        ee20(j)   = 1.0 + un20*sigma(5)
        ee24(j)   = 1.0 + un24*sigma(6)
        ee26(j)   = 1.0 + un26*sigma(7)
        ee27(j)   = 1.0 + un27*sigma(8)
        ee28(j)   = 1.0 + un28*sigma(9)
        ee29(j)   = 1.0 + un29*sigma(10)
        ee30(j)   = 1.0 + un30*sigma(11)
        ee31(j)   = 1.0 + un31*sigma(12)

        f(1)  = thm(13)/(tau+ee01(j)) ! a bit differently than the others
        r(1)  = thm(14)/(tau+ee01(j))

        f(12) = f(12) * ee12(j)
        f(16) = f(16) * ee16(j)
        f(17) = f(17) * ee17(j)
        f(20) = f(20) * ee20(j)
        f(24) = f(24) * ee24(j)
        f(26) = f(26) * ee26(j)
        f(27) = f(27) * ee27(j)
        f(28) = f(28) * ee28(j)
        f(29) = f(29) * ee29(j)
        f(30) = f(30) * ee30(j)
        f(31) = f(31) * ee31(j)

        ELSE   ! yes_monte = 0

        ENDIF  ! yes_monte

      END IF ! itime = 5


c -------- the next section, at itime = 7, will write out abundances at each time step
c --------- uncomment and use later when we want to pipe this to the user

c        IF (itime.eq.7) THEN         !after runge kutta 2nd loop in nuccommcb.f, line 147
c
c         IF(yes_monte.eq.0) THEN    ! only for a single run
cc            write out time, temp, neutron(1), proton(2), deuterium(3), tritium (4), 3he (5),
cc               4he (6), ?? , 7li (8) 
c			 WRITE (38,201)t,t9,(y(j),j=1,8)
c			 WRITE (39,201)t,t9,(y(j),j=1,8)
c201          FORMAT(2(1x,1pe13.6)/4(2(1x,1pe13.6))//)
c
c        ENDIF  ! yes_monte
c
c      END IF ! itime = 7


C20--------WRITE INTO FILE------------------------------------------------------

      IF (itime.eq.8) THEN         !Right after a run.

          WRITE (39,219) etaout(it),yes_monte

219       FORMAT(' itime=8', 1x, 1pe13.6, 'yes_monte = ', i4)

      IF(yes_monte.eq.0) THEN    ! only for a single run
      
c      WRITE (3,210)itime
c 210  format(/'        START itime=8 ... itime = ',i4)

        xout(it,8) = xout(it,8) + xout(it,9)  !Add beryllium to lithium.
        xout(it,5) = xout(it,5) + xout(it,4)  !Add tritium to helium-3.
        xout(it,6) = xout(it,6)-0.0025  
     |              !Radiative, coulomb, finite-temperature corrections (Ref 1).


c          WRITE (6,220) etaout(it),xout(it,3)
c          WRITE (6,220) etaout(it),xout(it,5) 
c          WRITE (6,220) etaout(it),xout(it,6)
c          WRITE (6,220) etaout(it),xout(it,8)
c          WRITE (6,221) nruns, xnu, etaout(it)

          WRITE (39,220) etaout(it),xout(it,3),realjunk
          WRITE (39,220) etaout(it),xout(it,5),realjunk 
          WRITE (39,220) etaout(it),xout(it,6),realjunk
          WRITE (39,220) etaout(it),xout(it,8),realjunk
          WRITE (39,221) nruns, xnu, etaout(it)
          
          realjunk=9.999999E-15
          
          WRITE (42,220) etaout(it),xout(it,3),realjunk
          WRITE (42,220) etaout(it),xout(it,5),realjunk
          WRITE (42,220) etaout(it),xout(it,6),realjunk
          WRITE (42,220) etaout(it),xout(it,8),realjunk
          WRITE (42,221) nruns, xnu, etaout(it)

 220    FORMAT(3(1pe13.6,1x))
 221    FORMAT(i6,1x,2(1pe13.6,1x))
 
       ELSE ! yes_monte
       ENDIF ! yes_monte
       
      END IF ! itime = 8

C30--------CLOSE FILE-----------------------------------------------------------

      IF (itime.eq.10) THEN        !End of program.
c        CLOSE (unit=3)


      END IF ! itime = 10
      RETURN

C----------REFERENCES-----------------------------------------------------------
C     1) D.A. Dicus, E.W. Kolb, A.M. Gleeson, E.C.G. Sudarshan, V.L. Teplitz,
C        M.S. Turner, Phys. Rev. D., 26,2694 (1982).

      END
