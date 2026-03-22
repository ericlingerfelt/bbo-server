C========================IDENTIFICATION DIVISION==============================
      PROGRAM nucb
c
c     new truncated version 4/29/06
c
c     read all data in from a file
c
c     multiple runs not allowed - control this from outside the program
c

cccccccccccccccccccccccccc modified for hp computer 6/5/92
cccccccccccccccccccccccc  runs in batch mode
c		take out all lines write (iw,  ) ...no output to 
c		batout.inc
cccccccccccccccccccccccccccccccccccccccccccccccccccc
c		direct all output to unit 6 (previously unit 2)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c		also comment out lines near line 460 (close statements)
c		and open statements
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c		use an assign statement in vax, use a redirected I/O on HP
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c		vax: assign unit=5 batin.inc
c		     assign unit=6 nuc123.dat
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c		hp: a.out <batin.inc>nuc123.dat
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
C----------LINKAGES.
C     CALLED BY - none
C     CALLS     - [subroutine] help, setcom, setmod, run, output

C----------REMARKS.
C     Control program -
C       Offers user the main menu and channels through to various options.
C     Implementation -
C       To run this program, NUC123.FOR must be linked with NUCCOM 
C       (containing the computation subroutines), NUCRAT (with the
C       reaction rates), and NUCINT (with an interface subroutine).
C       This program has been written to be compatible with
C       ANSI FORTRAN-77 with the exception of the END DO statement
C       used to limit the number of statement labels.
C       The code was developed on the VAX/VMS system.
C     Notes -
C       The program utilizes Wagoner's code as the core of the computational
C       routines.
C     Documentation -
C       Kawano, L., 1992, Fermilab preprint FERMILAB-PUB-92/04-A,
C       Kellogg Radiation Lab preprint OAP-714.
C     Copy -
C       Version 4.1 (December 1991)

C----------PARAMETERS.
      PARAMETER (nrec=88)          !Number of nuclear reactions.
      PARAMETER (nnuc=26)          !Number of nuclides in calculation.

C----------COMMON AREAS.
      COMMON /recpr0/ reacpr                         !Reaction parameter values.
      COMMON /recpr/  iform,ii,jj,kk,ll,rev,q9       !Reaction parameter names.
      COMMON /rates/  f,r                            !Reaction rates.
      COMMON /ratecoeff/  aa                         ! Rate params - new
      COMMON /rateuncer/un1,un12,un16,un17,un20,un24,
     |                un26,un27,un28,un29,un30,un31
      COMMON /switcher/ switch                     ! for new / old rates
      COMMON /compr0/ cy0,ct0,t9i0,t9f0,ytmin0,inc0  !Default comp parameters.
      COMMON /compr/  cy,ct,t9i,t9f,ytmin,inc        !Computation parameters.
      COMMON /modpr0/ c0,cosmo0,xi0                  !Default model parameters.
      COMMON /modpr/  g,tau,xnu,c,cosmo,xi           !Model parameters.
      COMMON /varpr0/ dt0,eta0                       !Default variationl params.
      COMMON /varpr/  dt1,eta1                       !Variational parameters.
      COMMON /check2/  itime                          !Computation location.
      COMMON /runopt/ irun,isize,jsize               !Run options.
      COMMON /outopt/ nout,outfile                   !Output option.

      COMMON /mcrun/  n_mcrun,yes_monte,nruns        ! for monte carlo


C==========================DECLARATION DIVISION=================================

C----------REACTION PARAMETERS FROM BLOCK DATA.
      REAL    reacpr(nrec,8)       !Reaction parameters.

C----------REACTION PARAMETERS.
      INTEGER iform(nrec)          !Reaction type code (1-11).
      INTEGER ii(nrec)             !Incoming nuclide type (1-26).
      INTEGER jj(nrec)             !Incoming light nuclide type (1-6).
      INTEGER kk(nrec)             !Outgoing light nuclide type (1-6).
      INTEGER ll(nrec)             !Outgoing nuclide type (1-26).
      REAL    rev(nrec)            !Reverse reaction coefficient.
      REAL    q9(nrec)             !Energy released in reaction.

C----------REACTION RATES.
      REAL    f(nrec)              !Forward reaction rate coefficients.
      REAL    r(nrec)              !Reverse reaction rate coefficients.
      REAL    aa(nrec,35)  !REACLIB RATE params - 35 per reaction only - NEW
      REAL un1,un12,un16,un17,un20,un24,   ! rate uncertainties
     |                un26,un27,un28,un29,un30,un31
      INTEGER switch(90)         ! switches for new / old reaction rates

C----------DEFAULT COMPUTATION PARAMETERS.
      REAL    cy0                  !Default cy.
      REAL    ct0                  !Default ct.
      REAL    t9i0                 !Default t9i.
      REAL    t9f0                 !Default t9f.
      REAL    ytmin0               !Default ytmin.
      INTEGER inc0                 !Default accumulation increment.

C----------COMPUTATIONAL PARAMETERS.
      REAL    cy                   !Time step limiting constant on abundances.
      REAL    ct                   !Time step limiting constant on temperature.
      REAL    t9i                  !Initial temperature (in 10**9 K).
      REAL    t9f                  !Final temperature (in 10**9 k).
      REAL    ytmin                !Smallest abundances allowed.
      INTEGER inc                  !Accumulation increment.

C----------DEFAULT MODEL PARAMETERS.
      REAL    c0(3)                !Default c.
      REAL    cosmo0               !Default cosmological constant.
      REAL    xi0(3)               !Default neutrino degeneracy parameters.

C----------EARLY UNIVERSE MODEL PARAMETERS.
      REAL    c(3)                 !c(1) is variation of gravitational constant.
     |                             !c(2) is neutron lifetime (sec).
     |                             !c(3) is number of neutrino species.
      REAL    cosmo                !Cosmological constant.
      REAL    xi(3)                !Neutrino degeneracy parameters.

C----------DEFAULT VARIATIONAL PARAMETERS.
      REAL    dt0                  !Default initial time step.
      REAL    eta0                 !Default baryon-to-photon ratio.

C----------VARIATIONAL PARAMETERS.
      REAL    dt1                  !Initial time step.
      REAL    eta1                 !Baryon-to-photon ratio.

C----------COMPUTATION LOCATION.
      INTEGER itime                !Time check.

C----------RUN OPTION.
      INTEGER irun                 !Run network size.
      INTEGER isize                !Number of nuclides in computation.
      INTEGER jsize                !Number of reactions in computation.

C----------OUTPUT FILE STATUS.
      INTEGER nout                 !Number of output requests.
      LOGICAL outfile              !Indicates if output file used.
      
C----------MONTE CARLO INFORMATION
      INTEGER n_mcrun            ! index of the monte carlo run
      INTEGER yes_monte          ! 1 for Monte Carlo, 0 for no Monte Carlo
      INTEGER nruns            ! total number of monte carlo runs


C----------USER RESPONSE VARIABLES.
      INTEGER inum                 !Selection number.

      CHARACTER input
      CHARACTER*30  char_dummy   ! label for reaction
      CHARACTER*30  dummy_uncer   ! label for reaction uncertainty
      
C===========================PROCEDURE DIVISION==================================

C10--------OPEN FILES ----------------------------------------

      OPEN (unit=36, file='bbn_input.txt', status='old') !input file
      OPEN (unit=39, file='bbn_output.txt', status='old') !output file
      OPEN (unit=42, file='bbn_outputa.txt', status='old') !output file (eta vs. abund)
      OPEN(unit=9,file='rates.dat', status='old')
      OPEN(unit=10,file='uncer.dat', status='old')
      OPEN(unit=11,file='switch.dat', status='old')




C10--------Call Interface Subroutine ----------------------------------------


      itime = 1                    !Time = beginning of program.
      CALL check1                   !Check interface subroutine.

C20--------INPUT INITIALIZATION INFORMATION ---------------------------

      DO i  = 1,nrec
C..........READ IN REACTION PARAMETERS FROM BLOCK DATA
        iform(i) = int(reacpr(i,2))!Reaction type.
        ii(i)    = int(reacpr(i,3))!Incoming nuclide type.
        jj(i)    = int(reacpr(i,4))!Incoming nuclide type.
        kk(i)    = int(reacpr(i,5))!Outgoing nuclide type.
        ll(i)    = int(reacpr(i,6))!Outgoing nuclide type.
        rev(i)   = reacpr(i,7)     !Reverse reaction coefficient.
        q9(i)    = reacpr(i,8)     !Energy released.
C..........INITIALIZE REACTION RATES.
        f(i)  = 0.                 !Forward rate coeff.
        r(i)  = 0.                 !Reverse rate coeff.
      END DO
C..........SET RUN OPTIONS TO DEFAULT.
      irun       = 1               !Do full run.
      isize      = nnuc            !Use all 26 nuclides.
      jsize      = nrec            !Use all 88 reactions.
C..........SET VALUES TO DEFAULT -- these come from BLOCK DATA at the end of 
C..........                          NUCRATB.f
c      DATA cy0    /.300/           !Default time step limiting constant.
c      DATA ct0    /.030/           !Default time step limiting constant.
c      DATA t9i0   /1.00e+02/       !Default initial temperature.
c      DATA t9f0   /1.00e-02/       !Default final temperature.
c      DATA ytmin0 /1.00e-25/       !Default smallest abundances allowed.
c      DATA inc0   /30/             !Default accumulation increment.
c      DATA c0     /1.00,885.7,3.0/!Default grav constant, neutron lifetime, number neutrinos
c      DATA cosmo0 /0.00/           !Default cosmological constant.
c      DATA xi0    /0.00,0.00,0.00/ !Default neutrino degeneracy parameter.
c      DATA dt0    /1.00e-04/       !Default initial time step.
c      DATA eta0   /6.0e-10/      !Default baryon-to-photon ratio.

      yes_monte = 0                !Default is no monte carlo
      cy    = cy0                  !Time step limiting constant on abundances.
      ct    = ct0                  !Time step limiting constant on temperature.
      t9i   = t9i0                 !Initial temperature.
      t9f   = t9f0                 !Final temperature.
      ytmin = ytmin0               !Smallest abundances allowed.
      inc   = inc0                 !Accumulation increment.
      c(1)  = c0(1)                !Variation of gravitational constant.
      c(2)  = c0(2)                !Neutron lifetime.
      c(3)  = c0(3)                !Number of neutrino species.
      cosmo = cosmo0               !Cosmological constant.
      xi(1) = xi0(1)               !Electron degeneracy parameter.
      xi(2) = xi0(2)               !Muon degeneray parameter.
      xi(3) = xi0(3)               !Tauon degeneracy parameter.
      dt1   = dt0                  !Initial time step.
      eta1  = eta0                 !Baryon-to-photon ratio.



C----------Read in Run Parameters from File
      READ(36,9) yes_monte    !1 for monte carlo, 0 if not 
      READ(36,9) nruns     ! total number of monte carlo runs
      READ(36,10) eta1    !Baryon-to-photon ratio.
      READ(36,10) c(1)    !Variation of gravitational constant.
      READ(36,10) c(2)    !Neutron lifetime.
      READ(36,10) c(3)    !Number of neutrino species.
      READ(36,10) xi(1)   !Electron degeneracy parameter.
      READ(36,10) xi(2)   !Muon degeneray parameter.
      READ(36,10) xi(3)   !Tauon degeneracy parameter.
      READ(36,10) cosmo   !Cosmological constant.

C---------- These parameters are usually not changed

      READ(36,10) cy      !Time step limiting constant on abundances.
      READ(36,10) ct      !Time step limiting constant on temperature.
      READ(36,10) t9i     !Initial temperature.
      READ(36,10) t9f     !Final temperature.
      READ(36,10) ytmin   !Smallest abundances allowed.
      READ(36,10) inc     !Accumulation increment.
      READ(36,10) dt1     !Initial time step.

C---------- echo the input

      IF(nruns.gt.10000) nruns=10000  ! the maximum number I will allow

      WRITE(39,8) 
      WRITE(39,9) yes_monte  ! 1 if monte carlo, 0 if not
      WRITE(39,9) nruns     ! total number of monte carlo runs
      WRITE(39,10) eta1    !Baryon-to-photon ratio.
      WRITE(39,10) c(1)    !Variation of gravitational constant.
      WRITE(39,10) c(2)    !Neutron lifetime.
      WRITE(39,10) c(3)    !Number of neutrino species.
      WRITE(39,10) xi(1)   !Electron degeneracy parameter.
      WRITE(39,10) xi(2)   !Muon degeneray parameter.
      WRITE(39,10) xi(3)   !Tauon degeneracy parameter.
      WRITE(39,10) cosmo   !Cosmological constant.
      WRITE(39,10) cy      !Time step limiting constant on abundances.
      WRITE(39,10) ct      !Time step limiting constant on temperature.
      WRITE(39,10) t9i     !Initial temperature.
      WRITE(39,10) t9f     !Final temperature.
      WRITE(39,10) ytmin   !Smallest abundances allowed.
      WRITE(39,10) inc     !Accumulation increment.
      WRITE(39,10) dt1     !Initial time step.

8     FORMAT('input parameters in nucb.f from bbn_input.f')   
9     FORMAT(i6)   
10    FORMAT(1pe13.6)   


      
C----------Read in Reaction Rate (Thielmann format) Parameters from File

       WRITE(39,18)
18     FORMAT('input REACTION RATE parameters in nucb.f from rates.dat')   

          DO j=1,nrec      
            DO i=1,35       ! up to 21 parameters/reaction allowed
              aa(j,i)=0.0       ! initialize rate parameters
            END DO
          END DO

c      exactly 5 sets of 7 params for nrec reactions  
c      means that we need to add zeroes for the ones that 

         DO k=1,nrec
         
          READ(9,101,END=120)j,n_sets,char_dummy !reac number, num of sets, reaction name
          WRITE(39,101)j,n_sets,char_dummy
 101      FORMAT(i4,i4,a30)

          IF((n_sets.lt.1).or.(n_sets.gt.5)) THEN
             WRITE(39,102)
 102         FORMAT('PROBLEM - too many or too few parameters')
          ENDIF

          READ(9,103,END=120)(aa(j,i),i=1,7)  ! read first set of seven
          WRITE(39,103)(aa(j,i),i=1,7)
 103      FORMAT(4(0pe13.6)/3(0pe13.6))

          IF(n_sets.gt.1) THEN
             READ(9,104,END=120)(aa(j,i),i=8,14)  ! read second set of seven
             WRITE(39,104)(aa(j,i),i=8,14)
 104         FORMAT(4(0pe13.6)/3(0pe13.6))
          ENDIF

          IF(n_sets.gt.2) THEN
             READ(9,105,END=120)(aa(j,i),i=15,21)  ! read third set of seven
             WRITE(39,105)(aa(j,i),i=15,21)
 105         FORMAT(4(0pe13.6)/3(0pe13.6))
          ENDIF

          IF(n_sets.gt.3) THEN
             READ(9,106,END=120)(aa(j,i),i=22,28)  ! read fourth set of seven
             WRITE(39,106)(aa(j,i),i=22,28)
 106         FORMAT(4(0pe13.6)/3(0pe13.6))
          ENDIF

          IF(n_sets.gt.4) THEN
             READ(9,107,END=120)(aa(j,i),i=29,35)  ! read fifth set of seven
             WRITE(39,107)(aa(j,i),i=29,35)
 107         FORMAT(4(0pe13.6)/3(0pe13.6))
          ENDIF


         END DO   ! k = 1, nrec  read loop over reactions

 120     CONTINUE ! reached end of file


C61--------READ IN RATE UNCERTAINTIES -- ONE TIME ONLY - NEW ----------------

         READ(10,125)un1
         READ(10,125)un12
         READ(10,125)un16
         READ(10,125)un17
         READ(10,125)un20
         READ(10,125)un24
         READ(10,125)un26
         READ(10,125)un27
         READ(10,125)un28
         READ(10,125)un29
         READ(10,125)un30
         READ(10,125)un31

         WRITE(39,124)
         WRITE(39,125)un1
         WRITE(39,125)un12
         WRITE(39,125)un16
         WRITE(39,125)un17
         WRITE(39,125)un20
         WRITE(39,125)un24
         WRITE(39,125)un26
         WRITE(39,125)un27
         WRITE(39,125)un28
         WRITE(39,125)un29
         WRITE(39,125)un30
         WRITE(39,125)un31
124      FORMAT(' rate uncertainties read in nucb.f from uncer.dat')
125      FORMAT(e13.6)

C61--------READ IN RATE SWITCH = 1 for Thielmann, 0 for old -----------

         WRITE(39,129)
129      FORMAT(' rate switch values read in nucb.f from switch.dat')
          DO i=1,90   ! note: NOT j=1,90, as I read in the reaction number...
            READ(11,130,END=140)j,switch(j),char_dummy !reac number, switch
            WRITE(39,130)j,switch(j),char_dummy 
130         FORMAT(i4,i4,a30)
          ENDDO

140      CONTINUE


C----------RUN section 

        itime = 2                  !Time = beginning of run section.
        CALL check1                 !Check interface subroutine.

        CALL run                    ! do the calculation
                                    ! NOTE: the monte carlo never returns from here
                                    ! have to call output separately

        itime = 9                  !Time = end of run section.
        CALL check1                 !Check interface subroutine.

        CALL OUTPUT                ! write values to output file
        
        itime = 10                 !Time = end of program.
        CALL check1                 !Check interface subroutine.

        STOP

      END




C========================IDENTIFICATION DIVISION================================

      SUBROUTINE run

C----------LINKAGES.
C     CALLED BY - [program] nuc123
C     CALLS     - [subroutine] driver

C----------REMARKS.
C     Activates computation routine.

C----------PARAMETERS.
      PARAMETER (nrec=88)          !Number of nuclear reactions.
      PARAMETER (lrec=64)          !Total # of nuclear reactions for irun = 2.
      PARAMETER (krec=34)          !Total # of nuclear reactions for irun = 3.
      PARAMETER (nnuc=26)          !Number of nuclides in calculation.
      PARAMETER (lnuc=18)          !Total # of nuclides for irun = 2.
      PARAMETER (knuc=9)           !Total # of nuclides for irun = 3.

C----------COMMON AREAS.
      COMMON /modpr/  g,tau,xnu,c,cosmo,xi           !Model parameters.
      COMMON /varpr/  dt1,eta1                       !Variational parameters.
      COMMON /check2/  itime                          !Computation location.
      COMMON /runopt/ irun,isize,jsize               !Run options.


C==========================DECLARATION DIVISION=================================

C----------MODEL PARAMETERS.
      REAL    eta1                 !Baryon-to-photon ratio.
      REAL    c(3)                 !c(1) is variation of gravitational constant.
     |                             !c(2) is neutron lifetime (sec).
     |                             !c(3) is number of neutrino species.
      REAL    cosmo                !Cosmological constant.
      REAL    xi(3)                !Neutrino degeneracy parameters.

C----------RUN OPTION.
      INTEGER irun                 !Run network size.
      INTEGER isize                !Number of nuclides in computation.
      INTEGER jsize                !Number of reactions in computation.

C----------FLAG AND LABELS.
      INTEGER itime                !Computation location.

C----------EQUIVALENCE VARIABLE.
      REAL    qvary(7)             !Array set equal to c, cosmo, and xi.

C----------EQUIVALENCE STATEMENTS.
      EQUIVALENCE (qvary(1),c(1)), (qvary(4),cosmo), (qvary(5),xi(1))


C===========================PROCEDURE DIVISION==================================

C22--------GO SECTION-----------------------------------------------------------

 220  CONTINUE
        WRITE (39,2200)
 2200   FORMAT (' ','Begin computation run....')

        itime = 3
        CALL check1                 !Call interface subr before computation.
                                    ! NOTE: the monte carlo never returns from here
                                    ! have to call output separately
                                    
        CALL driver                !Do nucleosynthesis computation.

        itime = 8
        CALL check1                 !Call interface subr after computation.

        WRITE (39,2202)
 2202   FORMAT ('Computation completed')

 240  CONTINUE
        RETURN

       END

C========================IDENTIFICATION DIVISION================================

      SUBROUTINE output

C----------LINKAGES.
C     CALLED BY - [program] nuc123
C     CALLS     - none

C----------REMARKS.
C     Outputs computational results either into an output file or onto 
C     the screen

C----------PARAMETERS.
      PARAMETER (ir=4)             !Input unit number.
      PARAMETER (iw=6)             !Output unit number.
      PARAMETER (nnuc=26)          !Number of nuclides in calculation.
      PARAMETER (itmax=40)         !Maximum # of line to be printed.

C----------COMMON AREAS.
      COMMON /compr/  cy,ct,t9i,t9f,ytmin,inc        !Computation parameters.
      COMMON /modpr/  g,tau,xnu,c,cosmo,xi           !Model parameters.
      COMMON /flags/  ltime,is,ip,it,mbad            !Flags, counters.
      COMMON /outdat/ xout,thmout,t9out,tout,dtout,  !Output data.
     |                etaout,hubout
      COMMON /outopt/ nout,outfile                   !Output option.


C==========================DECLARATION DIVISION=================================

C----------COMPUTATION SETTINGS.
      REAL    cy                   !Time step limiting constant on abundances.
      REAL    ct                   !Time step limiting constant on temperature.
      REAL    t9i                  !Initial temperature (in 10**9 K).
      REAL    t9f                  !Final temperature (in 10**9 K).
      REAL    ytmin                !Smallest abundances allowed.

C----------EARLY UNIVERSE MODEL PARAMETERS.
      REAL    c(3)                 !c(1) is variation of gravitational constant.
     |                             !c(2) is neutron lifetime (sec).
     |                             !c(3) is number of neutrino species.
      REAL    cosmo                !Cosmological constant.
      REAL    xi(3)                !Neutrino degeneracy parameters.

C----------COUNTER.
      INTEGER it                   !# times accumulated in output buffer.

C----------OUTPUT ARRAYS.
      REAL    xout(itmax,nnuc)     !Nuclide mass fractions.
      REAL    thmout(itmax,6)      !Thermodynamic variables.
      REAL    t9out(itmax)         !Temperature (in units of 10**9 K).
      REAL    tout(itmax)          !Time.
      REAL    dtout(itmax)         !Time step.
      REAL    etaout(itmax)        !Baryon-to-photon ratio.
      REAL    hubout(itmax)        !Expansion rate.

C----------OUTPUT FILE STATUS.
      INTEGER nout                 !Number of output requests.
      LOGICAL outfile              !Indicates if output file used.

C----------USER INTERACTION VARIABLES.
      INTEGER inum                 !Selection number.


C===========================PROCEDURE DIVISION==================================

          WRITE (39,2000)
 2000     FORMAT (54x,'NUCLIDE ABUNDANCE YIELDS',/,
     |            54x,'------- --------- ------',//)

        WRITE (39,2002) cy,ct,t9i,t9f,ytmin
 2002   FORMAT (' Computational parameters:',/,
     |          '   cy = ',f5.3,'/  ct = ',f5.3,
     |          '/  initial temp = ',1pe8.2,
     |          '/  final temp = ',1pe8.2,
     |          '/  smallest abundances allowed = ',1pe8.2)

        WRITE (39,2004) c(1),c(2),c(3),cosmo,xi(1),xi(2),xi(3)
 2004   FORMAT (' Model parameters:',/,
     |          '   g = ',f5.2,'/  tau = ',f6.2,
     |          '/  # nu = ',f5.2,'/  lambda = ',1pe10.3,
     |          '/  xi-e = ',e10.3,'/  xi-m = ',e10.3,
     |          '/  xi-t = ',e10.3,/)

C..........PRINT HEADINGS, ABUNDANCES FOR NEUTRON TO LI8.         
        WRITE (39,2006)
 2006   FORMAT (4x,'Temp',8x,'N/H',10x,'P',10x,'D/H',9x,'T/H',8x,
     |          'He3/H',8x,'He4',8x,'Li6/H',7x,'Li7/H',7x,
     |          'Be7/H',6x,'Li8/H&up',/,132('-'))
        DO j = 1,it
          WRITE (39,2008) t9out(j),(xout(j,i),i=1,10)
 2008     FORMAT (1pe10.3,1p10e12.3)
        END DO

C..........PRINT THERMODYNAMIC QUANTITIES.         
        WRITE (39,2010)
 2010   FORMAT (' ',/,4x,'Temp',9x,'T',10x,'rhog',8x,'rhoe',7x,
     |              'rhone',8x,'rhob',8x,'phie',9x,'dt',9x,
     |              'eta',10x,'H',/,132('-')) 
        DO j = 1,it
          WRITE (39,2012) t9out(j),tout(j),(thmout(j,i),i=1,5),dtout(j),
     |                   etaout(j),hubout(j)
 2012     FORMAT (1pe10.3,9e12.3)
        END DO

        WRITE (39,2014)
 2014   FORMAT (///)


        RETURN

      END
