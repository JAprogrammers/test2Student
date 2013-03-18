C               Podprogrami: WD, APHB
C
C               Podprograma  WD
C               Poprawki ¬. dekemwri¨ 1991, ¬ maj 1992
C               Poprawki ¬. dekemwri¨ 1993
C               Poprawki ¬. januar ¬ mart 1996
C               Poprawki ¬. april 2000
C                           juli 2000
C               english, august 2000
C                       02.08
C               Variant 7 2010     
C               Variant 8 2012
C			Variant 8.1 2 2013
$DEBUG    
        SUBROUTINE WD
        REAL KFE1,KFE2,MWD(81),MWDP(20),P1501,P1502
        CHARACTER*4 TEXT(80),TYP*20,JQ*1,INFILE*14,OUFILE*14,
     *              INF*8,OUF*8,TYPSS*7,TYPSR*7,TEXTP(18),infst*8,
     *              typp*7
        INTEGER YE,MO,DA,HO,MI,SE,SS,MASN(18),NOMM,ZAG,nist
	  INTEGER LEN1, LEN2, NOMAG, NOMAM
        LOGICAL LEXI,LEXO,PEC
        INTEGER ZI(10),KREG,NOMW,NOMZ,BROIZ,NOM,NOMN,NOMK,NOMSY,NOMT
        REAL CISLO
        CHARACTER*1 ZIF(10),SPAZ,TOC,SYM,SAPIS*80,TAB
	  CHARACTER*7 IME
        COMMON /CWD/ MWD
     *         /KOEF/ AST(80,2),ARO(80,2)
        COMMON /CPEC/ PEC,NOMM
        COMMON /CINT/ IWSK,IWRK,ISSN,IWSN,IWSNWI,IWSNSL,IWSNRA,IWSNFZ
      DATA  TEXT / 'P2n ','p   ','U1n ','ssn ','D1  ','Da1 ','Di2 ',
     *      'l1  ','l2  ','del ','Z1  ','nsk ','bs1 ','hs1 ','b11 ',
     *      'b1  ','h`k1','h11 ','hkl1','bkl1','Z2  ','nrk ','bs2 ',
     *      'hs2 ','b21 ','b2  ','h`k2','h21 ','bs3 ','hs3 ','b3  ',
     *      'b31 ','h3  ','hm  ','bsk ','bkp ','akp ','tsn ','qd1 ',
     *      'qc1 ','y1  ','a1  ','uk1 ','nel1','dpr1','dis1','deli',
     *      'ks1 ','liz1','pmeh','ro12','ro22','tprh','tnos','tnor',
     *      'tkss','tksr','tsta','Upon','f1  ','cfez','cfea','kfe1',
     *      'p151','kfe2','p152','uk11','uk12','uk13','uk14','bpr1',
     *      'apr1','Spr1','bis1','ais1','n1w ','n1sh','bb2 ','bb21',
     *      'hb2 '/
        DATA ZIF/'0','1','2','3','4','5','6','7','8','9'/,
     *       SPAZ/' '/, TOC/'.'/
    1 FORMAT($,' Enter name of input file(press ENTER, if you want "',
     *         A8,'"):')    
    2 format(A8)
    3 format('   There is not found file with this name :',11x,a14)
    4 format($,' Do you want to repeat           ?(Y/N): ')
    5 format(a1)
    6 format('    Input file   ',32x,': ',a14)
    7 format(/,a20)
    8 format(//,10f8.3)
    9 format($,' Enter name of output file(press ENTER, if you want "',
     *        a8,'"): ')
   10 FORMAT('  File with name ',34X,': ',A14,' exists !')
   11 FORMAT($,' Do you want to delete? (Y/N): Y ')
   12 FORMAT(2X,'The old file',32X,': ',A14,' is deleted')
   13 FORMAT(2X,'New output file ',32X,': ',A14)
   17 FORMAT(5X,'File results      :',3X,A14,
     *       10X,'time',5X,I2,':',I2)
   19 FORMAT(//,A80)
   20 FORMAT(2X,'     Program works. Please wait.')
   21 FORMAT(2X,'steel stator :',A7,' steel rotor ',A7)
   22 FORMAT(21X,'PROGRAM ASM3 , Variant 8 , 2013',/,5X,
     *      'ELECTROMAGNETIC CALCULATIONS OF ASYNCHRONOUS MOTOR',
     *      ', three-phase',
     */,5X,'low voltage, simple cage, double cage, cast-aluminium, bar',
     */,5X,'owner:ELPROM-      '/,5x,
     *'authors:D.Jetchev,jetch@tu-sofia.bg, G.Todorov,gtto@tu-sofia.bg')
   26 FORMAT(' File names are up to 8 symbols  ',
     *        'with default extensions       :',
     */,'   file for input data  : .DAT ; file for results  : .LIS ')
   23 FORMAT(5X,'File input data   :   ',A14,
     *       10X,'date',5X,I2,'-',I2,'-',I4)
   28 FORMAT(  26X,A20)
   29 FORMAT(/,26X,'INPUT DATA   :')
   30 FORMAT(' incorrect : nss = ',I2,' rectang. conductor ', 3F8.3)
   31 FORMAT(' incorrect : nss = ',I2,' round conductor  ', 2F8.3)
   32 FORMAT(' incorrect : nss = ',I2,'n1w,bis1,b1 = ',I3, 2F8.3,/,
     *       '                      ','n1sh,ais1,h1 = ',I3, 2F8.3)
   36 FORMAT($,' Enter steel type for rotor  (default is ',A7,'): ')
   37 FORMAT('   Rotor   steel type is ',A7)
   38 FORMAT(A7)
C
      CALL GETDAT (YE,MO,DA)
      WRITE( *,22)
      WRITE( *,26)
C
      CALL FOUT (infst)
C 

      DO 942 I=1,8
        IF(INFst(I:I) .EQ. ' ') GOTO 943
  942 CONTINUE
      I=9
  943 NIST=I-1
        INFILE=' '
        OUFILE=' '
   40   IF(INFILE .EQ. ' ') THEN
   41     WRITE(*,1)infst
          READ(*,2) INF
          DO 42 I=1,8
            IF(INF(I:I) .EQ. ' ') GOTO 43
   42     CONTINUE
          I=9
   43     NI=I-1
          IF(NI .EQ. 0) then
            infile(1:nist)=infst(1:nist)
            INFILE(1+NIst:4+NIst)='.DAT'
            NI = nist
          else
            INFILE(1:NI  )=INF(1:NI)
            INFILE(1+NI:4+NI)='.DAT'
          endif
          INQUIRE(FILE=INFILE,EXIST=LEXI,IOSTAT=IOS)
          IF(.NOT. LEXI) THEN
            WRITE(*,3)INFILE
            INFILE=' '
            WRITE(*, 4)

            READ(*,5) JQ
            IF(JQ .EQ. 'N' .OR. JQ .EQ. 'n') STOP
            GOTO 40
          END IF
        END IF
        WRITE (*,6) INFILE
        OPEN  (1,FILE=INFILE,STATUS='OLD')
        infst(1: 8)='        '
        write(4,2  ,REC=10,ERR=120)infst
        infst(1:ni)=infile(1:ni)
        write(4,2  ,REC=10,ERR=120)infst
        READ  (1,7,END=120) TYP
C
        NOMW = 0
C                                tabulazija prez 8
        TAB  = CHAR(9)

C	     write(*,*)'  fajl vhodni danni '
        DO 300 NOMZ = 1,8
          READ(1,19,END=291) SAPIS
          IF( NOMZ .NE. 8) THEN
            BROIZ = 10
          ELSE
            BROIZ = 10
          ENDIF
C                               Zagubeni pozicii zaradi tabulazija
          ZAG = 0
          DO 290 NOM = 1,BROIZ
            NOMN = 8*(NOM - 1) + 1 - ZAG
            NOMK = NOMN + 7
            KREG = 0
            DO 44 J = 1, 8
              ZI(J)=0
  44        CONTINUE
            DO 280 I = 1, 8
              NOMSY = NOMN -1 + I
              SYM   = SAPIS(NOMSY:NOMSY)
              IF ( SYM .EQ. SPAZ ) THEN
                IF ( KREG .EQ. 1) THEN
                  NOMT = I
                  KREG = 3
                ELSE IF(KREG .EQ. 2) THEN
                  KREG = 3
                ENDIF
                GOTO 280
              ENDIF
              IF ( SYM .EQ. TAB) THEN
                 ZAG = ZAG + 8 - I
                 IF (KREG .EQ. 0 .OR. KREG .EQ. 1) NOMT = I
                 GOTO 45
              ENDIF
              DO 270 J = 1, 10
                IF ( SYM .EQ. ZIF(J)) THEN
                  ZI(I) = J - 1
                  IF (KREG .EQ. 0) KREG = 1
                  IF (KREG .EQ. 3) THEN
                    WRITE(*,27) NOMZ
   27       FORMAT(2X,'Incorrect record of 2 values in line number',I2)
                    STOP
                  ENDIF
                  GOTO 280
                ENDIF
  270         CONTINUE
              IF( SYM .EQ. TOC) THEN
                IF (KREG .EQ. 0 .OR. KREG .EQ. 1) THEN
                  NOMT = I
                  KREG = 2
                ELSE
                  WRITE(*,271) NOMZ
  271             FORMAT(2X,'Error: 2 decemal points in one field',
     *                   2X,'in line number ',I2)
                  STOP
                ENDIF
              ELSE
                WRITE(*,272) NOMZ
  272           FORMAT(2X,'Incorrect symbol in line number',I2)
                STOP
              ENDIF
  280       CONTINUE
            IF ( KREG .EQ. 0 .OR. KREG .EQ. 1) NOMT = 9
   45       CISLO = 0.
            IF (NOMT .GT. 1) THEN
              DO 281 I = 1,NOMT-1
                CISLO = CISLO + ZI(I)*10.**(NOMT-1-I)
  281         CONTINUE
            ENDIF
            IF (NOMT .LT. 8) THEN
              DO 282 I = NOMT+1, 8
                CISLO = CISLO + ZI(I)*10.**(NOMT-I)
  282         CONTINUE
            ENDIF
            NOMW = NOMW + 1
            MWD(NOMW) = CISLO
  290     CONTINUE
          GOTO 300
C					ako njama red 7. (stojnosti 71,72,...80)
  291     DO 292 I=71,80
            MWD(I)=0.
  292     CONTINUE
  300   CONTINUE
C        write(*,*) ' kraj fajl vhodni danni'
        DO 102 I=1,78
          IF ( MWD(I) .LT. 0.) THEN
            WRITE(*,101) I , MWD(I)
  101       FORMAT(' Incorrect input data No.:',I4,2X,F8.3)
            STOP 101
          ENDIF
  102   CONTINUE
        CLOSE(1)
C
        CONTINUE
C                                      
	   TYPSS = ' '
C                  Izwikwane APHB za stator
        CALL APHB(TYPSS,KFE1,P1501,AST,1)
        READ(4,38,REC=12,ERR=121)TYPSR
        WRITE(*,36) TYPSR
        TYPP=' '
        READ( *,38) TYPP
        IF(TYPP .NE. ' ') TYPSR=TYPP
	  WRITE(*,37) TYPSR
C				Ako statorna i rotorna lamarina sa rawni

C                          samo golemi bukwi
        LEN1=LEN(TYPSS)
	  LEN2=LEN(TYPSR)
	  NOMAG=ICHAR('A')
	  NOMAM=ICHAR('a')
        DO 236 J=1,LEN1
          NOMT=ICHAR(TYPSS(J:J))
          IF (NOMT .GE. NOMAM .AND. NOMT .LT. NOMAM+26) THEN
              NOMT = NOMAG + NOMT - NOMAM
	        TYPSS(J:J)=CHAR(NOMT)
	    ENDIF
  236   CONTINUE             
        DO 237 J=1,LEN2
          NOMT=ICHAR(TYPSR(J:J))
          IF (NOMT .GE. NOMAM .AND. NOMT .LT. NOMAM+26) THEN
              NOMT = NOMAG + NOMT - NOMAM
	        TYPSR(J:J)=CHAR(NOMT)
	    ENDIF
  237   CONTINUE   
        IF (TYPSR .EQ. TYPSS) THEN
          TYPSR = TYPSS
          KFE2  = KFE1
          P1502 = P1501
          DO 74 I=1,80
            DO 74 J=1,2
              ARO(I,J) = AST(I,J)
   74     CONTINUE
          WRITE(4,302,REC=12,ERR= 121)TYPSR
  302     FORMAT(A7)
        ELSE
C				Izwikwane APHB za rotor
          CALL APHB(TYPSR,KFE2,P1502,ARO,2)
        ENDIF
C                                      izbor za izhoden fajl
        IF(OUFILE .EQ. ' ') THEN
   75     OUFILE(1:NI) = INFILE(1:NI)
          WRITE ( *, 9) OUFILE
          READ  ( *, 2) OUF
          DO 80 I=1,8
            IF(OUF(I:I) .EQ. ' ') GOTO 90
   80     CONTINUE
          I=9
   90     NO=I-1
          IF ( NO .EQ. 0) THEN
            OUF(1:NI) = OUFILE(1:NI)
            NO  = NI
          ELSE
            OUFILE=' '
            OUFILE(1:NO  )=OUF(1:NO)
          ENDIF
          OUFILE(1+NO:4+NO)='.LIS'
          INQUIRE(FILE=OUFILE,EXIST=LEXO,IOSTAT=IOS)
  200          FORMAT(A12)
          IF(.NOT. LEXO) THEN
            WRITE(*,13)OUFILE
          ELSE
C                         Takav fajl ima      
            WRITE(*,10)OUFILE
            WRITE(*,11)
            READ(*,5) JQ
            IF(JQ .EQ. 'N' .OR. JQ .EQ. 'n') THEN
              WRITE(*, 4)
              READ(*,5) JQ
              IF(JQ .EQ. 'N' .OR. JQ .EQ. 'n') STOP
              OUFILE=' '
              GOTO 75
            ELSE
              OPEN(2,FILE=OUFILE,STATUS='OLD')
              CLOSE(2,STATUS='DELETE')
              WRITE(*,12)OUFILE
              WRITE(*,13)OUFILE
            END IF
          END IF
        END IF
        CALL GETDAT (YE,MO,DA)
        CALL GETTIM (HO,MI,SE,SS)
        CONTINUE
C       CALL TIME(10,TSTRP)
C       OUFW = OUFILE
        OPEN (2,FILE=OUFILE,STATUS='NEW')
        WRITE (2,22)
        WRITE (2,23) INFILE, DA, MO, YE
        WRITE (2,17) OUFILE, HO, MI
        WRITE (2,29)
        WRITE (2,28) TYP
  711 FORMAT(2X,4(A4,'=',F8.2,3X),A4,'=',F8.2)
  712 FORMAT(2X,A4,'=',F8.4,3X,A4,'=',F8.4,3X,A4,'=',F8.2,3X,
     *          A4,'=',F8.2,3X,A4,'=',F8.2)
  713 FORMAT(2X,A4,'=',F8.2,3X,A4,'=',F8.0,3X,A4,'=',F8.2,3X,
     *          A4,'=',F8.0,3X,A4,'=',F8.2)
  714 FORMAT(2X,A4,'=',F8.0,3X,A4,'=',F8.0,3X,A4,'=',F8.2,3X,
     *          A4,'=',F8.2,3X,A4,'=',F8.2)
  715 FORMAT(2X,A4,'=',F8.2,3X,A4,'=',F8.2,3X,A4,'=',F8.0,3X,
     *          A4,'=',F8.0,3X,A4,'=',F8.0)
  716 FORMAT(2X,A4,'=',F8.0,3X,A4,'=',F8.0,3X,A4,'=',F8.0,3X,
     *          A4,'=',F8.0,3X,A4,'=',F8.2)
  717 FORMAT(2X,A4,'=',F8.2,3X,A4,'=',F8.0,3X,A4,'=',F8.0,3X,
     *          A4,'=',F8.0,3X,A4,'=',F8.0)
  718 FORMAT(2X,A4,'=',F8.0,3X,A4,'=',F8.0,3X,A4,'=',F8.3,3X,
     *          A4,'=',F8.3,3X,A4,'=',F8.3)
  719 FORMAT(2X,A4,'=',F8.3,3X,A4,'=',F8.3,3X,A4,'=',F8.3,3X,
     *          A4,'=',F8.3,3X,A4,'=',F8.3)
C 720 FORMAT(2X,A4,'=',F8.3,3X,A4,'=',F8.3,3X,A4,'=',F8.3)
        DO 110 J=1,14
          IN=1+5*(J-1)
          IK=IN+4
      goto(813,811,814,811,814,811,811,815,816,811,812,811,811,817),j
  811       WRITE(2,711) (TEXT(I),MWD(I),I=IN,IK)
            goto 110
  812       WRITE(2,712) (TEXT(I),MWD(I),I=IN,IK)
            goto 110
  813       WRITE(2,713) (TEXT(I),MWD(I),I=IN,IK)
            goto 110
  814       WRITE(2,714) (TEXT(I),MWD(I),I=IN,IK)
            goto 110
  815       WRITE(2,715) (TEXT(I),MWD(I),I=IN,IK)
            goto 110
  816       WRITE(2,716) (TEXT(I),MWD(I),I=IN,IK)
            goto 110
  817       WRITE(2,717) (TEXT(I),MWD(I),I=IN,IK)
  110   CONTINUE
        if(mwd(71).gt.0. .or. mwd(78).gt.0.) then
          write(2,719)(text(i),mwd(i),i=71,75)
          write(2,718)(text(i),mwd(i),i=76,80)
        endif
        write(2,21) typss,typsr
        IWSN = IFIX(MWD(38))
        if (iwsn .eq. 0) then
          iwsnko = 1111
        else if (iwsn .lt. 10) then
               iwsnko = 1000*iwsn + 111
             else if (iwsn .lt. 100) then
                    iwsnko = 100*iwsn + 11
                  else if (iwsn .lt. 1000) then
                         iwsnko = 10*iwsn + 1
                       else
                         iwsnko = iwsn
        endif
        iwsnwi = ifix( iwsnko/1000.0)
        iwsnsl = ifix((iwsnko-iwsnwi*1000)/100.)
        iwsnra = ifix((iwsnko-iwsnwi*1000-iwsnsl*100)/10.)
        iwsnfz = ifix((iwsnko-iwsnwi*1000-iwsnsl*100-iwsnra*10)/1.)
C
        NOMM = 0
        ISSN = IFIX(MWD( 4))
        IWSK = IFIX(MWD(12))
        IWRK = IFIX(MWD(22))
        if( iwsn .ne. iwsnko) then
          NOMM = NOMM +1
          MASN(NOMM) = 38
          MWD(38) = iwsnko+0.0001
        ENDIF
        IF ((ISSN .LT. 1 .OR. ISSN .GT. 2) .or.
     *      (IWSK .LT. 1 .OR. IWSK .GT. 6) .or.
     *      (IWRK .LT. 1 .OR. IWRK .GT. 8) .or.
     *      (IWSNwi .LT. 1 .OR. IWSNwi .GT. 4)) then
          write(2,*) ' Incorrect input data swc,nss,nrs,swt'
          write(2,*) issn,iwsk,iwrk,iwsnwi
          stop 600
        endif
        P   = MWD(2)
        Z1  = MWD(11)
        QD1 = MWD(39)
        QC1 = MWD(40)
        Q1  = QD1/QC1
        if ( (iwsnfz.eq.1 .and. ifix(Z1/(6.*q1)) .ne. ifix(p))
     *  .or. (iwsnfz.eq.2 .and. ifix(Z1/(3.*q1)) .ne. ifix(p))) then
          WRITE (*,24) QD1,QC1,Z1,P
          WRITE (2,24) QD1,QC1,Z1,P
   24     FORMAT(5X,'incorrect     ',/,
     *     5X,'QD1 =',F2.0,'  QC1 =',F2.0,'  Z1 =',F4.0,'  P =',F2.0)
                 STOP
        ENDIF
        IF(IWSK .EQ. 5 .OR. IWSK .EQ. 6) THEN
          IF(MWD(71).EQ.0 .OR. MWD(72).EQ.0)THEN
            WRITE(*,30) IWSK,MWD(71),MWD(72),MWD(73)
            WRITE(2,30) IWSK,MWD(71),MWD(72),MWD(73)
            STOP
          ELSE
            IF(MWD(45).GT.0 .OR. MWD(46) .GT. 0) THEN
               WRITE(2,31) IWSK,MWD(45),MWD(46)
               WRITE(*,31) IWSK,MWD(45),MWD(46)
            ENDIF
              B1  =MWD(16)
              HK1P=MWD(17)
              DEIS=MWD(47)
              BIS1=MWD(74)
              AIS1=MWD(75)
              N1SH=MWD(76)
              N1W =MWD(77)
             IF(N1SH*BIS1+2.*DEIS.GT.B1 .OR.
     *          N1W*AIS1+2.*DEIS.GT.HK1P) THEN
                WRITE(*,32) IWSK,N1SH,BIS1,B1,N1W,AIS1,HK1P
                WRITE(2,32) IWSK,N1SH,BIS1,B1,N1W,AIS1,HK1P
                STOP
             ENDIF
          ENDIF
        ENDIF
        GOTO (201,202,201,202,202,202), IWSK
  201      IF( MWD(19) .GT. MWD(15)) THEN
              NOMM = NOMM +1
              MASN(NOMM) = 19
              MWD(19) = MWD(15)
           ENDIF
           GOTO 202
  202   CONTINUE
        IF( MWD(47) .LE. 0.) THEN
          NOMM = NOMM +1
          MASN(NOMM) = 47
          MWD(47) = 0.31
        ENDIF
        IF( MWD(48) .LE. 0.) THEN
          NOMM = NOMM +1
          MASN(NOMM) = 48
          MWD(48) = 1.1
        ENDIF
        IF( MWD(49) .LE. 0.) THEN
          NOMM = NOMM +1
          MASN(NOMM) = 49
          MWD(49) = 10.
        ENDIF
        IF( MWD(51) .LE. 0.) THEN
          NOMM = NOMM +1
          MASN(NOMM) = 51
          MWD(51) = 0.0175
        ENDIF
        IF( MWD(52) .LE. 0.) THEN
          NOMM = NOMM +1
          MASN(NOMM) = 52
          MWD(52) = 0.0385
        ENDIF
        IF( MWD(58) .LE. 0.) THEN
          NOMM = NOMM +1
          MASN(NOMM) = 58
          MWD(58) = 75.
        ENDIF
        IF( MWD(53) .LE. 0.) THEN
          NOMM = NOMM +1
          MASN(NOMM) = 53
          MWD(53) = MWD(58)
        ENDIF
        IF( MWD(54) .LE. 0.) THEN
          NOMM = NOMM +1
          MASN(NOMM) = 54
          MWD(54) = MWD(58)
        ENDIF
        IF( MWD(55) .LE. 0.) THEN
          NOMM = NOMM +1
          MASN(NOMM) = 55
          MWD(55) = MWD(58)
        ENDIF
        IF( MWD(56) .LE. 0.) THEN
          NOMM = NOMM +1
          MASN(NOMM) = 56
          MWD(56) = MWD(58)
        ENDIF
        IF( MWD(57) .LE. 0.) THEN
          NOMM = NOMM +1
          MASN(NOMM) = 57
          MWD(57) = MWD(58)
        ENDIF
        IF( MWD(50) .LE. 0.) THEN
          NOMM = NOMM +1
          MASN(NOMM) = 50
          MWD(50) = 0.85*(3.0/MWD(2))**2*(1.0e-4*MWD(5)*MWD(8))**3
        ENDIF
        IF( MWD(60) .LE. 0.) THEN
          NOMM = NOMM +1
          MASN(NOMM) = 60
          MWD(60) = 50.
        ENDIF
        IF( MWD(61) .LE. 0.) THEN
          NOMM = NOMM +1
          MASN(NOMM) = 61
          MWD(61) = 1.9
        ENDIF
        IF( MWD(62) .LE. 0.) THEN
          NOMM = NOMM +1
          MASN(NOMM) = 62
          MWD(62) = 1.6
        ENDIF
        IF( MWD(63) .LE. 0.) THEN
          NOMM = NOMM + 1
          MASN(NOMM) = 63
          MWD(63) = KFE1
        ENDIF
        IF( MWD(64) .LE. 0.) THEN
          NOMM = NOMM + 1
          MASN(NOMM) = 64
          MWD(64) = P1501
        ENDIF
        IF( MWD(65) .LE. 0.) THEN
          NOMM = NOMM + 1
          MASN(NOMM) = 65
          MWD(65) = KFE2
        ENDIF
        IF( MWD(66) .LE. 0.) THEN
          NOMM = NOMM + 1
          MASN(NOMM) = 66
          MWD(66) = P1502
        ENDIF
        IF(IWSK .EQ. 5 .OR. IWSK .EQ. 6) THEN
          IF(MWD(45).GT.0) THEN
            NOMM = NOMM + 1
            MASN(NOMM)=45
            MWD(45)=0.
          ENDIF
          IF(MWD(46).GT.0) THEN
            NOMM = NOMM + 1
            MASN(NOMM)=46
            MWD(46)=0.
          ENDIF
          IF(MWD(73) .EQ. 0)THEN
            NOMM = NOMM + 1
            MASN(NOMM) = 73
            MWD(73)= MWD(71)*MWD(72)
          ENDIF
        ENDIF
        IF(NOMM .GT. 0) THEN
  205     FORMAT(5X,'changed input data')
          WRITE(2,205)
  215     FORMAT(2X,4(A4,'=',F8.2,3X),A4,'=',F8.2)
          iii = 0
          DO 216 I = 1, NOMM
            IND = MASN(I)
            if (ind .eq. 38) then
              write(2,218) Text(38),mwd(38)
  218         format(2x,a4,'=',f8.0)
            else
              iii  = iii + 1
              TEXTP(Iii) = TEXT(IND)
              MWDP (Iii) = MWD (IND)
            endif
  216     CONTINUE
          nomm = iii
          if (nomm .gt. 0) then
            DO 220 J = 1, 4
              NOMMN = 1+5*(J-1)
              NOMMK = NOMMN + 4
              IF(NOMMK .GT. NOMM) THEN
                NOMMK = NOMM
                WRITE(2,215) (TEXTP(I),MWDP(I), I= NOMMN,NOMMK)
                GOTO 120
              ELSE
                WRITE(2,215) (TEXTP(I),MWDP(I), I= NOMMN,NOMMK)
              ENDIF
  220       CONTINUE
          endif
        ENDIF
  120   CONTINUE
        WRITE( *,20)
C                       program works!
  501   FORMAT(I4,I2,I2)
  502   FORMAT(A7)
        CLOSE(14, STATUS='DELETE')
	  OPEN(14,FILE='OUT.INF',STATUS='NEW',ACCESS='sequential',
     *	  FORM='formatted')
        DO 510 I=1,9
	    read(4,501,REC=I) YE,MO,DA
	    write(14,501)     YE,MO,DA
  510   CONTINUE
          read(4,502,REC=10) IME
	    write(14,502)      IME
	    read(4,502,REC=11) IME
	    write(14,502)      IME
          read(4,502,REC=12) IME
	    write(14,502)      IME
        CLOSE(14)
	  CLOSE( 4)  
      RETURN
  121 WRITE(*,401)
  401 FORMAT(' ERROR')
      STOP 999
      END
C
C               Podprograma  APHB
C                 chete stomani ot fajl ELST.INF
C                 do 20 broja
      SUBROUTINE APHB (TYPS,KFE,P150,A,IDE)
        INTEGER I,J,NOMAG,NOMAM,NOMT,N,M,NREC,NRE,JN
        REAL H(80),P150,KFE,A(80,2),hh(8)
        CHARACTER*7 TYPS,TYPM(20),TYPMT,JQ*1,SYM*1,TYPST,TYPRO,TYP,typa
	  LOGICAL lexi
	  integer nn,ii,nnz,iii,nn1
	  real kf,p1
    1   FORMAT(I7)
    2   FORMAT(2X,'New file ! ')
    3   FORMAT(2X,'Error at read/write from file    !')
    4   FORMAT(A7)
    5   FORMAT(' There is information about following steel types ',
     *        '(<= 7 symbols) :',/,(10(1X,A7)))
    6   FORMAT(' Steel type       (default is ',A7,')        : ')
    7   FORMAT($,' New steel type  ? (Y/N): Y')
    8   FORMAT(A1)
    9   FORMAT(2X,'Coefficient KFE : ',$)
   10   FORMAT(2X,'Coefficient P150: ',$)
   11   FORMAT(F7.2)
   12   FORMAT(2X,'Intensities            H, A/cm',/,
     *         2X,'80 values for inductions B=0.025 - 2.000 T',/,
     *         2X,'FORMAT F8.3',/)
   13   FORMAT(2X,I2,' = ')
   14   FORMAT(2X,'Intensity              ',/,(10F8.2))
   15   FORMAT($,' Correct entry  ? (Y/N): Y')
   16   FORMAT($,' New record in file ELST.INF ? (Y/N): N')
   17   FORMAT(2X,'Thete are 20 steel types in the file')
   18   FORMAT(2X,'Steel   ',A7,' is written in the file')
   19   FORMAT(2X,'Repairing in array H ',/,
     *         2X,'number( 1 - 80) - FORMAT I2',/,
     *         2X,'number= 0 - end ',/)
   20   FORMAT(I2)
   21   FORMAT(2X,'? ',$)
   22   FORMAT(2X,'Incorrect data in file    ELST.INF .')
   23   FORMAT(10F8.2)
   24   FORMAT(2X,'Incorrect data for the steel  !')
   25   FORMAT(8F7.2)
   29   FORMAT($,' Enter steel type for stator (default is ',A7,'): ')
   30   FORMAT($,' Enter steel type for rotor  (default is ',A7,'): ')
   56   FORMAT('   Stator steel type is ',A7)
   57   FORMAT('   Rotor steel type is  ',A7)
        if(IDE .EQ.1 ) then
           OPEN(13,FILE=  'ELST.INF',STATUS='OLD',ACCESS='SEQUENTIAL',
     *         FORM='FORMATTED')
           INQUIRE(FILE='elstD.INF',EXIST=LEXI,IOSTAT=IOS)
           if(lexi) then
               OPEN(3,FILE='elstD.inf',STATUS='OLD',RECL=56,
     *               ACCESS='direct',FORM='FORMATTED') 
	         CLOSE(3,STATUS="delete")
	     endif
	     OPEN(3,FILE='elstD.inf',STATUS='NEW',RECL=56,
     *         ACCESS='DIRECT', FORM='FORMATTED')
	   endif
C
 111    format(i7)
 112    format(' n=',i7)
 113    format(2f7.2)
 114    format(' kfe=',f7.2,' p150=',f7.2)
 115    format(a7)
 116    format(a7)   
 117    format(1x,2f7.2)   
 118    format(8f7.2)
 119    format(1x,a7)
        if(IDE .EQ. 1) then
	   read(13,111)   nn
	   write(3,111,REC=1)   nn     
         do 120 ii=1,nn
	    nnz=(ii-1)*12+2
          read(13,115)typa   
	    write(3,116,REC=nnz)   typa    
          read(13,113)   kf,p1
	    write(3,113,REC=nnz+1)   kf,p1
          do 121 nn1=1,10   
              read(13,118)(hh(iii),iii=1,8)
	        write( 3,118,REC=nnz+2+nn1-1) (hh(iii),iii=1,8)
 121      continue
 120     continue
        endif
        TYPRO=' '
        N = 0
        NOMAG = ICHAR('A')
        NOMAM = ICHAR('a')
        READ(3,1,REC=1,ERR=31) N
        IF (N .GT. 0 ) GOTO 33
C                                 njama stomani vav fajla 
   31   WRITE(*,2)
        WRITE(*,6)
        READ (*,4) TYPS
        GOTO 38
C                     greshka pri chetene na tipowete stomani
   32   WRITE(*,3)
        CLOSE (3)
        STOP  333
C                                chetene na tipowete stomani
   33   DO 34 I = 1, N
          NREC  = 2 + 12*(I-1)
          READ(3,4,REC=NREC,ERR=32) TYPM(I)
   34   CONTINUE
   35   CONTINUE
        IF (TYPS .EQ. ' ') THEN
          WRITE(*,5) (TYPM(I),I=1,N)
          IF (IDE .EQ. 1) THEN
            READ(4,4,REC=11,ERR=101)TYPST
            TYP=TYPST
            WRITE(*,29)TYPST
            READ(*,4)TYPST
            IF(TYPST .EQ. ' ')TYPST=TYP
            TYPS=TYPST
	      WRITE(*,56) TYPST
          ELSE
            READ(4,4,REC=12,ERR=101)TYPRO
            TYP=TYPRO
            WRITE(*,30)TYPRO
            READ(*,4)TYPRO
            IF(TYPRO .EQ. ' ')TYPRO=TYP
            TYPS=TYPRO
	      WRITE(*,57)TYPRO
          ENDIF
        ENDIF
        IF(TYPRO .EQ. ' ') TYPRO=TYPS
        DO 37 I = 1,N
          TYPMT = TYPM(I)
          DO 36 J = 1, 7
            SYM = TYPS(J:J)
            NOMT= ICHAR(SYM)
            IF (NOMT .GE. NOMAM .AND. NOMT .LT. NOMAM+26) THEN
              NOMT = NOMAG + NOMT - NOMAM
              SYM  = CHAR(NOMT)
              TYPS(J:J) = SYM
            ENDIF
            IF( SYM .NE. TYPMT(J:J)) GOTO 37
   36     CONTINUE
          GOTO 48
   37   CONTINUE
C                                 njama takawa stomana
        WRITE(*, 7)
        READ (*,8) JQ
        IF (JQ .EQ. 'N' .OR. JQ .EQ. 'n') THEN
          TYPS = ' '
          GOTO 35
        ENDIF
C                     wawejdane na nowa stomana
   38   IF (N .GE. 20) THEN
C                          ogranichenie do 20
          WRITE(*,17)
          CLOSE(3)
          STOP 333
        ENDIF
        WRITE(*,9)
        READ(*,11) KFE
        WRITE(*,10)
        READ(*,11) P150
        WRITE(*,12)
        DO 39 J=1,80
          WRITE(*,13) J
          READ(*,11) H(J)
   39   CONTINUE
        WRITE(*,14) (H(J),J=1,80)
        WRITE(*,15)
        READ(*,8) JQ
        IF (JQ .EQ. 'N' .OR. JQ .EQ. 'n') THEN
          GOTO 40
        ELSE
          GOTO 42
        ENDIF
   40   WRITE(*,19)
   41   READ(*,20) II
        IF ( II .GT. 80 ) GOTO 40
        IF ( II .EQ.  0 ) GOTO 42
        WRITE(*,21)
        READ(*,11) H(II)
        GOTO 41
   42   WRITE(*,16)
        READ (*,8 ) JQ
        IF( JQ .EQ. 'Y' .OR. JQ .EQ. 'y') GOTO 43
        GOTO 51
   43   CONTINUE
C               Prowerka za korektnost
        IF ( KFE .LT. 0.9 .OR. KFE .GT. 1.0 ) GOTO 45
        IF ( P150.LT. 1.0 .OR. P150.GT.10.0 ) GOTO 45
        IF ( H(1).LT. 0.  .OR. H(1).GT. 1.0 ) GOTO 45
        DO 44 I = 2,80
          IF ( H(I) .LT. H(I-1) .OR. H(I) .GT. 600.) GOTO 45
   44   CONTINUE
        GOTO 46
C                                 nekorektna danna 
   45   WRITE(*,24)
        GOTO 40
C                                 zapis waw fajla na N. mjasto
   46   CONTINUE
        N  = N + 1
        WRITE(3,1,REC=1,ERR=32) N
        NREC  = 2 + 12*(N-1)
        WRITE(3,4,REC=NREC,ERR=32) TYPS
        WRITE(3,25,REC=NREC+1,ERR=32) KFE,P150
        DO 47 M=1,10
          JN = 1 + 8*(M-1)
          NRE=NREC+1+M
          WRITE(3,25,REC=NRE    ,ERR=32) (H(J),J = JN,JN + 7)
   47   CONTINUE
        WRITE(*,18) TYPS
        CLOSE(3,STATUS='KEEP')
        GOTO 51
C                                 ¨ima takawa stomana waw fajla
C                                 ·chetene ot fajla  ELST.INF
   48   NREC=2+12*(I-1)
        READ(3,25,REC=NREC+1,ERR=32) KFE,P150
        DO 49 M=1,10
          NRE=NREC+1+M
          JN = 1 + 8*(M-1)
          READ(3,25,REC=NRE   ,ERR=32) (H(J),J = JN,JN + 7)
   49   CONTINUE
        CLOSE(3)
C
C               Prowerka za korektnost
        IF ( KFE .LT. 0.9 .OR. KFE .GT. 1.0 ) GOTO 100
        IF ( P150.LT. 1.0 .OR. P150.GT.10.0 ) GOTO 100
        IF ( H(1).LT. 0.  .OR. H(1).GT. 1.0 ) GOTO 100
        DO 50 I = 2,80
          IF ( H(I) .LT. H(I-1) .OR. H(I) .GT. 600.) GOTO 100
   50   CONTINUE
        IF(IDE .EQ. 1) THEN
          WRITE(4,4,REC=11,ERR=101) TYPST
        ELSE
          WRITE(4,4,REC=12,ERR=101) TYPRO
C          CLOSE(4,STATUS='KEEP')
        ENDIF

C               Aproksimacionni koeficienti 
   51   A(1,2)=H(1)/0.025
        A(1,1)=0.
        DO 52 I=2,80
          B=0.025*I
          A(I,2)=(H(I)-H(I-1))/0.025
          A(I,1)=H(I)-A(I,2)*B
   52   CONTINUE
        RETURN
C                                 namerena e nekorektna stojnost 
C                                 waw fajla  ELST.INF
  100   WRITE(*,22)
        WRITE(*,22) KFE,P150
        WRITE(*,23) (H(I),I=1,80)
        CLOSE(3)
        STOP
  101   WRITE(*,3)
        CLOSE(4)
        STOP 444
      END
