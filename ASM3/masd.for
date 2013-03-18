C
C         Program ASM3   - triphazen asynchronen dwigatel
C         Asinhronen dwigatel s kyso-saedinen rotor, figuren kanal 
C       Fajlowe   Podprogrami
C       MASDK.FOR: MAIN, PECAT, PECAT1, PECAT2, PECAT3
C       FASD.FOR:  KDRK, OI1, OI2, OP1, OP2, PN
C       WD.FOR  :  WD,   APHB
C       GRS.FOR :  GRS1,GRS2,GRS3,GRS4,GRS5,GRS6,LZS FUN1,FUN2,FUN3
C       GRR.FOR :  GRR1,GRR2,GRR3,GRR4,GRR5,GRR6,GRR7,GRR8,LZRK
C       MAGW.FOR:  MW, HB, FZS, FZR
C       DOB.FOR :  HARM, INDUK
C       DOBS.FOR:  SAGUBI, PERIM, P2NIC
C       FOUT.FOR:  FOUT
C       Variant 1, 2
C       Variant 3   1993,1995
C       Variant 4   1996
C       Variant 5   2000
C       Variant 6   2000 english
C       Variant 7   2010
C       Variant 8   11.2012
C       Variant 8.1 2 2013
$DEBUG
      REAL I1,IOM,IOA,I2P,I2,I1R,I1A,I1N,I1P,I1L,I2M,IO,
     *     J1,JKP2,J1AS1,JPR2,
     *     K1,K2,KI,KRX,KZ,KFE1,KFE2,KY1,KQ1,KW1,KDN,KPI,KF1,KS1,
     *     KM,KBET1,KBET1P,KSCH1,KSCH2,KDEL,KAP,KR,KSK,KS,KX,
     *     L1,L2,LI,L4S1,LW1,LS1,LS2,LIS1,LPR1,
     *     LZ1,LZ2,LZ2K,LD1,LD2,LSK2,LZ2P,LZ1P,LAN(82),
     *     M,MN,MPUS,MEM,KB,KMU,LD1P,LD2P,
     *     N,N1,NN,NEL1,N1SH,N1W
        real pmas(42),ksmas(3)
        REAL KW(30),tabliza(19)
        integer itab
        REAL MUR(49,17),MUER(2,17),LAM(2,24),BIND(49,17),BE(2,17),
     *       F1MU(17),F2NI(49),PSIN(49),PSIM(17),PSE(2,17),
     *       KP (49,17),PPZ1,PPZ2,O1,O2,
     *       KMNI(49),KMRNI(49),KMMU(17),KMRMU(17),ALCN(49),K2N(49),
     *       SNIMU(500),SGRUPI(20)
        REAL SIGMAF,ALPHAS,BETAS,KGZ1,KGZ2,KFZ1,KFZ2,KO1,KO2,
     *       MDOP1,MDOP2,MDOP
        INTEGER MU(49,17),NIMU(500,5),KNIMU,KGRUPI,BRGR(20),klk
        LOGICAL PEC,LOM
        real pecatS(22),pecatI(22),pecatM(22)
        integer index
        CHARACTER*1 JQ
      COMMON PI,  K1, K2,  KI, KRX, U1PH,HA1, HA2,TA1,TA2,
     *       GA1, GA2,GZ1, GZ2,GCU1,KZ,  BDEL,BA1,BA2,FDEL,
     *       FZ1, FZ2,FA1, FA2,FO,  PFE, IOM, IOA,X12,PFEZ,
     *       PFEA,GFE,GZAG,ALD,DLZ1,KB , KMU, deteh
     *  /KOEF/ AST(80,2),ARO(80,2)
     *  /CWD/ P2N, P,   U1N, SSN,  D1,  DA1, DI2,  L1,   L2,   DEL,
     *        Z1,  WSK, BS1, HS1,  B11, B1,  HK1P, H11 , HKL1, BKL1,
     *        Z2,  WRK, BS2, HS2,  B21, B2,  HK2P, H21,  BS3,  HS3,
     *        B3,  B31, H3,  HM ,  BSK, BKP,  AKP,  WSN,  QD1, QC1,
     *        Y1,  A1,  UK1, NEL1, DPR1,DIS1, DEIS, KS1, LPR1, PMEX,
     *        RO120,RO220,TPH,TNS, TNR, TKS, TKR,  TSTA, UPON, F1,
     *        CFEZ,CFEA,KFE1,P1501,KFE2,P1502,UK11,UK12, UK13, UK14,
     *        BPR1,APR1,SPR1,BIS1,AIS1, N1SH,N1W, BPR2, BPR21,HPR2, D2
     *    /CBKZ1/ BK1(7),BZ1(7),NSS,BZ1I,BZ1M
     *    /CBKZ2/ BK2(15),BZ2(15),NSR,BZ2I,BZ2M
     *    /CST1/  HZ1,BZ1SR,B1SR,SK1,SIS1,SK1PR,BEK1,BES1,H1PR,LIS1,
     *            H16,H17,H1P,H1S,SK1P,SK1S,B11P,H1
     *    /CRO1/  HZ2,HK2,BZ2SR,B2SR,B3SR,SK2,BES2,BES32,BES3,H21P,
     *            BSCH2,HSCH2,BB2
     *    /CST2/  W1,KW1,Q1,BET1,KF1,KY1,KQ1,H2
      COMMON /CSBE/ KBET1,KBET1P,KDN,KAP,KSK
     *    /CKO1/  SIG1,ROD1,SIG2,ROD2,KSCH1,KSCH2
     *    /COI1/  TAU1,TAU2,LI,T1,T2,KDEL
     *    /C4S/   SEF1,LW1,L4S1,KS
     *    /CRX/   R1,LS1,XD1,XD2P,XSK2P,X1,LZ1,LD1,LD2,
     *            TKPHS,TKPHR,TKNS,TKNR,TKKS,TKKR,TKSS
     *    /CNP/   N1,PDOPI,PP,S,E1
     *    /CI/    I2P,I2,I1R,I1A,I1
     *    /CPAR/  P1,P2,PEL1,PEL2,PSAG,COSPH,ETA,M,J1
     *    /CLZR/  BRE,B(82),H(82),Q(82),ALFA(82),LAN,HB21,HB31,HB32
      COMMON /CWDP/ KLUR,RCG
     *       /CDOBS/SIGMAF,ALPHAS,BETAS,KGZ1,KGZ2,KFZ1,KFZ2,KO1,KO2
     *       /CPEC/ PEC,NOMM
     *       /CINT/ IWSK,IWRK,ISSN,IWSN,IWSNWI,IWSNSL,IWSNRA,IWSNFZ
     *       /CSIN/ KW
      DATA KSMAS/0.47, 0.34, 0.24/,
     *     PMAS/0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3,
     *          1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1,
     *          2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9,
     *          3.0, 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.8, 3.9, 4.0,
     *          4.1, 4.2, 4.3, 4.4, 4.5/
    1   FORMAT(3F9.0,5F9.3)
    2   FORMAT(8F9.3)
    3   FORMAT(8F9.0)
    4   FORMAT(10F7.3)
    5   FORMAT('Counter for no-load',I3)
    7   FORMAT(2f9.3,2f9.0,2f9.2)
    8   FORMAT( F9.4, F9.1,2F9.3,2F9.0,2F8.1,F7.2)
    9   FORMAT(8F9.1)
   10   FORMAT( F6.3, F8.2, F8.1,3F8.3,3F8.2,F8.1)
   11   FORMAT(/,26X,'Results :')
   12   FORMAT(26X,'Parameter')
   13   FORMAT(5X,'w1',7X,'l4s1',5X,'lw1',6X,'kf1',5X,'kdel',7X,
     *       'kq1',6X,'ky1',6X,'kw1')
   14   FORMAT(/,5X,'r1/20',3X,'r2p/20',4X,'Sk1',6X,'Sk2')
  114   FORMAT(/,5X,'r1/20',3X,'r2p/20',4X,'Sk1',6X,'Sk2',
     *           6X,'Gcu1',5X,'Gal2',5X,'Gfe',6X,'Gtot')
  115   FORMAT(2F9.3,2F9.0,4F9.1)
   15   FORMAT(/,5X,'Gz1',6X,'Gz2',6X,'Ga1',6X,'Ga2',6X,'Gcu1',5X,
     *       'Gal2',5X,'Gfe',6X,'Gtot')
   16   FORMAT(/,5X,'RATED POWER    :',3X,'P2n=',F8.3,' kW,',3X,
     *          'temp.st.=',F4.0,',',2X,'temp.rot.=',F4.0)
   17   FORMAT(2F9.3,2F9.0,4F9.3)
   18   FORMAT(/,13X,'RATED POWER (Re-calculated)',
     *           5X,'class insul.temp.=',F4.0)
   19   FORMAT(8F9.3)
   20   FORMAT(3X,'lz1',4X,'ls1',4X,'ld1',4X,'lz2',4X,'ls2',4X,
     *        'ld2',4X,'lsk2',3X,'lzm')
   24   FORMAT(/,21X,'P2n is to big')
   23   FORMAT(5X,'s',5X,'I1k',4X,'Mk',7X,'x1k',5X,'x2k',5X,'r2k',
     *        5X,'ki',5X,'I1lk',4X,'Pk ')
   25   FORMAT(/,18X,'PERFORMANCE CHARACTERISTICS',
     *          2X,'temp.st.=',F4.0,2X,'temp.rot.=',F4.0)
   26   FORMAT(5X,'s',8X,'n',8X,'I1',7X,'I1l',6X,'P1',7X,'P2',6X,
     *        'M',7X,'eta',5X,'kPn')
   27   FORMAT(/,18X,'START CHARACTERISTICS',
     *          2X,'temt.st.=',F4.0,2X,'temp.rot.=',F4.0)
   28   FORMAT(/,5X,'s',5X,'I1k',4X,'Mk',7X,'x1k',5X,'x2k',5X,'r2k',
     *        5X,'km',6X,'ki',5X,'I1lk',4X,'Mem')
   29   FORMAT(/,23X,'NO LOAD   ',5X,'temp.=',F4.0)
   30   FORMAT(5X,'E1',7X,'r1',7X,'x12',6X,'pel1',5X,'pel2',5X,
     *          'pzag',5X,'P1',7X,'P2')
   31   FORMAT( F9.1,2F9.3,5F9.0)
   32   FORMAT(/,21X,'Upon=',F8.3,
     *          2X,'temp.st.=',F4.0,2X,'temp.rot.=',F4.0)
   33   FORMAT(' Do you like to continue (Y/N) :Y')
   37   FORMAT('+Do you like to continue (Y/N) :',$)
   34   FORMAT(5X,'No possible to find a rated-power point!')
   35   FORMAT(2X,'The size : b2=',F6.2,' and b21=',F6.2,
     *          ' are not correct for slot  No:',I2)
   36   FORMAT(A1)
  116   FORMAT(1X,'   Inpossible to compute power ',F7.0)
  117   FORMAT(/,5X,'r1/20',3X,'r2p/20',4X,'Sk1',6X,'Sk2',6X,'kw5',
     *         6X,'kw7')
  118   FORMAT(10x,'No load')
  119   FORMAT(10x,'Load with rated power')
  120   FORMAT(10x,'Performance characteristics')
  121   FORMAT(10x,'Start characteristics')
C
        PI  = 3.1415926
        deteh = 0.1
        KLUR = 1
        SQ3 = SQRT(3.)
        HKR = 200.E2/1.E3
        CALL WD
		PEC=.true.
        itab=1
        tabliza(itab)=l1
        itab=itab+1
        tabliza(itab)=uk1
        itab=itab+1
        tabliza(itab)=dpr1
        itab=itab+1
        tabliza(itab)=nel1
C
        P2N  = 1.E3*P2N
        IF (HM .GT. 0.) THEN
          LOM = .TRUE.
        ELSE
          LOM = .FALSE.
        ENDIF
        if (IWSNsl .eq. 1) then
          KS = KSMAS(1)
        else
          KS = KSMAS(2)
        endif
        if (hkl1 .le. 0.) hkl1 = 0.001
        if (bs2 .le. 0. .and. hs2 .le. 0.) bs2 = 0.001
        if (bs3 .le. 0. .and. hs3 .le. 0.) bs3 = 0.001
        if (IWSNfz .eq. 2) KS = KSMAS(3)
C               Geometrichni razmeri stator
      GOTO (41,42,43,44,45,46),IWSK
   41   CALL GRS1
        GOTO 47
   42   CALL GRS2
        GOTO 47
   43   CALL GRS3
        GOTO 47
   44   CALL GRS4
        GOTO 47
   45   CALL GRS5
        GOTO 47
   46   CALL GRS6
        GOTO 47
   47   CONTINUE
C               Obshti izchislenija 1
        CALL OI1
C               Kanalna prowodimost stator
        CALL LZS
C               Geometrichni razmeri rotor
      GOTO (51,52,53,54,55,56,57,58),IWRK
   51   CALL GRR1
        SPR2=SK2
        GOTO 60
   52   CALL GRR2
        IF(HPR2 .GT. 0.) THEN
          SPR2=(BPR2+BPR21)/2.*HPR2
        ELSE
          SPR2=SK2
        ENDIF
        GOTO 60
   53   CALL GRR3
        IF(HPR2 .GT. 0.) THEN
          SPR2=(BPR2+BPR21)/2.*HPR2
        ELSE
          SPR2=SK2
        ENDIF
        SPR2=SK2
        GOTO 60
   54   CALL GRR4
        SPR2=SK2
        GOTO 60
   55   CALL GRR5
        IF(BPR2 .GT. 0.) THEN
          SPR2=PI*BPR2**2/4.
        ELSE
          SPR2=SK2
        ENDIF
        GOTO 60
   56   CALL GRR6
        SPR2=SK2
        GOTO 60
   57   CALL GRR7
        SPR2=SK2
        GOTO 60
   58   CALL GRR8
        SPR2=SK2
        GOTO 60
   60   CONTINUE
C               Kanalna prowodimost rotor
        CALL LZRK(0.,TNR,KR,LZ2,LZ2K)
C               Koeficienti na diferenzialno razsejwane
        CALL KDRK
C               Obshti izchislenija 2
        CALL OI2
C               IZCHISLENIE NA ROTOR
        DKP2 = D2 - BKP
        SKP2 = AKP*BKP
        DELT = 2.*SIN(PI*P/Z2)
        RKP2 = 1.0E-3*RO220*PI*DKP2/Z2/SKP2
        RPR2 = 1.0E-3*RO220*L2/SPR2
        GAL2 = 2.7E-6*(Z2*SK2*L2+2.*PI*DKP2*SKP2)
        LS2  = DKP2/(Z2*LI*DELT**2)*ALOG(4.7*DKP2/(2.*BKP+AKP))
        R2P  = KRX*(RPR2+2.*RKP2/DELT**2)
        G    = GFE+GCU1+GAL2
        R120 = R1
        R2P20= R2P
        WRITE (2, 11)
        WRITE (2, 12)
        WRITE (2, 13)
        WRITE (2,  1) W1,L4S1,LW1,KF1,KDEL,KQ1,KY1,KW1
        IF (PEC) THEN
          IF (IWSNwi .EQ. 4) THEN
            WRITE (2, 117)
            WRITE (2,  7) R120,R2P20,SK1,SK2,KW(5),KW(7)
          ELSE
            WRITE (2, 14)
            WRITE (2,  7) R120,R2P20,SK1,SK2
          ENDIF
          WRITE (2, 15)
         IF(GZ1.LT.100..AND.GZ2.LT.100..AND.GA1.LT.100..AND.GA2.LT.100.
     *      .AND.GCU1.LT.100..AND.GAL2.LT.100.
     *      .AND.GFE.LT.100..AND.GZAG.LT.200.) THEN
            WRITE (2, 19) GZ1,GZ2,GA1,GA2,GCU1,GAL2,GFE,GZAG
         ELSE
            WRITE (2,  9) GZ1,GZ2,GA1,GA2,GCU1,GAL2,GFE,GZAG
         ENDIF
        ELSE
          WRITE(2,114)
          IF(GCU1.LT.100..AND.GAL2.LT.100..AND.GFE.LT.100.
     *      .AND.GZAG.LT.200.) THEN
            WRITE(2, 17)  R120,R2P20,SK1,SK2,GCU1,GAL2,GFE,GZAG
          ELSE
            WRITE(2,115)  R120,R2P20,SK1,SK2,GCU1,GAL2,GFE,GZAG
          ENDIF
        ENDIF
        itab=itab+1
        tabliza(itab)=kf1
        itab=14
        tabliza(itab)=gcu1
        itab=itab+1
        tabliza(itab)=gal2
        itab=itab+1
        tabliza(itab)=gfe
        itab=itab+1
        tabliza(itab)=gzag
C          Postojanni danni
C
        CALL HARM(PI,QD1,P,Z1,Z2,T1,T2,DEL,BS1,BSCH2,Y1,DA1,
     *            L1,L2,
     *            MUR,MU,IE,JE,IM,JM,NY,LAM,MUER,ED,
     *            KP,O1,O2,
     *            KNIMU,NIMU,SNIMU,KGRUPI,SGRUPI,BRGR,
     *            IWRK,B2,B21,H2,B3,HS2,BES2,BES32,BES3,PK,
     *            BSK,TAU1,ALCN,K2N)
C      *******************************
C          PRAZEN HOD
C      *******************************
        WRITE (*, 118)
        PDOP   = PDOPI
        PMEX0 = PMEX
        R1 = TKPHS * R120
        R2P= TKPHR * R2P20
        I2 = 0.1 * I2
C       S  = 0.0001
        s  = 0.005
        TEMP = TPH
        P2R  = 0.
        E1P=E1
        PP = PDOP + PMEX0 + P2R
        DLZMP = 0.
        CALL LZRK(S*F1,TEMP,KR,LZ2,LZ2K)
        KMK = 1
        klk = 0
   64   CONTINUE
        CALL MW(E1P)
        klk = klk + 1
   65   CONTINUE
        X2P =K2*(LZ2+HSCH2/BSCH2+DLZMP+LS2)+XD2P+0.5*XSK2P
        CALL OP1(E1P,R1,X1,R2P/S,X2P)            
C	  write(2,*) 'e1p',e1p,' e1',e1
        IF( LOM ) THEN
          DLZM=0.3+1145.*HM/I2
          EPSI=ABS(DLZM-DLZMP)/DLZM
          IF(EPSI.GT.0.05) THEN
            DLZMP=DLZM
            GO TO 65
          END IF
        END IF
        EPSI=ABS(E1P-E1)/E1
C       IF (EPSI .GT. 0.001) THEN
        IF (EPSI .GT. 0.01 ) THEN
          E1P=(4.0*E1P+E1)/5.0
          GOTO 64
        ENDIF
        CALL OP2(R1,X1,R2P/S,MEM,COSPH0,PMEX0,PDOP)
C
        IF (IWSNwi .NE. 4) THEN
        IF (KMK .EQ. 1 .OR. KMK .EQ. 2 ) THEN
          IO = SQRT(IOA**2 + IOM**2)
          CALL INDUK(I1,IO,COSPH,COSPH0,S,P,F1,PI,Z1,Z2,ALD,
     *             BDEL,D1,BSK,ED,QD1,Y1,KW1,BS1,BSCH2,
     *             MUR,MUER,LAM,IE,JE,IM,JM,NY,
     *             F1MU,F2NI,BIND,BE,PSIN,PSIM,PSE,BDEL3,
     *             KMNI,KMRNI,KMMU,KMRMU)
          IF (KLUR .EQ. 1) THEN
            CALL SAGUBI (IM,JM,IE,JE,BDEL,BZ1I,BZ2I,GZ1,GZ2,
     *                   O1,O2,TAU1,TAU2,MUR,
     *                   BIND,KP,F2NI,KMNI,KMRNI,F1MU,KMMU,KMRMU,
     *                   PPZ1,PPZ2,PPH1,PPH2,
     *          TNR,TKNR,RO220,SK2,IWRK,B2,B21,H2,B3,HS2,BS2,Z2,L2,
     *          BES2,BES32,BES3,PI,PK,RCG,P,DEL,KDEL,DLZ2,HSCH2,BSCH2,
     *          ALCN,PC,MDOP1,MDOP2,MDOP)
            PSU = PPZ1 + PPZ2 + PPH1 + PPH2 + PC
            PSAG = PSAG - PDOP + PSU
            P2  = P2 + PDOP - PSU
            PDOP = PSU
          ENDIF
        ENDIF
        ENDIF
        IF (KMK .EQ. 1) THEN
          PP = PMEX0 + PDOP
          V1 = PP*X2P**2+3.*E1P**2*R2P
          V2 = 9.*E1P**4-4.*PP*V1
          IF (V2 .LT. 0.) V2 = 0.
          S  = R2P*(3.*E1P**2-SQRT(V2))/(2.*V1)
          KMK= 2
          GOTO 65
        ENDIF
        KMK = KMK + 1
        IF (ISSN .EQ. 1) THEN
          I1L=I1
        ELSE
          I1L=SQ3*I1
        ENDIF
        JPR2= I2/SPR2
        JKP2= I2/(DELT*SKP2)
        AS1 = I1*UK1/(A1*T1)
        J1AS1=J1*AS1
        LD1 = XD1 /K1
        LD2 = XD2P/K2
        LSK2= 0.5*XSK2P/K2
        LZ2P= LZ2 + HSCH2/BSCH2
        N = N1*(1.-S)
        PFE0 = PFE
        PFED = PFE + PDOP
        index=22
        pecatS(index)=s
        pecatI(index)=i1l
        pecatM(index)=30/pi*pmex0/(n1*(1-s))
        index=21
        WRITE (2, 29) TPH
        IF (PEC) THEN
          CALL PECAT1( E1,BDEL,BZ1I,BZ2I,BA1,BA2,BZ1M,BZ2M,
     *                FO,FDEL,FZ1,FZ2,FA1,FA2,IOM,KZ,
     *                LZ1,LS1,LD1,LZ2P,LS2,LD2,LSK2,DLZM,
     *                I1,I1L,S,R1,R2P,X1,X2P,X12,
     *                PEL1,PFE0,PDOP,PMEX0,PFED,PSAG,P1)
        ELSE
          CALL PECAT3(E1,BDEL,BZ1I,BZ2I,BA1,BA2,BZ1M,BZ2M,
     *                FO,FDEL,FZ1,FZ2,FA1,FA2,IOM,KZ,
     *                I1,I1L,S,R1,R2P,X1,X2P,X12,
     *                PEL1,PFE0,PDOP,PMEX0,PFED,PSAG,P1)
        ENDIF
        JQ='Y'
C         *****************************************************
C          Rated power
C         *****************************************************
        psum0=pdop
        WRITE (*, 119)
        R1   = TKNS*R120
        R2P  = TKNR*R2P20
        DLZMP= 0.
        DLZM = 0.
        S    = 0.01
        PMEX = PMEX0*(1.-S)**2
        E1P  = 0.98*E1
        PP   = P2N + PMEX + PDOP
        CALL LZRK(S*F1,TNS,KR,LZ2,LZ2K)
        KLU = 1
        klk = 0
        KK   = 0
        I1P  = i1
        I2P  = I1P
        I2   = KI*I2P
        KDN  = 0.636+2.5*SQRT(DEL/(T1+T2))
   70   CONTINUE
        CALL MW(E1P)
        klk = klk + 1
   71   CONTINUE
        PMEX = PMEX0*(1.-S)**2
C               Zagubi, moshtnosti
        call pn(i1 ,x1 )
        I2M  = I2 *SQRT(2.)
        IF ( LOM ) THEN
          DLZ2 = 0.
          HHP  = (I2M - 1630.*BSCH2)/BB2
          IF (HHP .LT. HKR) THEN
            DLZM = 0.3+1145.*HM/I2
          ELSE
            DLZM = 0.3+1145. * (HSCH2+HM)/I2 - HSCH2/BSCH2
          ENDIF
        ELSE
          C2   = (T2 - BSCH2)*(1. - KAP)
          DLZ2 = HSCH2/BSCH2 * C2/(C2 + BSCH2)
        END IF
        X2P  =K2*(LZ2K+HSCH2/BSCH2-DLZ2+DLZM+LS2)+KAP*XD2P+
     *          I1P/(I1P + I2P) * 0.5*XSK2P
c       X2P = K2*(LZ2 +HSCH2/BSCH2+DLZMP+LS2)+XD2P
c    *          + 0.5 * I1/(I1 + I2P) * XSK2P
        KK   = KK + 1
        IF (KK .GE. 1000) THEN
          WRITE(*,34)
C                          problem with rated power point
          WRITE(2,34)
          STOP 100
        ENDIF
        PP   = P2N + PMEX + PDOP
        V1   = PP*X2P**2+3.*E1P**2*R2P
        V2   = 9.*E1P**4-4.*PP*V1
   72   continue
        CALL OP1(E1P,R1,X1 ,R2P/S,X2P)
C       write(2,*) 'e1p',e1p,' e1',e1
        EPSI=ABS(E1P-E1)/E1
        IF (EPSI .GT. 0.001) THEN
          E1P=(9.0*E1P+E1)/10.0
          GOTO 70
        ENDIF
        IF (KLU .EQ. 1 ) THEN
         IF(IWSNwi .NE. 4) THEN
          IO = SQRT(IOA**2 + IOM**2)
          CALL INDUK(I1,IO,COSPH,COSPH0,S,P,F1,PI,Z1,Z2,ALD,
     *             BDEL,D1,BSK,ED,QD1,Y1,KW1,BS1,BSCH2,
     *             MUR,MUER,LAM,IE,JE,IM,JM,NY,
     *             F1MU,F2NI,BIND,BE,PSIN,PSIM,PSE,BDEL3,
     *             KMNI,KMRNI,KMMU,KMRMU)
          CALL SAGUBI (IM,JM,IE,JE,BDEL,BZ1I,BZ2I,GZ1,GZ2,
     *                   O1,O2,TAU1,TAU2,MUR,
     *                   BIND,KP,F2NI,KMNI,KMRNI,F1MU,KMMU,KMRMU,
     *                   PPZ1,PPZ2,PPH1,PPH2,
     *          TNR,TKNR,RO220,SK2,IWRK,B2,B21,H2,B3,HS2,BS2,Z2,L2,
     *          BES2,BES32,BES3,PI,PK,RCG,P,DEL,KDEL,DLZ2,HSCH2,BSCH2,
     *          ALCN,PC,MDOP1,MDOP2,MDOP)
          PSU = PPZ1+PPZ2+PPH1+PPH2+PC
          PDOP = PSU
         ENDIF
          CALL OP2(R1,X1,R2P/S,MEM,COSPH0,PMEX,PDOP)
          if (abs(P2-P2n)/P2n .gt. 0.005) then
C           write(2,*)'s',s,' P2',P2,' I1',I1
            IF ( P2 .LT. P2N) THEN
C             S = S * P2N/P2
              if(P2 .lt. 0.9*P2N) then
                s = s + 0.01
              else
                s = s + 0.001
              endif
            else
C             s = s - 0.001
              s = s*P2n/P2
            endif
            GOTO 71
          ENDIF
          SN  = S
          NN  = N1*(1.-SN)
          N   = NN
          I1N = I1
          MN  = M
          E1N = E1
          JPR2= I2/SPR2
          JKP2= I2/(DELT*SKP2)
          AS1 = I1*UK1/(A1*T1)
          J1AS1=J1*AS1
          LD1 = XD1 /K1
          LD2 = XD2P/K2
          LSK2= 0.5 * I1/(I1 + I2P) * XSK2P/K2
          LZ2P=LZ2+HSCH2/BSCH2
          IF (ISSN .EQ. 1) THEN
            I1L = I1N
          ELSE
            I1L = SQ3*I1N
          ENDIF
        ELSE
          GAM = ATAN((I1R*R1-I1A*X1 )/(E1+I1A*R1+I1R*X1 ))
          PSI = ATAN(I1R/I1A)
          COSPH=COS(PSI-GAM)
          P1  = 3.*U1PH*I1*COSPH
          PEL1= 3.*I1**2*R120*TKSS
          PEM = P1 - PEL1 - PFED
          PEL2= PEM*S
          PDOP=0.005*P1
          PSAG= PEL1 + PEL2 + PFED + PMEX0 + PDOP
          P2  = P1 - PSAG
C         IF (ABS(P2-P2N) .GT. 0.001*P2N) THEN
          IF (ABS(P2-P2N) .GT. 0.005*P2N) THEN
            S = S*P2N/P2
C           write(2,*) ' i1',i1,' s',s
            GOTO 72
          ENDIF
          N = N1*(1-S)
          M =30./PI*P2/N
          IF (ISSN .EQ. 1) THEN
            I1L = I1
          ELSE
            I1L = SQ3*I1
          ENDIF
          ETA = 100.*P2/P1
        ENDIF
        IF (KLU .EQ. 1 ) THEN
         IF ( PEC ) THEN
          P2P = 1.E-3*P2
          WRITE (2, 16) P2P,TNS,TNR
          CALL PECAT(  E1,BDEL,BZ1I,BZ2I,BA1,BA2,BZ1M,BZ2M,
     *               FO,FDEL,FZ1,FZ2,FA1,FA2,IOM,KZ,
     *               LZ1,LS1,LD1,LZ2P,LS2,LD2,LSK2,DLZM,
     *               I1,I1L,J1,JPR2,JKP2,AS1,S,N,
     *               PEL1,PEL2,PFE,PDOP,PMEX,PFE+PDOP,PSAG,P1,
     *               M,COSPH,ETA,R1,R2P,X1,X2P,X12)
C     
          itab=6
          tabliza(itab)=bdel
          itab=itab+1
          tabliza(itab)=i1l
          itab=itab+1
          tabliza(itab)=cosph
          itab=itab+1
          tabliza(itab)=eta
C
         ENDIF
          KLU = 2
          KK = 0
          GOTO 70
        ELSE
          WRITE (2, 18) TSTA
          CALL PECAT2(I1,I1L,S,N,M,COSPH,ETA,
     *                PEL1,PEL2,PFED,PDOP,PMEX0,PSAG,P1,P2)
          itab=itab+1
          tabliza(itab)=eta
        ENDIF
        E1N   = E1
        PDOPN = PDOP
        IF ( .NOT. PEC) GOTO 300
C            *******************************
C                 performance chracteristics
C            *******************************
        continue
        WRITE( *, 120)
        I1 =I1N
        S  = SN*PMAS(1)
        WRITE (2, 25) TNS,TNR
        WRITE (2, 26)
        R1 = R120 * TKNS
        R2P= R2P20* TKNR
        TEMP= TNR
        etamax=0
        DO 75 I=1,12
c       do 75 i = 1,40
          s = 1.005*s
          P2R =PMAS(I)*P2N
          E1P =E1
          PMEX = PMEX0*(1.-S)**2
          PP = PMEX + P2R +PDOP
          KK = 0
          i1p=i1
          CALL LZRK(S*F1,TEMP,KR,LZ2,LZ2K)
   73     CALL MW(E1P)
C         write(2,*) 'e1p',e1p
   74     continue
          call pn(i1,x1)
          KK = KK + 1
          IF (KK .GT.1000) THEN
            WRITE(2,116) P2R
c           IF( I .GE. 6 ) THEN
              GOTO 300
c           ELSE
c             STOP
c           ENDIF
          ENDIF
          IF ( LOM ) THEN
            DLZM=0.3+1145.*HM/I2
            EPSI=ABS(DLZM-DLZMP)/DLZM
            IF (EPSI.GT.0.05) THEN
              DLZMP=(4*DLZM+DLZMP)/5
              GOTO 74
            ENDIF
          ENDIF
          X2P  =K2*(LZ2K+HSCH2/BSCH2-DLZ2+DLZM+LS2)+KAP*XD2P+
     *                            0.5*XSK2P
          V1  = PP*X2P**2+3.*E1P**2*R2P
          V2  = 9.*E1P**4-4.*PP*V1
          IF (V2 .LE. 0.) THEN
            E1P=1.005*E1P
            IF (E1P .LT. U1PH) GOTO 73
              WRITE(*,24)
              IF (I .GE. 6) THEN
                GOTO 300
              ELSE
                STOP
              ENDIF
          ENDIF
C         S   = R2P*(3.*E1P**2-SQRT(V2))/2./V1
          PMEX= PMEX0*(1.-S)**2
          CALL OP1(E1P,R1,X1,R2P/S,X2P)
C         write(2,*) 'e1p',e1p,' e1',e1
          if( e1 .lt. 0) then
            e1p = e1 - 0.01*u1ph
            goto 73
          endif
          EPSI = ABS(E1P-E1)/E1
          IF (EPSI .GT. 0.001) THEN
            E1P = (9.0*E1P+E1)/10.0
            GOTO 73
          ENDIF
C          CALL OP2(R1,X1,R2P/S,MEM,COSPH0,PMEX,PDOP+psu)
          CALL OP2(R1,X1,R2P/S,MEM,COSPH0,PMEX,PDOP+1.2*psum0)
C
          if( abs(P2-P2R)/P2R .gt. 0.01) then
C           write(2,*) 'P2 ',P2,' P2R',P2R,' s',s
            if( P2 .lt. P2R) then
              if (P2 .lt. 0.8*P2R) then
                s = s + 0.01
              else
                s = s + 0.001
              endif
            else
              s = s*P2R/P2
            endif
            goto 74
          endif
          IF (ISSN .EQ. 1) THEN
            I1L = I1
          ELSE
            I1L = SQ3*I1
          ENDIF
          KM  = M/MN
          JPR2= I2/SK2
          JKP2= I2/(DELT*SKP2)
          AS1 = I1*UK1/(A1*T1)
          J1AS1=J1*AS1
          LD1 = XD1 /K1
          LD2 = XD2P/K2
          LSK2= 0.5*I1/(I1 + I2P)*XSK2P/K2
          LZ2P= LZ2 + HSCH2/BSCH2
          N = N1*(1.-S)
          WRITE(2,8)S,N,I1,I1L,P1,P2,M,ETA,PMAS(I)
           if(i .eq. 7) then
              itab=18
              tabliza(itab)=eta
           endif  
          if(index .gt. 9) then
             pecatS(index)=s
             pecatI(index)=i1l
             pecatM(index)=m
             index=index-1
          endif
          if(eta .gt. etamax) etamax=eta
   75   CONTINUE
          itab=itab+1
          tabliza(itab)=etamax

C                  Start characteristics
  300   WRITE (2, 27) TKS,TKR
        WRITE (*, 121)
        index=1
        R1   = TKKS*R120
        DLZMP= 0.
        DLZM = 0.
        S    = 1.0
        E1P  = 0.5*U1PH
        I1P  = 4.0*I1N
        I2P  = I1P
        I2   = KI*I2P
        KDN  = 0.636+2.5*SQRT(DEL/(T1+T2))
        KLU  = 1
        KLL  = 0
   80   CONTINUE
        klk = 0
        PMEX = PMEX0*(1.-S)**2
        CALL LZRK(S*F1,TKR,KR,LZ2,LZ2K)
        R2KP = TKKR*KRX*(KR*RPR2 + 2.*RKP2/DELT**2)
   81   CALL MW(E1P)
        klk = klk + 1
        if (klk .gt.100) stop 100
C       write(2,*) 'e1p',e1p
   82   CALL PN(I1P,X1N)
        I2M  = I2 *SQRT(2.)
        IF ( LOM ) THEN
          DLZ2 = 0.
          HHP  = (I2M - 1630.*BSCH2)/BB2
          IF (HHP .LT. HKR) THEN
            DLZM = 0.3+1145.*HM/I2
          ELSE
            DLZM = 0.3+1145. * (HSCH2+HM)/I2 - HSCH2/BSCH2
          ENDIF
        ELSE
          C2   = (T2 - BSCH2)*(1. - KAP)
          DLZ2 = HSCH2/BSCH2 * C2/(C2 + BSCH2)
        END IF
        X2NKP=K2*(LZ2K+HSCH2/BSCH2-DLZ2+DLZM+LS2)+KAP*XD2P+
     *          I1P/(I1P + I2P) * 0.5*XSK2P
        if(iom*r1 .gt. 0.9*u1ph) then
          e1p = e1p - 0.02*u1ph
          goto 81
        endif
        CALL OP1(E1P,R1,X1N,R2KP/S,X2NKP)
C       write(2,*) 'e1p',e1p,' e1',e1
        if(e1 .lt. e1p/2.0) then
          e1p = e1p - 0.02*u1ph
          goto 81
        endif
        EPSI = ABS(I1 - I1P)/I1
        IF (EPSI .GT. 1.E-3) THEN
C         write(2,*) 'i1p',i1p,' i1',i1
          I1P = (    I1P+2.0*I1)/3.
          I2P  = I2P * I1P/I1
          goto 82
        ENDIF
        EPSI = ABS(E1 - E1P)/E1
        IF (EPSI .GT. 1.E-3) THEN
          E1P = (4*E1P+ E1)/5.
          goto 81
        ENDIF
        CALL OP2(R1,X1N,R2KP/S,MEM,COSPH0,PMEX,PDOP)
        KPI = I1/I1N
        IF (ISSN .EQ. 1) THEN
          I1L = I1
        ELSE
          I1L = SQ3*I1
        ENDIF
        IF (KLU .EQ. 5) GOTO 99
        IF (S .GT. 0.99) THEN
          IF ( PEC ) THEN
            LZ1P= LZ1 - KBET1P*DLZ1
            LD1P= KAP*XD1 /K1
            LD2P= KAP*XD2P/K2
            LSK2= I1/(I1 + I2P)*0.5*XSK2P/K2
            LZ2P= LZ2K + HSCH2/BSCH2 -DLZ2
            WRITE (2, 20)
            WRITE (2,  4) LZ1P,LS1,LD1P,LZ2P,LS2,LD2P,LSK2,DLZM
          ENDIF
          WRITE (2, 28)
        ENDIF
        IO = SQRT(IOA**2 + IOM**2)
        CALL INDUK(I1,IO,COSPH,COSPH0,S,P,F1,PI,Z1,Z2,ALD,
     *             BDEL,D1,BSK,ED,QD1,Y1,KW1,BS1,BSCH2,
     *             MUR,MUER,LAM,IE,JE,IM,JM,NY,
     *             F1MU,F2NI,BIND,BE,PSIN,PSIM,PSE,BDEL3,
     *             KMNI,KMRNI,KMMU,KMRMU)
        CALL SAGUBI (IM,JM,IE,JE,BDEL,BZ1I,BZ2I,GZ1,GZ2,
     *                   O1,O2,TAU1,TAU2,MUR,
     *                   BIND,KP,F2NI,KMNI,KMRNI,F1MU,KMMU,KMRMU,
     *                   PPZ1,PPZ2,PPH1,PPH2,
     *          TNR,TKNR,RO220,SK2,IWRK,B2,B21,H2,B3,HS2,BS2,Z2,L2,
     *          BES2,BES32,BES3,PI,PK,RCG,P,DEL,KDEL,DLZ2,HSCH2,BSCH2,
     *          ALCN,PC,MDOP1,MDOP2,MDOP)
        AMEM = MEM + MDOP
        M    = M   + MDOP
        MPUS = M
        KM  = M /MN
        kx=lz2k/lz2
c       write (2,*)' iom,ioa,cosph',iom,ioa,cosph
        WRITE (2,910)  S,I1,M,X1N,X2NKP,R2KP,KM,KPI,I1L,AMEM
C       WRITE (2,910)  S,I1,M,X1N,X2NKP,R2KP,KM,KPI,kr ,kx
  910   FORMAT( F6.3, F8.2, F8.1,3F8.3,2F8.2, F8.2, F8.1)
        if( s .gt. 0.999) then
          itab=11
          tabliza(itab)=km
          itab=itab+1
          tabliza(itab)=kpi
          itab=itab+1
          tabliza(itab)=i1l
        endif
        IF ( .NOT. PEC) s = 0.
        if(index .lt. 10) then
          pecatS(index)=s
          pecatI(index)=i1l
          pecatM(index)=m
          index=index+1
        endif
        s = s - 0.10
        IF (s .GE. 0.019) GOTO 80
        write(2,104)
        write(2,101)(pecatS(index),index=1,9)
        write(2,102)(pecatI(index),index=1,9)
        write(2,103)(pecatM(index),index=1,9)
 101    format(1x,'      s',9f8.3)
 102    format(1x,'     i1',9f8.3)
 103    format(1x,'      m',9f8.3,/)
 104    format(/,10x,'Total characteristics ',/)
        write(2,101)(pecatS(index),index=10,18)
        write(2,102)(pecatI(index),index=10,18)
        write(2,103)(pecatM(index),index=10,18)
        IF (UPON .LE. 0. .OR. UPON .GE. U1N) GOTO 100
          IF (ISSN .EQ. 1) THEN
            U1PH = UPON/SQ3
          ELSE
            U1PH = UPON
          ENDIF
          WRITE(2, 32) UPON,TKS,TKR
          IF (UPON .LT. 0.3*U1N) THEN
            E1P = 0.90*U1PH
            I1P = I1N
          ELSE
            E1P = 0.7*U1PH
            I1P = 4.*I1N
          ENDIF
          I2  = KI*I1P
          PDOP = PDOPN
          S   = 1.
          KLU = 5
          GOTO 80
   99   CONTINUE
        LZ1P= LZ1 - KBET1P*DLZ1
        LD1P= KAP*XD1 /K1
        LD2P= KAP*XD2P/K2
        LSK2= I1 / (I1 + I2P)*0.5*XSK2P/K2
        LZ2P= LZ2K + HSCH2/BSCH2 -DLZ2
        IF ( PEC ) THEN
          WRITE (2, 20)
          WRITE (2,  4) LZ1P,LS1,LD1P,LZ2P,LS2,LD2P,LSK2,DLZM
        ENDIF
        WRITE (2, 23)
        WRITE (2, 210) S,I1,M,X1N,X2NKP,R2KP,KPI,I1L,P1
  210   FORMAT( F6.3, F8.2, F8.1,3F8.3,2F8.2,F8.0)
  100   WRITE(*,1001)
 1001   FORMAT(' End of the program !')
        stop
        END
C
        SUBROUTINE PECAT(E1,BDEL,BZ1I,BZ2I,BA1,BA2,BZ1M,BZ2M,
     *                   FO,FDEL,FZ1,FZ2,FA1,FA2,IOM,KZ,
     *                   LZ1,LS1,LD1,LZ2P,LS2,LD2,LSK2,DLZM,
     *                   I1,I1L,J1,JPR2,JKP2,AS1,S,N,
     *                   PEL1,PEL2,PFE,PDOP,PMEX,PFE0,PSAG,P1,
     *                   M,COSPH,ETA,R1,R2P,X1,X2P,X12)
        REAL IOM,I1,I1L,J1,JPR2,JKP2,KZ,LZ1,LS1,LD1,LZ2P,LS2,LD2,LSK2,
     *       N,M
    2   FORMAT(8F9.3)
    3   FORMAT(8F9.0)
    4   FORMAT( F9.1,7F9.3)
    5   FORMAT(6F9.0,2F9.3)
    6   FORMAT(6F9.3, F9.4, F9.1)
    7   FORMAT(3F9.3,5f9.3)
   18   FORMAT(/,5X,'E1',7X,'Bdel',5X,'Bz1',6X,'Bz2',6X,'Ba1',6X,
     *        'Ba2',6X,'Bz1m',5X,'Bz2m')
   19   FORMAT(/,5X,'Fo',7X,'Fdel',5X,'Fz1',6X,'Fz2',6X,'Fa1',6X,
     *        'Fa2',6X,'Iom',6X,'kz')
   20   FORMAT(/,5X,'lz1',6X,'ls1',6X,'ld1',6X,'lz2',6X,'ls2',6X,
     *        'ld2',6X,'lsk2',5X,'lzm ')
   21   FORMAT(/,5X,'I1',7X,'I1l',6X,'j1',7X,'jpr2',5X,'jkp2',5X,
     *        'AS1',6X,'s',8X,'n')
   22   FORMAT(/,5X,'pel1',5X,'pel2',5X,'pfe ',5X,'padd',5X,'pmeh',5X,
     *        'pfe0',5X,'pzag',5X,'P1')
   23   FORMAT(/,5X,'M',8X,'cosFI',4X,'eta',6X,'r1',7X,'r2p',6X,'x1',
     *         7X,'x2p',6X,'x12')
        WRITE (2, 18)
        WRITE (2,  4) E1,BDEL,BZ1I,BZ2I,BA1,BA2,BZ1M,BZ2M
        WRITE (2, 19)
        WRITE (2,  5) FO,FDEL,FZ1,FZ2,FA1,FA2,IOM,KZ
        WRITE (2, 20)
        WRITE (2,  2) LZ1,LS1,LD1,LZ2P,LS2,LD2,LSK2,DLZM
        WRITE (2, 21)
        WRITE (2,  6) I1,I1L,J1,JPR2,JKP2,AS1,S,N
        WRITE (2, 22)
        WRITE (2,  3) PEL1,PEL2,PFE,PDOP,PMEX,PFE0,PSAG,P1
        WRITE (2, 23)
        WRITE (2,  7) M,COSPH,ETA,R1,R2P,X1,X2P,X12
        RETURN
        END
C
        SUBROUTINE PECAT1(E1,BDEL,BZ1I,BZ2I,BA1,BA2,BZ1M,BZ2M,
     *                    FO,FDEL,FZ1,FZ2,FA1,FA2,IOM,KZ,
     *                    LZ1,LS1,LD1,LZ2P,LS2,LD2,LSK2,DLZM,
     *                    I1,I1L,S,R1,R2P,X1,X2P,X12,
     *                    PEL1,PFE,PDOP,PMEX,PFE0,PSAG,P1)
        REAL IOM,I1,I1L,KZ,LZ1,LS1,LD1,LZ2P,LS2,LD2,LSK2
    2   FORMAT(8F9.3)
    3   FORMAT(8F9.0)
    4   FORMAT( F9.1,7F9.3)
    5   FORMAT(6F9.0,2F9.3)
C   6   FORMAT(2F9.3, F9.4,5F9.3)
    6   format(2f9.3, f9.4,5f9.3)
   18   FORMAT(/,5X,'E1',7X,'Bdel',5X,'Bz1',6X,'Bz2',6X,'Ba1',6X,
     *        'Ba2',6X,'Bz1m',5X,'Bz2m')
   19   FORMAT(/,5X,'Fo',7X,'Fdel',5X,'Fz1',6X,'Fz2',6X,'Fa1',6X,
     *        'Fa2',6X,'Iom',6X,'kz')
   20   FORMAT(/,5X,'lz1',6X,'ls1',6X,'ld1',6X,'lz2',6X,'ls2',6X,
     *        'ld2',6X,'lsk2',5X,'lzm ')
   21   FORMAT(/,5X,'I1',7X,'I1l',6X,'s',8X,'r1',7X,'r2p',6X,'x1',
     *        7X,'x2p',6X,'x12')
   22   FORMAT(/,5X,'pel1',5X,'pfe ',5X,'padd',5X,'pmeh',5X,
     *        'pfe0',5X,'pzag',5X,'P1')
        WRITE (2, 18)
        WRITE (2,  4) E1,BDEL,BZ1I,BZ2I,BA1,BA2,BZ1M,BZ2M
        WRITE (2, 19)
        WRITE (2,  5) FO,FDEL,FZ1,FZ2,FA1,FA2,IOM,KZ
        WRITE (2, 20)
        WRITE (2,  2) LZ1,LS1,LD1,LZ2P,LS2,LD2,LSK2,DLZM
        WRITE (2, 21)
        WRITE (2,  6) I1,I1L,S,R1,R2P,X1,X2P,X12
        WRITE (2, 22)
        WRITE (2,  3) PEL1,PFE,PDOP,PMEX,PFE0,PSAG,P1
        RETURN
        END
C
        SUBROUTINE PECAT2(I1,I1L,S,N,M,COSPH,ETA,
     *                   PEL1,PEL2,PFE,PDOP,PMEX,PSAG,P1,P2)
        REAL I1,I1L,N,M
    3   FORMAT(8F9.0)
    6   FORMAT(2F9.3, F9.4,F8.1,3F9.3)
   21   FORMAT(5X,'I1',7X,'I1l',6X,'s',8X,'n',8X,'M',8X,'cosFI',4X,
     *              'eta')
   22   FORMAT(/,5X,'pel1',5X,'pel2',5X,'pfe ',5X,'padd',5X,'pmeh',5X,
     *        'pzag',5X,'P1',7X,'P2')
        WRITE (2, 21)
        WRITE (2,  6) I1,I1L,S,N,M,COSPH,ETA
        WRITE (2, 22)
        WRITE (2,  3) PEL1,PEL2,PFE,PDOP,PMEX,PSAG,P1,P2
        RETURN
        END
C
        SUBROUTINE PECAT3(E1,BDEL,BZ1I,BZ2I,BA1,BA2,BZ1M,BZ2M,
     *                    FO,FDEL,FZ1,FZ2,FA1,FA2,IOM,KZ,
     *                    I1,I1L,S,R1,R2P,X1,X2P,X12,
     *                    PEL1,PFE,PDOP,PMEX,PFE0,PSAG,P1)
        REAL IOM,I1,I1L,KZ
    3   FORMAT(8F9.0)
    4   FORMAT( F9.1,7F9.3)
    5   FORMAT(6F9.0,2F9.3)
    6   format(2f9.3, f9.4,5f9.4)
   18   FORMAT(/,5X,'E1',7X,'Bdel',5X,'Bz1',6X,'Bz2',6X,'Ba1',6X,
     *        'Ba2',6X,'Bz1m',5X,'Bz2m')
   19   FORMAT(/,5X,'Fo',7X,'Fdel',5X,'Fz1',6X,'Fz2',6X,'Fa1',6X,
     *        'Fa2',6X,'Iom',6X,'kz')
   21   FORMAT(/,5X,'I1',7X,'I1l',6X,'s',8X,'r1',7X,'r2p',6X,'x1',
     *        7X,'x2p',6X,'x12')
   22   FORMAT(/,5X,'pel1',5X,'pfe ',5X,'padd',5X,'pmeh',5X,
     *        'pfeo',5X,'pzag',5X,'P1')
        WRITE (2, 18)
        WRITE (2,  4) E1,BDEL,BZ1I,BZ2I,BA1,BA2,BZ1M,BZ2M
        WRITE (2, 19)
        WRITE (2,  5) FO,FDEL,FZ1,FZ2,FA1,FA2,IOM,KZ
        WRITE (2, 21)
        WRITE (2,  6) I1,I1L,S,R1,R2P,X1,X2P,X12
        WRITE (2, 22)
        WRITE (2,  3) PEL1,PFE,PDOP,PMEX,PFE0,PSAG,P1
        RETURN
        END
C
