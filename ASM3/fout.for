C            ASD3, ASM3
C					FOUT
C       ASM3, 02.08
C		Variant 2010
C           Variant 8 2012
$DEBUG			
      SUBROUTINE FOUT(INFST)
        INTEGER YE,MO,DA,       YE9,MO9,        DA9,
     *          YE1,YE2,YE3,    MO1,MO2,MO3,    DA1,DA2,DA3,
     *          YE4,YE5,YE6,YE7,MO4,MO5,MO6,MO7,DA4,DA5,DA6,DA7,
     *          YE13,YE246,     MO13,MO246,     DA13,DA246
	  integer i
        character infst*8, stom*7, info*8
        logical lexi, lexiD
	  character*1 char1,char2,char3,char4, char8*8
	  integer i1,i2,i3,i4,i5 
C
 170    FORMAT(//,10X,'     Call author        !',//,
     *   '  D.Jetchev,tel.(02)884 58 41, email:jetch@tu-sofia.bg',//)
C	     i1=ichar('A')
C	     i2=ichar('a')
C	     write(*,101) i1,i2
C  101     format(' A ',i4,' a ',i4)
C  	     i3=ichar('Z')
C	     i4=ichar('z')
	     
C         char8='112m2'
C	    do 110 i=1,5
C            i5=ichar(char8(i:i))
C            if(i5 .GT. i2 .AND. i5 .LT. i4) THEN
C	         i5=i5-32
C               char8(i:i)=   char(i5)
C	      endif
C  110     continue
C          write(*,102) char8
C  102     FORMAT(1x,  A8)
C 
        GOTO 305
 302       write(*,*) ' Error in information file '
           stop  005
 305	  INQUIRE(FILE='OUT.INF',EXIST=LEXI,IOSTAT=IOS)
        IF ( .not.LEXI) THEN
           WRITE(*,*)
           WRITE(*,*) '     The program package is not complete!'
           WRITE(*,170)
           STOP 006
        ENDIF
C 
        OPEN(14,FILE='OUT.INF',STATUS='OLD',RECL=8,ACCESS='sequential',
     *        FORM='FORMATTED')
        INQUIRE(FILE='OUTD.INF',EXIST=LEXI,IOSTAT=IOS)
        if(lexi) then
           OPEN(4,FILE='OUTD.INF',STATUS='OLD',RECL=8,ACCESS='direct',
     *        FORM='FORMATTED') 
	     CLOSE(4,STATUS="delete")
	  endif
	  OPEN(4,FILE='OUTD.INF',STATUS='NEW',RECL=8,ACCESS='direct',
     *        FORM='FORMATTED')
C      
C         Fajl 4: OUTD.inf e direct       Fajl 14: OUT.inf e sequential
C
        read(14,301)   YE,MO,DA
	  write(4,301,REC=1)   YE,MO,DA
C	     write(*,294) YE,MO,DA
	  DO 290 i=2,9
	     read(14,301) YE,MO,DA
		 write(4,301,REC=i) YE,MO,DA
 290    continue
 	  DO 291 i=10,20
	     read(14,292,END=293) info
		 write(4,292,REC=i) info
 291    continue
 292    format(a8)  	 
 295    format(1x,a8)
 301    format(i4,i2,i2)
C
 293    continue			
        READ(4,301,REC=1, ERR=302)YE1,MO1,DA1
	  READ(4,301,REC=2, ERR=302)YE2,MO2,DA2
        READ(4,301,REC=3, ERR=302)YE3,MO3,DA3
C      
        YE13=YE1+YE3
        MO13=MO1+MO3
        DA13=DA1+DA3
        READ(4,301,REC=5,ERR=302)YE5,MO5,DA5
C      .....
        IF(YE13 .NE. YE5 .OR. MO13 .NE. MO5 .OR. DA13 .NE. DA5) THEN
          WRITE(*,*)
          WRITE(*,*) '     The program package is not complete!'
          WRITE(*,170)
          STOP 005
        ENDIF
        READ(4,301,REC=2,ERR=302)YE2,MO2,DA2
        READ(4,301,REC=4,ERR=302)YE4,MO4,DA4
        READ(4,301,REC=6,ERR=302)YE6,MO6,DA6
C       ......
        YE246=YE2+YE4+YE6
        MO246=MO2+MO4+MO6
        DA246=DA2+DA4+DA6
        READ(4,301,REC=7,ERR=302)YE7,MO7,DA7
C      ......
        IF(YE246 .NE. YE7 .OR. MO246 .NE. MO7 .OR. DA246 .NE. DA7) THEN
          WRITE(*,*)
          WRITE(*,*) '     The information is incorect !'
          WRITE(*,170)
          STOP 007
        ENDIF
        CALL GETDAT (YE,MO,DA)
        IF ( YE .LT. YE4) THEN
          WRITE(*,*)  '  Error:    date ! '
          WRITE(*,306) YE4,MO4,DA4
 306      FORMAT(' The last execution was on date:',i4,'-',i2,'-',i2)
          WRITE(*,*)'  Check the computer date '
          WRITE(*,*)'  Enter the correct current date'
          STOP 001
        ENDIF
        IF ( YE .GT. YE6) THEN
          WRITE(*,*)
          WRITE(*,*)
          WRITE(*,303) YE6
 303      FORMAT(5X,'The program is valid until end of',i4,'year')
          WRITE(*,170)
          WRITE(4,301,REC=8,ERR=302)YE,MO,DA
          READ(4,301,REC=9,ERR=302)YE9,MO9,DA9
          YE9 = YE9 + 1
          WRITE(*,*)'  3 extra execution allowed !'
          WRITE(*,304) YE9
 304      FORMAT('    This is number ',i2)
          IF ( YE9 .GT. 3) THEN
            WRITE(*,*) '     Sorry !  Jetchev.'
            STOP 007
          ELSE
            WRITE(4,301,REC=9,ERR=302)YE9,MO9,DA9
          ENDIF
        ELSE
          WRITE(4,301,REC=4,ERR=302)YE,MO,DA
          YE246=YE2+YE +YE6
          MO246=MO2+MO +MO6
          DA246=DA2+DA +DA6
          WRITE(4,301,REC=7,ERR=302)YE246,MO246,DA246
        ENDIF
        READ(4,2  ,REC=10,ERR=302)infst
  2     format(a8)
        READ(4,4,REC=11,ERR=302)STOM
        IF(STOM .EQ. ' ') THEN
           STOM='2212I'
           WRITE(4,4,REC=11,ERR=302)STOM
        ENDIF
        READ(4,4,REC=12,ERR=302)STOM
        IF(STOM .EQ. ' ') THEN
           STOM='2212I'
           WRITE(4,4,REC=12,ERR=302)STOM
        ENDIF
  4     FORMAT(A8)
        RETURN
      END
