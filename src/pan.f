      SUBROUTINE PAN(A, M, C, N, RESULT)
C
C     TRANSLATION OF AMENDED VERSION OF APPLIED STATISTICS ALGORITHM
C     AS 153 (AS R52), VOL. 33, 363-366, 1984.
C     BY R.W. FAREBROTHER  (ORIGINALLY NAMED GRADSOL OR PAN)
C
C     GRADSOL EVALUATES THE PROBABILITY THAT A WEIGHTED SUM OF
C     SQUARED STANDARD NORMAL VARIATES DIVIDED BY X TIMES THE UNWEIGHTED
C     SUM IS LESS THAN A GIVEN CONSTANT, I.E. THAT
C     A1.U1**2 + A2.U2**2 + ... + AM.UM**2 <
C     X*(U1**2 + U2**2 + ... + UM**2) + C
C     WHERE THE U'S ARE STANDARD NORMAL VARIABLES.
C     FOR THE DURBIN-WATSON STATISTIC, X = DW, C = 0, AND
C     A ARE THE NON-ZERO EIGENVALUES OF THE "M*A" MATRIX.
C
C     THE ELEMENTS A(I) MUST BE ORDERED.  A(0) = X
C     N = THE NUMBER OF TERMS IN THE SERIES.   THIS DETERMINES THE
C     ACCURACY AND ALSO THE SPEED.   NORMALLY N SHOULD BE ABOUT 10-15.
C     --------------
C     ORIGINALLY FROM STATLIB.  REVISED 5/3/1996 BY CLINT CUMMINS:
C     1. DIMENSION A STARTING FROM 0  (FORTRAN 77)
C        IF THE USER DOES NOT INITIALIZE A(0) = X,
C        THERE WOULD BE UNPREDICTABLE RESULTS, SINCE A(0) IS ACCESSED
C        WHEN J2=0 FOR THE FINAL DO 60 LOOP.
C     2. USE X VARIABLE TO AGREE WITH PUBLISHED CODE
C     3. FIX BUG 2 LINES BELOW  DO 60 L2 = J2, NU, D
C        PROD = A(J2)  -->  PROD = A(L2)
C        (PRIOR TO THIS FIX, ONLY THE TESTS WITH M=3 WORKED CORRECTLY)
C     4. TRANSLATE TO UPPERCASE AND REMOVE TABS
C     TESTED SUCCESSFULLY ON THE FOLLOWING BENCHMARKS:
C     1. FAREBROTHER 1984 TABLE (X=0):
C        A           C   PROBABILITY
C        1,3,6       1   .0542
C        1,3,6       7   .4936
C        1,3,6      20   .8760
C        1,3,5,7,9   5   .0544
C        1,3,5,7,9  20   .4853
C        1,3,5,7,9  50   .9069
C        3,4,5,6,7   5   .0405
C        3,4,5,6,7  20   .4603
C        3,4,5,6,7  50   .9200
C     2. DURBIN-WATSON 1951/71 SPIRITS DATASET, FOR X=.2,.3,...,3.8, C=0
C        COMPARED WITH BETA APPROXIMATION  (M=66), A SORTED IN REVERSE ORDER
C     3. JUDGE, ET AL 2ND ED. P.399 DATASET, FOR X=.2,.3,...,3.8, C=0
C        COMPARED WITH BETA APPROXIMATION  (M=8), A SORTED IN EITHER ORDER
C
      INTEGER M, N
      DOUBLE PRECISION A(0:M), C, X, RESULT
C
C     LOCAL VARIABLES
C
      INTEGER D, H, I, J1, J2, J3, J4, K, L1, L2, NU, N2
      DOUBLE PRECISION NUM, PIN, PROD, SGN, SUM, SUM1, U, V, Y
      DOUBLE PRECISION ZERO, ONE, HALF, TWO
      DATA ZERO/0.D0/, ONE/1.D0/, HALF/0.5D0/, TWO/2.D0/
C
C     SET NU = INDEX OF 1ST A(I) >= X.
C     ALLOW FOR THE A'S BEING IN REVERSE ORDER.
C
      IF (A(1) .GT. A(M)) THEN
        H = M
        K = -1
        I = 1
      ELSE
        H = 1
        K = 1
        I = M
      ENDIF
      X = A(0)
      DO 10 NU = H, I, K
        IF (A(NU) .GE. X) GO TO 20
   10 CONTINUE
C
C     IF ALL A'S ARE -VE AND C >= 0, THEN PROBABILITY = 1.
C
      IF (C .GE. ZERO) THEN
        RESULT = ONE
        RETURN
      ENDIF
C
C     SIMILARLY IF ALL THE A'S ARE +VE AND C <= 0, THEN PROBABILITY = 0.
C
   20 IF (NU .EQ. H .AND. C .LE. ZERO) THEN
        RESULT = ZERO
        RETURN
      ENDIF
C
      IF (K .EQ. 1) NU = NU - 1
      H = M - NU
      IF (C .EQ. ZERO) THEN
        Y = H - NU
      ELSE
        Y = C * (A(1) - A(M))
      ENDIF
C
      IF (Y .GE. ZERO) THEN
        D = 2
        H = NU
        K = -K
        J1 = 0
        J2 = 2
        J3 = 3
        J4 = 1
      ELSE
        D = -2
        NU = NU + 1
        J1 = M - 2
        J2 = M - 1
        J3 = M + 1
        J4 = M
      ENDIF
      PIN = TWO * DATAN(ONE) / N
      SUM = HALF * (K + 1)
      SGN = K / DBLE(N)
      N2 = N + N - 1
C
C       FIRST INTEGRALS
C
      DO 70 L1 = H-2*(H/2), 0, -1
        DO 60 L2 = J2, NU, D
          SUM1 = A(J4)
C         FIX BY CLINT CUMMINS 5/3/96
C          PROD = A(J2)
          PROD = A(L2)
          U = HALF * (SUM1 + PROD)
          V = HALF * (SUM1 - PROD)
          SUM1 = ZERO
          DO 50 I = 1, N2, 2
            Y = U - V * DCOS(DBLE(I)*PIN)
            NUM = Y - X
            PROD = DEXP(-C/NUM)
            DO 30 K = 1, J1
              PROD = PROD * NUM / (Y - A(K))
   30       CONTINUE
            DO 40 K = J3, M
              PROD = PROD * NUM / (Y - A(K))
   40       CONTINUE
            SUM1 = SUM1 + DSQRT(DABS(PROD))
   50       CONTINUE
          SGN = -SGN
          SUM = SUM + SGN * SUM1
          J1 = J1 + D
          J3 = J3 + D
          J4 = J4 + D
   60   CONTINUE
C
C       SECOND INTEGRAL.
C
        IF (D .EQ. 2) THEN
          J3 = J3 - 1
        ELSE
          J1 = J1 + 1
        ENDIF
        J2 = 0
        NU = 0
   70 CONTINUE
C
      RESULT = SUM
      RETURN
      END
