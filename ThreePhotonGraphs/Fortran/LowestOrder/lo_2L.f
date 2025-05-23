c *** lo_2L.f
c *** three-photon-annihilation energy
c *** Region 2: the decay triangle, log part
c *** units of m alpha^7/pi^3
c *** result is 
c *** 

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	double precision lam
	COMMON/BVEG1/NCALL,ITMX,NPRN,NDEV,XL(16),XU(16),ACC
	COMMON/BVEG2/IT,NDO
	common/bveg2a/SI,SWGT,SCHI,XI(50,16)
	COMMON/BVEG5/DSEED
	COMMON/CNSTS/PI,zeta2
	dimension xx(16)
c	OPEN(UNIT=2,FILE='lo_2L.out',STATUS='NEW')
c	OPEN(UNIT=3,FILE='lo_2L.dat1',STATUS='OLD')
c	OPEN(UNIT=4,FILE='lo_2L.dat1',STATUS='NEW')
c	READ(3,*)IT,NDO,SI,SWGT,SCHI,DSEED,XI
	PI = 3.14159265358979323846264338327950D0
	zeta2 = pi*pi/6.d0
	ITMX	= 10
	NPRN	= 0
	NDIM	= 2
	NDEV	= 2
	NCALL	= 1 000
	CALL VEGAS(NDIM,Z,AVGI,SD,CHI2A)
	NCALL	= 10 000
	CALL VEGAS1(NDIM,Z,AVGI,SD,CHI2A)
	NCALL	= 100 000
	CALL VEGAS1(NDIM,Z,AVGI,SD,CHI2A)
	NCALL	= 1 000 000
c	CALL VEGAS1(NDIM,Z,AVGI,SD,CHI2A)
	NCALL	= 10 000 000
c	CALL VEGAS1(NDIM,Z,AVGI,SD,CHI2A)
	NCALL	= 100 000 000
c	CALL VEGAS1(NDIM,Z,AVGI,SD,CHI2A)
	NCALL	= 1 000 000 000
c	CALL VEGAS1(NDIM,Z,AVGI,SD,CHI2A)
c	WRITE(4,*)IT,NDO,SI,SWGT,SCHI,DSEED,XI
	CLOSE(UNIT=2,STATUS='KEEP')
c	CLOSE(UNIT=3,STATUS='KEEP')
c	CLOSE(UNIT=4,STATUS='KEEP')
	END
c
c
      INCLUDE 'lo_2L_fn.f'
      INCLUDE 'vegas.f'
c
c
