c *** lo_im.f
c *** three-photon-annihilation energy
c *** Region 2: the decay triangle, imaginary part
c *** Integration limits are 0 to 1 for variables 1, 2, 3, 4
c *** units of m alpha^7/pi^3
c *** result is x) 1.04 (10*100M)
c *** 

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	double precision lam
	COMMON/BVEG1/NCALL,ITMX,NPRN,NDEV,XL(16),XU(16),ACC
	COMMON/BVEG2/IT,NDO
	common/bveg2a/SI,SWGT,SCHI,XI(50,16)
	COMMON/BVEG5/DSEED
	COMMON/CNSTS/PI,zeta2
	dimension xx(16)
c	OPEN(UNIT=2,FILE='lo_im.out',STATUS='NEW')
c	OPEN(UNIT=3,FILE='lo_im.dat1',STATUS='OLD')
c	OPEN(UNIT=4,FILE='lo_im.dat1',STATUS='NEW')
c	READ(3,*)IT,NDO,SI,SWGT,SCHI,DSEED,XI
	PI = 3.14159265358979323846264338327950D0
	zeta2 = pi*pi/6.d0
	ITMX	= 10
	NPRN	= 0
	NDIM	= 6
	NDEV	= 2
        xu(1) = 1.d0
        xu(2) = 1.d0
        xu(3) = 1.d0
        xu(4) = pi
        xu(5) = 2.d0*pi
        xl(6) = -1.d0
        xu(6) = 1.d0
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
c	CLOSE(UNIT=2,STATUS='KEEP')
c	CLOSE(UNIT=3,STATUS='KEEP')
c	CLOSE(UNIT=4,STATUS='KEEP')
	END
c
c
      INCLUDE 'dblvert_3B_fn.f'
      INCLUDE 'trdvreout2.f'
      INCLUDE 'vegas.f'
c
c