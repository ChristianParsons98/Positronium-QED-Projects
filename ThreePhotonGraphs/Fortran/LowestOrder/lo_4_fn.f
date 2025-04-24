      double precision function fxn(xx,wgt)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c Region 4: 1<p<infinty, 1<q<infinity
c Limits are 0 to 1 for xx(1), xx(2)
c and -1 to 1 for xx(3)
      xp = xx(1)
      xq = xx(2)
      ut = xx(3)
c
      p = (1.d0+xp)/(1.d0-xp)
      q = (1.d0+xq)/(1.d0-xq)
      psp = 0.5*(1.+p)**2
      psq = 0.5*(1.+q)**2
      ps = p*p*q*q*psp*psq
      wp = dsqrt(p*p+1.)
      wq = dsqrt(q*q+1.)
      s = dsqrt(p*p+q*q+2.*p*q*ut)
      ws = dsqrt(s*s+1.)
c
      inta =0
c
      intb =0
c
      intc =0
c
      intd =0
c
      inte =0
c
      intf =0
c
      int = inta+intb+intc+intd+inte+intf
      fxn = (2./3.)*ps*int
      return
      end
