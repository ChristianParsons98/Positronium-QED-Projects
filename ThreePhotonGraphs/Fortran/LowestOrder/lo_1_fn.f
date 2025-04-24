      double precision function fxn(xx,wgt)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c Region 1: 0<p+q<1
c Limits are 0 to 1 for xx(1), xx(2)
c and -1 to 1 for ut=xx(3)
      xp = xx(1)
      xq = xx(2)
      ut = xx(3)
c
      p = xp*(1.d0-xq)
      q = xp*xq
      ps = p*p*q*q*xp
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
