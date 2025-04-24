      double precision function fxn(xx,wgt)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c The imaginary part
c Upper limits are 1, 1 for xx(1-2)
      xp = xx(1)
      xq = xx(2)
c
      p = xp
      q = 1.-xp*(1.-xq)
      wp = dsqrt(p*p+1.)
      wq = dsqrt(q*q+1.)
      cthq = 1.+2.*(1.-p-q)/(p*q)
      pp = p*p
      qq = q*q
      pq = p*q*cthq
      ps = xp
      fac = p*q*(2.-p-q)
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
      gbar = inta+intb+intc+intd+inte+intf
c
      fxn = (2./3.)*ps*fac*gbar
      return
      end
