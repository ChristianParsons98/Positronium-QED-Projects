      double precision function fxn(xx,wgt)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c Region 2: the decay triangle p+q>1, p<1, q<1
c Limits are 0 to 1 for xx(1), xx(2)
c and -1 to 1 for xx(3)
      xp = xx(1)
      xq = xx(2)
      ut = xx(3)
c
      p = xp
      q = 1.-xp*(1.-xq)
      ps = p*p*q*q*xp
      wp = dsqrt(p*p+1.)
      wq = dsqrt(q*q+1.)
      s = dsqrt(p*p+q*q+2.*p*q*ut)
      ws = dsqrt(s*s+1.)
      ubar = 1.+2.*(1.-p-q)/(p*q)
c
      intaNC =0
c
      intbNC =0
c
      intcNC =0
c
      intdNC =0
c
      inteNC =0
c
      intfNC =0
c
      gNC = intaNC+intbNC+intcNC+intdNC+inteNC+intfNC
      fxn = (2./3.)*ps*gNC
      return
      end
