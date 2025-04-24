      double precision function fxn(xx,wgt)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c Region 2: the decay triangle p+q>1, p<1, q<1
c Limits are 0 to 1 for xx(1,2)
      xp = xx(1)
      xq = xx(2)
c
      p = xp
      q = 1.-xp*(1.-xq)
      wp = dsqrt(p*p+1.)
      wq = dsqrt(q*q+1.)
      ps = p*p*q*q*xp
c
      fcut =0
      fxn = (2.d0/3.d0)*ps*pi*fcut/(-2.*p*q)
      return
      end
