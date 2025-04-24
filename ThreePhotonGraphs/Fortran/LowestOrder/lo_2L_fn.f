      double precision function fxn(xx,wgt)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c Region 2: the decay triangle p+q>1, p<1, q<1
c Limits are 0 to 1 for xx(1), xx(2)
      xp = xx(1)
      xq = xx(2)
c
      p = xp
      q = 1.-xp*(1.-xq)
      ps = p*p*q*q*xp
      ubar = 1.+2.*(1.-p-q)/(p*q)
      logfac = dlog((1.-ubar)/(1.+ubar))
c
      fcut =0
      fxn = (2./3.)*ps*fcut*logfac/(-2.*p*q)
      return
      end
