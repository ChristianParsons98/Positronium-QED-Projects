      double precision function fxn(xx,wgt)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c The non-logarithmic real part
c Upper limits are 1, 1, pi, for xx(1-3)
      xp = xx(1)
      xq = xx(2)
      thq = xx(3)
c
c Region S: full subtracted integration
      p = xp/(1.-xp)
      q = xq/(1.-xq)
      pqpsfac = ((1.+p)*(1.+q))**2
c Region 1: p>0, q>0, p+q<1
c      p = xp*(1.-xq)
c      q = xp*xq
c      pqpsfac = xp
c Region 1A: p>q, p+q<1
c      p = xp*(1.-xq/2.)
c      q = xp*xq/2.
c      pqpsfac = xp/2.
c Region 1B: p<q, p+q<1
c      p = xp*xq/2.
c      q = xp*(1.-xq/2.)
c      pqpsfac = xp/2.
c Region 2: p<1, q<1, p+q>1
c      p = xp
c      q = 1.-xp*(1.-xq)
c      pqpsfac = xp
c Region 2A: p<q<1, p+q>1
c      p = xp*(1.+xq)/2.
c      q = 1.-xp*(1.-xq)/2.
c      pqpsfac = xp/2.
c Region 2B: q<p<1, p+q>1
c      p = 1.-xp*(1.-xq)/2.
c      q = xp*(1.+xq)/2.
c      pqpsfac = xp/2.
c Region 3A: p>1, 0<q<1
c      p = (1.+xp)/(1.-xp)
c      q = xq
c      pqpsfac = 0.5*(1.+p)**2
c Region 3B: 0<p<1, q>1
c      p = xp
c      q = (1.+xq)/(1.-xq)
c      pqpsfac = 0.5*(1.+q)**2
c Region 4: p>1, q>1
c      p = (1.+xp)/(1.-xp)
c      q = (1.+xq)/(1.-xq)
c      pqpsfac = 0.25*((1.+p)*(1.+q))**2
c
      wp = dsqrt(p*p+1.)
      wq = dsqrt(q*q+1.)
      cthq = dcos(thq)
      sthq = dsin(thq)
      pp = p*p
      qq = q*q
      pq = p*q*cthq
      s  = dsqrt(pp+2.*pq+qq)
      ws = dsqrt(s*s+1.)
      ps = sthq*pqpsfac
      fac = p*p*q*q*r*r
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
      hh = inta+intb+intc+intd+inte+intf
c
c *** if inside decay triangle then apply subtraction ***
      one = 1.d0
      IF ((p.LT.one) .AND. (q.LT.one) .AND. ((p+q).GT.one)) THEN
      cthqbar = 1.+2.*(1.-p-q)/(p*q)
      sthqbar = 2.*dsqrt((1.-p)*(1.-q)*(p+q-1.))/(p*q)
      pq = p*q*cthqbar
      s  = dsqrt(pp+2.*pq+qq)
      ws = dsqrt(s*s+1.)
c
      gbara =0
c
      gbarb =0
c
      gbarc =0
c
      gbard =0
c
      gbare =0
c
      gbarf =0
c
      gbar = gbara+gbarb+gbarc+gbard+gbare+gbarf
      h = hh+2.*(2.-p-q)*gbar/(4.*(1.-p-q)+2.*p*q*(1.-cthq))
      ELSE
      h = hh
      END IF
c
      fxn = (2./(3.*pi))*ps*fac*h
      return
      end
