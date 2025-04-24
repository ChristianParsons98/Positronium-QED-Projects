      double precision function fxn(xx,wgt)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c The logarithmic real part
c Upper limits are 1, 1, 1, pi, 2*pi for xx(1-5)
      xp = xx(1)
      xq = xx(2)
      xr = xx(3)
      thr = xx(4)
      phr = xx(5)
c
      fxn = (fxns(xp,xq,xr,thr,phr)+fxns(xp,xq,xr,pi-thr,pi+phr))/2.
      return
      end
c
c
      double precision function fxns(xp,xq,xr,thr,phr)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
c
      p = xp
      q = 1.-xp*(1.-xq)
      r = xr/(1.-xr)
      wp = dsqrt(p*p+1.)
      wq = dsqrt(q*q+1.)
      wr = dsqrt(r*r+1.)
      cthq = 1.+2.*(1.-p-q)/(p*q)
      sthq = 2.*dsqrt((1.-p)*(1.-q)*(p+q-1.))/(p*q)
      cthr = dcos(thr)
      sthr = dsin(thr)
      cphr = dcos(phr)
      pp = p*p
      qq = q*q
      rr = r*r
      pq = p*q*cthq
      pr = p*r*cthr
      qr = q*r*(sthq*sthr*cphr+cthq*cthr)
      wqpr = dsqrt(rr+2.*qr+qq+1.)
      wpmr = dsqrt(rr-2.*pr+pp+1.)
      ps = xp*(1.+r)**2*dsin(thr)
      fac = p*q*r*r*(2.-p-q)
      logfac = dlog((p+q-1.)/((1.-p)*(1.-q)))/pi
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
      fxns = (2./3.)*ps*fac*logfac*gbar
      return
      end
