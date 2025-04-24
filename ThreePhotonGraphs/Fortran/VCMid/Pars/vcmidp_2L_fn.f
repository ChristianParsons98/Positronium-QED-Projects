      double precision function fxn(xx,wgt)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c Region 2: the decay triangle p+q>1, p<1, q<1
c Limits are 0 to 1 for xx(1,2)
c Limits are 0 to pi for xx(4) and 0 to 2pi for xx(5)
      xp = xx(1)
      xq = xx(2)
      x = xx(3)
      u = xx(4)
c
c
c
c
      fxx = fxns(xp,xq,x,u)
c
c
      fxn = fxx
      return
      end
c
      double precision function fxns(xp,xq,x,u)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c
c
      p = xp
      q = 1.-xp*(1.-xq)
      a = u+x-2.*u*x-1.+x*u*u
      b = (1./a)*(2.-2.*u-2.*x+6.*u*x-4.*x*u*u)
      c = (1./a)*(4.*x*u*u-4.*x*u+x)
      d = c-(1./4.)*b*b
      wv = dsqrt(p*p-d)
      r = xr/(1.-xr)
      ut = 1.+2.*(1.-p-q)/(p*q)
      sut = dsqrt(1.-ut*ut)
      pq = p*q*ut
      pr = p*r*dcos(thr)
      qr = q*r*(ut*dcos(thr)+sut*dsin(thr)*dcos(phr))
      pp = p*p
      qq = q*q
      rr = r*r
      wp = dsqrt(p*p+1.)
      wq = dsqrt(q*q+1.)
      wr = dsqrt(r*r+1.)
      wpr = dsqrt(p*p+r*r+2.*pr+1.)
      wpq = dsqrt(p*p+q*q+2.*pq+1.)
      wqr = dsqrt(q*q+r*r-2.*qr+1.)
      ps = p*p*q*q*xp
      s = dsqrt(p*p+q*q+2.*p*q*ut)
      logfac = dlog((1.-ut)/(1.+ut))
c
      fcut =        tra(p,q,pp,qq,pq,x,u)/
     -   (4.*p*q*(-1 + p - wp)**2*(-1 + p + wp)**2*(-1 + q - wq)**2*
     -     (-1 + q + wq)**2*(b/2. + p - wv)*(b/2. + p + wv)) + 
     -  trb(p,q,pp,qq,pq,x,u)/
     -   (4.*p*q*(-1 + p - wp)*(-1 + p + wp)*(-1 + p + q - wpq)*
     -     (-1 + p + q + wpq)*(-1 + q - wq)**2*(-1 + q + wq)**2*
     -     (b/2. + p - wv)*(b/2. + p + wv)) + 
     -  trc(p,q,pp,qq,pq,x,u)/
     -   (4.*p*q*(-1 + p - wp)**2*(-1 + p + wp)**2*(-1 + p + q - wpq)*
     -     (-1 + p + q + wpq)*(-1 + q - wq)*(-1 + q + wq)*(b/2. + p - wv)*
     -     (b/2. + p + wv))
      fxns = -1.d0*(2./3.)*(ps/a)*fcut*logfac/(-2.*p*q)
      return
      end
