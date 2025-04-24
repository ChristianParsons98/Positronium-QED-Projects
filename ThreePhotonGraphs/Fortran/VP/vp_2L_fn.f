      double precision function fxn(xx,wgt)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c Region 2: the decay triangle p+q>1, p<1, q<1
c Limits are 0 to 1 for xx(1,2)
c Limits are 0 to pi for xx(4) and 0 to 2pi for xx(5)
      xp = xx(1)
      xq = xx(2)
      xr = xx(3)
      thr = xx(4)
      phr = xx(5)
c
c
c
c
      fxx = (fxns(xp,xq,xr,   thr,   phr)
     1     + fxns(xp,xq,xr,pi-thr,pi+phr))/2.
c
c
      fxn = fxx
      return
      end
c
      double precision function fxns(xp,xq,xr,thr,phr)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c
c
      p = xp
      q = 1.-xp*(1.-xq)
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
      wpr = dsqrt(p*p+r*r-2.*pr+1.)
      wprp = dsqrt(p*p+r*r+2.*pr+1.)
      wpq = dsqrt(p*p+q*q+2.*pq+1.)
      wqr = dsqrt(q*q+r*r+2.*qr+1.)
      wpqr = dsqrt(p*p+q*q+r*r+2.*qr+2*pq+2*pr+1.)
      mp = dsqrt(p*p+m)
      mq = dsqrt(q*q+m)
      mr = dsqrt(r*r+m)
      mpr = dsqrt(p*p+r*r-2.*pr+m)
      mprp = dsqrt(p*p+r*r+2.*pr+m)
      mpq = dsqrt(p*p+q*q+2.*pq+m)
      mqr = dsqrt(q*q+r*r+2.*qr+m)
      mpqr = dsqrt(p*p+q*q+r*r+2.*qr+2*pq+2*pr+m)
      ps = p*p*q*q*r*r*dsin(thr)*(1+r)**2*xp
      s = dsqrt(p*p+q*q+2.*p*q*ut)
      logfac = dlog((1.-ut)/(1.+ut))
c
      fcut =        -tra(p,q,-2 + p + wpr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*q*(-1 + p - wp)*(-1 + p + wp)*wpr*(-1 + q - wq)*(-1 + q + wq)*
     -     (-2 + p + q + wpr - wqr)*(-2 + p + q + wpr + wqr)*
     -     (-2 + p + wpr - wr)*(p + wpr - wr)*(-2 + p + wpr + wr)*
     -     (p + wpr + wr)) + tra(p,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*q*(-1 + p - wp)*(-1 + p + wp)*(-1 + q - wq)*(-1 + q + wq)*
     -     (p - wpr - wr)*(p + wpr - wr)*wr*(-2 + q - wqr + wr)*
     -     (-2 + q + wqr + wr)*(-2 + 2*wr)) - 
     -  tra(p,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*q*(-1 + p - wp)*(-1 + p + wp)*(-1 + q - wq)*(-1 + q + wq)*
     -     (-2 + p - wpr - wr)*(-2 + p + wpr - wr)*wr*(q - wqr + wr)*
     -     (q + wqr + wr)*(2 + 2*wr)) + 
     -  ((-2 + p + q - s)*tra(p,2 - p - s,-2 + p + s + wqr,pp,qq,rr,pq,pr,qr,
     -      m))/
     -   (8.*p*(2 - p + q - s)*s*(-1 + p - wp)*(-1 + p + wp)*(1 - p - s - wq)*
     -     (1 - p - s + wq)*(-s - wpr - wqr)*(-s + wpr - wqr)*wqr*
     -     (-2 + p + s + wqr - wr)*(p + s + wqr - wr)*(-2 + p + s + wqr + wr)*
     -     (p + s + wqr + wr)) + 
     -  trb(p,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*q*(-1 + p - wp)*(-1 + p + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*
     -     (-2 + p + q - wpqr + wr)*(-2 + p + q + wpqr + wr)*
     -     (-2 + q - wqr + wr)*(-2 + q + wqr + wr)*(-2 + 2*wr)) - 
     -  trb(p,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*q*(-1 + p - wp)*(-1 + p + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*
     -     (p + q - wpqr + wr)*(p + q + wpqr + wr)*(q - wqr + wr)*
     -     (q + wqr + wr)*(2 + 2*wr)) + 
     -  ((-2 + p + q - s)*trb(p,2 - p - s,-2 + p + s + wqr,pp,qq,rr,pq,pr,qr,
     -      m))/
     -   (8.*p*(2 - p + q - s)*s*(-1 + p - wp)*(-1 + p + wp)*(1 - p - s - wq)*
     -     (1 - p - s + wq)*wqr*(p - wpqr + wqr)*(p + wpqr + wqr)*
     -     (-2 + p + s + wqr - wr)*(p + s + wqr - wr)*(-2 + p + s + wqr + wr)*
     -     (p + s + wqr + wr)) + 
     -  ((-2 + p + q - s)*trb(2 - q - s,q,-2 + s + wpqr,pp,qq,rr,pq,pr,qr,m))/
     -   (8.*q*(2 + p - q - s)*s*(1 - q - s - wp)*(1 - q - s + wp)*wpqr*
     -     (-1 + q - wq)*(-1 + q + wq)*(-2 + q + s + wpqr - wqr)*
     -     (-2 + q + s + wpqr + wqr)*(-2 + s + wpqr - wr)*(s + wpqr - wr)*
     -     (-2 + s + wpqr + wr)*(s + wpqr + wr)) + 
     -  trc(p,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*q*(-1 + p - wp)*(-1 + p + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*
     -     (-2 + p + q - wpqr + wr)*(-2 + p + q + wpqr + wr)*
     -     (-2 + p - wprp + wr)*(-2 + p + wprp + wr)*(-2 + 2*wr)) - 
     -  trc(p,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*q*(-1 + p - wp)*(-1 + p + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*
     -     (p + q - wpqr + wr)*(p + q + wpqr + wr)*(p - wprp + wr)*
     -     (p + wprp + wr)*(2 + 2*wr)) + 
     -  ((-2 + p + q - s)*trc(2 - q - s,q,-2 + s + wpqr,pp,qq,rr,pq,pr,qr,m))/
     -   (8.*q*(2 + p - q - s)*s*(1 - q - s - wp)*(1 - q - s + wp)*wpqr*
     -     (-q + wpqr - wprp)*(-q + wpqr + wprp)*(-1 + q - wq)*(-1 + q + wq)*
     -     (-2 + s + wpqr - wr)*(s + wpqr - wr)*(-2 + s + wpqr + wr)*
     -     (s + wpqr + wr)) + ((-2 + p + q - s)*
     -     trc(2 - q - s,q,-2 + q + s + wprp,pp,qq,rr,pq,pr,qr,m))/
     -   (8.*q*(2 + p - q - s)*s*(1 - q - s - wp)*(1 - q - s + wp)*wprp*
     -     (q - wpqr + wprp)*(q + wpqr + wprp)*(-1 + q - wq)*(-1 + q + wq)*
     -     (-2 + q + s + wprp - wr)*(q + s + wprp - wr)*
     -     (-2 + q + s + wprp + wr)*(q + s + wprp + wr))
      fxns = (2./pi)*(2./3.)*ps*fcut*logfac/(-2.*p*q)
      return
      end
