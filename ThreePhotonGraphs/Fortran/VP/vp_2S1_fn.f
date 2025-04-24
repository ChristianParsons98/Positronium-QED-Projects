      double precision function fxn(xx,wgt)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c Region 2: the decay triangle p+q>1, p<1, q<1
c Limits are 0 to 1 for xx(1), xx(2)
c and -1 to 1 for xx(3)
      xp = xx(1)
      xq = xx(2)
      xr = xx(3)
      thr = xx(4)
      phr = xx(5)
      ut = xx(6)
c
c
c
c
      fxx = (fxnn(xp,xq,xr,   thr,   phr,ut)
     1     + fxnn(xp,xq,xr,pi-thr,pi+phr,ut))/2.
c
c
      fxn = fxx
      return
      end
c
      double precision function fxnn(xp,xq,xr,thr,phr,ut)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c
c
      p = xp
      q = 1.-xp*(1.-xq)
      r = xr/(1.-xr)
      ps = p*p*q*q*r*r*dsin(thr)*(1+r)**2*xp
      ubar = 1.+2.*(1.-p-q)/(p*q)
      den = -2*p*q*(ut-ubar)
c
      fna = fnu(p,q,r,thr,phr,ut)
      fnb = fnbar(p,q,r,thr,phr)
      fxnn = (2./(pi))*(2./3.)*ps*(fna-(fnb/(den)))
      return
      end
c
c
c
c
c
      double precision function fnu(p,q,r,thr,phr,ut)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c
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
      s = dsqrt(p*p+q*q+2.*p*q*ut)
c
      fnua =        tra(-p,2 + p + s,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*s*(2 + p - q + s)*(2 + p + q + s)*(-1 - p - wp)*(-1 - p + wp)*
     -     (1 + p + s - wq)*(1 + p + s + wq)*(-p - wpr - wr)*(-p + wpr - wr)*
     -     wr*(p + s - wqr + wr)*(p + s + wqr + wr)*(-2 + 2*wr)) - 
     -  tra(-p,2 + p + s,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*s*(2 + p - q + s)*(2 + p + q + s)*(-1 - p - wp)*(-1 - p + wp)*
     -     (1 + p + s - wq)*(1 + p + s + wq)*(-2 - p - wpr - wr)*
     -     (-2 - p + wpr - wr)*wr*(2 + p + s - wqr + wr)*
     -     (2 + p + s + wqr + wr)*(2 + 2*wr)) - 
     -  tra(p,-q,q + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*q*(-2 + p - q - s)*(-2 + p - q + s)*(-1 + p - wp)*
     -     (-1 + p + wp)*(-1 - q - wq)*(-1 - q + wq)*(-2 + p - q - wpr - wqr)*
     -     (-2 + p - q + wpr - wqr)*wqr*(q + wqr - wr)*(2 + q + wqr - wr)*
     -     (q + wqr + wr)*(2 + q + wqr + wr)) - 
     -  tra(p,q,-2 + p + wpr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*q*(-2 + p + q - s)*(-2 + p + q + s)*(-1 + p - wp)*
     -     (-1 + p + wp)*wpr*(-1 + q - wq)*(-1 + q + wq)*
     -     (-2 + p + q + wpr - wqr)*(-2 + p + q + wpr + wqr)*
     -     (-2 + p + wpr - wr)*(p + wpr - wr)*(-2 + p + wpr + wr)*
     -     (p + wpr + wr)) + tra(p,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*q*(-2 + p + q - s)*(-2 + p + q + s)*(-1 + p - wp)*
     -     (-1 + p + wp)*(-1 + q - wq)*(-1 + q + wq)*(p - wpr - wr)*
     -     (p + wpr - wr)*wr*(-2 + q - wqr + wr)*(-2 + q + wqr + wr)*
     -     (-2 + 2*wr)) - tra(p,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*q*(-2 + p + q - s)*(-2 + p + q + s)*(-1 + p - wp)*
     -     (-1 + p + wp)*(-1 + q - wq)*(-1 + q + wq)*(-2 + p - wpr - wr)*
     -     (-2 + p + wpr - wr)*wr*(q - wqr + wr)*(q + wqr + wr)*(2 + 2*wr)) - 
     -  tra(p,2 - p - s,-2 + p + s + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*(2 - p - q - s)*(2 - p + q - s)*s*(-1 + p - wp)*(-1 + p + wp)*
     -     (1 - p - s - wq)*(1 - p - s + wq)*(-s - wpr - wqr)*
     -     (-s + wpr - wqr)*wqr*(-2 + p + s + wqr - wr)*(p + s + wqr - wr)*
     -     (-2 + p + s + wqr + wr)*(p + s + wqr + wr)) - 
     -  tra(p,2 - p + s,-2 + p + wpr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*s*(2 - p - q + s)*(2 - p + q + s)*(-1 + p - wp)*(-1 + p + wp)*
     -     wpr*(1 - p + s - wq)*(1 - p + s + wq)*(s + wpr - wqr)*
     -     (s + wpr + wqr)*(-2 + p + wpr - wr)*(p + wpr - wr)*
     -     (-2 + p + wpr + wr)*(p + wpr + wr)) - 
     -  tra(p,1 - wq,-1 + wq + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*(-1 + p - wp)*(-1 + p + wp)*(1 - q - wq)*(1 + q - wq)*
     -     (-1 + p - s - wq)*(-1 + p + s - wq)*wq*(-1 + p - wpr - wq - wqr)*
     -     (-1 + p + wpr - wq - wqr)*wqr*(-1 + wq + wqr - wr)*
     -     (1 + wq + wqr - wr)*(-1 + wq + wqr + wr)*(1 + wq + wqr + wr)) - 
     -  tra(p,1 + wq,-2 + p + wpr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*(-1 + p - wp)*(-1 + p + wp)*wpr*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-1 + p - s + wq)*(-1 + p + s + wq)*(-1 + p + wpr + wq - wqr)*
     -     (-1 + p + wpr + wq + wqr)*(-2 + p + wpr - wr)*(p + wpr - wr)*
     -     (-2 + p + wpr + wr)*(p + wpr + wr)) + 
     -  tra(p,1 + wq,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*(-1 + p - wp)*(-1 + p + wp)*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-1 + p - s + wq)*(-1 + p + s + wq)*(p - wpr - wr)*(p + wpr - wr)*
     -     wr*(-1 + wq - wqr + wr)*(-1 + wq + wqr + wr)*(-2 + 2*wr)) - 
     -  tra(p,1 + wq,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*(-1 + p - wp)*(-1 + p + wp)*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-1 + p - s + wq)*(-1 + p + s + wq)*(-2 + p - wpr - wr)*
     -     (-2 + p + wpr - wr)*wr*(1 + wq - wqr + wr)*(1 + wq + wqr + wr)*
     -     (2 + 2*wr)) - tra(p,2 - p - wpr + wqr,-2 + p + wpr,pp,qq,rr,pq,pr,
     -    qr,m)/
     -   (8.*p*(-1 + p - wp)*(-1 + p + wp)*wpr*wqr*(2 - p - q - wpr + wqr)*
     -     (2 - p + q - wpr + wqr)*(-s - wpr + wqr)*(s - wpr + wqr)*
     -     (1 - p - wpr - wq + wqr)*(1 - p - wpr + wq + wqr)*
     -     (-2 + p + wpr - wr)*(p + wpr - wr)*(-2 + p + wpr + wr)*
     -     (p + wpr + wr)) - tra(p,wqr - wr,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*(-1 + p - wp)*(-1 + p + wp)*wqr*(-2 + p - wpr - wr)*
     -     (-2 + p + wpr - wr)*(-q + wqr - wr)*(q + wqr - wr)*
     -     (-2 + p - s + wqr - wr)*(-2 + p + s + wqr - wr)*
     -     (-1 - wq + wqr - wr)*(-1 + wq + wqr - wr)*wr*(2 + 2*wr)) + 
     -  tra(p,2 + wqr - wr,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*(-1 + p - wp)*(-1 + p + wp)*wqr*(p - wpr - wr)*(p + wpr - wr)*
     -     (2 - q + wqr - wr)*(2 + q + wqr - wr)*(p - s + wqr - wr)*
     -     (p + s + wqr - wr)*(1 - wq + wqr - wr)*(1 + wq + wqr - wr)*wr*
     -     (-2 + 2*wr)) + tra(2 - q + s,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*s*(2 - p - q + s)*(2 + p - q + s)*(1 - q + s - wp)*
     -     (1 - q + s + wp)*(-1 + q - wq)*(-1 + q + wq)*
     -     (2 - q + s - wpr - wr)*(2 - q + s + wpr - wr)*wr*
     -     (-2 + q - wqr + wr)*(-2 + q + wqr + wr)*(-2 + 2*wr)) - 
     -  tra(2 - q + s,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*s*(2 - p - q + s)*(2 + p - q + s)*(1 - q + s - wp)*
     -     (1 - q + s + wp)*(-1 + q - wq)*(-1 + q + wq)*(-q + s - wpr - wr)*
     -     (-q + s + wpr - wr)*wr*(q - wqr + wr)*(q + wqr + wr)*(2 + 2*wr)) - 
     -  tra(2 + q + s,-q,q + s + wpr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*q*s*(2 - p + q + s)*(2 + p + q + s)*(1 + q + s - wp)*
     -     (1 + q + s + wp)*wpr*(-1 - q - wq)*(-1 - q + wq)*(s + wpr - wqr)*
     -     (s + wpr + wqr)*(q + s + wpr - wr)*(2 + q + s + wpr - wr)*
     -     (q + s + wpr + wr)*(2 + q + s + wpr + wr)) - 
     -  tra(2 + q + s,-q,q + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*q*s*(2 - p + q + s)*(2 + p + q + s)*(1 + q + s - wp)*
     -     (1 + q + s + wp)*(-1 - q - wq)*(-1 - q + wq)*(s - wpr - wqr)*
     -     (s + wpr - wqr)*wqr*(q + wqr - wr)*(2 + q + wqr - wr)*
     -     (q + wqr + wr)*(2 + q + wqr + wr)) + 
     -  tra(1 - wp,1 + s + wp,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*(1 - p - wp)*(1 + p - wp)*wp*(1 - q + s + wp)*
     -     (1 + q + s + wp)*(s + wp - wq)*(s + wp + wq)*(1 - wp - wpr - wr)*
     -     (1 - wp + wpr - wr)*wr*(-1 + s + wp - wqr + wr)*
     -     (-1 + s + wp + wqr + wr)*(-2 + 2*wr)) - 
     -  tra(1 - wp,1 + s + wp,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*(1 - p - wp)*(1 + p - wp)*wp*(1 - q + s + wp)*
     -     (1 + q + s + wp)*(s + wp - wq)*(s + wp + wq)*(-1 - wp - wpr - wr)*
     -     (-1 - wp + wpr - wr)*wr*(1 + s + wp - wqr + wr)*
     -     (1 + s + wp + wqr + wr)*(2 + 2*wr)) - 
     -  tra(1 + wp,-q,q + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*q*wp*(1 - p + wp)*(1 + p + wp)*(-1 - q - s + wp)*
     -     (-1 - q + s + wp)*(-1 - q - wq)*(-1 - q + wq)*
     -     (-1 - q + wp - wpr - wqr)*(-1 - q + wp + wpr - wqr)*wqr*
     -     (q + wqr - wr)*(2 + q + wqr - wr)*(q + wqr + wr)*(2 + q + wqr + wr)
     -     ) - tra(1 + wp,q,-1 + wp + wpr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*q*wp*(1 - p + wp)*(1 + p + wp)*(-1 + q - s + wp)*
     -     (-1 + q + s + wp)*wpr*(-1 + q - wq)*(-1 + q + wq)*
     -     (-1 + q + wp + wpr - wqr)*(-1 + q + wp + wpr + wqr)*
     -     (-1 + wp + wpr - wr)*(1 + wp + wpr - wr)*(-1 + wp + wpr + wr)*
     -     (1 + wp + wpr + wr)) + 
     -  tra(1 + wp,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*wp*(1 - p + wp)*(1 + p + wp)*(-1 + q - s + wp)*
     -     (-1 + q + s + wp)*(-1 + q - wq)*(-1 + q + wq)*(1 + wp - wpr - wr)*
     -     (1 + wp + wpr - wr)*wr*(-2 + q - wqr + wr)*(-2 + q + wqr + wr)*
     -     (-2 + 2*wr)) - tra(1 + wp,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*wp*(1 - p + wp)*(1 + p + wp)*(-1 + q - s + wp)*
     -     (-1 + q + s + wp)*(-1 + q - wq)*(-1 + q + wq)*(-1 + wp - wpr - wr)*
     -     (-1 + wp + wpr - wr)*wr*(q - wqr + wr)*(q + wqr + wr)*(2 + 2*wr))\
     -   - tra(1 + wp,1 - s - wp,-1 + s + wp + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*s*(1 - q - s - wp)*(1 + q - s - wp)*wp*(1 - p + wp)*(1 + p + wp)*
     -     (-s - wp - wq)*(-s - wp + wq)*(-s - wpr - wqr)*(-s + wpr - wqr)*
     -     wqr*(-1 + s + wp + wqr - wr)*(1 + s + wp + wqr - wr)*
     -     (-1 + s + wp + wqr + wr)*(1 + s + wp + wqr + wr)) - 
     -  tra(1 + wp,1 + s - wp,-1 + wp + wpr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*s*(1 - q + s - wp)*(1 + q + s - wp)*wp*(1 - p + wp)*(1 + p + wp)*
     -     wpr*(s - wp - wq)*(s - wp + wq)*(s + wpr - wqr)*(s + wpr + wqr)*
     -     (-1 + wp + wpr - wr)*(1 + wp + wpr - wr)*(-1 + wp + wpr + wr)*
     -     (1 + wp + wpr + wr)) - 
     -  tra(1 + wp,1 - wq,-1 + wq + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*wp*(1 - p + wp)*(1 + p + wp)*(1 - q - wq)*(1 + q - wq)*
     -     (-s + wp - wq)*(s + wp - wq)*wq*(wp - wpr - wq - wqr)*
     -     (wp + wpr - wq - wqr)*wqr*(-1 + wq + wqr - wr)*(1 + wq + wqr - wr)*
     -     (-1 + wq + wqr + wr)*(1 + wq + wqr + wr)) - 
     -  tra(1 + wp,1 + wq,-1 + wp + wpr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*wp*(1 - p + wp)*(1 + p + wp)*wpr*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-s + wp + wq)*(s + wp + wq)*(wp + wpr + wq - wqr)*
     -     (wp + wpr + wq + wqr)*(-1 + wp + wpr - wr)*(1 + wp + wpr - wr)*
     -     (-1 + wp + wpr + wr)*(1 + wp + wpr + wr)) + 
     -  tra(1 + wp,1 + wq,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wp*(1 - p + wp)*(1 + p + wp)*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-s + wp + wq)*(s + wp + wq)*(1 + wp - wpr - wr)*
     -     (1 + wp + wpr - wr)*wr*(-1 + wq - wqr + wr)*(-1 + wq + wqr + wr)*
     -     (-2 + 2*wr)) - tra(1 + wp,1 + wq,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wp*(1 - p + wp)*(1 + p + wp)*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-s + wp + wq)*(s + wp + wq)*(-1 + wp - wpr - wr)*
     -     (-1 + wp + wpr - wr)*wr*(1 + wq - wqr + wr)*(1 + wq + wqr + wr)*
     -     (2 + 2*wr)) - tra(1 + wp,1 - wp - wpr + wqr,-1 + wp + wpr,pp,qq,rr,
     -    pq,pr,qr,m)/
     -   (8.*wp*(1 - p + wp)*(1 + p + wp)*wpr*wqr*(-s - wpr + wqr)*
     -     (s - wpr + wqr)*(1 - q - wp - wpr + wqr)*(1 + q - wp - wpr + wqr)*
     -     (-wp - wpr - wq + wqr)*(-wp - wpr + wq + wqr)*(-1 + wp + wpr - wr)*
     -     (1 + wp + wpr - wr)*(-1 + wp + wpr + wr)*(1 + wp + wpr + wr)) - 
     -  tra(1 + wp,wqr - wr,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wp*(1 - p + wp)*(1 + p + wp)*wqr*(-1 + wp - wpr - wr)*
     -     (-1 + wp + wpr - wr)*(-q + wqr - wr)*(q + wqr - wr)*
     -     (-1 - s + wp + wqr - wr)*(-1 + s + wp + wqr - wr)*
     -     (-1 - wq + wqr - wr)*(-1 + wq + wqr - wr)*wr*(2 + 2*wr)) + 
     -  tra(1 + wp,2 + wqr - wr,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wp*(1 - p + wp)*(1 + p + wp)*wqr*(1 + wp - wpr - wr)*
     -     (1 + wp + wpr - wr)*(2 - q + wqr - wr)*(2 + q + wqr - wr)*
     -     (1 - s + wp + wqr - wr)*(1 + s + wp + wqr - wr)*
     -     (1 - wq + wqr - wr)*(1 + wq + wqr - wr)*wr*(-2 + 2*wr)) + 
     -  tra(1 + s - wq,1 + wq,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*(1 - p + s - wq)*(1 + p + s - wq)*(s - wp - wq)*(s + wp - wq)*
     -     wq*(1 - q + wq)*(1 + q + wq)*(1 + s - wpr - wq - wr)*
     -     (1 + s + wpr - wq - wr)*wr*(-1 + wq - wqr + wr)*
     -     (-1 + wq + wqr + wr)*(-2 + 2*wr)) - 
     -  tra(1 + s - wq,1 + wq,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*(1 - p + s - wq)*(1 + p + s - wq)*(s - wp - wq)*(s + wp - wq)*
     -     wq*(1 - q + wq)*(1 + q + wq)*(-1 + s - wpr - wq - wr)*
     -     (-1 + s + wpr - wq - wr)*wr*(1 + wq - wqr + wr)*
     -     (1 + wq + wqr + wr)*(2 + 2*wr)) - 
     -  tra(1 + s + wq,1 - wq,-1 + s + wpr + wq,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*s*wpr*(1 - q - wq)*(1 + q - wq)*wq*(1 - p + s + wq)*
     -     (1 + p + s + wq)*(s - wp + wq)*(s + wp + wq)*(s + wpr - wqr)*
     -     (s + wpr + wqr)*(-1 + s + wpr + wq - wr)*(1 + s + wpr + wq - wr)*
     -     (-1 + s + wpr + wq + wr)*(1 + s + wpr + wq + wr)) - 
     -  tra(1 + s + wq,1 - wq,-1 + wq + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*s*(1 - q - wq)*(1 + q - wq)*wq*(1 - p + s + wq)*(1 + p + s + wq)*
     -     (s - wp + wq)*(s + wp + wq)*(s - wpr - wqr)*(s + wpr - wqr)*wqr*
     -     (-1 + wq + wqr - wr)*(1 + wq + wqr - wr)*(-1 + wq + wqr + wr)*
     -     (1 + wq + wqr + wr)) - 
     -  tra(2 + q + wpr + wqr,-q,q + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*q*wpr*(-1 - q - wq)*(-1 - q + wq)*wqr*(2 - p + q + wpr + wqr)*
     -     (2 + p + q + wpr + wqr)*(-s + wpr + wqr)*(s + wpr + wqr)*
     -     (1 + q - wp + wpr + wqr)*(1 + q + wp + wpr + wqr)*(q + wqr - wr)*
     -     (2 + q + wqr - wr)*(q + wqr + wr)*(2 + q + wqr + wr)) - 
     -  tra(1 + wpr + wq + wqr,1 - wq,-1 + wq + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*wpr*(1 - q - wq)*(1 + q - wq)*wq*wqr*(-s + wpr + wqr)*
     -     (s + wpr + wqr)*(1 - p + wpr + wq + wqr)*(1 + p + wpr + wq + wqr)*
     -     (-wp + wpr + wq + wqr)*(wp + wpr + wq + wqr)*(-1 + wq + wqr - wr)*
     -     (1 + wq + wqr - wr)*(-1 + wq + wqr + wr)*(1 + wq + wqr + wr)) + 
     -  tra(-wpr + wr,2 + s + wpr - wr,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*wpr*(s + wpr - wqr)*(s + wpr + wqr)*(2 - q + s + wpr - wr)*
     -     (2 + q + s + wpr - wr)*(1 + s + wpr - wq - wr)*
     -     (1 + s + wpr + wq - wr)*wr*(-p - wpr + wr)*(p - wpr + wr)*
     -     (-1 - wp - wpr + wr)*(-1 + wp - wpr + wr)*(-2 + 2*wr)) - 
     -  tra(2 - wpr + wr,s + wpr - wr,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*wpr*(s + wpr - wqr)*(s + wpr + wqr)*(-q + s + wpr - wr)*
     -     (q + s + wpr - wr)*(-1 + s + wpr - wq - wr)*
     -     (-1 + s + wpr + wq - wr)*wr*(2 - p - wpr + wr)*(2 + p - wpr + wr)*
     -     (1 - wp - wpr + wr)*(1 + wp - wpr + wr)*(2 + 2*wr)) + 
     -  tra(wpr + wr,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*wpr*(-1 + q - wq)*(-1 + q + wq)*wr*(-p + wpr + wr)*
     -     (p + wpr + wr)*(-2 + q - s + wpr + wr)*(-2 + q + s + wpr + wr)*
     -     (-1 - wp + wpr + wr)*(-1 + wp + wpr + wr)*(-2 + q - wqr + wr)*
     -     (-2 + q + wqr + wr)*(-2 + 2*wr)) + 
     -  tra(wpr + wr,1 + wq,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wpr*wq*(1 - q + wq)*(1 + q + wq)*wr*(-p + wpr + wr)*
     -     (p + wpr + wr)*(-1 - wp + wpr + wr)*(-1 + wp + wpr + wr)*
     -     (-1 - s + wpr + wq + wr)*(-1 + s + wpr + wq + wr)*
     -     (-1 + wq - wqr + wr)*(-1 + wq + wqr + wr)*(-2 + 2*wr)) + 
     -  tra(wpr + wr,2 + wqr - wr,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wpr*wqr*(-s + wpr + wqr)*(s + wpr + wqr)*(2 - q + wqr - wr)*
     -     (2 + q + wqr - wr)*(1 - wq + wqr - wr)*(1 + wq + wqr - wr)*wr*
     -     (-p + wpr + wr)*(p + wpr + wr)*(-1 - wp + wpr + wr)*
     -     (-1 + wp + wpr + wr)*(-2 + 2*wr)) - 
     -  tra(2 + wpr + wr,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*wpr*(-1 + q - wq)*(-1 + q + wq)*wr*(2 - p + wpr + wr)*
     -     (2 + p + wpr + wr)*(q - s + wpr + wr)*(q + s + wpr + wr)*
     -     (1 - wp + wpr + wr)*(1 + wp + wpr + wr)*(q - wqr + wr)*
     -     (q + wqr + wr)*(2 + 2*wr)) - 
     -  tra(2 + wpr + wr,1 + wq,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wpr*wq*(1 - q + wq)*(1 + q + wq)*wr*(2 - p + wpr + wr)*
     -     (2 + p + wpr + wr)*(1 - wp + wpr + wr)*(1 + wp + wpr + wr)*
     -     (1 - s + wpr + wq + wr)*(1 + s + wpr + wq + wr)*
     -     (1 + wq - wqr + wr)*(1 + wq + wqr + wr)*(2 + 2*wr)) - 
     -  tra(2 + wpr + wr,wqr - wr,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wpr*wqr*(-s + wpr + wqr)*(s + wpr + wqr)*(-q + wqr - wr)*
     -     (q + wqr - wr)*(-1 - wq + wqr - wr)*(-1 + wq + wqr - wr)*wr*
     -     (2 - p + wpr + wr)*(2 + p + wpr + wr)*(1 - wp + wpr + wr)*
     -     (1 + wp + wpr + wr)*(2 + 2*wr)) + 
     -  tra(s - wqr + wr,2 + wqr - wr,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*(s - wpr - wqr)*(s + wpr - wqr)*wqr*(2 - q + wqr - wr)*
     -     (2 + q + wqr - wr)*(1 - wq + wqr - wr)*(1 + wq + wqr - wr)*wr*
     -     (-p + s - wqr + wr)*(p + s - wqr + wr)*(-1 + s - wp - wqr + wr)*
     -     (-1 + s + wp - wqr + wr)*(-2 + 2*wr)) - 
     -  tra(2 + s - wqr + wr,wqr - wr,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*(s - wpr - wqr)*(s + wpr - wqr)*wqr*(-q + wqr - wr)*
     -     (q + wqr - wr)*(-1 - wq + wqr - wr)*(-1 + wq + wqr - wr)*wr*
     -     (2 - p + s - wqr + wr)*(2 + p + s - wqr + wr)*
     -     (1 + s - wp - wqr + wr)*(1 + s + wp - wqr + wr)*(2 + 2*wr))
      fnub =        -trb(-p,-q,p + q + wpqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*q*(-2 - p - q - s)*(-2 - p - q + s)*(-1 - p - wp)*
     -     (-1 - p + wp)*wpqr*(-1 - q - wq)*(-1 - q + wq)*(p + wpqr - wqr)*
     -     (p + wpqr + wqr)*(p + q + wpqr - wr)*(2 + p + q + wpqr - wr)*
     -     (p + q + wpqr + wr)*(2 + p + q + wpqr + wr)) - 
     -  trb(-p,2 + p - s,-2 + s + wpqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*(2 + p - q - s)*(2 + p + q - s)*s*(-1 - p - wp)*(-1 - p + wp)*
     -     wpqr*(1 + p - s - wq)*(1 + p - s + wq)*(p + wpqr - wqr)*
     -     (p + wpqr + wqr)*(-2 + s + wpqr - wr)*(s + wpqr - wr)*
     -     (-2 + s + wpqr + wr)*(s + wpqr + wr)) + 
     -  trb(-p,2 + p + s,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*s*(2 + p - q + s)*(2 + p + q + s)*(-1 - p - wp)*(-1 - p + wp)*
     -     (1 + p + s - wq)*(1 + p + s + wq)*wr*(s - wpqr + wr)*
     -     (s + wpqr + wr)*(p + s - wqr + wr)*(p + s + wqr + wr)*(-2 + 2*wr))\
     -   - trb(-p,2 + p + s,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*s*(2 + p - q + s)*(2 + p + q + s)*(-1 - p - wp)*(-1 - p + wp)*
     -     (1 + p + s - wq)*(1 + p + s + wq)*wr*(2 + s - wpqr + wr)*
     -     (2 + s + wpqr + wr)*(2 + p + s - wqr + wr)*(2 + p + s + wqr + wr)*
     -     (2 + 2*wr)) - trb(-p,1 - wq,-1 + p + wpqr + wq,pp,qq,rr,pq,pr,qr,
     -    m)/(8.*p*(-1 - p - wp)*(-1 - p + wp)*wpqr*(1 - q - wq)*(1 + q - wq)*
     -     (-1 - p - s - wq)*(-1 - p + s - wq)*wq*(p + wpqr - wqr)*
     -     (p + wpqr + wqr)*(-1 + p + wpqr + wq - wr)*
     -     (1 + p + wpqr + wq - wr)*(-1 + p + wpqr + wq + wr)*
     -     (1 + p + wpqr + wq + wr)) - 
     -  trb(-p,p + wpqr - wr,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*(-1 - p - wp)*(-1 - p + wp)*wpqr*(p + wpqr - wqr)*
     -     (p + wpqr + wqr)*(p - q + wpqr - wr)*(p + q + wpqr - wr)*
     -     (-2 - s + wpqr - wr)*(-2 + s + wpqr - wr)*
     -     (-1 + p + wpqr - wq - wr)*(-1 + p + wpqr + wq - wr)*wr*(2 + 2*wr))\
     -   + trb(-p,2 + p + wpqr - wr,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*(-1 - p - wp)*(-1 - p + wp)*wpqr*(p + wpqr - wqr)*
     -     (p + wpqr + wqr)*(2 + p - q + wpqr - wr)*(2 + p + q + wpqr - wr)*
     -     (-s + wpqr - wr)*(s + wpqr - wr)*(1 + p + wpqr - wq - wr)*
     -     (1 + p + wpqr + wq - wr)*wr*(-2 + 2*wr)) - 
     -  trb(p,-q,q + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*q*(-2 + p - q - s)*(-2 + p - q + s)*(-1 + p - wp)*
     -     (-1 + p + wp)*(-1 - q - wq)*(-1 - q + wq)*wqr*(p - wpqr + wqr)*
     -     (p + wpqr + wqr)*(q + wqr - wr)*(2 + q + wqr - wr)*(q + wqr + wr)*
     -     (2 + q + wqr + wr)) + 
     -  trb(p,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*q*(-2 + p + q - s)*(-2 + p + q + s)*(-1 + p - wp)*
     -     (-1 + p + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*
     -     (-2 + p + q - wpqr + wr)*(-2 + p + q + wpqr + wr)*
     -     (-2 + q - wqr + wr)*(-2 + q + wqr + wr)*(-2 + 2*wr)) - 
     -  trb(p,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*q*(-2 + p + q - s)*(-2 + p + q + s)*(-1 + p - wp)*
     -     (-1 + p + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*(p + q - wpqr + wr)*
     -     (p + q + wpqr + wr)*(q - wqr + wr)*(q + wqr + wr)*(2 + 2*wr)) - 
     -  trb(p,2 - p - s,-2 + p + s + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*(2 - p - q - s)*(2 - p + q - s)*s*(-1 + p - wp)*(-1 + p + wp)*
     -     (1 - p - s - wq)*(1 - p - s + wq)*wqr*(p - wpqr + wqr)*
     -     (p + wpqr + wqr)*(-2 + p + s + wqr - wr)*(p + s + wqr - wr)*
     -     (-2 + p + s + wqr + wr)*(p + s + wqr + wr)) - 
     -  trb(p,1 - wq,-1 + wq + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*(-1 + p - wp)*(-1 + p + wp)*(1 - q - wq)*(1 + q - wq)*
     -     (-1 + p - s - wq)*(-1 + p + s - wq)*wq*wqr*(p - wpqr + wqr)*
     -     (p + wpqr + wqr)*(-1 + wq + wqr - wr)*(1 + wq + wqr - wr)*
     -     (-1 + wq + wqr + wr)*(1 + wq + wqr + wr)) + 
     -  trb(p,1 + wq,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*(-1 + p - wp)*(-1 + p + wp)*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-1 + p - s + wq)*(-1 + p + s + wq)*wr*(-1 + p - wpqr + wq + wr)*
     -     (-1 + p + wpqr + wq + wr)*(-1 + wq - wqr + wr)*
     -     (-1 + wq + wqr + wr)*(-2 + 2*wr)) - 
     -  trb(p,1 + wq,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*(-1 + p - wp)*(-1 + p + wp)*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-1 + p - s + wq)*(-1 + p + s + wq)*wr*(1 + p - wpqr + wq + wr)*
     -     (1 + p + wpqr + wq + wr)*(1 + wq - wqr + wr)*(1 + wq + wqr + wr)*
     -     (2 + 2*wr)) - trb(p,wqr - wr,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*(-1 + p - wp)*(-1 + p + wp)*wqr*(p - wpqr + wqr)*
     -     (p + wpqr + wqr)*(-q + wqr - wr)*(q + wqr - wr)*
     -     (-2 + p - s + wqr - wr)*(-2 + p + s + wqr - wr)*
     -     (-1 - wq + wqr - wr)*(-1 + wq + wqr - wr)*wr*(2 + 2*wr)) + 
     -  trb(p,2 + wqr - wr,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*(-1 + p - wp)*(-1 + p + wp)*wqr*(p - wpqr + wqr)*
     -     (p + wpqr + wqr)*(2 - q + wqr - wr)*(2 + q + wqr - wr)*
     -     (p - s + wqr - wr)*(p + s + wqr - wr)*(1 - wq + wqr - wr)*
     -     (1 + wq + wqr - wr)*wr*(-2 + 2*wr)) - 
     -  trb(2 - q - s,q,-2 + s + wpqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*q*(2 - p - q - s)*(2 + p - q - s)*s*(1 - q - s - wp)*
     -     (1 - q - s + wp)*wpqr*(-1 + q - wq)*(-1 + q + wq)*
     -     (-2 + q + s + wpqr - wqr)*(-2 + q + s + wpqr + wqr)*
     -     (-2 + s + wpqr - wr)*(s + wpqr - wr)*(-2 + s + wpqr + wr)*
     -     (s + wpqr + wr)) + trb(2 - q + s,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*s*(2 - p - q + s)*(2 + p - q + s)*(1 - q + s - wp)*
     -     (1 - q + s + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*(s - wpqr + wr)*
     -     (s + wpqr + wr)*(-2 + q - wqr + wr)*(-2 + q + wqr + wr)*(-2 + 2*wr)
     -     ) - trb(2 - q + s,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*s*(2 - p - q + s)*(2 + p - q + s)*(1 - q + s - wp)*
     -     (1 - q + s + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*
     -     (2 + s - wpqr + wr)*(2 + s + wpqr + wr)*(q - wqr + wr)*
     -     (q + wqr + wr)*(2 + 2*wr)) - 
     -  trb(2 + q + s,-q,q + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*q*s*(2 - p + q + s)*(2 + p + q + s)*(1 + q + s - wp)*
     -     (1 + q + s + wp)*(-1 - q - wq)*(-1 - q + wq)*wqr*
     -     (2 + q + s - wpqr + wqr)*(2 + q + s + wpqr + wqr)*(q + wqr - wr)*
     -     (2 + q + wqr - wr)*(q + wqr + wr)*(2 + q + wqr + wr)) - 
     -  trb(1 - wp,-q,-1 + q + wp + wpqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*q*(1 - p - wp)*(1 + p - wp)*(-1 - q - s - wp)*(-1 - q + s - wp)*
     -     wp*wpqr*(-1 - q - wq)*(-1 - q + wq)*(-1 + wp + wpqr - wqr)*
     -     (-1 + wp + wpqr + wqr)*(-1 + q + wp + wpqr - wr)*
     -     (1 + q + wp + wpqr - wr)*(-1 + q + wp + wpqr + wr)*
     -     (1 + q + wp + wpqr + wr)) - 
     -  trb(1 - wp,1 - s + wp,-2 + s + wpqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*s*(1 - p - wp)*(1 + p - wp)*wp*(1 - q - s + wp)*(1 + q - s + wp)*
     -     wpqr*(-s + wp - wq)*(-s + wp + wq)*(-1 + wp + wpqr - wqr)*
     -     (-1 + wp + wpqr + wqr)*(-2 + s + wpqr - wr)*(s + wpqr - wr)*
     -     (-2 + s + wpqr + wr)*(s + wpqr + wr)) + 
     -  trb(1 - wp,1 + s + wp,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*(1 - p - wp)*(1 + p - wp)*wp*(1 - q + s + wp)*
     -     (1 + q + s + wp)*(s + wp - wq)*(s + wp + wq)*wr*(s - wpqr + wr)*
     -     (s + wpqr + wr)*(-1 + s + wp - wqr + wr)*(-1 + s + wp + wqr + wr)*
     -     (-2 + 2*wr)) - trb(1 - wp,1 + s + wp,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*(1 - p - wp)*(1 + p - wp)*wp*(1 - q + s + wp)*
     -     (1 + q + s + wp)*(s + wp - wq)*(s + wp + wq)*wr*
     -     (2 + s - wpqr + wr)*(2 + s + wpqr + wr)*(1 + s + wp - wqr + wr)*
     -     (1 + s + wp + wqr + wr)*(2 + 2*wr)) - 
     -  trb(1 - wp,1 - wq,-2 + wp + wpqr + wq,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*(1 - p - wp)*(1 + p - wp)*wp*wpqr*(1 - q - wq)*(1 + q - wq)*
     -     (-s - wp - wq)*(s - wp - wq)*wq*(-1 + wp + wpqr - wqr)*
     -     (-1 + wp + wpqr + wqr)*(-2 + wp + wpqr + wq - wr)*
     -     (wp + wpqr + wq - wr)*(-2 + wp + wpqr + wq + wr)*
     -     (wp + wpqr + wq + wr)) - 
     -  trb(1 - wp,-1 + wp + wpqr - wr,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*(1 - p - wp)*(1 + p - wp)*wp*wpqr*(-1 + wp + wpqr - wqr)*
     -     (-1 + wp + wpqr + wqr)*(-2 - s + wpqr - wr)*(-2 + s + wpqr - wr)*
     -     (-1 - q + wp + wpqr - wr)*(-1 + q + wp + wpqr - wr)*
     -     (-2 + wp + wpqr - wq - wr)*(-2 + wp + wpqr + wq - wr)*wr*(2 + 2*wr)
     -     ) + trb(1 - wp,1 + wp + wpqr - wr,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*(1 - p - wp)*(1 + p - wp)*wp*wpqr*(-1 + wp + wpqr - wqr)*
     -     (-1 + wp + wpqr + wqr)*(-s + wpqr - wr)*(s + wpqr - wr)*
     -     (1 - q + wp + wpqr - wr)*(1 + q + wp + wpqr - wr)*
     -     (wp + wpqr - wq - wr)*(wp + wpqr + wq - wr)*wr*(-2 + 2*wr)) - 
     -  trb(1 + wp,-q,q + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*q*wp*(1 - p + wp)*(1 + p + wp)*(-1 - q - s + wp)*
     -     (-1 - q + s + wp)*(-1 - q - wq)*(-1 - q + wq)*wqr*
     -     (1 + wp - wpqr + wqr)*(1 + wp + wpqr + wqr)*(q + wqr - wr)*
     -     (2 + q + wqr - wr)*(q + wqr + wr)*(2 + q + wqr + wr)) + 
     -  trb(1 + wp,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*wp*(1 - p + wp)*(1 + p + wp)*(-1 + q - s + wp)*
     -     (-1 + q + s + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*
     -     (-1 + q + wp - wpqr + wr)*(-1 + q + wp + wpqr + wr)*
     -     (-2 + q - wqr + wr)*(-2 + q + wqr + wr)*(-2 + 2*wr)) - 
     -  trb(1 + wp,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*wp*(1 - p + wp)*(1 + p + wp)*(-1 + q - s + wp)*
     -     (-1 + q + s + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*
     -     (1 + q + wp - wpqr + wr)*(1 + q + wp + wpqr + wr)*(q - wqr + wr)*
     -     (q + wqr + wr)*(2 + 2*wr)) - 
     -  trb(1 + wp,1 - s - wp,-1 + s + wp + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*s*(1 - q - s - wp)*(1 + q - s - wp)*wp*(1 - p + wp)*(1 + p + wp)*
     -     (-s - wp - wq)*(-s - wp + wq)*wqr*(1 + wp - wpqr + wqr)*
     -     (1 + wp + wpqr + wqr)*(-1 + s + wp + wqr - wr)*
     -     (1 + s + wp + wqr - wr)*(-1 + s + wp + wqr + wr)*
     -     (1 + s + wp + wqr + wr)) - 
     -  trb(1 + wp,1 - wq,-1 + wq + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*wp*(1 - p + wp)*(1 + p + wp)*(1 - q - wq)*(1 + q - wq)*
     -     (-s + wp - wq)*(s + wp - wq)*wq*wqr*(1 + wp - wpqr + wqr)*
     -     (1 + wp + wpqr + wqr)*(-1 + wq + wqr - wr)*(1 + wq + wqr - wr)*
     -     (-1 + wq + wqr + wr)*(1 + wq + wqr + wr)) + 
     -  trb(1 + wp,1 + wq,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wp*(1 - p + wp)*(1 + p + wp)*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-s + wp + wq)*(s + wp + wq)*wr*(wp - wpqr + wq + wr)*
     -     (wp + wpqr + wq + wr)*(-1 + wq - wqr + wr)*(-1 + wq + wqr + wr)*
     -     (-2 + 2*wr)) - trb(1 + wp,1 + wq,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wp*(1 - p + wp)*(1 + p + wp)*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-s + wp + wq)*(s + wp + wq)*wr*(2 + wp - wpqr + wq + wr)*
     -     (2 + wp + wpqr + wq + wr)*(1 + wq - wqr + wr)*(1 + wq + wqr + wr)*
     -     (2 + 2*wr)) - trb(1 + wp,wqr - wr,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wp*(1 - p + wp)*(1 + p + wp)*wqr*(1 + wp - wpqr + wqr)*
     -     (1 + wp + wpqr + wqr)*(-q + wqr - wr)*(q + wqr - wr)*
     -     (-1 - s + wp + wqr - wr)*(-1 + s + wp + wqr - wr)*
     -     (-1 - wq + wqr - wr)*(-1 + wq + wqr - wr)*wr*(2 + 2*wr)) + 
     -  trb(1 + wp,2 + wqr - wr,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wp*(1 - p + wp)*(1 + p + wp)*wqr*(1 + wp - wpqr + wqr)*
     -     (1 + wp + wpqr + wqr)*(2 - q + wqr - wr)*(2 + q + wqr - wr)*
     -     (1 - s + wp + wqr - wr)*(1 + s + wp + wqr - wr)*
     -     (1 - wq + wqr - wr)*(1 + wq + wqr - wr)*wr*(-2 + 2*wr)) - 
     -  trb(1 - s - wq,1 + wq,-2 + s + wpqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*s*wpqr*(1 - p - s - wq)*(1 + p - s - wq)*(-s - wp - wq)*
     -     (-s + wp - wq)*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-1 + s + wpqr + wq - wqr)*(-1 + s + wpqr + wq + wqr)*
     -     (-2 + s + wpqr - wr)*(s + wpqr - wr)*(-2 + s + wpqr + wr)*
     -     (s + wpqr + wr)) + trb(1 + s - wq,1 + wq,-2 + wr,pp,qq,rr,pq,pr,qr,
     -    m)/(16.*s*(1 - p + s - wq)*(1 + p + s - wq)*(s - wp - wq)*
     -     (s + wp - wq)*wq*(1 - q + wq)*(1 + q + wq)*wr*(s - wpqr + wr)*
     -     (s + wpqr + wr)*(-1 + wq - wqr + wr)*(-1 + wq + wqr + wr)*
     -     (-2 + 2*wr)) - trb(1 + s - wq,1 + wq,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*(1 - p + s - wq)*(1 + p + s - wq)*(s - wp - wq)*(s + wp - wq)*
     -     wq*(1 - q + wq)*(1 + q + wq)*wr*(2 + s - wpqr + wr)*
     -     (2 + s + wpqr + wr)*(1 + wq - wqr + wr)*(1 + wq + wqr + wr)*
     -     (2 + 2*wr)) - trb(1 + s + wq,1 - wq,-1 + wq + wqr,pp,qq,rr,pq,pr,
     -    qr,m)/
     -   (8.*s*(1 - q - wq)*(1 + q - wq)*wq*(1 - p + s + wq)*(1 + p + s + wq)*
     -     (s - wp + wq)*(s + wp + wq)*wqr*(1 + s - wpqr + wq + wqr)*
     -     (1 + s + wpqr + wq + wqr)*(-1 + wq + wqr - wr)*(1 + wq + wqr - wr)*
     -     (-1 + wq + wqr + wr)*(1 + wq + wqr + wr)) - 
     -  trb(wpqr - wqr,-q,q + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*q*wpqr*(-1 - q - wq)*(-1 - q + wq)*(-p + wpqr - wqr)*
     -     (p + wpqr - wqr)*(-2 - q - s + wpqr - wqr)*
     -     (-2 - q + s + wpqr - wqr)*(-1 - wp + wpqr - wqr)*
     -     (-1 + wp + wpqr - wqr)*wqr*(q + wqr - wr)*(2 + q + wqr - wr)*
     -     (q + wqr + wr)*(2 + q + wqr + wr)) - 
     -  trb(wpqr - wqr,1 - wq,-1 + wq + wqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*wpqr*(1 - q - wq)*(1 + q - wq)*wq*(-p + wpqr - wqr)*
     -     (p + wpqr - wqr)*(-1 - wp + wpqr - wqr)*(-1 + wp + wpqr - wqr)*
     -     (-1 - s + wpqr - wq - wqr)*(-1 + s + wpqr - wq - wqr)*wqr*
     -     (-1 + wq + wqr - wr)*(1 + wq + wqr - wr)*(-1 + wq + wqr + wr)*
     -     (1 + wq + wqr + wr)) - 
     -  trb(wpqr - wqr,2 - s - wpqr + wqr,-2 + s + wpqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*s*wpqr*(-p + wpqr - wqr)*(p + wpqr - wqr)*(-1 - wp + wpqr - wqr)*
     -     (-1 + wp + wpqr - wqr)*wqr*(2 - q - s - wpqr + wqr)*
     -     (2 + q - s - wpqr + wqr)*(1 - s - wpqr - wq + wqr)*
     -     (1 - s - wpqr + wq + wqr)*(-2 + s + wpqr - wr)*(s + wpqr - wr)*
     -     (-2 + s + wpqr + wr)*(s + wpqr + wr)) - 
     -  trb(wpqr - wqr,wqr - wr,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wpqr*(-p + wpqr - wqr)*(p + wpqr - wqr)*(-1 - wp + wpqr - wqr)*
     -     (-1 + wp + wpqr - wqr)*wqr*(-2 - s + wpqr - wr)*
     -     (-2 + s + wpqr - wr)*(-q + wqr - wr)*(q + wqr - wr)*
     -     (-1 - wq + wqr - wr)*(-1 + wq + wqr - wr)*wr*(2 + 2*wr)) + 
     -  trb(wpqr - wqr,2 + wqr - wr,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wpqr*(-p + wpqr - wqr)*(p + wpqr - wqr)*(-1 - wp + wpqr - wqr)*
     -     (-1 + wp + wpqr - wqr)*wqr*(-s + wpqr - wr)*(s + wpqr - wr)*
     -     (2 - q + wqr - wr)*(2 + q + wqr - wr)*(1 - wq + wqr - wr)*
     -     (1 + wq + wqr - wr)*wr*(-2 + 2*wr)) - 
     -  trb(-q + wpqr - wr,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*wpqr*(-1 + q - wq)*(-1 + q + wq)*(-p - q + wpqr - wr)*
     -     (p - q + wpqr - wr)*(-2 - s + wpqr - wr)*(-2 + s + wpqr - wr)*
     -     (-1 - q - wp + wpqr - wr)*(-1 - q + wp + wpqr - wr)*wr*
     -     (q - wqr + wr)*(q + wqr + wr)*(2 + 2*wr)) + 
     -  trb(2 - q + wpqr - wr,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*wpqr*(-1 + q - wq)*(-1 + q + wq)*(2 - p - q + wpqr - wr)*
     -     (2 + p - q + wpqr - wr)*(-s + wpqr - wr)*(s + wpqr - wr)*
     -     (1 - q - wp + wpqr - wr)*(1 - q + wp + wpqr - wr)*wr*
     -     (-2 + q - wqr + wr)*(-2 + q + wqr + wr)*(-2 + 2*wr)) - 
     -  trb(-1 + wpqr - wq - wr,1 + wq,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wpqr*wq*(1 - q + wq)*(1 + q + wq)*(-2 - s + wpqr - wr)*
     -     (-2 + s + wpqr - wr)*(-1 - p + wpqr - wq - wr)*
     -     (-1 + p + wpqr - wq - wr)*(-2 - wp + wpqr - wq - wr)*
     -     (-2 + wp + wpqr - wq - wr)*wr*(1 + wq - wqr + wr)*
     -     (1 + wq + wqr + wr)*(2 + 2*wr)) + 
     -  trb(1 + wpqr - wq - wr,1 + wq,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wpqr*wq*(1 - q + wq)*(1 + q + wq)*(-s + wpqr - wr)*
     -     (s + wpqr - wr)*(1 - p + wpqr - wq - wr)*(1 + p + wpqr - wq - wr)*
     -     (-wp + wpqr - wq - wr)*(wp + wpqr - wq - wr)*wr*
     -     (-1 + wq - wqr + wr)*(-1 + wq + wqr + wr)*(-2 + 2*wr)) + 
     -  trb(s - wqr + wr,2 + wqr - wr,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*wqr*(2 - q + wqr - wr)*(2 + q + wqr - wr)*(1 - wq + wqr - wr)*
     -     (1 + wq + wqr - wr)*wr*(s - wpqr + wr)*(s + wpqr + wr)*
     -     (-p + s - wqr + wr)*(p + s - wqr + wr)*(-1 + s - wp - wqr + wr)*
     -     (-1 + s + wp - wqr + wr)*(-2 + 2*wr)) - 
     -  trb(2 + s - wqr + wr,wqr - wr,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*wqr*(-q + wqr - wr)*(q + wqr - wr)*(-1 - wq + wqr - wr)*
     -     (-1 + wq + wqr - wr)*wr*(2 + s - wpqr + wr)*(2 + s + wpqr + wr)*
     -     (2 - p + s - wqr + wr)*(2 + p + s - wqr + wr)*
     -     (1 + s - wp - wqr + wr)*(1 + s + wp - wqr + wr)*(2 + 2*wr))
      fnuc =        -trc(-p,-q,p + q + wpqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*q*(-2 - p - q - s)*(-2 - p - q + s)*(-1 - p - wp)*
     -     (-1 - p + wp)*wpqr*(q + wpqr - wprp)*(q + wpqr + wprp)*
     -     (-1 - q - wq)*(-1 - q + wq)*(p + q + wpqr - wr)*
     -     (2 + p + q + wpqr - wr)*(p + q + wpqr + wr)*(2 + p + q + wpqr + wr)
     -     ) - trc(-p,q,p + wprp,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*q*(-2 - p + q - s)*(-2 - p + q + s)*(-1 - p - wp)*
     -     (-1 - p + wp)*wprp*(q - wpqr + wprp)*(q + wpqr + wprp)*
     -     (-1 + q - wq)*(-1 + q + wq)*(p + wprp - wr)*(2 + p + wprp - wr)*
     -     (p + wprp + wr)*(2 + p + wprp + wr)) - 
     -  trc(-p,2 + p - s,-2 + s + wpqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*(2 + p - q - s)*(2 + p + q - s)*s*(-1 - p - wp)*(-1 - p + wp)*
     -     wpqr*(-2 - p + s + wpqr - wprp)*(-2 - p + s + wpqr + wprp)*
     -     (1 + p - s - wq)*(1 + p - s + wq)*(-2 + s + wpqr - wr)*
     -     (s + wpqr - wr)*(-2 + s + wpqr + wr)*(s + wpqr + wr)) - 
     -  trc(-p,2 + p + s,p + wprp,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*s*(2 + p - q + s)*(2 + p + q + s)*(-1 - p - wp)*(-1 - p + wp)*
     -     wprp*(2 + p + s - wpqr + wprp)*(2 + p + s + wpqr + wprp)*
     -     (1 + p + s - wq)*(1 + p + s + wq)*(p + wprp - wr)*
     -     (2 + p + wprp - wr)*(p + wprp + wr)*(2 + p + wprp + wr)) + 
     -  trc(-p,2 + p + s,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*s*(2 + p - q + s)*(2 + p + q + s)*(-1 - p - wp)*(-1 - p + wp)*
     -     (1 + p + s - wq)*(1 + p + s + wq)*wr*(s - wpqr + wr)*
     -     (s + wpqr + wr)*(-2 - p - wprp + wr)*(-2 - p + wprp + wr)*
     -     (-2 + 2*wr)) - trc(-p,2 + p + s,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*s*(2 + p - q + s)*(2 + p + q + s)*(-1 - p - wp)*(-1 - p + wp)*
     -     (1 + p + s - wq)*(1 + p + s + wq)*wr*(2 + s - wpqr + wr)*
     -     (2 + s + wpqr + wr)*(-p - wprp + wr)*(-p + wprp + wr)*(2 + 2*wr))\
     -   - trc(-p,wpqr - wprp,p + wprp,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*(-1 - p - wp)*(-1 - p + wp)*wpqr*(-q + wpqr - wprp)*
     -     (q + wpqr - wprp)*(-2 - p - s + wpqr - wprp)*
     -     (-2 - p + s + wpqr - wprp)*wprp*(-1 + wpqr - wprp - wq)*
     -     (-1 + wpqr - wprp + wq)*(p + wprp - wr)*(2 + p + wprp - wr)*
     -     (p + wprp + wr)*(2 + p + wprp + wr)) - 
     -  trc(-p,1 - wq,-1 + p + wpqr + wq,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*(-1 - p - wp)*(-1 - p + wp)*wpqr*(1 - q - wq)*(1 + q - wq)*
     -     (-1 - p - s - wq)*(-1 - p + s - wq)*wq*(-1 + wpqr - wprp + wq)*
     -     (-1 + wpqr + wprp + wq)*(-1 + p + wpqr + wq - wr)*
     -     (1 + p + wpqr + wq - wr)*(-1 + p + wpqr + wq + wr)*
     -     (1 + p + wpqr + wq + wr)) - 
     -  trc(-p,1 + wq,p + wprp,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*p*(-1 - p - wp)*(-1 - p + wp)*wprp*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-1 - p - s + wq)*(-1 - p + s + wq)*(1 - wpqr + wprp + wq)*
     -     (1 + wpqr + wprp + wq)*(p + wprp - wr)*(2 + p + wprp - wr)*
     -     (p + wprp + wr)*(2 + p + wprp + wr)) - 
     -  trc(-p,p + wpqr - wr,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*(-1 - p - wp)*(-1 - p + wp)*wpqr*(p - q + wpqr - wr)*
     -     (p + q + wpqr - wr)*(-2 - s + wpqr - wr)*(-2 + s + wpqr - wr)*
     -     (-1 + p + wpqr - wq - wr)*(-1 + p + wpqr + wq - wr)*wr*
     -     (-p - wprp + wr)*(-p + wprp + wr)*(2 + 2*wr)) + 
     -  trc(-p,2 + p + wpqr - wr,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*(-1 - p - wp)*(-1 - p + wp)*wpqr*(2 + p - q + wpqr - wr)*
     -     (2 + p + q + wpqr - wr)*(-s + wpqr - wr)*(s + wpqr - wr)*
     -     (1 + p + wpqr - wq - wr)*(1 + p + wpqr + wq - wr)*wr*
     -     (-2 - p - wprp + wr)*(-2 - p + wprp + wr)*(-2 + 2*wr)) + 
     -  trc(p,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*q*(-2 + p + q - s)*(-2 + p + q + s)*(-1 + p - wp)*
     -     (-1 + p + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*
     -     (-2 + p + q - wpqr + wr)*(-2 + p + q + wpqr + wr)*
     -     (-2 + p - wprp + wr)*(-2 + p + wprp + wr)*(-2 + 2*wr)) - 
     -  trc(p,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*q*(-2 + p + q - s)*(-2 + p + q + s)*(-1 + p - wp)*
     -     (-1 + p + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*(p + q - wpqr + wr)*
     -     (p + q + wpqr + wr)*(p - wprp + wr)*(p + wprp + wr)*(2 + 2*wr)) + 
     -  trc(p,1 + wq,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*(-1 + p - wp)*(-1 + p + wp)*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-1 + p - s + wq)*(-1 + p + s + wq)*wr*(-2 + p - wprp + wr)*
     -     (-2 + p + wprp + wr)*(-1 + p - wpqr + wq + wr)*
     -     (-1 + p + wpqr + wq + wr)*(-2 + 2*wr)) - 
     -  trc(p,1 + wq,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*p*(-1 + p - wp)*(-1 + p + wp)*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-1 + p - s + wq)*(-1 + p + s + wq)*wr*(p - wprp + wr)*
     -     (p + wprp + wr)*(1 + p - wpqr + wq + wr)*(1 + p + wpqr + wq + wr)*
     -     (2 + 2*wr)) - trc(2 - q - s,q,-2 + s + wpqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*q*(2 - p - q - s)*(2 + p - q - s)*s*(1 - q - s - wp)*
     -     (1 - q - s + wp)*wpqr*(-q + wpqr - wprp)*(-q + wpqr + wprp)*
     -     (-1 + q - wq)*(-1 + q + wq)*(-2 + s + wpqr - wr)*(s + wpqr - wr)*
     -     (-2 + s + wpqr + wr)*(s + wpqr + wr)) - 
     -  trc(2 - q - s,q,-2 + q + s + wprp,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*q*(2 - p - q - s)*(2 + p - q - s)*s*(1 - q - s - wp)*
     -     (1 - q - s + wp)*wprp*(q - wpqr + wprp)*(q + wpqr + wprp)*
     -     (-1 + q - wq)*(-1 + q + wq)*(-2 + q + s + wprp - wr)*
     -     (q + s + wprp - wr)*(-2 + q + s + wprp + wr)*(q + s + wprp + wr))\
     -   + trc(2 - q + s,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*s*(2 - p - q + s)*(2 + p - q + s)*(1 - q + s - wp)*
     -     (1 - q + s + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*(s - wpqr + wr)*
     -     (s + wpqr + wr)*(-q + s - wprp + wr)*(-q + s + wprp + wr)*
     -     (-2 + 2*wr)) - trc(2 - q + s,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*s*(2 - p - q + s)*(2 + p - q + s)*(1 - q + s - wp)*
     -     (1 - q + s + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*
     -     (2 + s - wpqr + wr)*(2 + s + wpqr + wr)*(2 - q + s - wprp + wr)*
     -     (2 - q + s + wprp + wr)*(2 + 2*wr)) - 
     -  trc(1 - wp,-q,-1 + q + wp + wpqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*q*(1 - p - wp)*(1 + p - wp)*(-1 - q - s - wp)*(-1 - q + s - wp)*
     -     wp*wpqr*(q + wpqr - wprp)*(q + wpqr + wprp)*(-1 - q - wq)*
     -     (-1 - q + wq)*(-1 + q + wp + wpqr - wr)*(1 + q + wp + wpqr - wr)*
     -     (-1 + q + wp + wpqr + wr)*(1 + q + wp + wpqr + wr)) - 
     -  trc(1 - wp,q,-1 + wp + wprp,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*q*(1 - p - wp)*(1 + p - wp)*(-1 + q - s - wp)*(-1 + q + s - wp)*
     -     wp*wprp*(q - wpqr + wprp)*(q + wpqr + wprp)*(-1 + q - wq)*
     -     (-1 + q + wq)*(-1 + wp + wprp - wr)*(1 + wp + wprp - wr)*
     -     (-1 + wp + wprp + wr)*(1 + wp + wprp + wr)) - 
     -  trc(1 - wp,1 - s + wp,-2 + s + wpqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*s*(1 - p - wp)*(1 + p - wp)*wp*(1 - q - s + wp)*(1 + q - s + wp)*
     -     wpqr*(-1 + s - wp + wpqr - wprp)*(-1 + s - wp + wpqr + wprp)*
     -     (-s + wp - wq)*(-s + wp + wq)*(-2 + s + wpqr - wr)*(s + wpqr - wr)*
     -     (-2 + s + wpqr + wr)*(s + wpqr + wr)) - 
     -  trc(1 - wp,1 + s + wp,-1 + wp + wprp,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*s*(1 - p - wp)*(1 + p - wp)*wp*(1 - q + s + wp)*(1 + q + s + wp)*
     -     wprp*(1 + s + wp - wpqr + wprp)*(1 + s + wp + wpqr + wprp)*
     -     (s + wp - wq)*(s + wp + wq)*(-1 + wp + wprp - wr)*
     -     (1 + wp + wprp - wr)*(-1 + wp + wprp + wr)*(1 + wp + wprp + wr)) + 
     -  trc(1 - wp,1 + s + wp,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*(1 - p - wp)*(1 + p - wp)*wp*(1 - q + s + wp)*
     -     (1 + q + s + wp)*(s + wp - wq)*(s + wp + wq)*wr*(s - wpqr + wr)*
     -     (s + wpqr + wr)*(-1 - wp - wprp + wr)*(-1 - wp + wprp + wr)*
     -     (-2 + 2*wr)) - trc(1 - wp,1 + s + wp,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*(1 - p - wp)*(1 + p - wp)*wp*(1 - q + s + wp)*
     -     (1 + q + s + wp)*(s + wp - wq)*(s + wp + wq)*wr*
     -     (2 + s - wpqr + wr)*(2 + s + wpqr + wr)*(1 - wp - wprp + wr)*
     -     (1 - wp + wprp + wr)*(2 + 2*wr)) - 
     -  trc(1 - wp,wpqr - wprp,-1 + wp + wprp,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*(1 - p - wp)*(1 + p - wp)*wp*wpqr*(-q + wpqr - wprp)*
     -     (q + wpqr - wprp)*(-1 - s - wp + wpqr - wprp)*
     -     (-1 + s - wp + wpqr - wprp)*wprp*(-1 + wpqr - wprp - wq)*
     -     (-1 + wpqr - wprp + wq)*(-1 + wp + wprp - wr)*(1 + wp + wprp - wr)*
     -     (-1 + wp + wprp + wr)*(1 + wp + wprp + wr)) - 
     -  trc(1 - wp,1 - wq,-2 + wp + wpqr + wq,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*(1 - p - wp)*(1 + p - wp)*wp*wpqr*(1 - q - wq)*(1 + q - wq)*
     -     (-s - wp - wq)*(s - wp - wq)*wq*(-1 + wpqr - wprp + wq)*
     -     (-1 + wpqr + wprp + wq)*(-2 + wp + wpqr + wq - wr)*
     -     (wp + wpqr + wq - wr)*(-2 + wp + wpqr + wq + wr)*
     -     (wp + wpqr + wq + wr)) - 
     -  trc(1 - wp,1 + wq,-1 + wp + wprp,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*(1 - p - wp)*(1 + p - wp)*wp*wprp*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-s - wp + wq)*(s - wp + wq)*(1 - wpqr + wprp + wq)*
     -     (1 + wpqr + wprp + wq)*(-1 + wp + wprp - wr)*(1 + wp + wprp - wr)*
     -     (-1 + wp + wprp + wr)*(1 + wp + wprp + wr)) - 
     -  trc(1 - wp,-1 + wp + wpqr - wr,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*(1 - p - wp)*(1 + p - wp)*wp*wpqr*(-2 - s + wpqr - wr)*
     -     (-2 + s + wpqr - wr)*(-1 - q + wp + wpqr - wr)*
     -     (-1 + q + wp + wpqr - wr)*(-2 + wp + wpqr - wq - wr)*
     -     (-2 + wp + wpqr + wq - wr)*wr*(1 - wp - wprp + wr)*
     -     (1 - wp + wprp + wr)*(2 + 2*wr)) + 
     -  trc(1 - wp,1 + wp + wpqr - wr,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*(1 - p - wp)*(1 + p - wp)*wp*wpqr*(-s + wpqr - wr)*
     -     (s + wpqr - wr)*(1 - q + wp + wpqr - wr)*(1 + q + wp + wpqr - wr)*
     -     (wp + wpqr - wq - wr)*(wp + wpqr + wq - wr)*wr*
     -     (-1 - wp - wprp + wr)*(-1 - wp + wprp + wr)*(-2 + 2*wr)) + 
     -  trc(1 + wp,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*wp*(1 - p + wp)*(1 + p + wp)*(-1 + q - s + wp)*
     -     (-1 + q + s + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*
     -     (-1 + q + wp - wpqr + wr)*(-1 + q + wp + wpqr + wr)*
     -     (-1 + wp - wprp + wr)*(-1 + wp + wprp + wr)*(-2 + 2*wr)) - 
     -  trc(1 + wp,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*wp*(1 - p + wp)*(1 + p + wp)*(-1 + q - s + wp)*
     -     (-1 + q + s + wp)*(-1 + q - wq)*(-1 + q + wq)*wr*
     -     (1 + q + wp - wpqr + wr)*(1 + q + wp + wpqr + wr)*
     -     (1 + wp - wprp + wr)*(1 + wp + wprp + wr)*(2 + 2*wr)) + 
     -  trc(1 + wp,1 + wq,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wp*(1 - p + wp)*(1 + p + wp)*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-s + wp + wq)*(s + wp + wq)*wr*(-1 + wp - wprp + wr)*
     -     (-1 + wp + wprp + wr)*(wp - wpqr + wq + wr)*(wp + wpqr + wq + wr)*
     -     (-2 + 2*wr)) - trc(1 + wp,1 + wq,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wp*(1 - p + wp)*(1 + p + wp)*wq*(1 - q + wq)*(1 + q + wq)*
     -     (-s + wp + wq)*(s + wp + wq)*wr*(1 + wp - wprp + wr)*
     -     (1 + wp + wprp + wr)*(2 + wp - wpqr + wq + wr)*
     -     (2 + wp + wpqr + wq + wr)*(2 + 2*wr)) - 
     -  trc(2 - s - wpqr - wprp,wpqr + wprp,-2 + s + wpqr,pp,qq,rr,pq,pr,qr,
     -    m)/(8.*s*wpqr*(2 - p - s - wpqr - wprp)*(2 + p - s - wpqr - wprp)*
     -     (1 - s - wp - wpqr - wprp)*(1 - s + wp - wpqr - wprp)*wprp*
     -     (-q + wpqr + wprp)*(q + wpqr + wprp)*(-1 + wpqr + wprp - wq)*
     -     (-1 + wpqr + wprp + wq)*(-2 + s + wpqr - wr)*(s + wpqr - wr)*
     -     (-2 + s + wpqr + wr)*(s + wpqr + wr)) - 
     -  trc(1 - s - wq,1 + wq,-2 + s + wpqr,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*s*wpqr*(1 - p - s - wq)*(1 + p - s - wq)*(-s - wp - wq)*
     -     (-s + wp - wq)*(-1 + wpqr - wprp - wq)*(-1 + wpqr + wprp - wq)*wq*
     -     (1 - q + wq)*(1 + q + wq)*(-2 + s + wpqr - wr)*(s + wpqr - wr)*
     -     (-2 + s + wpqr + wr)*(s + wpqr + wr)) - 
     -  trc(1 - s - wq,1 + wq,-1 + s + wprp + wq,pp,qq,rr,pq,pr,qr,m)/
     -   (8.*s*wprp*(1 - p - s - wq)*(1 + p - s - wq)*(-s - wp - wq)*
     -     (-s + wp - wq)*wq*(1 - q + wq)*(1 + q + wq)*(1 - wpqr + wprp + wq)*
     -     (1 + wpqr + wprp + wq)*(-1 + s + wprp + wq - wr)*
     -     (1 + s + wprp + wq - wr)*(-1 + s + wprp + wq + wr)*
     -     (1 + s + wprp + wq + wr)) + 
     -  trc(1 + s - wq,1 + wq,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*(1 - p + s - wq)*(1 + p + s - wq)*(s - wp - wq)*(s + wp - wq)*
     -     wq*(1 - q + wq)*(1 + q + wq)*wr*(s - wpqr + wr)*(s + wpqr + wr)*
     -     (-1 + s - wprp - wq + wr)*(-1 + s + wprp - wq + wr)*(-2 + 2*wr)) - 
     -  trc(1 + s - wq,1 + wq,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*(1 - p + s - wq)*(1 + p + s - wq)*(s - wp - wq)*(s + wp - wq)*
     -     wq*(1 - q + wq)*(1 + q + wq)*wr*(2 + s - wpqr + wr)*
     -     (2 + s + wpqr + wr)*(1 + s - wprp - wq + wr)*
     -     (1 + s + wprp - wq + wr)*(2 + 2*wr)) - 
     -  trc(-q + wpqr - wr,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*wpqr*(-q + wpqr - wprp)*(-q + wpqr + wprp)*(-1 + q - wq)*
     -     (-1 + q + wq)*(-p - q + wpqr - wr)*(p - q + wpqr - wr)*
     -     (-2 - s + wpqr - wr)*(-2 + s + wpqr - wr)*
     -     (-1 - q - wp + wpqr - wr)*(-1 - q + wp + wpqr - wr)*wr*(2 + 2*wr))\
     -   + trc(2 - q + wpqr - wr,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*wpqr*(-q + wpqr - wprp)*(-q + wpqr + wprp)*(-1 + q - wq)*
     -     (-1 + q + wq)*(2 - p - q + wpqr - wr)*(2 + p - q + wpqr - wr)*
     -     (-s + wpqr - wr)*(s + wpqr - wr)*(1 - q - wp + wpqr - wr)*
     -     (1 - q + wp + wpqr - wr)*wr*(-2 + 2*wr)) - 
     -  trc(-wprp - wr,wpqr + wprp,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wpqr*wprp*(-q + wpqr + wprp)*(q + wpqr + wprp)*
     -     (-1 + wpqr + wprp - wq)*(-1 + wpqr + wprp + wq)*
     -     (-2 - s + wpqr - wr)*(-2 + s + wpqr - wr)*(-p - wprp - wr)*
     -     (p - wprp - wr)*(-1 - wp - wprp - wr)*(-1 + wp - wprp - wr)*wr*
     -     (2 + 2*wr)) - trc(-wprp - wr,2 + s + wprp + wr,wr,pp,qq,rr,pq,pr,
     -    qr,m)/
     -   (16.*s*wprp*(-p - wprp - wr)*(p - wprp - wr)*(-1 - wp - wprp - wr)*
     -     (-1 + wp - wprp - wr)*wr*(2 + s - wpqr + wr)*(2 + s + wpqr + wr)*
     -     (2 - q + s + wprp + wr)*(2 + q + s + wprp + wr)*
     -     (1 + s + wprp - wq + wr)*(1 + s + wprp + wq + wr)*(2 + 2*wr)) + 
     -  trc(2 - wprp - wr,wpqr + wprp,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wpqr*wprp*(-q + wpqr + wprp)*(q + wpqr + wprp)*
     -     (-1 + wpqr + wprp - wq)*(-1 + wpqr + wprp + wq)*(-s + wpqr - wr)*
     -     (s + wpqr - wr)*(2 - p - wprp - wr)*(2 + p - wprp - wr)*
     -     (1 - wp - wprp - wr)*(1 + wp - wprp - wr)*wr*(-2 + 2*wr)) + 
     -  trc(2 - wprp - wr,s + wprp + wr,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*s*wprp*(2 - p - wprp - wr)*(2 + p - wprp - wr)*
     -     (1 - wp - wprp - wr)*(1 + wp - wprp - wr)*wr*(s - wpqr + wr)*
     -     (s + wpqr + wr)*(-q + s + wprp + wr)*(q + s + wprp + wr)*
     -     (-1 + s + wprp - wq + wr)*(-1 + s + wprp + wq + wr)*(-2 + 2*wr)) - 
     -  trc(wprp - wr,q,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*wprp*(q - wpqr + wprp)*(q + wpqr + wprp)*(-1 + q - wq)*
     -     (-1 + q + wq)*(-p + wprp - wr)*(p + wprp - wr)*
     -     (-2 + q - s + wprp - wr)*(-2 + q + s + wprp - wr)*
     -     (-1 - wp + wprp - wr)*(-1 + wp + wprp - wr)*wr*(2 + 2*wr)) - 
     -  trc(wprp - wr,1 + wq,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wprp*wq*(1 - q + wq)*(1 + q + wq)*(1 - wpqr + wprp + wq)*
     -     (1 + wpqr + wprp + wq)*(-p + wprp - wr)*(p + wprp - wr)*
     -     (-1 - wp + wprp - wr)*(-1 + wp + wprp - wr)*
     -     (-1 - s + wprp + wq - wr)*(-1 + s + wprp + wq - wr)*wr*(2 + 2*wr))\
     -   + trc(2 + wprp - wr,q,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*q*wprp*(q - wpqr + wprp)*(q + wpqr + wprp)*(-1 + q - wq)*
     -     (-1 + q + wq)*(2 - p + wprp - wr)*(2 + p + wprp - wr)*
     -     (q - s + wprp - wr)*(q + s + wprp - wr)*(1 - wp + wprp - wr)*
     -     (1 + wp + wprp - wr)*wr*(-2 + 2*wr)) + 
     -  trc(2 + wprp - wr,1 + wq,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wprp*wq*(1 - q + wq)*(1 + q + wq)*(1 - wpqr + wprp + wq)*
     -     (1 + wpqr + wprp + wq)*(2 - p + wprp - wr)*(2 + p + wprp - wr)*
     -     (1 - wp + wprp - wr)*(1 + wp + wprp - wr)*(1 - s + wprp + wq - wr)*
     -     (1 + s + wprp + wq - wr)*wr*(-2 + 2*wr)) - 
     -  trc(-1 + wpqr - wq - wr,1 + wq,wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wpqr*(-1 + wpqr - wprp - wq)*(-1 + wpqr + wprp - wq)*wq*
     -     (1 - q + wq)*(1 + q + wq)*(-2 - s + wpqr - wr)*
     -     (-2 + s + wpqr - wr)*(-1 - p + wpqr - wq - wr)*
     -     (-1 + p + wpqr - wq - wr)*(-2 - wp + wpqr - wq - wr)*
     -     (-2 + wp + wpqr - wq - wr)*wr*(2 + 2*wr)) + 
     -  trc(1 + wpqr - wq - wr,1 + wq,-2 + wr,pp,qq,rr,pq,pr,qr,m)/
     -   (16.*wpqr*(-1 + wpqr - wprp - wq)*(-1 + wpqr + wprp - wq)*wq*
     -     (1 - q + wq)*(1 + q + wq)*(-s + wpqr - wr)*(s + wpqr - wr)*
     -     (1 - p + wpqr - wq - wr)*(1 + p + wpqr - wq - wr)*
     -     (-wp + wpqr - wq - wr)*(wp + wpqr - wq - wr)*wr*(-2 + 2*wr))
c
      fnu = fnua+fnub+fnuc
      return
      end
c
c
c
c
c
      double precision function fnbar(p,q,r,thr,phr)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
c
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
      s = dsqrt(p*p+q*q+2.*p*q*ut)
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
c
      fnbar = fcut
      return
      end
