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
      fxx = (fxns(xp,xq,xr,   thr,   phr)
     1     + fxns(xp,xq,xr,pi-thr,pi+phr))/2.
c
c
      fxn = fxx
      return
      end
c
c
      double precision function fxns(xp,xq,xr,thr,phr)
      implicit double precision (a-z)
      common/cnsts/pi,zeta2
      dimension xx(16)
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
      wpr = dsqrt(p*p+r*r+2.*pr+1.)
      wpq = dsqrt(p*p+q*q+2.*pq+1.)
      wqr = dsqrt(q*q+r*r-2.*qr+1.)
      ps = p*p*q*q*r*r*dsin(thr)*(1+r)**2*xp
      s = dsqrt(p*p+q*q+2.*p*q*ut)
c
      fcut =        (-tra(p,q,r,pp,qq,rr,pq,pr,qr) + ztra(p,q,r,pp,qq,rr,pq,pr,qr))/
     -   (8.*p*q*r*(-1 + p - wp)*(-1 + p + wp)*(-1 + p + r - wpr)*
     -     (-1 + p + r + wpr)*(-1 + q - wq)*(-1 + q + wq)*(-1 + q - r - wqr)*
     -     (-1 + q - r + wqr)*(-1 + r - wr)*(1 + r - wr)*(-1 + r + wr)*
     -     (1 + r + wr)) - (tra(p,q,-1 + q + wqr,pp,qq,rr,pq,pr,qr) - 
     -     ztra(p,q,-1 + q + wqr,pp,qq,rr,pq,pr,qr))/
     -   (8.*p*q*(-1 + p - wp)*(-1 + p + wp)*(-1 + q - wq)*(-1 + q + wq)*wqr*
     -     (-1 + q - r + wqr)*(-1 + q + r + wqr)*(-2 + p + q - wpr + wqr)*
     -     (-2 + p + q + wpr + wqr)*(-2 + q + wqr - wr)*(q + wqr - wr)*
     -     (-2 + q + wqr + wr)*(q + wqr + wr)) - 
     -  (-tra(p,q,-1 + wr,pp,qq,rr,pq,pr,qr) + 
     -     ztra(p,q,-1 + wr,pp,qq,rr,pq,pr,qr))/
     -   (16.*p*q*(-1 + p - wp)*(-1 + p + wp)*(-1 + q - wq)*(-1 + q + wq)*
     -     (q - wqr - wr)*(q + wqr - wr)*wr*(-1 - r + wr)*(-1 + r + wr)*
     -     (-2 + p - wpr + wr)*(-2 + p + wpr + wr)*(-2 + 2*wr)) + 
     -  (-tra(p,q,1 + wr,pp,qq,rr,pq,pr,qr) + 
     -     ztra(p,q,1 + wr,pp,qq,rr,pq,pr,qr))/
     -   (16.*p*q*(-1 + p - wp)*(-1 + p + wp)*(-1 + q - wq)*(-1 + q + wq)*
     -     (-2 + q - wqr - wr)*(-2 + q + wqr - wr)*wr*(1 - r + wr)*
     -     (1 + r + wr)*(p - wpr + wr)*(p + wpr + wr)*(2 + 2*wr)) + 
     -  ((-2 + p + q - s)*(tra(2 - q - s,q,-1 + q + s + wpr,pp,qq,rr,pq,pr,
     -        qr) - ztra(2 - q - s,q,-1 + q + s + wpr,pp,qq,rr,pq,pr,qr)))/
     -   (8.*q*(2 + p - q - s)*s*(1 - q - s - wp)*(1 - q - s + wp)*wpr*
     -     (-1 + q - r + s + wpr)*(-1 + q + r + s + wpr)*(-1 + q - wq)*
     -     (-1 + q + wq)*(-s - wpr - wqr)*(-s - wpr + wqr)*
     -     (-2 + q + s + wpr - wr)*(q + s + wpr - wr)*(-2 + q + s + wpr + wr)*
     -     (q + s + wpr + wr)) + 
     -  (-trb(p,q,r,pp,qq,rr,pq,pr,qr) + ztrb(p,q,r,pp,qq,rr,pq,pr,qr))/
     -   (8.*p*q*r*(-1 + p + q - wpq)*(-1 + p + q + wpq)*(-1 + p + r - wpr)*
     -     (-1 + p + r + wpr)*(-1 + q - wq)*(-1 + q + wq)*(-1 + q - r - wqr)*
     -     (-1 + q - r + wqr)*(-1 + r - wr)*(1 + r - wr)*(-1 + r + wr)*
     -     (1 + r + wr)) - (trb(p,q,-1 + q + wqr,pp,qq,rr,pq,pr,qr) - 
     -     ztrb(p,q,-1 + q + wqr,pp,qq,rr,pq,pr,qr))/
     -   (8.*p*q*(-1 + p + q - wpq)*(-1 + p + q + wpq)*(-1 + q - wq)*
     -     (-1 + q + wq)*wqr*(-1 + q - r + wqr)*(-1 + q + r + wqr)*
     -     (-2 + p + q - wpr + wqr)*(-2 + p + q + wpr + wqr)*
     -     (-2 + q + wqr - wr)*(q + wqr - wr)*(-2 + q + wqr + wr)*
     -     (q + wqr + wr)) - (-trb(p,q,-1 + wr,pp,qq,rr,pq,pr,qr) + 
     -     ztrb(p,q,-1 + wr,pp,qq,rr,pq,pr,qr))/
     -   (16.*p*q*(-1 + p + q - wpq)*(-1 + p + q + wpq)*(-1 + q - wq)*
     -     (-1 + q + wq)*(q - wqr - wr)*(q + wqr - wr)*wr*(-1 - r + wr)*
     -     (-1 + r + wr)*(-2 + p - wpr + wr)*(-2 + p + wpr + wr)*(-2 + 2*wr))
     -   + (-trb(p,q,1 + wr,pp,qq,rr,pq,pr,qr) + 
     -     ztrb(p,q,1 + wr,pp,qq,rr,pq,pr,qr))/
     -   (16.*p*q*(-1 + p + q - wpq)*(-1 + p + q + wpq)*(-1 + q - wq)*
     -     (-1 + q + wq)*(-2 + q - wqr - wr)*(-2 + q + wqr - wr)*wr*
     -     (1 - r + wr)*(1 + r + wr)*(p - wpr + wr)*(p + wpr + wr)*(2 + 2*wr))
     -    + ((-2 + p + q - s)*(trb(2 - q - s,q,-1 + q + s + wpr,pp,qq,rr,pq,
     -        pr,qr) - ztrb(2 - q - s,q,-1 + q + s + wpr,pp,qq,rr,pq,pr,qr)))/
     -   (8.*q*(2 + p - q - s)*s*(1 - s - wpq)*(1 - s + wpq)*wpr*
     -     (-1 + q - r + s + wpr)*(-1 + q + r + s + wpr)*(-1 + q - wq)*
     -     (-1 + q + wq)*(-s - wpr - wqr)*(-s - wpr + wqr)*
     -     (-2 + q + s + wpr - wr)*(q + s + wpr - wr)*(-2 + q + s + wpr + wr)*
     -     (q + s + wpr + wr)) + 
     -  (-trc(p,q,r,pp,qq,rr,pq,pr,qr) + ztrc(p,q,r,pp,qq,rr,pq,pr,qr))/
     -   (8.*p*q*r*(-1 + p - wp)*(-1 + p + wp)*(-1 + p + q - wpq)*
     -     (-1 + p + q + wpq)*(-1 + p + r - wpr)*(-1 + p + r + wpr)*
     -     (-1 + q - r - wqr)*(-1 + q - r + wqr)*(-1 + r - wr)*(1 + r - wr)*
     -     (-1 + r + wr)*(1 + r + wr)) - 
     -  (trc(p,q,-1 + q + wqr,pp,qq,rr,pq,pr,qr) - 
     -     ztrc(p,q,-1 + q + wqr,pp,qq,rr,pq,pr,qr))/
     -   (8.*p*q*(-1 + p - wp)*(-1 + p + wp)*(-1 + p + q - wpq)*
     -     (-1 + p + q + wpq)*wqr*(-1 + q - r + wqr)*(-1 + q + r + wqr)*
     -     (-2 + p + q - wpr + wqr)*(-2 + p + q + wpr + wqr)*
     -     (-2 + q + wqr - wr)*(q + wqr - wr)*(-2 + q + wqr + wr)*
     -     (q + wqr + wr)) - (-trc(p,q,-1 + wr,pp,qq,rr,pq,pr,qr) + 
     -     ztrc(p,q,-1 + wr,pp,qq,rr,pq,pr,qr))/
     -   (16.*p*q*(-1 + p - wp)*(-1 + p + wp)*(-1 + p + q - wpq)*
     -     (-1 + p + q + wpq)*(q - wqr - wr)*(q + wqr - wr)*wr*(-1 - r + wr)*
     -     (-1 + r + wr)*(-2 + p - wpr + wr)*(-2 + p + wpr + wr)*(-2 + 2*wr))
     -   + (-trc(p,q,1 + wr,pp,qq,rr,pq,pr,qr) + 
     -     ztrc(p,q,1 + wr,pp,qq,rr,pq,pr,qr))/
     -   (16.*p*q*(-1 + p - wp)*(-1 + p + wp)*(-1 + p + q - wpq)*
     -     (-1 + p + q + wpq)*(-2 + q - wqr - wr)*(-2 + q + wqr - wr)*wr*
     -     (1 - r + wr)*(1 + r + wr)*(p - wpr + wr)*(p + wpr + wr)*(2 + 2*wr))
     -    + ((-2 + p + q - s)*(trc(2 - q - s,q,-1 + q + s + wpr,pp,qq,rr,pq,
     -        pr,qr) - ztrc(2 - q - s,q,-1 + q + s + wpr,pp,qq,rr,pq,pr,qr)))/
     -   (8.*q*(2 + p - q - s)*s*(1 - q - s - wp)*(1 - q - s + wp)*
     -     (1 - s - wpq)*(1 - s + wpq)*wpr*(-1 + q - r + s + wpr)*
     -     (-1 + q + r + s + wpr)*(-s - wpr - wqr)*(-s - wpr + wqr)*
     -     (-2 + q + s + wpr - wr)*(q + s + wpr - wr)*(-2 + q + s + wpr + wr)*
     -     (q + s + wpr + wr))
      fxns = (2.d0/(pi))*(2.d0/3.d0)*ps*pi*fcut/(-2.*p*q)
      return
      end
