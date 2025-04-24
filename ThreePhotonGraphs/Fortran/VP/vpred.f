      double precision function tra(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      ans2=8.0*pr*r0-6.0*pr*rr+10.0*pr+12.0*q0**2*r0**2+24.0*q0**2*r0+
     . 18.0*q0*r0**3+42.0*q0*r0**2+14.0*q0*r0*rr+30.0*q0*r0+22.0*q0*rr+
     . 30.0*q0-10.0*qq*r0**2-24.0*qq*r0-2.0*qq*rr-10.0*qq-26.0*qr*r0**2
     . -72.0*qr*r0-6.0*qr*rr-46.0*qr-12.0*r0**3-24.0*r0**2-4.0*r0*rr+
     . 12.0*r0-8.0*rr+24.0
      ans1=-6.0*p0**2*q0**2*r0-6.0*p0**2*q0**2+2.0*p0**2*q0*qr-9.0*p0**
     . 2*q0*r0**2-6.0*p0**2*q0*r0+p0**2*q0*rr+3.0*p0**2*q0+4.0*p0**2*qq
     . *r0+10.0*p0**2*qq+8.0*p0**2*qr*r0+14.0*p0**2*qr+6.0*p0**2*r0**2-
     . 2.0*p0**2*rr-6.0*p0**2-8.0*p0*pq*r0-12.0*p0*pq+2.0*p0*pr*q0**2-
     . 2.0*p0*pr*qq-8.0*p0*pr+9.0*p0*q0**2*r0**2+18.0*p0*q0**2*r0-p0*q0
     . **2*rr+9.0*p0*q0**2+4.0*p0*q0*qr+12.0*p0*q0*r0**3+18.0*p0*q0*r0
     . **2+4.0*p0*q0*r0*rr+6.0*p0*q0*rr-6.0*p0*q0-9.0*p0*qq*r0**2-22.0*
     . p0*qq*r0+p0*qq*rr-13.0*p0*qq-16.0*p0*qr*r0**2-24.0*p0*qr*r0-8.0*
     . p0*qr-6.0*p0*r0**3-6.0*p0*r0**2+6.0*p0*r0*rr+6.0*p0*r0+6.0*p0*rr
     . +6.0*p0+4.0*pp*q0**2*r0-2.0*pp*q0**2-2.0*pp*q0*qr+9.0*pp*q0*r0**
     . 2+6.0*pp*q0*r0-pp*q0*rr+9.0*pp*q0-2.0*pp*qq*r0-2.0*pp*qq-8.0*pp*
     . qr*r0-8.0*pp*qr-8.0*pp*r0**2+4.0*pp*rr+8.0*pp+2.0*pq*pr-4.0*pq*
     . q0*r0-12.0*pq*q0-2.0*pq*qr+4.0*pq*r0**3+14.0*pq*r0**2-4.0*pq*r0*
     . rr+24.0*pq*r0-6.0*pq*rr+22.0*pq-8.0*pr*q0**2*r0-2.0*pr*q0**2-
     . 16.0*pr*q0*r0**2-24.0*pr*q0*r0-24.0*pr*q0+8.0*pr*qq*r0+8.0*pr*qq
     . +16.0*pr*qr*r0+16.0*pr*qr+6.0*pr*r0**2+ans2
      tra=2.0*ans1
      return
      end
c
      double precision function tra100(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      tra100=2.0*(-12.0*p0*q0**2*r0-12.0*p0*q0**2+4.0*p0*q0*qr-18.0*p0*
     . q0*r0**2-12.0*p0*q0*r0+2.0*p0*q0*rr+6.0*p0*q0+8.0*p0*qq*r0+20.0*
     . p0*qq+16.0*p0*qr*r0+28.0*p0*qr+12.0*p0*r0**2-4.0*p0*rr-12.0*p0-
     . 8.0*pq*r0-12.0*pq+2.0*pr*q0**2-2.0*pr*qq-8.0*pr+9.0*q0**2*r0**2+
     . 18.0*q0**2*r0-q0**2*rr+9.0*q0**2+4.0*q0*qr+12.0*q0*r0**3+18.0*q0
     . *r0**2+4.0*q0*r0*rr+6.0*q0*rr-6.0*q0-9.0*qq*r0**2-22.0*qq*r0+qq*
     . rr-13.0*qq-16.0*qr*r0**2-24.0*qr*r0-8.0*qr-6.0*r0**3-6.0*r0**2+
     . 6.0*r0*rr+6.0*r0+6.0*rr+6.0)
      return
      end
c
      double precision function tra010(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      tra010=2.0*(-12.0*p0**2*q0*r0-12.0*p0**2*q0+2.0*p0**2*qr-9.0*p0**
     . 2*r0**2-6.0*p0**2*r0+p0**2*rr+3.0*p0**2+4.0*p0*pr*q0+18.0*p0*q0*
     . r0**2+36.0*p0*q0*r0-2.0*p0*q0*rr+18.0*p0*q0+4.0*p0*qr+12.0*p0*r0
     . **3+18.0*p0*r0**2+4.0*p0*r0*rr+6.0*p0*rr-6.0*p0+8.0*pp*q0*r0-4.0
     . *pp*q0-2.0*pp*qr+9.0*pp*r0**2+6.0*pp*r0-pp*rr+9.0*pp-4.0*pq*r0-
     . 12.0*pq-16.0*pr*q0*r0-4.0*pr*q0-16.0*pr*r0**2-24.0*pr*r0-24.0*pr
     . +24.0*q0*r0**2+48.0*q0*r0+18.0*r0**3+42.0*r0**2+14.0*r0*rr+30.0*
     . r0+22.0*rr+30.0)
      return
      end
c
      double precision function tra110(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      tra110=4.0*(-12.0*p0*q0*r0-12.0*p0*q0+2.0*p0*qr-9.0*p0*r0**2-6.0*
     . p0*r0+p0*rr+3.0*p0+2.0*pr*q0+9.0*q0*r0**2+18.0*q0*r0-q0*rr+9.0*
     . q0+2.0*qr+6.0*r0**3+9.0*r0**2+2.0*r0*rr+3.0*rr-3.0)
      return
      end
c
      double precision function tra101(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      tra101=4.0*(-6.0*p0*q0**2-18.0*p0*q0*r0-6.0*p0*q0+4.0*p0*qq+8.0*
     . p0*qr+12.0*p0*r0-4.0*pq+9.0*q0**2*r0+9.0*q0**2+18.0*q0*r0**2+
     . 18.0*q0*r0+2.0*q0*rr-9.0*qq*r0-11.0*qq-16.0*qr*r0-12.0*qr-9.0*r0
     . **2-6.0*r0+3.0*rr+3.0)
      return
      end
c
      double precision function tra001(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      tra001=4.0*(-3.0*p0**2*q0**2-9.0*p0**2*q0*r0-3.0*p0**2*q0+2.0*p0
     . **2*qq+4.0*p0**2*qr+6.0*p0**2*r0-4.0*p0*pq+9.0*p0*q0**2*r0+9.0*
     . p0*q0**2+18.0*p0*q0*r0**2+18.0*p0*q0*r0+2.0*p0*q0*rr-9.0*p0*qq*
     . r0-11.0*p0*qq-16.0*p0*qr*r0-12.0*p0*qr-9.0*p0*r0**2-6.0*p0*r0+
     . 3.0*p0*rr+3.0*p0+2.0*pp*q0**2+9.0*pp*q0*r0+3.0*pp*q0-pp*qq-4.0*
     . pp*qr-8.0*pp*r0-2.0*pq*q0+6.0*pq*r0**2+14.0*pq*r0-2.0*pq*rr+12.0
     . *pq-4.0*pr*q0**2-16.0*pr*q0*r0-12.0*pr*q0+4.0*pr*qq+8.0*pr*qr+
     . 6.0*pr*r0+4.0*pr+12.0*q0**2*r0+12.0*q0**2+27.0*q0*r0**2+42.0*q0*
     . r0+7.0*q0*rr+15.0*q0-10.0*qq*r0-12.0*qq-26.0*qr*r0-36.0*qr-18.0*
     . r0**2-24.0*r0-2.0*rr+6.0)
      return
      end
c
      double precision function tra011(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      tra011=4.0*(-6.0*p0**2*q0-9.0*p0**2*r0-3.0*p0**2+18.0*p0*q0*r0+
     . 18.0*p0*q0+18.0*p0*r0**2+18.0*p0*r0+2.0*p0*rr+4.0*pp*q0+9.0*pp*
     . r0+3.0*pp-2.0*pq-8.0*pr*q0-16.0*pr*r0-12.0*pr+24.0*q0*r0+24.0*q0
     . +27.0*r0**2+42.0*r0+7.0*rr+15.0)
      return
      end
c
      double precision function tra111(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      tra111=8.0*(-6.0*p0*q0-9.0*p0*r0-3.0*p0+9.0*q0*r0+9.0*q0+9.0*r0**
     . 2+9.0*r0+rr)
      return
      end
c
c
      double precision function trb(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      ans2=-2.0*pq*qq+8.0*pq*qr+8.0*pq*r0**3+2.0*pq*r0**2-8.0*pq*r0*rr+
     . 8.0*pq*r0+6.0*pq*rr+6.0*pq+2.0*pr*q0**3-16.0*pr*q0**2*r0-18.0*pr
     . *q0**2-2.0*pr*q0*qq-24.0*pr*q0*r0**2-8.0*pr*q0*r0-8.0*pr*q0*rr+
     . 12.0*pr*q0+16.0*pr*qq*r0+18.0*pr*qq+32.0*pr*qr*r0+32.0*pr*qr+
     . 22.0*pr*r0**2+16.0*pr*r0+10.0*pr*rr-6.0*pr-4.0*q0**2*qr+12.0*q0
     . **2*r0**2+24.0*q0**2*r0-4.0*q0**2*rr+8.0*q0*qr+12.0*q0*r0**3+
     . 12.0*q0*r0**2+4.0*q0*r0*rr-12.0*q0*r0+12.0*q0*rr+12.0*q0+4.0*qq*
     . qr-16.0*qq*r0**2-32.0*qq*r0+8.0*qq*rr+4.0*qq-20.0*qr*r0**2-40.0*
     . qr*r0+4.0*qr*rr-4.0*qr-12.0*r0**3-12.0*r0**2-4.0*r0*rr+12.0*r0-
     . 4.0*rr+12.0
      ans1=-6.0*p0**2*q0**2*r0-6.0*p0**2*q0**2-2.0*p0**2*q0*qr+3.0*p0**
     . 2*q0*r0**2+30.0*p0**2*q0*r0+5.0*p0**2*q0*rr+27.0*p0**2*q0-6.0*p0
     . **2*qq-8.0*p0**2*qr*r0-14.0*p0**2*qr+6.0*p0**2*r0**2-10.0*p0**2*
     . rr-6.0*p0**2+12.0*p0*pq*q0*r0+16.0*p0*pq*q0+2.0*p0*pq*qr-16.0*p0
     . *pq*r0-20.0*p0*pq+2.0*p0*pr*q0**2-8.0*p0*pr-6.0*p0*q0**3*r0-6.0*
     . p0*q0**3-2.0*p0*q0**2*qr+6.0*p0*q0**2*r0**2+42.0*p0*q0**2*r0+
     . 10.0*p0*q0**2*rr+30.0*p0*q0**2+6.0*p0*q0*qq*r0+6.0*p0*q0*qq+12.0
     . *p0*q0*r0**3+48.0*p0*q0*r0**2+20.0*p0*q0*r0*rr+12.0*p0*q0*r0-8.0
     . *p0*q0*rr-12.0*p0*q0+2.0*p0*qq*qr-14.0*p0*qq*r0**2-46.0*p0*qq*r0
     . -2.0*p0*qq*rr-34.0*p0*qq-28.0*p0*qr*r0**2-72.0*p0*qr*r0-4.0*p0*
     . qr*rr-48.0*p0*qr-6.0*p0*r0**3-30.0*p0*r0**2-26.0*p0*r0*rr+6.0*p0
     . *r0-10.0*p0*rr+30.0*p0-2.0*pp*q0**2-3.0*pp*q0*r0**2-14.0*pp*q0*
     . r0-5.0*pp*q0*rr-7.0*pp*q0+2.0*pp*qq+8.0*pp*qr*r0+12.0*pp*qr-4.0*
     . pp*r0**2+8.0*pp*rr+4.0*pp-6.0*pq**2*r0-4.0*pq**2-2.0*pq*pr*q0+
     . 2.0*pq*pr+6.0*pq*q0**2*r0+2.0*pq*q0**2+8.0*pq*q0*r0**2-4.0*pq*q0
     . *r0-8.0*pq*q0*rr+4.0*pq*q0-6.0*pq*qq*r0+ans2
      trb=2.0*ans1
      return
      end
c
      double precision function trb100(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      trb100=4.0*(-6.0*p0*q0**2*r0-6.0*p0*q0**2-2.0*p0*q0*qr+3.0*p0*q0*
     . r0**2+30.0*p0*q0*r0+5.0*p0*q0*rr+27.0*p0*q0-6.0*p0*qq-8.0*p0*qr*
     . r0-14.0*p0*qr+6.0*p0*r0**2-10.0*p0*rr-6.0*p0+6.0*pq*q0*r0+8.0*pq
     . *q0+pq*qr-8.0*pq*r0-10.0*pq+pr*q0**2-4.0*pr-3.0*q0**3*r0-3.0*q0
     . **3-q0**2*qr+3.0*q0**2*r0**2+21.0*q0**2*r0+5.0*q0**2*rr+15.0*q0
     . **2+3.0*q0*qq*r0+3.0*q0*qq+6.0*q0*r0**3+24.0*q0*r0**2+10.0*q0*r0
     . *rr+6.0*q0*r0-4.0*q0*rr-6.0*q0+qq*qr-7.0*qq*r0**2-23.0*qq*r0-qq*
     . rr-17.0*qq-14.0*qr*r0**2-36.0*qr*r0-2.0*qr*rr-24.0*qr-3.0*r0**3-
     . 15.0*r0**2-13.0*r0*rr+3.0*r0-5.0*rr+15.0)
      return
      end
c
      double precision function trb010(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      trb010=2.0*(-12.0*p0**2*q0*r0-12.0*p0**2*q0-2.0*p0**2*qr+3.0*p0**
     . 2*r0**2+30.0*p0**2*r0+5.0*p0**2*rr+27.0*p0**2+12.0*p0*pq*r0+16.0
     . *p0*pq+4.0*p0*pr*q0-18.0*p0*q0**2*r0-18.0*p0*q0**2-4.0*p0*q0*qr+
     . 12.0*p0*q0*r0**2+84.0*p0*q0*r0+20.0*p0*q0*rr+60.0*p0*q0+6.0*p0*
     . qq*r0+6.0*p0*qq+12.0*p0*r0**3+48.0*p0*r0**2+20.0*p0*r0*rr+12.0*
     . p0*r0-8.0*p0*rr-12.0*p0-4.0*pp*q0-3.0*pp*r0**2-14.0*pp*r0-5.0*pp
     . *rr-7.0*pp-2.0*pq*pr+12.0*pq*q0*r0+4.0*pq*q0+8.0*pq*r0**2-4.0*pq
     . *r0-8.0*pq*rr+4.0*pq+6.0*pr*q0**2-32.0*pr*q0*r0-36.0*pr*q0-2.0*
     . pr*qq-24.0*pr*r0**2-8.0*pr*r0-8.0*pr*rr+12.0*pr-8.0*q0*qr+24.0*
     . q0*r0**2+48.0*q0*r0-8.0*q0*rr+8.0*qr+12.0*r0**3+12.0*r0**2+4.0*
     . r0*rr-12.0*r0+12.0*rr+12.0)
      return
      end
c
      double precision function trb110(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      trb110=4.0*(-12.0*p0*q0*r0-12.0*p0*q0-2.0*p0*qr+3.0*p0*r0**2+30.0
     . *p0*r0+5.0*p0*rr+27.0*p0+6.0*pq*r0+8.0*pq+2.0*pr*q0-9.0*q0**2*r0
     . -9.0*q0**2-2.0*q0*qr+6.0*q0*r0**2+42.0*q0*r0+10.0*q0*rr+30.0*q0+
     . 3.0*qq*r0+3.0*qq+6.0*r0**3+24.0*r0**2+10.0*r0*rr+6.0*r0-4.0*rr-
     . 6.0)
      return
      end
c
      double precision function trb101(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      trb101=4.0*(-6.0*p0*q0**2+6.0*p0*q0*r0+30.0*p0*q0-8.0*p0*qr+12.0*
     . p0*r0+6.0*pq*q0-8.0*pq-3.0*q0**3+6.0*q0**2*r0+21.0*q0**2+3.0*q0*
     . qq+18.0*q0*r0**2+48.0*q0*r0+10.0*q0*rr+6.0*q0-14.0*qq*r0-23.0*qq
     . -28.0*qr*r0-36.0*qr-9.0*r0**2-30.0*r0-13.0*rr+3.0)
      return
      end
c
      double precision function trb001(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      trb001=4.0*(-3.0*p0**2*q0**2+3.0*p0**2*q0*r0+15.0*p0**2*q0-4.0*p0
     . **2*qr+6.0*p0**2*r0+6.0*p0*pq*q0-8.0*p0*pq-3.0*p0*q0**3+6.0*p0*
     . q0**2*r0+21.0*p0*q0**2+3.0*p0*q0*qq+18.0*p0*q0*r0**2+48.0*p0*q0*
     . r0+10.0*p0*q0*rr+6.0*p0*q0-14.0*p0*qq*r0-23.0*p0*qq-28.0*p0*qr*
     . r0-36.0*p0*qr-9.0*p0*r0**2-30.0*p0*r0-13.0*p0*rr+3.0*p0-3.0*pp*
     . q0*r0-7.0*pp*q0+4.0*pp*qr-4.0*pp*r0-3.0*pq**2+3.0*pq*q0**2+8.0*
     . pq*q0*r0-2.0*pq*q0-3.0*pq*qq+12.0*pq*r0**2+2.0*pq*r0-4.0*pq*rr+
     . 4.0*pq-8.0*pr*q0**2-24.0*pr*q0*r0-4.0*pr*q0+8.0*pr*qq+16.0*pr*qr
     . +22.0*pr*r0+8.0*pr+12.0*q0**2*r0+12.0*q0**2+18.0*q0*r0**2+12.0*
     . q0*r0+2.0*q0*rr-6.0*q0-16.0*qq*r0-16.0*qq-20.0*qr*r0-20.0*qr-
     . 18.0*r0**2-12.0*r0-2.0*rr+6.0)
      return
      end
c
      double precision function trb011(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      trb011=4.0*(-6.0*p0**2*q0+3.0*p0**2*r0+15.0*p0**2+6.0*p0*pq-9.0*
     . p0*q0**2+12.0*p0*q0*r0+42.0*p0*q0+3.0*p0*qq+18.0*p0*r0**2+48.0*
     . p0*r0+10.0*p0*rr+6.0*p0-3.0*pp*r0-7.0*pp+6.0*pq*q0+8.0*pq*r0-2.0
     . *pq-16.0*pr*q0-24.0*pr*r0-4.0*pr+24.0*q0*r0+24.0*q0+18.0*r0**2+
     . 12.0*r0+2.0*rr-6.0)
      return
      end
c
      double precision function trb111(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      trb111=4.0*(-12.0*p0*q0+6.0*p0*r0+30.0*p0+6.0*pq-9.0*q0**2+12.0*
     . q0*r0+42.0*q0+3.0*qq+18.0*r0**2+48.0*r0+10.0*rr+6.0)
      return
      end
c
c
      double precision function trc(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      ans2=36.0*pp*r0+12.0*pp*rr+24.0*pp-6.0*pq**2*r0-4.0*pq**2+2.0*pq*
     . pr*q0+4.0*pq*pr-4.0*pq*q0*r0-4.0*pq*q0+2.0*pq*qr+8.0*pq*r0**3+
     . 4.0*pq*r0**2-8.0*pq*r0*rr+12.0*pq*r0-4.0*pq*rr+16.0*pq-8.0*pr*q0
     . **2*r0-14.0*pr*q0**2-28.0*pr*q0*r0**2-24.0*pr*q0*r0-4.0*pr*q0*rr
     . +12.0*pr*q0+8.0*pr*qq*r0+8.0*pr*qq+32.0*pr*qr*r0+24.0*pr*qr+36.0
     . *pr*r0**2+48.0*pr*r0+12.0*pr*rr+12.0*pr+12.0*q0**2*r0**2+24.0*q0
     . **2*r0+18.0*q0*r0**3+18.0*q0*r0**2+14.0*q0*r0*rr-18.0*q0*r0+6.0*
     . q0*rr+6.0*q0-10.0*qq*r0**2-16.0*qq*r0-2.0*qq*rr-2.0*qq-26.0*qr*
     . r0**2-32.0*qr*r0-6.0*qr*rr-6.0*qr-24.0*r0**3-36.0*r0**2-24.0*r0*
     . rr-24.0*r0-12.0*rr-12.0
      ans1=-6.0*p0**3*q0*r0-6.0*p0**3*q0+2.0*p0**3*qr+12.0*p0**3*r0+
     . 12.0*p0**3+6.0*p0**2*pq*r0+2.0*p0**2*pq-2.0*p0**2*pr*q0-6.0*p0**
     . 2*q0**2*r0-6.0*p0**2*q0**2+2.0*p0**2*q0*qr+6.0*p0**2*q0*r0**2+
     . 42.0*p0**2*q0*r0+10.0*p0**2*q0*rr+30.0*p0**2*q0-2.0*p0**2*qq-
     . 16.0*p0**2*qr*r0-14.0*p0**2*qr-36.0*p0**2*r0-24.0*p0**2*rr-36.0*
     . p0**2+6.0*p0*pp*q0*r0+6.0*p0*pp*q0-2.0*p0*pp*qr-12.0*p0*pp*r0-
     . 12.0*p0*pp+12.0*p0*pq*q0*r0+16.0*p0*pq*q0-2.0*p0*pq*qr+8.0*p0*pq
     . *r0**2-16.0*p0*pq*r0-8.0*p0*pq*rr-20.0*p0*pq-2.0*p0*pr*q0**2+4.0
     . *p0*pr*q0+3.0*p0*q0**2*r0**2+18.0*p0*q0**2*r0+5.0*p0*q0**2*rr+
     . 15.0*p0*q0**2+4.0*p0*q0*qr+12.0*p0*q0*r0**3+54.0*p0*q0*r0**2+
     . 20.0*p0*q0*r0*rr+24.0*p0*q0*r0+2.0*p0*q0*rr-30.0*p0*q0-3.0*p0*qq
     . *r0**2-14.0*p0*qq*r0-5.0*p0*qq*rr-11.0*p0*qq-24.0*p0*qr*r0**2-
     . 56.0*p0*qr*r0-8.0*p0*qr*rr-32.0*p0*qr-12.0*p0*r0**3-72.0*p0*r0**
     . 2-36.0*p0*r0*rr-60.0*p0*r0-24.0*p0*rr-6.0*pp*pq*r0-2.0*pp*pq+2.0
     . *pp*pr*q0-6.0*pp*q0**2-14.0*pp*q0*r0**2-34.0*pp*q0*r0-2.0*pp*q0*
     . rr-10.0*pp*q0+2.0*pp*qq+16.0*pp*qr*r0+14.0*pp*qr+12.0*pp*r0**2+
     . ans2
      trc=2.0*ans1
      return
      end
c
      double precision function trc100(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      trc100=2.0*(-18.0*p0**2*q0*r0-18.0*p0**2*q0+6.0*p0**2*qr+36.0*p0
     . **2*r0+36.0*p0**2+12.0*p0*pq*r0+4.0*p0*pq-4.0*p0*pr*q0-12.0*p0*
     . q0**2*r0-12.0*p0*q0**2+4.0*p0*q0*qr+12.0*p0*q0*r0**2+84.0*p0*q0*
     . r0+20.0*p0*q0*rr+60.0*p0*q0-4.0*p0*qq-32.0*p0*qr*r0-28.0*p0*qr-
     . 72.0*p0*r0-48.0*p0*rr-72.0*p0+6.0*pp*q0*r0+6.0*pp*q0-2.0*pp*qr-
     . 12.0*pp*r0-12.0*pp+12.0*pq*q0*r0+16.0*pq*q0-2.0*pq*qr+8.0*pq*r0
     . **2-16.0*pq*r0-8.0*pq*rr-20.0*pq-2.0*pr*q0**2+4.0*pr*q0+3.0*q0**
     . 2*r0**2+18.0*q0**2*r0+5.0*q0**2*rr+15.0*q0**2+4.0*q0*qr+12.0*q0*
     . r0**3+54.0*q0*r0**2+20.0*q0*r0*rr+24.0*q0*r0+2.0*q0*rr-30.0*q0-
     . 3.0*qq*r0**2-14.0*qq*r0-5.0*qq*rr-11.0*qq-24.0*qr*r0**2-56.0*qr*
     . r0-8.0*qr*rr-32.0*qr-12.0*r0**3-72.0*r0**2-36.0*r0*rr-60.0*r0-
     . 24.0*rr)
      return
      end
c
      double precision function trc010(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      trc010=4.0*(-3.0*p0**3*r0-3.0*p0**3-p0**2*pr-6.0*p0**2*q0*r0-6.0*
     . p0**2*q0+p0**2*qr+3.0*p0**2*r0**2+21.0*p0**2*r0+5.0*p0**2*rr+
     . 15.0*p0**2+3.0*p0*pp*r0+3.0*p0*pp+6.0*p0*pq*r0+8.0*p0*pq-2.0*p0*
     . pr*q0+2.0*p0*pr+3.0*p0*q0*r0**2+18.0*p0*q0*r0+5.0*p0*q0*rr+15.0*
     . p0*q0+2.0*p0*qr+6.0*p0*r0**3+27.0*p0*r0**2+10.0*p0*r0*rr+12.0*p0
     . *r0+p0*rr-15.0*p0+pp*pr-6.0*pp*q0-7.0*pp*r0**2-17.0*pp*r0-pp*rr-
     . 5.0*pp+pq*pr-2.0*pq*r0-2.0*pq-8.0*pr*q0*r0-14.0*pr*q0-14.0*pr*r0
     . **2-12.0*pr*r0-2.0*pr*rr+6.0*pr+12.0*q0*r0**2+24.0*q0*r0+9.0*r0
     . **3+9.0*r0**2+7.0*r0*rr-9.0*r0+3.0*rr+3.0)
      return
      end
c
      double precision function trc110(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      trc110=4.0*(-9.0*p0**2*r0-9.0*p0**2-2.0*p0*pr-12.0*p0*q0*r0-12.0*
     . p0*q0+2.0*p0*qr+6.0*p0*r0**2+42.0*p0*r0+10.0*p0*rr+30.0*p0+3.0*
     . pp*r0+3.0*pp+6.0*pq*r0+8.0*pq-2.0*pr*q0+2.0*pr+3.0*q0*r0**2+18.0
     . *q0*r0+5.0*q0*rr+15.0*q0+2.0*qr+6.0*r0**3+27.0*r0**2+10.0*r0*rr+
     . 12.0*r0+rr-15.0)
      return
      end
c
      double precision function trc101(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      trc101=4.0*(-9.0*p0**2*q0+18.0*p0**2+6.0*p0*pq-6.0*p0*q0**2+12.0*
     . p0*q0*r0+42.0*p0*q0-16.0*p0*qr-36.0*p0+3.0*pp*q0-6.0*pp+6.0*pq*
     . q0+8.0*pq*r0-8.0*pq+3.0*q0**2*r0+9.0*q0**2+18.0*q0*r0**2+54.0*q0
     . *r0+10.0*q0*rr+12.0*q0-3.0*qq*r0-7.0*qq-24.0*qr*r0-28.0*qr-18.0*
     . r0**2-72.0*r0-18.0*rr-30.0)
      return
      end
c
      double precision function trc001(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      trc001=4.0*(-3.0*p0**3*q0+6.0*p0**3+3.0*p0**2*pq-3.0*p0**2*q0**2+
     . 6.0*p0**2*q0*r0+21.0*p0**2*q0-8.0*p0**2*qr-18.0*p0**2+3.0*p0*pp*
     . q0-6.0*p0*pp+6.0*p0*pq*q0+8.0*p0*pq*r0-8.0*p0*pq+3.0*p0*q0**2*r0
     . +9.0*p0*q0**2+18.0*p0*q0*r0**2+54.0*p0*q0*r0+10.0*p0*q0*rr+12.0*
     . p0*q0-3.0*p0*qq*r0-7.0*p0*qq-24.0*p0*qr*r0-28.0*p0*qr-18.0*p0*r0
     . **2-72.0*p0*r0-18.0*p0*rr-30.0*p0-3.0*pp*pq-14.0*pp*q0*r0-17.0*
     . pp*q0+8.0*pp*qr+12.0*pp*r0+18.0*pp-3.0*pq**2-2.0*pq*q0+12.0*pq*
     . r0**2+4.0*pq*r0-4.0*pq*rr+6.0*pq-4.0*pr*q0**2-28.0*pr*q0*r0-12.0
     . *pr*q0+4.0*pr*qq+16.0*pr*qr+36.0*pr*r0+24.0*pr+12.0*q0**2*r0+
     . 12.0*q0**2+27.0*q0*r0**2+18.0*q0*r0+7.0*q0*rr-9.0*q0-10.0*qq*r0-
     . 8.0*qq-26.0*qr*r0-16.0*qr-36.0*r0**2-36.0*r0-12.0*rr-12.0)
      return
      end
c
      double precision function trc011(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      trc011=4.0*(-3.0*p0**3-6.0*p0**2*q0+6.0*p0**2*r0+21.0*p0**2+3.0*
     . p0*pp+6.0*p0*pq+6.0*p0*q0*r0+18.0*p0*q0+18.0*p0*r0**2+54.0*p0*r0
     . +10.0*p0*rr+12.0*p0-14.0*pp*r0-17.0*pp-2.0*pq-8.0*pr*q0-28.0*pr*
     . r0-12.0*pr+24.0*q0*r0+24.0*q0+27.0*r0**2+18.0*r0+7.0*rr-9.0)
      return
      end
c
      double precision function trc111(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      trc111=4.0*(-9.0*p0**2-12.0*p0*q0+12.0*p0*r0+42.0*p0+3.0*pp+6.0*
     . pq+6.0*q0*r0+18.0*q0+18.0*r0**2+54.0*r0+10.0*rr+12.0)
      return
      end
c
c
c
c
      double precision function mtra(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      ans2=7.0*p0*rr+4.0*pp*q0**2*r0-2.0*pp*q0**2-2.0*pp*q0*qr+9.0*pp*
     . q0*r0**2+6.0*pp*q0*r0-pp*q0*rr+12.0*pp*q0-2.0*pp*qq*r0-2.0*pp*qq
     . -8.0*pp*qr*r0-8.0*pp*qr-8.0*pp*r0**2+4.0*pp*rr+2.0*pq*pr-4.0*pq*
     . q0*r0-12.0*pq*q0-2.0*pq*qr+4.0*pq*r0**3+14.0*pq*r0**2-4.0*pq*r0*
     . rr+26.0*pq*r0-6.0*pq*rr+30.0*pq-8.0*pr*q0**2*r0-2.0*pr*q0**2-
     . 16.0*pr*q0*r0**2-24.0*pr*q0*r0-28.0*pr*q0+8.0*pr*qq*r0+8.0*pr*qq
     . +16.0*pr*qr*r0+16.0*pr*qr+6.0*pr*r0**2+4.0*pr*r0-6.0*pr*rr+20.0*
     . pr+12.0*q0**2*r0**2+24.0*q0**2*r0+18.0*q0*r0**3+39.0*q0*r0**2+
     . 14.0*q0*r0*rr+12.0*q0*r0+21.0*q0*rr+12.0*q0-10.0*qq*r0**2-24.0*
     . qq*r0-2.0*qq*rr-8.0*qq-26.0*qr*r0**2-68.0*qr*r0-6.0*qr*rr-36.0*
     . qr-12.0*r0**3-24.0*r0**2-4.0*r0*rr-8.0*rr
      ans1=-3.0*m**4*p0+3.0*m**4*q0+12.0*m**4*r0+12.0*m**4+3.0*m**2*p0
     . **2*q0-6.0*m**2*p0**2-3.0*m**2*p0*q0**2-6.0*m**2*p0*q0*r0+3.0*m
     . **2*p0*qq+4.0*m**2*p0*qr-3.0*m**2*p0*r0**2+6.0*m**2*p0*r0-m**2*
     . p0*rr+9.0*m**2*p0-3.0*m**2*pp*q0+8.0*m**2*pp-2.0*m**2*pq*r0-8.0*
     . m**2*pq+4.0*m**2*pr*q0+4.0*m**2*pr*r0-10.0*m**2*pr+3.0*m**2*q0*
     . r0**2+18.0*m**2*q0*r0+m**2*q0*rr+15.0*m**2*q0-2.0*m**2*qq-4.0*m
     . **2*qr*r0-10.0*m**2*qr+12.0*m**2-6.0*p0**2*q0**2*r0-6.0*p0**2*q0
     . **2+2.0*p0**2*q0*qr-9.0*p0**2*q0*r0**2-6.0*p0**2*q0*r0+p0**2*q0*
     . rr+4.0*p0**2*qq*r0+10.0*p0**2*qq+8.0*p0**2*qr*r0+14.0*p0**2*qr+
     . 6.0*p0**2*r0**2-2.0*p0**2*rr-8.0*p0*pq*r0-12.0*p0*pq+2.0*p0*pr*
     . q0**2-2.0*p0*pr*qq-8.0*p0*pr+9.0*p0*q0**2*r0**2+18.0*p0*q0**2*r0
     . -p0*q0**2*rr+12.0*p0*q0**2+4.0*p0*q0*qr+12.0*p0*q0*r0**3+18.0*p0
     . *q0*r0**2+4.0*p0*q0*r0*rr+6.0*p0*q0*r0+6.0*p0*q0*rr-6.0*p0*q0-
     . 9.0*p0*qq*r0**2-22.0*p0*qq*r0+p0*qq*rr-16.0*p0*qq-16.0*p0*qr*r0
     . **2-24.0*p0*qr*r0-12.0*p0*qr-6.0*p0*r0**3-3.0*p0*r0**2+6.0*p0*r0
     . *rr+ans2
      mtra=2.0*m**2*ans1
      return
      end
c
      double precision function mtra100(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtra100=2.0*m**2*(-3.0*m**4+6.0*m**2*p0*q0-12.0*m**2*p0-3.0*m**2*
     . q0**2-6.0*m**2*q0*r0+3.0*m**2*qq+4.0*m**2*qr-3.0*m**2*r0**2+6.0*
     . m**2*r0-m**2*rr+9.0*m**2-12.0*p0*q0**2*r0-12.0*p0*q0**2+4.0*p0*
     . q0*qr-18.0*p0*q0*r0**2-12.0*p0*q0*r0+2.0*p0*q0*rr+8.0*p0*qq*r0+
     . 20.0*p0*qq+16.0*p0*qr*r0+28.0*p0*qr+12.0*p0*r0**2-4.0*p0*rr-8.0*
     . pq*r0-12.0*pq+2.0*pr*q0**2-2.0*pr*qq-8.0*pr+9.0*q0**2*r0**2+18.0
     . *q0**2*r0-q0**2*rr+12.0*q0**2+4.0*q0*qr+12.0*q0*r0**3+18.0*q0*r0
     . **2+4.0*q0*r0*rr+6.0*q0*r0+6.0*q0*rr-6.0*q0-9.0*qq*r0**2-22.0*qq
     . *r0+qq*rr-16.0*qq-16.0*qr*r0**2-24.0*qr*r0-12.0*qr-6.0*r0**3-3.0
     . *r0**2+6.0*r0*rr+7.0*rr)
      return
      end
c
      double precision function mtra010(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtra010=2.0*m**2*(3.0*m**4+3.0*m**2*p0**2-6.0*m**2*p0*q0-6.0*m**2
     . *p0*r0-3.0*m**2*pp+4.0*m**2*pr+3.0*m**2*r0**2+18.0*m**2*r0+m**2*
     . rr+15.0*m**2-12.0*p0**2*q0*r0-12.0*p0**2*q0+2.0*p0**2*qr-9.0*p0
     . **2*r0**2-6.0*p0**2*r0+p0**2*rr+4.0*p0*pr*q0+18.0*p0*q0*r0**2+
     . 36.0*p0*q0*r0-2.0*p0*q0*rr+24.0*p0*q0+4.0*p0*qr+12.0*p0*r0**3+
     . 18.0*p0*r0**2+4.0*p0*r0*rr+6.0*p0*r0+6.0*p0*rr-6.0*p0+8.0*pp*q0*
     . r0-4.0*pp*q0-2.0*pp*qr+9.0*pp*r0**2+6.0*pp*r0-pp*rr+12.0*pp-4.0*
     . pq*r0-12.0*pq-16.0*pr*q0*r0-4.0*pr*q0-16.0*pr*r0**2-24.0*pr*r0-
     . 28.0*pr+24.0*q0*r0**2+48.0*q0*r0+18.0*r0**3+39.0*r0**2+14.0*r0*
     . rr+12.0*r0+21.0*rr+12.0)
      return
      end
c
      double precision function mtra110(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtra110=4.0*m**2*(3.0*m**2*p0-3.0*m**2*q0-3.0*m**2*r0-12.0*p0*q0*
     . r0-12.0*p0*q0+2.0*p0*qr-9.0*p0*r0**2-6.0*p0*r0+p0*rr+2.0*pr*q0+
     . 9.0*q0*r0**2+18.0*q0*r0-q0*rr+12.0*q0+2.0*qr+6.0*r0**3+9.0*r0**2
     . +2.0*r0*rr+3.0*r0+3.0*rr-3.0)
      return
      end
c
      double precision function mtra101(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtra101=4.0*m**2*(-3.0*m**2*q0-3.0*m**2*r0+3.0*m**2-6.0*p0*q0**2-
     . 18.0*p0*q0*r0-6.0*p0*q0+4.0*p0*qq+8.0*p0*qr+12.0*p0*r0-4.0*pq+
     . 9.0*q0**2*r0+9.0*q0**2+18.0*q0*r0**2+18.0*q0*r0+2.0*q0*rr+3.0*q0
     . -9.0*qq*r0-11.0*qq-16.0*qr*r0-12.0*qr-9.0*r0**2-3.0*r0+3.0*rr)
      return
      end
c
      double precision function mtra001(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtra001=4.0*m**2*(6.0*m**4-3.0*m**2*p0*q0-3.0*m**2*p0*r0+3.0*m**2
     . *p0-m**2*pq+2.0*m**2*pr+3.0*m**2*q0*r0+9.0*m**2*q0-2.0*m**2*qr-
     . 3.0*p0**2*q0**2-9.0*p0**2*q0*r0-3.0*p0**2*q0+2.0*p0**2*qq+4.0*p0
     . **2*qr+6.0*p0**2*r0-4.0*p0*pq+9.0*p0*q0**2*r0+9.0*p0*q0**2+18.0*
     . p0*q0*r0**2+18.0*p0*q0*r0+2.0*p0*q0*rr+3.0*p0*q0-9.0*p0*qq*r0-
     . 11.0*p0*qq-16.0*p0*qr*r0-12.0*p0*qr-9.0*p0*r0**2-3.0*p0*r0+3.0*
     . p0*rr+2.0*pp*q0**2+9.0*pp*q0*r0+3.0*pp*q0-pp*qq-4.0*pp*qr-8.0*pp
     . *r0-2.0*pq*q0+6.0*pq*r0**2+14.0*pq*r0-2.0*pq*rr+13.0*pq-4.0*pr*
     . q0**2-16.0*pr*q0*r0-12.0*pr*q0+4.0*pr*qq+8.0*pr*qr+6.0*pr*r0+2.0
     . *pr+12.0*q0**2*r0+12.0*q0**2+27.0*q0*r0**2+39.0*q0*r0+7.0*q0*rr+
     . 6.0*q0-10.0*qq*r0-12.0*qq-26.0*qr*r0-34.0*qr-18.0*r0**2-24.0*r0-
     . 2.0*rr)
      return
      end
c
      double precision function mtra011(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtra011=4.0*m**2*(-3.0*m**2*p0+3.0*m**2*r0+9.0*m**2-6.0*p0**2*q0-
     . 9.0*p0**2*r0-3.0*p0**2+18.0*p0*q0*r0+18.0*p0*q0+18.0*p0*r0**2+
     . 18.0*p0*r0+2.0*p0*rr+3.0*p0+4.0*pp*q0+9.0*pp*r0+3.0*pp-2.0*pq-
     . 8.0*pr*q0-16.0*pr*r0-12.0*pr+24.0*q0*r0+24.0*q0+27.0*r0**2+39.0*
     . r0+7.0*rr+6.0)
      return
      end
c
      double precision function mtra111(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtra111=4.0*m**2*(-3.0*m**2-12.0*p0*q0-18.0*p0*r0-6.0*p0+18.0*q0*
     . r0+18.0*q0+18.0*r0**2+18.0*r0+2.0*rr+3.0)
      return
      end
c
c
      double precision function mtrb(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      ans3=-16.0*qq*r0**2-44.0*qq*r0+8.0*qq*rr-18.0*qq-20.0*qr*r0**2-
     . 52.0*qr*r0+4.0*qr*rr-24.0*qr-6.0*r0**3-18.0*r0**2-10.0*r0*rr-6.0
     . *rr
      ans2=10.0*p0*q0**2*rr+24.0*p0*q0**2+6.0*p0*q0*qq*r0+6.0*p0*q0*qq+
     . 12.0*p0*q0*r0**3+48.0*p0*q0*r0**2+20.0*p0*q0*r0*rr+24.0*p0*q0*r0
     . -8.0*p0*q0*rr+18.0*p0*q0+2.0*p0*qq*qr-14.0*p0*qq*r0**2-46.0*p0*
     . qq*r0-2.0*p0*qq*rr-36.0*p0*qq-28.0*p0*qr*r0**2-72.0*p0*qr*r0-4.0
     . *p0*qr*rr-46.0*p0*qr-6.0*p0*r0**3-21.0*p0*r0**2-26.0*p0*r0*rr-
     . 11.0*p0*rr-2.0*pp*q0**2-3.0*pp*q0*r0**2-14.0*pp*q0*r0-5.0*pp*q0*
     . rr-4.0*pp*q0+2.0*pp*qq+8.0*pp*qr*r0+12.0*pp*qr-4.0*pp*r0**2+8.0*
     . pp*rr-6.0*pq**2*r0-4.0*pq**2-2.0*pq*pr*q0+2.0*pq*pr+6.0*pq*q0**2
     . *r0+2.0*pq*q0**2+8.0*pq*q0*r0**2-4.0*pq*q0*r0-8.0*pq*q0*rr+12.0*
     . pq*q0-6.0*pq*qq*r0-2.0*pq*qq+8.0*pq*qr+8.0*pq*r0**3+2.0*pq*r0**2
     . -8.0*pq*r0*rr-8.0*pq*r0+6.0*pq*rr-18.0*pq+2.0*pr*q0**3-16.0*pr*
     . q0**2*r0-18.0*pr*q0**2-2.0*pr*q0*qq-24.0*pr*q0*r0**2-8.0*pr*q0*
     . r0-8.0*pr*q0*rr+14.0*pr*q0+16.0*pr*qq*r0+18.0*pr*qq+32.0*pr*qr*
     . r0+32.0*pr*qr+22.0*pr*r0**2+8.0*pr*r0+10.0*pr*rr-12.0*pr-4.0*q0
     . **2*qr+12.0*q0**2*r0**2+36.0*q0**2*r0-4.0*q0**2*rr+18.0*q0**2+
     . 8.0*q0*qr+12.0*q0*r0**3+30.0*q0*r0**2+4.0*q0*r0*rr+6.0*q0*rr+4.0
     . *qq*qr+ans3
      ans1=3.0*m**4*p0+6.0*m**4*q0-6.0*m**4*r0-6.0*m**4+3.0*m**2*p0**2*
     . q0-6.0*m**2*p0**2+6.0*m**2*p0*q0**2-12.0*m**2*p0*q0*r0-30.0*m**2
     . *p0*q0+2.0*m**2*p0*qq-2.0*m**2*p0*qr-9.0*m**2*p0*r0**2+6.0*m**2*
     . p0*r0+m**2*p0*rr+27.0*m**2*p0-3.0*m**2*pp*q0+4.0*m**2*pp-8.0*m**
     . 2*pq*q0+16.0*m**2*pq*r0+24.0*m**2*pq-2.0*m**2*pr*q0+8.0*m**2*pr*
     . r0+6.0*m**2*pr-12.0*m**2*q0**2*r0-18.0*m**2*q0**2-18.0*m**2*q0*
     . r0**2-12.0*m**2*q0*r0+6.0*m**2*q0*rr+6.0*m**2*q0+12.0*m**2*qq*r0
     . +22.0*m**2*qq+12.0*m**2*qr*r0+20.0*m**2*qr-6.0*m**2*r0**3+6.0*m
     . **2*r0**2+6.0*m**2*r0*rr+18.0*m**2*r0+2.0*m**2*rr+18.0*m**2-6.0*
     . p0**2*q0**2*r0-6.0*p0**2*q0**2-2.0*p0**2*q0*qr+3.0*p0**2*q0*r0**
     . 2+30.0*p0**2*q0*r0+5.0*p0**2*q0*rr+24.0*p0**2*q0-6.0*p0**2*qq-
     . 8.0*p0**2*qr*r0-14.0*p0**2*qr+6.0*p0**2*r0**2-10.0*p0**2*rr+12.0
     . *p0*pq*q0*r0+16.0*p0*pq*q0+2.0*p0*pq*qr-16.0*p0*pq*r0-20.0*p0*pq
     . +2.0*p0*pr*q0**2-8.0*p0*pr-6.0*p0*q0**3*r0-6.0*p0*q0**3-2.0*p0*
     . q0**2*qr+6.0*p0*q0**2*r0**2+42.0*p0*q0**2*r0+ans2
      mtrb=2.0*m**2*ans1
      return
      end
c
      double precision function mtrb100(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtrb100=2.0*m**2*(3.0*m**4+6.0*m**2*p0*q0-12.0*m**2*p0+6.0*m**2*
     . q0**2-12.0*m**2*q0*r0-30.0*m**2*q0+2.0*m**2*qq-2.0*m**2*qr-9.0*m
     . **2*r0**2+6.0*m**2*r0+m**2*rr+27.0*m**2-12.0*p0*q0**2*r0-12.0*p0
     . *q0**2-4.0*p0*q0*qr+6.0*p0*q0*r0**2+60.0*p0*q0*r0+10.0*p0*q0*rr+
     . 48.0*p0*q0-12.0*p0*qq-16.0*p0*qr*r0-28.0*p0*qr+12.0*p0*r0**2-
     . 20.0*p0*rr+12.0*pq*q0*r0+16.0*pq*q0+2.0*pq*qr-16.0*pq*r0-20.0*pq
     . +2.0*pr*q0**2-8.0*pr-6.0*q0**3*r0-6.0*q0**3-2.0*q0**2*qr+6.0*q0
     . **2*r0**2+42.0*q0**2*r0+10.0*q0**2*rr+24.0*q0**2+6.0*q0*qq*r0+
     . 6.0*q0*qq+12.0*q0*r0**3+48.0*q0*r0**2+20.0*q0*r0*rr+24.0*q0*r0-
     . 8.0*q0*rr+18.0*q0+2.0*qq*qr-14.0*qq*r0**2-46.0*qq*r0-2.0*qq*rr-
     . 36.0*qq-28.0*qr*r0**2-72.0*qr*r0-4.0*qr*rr-46.0*qr-6.0*r0**3-
     . 21.0*r0**2-26.0*r0*rr-11.0*rr)
      return
      end
c
      double precision function mtrb010(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtrb010=2.0*m**2*(6.0*m**4+3.0*m**2*p0**2+12.0*m**2*p0*q0-12.0*m
     . **2*p0*r0-30.0*m**2*p0-3.0*m**2*pp-8.0*m**2*pq-2.0*m**2*pr-24.0*
     . m**2*q0*r0-36.0*m**2*q0-18.0*m**2*r0**2-12.0*m**2*r0+6.0*m**2*rr
     . +6.0*m**2-12.0*p0**2*q0*r0-12.0*p0**2*q0-2.0*p0**2*qr+3.0*p0**2*
     . r0**2+30.0*p0**2*r0+5.0*p0**2*rr+24.0*p0**2+12.0*p0*pq*r0+16.0*
     . p0*pq+4.0*p0*pr*q0-18.0*p0*q0**2*r0-18.0*p0*q0**2-4.0*p0*q0*qr+
     . 12.0*p0*q0*r0**2+84.0*p0*q0*r0+20.0*p0*q0*rr+48.0*p0*q0+6.0*p0*
     . qq*r0+6.0*p0*qq+12.0*p0*r0**3+48.0*p0*r0**2+20.0*p0*r0*rr+24.0*
     . p0*r0-8.0*p0*rr+18.0*p0-4.0*pp*q0-3.0*pp*r0**2-14.0*pp*r0-5.0*pp
     . *rr-4.0*pp-2.0*pq*pr+12.0*pq*q0*r0+4.0*pq*q0+8.0*pq*r0**2-4.0*pq
     . *r0-8.0*pq*rr+12.0*pq+6.0*pr*q0**2-32.0*pr*q0*r0-36.0*pr*q0-2.0*
     . pr*qq-24.0*pr*r0**2-8.0*pr*r0-8.0*pr*rr+14.0*pr-8.0*q0*qr+24.0*
     . q0*r0**2+72.0*q0*r0-8.0*q0*rr+36.0*q0+8.0*qr+12.0*r0**3+30.0*r0
     . **2+4.0*r0*rr+6.0*rr)
      return
      end
c
      double precision function mtrb110(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtrb110=4.0*m**2*(3.0*m**2*p0+6.0*m**2*q0-6.0*m**2*r0-15.0*m**2-
     . 12.0*p0*q0*r0-12.0*p0*q0-2.0*p0*qr+3.0*p0*r0**2+30.0*p0*r0+5.0*
     . p0*rr+24.0*p0+6.0*pq*r0+8.0*pq+2.0*pr*q0-9.0*q0**2*r0-9.0*q0**2-
     . 2.0*q0*qr+6.0*q0*r0**2+42.0*q0*r0+10.0*q0*rr+24.0*q0+3.0*qq*r0+
     . 3.0*qq+6.0*r0**3+24.0*r0**2+10.0*r0*rr+12.0*r0-4.0*rr+9.0)
      return
      end
c
      double precision function mtrb101(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtrb101=4.0*m**2*(-6.0*m**2*q0-9.0*m**2*r0+3.0*m**2-6.0*p0*q0**2+
     . 6.0*p0*q0*r0+30.0*p0*q0-8.0*p0*qr+12.0*p0*r0+6.0*pq*q0-8.0*pq-
     . 3.0*q0**3+6.0*q0**2*r0+21.0*q0**2+3.0*q0*qq+18.0*q0*r0**2+48.0*
     . q0*r0+10.0*q0*rr+12.0*q0-14.0*qq*r0-23.0*qq-28.0*qr*r0-36.0*qr-
     . 9.0*r0**2-21.0*r0-13.0*rr)
      return
      end
c
      double precision function mtrb001(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtrb001=4.0*m**2*(-3.0*m**4-6.0*m**2*p0*q0-9.0*m**2*p0*r0+3.0*m**
     . 2*p0+8.0*m**2*pq+4.0*m**2*pr-6.0*m**2*q0**2-18.0*m**2*q0*r0-6.0*
     . m**2*q0+6.0*m**2*qq+6.0*m**2*qr-9.0*m**2*r0**2+6.0*m**2*r0+3.0*m
     . **2*rr+9.0*m**2-3.0*p0**2*q0**2+3.0*p0**2*q0*r0+15.0*p0**2*q0-
     . 4.0*p0**2*qr+6.0*p0**2*r0+6.0*p0*pq*q0-8.0*p0*pq-3.0*p0*q0**3+
     . 6.0*p0*q0**2*r0+21.0*p0*q0**2+3.0*p0*q0*qq+18.0*p0*q0*r0**2+48.0
     . *p0*q0*r0+10.0*p0*q0*rr+12.0*p0*q0-14.0*p0*qq*r0-23.0*p0*qq-28.0
     . *p0*qr*r0-36.0*p0*qr-9.0*p0*r0**2-21.0*p0*r0-13.0*p0*rr-3.0*pp*
     . q0*r0-7.0*pp*q0+4.0*pp*qr-4.0*pp*r0-3.0*pq**2+3.0*pq*q0**2+8.0*
     . pq*q0*r0-2.0*pq*q0-3.0*pq*qq+12.0*pq*r0**2+2.0*pq*r0-4.0*pq*rr-
     . 4.0*pq-8.0*pr*q0**2-24.0*pr*q0*r0-4.0*pr*q0+8.0*pr*qq+16.0*pr*qr
     . +22.0*pr*r0+4.0*pr+12.0*q0**2*r0+18.0*q0**2+18.0*q0*r0**2+30.0*
     . q0*r0+2.0*q0*rr-16.0*qq*r0-22.0*qq-20.0*qr*r0-26.0*qr-9.0*r0**2-
     . 18.0*r0-5.0*rr)
      return
      end
c
      double precision function mtrb011(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtrb011=4.0*m**2*(-6.0*m**2*p0-12.0*m**2*q0-18.0*m**2*r0-6.0*m**2
     . -6.0*p0**2*q0+3.0*p0**2*r0+15.0*p0**2+6.0*p0*pq-9.0*p0*q0**2+
     . 12.0*p0*q0*r0+42.0*p0*q0+3.0*p0*qq+18.0*p0*r0**2+48.0*p0*r0+10.0
     . *p0*rr+12.0*p0-3.0*pp*r0-7.0*pp+6.0*pq*q0+8.0*pq*r0-2.0*pq-16.0*
     . pr*q0-24.0*pr*r0-4.0*pr+24.0*q0*r0+36.0*q0+18.0*r0**2+30.0*r0+
     . 2.0*rr)
      return
      end
c
      double precision function mtrb111(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtrb111=4.0*m**2*(-6.0*m**2-12.0*p0*q0+6.0*p0*r0+30.0*p0+6.0*pq-
     . 9.0*q0**2+12.0*q0*r0+42.0*q0+3.0*qq+18.0*r0**2+48.0*r0+10.0*rr+
     . 12.0)
      return
      end
c
c
      double precision function mtrc(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      ans3=27.0*q0*r0**2+14.0*q0*r0*rr-24.0*q0*r0+5.0*q0*rr-10.0*qq*r0
     . **2-16.0*qq*r0-2.0*qq*rr-26.0*qr*r0**2-40.0*qr*r0-6.0*qr*rr-4.0*
     . qr-18.0*r0**3-42.0*r0**2-30.0*r0*rr-14.0*rr
      ans2=-12.0*p0*pp+12.0*p0*pq*q0*r0+16.0*p0*pq*q0-2.0*p0*pq*qr+8.0*
     . p0*pq*r0**2-16.0*p0*pq*r0-8.0*p0*pq*rr-12.0*p0*pq-2.0*p0*pr*q0**
     . 2+4.0*p0*pr*q0+3.0*p0*q0**2*r0**2+18.0*p0*q0**2*r0+5.0*p0*q0**2*
     . rr+12.0*p0*q0**2+4.0*p0*q0*qr+12.0*p0*q0*r0**3+54.0*p0*q0*r0**2+
     . 20.0*p0*q0*r0*rr+36.0*p0*q0*r0+2.0*p0*q0*rr-6.0*p0*q0-3.0*p0*qq*
     . r0**2-14.0*p0*qq*r0-5.0*p0*qq*rr-8.0*p0*qq-24.0*p0*qr*r0**2-56.0
     . *p0*qr*r0-8.0*p0*qr*rr-30.0*p0*qr-12.0*p0*r0**3-54.0*p0*r0**2-
     . 36.0*p0*r0*rr-48.0*p0*r0-30.0*p0*rr-6.0*pp*pq*r0-2.0*pp*pq+2.0*
     . pp*pr*q0-6.0*pp*q0**2-14.0*pp*q0*r0**2-34.0*pp*q0*r0-2.0*pp*q0*
     . rr-12.0*pp*q0+2.0*pp*qq+16.0*pp*qr*r0+14.0*pp*qr+12.0*pp*r0**2+
     . 24.0*pp*r0+12.0*pp*rr+6.0*pp-6.0*pq**2*r0-4.0*pq**2+2.0*pq*pr*q0
     . +4.0*pq*pr-4.0*pq*q0*r0-4.0*pq*q0+2.0*pq*qr+8.0*pq*r0**3+4.0*pq*
     . r0**2-8.0*pq*r0*rr-4.0*pq*r0-4.0*pq*rr+6.0*pq-8.0*pr*q0**2*r0-
     . 14.0*pr*q0**2-28.0*pr*q0*r0**2-24.0*pr*q0*r0-4.0*pr*q0*rr+14.0*
     . pr*q0+8.0*pr*qq*r0+8.0*pr*qq+32.0*pr*qr*r0+24.0*pr*qr+36.0*pr*r0
     . **2+36.0*pr*r0+12.0*pr*rr-8.0*pr+12.0*q0**2*r0**2+24.0*q0**2*r0+
     . 18.0*q0*r0**3+ans3
      ans1=6.0*m**4*p0+3.0*m**4*q0-6.0*m**4*r0-6.0*m**4+6.0*m**2*p0**2*
     . q0-12.0*m**2*p0**2*r0-30.0*m**2*p0**2-8.0*m**2*p0*pq+3.0*m**2*p0
     . *q0**2-12.0*m**2*p0*q0*r0-24.0*m**2*p0*q0-3.0*m**2*p0*qq-2.0*m**
     . 2*p0*qr-18.0*m**2*p0*r0**2-12.0*m**2*p0*r0+6.0*m**2*p0*rr-6.0*m
     . **2*p0+2.0*m**2*pp*q0+12.0*m**2*pp*r0+18.0*m**2*pp+16.0*m**2*pq*
     . r0+10.0*m**2*pq-2.0*m**2*pr*q0+12.0*m**2*pr*r0+20.0*m**2*pr-9.0*
     . m**2*q0*r0**2+6.0*m**2*q0*r0+m**2*q0*rr+3.0*m**2*q0-2.0*m**2*qq+
     . 8.0*m**2*qr*r0-2.0*m**2*qr-6.0*m**2*r0**3+6.0*m**2*r0**2+6.0*m**
     . 2*r0*rr-18.0*m**2*r0+2.0*m**2*rr-6.0*m**2-6.0*p0**3*q0*r0-6.0*p0
     . **3*q0+2.0*p0**3*qr+12.0*p0**3*r0+12.0*p0**3+6.0*p0**2*pq*r0+2.0
     . *p0**2*pq-2.0*p0**2*pr*q0-6.0*p0**2*q0**2*r0-6.0*p0**2*q0**2+2.0
     . *p0**2*q0*qr+6.0*p0**2*q0*r0**2+42.0*p0**2*q0*r0+10.0*p0**2*q0*
     . rr+24.0*p0**2*q0-2.0*p0**2*qq-16.0*p0**2*qr*r0-14.0*p0**2*qr-
     . 24.0*p0**2*r0-24.0*p0**2*rr-6.0*p0**2+6.0*p0*pp*q0*r0+6.0*p0*pp*
     . q0-2.0*p0*pp*qr-12.0*p0*pp*r0+ans2
      mtrc=2.0*m**2*ans1
      return
      end
c
      double precision function mtrc100(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtrc100=2.0*m**2*(6.0*m**4+12.0*m**2*p0*q0-24.0*m**2*p0*r0-60.0*m
     . **2*p0-8.0*m**2*pq+3.0*m**2*q0**2-12.0*m**2*q0*r0-24.0*m**2*q0-
     . 3.0*m**2*qq-2.0*m**2*qr-18.0*m**2*r0**2-12.0*m**2*r0+6.0*m**2*rr
     . -6.0*m**2-18.0*p0**2*q0*r0-18.0*p0**2*q0+6.0*p0**2*qr+36.0*p0**2
     . *r0+36.0*p0**2+12.0*p0*pq*r0+4.0*p0*pq-4.0*p0*pr*q0-12.0*p0*q0**
     . 2*r0-12.0*p0*q0**2+4.0*p0*q0*qr+12.0*p0*q0*r0**2+84.0*p0*q0*r0+
     . 20.0*p0*q0*rr+48.0*p0*q0-4.0*p0*qq-32.0*p0*qr*r0-28.0*p0*qr-48.0
     . *p0*r0-48.0*p0*rr-12.0*p0+6.0*pp*q0*r0+6.0*pp*q0-2.0*pp*qr-12.0*
     . pp*r0-12.0*pp+12.0*pq*q0*r0+16.0*pq*q0-2.0*pq*qr+8.0*pq*r0**2-
     . 16.0*pq*r0-8.0*pq*rr-12.0*pq-2.0*pr*q0**2+4.0*pr*q0+3.0*q0**2*r0
     . **2+18.0*q0**2*r0+5.0*q0**2*rr+12.0*q0**2+4.0*q0*qr+12.0*q0*r0**
     . 3+54.0*q0*r0**2+20.0*q0*r0*rr+36.0*q0*r0+2.0*q0*rr-6.0*q0-3.0*qq
     . *r0**2-14.0*qq*r0-5.0*qq*rr-8.0*qq-24.0*qr*r0**2-56.0*qr*r0-8.0*
     . qr*rr-30.0*qr-12.0*r0**3-54.0*r0**2-36.0*r0*rr-48.0*r0-30.0*rr)
      return
      end
c
      double precision function mtrc010(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtrc010=2.0*m**2*(3.0*m**4+6.0*m**2*p0**2+6.0*m**2*p0*q0-12.0*m**
     . 2*p0*r0-24.0*m**2*p0+2.0*m**2*pp-2.0*m**2*pr-9.0*m**2*r0**2+6.0*
     . m**2*r0+m**2*rr+3.0*m**2-6.0*p0**3*r0-6.0*p0**3-2.0*p0**2*pr-
     . 12.0*p0**2*q0*r0-12.0*p0**2*q0+2.0*p0**2*qr+6.0*p0**2*r0**2+42.0
     . *p0**2*r0+10.0*p0**2*rr+24.0*p0**2+6.0*p0*pp*r0+6.0*p0*pp+12.0*
     . p0*pq*r0+16.0*p0*pq-4.0*p0*pr*q0+4.0*p0*pr+6.0*p0*q0*r0**2+36.0*
     . p0*q0*r0+10.0*p0*q0*rr+24.0*p0*q0+4.0*p0*qr+12.0*p0*r0**3+54.0*
     . p0*r0**2+20.0*p0*r0*rr+36.0*p0*r0+2.0*p0*rr-6.0*p0+2.0*pp*pr-
     . 12.0*pp*q0-14.0*pp*r0**2-34.0*pp*r0-2.0*pp*rr-12.0*pp+2.0*pq*pr-
     . 4.0*pq*r0-4.0*pq-16.0*pr*q0*r0-28.0*pr*q0-28.0*pr*r0**2-24.0*pr*
     . r0-4.0*pr*rr+14.0*pr+24.0*q0*r0**2+48.0*q0*r0+18.0*r0**3+27.0*r0
     . **2+14.0*r0*rr-24.0*r0+5.0*rr)
      return
      end
c
      double precision function mtrc110(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtrc110=4.0*m**2*(6.0*m**2*p0+3.0*m**2*q0-6.0*m**2*r0-12.0*m**2-
     . 9.0*p0**2*r0-9.0*p0**2-2.0*p0*pr-12.0*p0*q0*r0-12.0*p0*q0+2.0*p0
     . *qr+6.0*p0*r0**2+42.0*p0*r0+10.0*p0*rr+24.0*p0+3.0*pp*r0+3.0*pp+
     . 6.0*pq*r0+8.0*pq-2.0*pr*q0+2.0*pr+3.0*q0*r0**2+18.0*q0*r0+5.0*q0
     . *rr+12.0*q0+2.0*qr+6.0*r0**3+27.0*r0**2+10.0*r0*rr+18.0*r0+rr-
     . 3.0)
      return
      end
c
      double precision function mtrc101(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtrc101=4.0*m**2*(-12.0*m**2*p0-6.0*m**2*q0-18.0*m**2*r0-6.0*m**2
     . -9.0*p0**2*q0+18.0*p0**2+6.0*p0*pq-6.0*p0*q0**2+12.0*p0*q0*r0+
     . 42.0*p0*q0-16.0*p0*qr-24.0*p0+3.0*pp*q0-6.0*pp+6.0*pq*q0+8.0*pq*
     . r0-8.0*pq+3.0*q0**2*r0+9.0*q0**2+18.0*q0*r0**2+54.0*q0*r0+10.0*
     . q0*rr+18.0*q0-3.0*qq*r0-7.0*qq-24.0*qr*r0-28.0*qr-18.0*r0**2-
     . 54.0*r0-18.0*rr-24.0)
      return
      end
c
      double precision function mtrc001(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtrc001=4.0*m**2*(-3.0*m**4-6.0*m**2*p0**2-6.0*m**2*p0*q0-18.0*m
     . **2*p0*r0-6.0*m**2*p0+6.0*m**2*pp+8.0*m**2*pq+6.0*m**2*pr-9.0*m
     . **2*q0*r0+3.0*m**2*q0+4.0*m**2*qr-9.0*m**2*r0**2+6.0*m**2*r0+3.0
     . *m**2*rr-9.0*m**2-3.0*p0**3*q0+6.0*p0**3+3.0*p0**2*pq-3.0*p0**2*
     . q0**2+6.0*p0**2*q0*r0+21.0*p0**2*q0-8.0*p0**2*qr-12.0*p0**2+3.0*
     . p0*pp*q0-6.0*p0*pp+6.0*p0*pq*q0+8.0*p0*pq*r0-8.0*p0*pq+3.0*p0*q0
     . **2*r0+9.0*p0*q0**2+18.0*p0*q0*r0**2+54.0*p0*q0*r0+10.0*p0*q0*rr
     . +18.0*p0*q0-3.0*p0*qq*r0-7.0*p0*qq-24.0*p0*qr*r0-28.0*p0*qr-18.0
     . *p0*r0**2-54.0*p0*r0-18.0*p0*rr-24.0*p0-3.0*pp*pq-14.0*pp*q0*r0-
     . 17.0*pp*q0+8.0*pp*qr+12.0*pp*r0+12.0*pp-3.0*pq**2-2.0*pq*q0+12.0
     . *pq*r0**2+4.0*pq*r0-4.0*pq*rr-2.0*pq-4.0*pr*q0**2-28.0*pr*q0*r0-
     . 12.0*pr*q0+4.0*pr*qq+16.0*pr*qr+36.0*pr*r0+18.0*pr+12.0*q0**2*r0
     . +12.0*q0**2+27.0*q0*r0**2+27.0*q0*r0+7.0*q0*rr-12.0*q0-10.0*qq*
     . r0-8.0*qq-26.0*qr*r0-20.0*qr-27.0*r0**2-42.0*r0-15.0*rr)
      return
      end
c
      double precision function mtrc011(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtrc011=4.0*m**2*(-6.0*m**2*p0-9.0*m**2*r0+3.0*m**2-3.0*p0**3-6.0
     . *p0**2*q0+6.0*p0**2*r0+21.0*p0**2+3.0*p0*pp+6.0*p0*pq+6.0*p0*q0*
     . r0+18.0*p0*q0+18.0*p0*r0**2+54.0*p0*r0+10.0*p0*rr+18.0*p0-14.0*
     . pp*r0-17.0*pp-2.0*pq-8.0*pr*q0-28.0*pr*r0-12.0*pr+24.0*q0*r0+
     . 24.0*q0+27.0*r0**2+27.0*r0+7.0*rr-12.0)
      return
      end
c
      double precision function mtrc111(p0,q0,r0,pp,qq,rr,pq,pr,qr,m)
      implicit double precision (a-z)
      mtrc111=4.0*m**2*(-6.0*m**2-9.0*p0**2-12.0*p0*q0+12.0*p0*r0+42.0*
     . p0+3.0*pp+6.0*pq+6.0*q0*r0+18.0*q0+18.0*r0**2+54.0*r0+10.0*rr+
     . 18.0)
      return
      end
c
c
c
c
