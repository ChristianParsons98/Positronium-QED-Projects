      double precision function tra(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ans3=-6.0*pr*q0**2*r0+pr*q0**2*rr+12.0*pr*q0**2+4.0*pr*q0*qr*r0-
     . 4.0*pr*q0*qr+16.0*pr*q0*r0**3+26.0*pr*q0*r0**2-12.0*pr*q0*r0-2.0
     . *pr*q0*rr-16.0*pr*q0+11.0*pr*qq*r0**2+6.0*pr*qq*r0-3.0*pr*qq*rr-
     . 8.0*pr*qq-20.0*pr*qr*r0**2+4.0*pr*qr*rr+16.0*pr*qr-8.0*pr*r0**3-
     . 10.0*pr*r0**2-8.0*pr*r0*rr-4.0*pr*r0+2.0*pr*rr+8.0*pr-6.0*q0**2*
     . r0**3+6.0*q0**2*r0**2-2.0*q0**2*r0*rr+12.0*q0**2*r0-10.0*q0**2*
     . rr-24.0*q0**2-6.0*q0*qr*r0**2+4.0*q0*qr*r0-2.0*q0*qr*rr+6.0*q0*
     . r0**4+6.0*q0*r0**3+8.0*q0*r0**2*rr-24.0*q0*r0**2+10.0*q0*r0*rr+
     . 2.0*q0*rr**2+20.0*q0*rr+48.0*q0+8.0*qq*r0**3-8.0*qq*r0**2-16.0*
     . qq*r0+16.0*qq*rr+32.0*qq+8.0*qr**2*r0-8.0*qr**2-8.0*qr*r0**3+
     . 10.0*qr*r0**2-8.0*qr*r0*rr-4.0*qr*r0-2.0*qr*rr-8.0*qr-6.0*r0**4-
     . 4.0*r0**2*rr+24.0*r0**2-6.0*rr**2-32.0*rr-48.0
      ans2=19.0*p0*qq*r0**2+3.0*p0*qq*r0*rr+22.0*p0*qq*r0-9.0*p0*qq*rr-
     . 32.0*p0*qq-4.0*p0*qr**2*r0+4.0*p0*qr**2+16.0*p0*qr*r0**3-26.0*p0
     . *qr*r0**2-12.0*p0*qr*r0+2.0*p0*qr*rr+16.0*p0*qr+6.0*p0*r0**4-6.0
     . *p0*r0**3+8.0*p0*r0**2*rr-24.0*p0*r0**2-10.0*p0*r0*rr+2.0*p0*rr
     . **2+20.0*p0*rr+48.0*p0-11.0*pp*q0**2*r0**2+5.0*pp*q0**2*rr+22.0*
     . pp*q0**2-4.0*pp*q0*qr*r0+11.0*pp*q0*r0**3+19.0*pp*q0*r0**2-3.0*
     . pp*q0*r0*rr-22.0*pp*q0*r0-9.0*pp*q0*rr-32.0*pp*q0+13.0*pp*qq*r0
     . **2-7.0*pp*qq*rr-26.0*pp*qq+4.0*pp*qr**2-11.0*pp*qr*r0**2+6.0*pp
     . *qr*r0+3.0*pp*qr*rr+8.0*pp*qr-8.0*pp*r0**3-8.0*pp*r0**2+16.0*pp*
     . r0+16.0*pp*rr+32.0*pp+2.0*pq**2*r0**2+2.0*pq**2*rr-4.0*pq**2+4.0
     . *pq*pr*q0*r0+8.0*pq*pr*q0-8.0*pq*pr*qr-6.0*pq*pr*r0**2-4.0*pq*pr
     . *r0+2.0*pq*pr*rr+8.0*pq*pr-4.0*pq*q0*r0**3+2.0*pq*q0*r0**2+8.0*
     . pq*q0*r0+6.0*pq*q0*rr+8.0*pq*q0+6.0*pq*qr*r0**2-4.0*pq*qr*r0-2.0
     . *pq*qr*rr-8.0*pq*qr+pq*r0**4-2.0*pq*r0**2*rr-12.0*pq*r0**2+pq*rr
     . **2+16.0*pq-4.0*pr**2*q0**2+4.0*pr**2*q0*r0+4.0*pr**2*q0+4.0*pr
     . **2*qq-8.0*pr**2*r0-8.0*pr**2-13.0*pr*q0**2*r0**2+ans3
      ans1=15.0*p0**2*q0**2*r0**2-5.0*p0**2*q0**2*rr-30.0*p0**2*q0**2+
     . 8.0*p0**2*q0*qr-15.0*p0**2*q0*r0**3-21.0*p0**2*q0*r0**2+3.0*p0**
     . 2*q0*r0*rr+30.0*p0**2*q0*r0+3.0*p0**2*q0*rr+24.0*p0**2*q0-11.0*
     . p0**2*qq*r0**2+5.0*p0**2*qq*rr+22.0*p0**2*qq-4.0*p0**2*qr**2+
     . 13.0*p0**2*qr*r0**2-6.0*p0**2*qr*r0-p0**2*qr*rr-12.0*p0**2*qr+
     . 6.0*p0**2*r0**3+6.0*p0**2*r0**2+2.0*p0**2*r0*rr-12.0*p0**2*r0-
     . 10.0*p0**2*rr-24.0*p0**2-8.0*p0*pq*q0*r0**2+16.0*p0*pq*q0+4.0*p0
     . *pq*qr*r0-8.0*p0*pq*qr+4.0*p0*pq*r0**3+2.0*p0*pq*r0**2-8.0*p0*pq
     . *r0+6.0*p0*pq*rr+8.0*p0*pq-8.0*p0*pr*q0**2+8.0*p0*pr*q0*qr+4.0*
     . p0*pr*q0*r0**2+4.0*p0*pr*q0*r0-4.0*p0*pr*q0*rr-4.0*p0*pr*q0-4.0*
     . p0*pr*qq*r0-4.0*p0*pr*qr*r0-4.0*p0*pr*qr+6.0*p0*pr*r0**2+4.0*p0*
     . pr*r0+2.0*p0*pr*rr+15.0*p0*q0**2*r0**3-21.0*p0*q0**2*r0**2-3.0*
     . p0*q0**2*r0*rr-30.0*p0*q0**2*r0+3.0*p0*q0**2*rr+24.0*p0*q0**2-
     . 4.0*p0*q0*qr*r0**2+4.0*p0*q0*qr*r0+4.0*p0*q0*qr*rr+4.0*p0*q0*qr-
     . 15.0*p0*q0*r0**4+2.0*p0*q0*r0**2*rr+72.0*p0*q0*r0**2-3.0*p0*q0*
     . rr**2-12.0*p0*q0*rr-48.0*p0*q0-11.0*p0*qq*r0**3+ans2
      tra=4.0*ans1
      return
      end
c
      double precision function tra100(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      tra100=4.0*(30.0*p0*q0**2*r0**2-10.0*p0*q0**2*rr-60.0*p0*q0**2+
     . 16.0*p0*q0*qr-30.0*p0*q0*r0**3-42.0*p0*q0*r0**2+6.0*p0*q0*r0*rr+
     . 60.0*p0*q0*r0+6.0*p0*q0*rr+48.0*p0*q0-22.0*p0*qq*r0**2+10.0*p0*
     . qq*rr+44.0*p0*qq-8.0*p0*qr**2+26.0*p0*qr*r0**2-12.0*p0*qr*r0-2.0
     . *p0*qr*rr-24.0*p0*qr+12.0*p0*r0**3+12.0*p0*r0**2+4.0*p0*r0*rr-
     . 24.0*p0*r0-20.0*p0*rr-48.0*p0-8.0*pq*q0*r0**2+16.0*pq*q0+4.0*pq*
     . qr*r0-8.0*pq*qr+4.0*pq*r0**3+2.0*pq*r0**2-8.0*pq*r0+6.0*pq*rr+
     . 8.0*pq-8.0*pr*q0**2+8.0*pr*q0*qr+4.0*pr*q0*r0**2+4.0*pr*q0*r0-
     . 4.0*pr*q0*rr-4.0*pr*q0-4.0*pr*qq*r0-4.0*pr*qr*r0-4.0*pr*qr+6.0*
     . pr*r0**2+4.0*pr*r0+2.0*pr*rr+15.0*q0**2*r0**3-21.0*q0**2*r0**2-
     . 3.0*q0**2*r0*rr-30.0*q0**2*r0+3.0*q0**2*rr+24.0*q0**2-4.0*q0*qr*
     . r0**2+4.0*q0*qr*r0+4.0*q0*qr*rr+4.0*q0*qr-15.0*q0*r0**4+2.0*q0*
     . r0**2*rr+72.0*q0*r0**2-3.0*q0*rr**2-12.0*q0*rr-48.0*q0-11.0*qq*
     . r0**3+19.0*qq*r0**2+3.0*qq*r0*rr+22.0*qq*r0-9.0*qq*rr-32.0*qq-
     . 4.0*qr**2*r0+4.0*qr**2+16.0*qr*r0**3-26.0*qr*r0**2-12.0*qr*r0+
     . 2.0*qr*rr+16.0*qr+6.0*r0**4-6.0*r0**3+8.0*r0**2*rr-24.0*r0**2-
     . 10.0*r0*rr+2.0*rr**2+20.0*rr+48.0)
      return
      end
c
      double precision function tra010(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      tra010=4.0*(30.0*p0**2*q0*r0**2-10.0*p0**2*q0*rr-60.0*p0**2*q0+
     . 8.0*p0**2*qr-15.0*p0**2*r0**3-21.0*p0**2*r0**2+3.0*p0**2*r0*rr+
     . 30.0*p0**2*r0+3.0*p0**2*rr+24.0*p0**2-8.0*p0*pq*r0**2+16.0*p0*pq
     . -16.0*p0*pr*q0+8.0*p0*pr*qr+4.0*p0*pr*r0**2+4.0*p0*pr*r0-4.0*p0*
     . pr*rr-4.0*p0*pr+30.0*p0*q0*r0**3-42.0*p0*q0*r0**2-6.0*p0*q0*r0*
     . rr-60.0*p0*q0*r0+6.0*p0*q0*rr+48.0*p0*q0-4.0*p0*qr*r0**2+4.0*p0*
     . qr*r0+4.0*p0*qr*rr+4.0*p0*qr-15.0*p0*r0**4+2.0*p0*r0**2*rr+72.0*
     . p0*r0**2-3.0*p0*rr**2-12.0*p0*rr-48.0*p0-22.0*pp*q0*r0**2+10.0*
     . pp*q0*rr+44.0*pp*q0-4.0*pp*qr*r0+11.0*pp*r0**3+19.0*pp*r0**2-3.0
     . *pp*r0*rr-22.0*pp*r0-9.0*pp*rr-32.0*pp+4.0*pq*pr*r0+8.0*pq*pr-
     . 4.0*pq*r0**3+2.0*pq*r0**2+8.0*pq*r0+6.0*pq*rr+8.0*pq-8.0*pr**2*
     . q0+4.0*pr**2*r0+4.0*pr**2-26.0*pr*q0*r0**2-12.0*pr*q0*r0+2.0*pr*
     . q0*rr+24.0*pr*q0+4.0*pr*qr*r0-4.0*pr*qr+16.0*pr*r0**3+26.0*pr*r0
     . **2-12.0*pr*r0-2.0*pr*rr-16.0*pr-12.0*q0*r0**3+12.0*q0*r0**2-4.0
     . *q0*r0*rr+24.0*q0*r0-20.0*q0*rr-48.0*q0-6.0*qr*r0**2+4.0*qr*r0-
     . 2.0*qr*rr+6.0*r0**4+6.0*r0**3+8.0*r0**2*rr-24.0*r0**2+10.0*r0*rr
     . +2.0*rr**2+20.0*rr+48.0)
      return
      end
c
      double precision function tra110(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      tra110=4.0*(60.0*p0*q0*r0**2-20.0*p0*q0*rr-120.0*p0*q0+16.0*p0*qr
     . -30.0*p0*r0**3-42.0*p0*r0**2+6.0*p0*r0*rr+60.0*p0*r0+6.0*p0*rr+
     . 48.0*p0-8.0*pq*r0**2+16.0*pq-16.0*pr*q0+8.0*pr*qr+4.0*pr*r0**2+
     . 4.0*pr*r0-4.0*pr*rr-4.0*pr+30.0*q0*r0**3-42.0*q0*r0**2-6.0*q0*r0
     . *rr-60.0*q0*r0+6.0*q0*rr+48.0*q0-4.0*qr*r0**2+4.0*qr*r0+4.0*qr*
     . rr+4.0*qr-15.0*r0**4+2.0*r0**2*rr+72.0*r0**2-3.0*rr**2-12.0*rr-
     . 48.0)
      return
      end
c
      double precision function tra101(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      tra101=4.0*(60.0*p0*q0**2*r0-90.0*p0*q0*r0**2-84.0*p0*q0*r0+6.0*
     . p0*q0*rr+60.0*p0*q0-44.0*p0*qq*r0+52.0*p0*qr*r0-12.0*p0*qr+36.0*
     . p0*r0**2+24.0*p0*r0+4.0*p0*rr-24.0*p0-16.0*pq*q0*r0+4.0*pq*qr+
     . 12.0*pq*r0**2+4.0*pq*r0-8.0*pq+8.0*pr*q0*r0+4.0*pr*q0-4.0*pr*qq-
     . 4.0*pr*qr+12.0*pr*r0+4.0*pr+45.0*q0**2*r0**2-42.0*q0**2*r0-3.0*
     . q0**2*rr-30.0*q0**2-8.0*q0*qr*r0+4.0*q0*qr-60.0*q0*r0**3+4.0*q0*
     . r0*rr+144.0*q0*r0-33.0*qq*r0**2+38.0*qq*r0+3.0*qq*rr+22.0*qq-4.0
     . *qr**2+48.0*qr*r0**2-52.0*qr*r0-12.0*qr+24.0*r0**3-18.0*r0**2+
     . 16.0*r0*rr-48.0*r0-10.0*rr)
      return
      end
c
      double precision function tra001(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ans2=4.0*pr*q0*qr+48.0*pr*q0*r0**2+52.0*pr*q0*r0-12.0*pr*q0+22.0*
     . pr*qq*r0+6.0*pr*qq-40.0*pr*qr*r0-24.0*pr*r0**2-20.0*pr*r0-8.0*pr
     . *rr-4.0*pr-18.0*q0**2*r0**2+12.0*q0**2*r0-2.0*q0**2*rr+12.0*q0**
     . 2-12.0*q0*qr*r0+4.0*q0*qr+24.0*q0*r0**3+18.0*q0*r0**2+16.0*q0*r0
     . *rr-48.0*q0*r0+10.0*q0*rr+24.0*qq*r0**2-16.0*qq*r0-16.0*qq+8.0*
     . qr**2-24.0*qr*r0**2+20.0*qr*r0-8.0*qr*rr-4.0*qr-24.0*r0**3-8.0*
     . r0*rr+48.0*r0
      ans1=30.0*p0**2*q0**2*r0-45.0*p0**2*q0*r0**2-42.0*p0**2*q0*r0+3.0
     . *p0**2*q0*rr+30.0*p0**2*q0-22.0*p0**2*qq*r0+26.0*p0**2*qr*r0-6.0
     . *p0**2*qr+18.0*p0**2*r0**2+12.0*p0**2*r0+2.0*p0**2*rr-12.0*p0**2
     . -16.0*p0*pq*q0*r0+4.0*p0*pq*qr+12.0*p0*pq*r0**2+4.0*p0*pq*r0-8.0
     . *p0*pq+8.0*p0*pr*q0*r0+4.0*p0*pr*q0-4.0*p0*pr*qq-4.0*p0*pr*qr+
     . 12.0*p0*pr*r0+4.0*p0*pr+45.0*p0*q0**2*r0**2-42.0*p0*q0**2*r0-3.0
     . *p0*q0**2*rr-30.0*p0*q0**2-8.0*p0*q0*qr*r0+4.0*p0*q0*qr-60.0*p0*
     . q0*r0**3+4.0*p0*q0*r0*rr+144.0*p0*q0*r0-33.0*p0*qq*r0**2+38.0*p0
     . *qq*r0+3.0*p0*qq*rr+22.0*p0*qq-4.0*p0*qr**2+48.0*p0*qr*r0**2-
     . 52.0*p0*qr*r0-12.0*p0*qr+24.0*p0*r0**3-18.0*p0*r0**2+16.0*p0*r0*
     . rr-48.0*p0*r0-10.0*p0*rr-22.0*pp*q0**2*r0-4.0*pp*q0*qr+33.0*pp*
     . q0*r0**2+38.0*pp*q0*r0-3.0*pp*q0*rr-22.0*pp*q0+26.0*pp*qq*r0-
     . 22.0*pp*qr*r0+6.0*pp*qr-24.0*pp*r0**2-16.0*pp*r0+16.0*pp+4.0*pq
     . **2*r0+4.0*pq*pr*q0-12.0*pq*pr*r0-4.0*pq*pr-12.0*pq*q0*r0**2+4.0
     . *pq*q0*r0+8.0*pq*q0+12.0*pq*qr*r0-4.0*pq*qr+4.0*pq*r0**3-4.0*pq*
     . r0*rr-24.0*pq*r0+4.0*pr**2*q0-8.0*pr**2-26.0*pr*q0**2*r0-6.0*pr*
     . q0**2+ans2
      tra001=4.0*ans1
      return
      end
c
      double precision function tra011(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      tra011=4.0*(60.0*p0**2*q0*r0-45.0*p0**2*r0**2-42.0*p0**2*r0+3.0*
     . p0**2*rr+30.0*p0**2-16.0*p0*pq*r0+8.0*p0*pr*r0+4.0*p0*pr+90.0*p0
     . *q0*r0**2-84.0*p0*q0*r0-6.0*p0*q0*rr-60.0*p0*q0-8.0*p0*qr*r0+4.0
     . *p0*qr-60.0*p0*r0**3+4.0*p0*r0*rr+144.0*p0*r0-44.0*pp*q0*r0-4.0*
     . pp*qr+33.0*pp*r0**2+38.0*pp*r0-3.0*pp*rr-22.0*pp+4.0*pq*pr-12.0*
     . pq*r0**2+4.0*pq*r0+8.0*pq+4.0*pr**2-52.0*pr*q0*r0-12.0*pr*q0+4.0
     . *pr*qr+48.0*pr*r0**2+52.0*pr*r0-12.0*pr-36.0*q0*r0**2+24.0*q0*r0
     . -4.0*q0*rr+24.0*q0-12.0*qr*r0+4.0*qr+24.0*r0**3+18.0*r0**2+16.0*
     . r0*rr-48.0*r0+10.0*rr)
      return
      end
c
      double precision function tra111(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      tra111=8.0*(60.0*p0*q0*r0-45.0*p0*r0**2-42.0*p0*r0+3.0*p0*rr+30.0
     . *p0-8.0*pq*r0+4.0*pr*r0+2.0*pr+45.0*q0*r0**2-42.0*q0*r0-3.0*q0*
     . rr-30.0*q0-4.0*qr*r0+2.0*qr-30.0*r0**3+2.0*r0*rr+72.0*r0)
      return
      end
c
c
      double precision function trb(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ans4=6.0*q0*r0*rr+24.0*q0*r0-2.0*q0*rr**2-12.0*q0*rr-72.0*q0-9.0*
     . qq*qr*r0**2-2.0*qq*qr*r0+5.0*qq*qr*rr+16.0*qq*qr+3.0*qq*r0**4-
     . 6.0*qq*r0**3-4.0*qq*r0**2*rr-4.0*qq*r0**2+2.0*qq*r0*rr+20.0*qq*
     . r0+qq*rr**2-20.0*qq-8.0*qr**3+14.0*qr**2*r0**2-12.0*qr**2*r0+2.0
     . *qr**2*rr+16.0*qr*r0**3+6.0*qr*r0**2+16.0*qr*r0*rr-12.0*qr*r0-
     . 14.0*qr*rr-40.0*qr+6.0*r0**4+4.0*r0**2*rr-24.0*r0**2+6.0*rr**2+
     . 32.0*rr+48.0
      ans3=8.0*pr**2*r0+8.0*pr**2-2.0*pr*q0**3*r0-4.0*pr*q0**3+10.0*pr*
     . q0**2*r0**2+12.0*pr*q0**2*r0-2.0*pr*q0**2*rr-4.0*pr*q0**2+2.0*pr
     . *q0*qq*r0+4.0*pr*q0*qq+4.0*pr*q0*qr*r0-4.0*pr*q0*qr-8.0*pr*q0*r0
     . **3-18.0*pr*q0*r0**2-8.0*pr*q0*r0*rr-4.0*pr*q0*r0-6.0*pr*q0*rr-
     . 8.0*pr*q0-10.0*pr*qq*r0**2-4.0*pr*qq*r0+6.0*pr*qq*rr+16.0*pr*qq-
     . 8.0*pr*qr**2+14.0*pr*qr*r0**2+4.0*pr*qr*r0+2.0*pr*qr*rr-16.0*pr*
     . qr+8.0*pr*r0**3+10.0*pr*r0**2+8.0*pr*r0*rr+4.0*pr*r0-2.0*pr*rr-
     . 8.0*pr-3.0*q0**3*r0**3+3.0*q0**3*r0**2-q0**3*r0*rr+6.0*q0**3*r0-
     . 5.0*q0**3*rr-12.0*q0**3-3.0*q0**2*qr*r0**2+10.0*q0**2*qr*r0-q0**
     . 2*qr*rr-8.0*q0**2*qr+3.0*q0**2*r0**4+6.0*q0**2*r0**3+14.0*q0**2*
     . r0**2*rr-12.0*q0**2*r0**2+2.0*q0**2*r0*rr-12.0*q0**2*r0-q0**2*rr
     . **2+36.0*q0**2+7.0*q0*qq*r0**3-7.0*q0*qq*r0**2-3.0*q0*qq*r0*rr-
     . 14.0*q0*qq*r0+9.0*q0*qq*rr+20.0*q0*qq+16.0*q0*qr**2*r0-8.0*q0*qr
     . **2-20.0*q0*qr*r0**3+8.0*q0*qr*r0**2-12.0*q0*qr*r0*rr+8.0*q0*qr*
     . r0+8.0*q0*qr-6.0*q0*r0**4-6.0*q0*r0**3-24.0*q0*r0**2*rr+24.0*q0*
     . r0**2+ans4
      ans2=-10.0*p0*qq*r0**2-6.0*p0*qq*r0*rr-20.0*p0*qq*r0+10.0*p0*qq*
     . rr+24.0*p0*qq+12.0*p0*qr**2*r0-12.0*p0*qr**2-12.0*p0*qr*r0**3+
     . 16.0*p0*qr*r0**2-4.0*p0*qr*r0*rr+16.0*p0*qr*r0+8.0*p0*qr*rr+16.0
     . *p0*qr-6.0*p0*r0**4+6.0*p0*r0**3-8.0*p0*r0**2*rr+24.0*p0*r0**2+
     . 10.0*p0*r0*rr-2.0*p0*rr**2-20.0*p0*rr-48.0*p0+3.0*pp*q0**2*r0**2
     . -pp*q0**2*rr-6.0*pp*q0**2+8.0*pp*q0*qr*r0-3.0*pp*q0*r0**3-11.0*
     . pp*q0*r0**2-5.0*pp*q0*r0*rr+6.0*pp*q0*r0+9.0*pp*q0*rr+32.0*pp*q0
     . -7.0*pp*qq*r0**2+5.0*pp*qq*rr+14.0*pp*qq-8.0*pp*qr**2+5.0*pp*qr*
     . r0**2-10.0*pp*qr*r0+3.0*pp*qr*rr+8.0*pp*r0**3+8.0*pp*r0**2-16.0*
     . pp*r0-16.0*pp*rr-32.0*pp+4.0*pq*pr*r0**2+8.0*pq*pr*r0+3.0*pq*q0
     . **2*r0**2-pq*q0**2*rr-6.0*pq*q0**2+8.0*pq*q0*qr*r0-6.0*pq*q0*r0
     . **3-10.0*pq*q0*r0**2-6.0*pq*q0*r0*rr+12.0*pq*q0*r0+2.0*pq*q0*rr+
     . 28.0*pq*q0-7.0*pq*qq*r0**2+5.0*pq*qq*rr+14.0*pq*qq-8.0*pq*qr**2+
     . 10.0*pq*qr*r0**2-4.0*pq*qr*r0+2.0*pq*qr*rr-8.0*pq*qr+3.0*pq*r0**
     . 4+6.0*pq*r0**3-4.0*pq*r0**2*rr-2.0*pq*r0**2+6.0*pq*r0*rr-4.0*pq*
     . r0+pq*rr**2+10.0*pq*rr-24.0*pq-4.0*pr**2*q0*r0-12.0*pr**2*q0+
     . ans3
      ans1=-3.0*p0**2*q0**2*r0**2+p0**2*q0**2*rr+6.0*p0**2*q0**2-8.0*p0
     . **2*q0*qr*r0+3.0*p0**2*q0*r0**3+9.0*p0**2*q0*r0**2+9.0*p0**2*q0*
     . r0*rr-6.0*p0**2*q0*r0+p0**2*q0*rr-24.0*p0**2*q0+7.0*p0**2*qq*r0
     . **2-5.0*p0**2*qq*rr-14.0*p0**2*qq+8.0*p0**2*qr**2-9.0*p0**2*qr*
     . r0**2-2.0*p0**2*qr*r0-3.0*p0**2*qr*rr-4.0*p0**2*qr-6.0*p0**2*r0
     . **3-6.0*p0**2*r0**2-2.0*p0**2*r0*rr+12.0*p0**2*r0+10.0*p0**2*rr+
     . 24.0*p0**2+2.0*p0*pq*r0**2-4.0*p0*pq*r0*rr-10.0*p0*pq*rr-8.0*p0*
     . pq+4.0*p0*pr*q0*r0+4.0*p0*pr*q0+4.0*p0*pr*qr*r0+12.0*p0*pr*qr-
     . 6.0*p0*pr*r0**2-4.0*p0*pr*r0-2.0*p0*pr*rr-3.0*p0*q0**3*r0**2+p0*
     . q0**3*rr+6.0*p0*q0**3-6.0*p0*q0**2*qr*r0+4.0*p0*q0**2*qr+12.0*p0
     . *q0**2*r0**2+8.0*p0*q0**2*r0*rr-4.0*p0*q0**2*rr-36.0*p0*q0**2+
     . 7.0*p0*q0*qq*r0**2-5.0*p0*q0*qq*rr-14.0*p0*q0*qq+8.0*p0*q0*qr**2
     . -22.0*p0*q0*qr*r0**2+12.0*p0*q0*qr*r0-2.0*p0*q0*qr*rr+4.0*p0*q0*
     . qr+3.0*p0*q0*r0**4+14.0*p0*q0*r0**2*rr-24.0*p0*q0*r0**2-16.0*p0*
     . q0*r0*rr-p0*q0*rr**2-4.0*p0*q0*rr+72.0*p0*q0-2.0*p0*qq*qr*r0-4.0
     . *p0*qq*qr+10.0*p0*qq*r0**3+ans2
      trb=4.0*ans1
      return
      end
c
      double precision function trb100(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ans2=-8.0*r0**2*rr+24.0*r0**2+10.0*r0*rr-2.0*rr**2-20.0*rr-48.0
      ans1=-6.0*p0*q0**2*r0**2+2.0*p0*q0**2*rr+12.0*p0*q0**2-16.0*p0*q0
     . *qr*r0+6.0*p0*q0*r0**3+18.0*p0*q0*r0**2+18.0*p0*q0*r0*rr-12.0*p0
     . *q0*r0+2.0*p0*q0*rr-48.0*p0*q0+14.0*p0*qq*r0**2-10.0*p0*qq*rr-
     . 28.0*p0*qq+16.0*p0*qr**2-18.0*p0*qr*r0**2-4.0*p0*qr*r0-6.0*p0*qr
     . *rr-8.0*p0*qr-12.0*p0*r0**3-12.0*p0*r0**2-4.0*p0*r0*rr+24.0*p0*
     . r0+20.0*p0*rr+48.0*p0+2.0*pq*r0**2-4.0*pq*r0*rr-10.0*pq*rr-8.0*
     . pq+4.0*pr*q0*r0+4.0*pr*q0+4.0*pr*qr*r0+12.0*pr*qr-6.0*pr*r0**2-
     . 4.0*pr*r0-2.0*pr*rr-3.0*q0**3*r0**2+q0**3*rr+6.0*q0**3-6.0*q0**2
     . *qr*r0+4.0*q0**2*qr+12.0*q0**2*r0**2+8.0*q0**2*r0*rr-4.0*q0**2*
     . rr-36.0*q0**2+7.0*q0*qq*r0**2-5.0*q0*qq*rr-14.0*q0*qq+8.0*q0*qr
     . **2-22.0*q0*qr*r0**2+12.0*q0*qr*r0-2.0*q0*qr*rr+4.0*q0*qr+3.0*q0
     . *r0**4+14.0*q0*r0**2*rr-24.0*q0*r0**2-16.0*q0*r0*rr-q0*rr**2-4.0
     . *q0*rr+72.0*q0-2.0*qq*qr*r0-4.0*qq*qr+10.0*qq*r0**3-10.0*qq*r0**
     . 2-6.0*qq*r0*rr-20.0*qq*r0+10.0*qq*rr+24.0*qq+12.0*qr**2*r0-12.0*
     . qr**2-12.0*qr*r0**3+16.0*qr*r0**2-4.0*qr*r0*rr+16.0*qr*r0+8.0*qr
     . *rr+16.0*qr-6.0*r0**4+6.0*r0**3+ans2
      trb100=4.0*ans1
      return
      end
c
      double precision function trb010(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ans2=9.0*q0**2*r0**2-3.0*q0**2*r0*rr+18.0*q0**2*r0-15.0*q0**2*rr-
     . 36.0*q0**2-6.0*q0*qr*r0**2+20.0*q0*qr*r0-2.0*q0*qr*rr-16.0*q0*qr
     . +6.0*q0*r0**4+12.0*q0*r0**3+28.0*q0*r0**2*rr-24.0*q0*r0**2+4.0*
     . q0*r0*rr-24.0*q0*r0-2.0*q0*rr**2+72.0*q0+7.0*qq*r0**3-7.0*qq*r0
     . **2-3.0*qq*r0*rr-14.0*qq*r0+9.0*qq*rr+20.0*qq+16.0*qr**2*r0-8.0*
     . qr**2-20.0*qr*r0**3+8.0*qr*r0**2-12.0*qr*r0*rr+8.0*qr*r0+8.0*qr-
     . 6.0*r0**4-6.0*r0**3-24.0*r0**2*rr+24.0*r0**2+6.0*r0*rr+24.0*r0-
     . 2.0*rr**2-12.0*rr-72.0
      ans1=-6.0*p0**2*q0*r0**2+2.0*p0**2*q0*rr+12.0*p0**2*q0-8.0*p0**2*
     . qr*r0+3.0*p0**2*r0**3+9.0*p0**2*r0**2+9.0*p0**2*r0*rr-6.0*p0**2*
     . r0+p0**2*rr-24.0*p0**2+4.0*p0*pr*r0+4.0*p0*pr-9.0*p0*q0**2*r0**2
     . +3.0*p0*q0**2*rr+18.0*p0*q0**2-12.0*p0*q0*qr*r0+8.0*p0*q0*qr+
     . 24.0*p0*q0*r0**2+16.0*p0*q0*r0*rr-8.0*p0*q0*rr-72.0*p0*q0+7.0*p0
     . *qq*r0**2-5.0*p0*qq*rr-14.0*p0*qq+8.0*p0*qr**2-22.0*p0*qr*r0**2+
     . 12.0*p0*qr*r0-2.0*p0*qr*rr+4.0*p0*qr+3.0*p0*r0**4+14.0*p0*r0**2*
     . rr-24.0*p0*r0**2-16.0*p0*r0*rr-p0*rr**2-4.0*p0*rr+72.0*p0+6.0*pp
     . *q0*r0**2-2.0*pp*q0*rr-12.0*pp*q0+8.0*pp*qr*r0-3.0*pp*r0**3-11.0
     . *pp*r0**2-5.0*pp*r0*rr+6.0*pp*r0+9.0*pp*rr+32.0*pp+6.0*pq*q0*r0
     . **2-2.0*pq*q0*rr-12.0*pq*q0+8.0*pq*qr*r0-6.0*pq*r0**3-10.0*pq*r0
     . **2-6.0*pq*r0*rr+12.0*pq*r0+2.0*pq*rr+28.0*pq-4.0*pr**2*r0-12.0*
     . pr**2-6.0*pr*q0**2*r0-12.0*pr*q0**2+20.0*pr*q0*r0**2+24.0*pr*q0*
     . r0-4.0*pr*q0*rr-8.0*pr*q0+2.0*pr*qq*r0+4.0*pr*qq+4.0*pr*qr*r0-
     . 4.0*pr*qr-8.0*pr*r0**3-18.0*pr*r0**2-8.0*pr*r0*rr-4.0*pr*r0-6.0*
     . pr*rr-8.0*pr-9.0*q0**2*r0**3+ans2
      trb010=4.0*ans1
      return
      end
c
      double precision function trb110(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      trb110=4.0*(-12.0*p0*q0*r0**2+4.0*p0*q0*rr+24.0*p0*q0-16.0*p0*qr*
     . r0+6.0*p0*r0**3+18.0*p0*r0**2+18.0*p0*r0*rr-12.0*p0*r0+2.0*p0*rr
     . -48.0*p0+4.0*pr*r0+4.0*pr-9.0*q0**2*r0**2+3.0*q0**2*rr+18.0*q0**
     . 2-12.0*q0*qr*r0+8.0*q0*qr+24.0*q0*r0**2+16.0*q0*r0*rr-8.0*q0*rr-
     . 72.0*q0+7.0*qq*r0**2-5.0*qq*rr-14.0*qq+8.0*qr**2-22.0*qr*r0**2+
     . 12.0*qr*r0-2.0*qr*rr+4.0*qr+3.0*r0**4+14.0*r0**2*rr-24.0*r0**2-
     . 16.0*r0*rr-rr**2-4.0*rr+72.0)
      return
      end
c
      double precision function trb101(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      trb101=8.0*(-6.0*p0*q0**2*r0-8.0*p0*q0*qr+9.0*p0*q0*r0**2+18.0*p0
     . *q0*r0+9.0*p0*q0*rr-6.0*p0*q0+14.0*p0*qq*r0-18.0*p0*qr*r0-2.0*p0
     . *qr-18.0*p0*r0**2-12.0*p0*r0-2.0*p0*rr+12.0*p0+2.0*pq*r0-2.0*pq*
     . rr+2.0*pr*q0+2.0*pr*qr-6.0*pr*r0-2.0*pr-3.0*q0**3*r0-3.0*q0**2*
     . qr+12.0*q0**2*r0+4.0*q0**2*rr+7.0*q0*qq*r0-22.0*q0*qr*r0+6.0*q0*
     . qr+6.0*q0*r0**3+14.0*q0*r0*rr-24.0*q0*r0-8.0*q0*rr-qq*qr+15.0*qq
     . *r0**2-10.0*qq*r0-3.0*qq*rr-10.0*qq+6.0*qr**2-18.0*qr*r0**2+16.0
     . *qr*r0-2.0*qr*rr+8.0*qr-12.0*r0**3+9.0*r0**2-8.0*r0*rr+24.0*r0+
     . 5.0*rr)
      return
      end
c
      double precision function trb001(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ans2=-4.0*pr**2*q0+8.0*pr**2-2.0*pr*q0**3+20.0*pr*q0**2*r0+12.0*
     . pr*q0**2+2.0*pr*q0*qq+4.0*pr*q0*qr-24.0*pr*q0*r0**2-36.0*pr*q0*
     . r0-8.0*pr*q0*rr-4.0*pr*q0-20.0*pr*qq*r0-4.0*pr*qq+28.0*pr*qr*r0+
     . 4.0*pr*qr+24.0*pr*r0**2+20.0*pr*r0+8.0*pr*rr+4.0*pr-9.0*q0**3*r0
     . **2+6.0*q0**3*r0-q0**3*rr+6.0*q0**3-6.0*q0**2*qr*r0+10.0*q0**2*
     . qr+12.0*q0**2*r0**3+18.0*q0**2*r0**2+28.0*q0**2*r0*rr-24.0*q0**2
     . *r0+2.0*q0**2*rr-12.0*q0**2+21.0*q0*qq*r0**2-14.0*q0*qq*r0-3.0*
     . q0*qq*rr-14.0*q0*qq+16.0*q0*qr**2-60.0*q0*qr*r0**2+16.0*q0*qr*r0
     . -12.0*q0*qr*rr+8.0*q0*qr-24.0*q0*r0**3-18.0*q0*r0**2-48.0*q0*r0*
     . rr+48.0*q0*r0+6.0*q0*rr+24.0*q0-18.0*qq*qr*r0-2.0*qq*qr+12.0*qq*
     . r0**3-18.0*qq*r0**2-8.0*qq*r0*rr-8.0*qq*r0+2.0*qq*rr+20.0*qq+
     . 28.0*qr**2*r0-12.0*qr**2+48.0*qr*r0**2+12.0*qr*r0+16.0*qr*rr-
     . 12.0*qr+24.0*r0**3+8.0*r0*rr-48.0*r0
      ans1=-6.0*p0**2*q0**2*r0-8.0*p0**2*q0*qr+9.0*p0**2*q0*r0**2+18.0*
     . p0**2*q0*r0+9.0*p0**2*q0*rr-6.0*p0**2*q0+14.0*p0**2*qq*r0-18.0*
     . p0**2*qr*r0-2.0*p0**2*qr-18.0*p0**2*r0**2-12.0*p0**2*r0-2.0*p0**
     . 2*rr+12.0*p0**2+4.0*p0*pq*r0-4.0*p0*pq*rr+4.0*p0*pr*q0+4.0*p0*pr
     . *qr-12.0*p0*pr*r0-4.0*p0*pr-6.0*p0*q0**3*r0-6.0*p0*q0**2*qr+24.0
     . *p0*q0**2*r0+8.0*p0*q0**2*rr+14.0*p0*q0*qq*r0-44.0*p0*q0*qr*r0+
     . 12.0*p0*q0*qr+12.0*p0*q0*r0**3+28.0*p0*q0*r0*rr-48.0*p0*q0*r0-
     . 16.0*p0*q0*rr-2.0*p0*qq*qr+30.0*p0*qq*r0**2-20.0*p0*qq*r0-6.0*p0
     . *qq*rr-20.0*p0*qq+12.0*p0*qr**2-36.0*p0*qr*r0**2+32.0*p0*qr*r0-
     . 4.0*p0*qr*rr+16.0*p0*qr-24.0*p0*r0**3+18.0*p0*r0**2-16.0*p0*r0*
     . rr+48.0*p0*r0+10.0*p0*rr+6.0*pp*q0**2*r0+8.0*pp*q0*qr-9.0*pp*q0*
     . r0**2-22.0*pp*q0*r0-5.0*pp*q0*rr+6.0*pp*q0-14.0*pp*qq*r0+10.0*pp
     . *qr*r0-10.0*pp*qr+24.0*pp*r0**2+16.0*pp*r0-16.0*pp+8.0*pq*pr*r0+
     . 8.0*pq*pr+6.0*pq*q0**2*r0+8.0*pq*q0*qr-18.0*pq*q0*r0**2-20.0*pq*
     . q0*r0-6.0*pq*q0*rr+12.0*pq*q0-14.0*pq*qq*r0+20.0*pq*qr*r0-4.0*pq
     . *qr+12.0*pq*r0**3+18.0*pq*r0**2-8.0*pq*r0*rr-4.0*pq*r0+6.0*pq*rr
     . -4.0*pq+ans2
      trb001=4.0*ans1
      return
      end
c
      double precision function trb011(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      trb011=4.0*(-12.0*p0**2*q0*r0-8.0*p0**2*qr+9.0*p0**2*r0**2+18.0*
     . p0**2*r0+9.0*p0**2*rr-6.0*p0**2+4.0*p0*pr-18.0*p0*q0**2*r0-12.0*
     . p0*q0*qr+48.0*p0*q0*r0+16.0*p0*q0*rr+14.0*p0*qq*r0-44.0*p0*qr*r0
     . +12.0*p0*qr+12.0*p0*r0**3+28.0*p0*r0*rr-48.0*p0*r0-16.0*p0*rr+
     . 12.0*pp*q0*r0+8.0*pp*qr-9.0*pp*r0**2-22.0*pp*r0-5.0*pp*rr+6.0*pp
     . +12.0*pq*q0*r0+8.0*pq*qr-18.0*pq*r0**2-20.0*pq*r0-6.0*pq*rr+12.0
     . *pq-4.0*pr**2-6.0*pr*q0**2+40.0*pr*q0*r0+24.0*pr*q0+2.0*pr*qq+
     . 4.0*pr*qr-24.0*pr*r0**2-36.0*pr*r0-8.0*pr*rr-4.0*pr-27.0*q0**2*
     . r0**2+18.0*q0**2*r0-3.0*q0**2*rr+18.0*q0**2-12.0*q0*qr*r0+20.0*
     . q0*qr+24.0*q0*r0**3+36.0*q0*r0**2+56.0*q0*r0*rr-48.0*q0*r0+4.0*
     . q0*rr-24.0*q0+21.0*qq*r0**2-14.0*qq*r0-3.0*qq*rr-14.0*qq+16.0*qr
     . **2-60.0*qr*r0**2+16.0*qr*r0-12.0*qr*rr+8.0*qr-24.0*r0**3-18.0*
     . r0**2-48.0*r0*rr+48.0*r0+6.0*rr+24.0)
      return
      end
c
      double precision function trb111(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      trb111=8.0*(-12.0*p0*q0*r0-8.0*p0*qr+9.0*p0*r0**2+18.0*p0*r0+9.0*
     . p0*rr-6.0*p0+2.0*pr-9.0*q0**2*r0-6.0*q0*qr+24.0*q0*r0+8.0*q0*rr+
     . 7.0*qq*r0-22.0*qr*r0+6.0*qr+6.0*r0**3+14.0*r0*rr-24.0*r0-8.0*rr)
      return
      end
c
c
      double precision function trc(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ans4=-12.0*q0**2*r0+10.0*q0**2*rr+24.0*q0**2+6.0*q0*qr*r0**2-4.0*
     . q0*qr*r0+2.0*q0*qr*rr-6.0*q0*r0**4-6.0*q0*r0**3-8.0*q0*r0**2*rr+
     . 24.0*q0*r0**2-10.0*q0*r0*rr-2.0*q0*rr**2-20.0*q0*rr-48.0*q0-8.0*
     . qq*r0**3+8.0*qq*r0**2+16.0*qq*r0-16.0*qq*rr-32.0*qq-8.0*qr**2*r0
     . +8.0*qr**2+8.0*qr*r0**3-10.0*qr*r0**2+8.0*qr*r0*rr+4.0*qr*r0+2.0
     . *qr*rr+8.0*qr+6.0*r0**4+4.0*r0**2*rr-24.0*r0**2+6.0*rr**2+32.0*
     . rr+48.0
      ans3=-7.0*pp*qq*r0**2+5.0*pp*qq*rr+14.0*pp*qq+10.0*pp*qr*r0**2-
     . 4.0*pp*qr*r0-6.0*pp*qr*rr-16.0*pp*qr+3.0*pp*r0**4+6.0*pp*r0**3-
     . 4.0*pp*r0**2*rr-4.0*pp*r0**2-2.0*pp*r0*rr-20.0*pp*r0+pp*rr**2-
     . 20.0*pp-8.0*pq*pr**2-10.0*pq*pr*r0**2-4.0*pq*pr*r0-2.0*pq*pr*rr+
     . 8.0*pq*pr+2.0*pq*q0*r0**2+4.0*pq*q0*r0*rr-10.0*pq*q0*rr-8.0*pq*
     . q0-4.0*pq*qr*r0**2+8.0*pq*qr*r0+3.0*pq*r0**4-6.0*pq*r0**3-4.0*pq
     . *r0**2*rr-2.0*pq*r0**2-6.0*pq*r0*rr+4.0*pq*r0+pq*rr**2+10.0*pq*
     . rr-24.0*pq+8.0*pr**3+8.0*pr**2*q0**2-12.0*pr**2*q0*r0-12.0*pr**2
     . *q0-8.0*pr**2*qq+8.0*pr**2*qr+14.0*pr**2*r0**2+12.0*pr**2*r0+2.0
     . *pr**2*rr+9.0*pr*q0**2*r0**2-2.0*pr*q0**2*r0+3.0*pr*q0**2*rr+4.0
     . *pr*q0**2-4.0*pr*q0*qr*r0+12.0*pr*q0*qr-12.0*pr*q0*r0**3-16.0*pr
     . *q0*r0**2-4.0*pr*q0*r0*rr+16.0*pr*q0*r0-8.0*pr*q0*rr-16.0*pr*q0-
     . 5.0*pr*qq*r0**2-10.0*pr*qq*r0-3.0*pr*qq*rr+14.0*pr*qr*r0**2-4.0*
     . pr*qr*r0+2.0*pr*qr*rr-16.0*pr*qr+16.0*pr*r0**3-6.0*pr*r0**2+16.0
     . *pr*r0*rr-12.0*pr*r0+14.0*pr*rr+40.0*pr+6.0*q0**2*r0**3-6.0*q0**
     . 2*r0**2+2.0*q0**2*r0*rr+ans4
      ans2=-8.0*p0*pr**2-8.0*p0*pr*q0**2*r0+22.0*p0*pr*q0*r0**2+12.0*p0
     . *pr*q0*r0+2.0*p0*pr*q0*rr-4.0*p0*pr*q0+8.0*p0*pr*qq*r0-4.0*p0*pr
     . *qr*r0-4.0*p0*pr*qr-20.0*p0*pr*r0**3-8.0*p0*pr*r0**2-12.0*p0*pr*
     . r0*rr+8.0*p0*pr*r0-8.0*p0*pr-3.0*p0*q0**2*r0**3+9.0*p0*q0**2*r0
     . **2-9.0*p0*q0**2*r0*rr+6.0*p0*q0**2*r0+p0*q0**2*rr-24.0*p0*q0**2
     . +4.0*p0*q0*qr*r0-4.0*p0*q0*qr+3.0*p0*q0*r0**4+14.0*p0*q0*r0**2*
     . rr-24.0*p0*q0*r0**2+16.0*p0*q0*r0*rr-p0*q0*rr**2-4.0*p0*q0*rr+
     . 72.0*p0*q0+3.0*p0*qq*r0**3-11.0*p0*qq*r0**2+5.0*p0*qq*r0*rr-6.0*
     . p0*qq*r0+9.0*p0*qq*rr+32.0*p0*qq+4.0*p0*qr**2*r0-12.0*p0*qr**2-
     . 8.0*p0*qr*r0**3+18.0*p0*qr*r0**2-8.0*p0*qr*r0*rr-4.0*p0*qr*r0+
     . 6.0*p0*qr*rr+8.0*p0*qr-6.0*p0*r0**4+6.0*p0*r0**3-24.0*p0*r0**2*
     . rr+24.0*p0*r0**2-6.0*p0*r0*rr-24.0*p0*r0-2.0*p0*rr**2-12.0*p0*rr
     . -72.0*p0-7.0*pp*pq*r0**2+5.0*pp*pq*rr+14.0*pp*pq-2.0*pp*pr*q0*r0
     . +4.0*pp*pr*q0+9.0*pp*pr*r0**2-2.0*pp*pr*r0-5.0*pp*pr*rr-16.0*pp*
     . pr+7.0*pp*q0**2*r0**2-5.0*pp*q0**2*rr-14.0*pp*q0**2-10.0*pp*q0*
     . r0**3-10.0*pp*q0*r0**2+6.0*pp*q0*r0*rr+20.0*pp*q0*r0+10.0*pp*q0*
     . rr+24.0*pp*q0+ans3
      ans1=-3.0*p0**3*q0*r0**2+p0**3*q0*rr+6.0*p0**3*q0-2.0*p0**3*qr*r0
     . +4.0*p0**3*qr+3.0*p0**3*r0**3+3.0*p0**3*r0**2+p0**3*r0*rr-6.0*p0
     . **3*r0-5.0*p0**3*rr-12.0*p0**3+3.0*p0**2*pq*r0**2-p0**2*pq*rr-
     . 6.0*p0**2*pq-6.0*p0**2*pr*q0*r0-4.0*p0**2*pr*q0+3.0*p0**2*pr*r0
     . **2+10.0*p0**2*pr*r0+p0**2*pr*rr+8.0*p0**2*pr-3.0*p0**2*q0**2*r0
     . **2+p0**2*q0**2*rr+6.0*p0**2*q0**2+12.0*p0**2*q0*r0**2-8.0*p0**2
     . *q0*r0*rr-4.0*p0**2*q0*rr-36.0*p0**2*q0+3.0*p0**2*qq*r0**2-p0**2
     . *qq*rr-6.0*p0**2*qq-10.0*p0**2*qr*r0**2+12.0*p0**2*qr*r0+2.0*p0
     . **2*qr*rr+4.0*p0**2*qr+3.0*p0**2*r0**4-6.0*p0**2*r0**3+14.0*p0**
     . 2*r0**2*rr-12.0*p0**2*r0**2-2.0*p0**2*r0*rr+12.0*p0**2*r0-p0**2*
     . rr**2+36.0*p0**2+7.0*p0*pp*q0*r0**2-5.0*p0*pp*q0*rr-14.0*p0*pp*
     . q0+2.0*p0*pp*qr*r0-4.0*p0*pp*qr-7.0*p0*pp*r0**3-7.0*p0*pp*r0**2+
     . 3.0*p0*pp*r0*rr+14.0*p0*pp*r0+9.0*p0*pp*rr+20.0*p0*pp+8.0*p0*pq*
     . pr*r0+6.0*p0*pq*r0**3-10.0*p0*pq*r0**2+6.0*p0*pq*r0*rr-12.0*p0*
     . pq*r0+2.0*p0*pq*rr+28.0*p0*pq+8.0*p0*pr**2*q0-16.0*p0*pr**2*r0+
     . ans2
      trc=4.0*ans1
      return
      end
c
      double precision function trc100(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ans2=-8.0*pr-3.0*q0**2*r0**3+9.0*q0**2*r0**2-9.0*q0**2*r0*rr+6.0*
     . q0**2*r0+q0**2*rr-24.0*q0**2+4.0*q0*qr*r0-4.0*q0*qr+3.0*q0*r0**4
     . +14.0*q0*r0**2*rr-24.0*q0*r0**2+16.0*q0*r0*rr-q0*rr**2-4.0*q0*rr
     . +72.0*q0+3.0*qq*r0**3-11.0*qq*r0**2+5.0*qq*r0*rr-6.0*qq*r0+9.0*
     . qq*rr+32.0*qq+4.0*qr**2*r0-12.0*qr**2-8.0*qr*r0**3+18.0*qr*r0**2
     . -8.0*qr*r0*rr-4.0*qr*r0+6.0*qr*rr+8.0*qr-6.0*r0**4+6.0*r0**3-
     . 24.0*r0**2*rr+24.0*r0**2-6.0*r0*rr-24.0*r0-2.0*rr**2-12.0*rr-
     . 72.0
      ans1=-9.0*p0**2*q0*r0**2+3.0*p0**2*q0*rr+18.0*p0**2*q0-6.0*p0**2*
     . qr*r0+12.0*p0**2*qr+9.0*p0**2*r0**3+9.0*p0**2*r0**2+3.0*p0**2*r0
     . *rr-18.0*p0**2*r0-15.0*p0**2*rr-36.0*p0**2+6.0*p0*pq*r0**2-2.0*
     . p0*pq*rr-12.0*p0*pq-12.0*p0*pr*q0*r0-8.0*p0*pr*q0+6.0*p0*pr*r0**
     . 2+20.0*p0*pr*r0+2.0*p0*pr*rr+16.0*p0*pr-6.0*p0*q0**2*r0**2+2.0*
     . p0*q0**2*rr+12.0*p0*q0**2+24.0*p0*q0*r0**2-16.0*p0*q0*r0*rr-8.0*
     . p0*q0*rr-72.0*p0*q0+6.0*p0*qq*r0**2-2.0*p0*qq*rr-12.0*p0*qq-20.0
     . *p0*qr*r0**2+24.0*p0*qr*r0+4.0*p0*qr*rr+8.0*p0*qr+6.0*p0*r0**4-
     . 12.0*p0*r0**3+28.0*p0*r0**2*rr-24.0*p0*r0**2-4.0*p0*r0*rr+24.0*
     . p0*r0-2.0*p0*rr**2+72.0*p0+7.0*pp*q0*r0**2-5.0*pp*q0*rr-14.0*pp*
     . q0+2.0*pp*qr*r0-4.0*pp*qr-7.0*pp*r0**3-7.0*pp*r0**2+3.0*pp*r0*rr
     . +14.0*pp*r0+9.0*pp*rr+20.0*pp+8.0*pq*pr*r0+6.0*pq*r0**3-10.0*pq*
     . r0**2+6.0*pq*r0*rr-12.0*pq*r0+2.0*pq*rr+28.0*pq+8.0*pr**2*q0-
     . 16.0*pr**2*r0-8.0*pr**2-8.0*pr*q0**2*r0+22.0*pr*q0*r0**2+12.0*pr
     . *q0*r0+2.0*pr*q0*rr-4.0*pr*q0+8.0*pr*qq*r0-4.0*pr*qr*r0-4.0*pr*
     . qr-20.0*pr*r0**3-8.0*pr*r0**2-12.0*pr*r0*rr+8.0*pr*r0+ans2
      trc100=4.0*ans1
      return
      end
c
      double precision function trc010(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ans2=-8.0*r0**2*rr+24.0*r0**2-10.0*r0*rr-2.0*rr**2-20.0*rr-48.0
      ans1=-3.0*p0**3*r0**2+p0**3*rr+6.0*p0**3-6.0*p0**2*pr*r0-4.0*p0**
     . 2*pr-6.0*p0**2*q0*r0**2+2.0*p0**2*q0*rr+12.0*p0**2*q0+12.0*p0**2
     . *r0**2-8.0*p0**2*r0*rr-4.0*p0**2*rr-36.0*p0**2+7.0*p0*pp*r0**2-
     . 5.0*p0*pp*rr-14.0*p0*pp+8.0*p0*pr**2-16.0*p0*pr*q0*r0+22.0*p0*pr
     . *r0**2+12.0*p0*pr*r0+2.0*p0*pr*rr-4.0*p0*pr-6.0*p0*q0*r0**3+18.0
     . *p0*q0*r0**2-18.0*p0*q0*r0*rr+12.0*p0*q0*r0+2.0*p0*q0*rr-48.0*p0
     . *q0+4.0*p0*qr*r0-4.0*p0*qr+3.0*p0*r0**4+14.0*p0*r0**2*rr-24.0*p0
     . *r0**2+16.0*p0*r0*rr-p0*rr**2-4.0*p0*rr+72.0*p0-2.0*pp*pr*r0+4.0
     . *pp*pr+14.0*pp*q0*r0**2-10.0*pp*q0*rr-28.0*pp*q0-10.0*pp*r0**3-
     . 10.0*pp*r0**2+6.0*pp*r0*rr+20.0*pp*r0+10.0*pp*rr+24.0*pp+2.0*pq*
     . r0**2+4.0*pq*r0*rr-10.0*pq*rr-8.0*pq+16.0*pr**2*q0-12.0*pr**2*r0
     . -12.0*pr**2+18.0*pr*q0*r0**2-4.0*pr*q0*r0+6.0*pr*q0*rr+8.0*pr*q0
     . -4.0*pr*qr*r0+12.0*pr*qr-12.0*pr*r0**3-16.0*pr*r0**2-4.0*pr*r0*
     . rr+16.0*pr*r0-8.0*pr*rr-16.0*pr+12.0*q0*r0**3-12.0*q0*r0**2+4.0*
     . q0*r0*rr-24.0*q0*r0+20.0*q0*rr+48.0*q0+6.0*qr*r0**2-4.0*qr*r0+
     . 2.0*qr*rr-6.0*r0**4-6.0*r0**3+ans2
      trc010=4.0*ans1
      return
      end
c
      double precision function trc110(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      trc110=4.0*(-9.0*p0**2*r0**2+3.0*p0**2*rr+18.0*p0**2-12.0*p0*pr*
     . r0-8.0*p0*pr-12.0*p0*q0*r0**2+4.0*p0*q0*rr+24.0*p0*q0+24.0*p0*r0
     . **2-16.0*p0*r0*rr-8.0*p0*rr-72.0*p0+7.0*pp*r0**2-5.0*pp*rr-14.0*
     . pp+8.0*pr**2-16.0*pr*q0*r0+22.0*pr*r0**2+12.0*pr*r0+2.0*pr*rr-
     . 4.0*pr-6.0*q0*r0**3+18.0*q0*r0**2-18.0*q0*r0*rr+12.0*q0*r0+2.0*
     . q0*rr-48.0*q0+4.0*qr*r0-4.0*qr+3.0*r0**4+14.0*r0**2*rr-24.0*r0**
     . 2+16.0*r0*rr-rr**2-4.0*rr+72.0)
      return
      end
c
      double precision function trc101(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      trc101=4.0*(-18.0*p0**2*q0*r0-6.0*p0**2*qr+27.0*p0**2*r0**2+18.0*
     . p0**2*r0+3.0*p0**2*rr-18.0*p0**2+12.0*p0*pq*r0-12.0*p0*pr*q0+
     . 12.0*p0*pr*r0+20.0*p0*pr-12.0*p0*q0**2*r0+48.0*p0*q0*r0-16.0*p0*
     . q0*rr+12.0*p0*qq*r0-40.0*p0*qr*r0+24.0*p0*qr+24.0*p0*r0**3-36.0*
     . p0*r0**2+56.0*p0*r0*rr-48.0*p0*r0-4.0*p0*rr+24.0*p0+14.0*pp*q0*
     . r0+2.0*pp*qr-21.0*pp*r0**2-14.0*pp*r0+3.0*pp*rr+14.0*pp+8.0*pq*
     . pr+18.0*pq*r0**2-20.0*pq*r0+6.0*pq*rr-12.0*pq-16.0*pr**2-8.0*pr*
     . q0**2+44.0*pr*q0*r0+12.0*pr*q0+8.0*pr*qq-4.0*pr*qr-60.0*pr*r0**2
     . -16.0*pr*r0-12.0*pr*rr+8.0*pr-9.0*q0**2*r0**2+18.0*q0**2*r0-9.0*
     . q0**2*rr+6.0*q0**2+4.0*q0*qr+12.0*q0*r0**3+28.0*q0*r0*rr-48.0*q0
     . *r0+16.0*q0*rr+9.0*qq*r0**2-22.0*qq*r0+5.0*qq*rr-6.0*qq+4.0*qr**
     . 2-24.0*qr*r0**2+36.0*qr*r0-8.0*qr*rr-4.0*qr-24.0*r0**3+18.0*r0**
     . 2-48.0*r0*rr+48.0*r0-6.0*rr-24.0)
      return
      end
c
      double precision function trc001(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ans2=14.0*pp*q0**2*r0-30.0*pp*q0*r0**2-20.0*pp*q0*r0+6.0*pp*q0*rr
     . +20.0*pp*q0-14.0*pp*qq*r0+20.0*pp*qr*r0-4.0*pp*qr+12.0*pp*r0**3+
     . 18.0*pp*r0**2-8.0*pp*r0*rr-8.0*pp*r0-2.0*pp*rr-20.0*pp-20.0*pq*
     . pr*r0-4.0*pq*pr+4.0*pq*q0*r0+4.0*pq*q0*rr-8.0*pq*qr*r0+8.0*pq*qr
     . +12.0*pq*r0**3-18.0*pq*r0**2-8.0*pq*r0*rr-4.0*pq*r0-6.0*pq*rr+
     . 4.0*pq-12.0*pr**2*q0+28.0*pr**2*r0+12.0*pr**2+18.0*pr*q0**2*r0-
     . 2.0*pr*q0**2-4.0*pr*q0*qr-36.0*pr*q0*r0**2-32.0*pr*q0*r0-4.0*pr*
     . q0*rr+16.0*pr*q0-10.0*pr*qq*r0-10.0*pr*qq+28.0*pr*qr*r0-4.0*pr*
     . qr+48.0*pr*r0**2-12.0*pr*r0+16.0*pr*rr-12.0*pr+18.0*q0**2*r0**2-
     . 12.0*q0**2*r0+2.0*q0**2*rr-12.0*q0**2+12.0*q0*qr*r0-4.0*q0*qr-
     . 24.0*q0*r0**3-18.0*q0*r0**2-16.0*q0*r0*rr+48.0*q0*r0-10.0*q0*rr-
     . 24.0*qq*r0**2+16.0*qq*r0+16.0*qq-8.0*qr**2+24.0*qr*r0**2-20.0*qr
     . *r0+8.0*qr*rr+4.0*qr+24.0*r0**3+8.0*r0*rr-48.0*r0
      ans1=-6.0*p0**3*q0*r0-2.0*p0**3*qr+9.0*p0**3*r0**2+6.0*p0**3*r0+
     . p0**3*rr-6.0*p0**3+6.0*p0**2*pq*r0-6.0*p0**2*pr*q0+6.0*p0**2*pr*
     . r0+10.0*p0**2*pr-6.0*p0**2*q0**2*r0+24.0*p0**2*q0*r0-8.0*p0**2*
     . q0*rr+6.0*p0**2*qq*r0-20.0*p0**2*qr*r0+12.0*p0**2*qr+12.0*p0**2*
     . r0**3-18.0*p0**2*r0**2+28.0*p0**2*r0*rr-24.0*p0**2*r0-2.0*p0**2*
     . rr+12.0*p0**2+14.0*p0*pp*q0*r0+2.0*p0*pp*qr-21.0*p0*pp*r0**2-
     . 14.0*p0*pp*r0+3.0*p0*pp*rr+14.0*p0*pp+8.0*p0*pq*pr+18.0*p0*pq*r0
     . **2-20.0*p0*pq*r0+6.0*p0*pq*rr-12.0*p0*pq-16.0*p0*pr**2-8.0*p0*
     . pr*q0**2+44.0*p0*pr*q0*r0+12.0*p0*pr*q0+8.0*p0*pr*qq-4.0*p0*pr*
     . qr-60.0*p0*pr*r0**2-16.0*p0*pr*r0-12.0*p0*pr*rr+8.0*p0*pr-9.0*p0
     . *q0**2*r0**2+18.0*p0*q0**2*r0-9.0*p0*q0**2*rr+6.0*p0*q0**2+4.0*
     . p0*q0*qr+12.0*p0*q0*r0**3+28.0*p0*q0*r0*rr-48.0*p0*q0*r0+16.0*p0
     . *q0*rr+9.0*p0*qq*r0**2-22.0*p0*qq*r0+5.0*p0*qq*rr-6.0*p0*qq+4.0*
     . p0*qr**2-24.0*p0*qr*r0**2+36.0*p0*qr*r0-8.0*p0*qr*rr-4.0*p0*qr-
     . 24.0*p0*r0**3+18.0*p0*r0**2-48.0*p0*r0*rr+48.0*p0*r0-6.0*p0*rr-
     . 24.0*p0-14.0*pp*pq*r0-2.0*pp*pr*q0+18.0*pp*pr*r0-2.0*pp*pr+ans2
      trc001=4.0*ans1
      return
      end
c
      double precision function trc011(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      trc011=8.0*(-3.0*p0**3*r0-3.0*p0**2*pr-6.0*p0**2*q0*r0+12.0*p0**2
     . *r0-4.0*p0**2*rr+7.0*p0*pp*r0-8.0*p0*pr*q0+22.0*p0*pr*r0+6.0*p0*
     . pr-9.0*p0*q0*r0**2+18.0*p0*q0*r0-9.0*p0*q0*rr+6.0*p0*q0+2.0*p0*
     . qr+6.0*p0*r0**3+14.0*p0*r0*rr-24.0*p0*r0+8.0*p0*rr-pp*pr+14.0*pp
     . *q0*r0-15.0*pp*r0**2-10.0*pp*r0+3.0*pp*rr+10.0*pp+2.0*pq*r0+2.0*
     . pq*rr-6.0*pr**2+18.0*pr*q0*r0-2.0*pr*q0-2.0*pr*qr-18.0*pr*r0**2-
     . 16.0*pr*r0-2.0*pr*rr+8.0*pr+18.0*q0*r0**2-12.0*q0*r0+2.0*q0*rr-
     . 12.0*q0+6.0*qr*r0-2.0*qr-12.0*r0**3-9.0*r0**2-8.0*r0*rr+24.0*r0-
     . 5.0*rr)
      return
      end
c
      double precision function trc111(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      trc111=8.0*(-9.0*p0**2*r0-6.0*p0*pr-12.0*p0*q0*r0+24.0*p0*r0-8.0*
     . p0*rr+7.0*pp*r0-8.0*pr*q0+22.0*pr*r0+6.0*pr-9.0*q0*r0**2+18.0*q0
     . *r0-9.0*q0*rr+6.0*q0+2.0*qr+6.0*r0**3+14.0*r0*rr-24.0*r0+8.0*rr)
      return
      end
c
c
c
c
      double precision function ztra(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ans2=-13.0*pp*qq*rr-16.0*pp*r0**2+32.0*pp*r0+16.0*pp*rr-4.0*pq**2
     . *pr+2.0*pq**2*r0**2-4.0*pq**2*r0-2.0*pq**2*rr+8.0*pq*pr*q0+16.0*
     . pq*pr-4.0*pq*q0*r0**2+8.0*pq*q0*r0+4.0*pq*q0*rr-8.0*pq*r0**2+
     . 16.0*pq*r0+8.0*pq*rr-24.0*pr*q0**2+48.0*pr*q0+32.0*pr*qq-48.0*pr
     . +12.0*q0**2*r0**2-24.0*q0**2*r0-12.0*q0**2*rr-24.0*q0*r0**2+48.0
     . *q0*r0+24.0*q0*rr-16.0*qq*r0**2+32.0*qq*r0+16.0*qq*rr+24.0*r0**2
     . -48.0*r0-24.0*rr
      ans1=30.0*p0**3*q0**2*r0-24.0*p0**3*q0*r0-22.0*p0**3*qq*r0+24.0*
     . p0**3*r0-16.0*p0**2*pq*q0*r0-8.0*p0**2*pq*r0-30.0*p0**2*pr*q0**2
     . +24.0*p0**2*pr*q0+22.0*p0**2*pr*qq-24.0*p0**2*pr+15.0*p0**2*q0**
     . 2*r0**2-54.0*p0**2*q0**2*r0-15.0*p0**2*q0**2*rr-12.0*p0**2*q0*r0
     . **2+72.0*p0**2*q0*r0+12.0*p0**2*q0*rr-11.0*p0**2*qq*r0**2+54.0*
     . p0**2*qq*r0+11.0*p0**2*qq*rr+12.0*p0**2*r0**2-72.0*p0**2*r0-12.0
     . *p0**2*rr-22.0*p0*pp*q0**2*r0+32.0*p0*pp*q0*r0+26.0*p0*pp*qq*r0-
     . 32.0*p0*pp*r0+4.0*p0*pq**2*r0+16.0*p0*pq*pr*q0+8.0*p0*pq*pr-8.0*
     . p0*pq*q0*r0**2+8.0*p0*pq*q0*r0+8.0*p0*pq*q0*rr-4.0*p0*pq*r0**2-
     . 8.0*p0*pq*r0+4.0*p0*pq*rr+24.0*p0*pr*q0**2-48.0*p0*pr*q0-32.0*p0
     . *pr*qq+48.0*p0*pr-12.0*p0*q0**2*r0**2+48.0*p0*q0**2*r0+12.0*p0*
     . q0**2*rr+24.0*p0*q0*r0**2-96.0*p0*q0*r0-24.0*p0*q0*rr+16.0*p0*qq
     . *r0**2-64.0*p0*qq*r0-16.0*p0*qq*rr-24.0*p0*r0**2+96.0*p0*r0+24.0
     . *p0*rr+22.0*pp*pr*q0**2-32.0*pp*pr*q0-26.0*pp*pr*qq+32.0*pp*pr-
     . 11.0*pp*q0**2*r0**2+22.0*pp*q0**2*r0+11.0*pp*q0**2*rr+16.0*pp*q0
     . *r0**2-32.0*pp*q0*r0-16.0*pp*q0*rr+13.0*pp*qq*r0**2-26.0*pp*qq*
     . r0+ans2
      ztra=8.0*ans1
      return
      end
c
      double precision function ztra100(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztra100=16.0*(45.0*p0**2*q0**2*r0-36.0*p0**2*q0*r0-33.0*p0**2*qq*
     . r0+36.0*p0**2*r0-16.0*p0*pq*q0*r0-8.0*p0*pq*r0-30.0*p0*pr*q0**2+
     . 24.0*p0*pr*q0+22.0*p0*pr*qq-24.0*p0*pr+15.0*p0*q0**2*r0**2-54.0*
     . p0*q0**2*r0-15.0*p0*q0**2*rr-12.0*p0*q0*r0**2+72.0*p0*q0*r0+12.0
     . *p0*q0*rr-11.0*p0*qq*r0**2+54.0*p0*qq*r0+11.0*p0*qq*rr+12.0*p0*
     . r0**2-72.0*p0*r0-12.0*p0*rr-11.0*pp*q0**2*r0+16.0*pp*q0*r0+13.0*
     . pp*qq*r0-16.0*pp*r0+2.0*pq**2*r0+8.0*pq*pr*q0+4.0*pq*pr-4.0*pq*
     . q0*r0**2+4.0*pq*q0*r0+4.0*pq*q0*rr-2.0*pq*r0**2-4.0*pq*r0+2.0*pq
     . *rr+12.0*pr*q0**2-24.0*pr*q0-16.0*pr*qq+24.0*pr-6.0*q0**2*r0**2+
     . 24.0*q0**2*r0+6.0*q0**2*rr+12.0*q0*r0**2-48.0*q0*r0-12.0*q0*rr+
     . 8.0*qq*r0**2-32.0*qq*r0-8.0*qq*rr-12.0*r0**2+48.0*r0+12.0*rr)
      return
      end
c
      double precision function ztra010(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztra010=16.0*(30.0*p0**3*q0*r0-12.0*p0**3*r0-8.0*p0**2*pq*r0-30.0
     . *p0**2*pr*q0+12.0*p0**2*pr+15.0*p0**2*q0*r0**2-54.0*p0**2*q0*r0-
     . 15.0*p0**2*q0*rr-6.0*p0**2*r0**2+36.0*p0**2*r0+6.0*p0**2*rr-22.0
     . *p0*pp*q0*r0+16.0*p0*pp*r0+8.0*p0*pq*pr-4.0*p0*pq*r0**2+4.0*p0*
     . pq*r0+4.0*p0*pq*rr+24.0*p0*pr*q0-24.0*p0*pr-12.0*p0*q0*r0**2+
     . 48.0*p0*q0*r0+12.0*p0*q0*rr+12.0*p0*r0**2-48.0*p0*r0-12.0*p0*rr+
     . 22.0*pp*pr*q0-16.0*pp*pr-11.0*pp*q0*r0**2+22.0*pp*q0*r0+11.0*pp*
     . q0*rr+8.0*pp*r0**2-16.0*pp*r0-8.0*pp*rr+4.0*pq*pr-2.0*pq*r0**2+
     . 4.0*pq*r0+2.0*pq*rr-24.0*pr*q0+24.0*pr+12.0*q0*r0**2-24.0*q0*r0-
     . 12.0*q0*rr-12.0*r0**2+24.0*r0+12.0*rr)
      return
      end
c
      double precision function ztra110(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztra110=32.0*(45.0*p0**2*q0*r0-18.0*p0**2*r0-8.0*p0*pq*r0-30.0*p0
     . *pr*q0+12.0*p0*pr+15.0*p0*q0*r0**2-54.0*p0*q0*r0-15.0*p0*q0*rr-
     . 6.0*p0*r0**2+36.0*p0*r0+6.0*p0*rr-11.0*pp*q0*r0+8.0*pp*r0+4.0*pq
     . *pr-2.0*pq*r0**2+2.0*pq*r0+2.0*pq*rr+12.0*pr*q0-12.0*pr-6.0*q0*
     . r0**2+24.0*q0*r0+6.0*q0*rr+6.0*r0**2-24.0*r0-6.0*rr)
      return
      end
c
      double precision function ztra101(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztra101=16.0*(45.0*p0**2*q0**2-36.0*p0**2*q0-33.0*p0**2*qq+36.0*
     . p0**2-16.0*p0*pq*q0-8.0*p0*pq+30.0*p0*q0**2*r0-54.0*p0*q0**2-
     . 24.0*p0*q0*r0+72.0*p0*q0-22.0*p0*qq*r0+54.0*p0*qq+24.0*p0*r0-
     . 72.0*p0-11.0*pp*q0**2+16.0*pp*q0+13.0*pp*qq-16.0*pp+2.0*pq**2-
     . 8.0*pq*q0*r0+4.0*pq*q0-4.0*pq*r0-4.0*pq-12.0*q0**2*r0+24.0*q0**2
     . +24.0*q0*r0-48.0*q0+16.0*qq*r0-32.0*qq-24.0*r0+48.0)
      return
      end
c
      double precision function ztra001(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztra001=16.0*(15.0*p0**3*q0**2-12.0*p0**3*q0-11.0*p0**3*qq+12.0*
     . p0**3-8.0*p0**2*pq*q0-4.0*p0**2*pq+15.0*p0**2*q0**2*r0-27.0*p0**
     . 2*q0**2-12.0*p0**2*q0*r0+36.0*p0**2*q0-11.0*p0**2*qq*r0+27.0*p0
     . **2*qq+12.0*p0**2*r0-36.0*p0**2-11.0*p0*pp*q0**2+16.0*p0*pp*q0+
     . 13.0*p0*pp*qq-16.0*p0*pp+2.0*p0*pq**2-8.0*p0*pq*q0*r0+4.0*p0*pq*
     . q0-4.0*p0*pq*r0-4.0*p0*pq-12.0*p0*q0**2*r0+24.0*p0*q0**2+24.0*p0
     . *q0*r0-48.0*p0*q0+16.0*p0*qq*r0-32.0*p0*qq-24.0*p0*r0+48.0*p0-
     . 11.0*pp*q0**2*r0+11.0*pp*q0**2+16.0*pp*q0*r0-16.0*pp*q0+13.0*pp*
     . qq*r0-13.0*pp*qq-16.0*pp*r0+16.0*pp+2.0*pq**2*r0-2.0*pq**2-4.0*
     . pq*q0*r0+4.0*pq*q0-8.0*pq*r0+8.0*pq+12.0*q0**2*r0-12.0*q0**2-
     . 24.0*q0*r0+24.0*q0-16.0*qq*r0+16.0*qq+24.0*r0-24.0)
      return
      end
c
      double precision function ztra011(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztra011=32.0*(15.0*p0**3*q0-6.0*p0**3-4.0*p0**2*pq+15.0*p0**2*q0*
     . r0-27.0*p0**2*q0-6.0*p0**2*r0+18.0*p0**2-11.0*p0*pp*q0+8.0*p0*pp
     . -4.0*p0*pq*r0+2.0*p0*pq-12.0*p0*q0*r0+24.0*p0*q0+12.0*p0*r0-24.0
     . *p0-11.0*pp*q0*r0+11.0*pp*q0+8.0*pp*r0-8.0*pp-2.0*pq*r0+2.0*pq+
     . 12.0*q0*r0-12.0*q0-12.0*r0+12.0)
      return
      end
c
      double precision function ztra111(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztra111=32.0*(45.0*p0**2*q0-18.0*p0**2-8.0*p0*pq+30.0*p0*q0*r0-
     . 54.0*p0*q0-12.0*p0*r0+36.0*p0-11.0*pp*q0+8.0*pp-4.0*pq*r0+2.0*pq
     . -12.0*q0*r0+24.0*q0+12.0*r0-24.0)
      return
      end
c
c
      double precision function ztrb(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ans2=3.0*pp*q0**2*r0**2-6.0*pp*q0**2*r0-3.0*pp*q0**2*rr-16.0*pp*
     . q0*r0**2+32.0*pp*q0*r0+16.0*pp*q0*rr-7.0*pp*qq*r0**2+14.0*pp*qq*
     . r0+7.0*pp*qq*rr+16.0*pp*r0**2-32.0*pp*r0-16.0*pp*rr-6.0*pq*pr*q0
     . **2+28.0*pq*pr*q0+14.0*pq*pr*qq-24.0*pq*pr+3.0*pq*q0**2*r0**2-
     . 6.0*pq*q0**2*r0-3.0*pq*q0**2*rr-14.0*pq*q0*r0**2+28.0*pq*q0*r0+
     . 14.0*pq*q0*rr-7.0*pq*qq*r0**2+14.0*pq*qq*r0+7.0*pq*qq*rr+12.0*pq
     . *r0**2-24.0*pq*r0-12.0*pq*rr-12.0*pr*q0**3+36.0*pr*q0**2+20.0*pr
     . *q0*qq-72.0*pr*q0-20.0*pr*qq+48.0*pr+6.0*q0**3*r0**2-12.0*q0**3*
     . r0-6.0*q0**3*rr-18.0*q0**2*r0**2+36.0*q0**2*r0+18.0*q0**2*rr-
     . 10.0*q0*qq*r0**2+20.0*q0*qq*r0+10.0*q0*qq*rr+36.0*q0*r0**2-72.0*
     . q0*r0-36.0*q0*rr+10.0*qq*r0**2-20.0*qq*r0-10.0*qq*rr-24.0*r0**2+
     . 48.0*r0+24.0*rr
      ans1=-6.0*p0**3*q0**2*r0+24.0*p0**3*q0*r0+14.0*p0**3*qq*r0-24.0*
     . p0**3*r0+8.0*p0**2*pq*r0+6.0*p0**2*pr*q0**2-24.0*p0**2*pr*q0-
     . 14.0*p0**2*pr*qq+24.0*p0**2*pr-6.0*p0**2*q0**3*r0-3.0*p0**2*q0**
     . 2*r0**2+42.0*p0**2*q0**2*r0+3.0*p0**2*q0**2*rr+14.0*p0**2*q0*qq*
     . r0+12.0*p0**2*q0*r0**2-96.0*p0**2*q0*r0-12.0*p0**2*q0*rr+7.0*p0
     . **2*qq*r0**2-38.0*p0**2*qq*r0-7.0*p0**2*qq*rr-12.0*p0**2*r0**2+
     . 72.0*p0**2*r0+12.0*p0**2*rr+6.0*p0*pp*q0**2*r0-32.0*p0*pp*q0*r0-
     . 14.0*p0*pp*qq*r0+32.0*p0*pp*r0-8.0*p0*pq*pr+6.0*p0*pq*q0**2*r0-
     . 28.0*p0*pq*q0*r0-14.0*p0*pq*qq*r0+4.0*p0*pq*r0**2+16.0*p0*pq*r0-
     . 4.0*p0*pq*rr+6.0*p0*pr*q0**3-36.0*p0*pr*q0**2-14.0*p0*pr*q0*qq+
     . 72.0*p0*pr*q0+24.0*p0*pr*qq-48.0*p0*pr-3.0*p0*q0**3*r0**2+18.0*
     . p0*q0**3*r0+3.0*p0*q0**3*rr+18.0*p0*q0**2*r0**2-72.0*p0*q0**2*r0
     . -18.0*p0*q0**2*rr+7.0*p0*q0*qq*r0**2-34.0*p0*q0*qq*r0-7.0*p0*q0*
     . qq*rr-36.0*p0*q0*r0**2+144.0*p0*q0*r0+36.0*p0*q0*rr-12.0*p0*qq*
     . r0**2+44.0*p0*qq*r0+12.0*p0*qq*rr+24.0*p0*r0**2-96.0*p0*r0-24.0*
     . p0*rr-6.0*pp*pr*q0**2+32.0*pp*pr*q0+14.0*pp*pr*qq-32.0*pp*pr+
     . ans2
      ztrb=8.0*ans1
      return
      end
c
      double precision function ztrb100(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztrb100=8.0*(-18.0*p0**2*q0**2*r0+72.0*p0**2*q0*r0+42.0*p0**2*qq*
     . r0-72.0*p0**2*r0+16.0*p0*pq*r0+12.0*p0*pr*q0**2-48.0*p0*pr*q0-
     . 28.0*p0*pr*qq+48.0*p0*pr-12.0*p0*q0**3*r0-6.0*p0*q0**2*r0**2+
     . 84.0*p0*q0**2*r0+6.0*p0*q0**2*rr+28.0*p0*q0*qq*r0+24.0*p0*q0*r0
     . **2-192.0*p0*q0*r0-24.0*p0*q0*rr+14.0*p0*qq*r0**2-76.0*p0*qq*r0-
     . 14.0*p0*qq*rr-24.0*p0*r0**2+144.0*p0*r0+24.0*p0*rr+6.0*pp*q0**2*
     . r0-32.0*pp*q0*r0-14.0*pp*qq*r0+32.0*pp*r0-8.0*pq*pr+6.0*pq*q0**2
     . *r0-28.0*pq*q0*r0-14.0*pq*qq*r0+4.0*pq*r0**2+16.0*pq*r0-4.0*pq*
     . rr+6.0*pr*q0**3-36.0*pr*q0**2-14.0*pr*q0*qq+72.0*pr*q0+24.0*pr*
     . qq-48.0*pr-3.0*q0**3*r0**2+18.0*q0**3*r0+3.0*q0**3*rr+18.0*q0**2
     . *r0**2-72.0*q0**2*r0-18.0*q0**2*rr+7.0*q0*qq*r0**2-34.0*q0*qq*r0
     . -7.0*q0*qq*rr-36.0*q0*r0**2+144.0*q0*r0+36.0*q0*rr-12.0*qq*r0**2
     . +44.0*qq*r0+12.0*qq*rr+24.0*r0**2-96.0*r0-24.0*rr)
      return
      end
c
      double precision function ztrb010(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztrb010=8.0*(-12.0*p0**3*q0*r0+24.0*p0**3*r0+12.0*p0**2*pr*q0-
     . 24.0*p0**2*pr-18.0*p0**2*q0**2*r0-6.0*p0**2*q0*r0**2+84.0*p0**2*
     . q0*r0+6.0*p0**2*q0*rr+14.0*p0**2*qq*r0+12.0*p0**2*r0**2-96.0*p0
     . **2*r0-12.0*p0**2*rr+12.0*p0*pp*q0*r0-32.0*p0*pp*r0+12.0*p0*pq*
     . q0*r0-28.0*p0*pq*r0+18.0*p0*pr*q0**2-72.0*p0*pr*q0-14.0*p0*pr*qq
     . +72.0*p0*pr-9.0*p0*q0**2*r0**2+54.0*p0*q0**2*r0+9.0*p0*q0**2*rr+
     . 36.0*p0*q0*r0**2-144.0*p0*q0*r0-36.0*p0*q0*rr+7.0*p0*qq*r0**2-
     . 34.0*p0*qq*r0-7.0*p0*qq*rr-36.0*p0*r0**2+144.0*p0*r0+36.0*p0*rr-
     . 12.0*pp*pr*q0+32.0*pp*pr+6.0*pp*q0*r0**2-12.0*pp*q0*r0-6.0*pp*q0
     . *rr-16.0*pp*r0**2+32.0*pp*r0+16.0*pp*rr-12.0*pq*pr*q0+28.0*pq*pr
     . +6.0*pq*q0*r0**2-12.0*pq*q0*r0-6.0*pq*q0*rr-14.0*pq*r0**2+28.0*
     . pq*r0+14.0*pq*rr-36.0*pr*q0**2+72.0*pr*q0+20.0*pr*qq-72.0*pr+
     . 18.0*q0**2*r0**2-36.0*q0**2*r0-18.0*q0**2*rr-36.0*q0*r0**2+72.0*
     . q0*r0+36.0*q0*rr-10.0*qq*r0**2+20.0*qq*r0+10.0*qq*rr+36.0*r0**2-
     . 72.0*r0-36.0*rr)
      return
      end
c
      double precision function ztrb110(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztrb110=8.0*(-36.0*p0**2*q0*r0+72.0*p0**2*r0+24.0*p0*pr*q0-48.0*
     . p0*pr-36.0*p0*q0**2*r0-12.0*p0*q0*r0**2+168.0*p0*q0*r0+12.0*p0*
     . q0*rr+28.0*p0*qq*r0+24.0*p0*r0**2-192.0*p0*r0-24.0*p0*rr+12.0*pp
     . *q0*r0-32.0*pp*r0+12.0*pq*q0*r0-28.0*pq*r0+18.0*pr*q0**2-72.0*pr
     . *q0-14.0*pr*qq+72.0*pr-9.0*q0**2*r0**2+54.0*q0**2*r0+9.0*q0**2*
     . rr+36.0*q0*r0**2-144.0*q0*r0-36.0*q0*rr+7.0*qq*r0**2-34.0*qq*r0-
     . 7.0*qq*rr-36.0*r0**2+144.0*r0+36.0*rr)
      return
      end
c
      double precision function ztrb101(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztrb101=16.0*(-9.0*p0**2*q0**2+36.0*p0**2*q0+21.0*p0**2*qq-36.0*
     . p0**2+8.0*p0*pq-6.0*p0*q0**3-6.0*p0*q0**2*r0+42.0*p0*q0**2+14.0*
     . p0*q0*qq+24.0*p0*q0*r0-96.0*p0*q0+14.0*p0*qq*r0-38.0*p0*qq-24.0*
     . p0*r0+72.0*p0+3.0*pp*q0**2-16.0*pp*q0-7.0*pp*qq+16.0*pp+3.0*pq*
     . q0**2-14.0*pq*q0-7.0*pq*qq+4.0*pq*r0+8.0*pq-3.0*q0**3*r0+9.0*q0
     . **3+18.0*q0**2*r0-36.0*q0**2+7.0*q0*qq*r0-17.0*q0*qq-36.0*q0*r0+
     . 72.0*q0-12.0*qq*r0+22.0*qq+24.0*r0-48.0)
      return
      end
c
      double precision function ztrb001(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztrb001=16.0*(-3.0*p0**3*q0**2+12.0*p0**3*q0+7.0*p0**3*qq-12.0*p0
     . **3+4.0*p0**2*pq-3.0*p0**2*q0**3-3.0*p0**2*q0**2*r0+21.0*p0**2*
     . q0**2+7.0*p0**2*q0*qq+12.0*p0**2*q0*r0-48.0*p0**2*q0+7.0*p0**2*
     . qq*r0-19.0*p0**2*qq-12.0*p0**2*r0+36.0*p0**2+3.0*p0*pp*q0**2-
     . 16.0*p0*pp*q0-7.0*p0*pp*qq+16.0*p0*pp+3.0*p0*pq*q0**2-14.0*p0*pq
     . *q0-7.0*p0*pq*qq+4.0*p0*pq*r0+8.0*p0*pq-3.0*p0*q0**3*r0+9.0*p0*
     . q0**3+18.0*p0*q0**2*r0-36.0*p0*q0**2+7.0*p0*q0*qq*r0-17.0*p0*q0*
     . qq-36.0*p0*q0*r0+72.0*p0*q0-12.0*p0*qq*r0+22.0*p0*qq+24.0*p0*r0-
     . 48.0*p0+3.0*pp*q0**2*r0-3.0*pp*q0**2-16.0*pp*q0*r0+16.0*pp*q0-
     . 7.0*pp*qq*r0+7.0*pp*qq+16.0*pp*r0-16.0*pp+3.0*pq*q0**2*r0-3.0*pq
     . *q0**2-14.0*pq*q0*r0+14.0*pq*q0-7.0*pq*qq*r0+7.0*pq*qq+12.0*pq*
     . r0-12.0*pq+6.0*q0**3*r0-6.0*q0**3-18.0*q0**2*r0+18.0*q0**2-10.0*
     . q0*qq*r0+10.0*q0*qq+36.0*q0*r0-36.0*q0+10.0*qq*r0-10.0*qq-24.0*
     . r0+24.0)
      return
      end
c
      double precision function ztrb011(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztrb011=16.0*(-6.0*p0**3*q0+12.0*p0**3-9.0*p0**2*q0**2-6.0*p0**2*
     . q0*r0+42.0*p0**2*q0+7.0*p0**2*qq+12.0*p0**2*r0-48.0*p0**2+6.0*p0
     . *pp*q0-16.0*p0*pp+6.0*p0*pq*q0-14.0*p0*pq-9.0*p0*q0**2*r0+27.0*
     . p0*q0**2+36.0*p0*q0*r0-72.0*p0*q0+7.0*p0*qq*r0-17.0*p0*qq-36.0*
     . p0*r0+72.0*p0+6.0*pp*q0*r0-6.0*pp*q0-16.0*pp*r0+16.0*pp+6.0*pq*
     . q0*r0-6.0*pq*q0-14.0*pq*r0+14.0*pq+18.0*q0**2*r0-18.0*q0**2-36.0
     . *q0*r0+36.0*q0-10.0*qq*r0+10.0*qq+36.0*r0-36.0)
      return
      end
c
      double precision function ztrb111(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztrb111=16.0*(-18.0*p0**2*q0+36.0*p0**2-18.0*p0*q0**2-12.0*p0*q0*
     . r0+84.0*p0*q0+14.0*p0*qq+24.0*p0*r0-96.0*p0+6.0*pp*q0-16.0*pp+
     . 6.0*pq*q0-14.0*pq-9.0*q0**2*r0+27.0*q0**2+36.0*q0*r0-72.0*q0+7.0
     . *qq*r0-17.0*qq-36.0*r0+72.0)
      return
      end
c
c
      double precision function ztrc(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ans2=36.0*p0*q0*rr-16.0*p0*qq*r0**2+64.0*p0*qq*r0+16.0*p0*qq*rr+
     . 36.0*p0*r0**2-120.0*p0*r0-36.0*p0*rr+14.0*pp*pq*pr-7.0*pp*pq*r0
     . **2+14.0*pp*pq*r0+7.0*pp*pq*rr-14.0*pp*pr*q0**2+24.0*pp*pr*q0+
     . 14.0*pp*pr*qq-20.0*pp*pr+7.0*pp*q0**2*r0**2-14.0*pp*q0**2*r0-7.0
     . *pp*q0**2*rr-12.0*pp*q0*r0**2+24.0*pp*q0*r0+12.0*pp*q0*rr-7.0*pp
     . *qq*r0**2+14.0*pp*qq*r0+7.0*pp*qq*rr+10.0*pp*r0**2-20.0*pp*r0-
     . 10.0*pp*rr-8.0*pq*pr*q0-24.0*pq*pr+4.0*pq*q0*r0**2-8.0*pq*q0*r0-
     . 4.0*pq*q0*rr+12.0*pq*r0**2-24.0*pq*r0-12.0*pq*rr+24.0*pr*q0**2-
     . 48.0*pr*q0-32.0*pr*qq+48.0*pr-12.0*q0**2*r0**2+24.0*q0**2*r0+
     . 12.0*q0**2*rr+24.0*q0*r0**2-48.0*q0*r0-24.0*q0*rr+16.0*qq*r0**2-
     . 32.0*qq*r0-16.0*qq*rr-24.0*r0**2+48.0*r0+24.0*rr
      ans1=-6.0*p0**4*q0*r0+12.0*p0**4*r0+6.0*p0**3*pq*r0+6.0*p0**3*pr*
     . q0-12.0*p0**3*pr-6.0*p0**3*q0**2*r0-3.0*p0**3*q0*r0**2+42.0*p0**
     . 3*q0*r0+3.0*p0**3*q0*rr+6.0*p0**3*qq*r0+6.0*p0**3*r0**2-48.0*p0
     . **3*r0-6.0*p0**3*rr+14.0*p0**2*pp*q0*r0-20.0*p0**2*pp*r0-6.0*p0
     . **2*pq*pr+3.0*p0**2*pq*r0**2-34.0*p0**2*pq*r0-3.0*p0**2*pq*rr+
     . 6.0*p0**2*pr*q0**2-36.0*p0**2*pr*q0-6.0*p0**2*pr*qq+36.0*p0**2*
     . pr-3.0*p0**2*q0**2*r0**2+30.0*p0**2*q0**2*r0+3.0*p0**2*q0**2*rr+
     . 18.0*p0**2*q0*r0**2-108.0*p0**2*q0*r0-18.0*p0**2*q0*rr+3.0*p0**2
     . *qq*r0**2-38.0*p0**2*qq*r0-3.0*p0**2*qq*rr-18.0*p0**2*r0**2+
     . 108.0*p0**2*r0+18.0*p0**2*rr-14.0*p0*pp*pq*r0-14.0*p0*pp*pr*q0+
     . 20.0*p0*pp*pr+14.0*p0*pp*q0**2*r0+7.0*p0*pp*q0*r0**2-38.0*p0*pp*
     . q0*r0-7.0*p0*pp*q0*rr-14.0*p0*pp*qq*r0-10.0*p0*pp*r0**2+40.0*p0*
     . pp*r0+10.0*p0*pp*rr+28.0*p0*pq*pr+8.0*p0*pq*q0*r0-14.0*p0*pq*r0
     . **2+52.0*p0*pq*r0+14.0*p0*pq*rr-24.0*p0*pr*q0**2+72.0*p0*pr*q0+
     . 32.0*p0*pr*qq-72.0*p0*pr+12.0*p0*q0**2*r0**2-48.0*p0*q0**2*r0-
     . 12.0*p0*q0**2*rr-36.0*p0*q0*r0**2+120.0*p0*q0*r0+ans2
      ztrc=8.0*ans1
      return
      end
c
      double precision function ztrc100(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztrc100=8.0*(-24.0*p0**3*q0*r0+48.0*p0**3*r0+18.0*p0**2*pq*r0+
     . 18.0*p0**2*pr*q0-36.0*p0**2*pr-18.0*p0**2*q0**2*r0-9.0*p0**2*q0*
     . r0**2+126.0*p0**2*q0*r0+9.0*p0**2*q0*rr+18.0*p0**2*qq*r0+18.0*p0
     . **2*r0**2-144.0*p0**2*r0-18.0*p0**2*rr+28.0*p0*pp*q0*r0-40.0*p0*
     . pp*r0-12.0*p0*pq*pr+6.0*p0*pq*r0**2-68.0*p0*pq*r0-6.0*p0*pq*rr+
     . 12.0*p0*pr*q0**2-72.0*p0*pr*q0-12.0*p0*pr*qq+72.0*p0*pr-6.0*p0*
     . q0**2*r0**2+60.0*p0*q0**2*r0+6.0*p0*q0**2*rr+36.0*p0*q0*r0**2-
     . 216.0*p0*q0*r0-36.0*p0*q0*rr+6.0*p0*qq*r0**2-76.0*p0*qq*r0-6.0*
     . p0*qq*rr-36.0*p0*r0**2+216.0*p0*r0+36.0*p0*rr-14.0*pp*pq*r0-14.0
     . *pp*pr*q0+20.0*pp*pr+14.0*pp*q0**2*r0+7.0*pp*q0*r0**2-38.0*pp*q0
     . *r0-7.0*pp*q0*rr-14.0*pp*qq*r0-10.0*pp*r0**2+40.0*pp*r0+10.0*pp*
     . rr+28.0*pq*pr+8.0*pq*q0*r0-14.0*pq*r0**2+52.0*pq*r0+14.0*pq*rr-
     . 24.0*pr*q0**2+72.0*pr*q0+32.0*pr*qq-72.0*pr+12.0*q0**2*r0**2-
     . 48.0*q0**2*r0-12.0*q0**2*rr-36.0*q0*r0**2+120.0*q0*r0+36.0*q0*rr
     . -16.0*qq*r0**2+64.0*qq*r0+16.0*qq*rr+36.0*r0**2-120.0*r0-36.0*rr
     . )
      return
      end
c
      double precision function ztrc010(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztrc010=8.0*(-6.0*p0**4*r0+6.0*p0**3*pr-12.0*p0**3*q0*r0-3.0*p0**
     . 3*r0**2+42.0*p0**3*r0+3.0*p0**3*rr+14.0*p0**2*pp*r0+12.0*p0**2*
     . pr*q0-36.0*p0**2*pr-6.0*p0**2*q0*r0**2+60.0*p0**2*q0*r0+6.0*p0**
     . 2*q0*rr+18.0*p0**2*r0**2-108.0*p0**2*r0-18.0*p0**2*rr-14.0*p0*pp
     . *pr+28.0*p0*pp*q0*r0+7.0*p0*pp*r0**2-38.0*p0*pp*r0-7.0*p0*pp*rr+
     . 8.0*p0*pq*r0-48.0*p0*pr*q0+72.0*p0*pr+24.0*p0*q0*r0**2-96.0*p0*
     . q0*r0-24.0*p0*q0*rr-36.0*p0*r0**2+120.0*p0*r0+36.0*p0*rr-28.0*pp
     . *pr*q0+24.0*pp*pr+14.0*pp*q0*r0**2-28.0*pp*q0*r0-14.0*pp*q0*rr-
     . 12.0*pp*r0**2+24.0*pp*r0+12.0*pp*rr-8.0*pq*pr+4.0*pq*r0**2-8.0*
     . pq*r0-4.0*pq*rr+48.0*pr*q0-48.0*pr-24.0*q0*r0**2+48.0*q0*r0+24.0
     . *q0*rr+24.0*r0**2-48.0*r0-24.0*rr)
      return
      end
c
      double precision function ztrc110(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztrc110=8.0*(-24.0*p0**3*r0+18.0*p0**2*pr-36.0*p0**2*q0*r0-9.0*p0
     . **2*r0**2+126.0*p0**2*r0+9.0*p0**2*rr+28.0*p0*pp*r0+24.0*p0*pr*
     . q0-72.0*p0*pr-12.0*p0*q0*r0**2+120.0*p0*q0*r0+12.0*p0*q0*rr+36.0
     . *p0*r0**2-216.0*p0*r0-36.0*p0*rr-14.0*pp*pr+28.0*pp*q0*r0+7.0*pp
     . *r0**2-38.0*pp*r0-7.0*pp*rr+8.0*pq*r0-48.0*pr*q0+72.0*pr+24.0*q0
     . *r0**2-96.0*q0*r0-24.0*q0*rr-36.0*r0**2+120.0*r0+36.0*rr)
      return
      end
c
      double precision function ztrc101(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztrc101=16.0*(-12.0*p0**3*q0+24.0*p0**3+9.0*p0**2*pq-9.0*p0**2*q0
     . **2-9.0*p0**2*q0*r0+63.0*p0**2*q0+9.0*p0**2*qq+18.0*p0**2*r0-
     . 72.0*p0**2+14.0*p0*pp*q0-20.0*p0*pp+6.0*p0*pq*r0-34.0*p0*pq-6.0*
     . p0*q0**2*r0+30.0*p0*q0**2+36.0*p0*q0*r0-108.0*p0*q0+6.0*p0*qq*r0
     . -38.0*p0*qq-36.0*p0*r0+108.0*p0-7.0*pp*pq+7.0*pp*q0**2+7.0*pp*q0
     . *r0-19.0*pp*q0-7.0*pp*qq-10.0*pp*r0+20.0*pp+4.0*pq*q0-14.0*pq*r0
     . +26.0*pq+12.0*q0**2*r0-24.0*q0**2-36.0*q0*r0+60.0*q0-16.0*qq*r0+
     . 32.0*qq+36.0*r0-60.0)
      return
      end
c
      double precision function ztrc001(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztrc001=16.0*(-3.0*p0**4*q0+6.0*p0**4+3.0*p0**3*pq-3.0*p0**3*q0**
     . 2-3.0*p0**3*q0*r0+21.0*p0**3*q0+3.0*p0**3*qq+6.0*p0**3*r0-24.0*
     . p0**3+7.0*p0**2*pp*q0-10.0*p0**2*pp+3.0*p0**2*pq*r0-17.0*p0**2*
     . pq-3.0*p0**2*q0**2*r0+15.0*p0**2*q0**2+18.0*p0**2*q0*r0-54.0*p0
     . **2*q0+3.0*p0**2*qq*r0-19.0*p0**2*qq-18.0*p0**2*r0+54.0*p0**2-
     . 7.0*p0*pp*pq+7.0*p0*pp*q0**2+7.0*p0*pp*q0*r0-19.0*p0*pp*q0-7.0*
     . p0*pp*qq-10.0*p0*pp*r0+20.0*p0*pp+4.0*p0*pq*q0-14.0*p0*pq*r0+
     . 26.0*p0*pq+12.0*p0*q0**2*r0-24.0*p0*q0**2-36.0*p0*q0*r0+60.0*p0*
     . q0-16.0*p0*qq*r0+32.0*p0*qq+36.0*p0*r0-60.0*p0-7.0*pp*pq*r0+7.0*
     . pp*pq+7.0*pp*q0**2*r0-7.0*pp*q0**2-12.0*pp*q0*r0+12.0*pp*q0-7.0*
     . pp*qq*r0+7.0*pp*qq+10.0*pp*r0-10.0*pp+4.0*pq*q0*r0-4.0*pq*q0+
     . 12.0*pq*r0-12.0*pq-12.0*q0**2*r0+12.0*q0**2+24.0*q0*r0-24.0*q0+
     . 16.0*qq*r0-16.0*qq-24.0*r0+24.0)
      return
      end
c
      double precision function ztrc011(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztrc011=16.0*(-3.0*p0**4-6.0*p0**3*q0-3.0*p0**3*r0+21.0*p0**3+7.0
     . *p0**2*pp-6.0*p0**2*q0*r0+30.0*p0**2*q0+18.0*p0**2*r0-54.0*p0**2
     . +14.0*p0*pp*q0+7.0*p0*pp*r0-19.0*p0*pp+4.0*p0*pq+24.0*p0*q0*r0-
     . 48.0*p0*q0-36.0*p0*r0+60.0*p0+14.0*pp*q0*r0-14.0*pp*q0-12.0*pp*
     . r0+12.0*pp+4.0*pq*r0-4.0*pq-24.0*q0*r0+24.0*q0+24.0*r0-24.0)
      return
      end
c
      double precision function ztrc111(p0,q0,r0,pp,qq,rr,pq,pr,qr)
      implicit double precision (a-z)
      ztrc111=16.0*(-12.0*p0**3-18.0*p0**2*q0-9.0*p0**2*r0+63.0*p0**2+
     . 14.0*p0*pp-12.0*p0*q0*r0+60.0*p0*q0+36.0*p0*r0-108.0*p0+14.0*pp*
     . q0+7.0*pp*r0-19.0*pp+4.0*pq+24.0*q0*r0-48.0*q0-36.0*r0+60.0)
      return
      end
c
c
c
c
