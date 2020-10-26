        subroutine varsv(p,q, rp, rsv, 
     1      cosp, cosq, rsinp, rsinq, 
     1      sinpr, sinqr, pex,svex,iwat,dm)
c-----
c       p = rp  * h
c       q = rsv * h
c       rp  vertical wave number for P
c       rsv vertical wave number for SV
c       cosp=cosh(p)  rsinp =rp *sinh(p)  sinpr = sinh(p)/rp
c       cosq=cosh(q)  rsinsv=rsv*sinh(p)  sinpq = sinh(p)/rsv
c         The sin rd/r = d sin(rd)/(rd)
c              so check the size o rd
c              if(cdabs(p) .lt.1.0e-4)
c                       sinpr = dm
c              else
c                       sinpr = sinh(p)/rp
c       cosq=cosh(q)  rsinsv=rsv*sinh(p)  
c              if(cdabs(q) .lt.1.0e-4)
c                       sinqr = dm
c              else
c                       sinqr = sinh(q)/rsv
c-----
        implicit none
        COMPLEX*16 p, q
        COMPLEX*16 rp, rsv
        complex*16 cosp, cosq
        complex*16 rsinp, rsinq
        complex*16 sinpr, sinqr
        REAL *8 pex,svex
        integer iwat
        real*8 dm

        REAL*8 pr, pi, qr, qi
        COMPLEX*16 epp, epm, eqp, eqm
        COMPLEX*16 sinp, sinq

        REAL*8 PFAC, SVFAC
        
        pex  = 0.0d+00
        svex = 0.0d+00
        pr = dreal(p)
        pi = dimag(p)
        qr = dreal(q)
        qi = dimag(q)
        pex   = pr
        if(iwat.eq.1)then
c-----
c       fluid layer
c-----
            epp = dcmplx(dcos(pi), dsin(pi))/2.0
            epm = dconjg(epp)
            if(pr.lt.15.) then
                pfac=dexp(-2.*pr)
            else
                pfac  = 0.0d+00
            endif
            cosp = epp + pfac*epm
            sinp = epp - pfac*epm
            rsinp = rp *sinp
            if(dabs(pr) .lt. 1.0e-5 .and. cdabs(rp).lt.1.0e-5)then
                 sinpr = dm 
            else
                 sinpr = (sinp/rp)
            endif
            cosq  = 1.0d+00
            rsinq = 0.0d+00
            sinqr = 0.0d+00
        else
c-----
c       elastic layer
c-----
            svex = qr
            epp = dcmplx(dcos(pi), dsin(pi))/2.0
            epm = dconjg(epp)
            eqp = dcmplx(dcos(qi), dsin(qi))/2.0
            eqm = dconjg(eqp)
            if(pr.lt.15.) then
                pfac=dexp(-2.*pr)
            else
                pfac  = 0.0d+00
            endif
            cosp = (epp + pfac*epm)
            sinp = epp - pfac*epm
            rsinp = (rp *sinp)
            if(dabs(pr) .lt. 1.0e-5 .and. cdabs(rp).lt.1.0e-5)then
                 sinpr = dm 
            else
                 sinpr = (sinp/rp)
            endif
C           COSP  =COSP*DEXP(PEX)
C           SINPR=SINPR*DEXP(PEX)
C           RSINP=RSINP*DEXP(PEX)

            if(qr.lt.15.) then
                svfac=dexp(-2.*qr)
            else
                svfac  = 0.0d+00
            endif
            cosq = (eqp + svfac*eqm)
            sinq = eqp - svfac*eqm
            rsinq = (rsv*sinq)
            if(dabs(qr) .lt. 1.0e-5 .and. cdabs(rsv).lt.1.0e-5)then
                 sinqr = dm
            else
                 sinqr = (sinq/rsv)
            endif
C           COSQ =COSQ*DEXP(SVEX)
C           SINQR=SINQR*DEXP(SVEX)
C           RSINQ=RSINQ*DEXP(SVEX)

        endif
        return
        end
