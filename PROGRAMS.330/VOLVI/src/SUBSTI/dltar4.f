       function dltar4(wvno,omga)
        implicit double precision (a-h,o-z)
       real*8 dltar4
c-----
c       find P-SV dispersion values.
c-----
        parameter (NL=200)
        common/timod/d(NL),TA(NL),TC(NL),TL(NL),TN(NL),TF(NL),
     1      TRho(NL),
     2      qa(NL),qb(NL),etap(NL),etas(NL),
     3      frefp(NL), frefs(NL)
        real*4 d,TA,TC,TN,TL,TF,TRho,qa,
     1       qb,etap,etas,frefp,frefs
        common/pari/ mmax,mode
        integer mmax,mode
        common/water/iwat(NL)
        integer iwat

        real*8 wvno,omga
        real*8 wvno2, omga2


        integer i,j

        complex*16 gbr(2,5)
        COMPLEX*16 zA,zB,zC,zD,zE,zF
        COMPLEX*16 rp, rsv
        complex*16 p,q
        COMPLEX*16 NP, NSV
        COMPLEX*16 X11, X21, X31, X41
        COMPLEX*16 X12, X22, X32, X42

        complex*16 ch(5,5), cr(5)

        complex*16 ee(4,4), alp(2)
        
        complex*16 cosp, cosq
        complex*16 rsinp, rsinq
        complex*16 sinpr, sinqr
        REAL *8 pex,svex
        real*8 exsum

        real*8 exa

        complex*16 tcr(5)
   
c-----
c       set up starting values for bottom halfspace
c-----
        wvno2=wvno*wvno
        omga2 = omga*omga
        call evalg(0,mmax,mmax-1,gbr,1,
     1      wvno,omga,omga2,wvno2)

      cr(1) = gbr(1,1)
      cr(2) = gbr(1,2)
      cr(3) = gbr(1,3)
      cr(4) = gbr(1,4)
      cr(5) = gbr(1,5)


      do m=mmax-1,1,-1
        call  gettiegn(Za,Zb,Zc,Zd,Ze,Zf,omga2,wvno2,rp, rsv,
     1      x11,x21,x31,x41,x12,x22,x32,x42,NP, NSV,m,omga,wvno,
     2      dcmplx(1.0d+00,0.0d+00),dcmplx(1.0d+00,0.0d+00))
         p = rp  * dble(d(m))
         q = rsv * dble(d(m))
c-----
c     create the normalized cosh nu z etc
c-----
         call varsv(p,q,rp, rsv,
     1         cosp, cosq, rsinp, rsinq,
     1         sinpr, sinqr, pex,svex,iwat(m),dble(d(m)))
CRBHc-----
CRBHc     get elements of Haskell propagator which is
CRBHc     only needed for the eigenfunction code
CRBHc-----
CRBH         if(pex .gt. svex)then
CRBHc-----
CRBHc               PEX > SVEX, factor out PEX
CRBHc-----
CRBH                if((pex-svex).gt. 40.0d+00)then
CRBH                    dfac = 0.0d+00
CRBH                else
CRBH                    dfac = dexp(-(pex-svex))
CRBH                endif
CRBH                cpex = pex
CRBH                call hska(AA,cosp,rsinp,sinpr,
CRBH     1              dfac*cosq,dfac*rsinq,dfac*sinqr,NP,NSV,
CRBH     1              X11, X21, X31, X41,X12, X22, X32, X42,
CRBH     2              Trho(m),iwat(m),pex,om2)
CRBH         else
CRBHc-----
CRBHc               SVEX > PEX, factor out SVEX
CRBHc-----
CRBH                if((svex-pex).gt. 40.0d+00)then
CRBH                    dfac = 0.0d+00
CRBH                else
CRBH                    dfac = dexp(-(svex-pex))
CRBH                endif
CRBH                cpex = svex
CRBH                call hska(AA,dfac*cosp,dfac*rsinp,dfac*sinpr,
CRBH     1              cosq,rsinq,sinqr,NP,NSV,
CRBH     1              X11, X21, X31, X41,X12, X22, X32, X42,
CRBH     2              Trho(m),iwat(m),pex,om2)
CRBH         endif

         alp(1) = 0.5/np
         alp(2) = 0.5/nsv
         ee(1,1) =  x11
         ee(2,1) =  x21
         ee(3,1) =  x31
         ee(4,1) =  x41

         ee(1,2) =  x12
         ee(2,2) =  x22
         ee(3,2) =  x32
         ee(4,2) =  x42

c-----
c        get the 6x6 compound matrix
c-----
         call dnka(ch,cosp,rsinp,sinpr,cosq,rsinq,sinqr,
     1         NP,NSV,
     1         x11,x21,x31,x41,x12,x22,x32,x42,
     1         TRho(m),iwat(m),pex+svex,om2)
C         do i=1,5
C         do j=1,5
C            write(6,*)'ch(',i,',',j,'):',ch(1,j) 
C         enddo
C          enddo
C          do j=1,5
C            write(6,*)'cr(',j,'):',cr(j) 
C          enddo
           do i=1,5
              tcr(i) = 0.0
              do j=1,5
                 tcr(i) = tcr(i) + cr(j)*ch(j,i)
              enddo
           enddo
           call normc(tcr,exa,5)
           do j=1,5
               cr(j) = tcr(j)
           enddo
         exsum = exsum + pex + svex + exa
      enddo
      dltar4 = dreal(cr(1))
      
      return
      end
