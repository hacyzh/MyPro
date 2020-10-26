        subroutine gettiegn(a,b,c,d,e,f,omega2,wvno2,rp, rsv, 
     1      x11,x21,x31,x41,x12,x22,x32,x42,NP, NSV,m,omg,wvn,
     2      atna,atnb)
        implicit none
        COMPLEX*16 A,B,C,D,E,F
        real*8 wvno2, omega2
        COMPLEX*16 rp, rsv
c-----
c       get the modified eigen vectors x11 and x31 have 
c           lambda1 (rp ) factored out
c               modified eigen vectors x22 and x42 have 
c           lambda2 (rsv) factored out
c-----
        COMPLEX*16 X11, X21, X31, X41
        COMPLEX*16 X12, X22, X32, X42
c-----
c       norms
c-----
        COMPLEX*16 NP, NSV
        integer m
        real*8 omg, wvn
        complex*16 atna, atnb

        COMPLEX*16 xka2, xkb2

        integer NL
        parameter (NL=200)
        common/timod/td(NL),TA(NL),TC(NL),TL(NL),TN(NL),TF(NL),
     1      TRho(NL),
     2      qa(NL),qb(NL),etap(NL),etas(NL),
     3      frefp(NL), frefs(NL)
        real*4 td,TA,TC,TN,TL,TF,TRho,qa,
     1       qb,etap,etas,frefp,frefs
c-----
c       internal variables
c-----
        COMPLEX*16 L2(2)
        COMPLEX*16 bb, cc
        COMPLEX*16 CDSQRT
        COMPLEX*16 SRTBB4AC
        COMPLEX*16 ddef, aabc

        COMPLEX*16 ZFAC

c-----
c       first test to see if a fluid layer - if it is fluid, the
c       eigenfunctions are specially computed and we need only the
c       rp
c-----
C       WRITE(6,*)'TI:',m,TA(m),TC(m),TL(m),TN(m),TRHO(m)
        if(TL(m).eq.0.0 .or. TN(m).eq.0.0)then
            rp = cdsqrt(dcmplx(wvno2 -omega2*TRho(m)/(TA(m)), 0.0d+00))
            rsv = dcmplx(0.0d+000, 0.0d+00)
            return
        endif


        a = wvn * TF(m) / (TC(m))
        b = 1.0/(TC(m))
        c = - TRho(m)*omg*omg + wvn*wvn *
     1      (TA(m) -TF(m)*TF(m)/(TC(m)))
        d = - wvn
        e = 1.0/(TL(m))
        f = - TRho(m)*omg*omg

c-----
c       do algebra first to avoid numerical problems
c-----
        ddef = wvn*wvn - TRho(m)*omg*omg/(TL(m))
        aabc = wvn*wvn*TA(m)/TC(m) - TRho(m)*omg*omg/(TC(m))

c-----
c       Do the QUASI P and SV - WE MUST BE CAREFUL HERE CONCERNING
c       BRANCH CUTS OF THE SQUARE ROOT
c-----
c-----
c       The characteristic equation to be solved is
c
c       L^4 - L^2[ 2 ad +ec +fb ] + [ (d^2+ef)(a^2+bc)] = 0
c-----
        bb = 2.0d+00 * a*d + e*c +f*b
        cc = ddef * aabc
c----
c       ensure that the SQRT(bb*bb - 4.0D+00*cc) is in the
c       I and II quadrants
c-----

        SRTBB4AC = CDSQRT(bb*bb - 4.0D+00*cc)
        IF(DIMAG(SRTBB4AC) .lt.0.0D+00)THEN
            SRTBB4AC = - SRTBB4AC
        ENDIF
c-----
c       recall solutions of ax^2 + bx + c =0 are
c            a(x-x1)(x-x2)
c       thus if we know x1, x2 = c/x1
c
c-----
c       Determine L^2 with care for roundoff
c-----
     
        IF(DREAL(BB) .LT.0.0D+00 .AND. DREAL(SRTBB4AC).LT.0.0D+00)THEN
            L2(2) = ( bb - SRTBB4AC) / 2.0d+00
            if(cdabs(L2(2)).gt.0.0)then
                 L2(1) = cc/L2(2)
            else
                 L2(1) = ( bb + SRTBB4AC) / 2.0d+00
            endif
        ELSE
            L2(1) = ( bb + SRTBB4AC) / 2.0d+00
            if(cdabs(L2(1)).gt.0.0)then
                 L2(2) = cc/L2(1)
            else
                 L2(2) = ( bb - SRTBB4AC) / 2.0d+00
            endif
        ENDIF
c-----
c       Use the Lambda^2 values to form
c       xka^2 == k^2 - L(1)^2
c       xkb^2 == k^2 - L(2)^2
c       Associate the smallest xka, xkb with the P!
c-----
        xka2 = wvno2 - L2(1)
        xkb2 = wvno2 - L2(2)
        if(cdabs(xkb2) .lt. cdabs(xka2))THEN
                ZFAC = L2(1)
                L2(1) = L2(2)
                L2(2) = ZFAC
        endif
        rp  = CDSQRT(L2(1))
        rsv = CDSQRT(L2(2))

c-----
c       get the norms - note that the true norm will be 
c            2  NP amd 2 L(2) NSV
c       The factorization permits us to use the sin nz/n or n sin nz
c-----
C        NP  = (  L2(1)*(-2*a*b*d + 2*a*a*e + b*c*e - b*b*f)
C     1      + (a*a+b*c)*(2*b*d*d - 2*a*d*e + b*e*f - c*e*e) )
C        NSV = (- L2(2)*(2*b*d*d - 2*a*d*e - c*e*e + b*e*f)
C     1      + (d*d+e*f)*(2*a*b*d - 2*a*a*e + b*b*f - b*c*e) )
c-----
c       get the modified eigen vectors x11 and x31 have 
c           lambda1 (rp ) factored out
c               modified eigen vectors x22 and x42 have 
c           lambda2 (rsv) factored out
c-----
        x12 =              (b*d - a*e)
        x22 =  b*L2(2) - e*(b*c + a*a)
        x32 =    L2(2) -   (a*d + c*e)
        x42 = -a*L2(2) + d*(b*c + a*a)

        x11 = -e*L2(1) + b*(d*d + e*f)
        x21 =             ( b*d - a*e)
        x31 =  d*L2(1) - a*(d*d + e*f)
        x41 =  - ( L2(1) -  a*d - b*f)

c-----
c       TEST
c       Force the eigenfunctions to be as given in 7.4.4
c       note this will not work if wnv = 0
c-----
         if(wvn.ne.0.0)then
               zfac = wvn / x11
               x11  = x11 *zfac
               x21  = x21 *zfac
               x31  = x31 *zfac
               x41  = x41 *zfac
       
               zfac = wvn / x22
               x12  = x12 * zfac
               x22  = x22 * zfac
               x32  = x32 * zfac
               x42  = x42 * zfac
         endif
c-----
c       REDEFINE HERE USING THE adjusted eigenvectors
c       Note that TRUE NP  = 2 * np  * rp
c       Note that TRUE NSV = 2 * nsv * rsv     , where
c       but also note that since the 11 31 22 42 13 33 24 44 have
c       an eigenvalue factored out the normalization using these
c       elements will not use the rp and rsv
c-----  
        np   = x11*x41 - x21*x31
        nsv  = x12*x42 - x22*x32

        return
        end
