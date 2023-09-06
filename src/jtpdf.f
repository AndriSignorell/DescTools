c     proof of independence in Mark van de Wiel dissertation
c     http://alexandria.tue.nl/extra2/200012964.pdf
c     subroutine jtpdf(n0, n1, n, df0, df1, df)
      subroutine jtpdf(mxsum, pdf, ng, cgsize, pdf0, pdf1)
      integer mxsum, ng, cgsize(ng)
      double precision pdf(mxsum),pdf0(mxsum),pdf1(mxsum)

c     use R's C-code for calculating the Mann-Whitney pdf
      double precision fdwilcox
      external fdwilcox

      integer i, j, g, mn0, mn1
      double precision di, dm, dn, dmw

c     pdf0 is the storage for current MW variable; pdf1 for convolved ones

c     if only two groups it reduces to Wilcoxon-Mann-Whitney
      m = cgsize(ng-1) - cgsize(ng)
      n = cgsize(ng)
      mn1 = m*n
      dm = dble(m)
      dn = dble(n)
      do 10 i = 0, mn1
         di = dble(i)
         pdf(i+1) = fdwilcox(di, dm, dn)
 10   continue

      do 60 g = ng-2,1,-1
c     current pdf1 is in pdf; set it and reset pdf to 0
         do 20 i = 1, mn1+1
            pdf1(i) = pdf(i)
            pdf(i) = 0
 20      continue
c     obtain pdf for group g against {g+1,...,ng}
         m = cgsize(g) - cgsize(g+1)
         n = cgsize(g+1)
         mn0 = m*n
         dm = dble(m)
         dn = dble(n)
c         call intpr(" m",2,m,1)
c         call intpr(" n",2,n,1)
         do 30 i = 0, mn0
            di = dble(i)
            dmw = fdwilcox(di, dm, dn)
c            call dblepr(" dmw", 4, dmw, 1)
            pdf0(i+1) = dmw
 30      continue
c         call dblepr(" pdf0", 5, pdf0, mn0+1)
c     convolve pdf0 and pdf1 and store in pdf
         do 50 i = 0, mn0
            do 40 j = 0, mn1
               pdf(i+j+1) = pdf(i+j+1) + pdf0(i+1)*pdf1(j+1)
 40         continue
 50      continue
c     increment the range for net iteration of pdf1 (if needed)
         mn1 = mn0 + mn1
 60   continue

      return
      end
