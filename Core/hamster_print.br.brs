00010 ! Replace S:\Core\Hamster
50320   def library fnhamster_print(a$*20,mat lbl$,mat tln,c,mat p$; mat fltyp$,mat sln,mat mask,mat sp,mat c$,mat _k)
50322     library 'S:\Core\Library': fnopenprn,fncloseprn
50330     fnopenprn
50340     pr #255: "Field Name                     From   To  Format   "
50370     pr #255: "______________________________ ____ ____  _________"
50380     let p1=1
50382     if udim(mat sln)<udim(mat lbl$) then mat sln(udim(mat lbl$))
50390     for j=1 to udim(mat lbl$)
50400       if sln(j)=0 then let sln(j)=tln(j)
50402       let p2=p1+sln(j)
50410       pr #255,using 'form pos 1,c 30,2*n 5,"  ",cr 4," ",g 6': lbl$(j),p1,p2-1,fltyp$(j),str$(sln(j))
50430       let p1=p2
50440     next j
50450     fncloseprn
50460   fnend 
