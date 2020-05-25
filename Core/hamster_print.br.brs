! Replace S:\Core\Hamster
def library fnhamster_print(a$*20,mat lbl$,mat tln,c,mat p$; mat fltyp$,mat sln,mat mask,mat sp,mat c$,mat _k)
		library 'S:\Core\Library': fnopenprn,fncloseprn
		fnopenprn
		pr #255: "Field Name                     From   To  Format   "
		pr #255: "______________________________ ____ ____  _________"
		p1=1
		if udim(mat sln)<udim(mat lbl$) then mat sln(udim(mat lbl$))
		for j=1 to udim(mat lbl$)
			if sln(j)=0 then sln(j)=tln(j)
			p2=p1+sln(j)
			pr #255,using 'form pos 1,c 30,2*n 5,"  ",cr 4," ",g 6': lbl$(j),p1,p2-1,fltyp$(j),str$(sln(j))
			p1=p2
		next j
		fncloseprn
fnend
