! Replace Test\C2Index
	dim c$*40,keyform$*80,blank$(10)
	open #fin:=1: "Name=Temp.dat,KFName=Temp.idx,replace,RecL=64,KPs=1/3,KLn=2/2,Shr",internal,outIn,keyed 
	close #fin: 
	open #fin: "Name=Temp.dat,KFName=Temp.idx,Use,RecL=64,KPs=1/3,KLn=2/2,Shr",internal,outIn,keyed 
	write #fin,using keyform$="Form Pos 1,C 2,Pos 3,C 2": mat blank$
! Read #FIN,Key=KEY$: 
	editrec=rec(fin)
	rewrite #fin,using 'Form Pos 1,C 2',rec=editrec: "8"
	rewrite #fin,using 'Form Pos 3,C 2',rec=editrec: "9"
	rewrite #fin,using 'Form Pos 5,C 40',rec=editrec: 'eight - nine'
! Release #FIN:
	close #fin: 
! Execute 'Index Temp.dat Temp.idx 1/3 2/2 Replace,DupKeys'
	open #tmpfile:=12: "Name=Temp.dat,KFName=Temp.idx,Shr",internal,outIn,keyed 
	key$=rpad$(str$(8),2)&rpad$(str$(9),2) 
	read #tmpfile,using 'Form Pos 1,C 2,C 2,C 40',key=key$,reserve: a$,b$,c$ 
	! br 4.03jy gives and error 4272 on this line 
	! so does 4.03k
	rewrite #tmpfile,using 'form pos 1,C 2,C 2,c 40',reserve: a$,b$,c$
	reread #tmpfile,using 'form pos 1,C 2,C 2,c 40',reserve: a$,b$,c$
	reread #tmpfile,using 'form pos 1,C 2,C 2,c 40',reserve: a$,b$,c$
	rewrite #tmpfile,using 'form pos 1,C 2,C 2,c 40',reserve: a$,b$,c$
	reread #tmpfile,using 'form pos 1,C 2,C 2,c 40',reserve: a$,b$,c$
	reread #tmpfile,using 'form pos 1,C 2,C 2,c 40',reserve: a$,b$,c$
	reread #tmpfile,using 'form pos 1,C 2,C 2,c 40',reserve: a$,b$,c$
	pr a$,b$,c$
	close #tmpfile: ! ,free doesn't work any more
! free off the test files
! Execute 'Free Temp.dat -n' 
! Execute 'Free Temp.idx -n'
	stop 
