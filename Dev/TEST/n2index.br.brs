! Replace Test\N2Index
	dim b$*40,keyform$*80,blank$(10)
	open #fin:=1: "Name=Test\Temp.dat,KFName=Test\Temp.idx,Replace,RecL=64,KPs=1/3,KLn=2/2,Shr",internal,outIn,keyed 
	open #fish:=2: "Name=Test\Temp.dat,KFName=Test\TempFish.idx,Use,RecL=64,KPs=10,KLn=10,Shr",internal,outIn,keyed 
	keyform$='form ' : key$=''
	do while kps(fin,j+=1)>0
		keyform$=keyform$&'pos '&str$(kps(fin,j))&','
		keyform$=keyform$&'C '&str$(kln(fin,j))&','
		blank$(j)=rpt$(chr$(0),kln(fin,j))
		key$=key$&blank$(j)
	loop 
	keyform$=keyform$(1:len(keyform$)-1) ! remove the trailing comma
	mat blank$(j-1)
	write #fin,using keyform$,reserve: mat blank$
	read #fin,key=key$: 
	rewrite #fin,using 'form pos 1,N 2',same,reserve: 8
	read #fin,same: ! kj
	rewrite #fin,using 'form pos 3,N 2',same,reserve: 9
	read #fin,same: ! kj
	rewrite #fin,using 'form pos 5,C 40',same,reserve: 'eight - nine'
	release #fin: 
	close #fin: 
	close #fish: ioerr L220
L220: open #tmpfile:=12: "Name=TEST\Temp.dat,KFName=TEST\Temp.idx,Shr",internal,outIn,keyed 
	key$=lpad$(str$(8),2)&lpad$(str$(9),2) 
	read #tmpfile,using 'form pos 1,N 2,N 2,C 40',key=key$,reserve: a,b,b$
! kEY$=CNVRT$("PIC(##)",8)&CNVRT$("PIC(##)",9) 
	! Read #TMPFILE,Using 'form pos 1,N 2,N 2,C 40',Key=KEY$,Reserve: A,B,B$ 
	! Rick Graham's suggestion
	rewrite #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
	read #tmpfile,using 'form pos 1,n 2,n 2,c 40',same,reserve: a,b,b$
	reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
	rewrite #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
	read #tmpfile,using 'form pos 1,n 2,n 2,c 40',same,reserve: a,b,b$
	reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
	reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
	pr a,b,b$
	close #tmpfile: ! ,free doesn't work any more
! free off the test files
	execute 'Free test\Temp.dat -n' ioerr L350
L350: execute 'Free test\TempFish.idx -n' ioerr L360
L360: execute 'Free test\Temp.idx -n' ioerr L370
L370: stop 
