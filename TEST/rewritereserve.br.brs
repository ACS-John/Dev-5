! Replace Test\RewriteReserve
dim b$*40,keyform$*80,blank$(10)
open #fin:=1: "Name=Temp.dat,KFName=Temp.idx,Use,RecL=64,KPs=1/3,KLn=2/2,Shr",internal,outIn,keyed 
close #fin: 
open #fin: "Name=Temp.dat,KFName=Temp.idx,Use,RecL=64,KPs=1/3,KLn=2/2,Shr",internal,outIn,keyed 
keyform$='Form ' : key$=''
do while kps(fin,j+=1)>0
	keyform$=keyform$&'Pos '&str$(kps(fin,j))&','
	keyform$=keyform$&'C '&str$(kln(fin,j))&','
	blank$(j)=rpt$(chr$(0),kln(fin,j))
	key$=key$&blank$(j)
loop 
keyform$=keyform$(1:len(keyform$)-1) ! remove the trailing comma
mat blank$(j-1)
write #fin,using keyform$,reserve: mat blank$
read #fin,key=key$: 
rewrite #fin,using 'Form Pos 1,N 2',same,reserve: 8
rewrite #fin,using 'Form Pos 3,N 2',same,reserve: 9
rewrite #fin,using 'Form Pos 5,C 40',same,reserve: 'eight - nine'
release #fin: 
close #fin: 
open #tmpfile:=12: "Name=Temp.dat,KFName=Temp.idx,Shr",internal,outIn,keyed 
key$=lpad$(str$(8),2)&lpad$(str$(9),2) !:
read #tmpfile,using 'Form Pos 1,N 2,N 2,C 40',key=key$,reserve: a,b,b$
rewrite #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
rewrite #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
pr a,b,b$
close #tmpfile: ! ,free doesn't work any more
! free off the test files
execute 'Free Temp.dat -n'
execute 'Free Temp.idx -n'
stop 
