00010 ! Replace Test\RewriteReserve
00020   dim b$*40,keyform$*80,blank$(10)
00030   open #fin:=1: "Name=Temp.dat,KFName=Temp.idx,Use,RecL=64,KPs=1/3,KLn=2/2,Shr",internal,outIn,keyed 
00040   close #fin: 
00050   open #fin: "Name=Temp.dat,KFName=Temp.idx,Use,RecL=64,KPs=1/3,KLn=2/2,Shr",internal,outIn,keyed 
00060   keyform$='Form ' : key$=''
00070   do while kps(fin,j+=1)>0
00080     keyform$=keyform$&'Pos '&str$(kps(fin,j))&','
00090     keyform$=keyform$&'C '&str$(kln(fin,j))&','
00100     blank$(j)=rpt$(chr$(0),kln(fin,j))
00110     key$=key$&blank$(j)
00120   loop 
00130   keyform$=keyform$(1:len(keyform$)-1) ! remove the trailing comma
00140   mat blank$(j-1)
00150   write #fin,using keyform$,reserve: mat blank$
00160   read #fin,key=key$: 
00170   rewrite #fin,using 'Form Pos 1,N 2',same,reserve: 8
00180   rewrite #fin,using 'Form Pos 3,N 2',same,reserve: 9
00190   rewrite #fin,using 'Form Pos 5,C 40',same,reserve: 'eight - nine'
00200   release #fin: 
00210   close #fin: 
00220   open #tmpfile:=12: "Name=Temp.dat,KFName=Temp.idx,Shr",internal,outIn,keyed 
00230   key$=lpad$(str$(8),2)&lpad$(str$(9),2) !:
        read #tmpfile,using 'Form Pos 1,N 2,N 2,C 40',key=key$,reserve: a,b,b$
00231   rewrite #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
00232   reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
00233   reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
00234   rewrite #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
00235   reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
00236   reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
00237   reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
00240   pr a,b,b$
00250   close #tmpfile: ! ,free doesn't work any more
00260 ! free off the test files
00270   execute 'Free Temp.dat -n' !:
        execute 'Free Temp.idx -n'
00280   stop 
