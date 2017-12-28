00010 ! Replace Test\N2Index
00020   dim b$*40,keyform$*80,blank$(10)
00030   open #fin:=1: "Name=Test\Temp.dat,KFName=Test\Temp.idx,Replace,RecL=64,KPs=1/3,KLn=2/2,Shr",internal,outIn,keyed 
00040   open #fish:=2: "Name=Test\Temp.dat,KFName=Test\TempFish.idx,Use,RecL=64,KPs=10,KLn=10,Shr",internal,outIn,keyed 
00050   keyform$='Form ' : key$=''
00060   do while kps(fin,j+=1)>0
00070     keyform$=keyform$&'Pos '&str$(kps(fin,j))&','
00080     keyform$=keyform$&'C '&str$(kln(fin,j))&','
00090     blank$(j)=rpt$(chr$(0),kln(fin,j))
00100     key$=key$&blank$(j)
00110   loop 
00120   keyform$=keyform$(1:len(keyform$)-1) ! remove the trailing comma
00130   mat blank$(j-1)
00140   write #fin,using keyform$,reserve: mat blank$
00150   read #fin,key=key$: 
00160   rewrite #fin,using 'Form Pos 1,N 2',same,reserve: 8
00165   read #fin,same: ! kj
00170   rewrite #fin,using 'Form Pos 3,N 2',same,reserve: 9
00175   read #fin,same: ! kj
00180   rewrite #fin,using 'Form Pos 5,C 40',same,reserve: 'eight - nine'
00190   release #fin: 
00200   close #fin: 
00210   close #fish: ioerr L220
00220 L220: open #tmpfile:=12: "Name=TEST\Temp.dat,KFName=TEST\Temp.idx,Shr",internal,outIn,keyed 
00230   key$=lpad$(str$(8),2)&lpad$(str$(9),2) !:
        read #tmpfile,using 'Form Pos 1,N 2,N 2,C 40',key=key$,reserve: a,b,b$
00231 ! kEY$=CNVRT$("PIC(##)",8)&CNVRT$("PIC(##)",9) !:
        ! Read #TMPFILE,Using 'Form Pos 1,N 2,N 2,C 40',Key=KEY$,Reserve: A,B,B$ !:
        ! Rick Graham's suggestion
00240   rewrite #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
00250   read #tmpfile,using 'form pos 1,n 2,n 2,c 40',same,reserve: a,b,b$
00260   reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
00270   rewrite #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
00280   read #tmpfile,using 'form pos 1,n 2,n 2,c 40',same,reserve: a,b,b$
00290   reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
00300   reread #tmpfile,using 'form pos 1,n 2,n 2,c 40',reserve: a,b,b$
00310   pr a,b,b$
00320   close #tmpfile: ! ,free doesn't work any more
00330 ! free off the test files
00340   execute 'Free test\Temp.dat -n' ioerr L350
00350 L350: execute 'Free test\TempFish.idx -n' ioerr L360
00360 L360: execute 'Free test\Temp.idx -n' ioerr L370
00370 L370: stop 
