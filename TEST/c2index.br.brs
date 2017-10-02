00010 ! Replace Test\C2Index
00020   dim c$*40,keyform$*80,blank$(10)
00030   open #fin:=1: "Name=Temp.dat,KFName=Temp.idx,replace,RecL=64,KPs=1/3,KLn=2/2,Shr",internal,outin,keyed 
00040   close #fin: 
00050   open #fin: "Name=Temp.dat,KFName=Temp.idx,Use,RecL=64,KPs=1/3,KLn=2/2,Shr",internal,outin,keyed 
00060   write #fin,using keyform$="Form Pos 1,C 2,Pos 3,C 2": mat blank$
00070 ! Read #FIN,Key=KEY$: !:
        let editrec=rec(fin)
00080   rewrite #fin,using 'Form Pos 1,C 2',rec=editrec: "8"
00090   rewrite #fin,using 'Form Pos 3,C 2',rec=editrec: "9"
00100   rewrite #fin,using 'Form Pos 5,C 40',rec=editrec: 'eight - nine'
00110 ! Release #FIN:
00120   close #fin: 
00121 ! Execute 'Index Temp.dat Temp.idx 1/3 2/2 Replace,DupKeys'
00130   open #tmpfile:=12: "Name=Temp.dat,KFName=Temp.idx,Shr",internal,outin,keyed 
00140   let key$=rpad$(str$(8),2)&rpad$(str$(9),2) !:
        read #tmpfile,using 'Form Pos 1,C 2,C 2,C 40',key=key$,reserve: a$,b$,c$ !:
        ! br 4.03jy gives and error 4272 on this line !:
        ! so does 4.03k
00150   rewrite #tmpfile,using 'form pos 1,C 2,C 2,c 40',reserve: a$,b$,c$
00160   reread #tmpfile,using 'form pos 1,C 2,C 2,c 40',reserve: a$,b$,c$
00170   reread #tmpfile,using 'form pos 1,C 2,C 2,c 40',reserve: a$,b$,c$
00180   rewrite #tmpfile,using 'form pos 1,C 2,C 2,c 40',reserve: a$,b$,c$
00190   reread #tmpfile,using 'form pos 1,C 2,C 2,c 40',reserve: a$,b$,c$
00200   reread #tmpfile,using 'form pos 1,C 2,C 2,c 40',reserve: a$,b$,c$
00210   reread #tmpfile,using 'form pos 1,C 2,C 2,c 40',reserve: a$,b$,c$
00220   pr a$,b$,c$
00230   close #tmpfile: ! ,free doesn't work any more
00240 ! free off the test files
00250 ! Execute 'Free Temp.dat -n' !:
        ! Execute 'Free Temp.idx -n'
00260   stop 
