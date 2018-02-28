00010 ! Replace S:\acsPR\newjcPctCmp
00020 ! Enter Percent Complete
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fncno,fnerror,fnxit,fntop,fnmsgbox,fnTos,fnLbl,fnTxt,fncmbjob,fncmbcategory,fnCmdKey,fnAcs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,ml$(2)*60,resp$(5)*30
00080   dim jn$*6,jno$*6,n$*40,cn$*11,k$*25
00090 ! ______________________________________________________________________
00100   fntop("S:\acsPR\jcPctCmp",cap$="Enter Percent Complete")
00110   fncno(cno)
00120 ! 
00130 ! ______________________________________________________________________
00140   open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,outIn,keyed 
00150   open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,outIn,keyed 
00160 TRANSACTION_ENTRY: ! 
00165   cn=l10=l12=l13=0
00170   fnTos(sn$="Percentinput") !:
        respc=0 : frac=0 !:
        mylen=28 : mypos=mylen+3
00180   fnLbl(1,1,"Job Number:",mylen,1)
00190   fncmbjob(1,mypos) !:
        resp$(respc+=1)=jn$
00200   fnLbl(2,1,"Category:",mylen,1)
00210   fncmbcategory(2,mypos) !:
        resp$(respc+=1)=str$(cn)
00220   fnLbl(3,1,"Percent Complete - Labor:",mylen,1)
00230   fnTxt(3,mypos,3,3,0,"30",0,"Enter whole numbers. For example:  10% would be entered as 10") !:
        resp$(respc+=1)=str$(l12)
00240   fnLbl(4,1,"Percent Complete - Other:",mylen,1)
00250   fnTxt(4,mypos,3,3,0,"30",0,"Enter whole numbers. For example:  10% would be entered as 10") !:
        resp$(respc+=1)=str$(l13)
00260   fnLbl(5,1,"Units Completed:",mylen,1)
00270   fnTxt(5,mypos,7,7,0,"30",0,"If you are tracking units on the category, enter the units completed.") !:
        resp$(respc+=1)=str$(l10)
00280   fnCmdKey("&Update Job",1,1,0,"Posts these percentages immediately to this job.")
00290   fnCmdKey("&Complete",5,0,1,"Returns you to main menu.")
00300   fnAcs(sn$,0,mat resp$,ckey)
00310   if ckey=5 then goto XIT
00320   jn$=lpad$(trim$(resp$(1)(1:6)),6) ! job number
00330   if trim$(jn$)="" then goto L340 else goto L350
00340 L340: mat ml$(2) !:
        ml$(1)="You failed to enter a job number. You can not continue" !:
        ml$(2)="without a job number." !:
        fnmsgbox(mat ml$,resp$,cap$,0): goto TRANSACTION_ENTRY
00350 L350: cn=val(resp$(2)(1:5)) ! category
00360   if cn=0 then goto L370 else goto L380
00370 L370: mat ml$(2) !:
        ml$(1)="You failed to enter a category number. You cannot continue" !:
        ml$(2)="without a category number." !:
        fnmsgbox(mat ml$,resp$,cap$,0) !:
        goto TRANSACTION_ENTRY
00380 L380: cn$=lpad$(rtrm$(jn$),6)&lpad$(str$(cn),5)
00390   read #2,using L540,key=cn$: k$,rl10,rl12,rl13 nokey L410
00400   goto L420
00410 L410: mat ml$(2) !:
        ml$(1)="There is no job # "&trim$(jn$)&" with a category # "&str$(cn)&"." !:
        ml$(2)="You must enter a valid job or category number." !:
        fnmsgbox(mat ml$,resp$,cap$,0) !:
        goto TRANSACTION_ENTRY
00420 L420: l12=val(resp$(3)) ! labor %
00430   l13=val(resp$(4)) ! other %
00440   l10=val(resp$(5)) ! units
00450   cn$=lpad$(rtrm$(jn$),6)&lpad$(str$(cn),5)
00460   read #2,using L540,key=cn$: k$,rl10,rl12,rl13
00470   if l10=0 then goto L480 else goto L490
00480 L480: l10=rl10
00490 L490: if l12=0 then goto L500 else goto L510
00500 L500: l12=rl12
00510 L510: if l13=0 then goto L520 else goto L530
00520 L520: l13=rl13
00530 L530: rewrite #2,using L540,key=cn$: k$,l10,l12,l13
00540 L540: form pos 12,c 25,pos 100,pd 7.2,pos 114,2*pd 2
00550   goto TRANSACTION_ENTRY
00560 ! ______________________________________________________________________
00570 DONE: ! 
00580   close #2: 
00590   fnxit
00600 ! ______________________________________________________________________
00610 ! <Updateable Region: ERTN>
00620 ERTN: fnerror(program$,err,line,act$,"xit")
00630   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00640   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00650   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00660 ERTN_EXEC_ACT: execute act$ : goto ERTN
00670 ! /region
00680 ! ______________________________________________________________________
00690 XIT: fnxit
00700 ! ______________________________________________________________________
