00010 !  Replace S:\Core\Locate2
00020 !
00030   library 'S:\Core\Library': fnopenprn,fngetdir,fnerror,fnTos,fnflexadd1,fnAcs,fnflexinit1,fngetcd,fnTxt,fnLbl,fntop,fnChk,fnxit,fnCmdSet
00040   on error goto Ertn
00050 !
00060   dim a$*132,prg$*40,lc$*40,dur$*40,rep$*40,resp$(20)*100,txt$*100
00070   dim brfn$(1000)*255,brsfn$(1000)*255,dur$*200,item$(1000)*255
00080   dim report$*256,subprocfile$*256,procfile$*256,tempfile1$*256,tempfile2$*256
00100 !
00120   fngetcd(dur$)
00140   fntop(program$,'Locate2')
00150   filter$="*.br, *.br"
00160   cancel=5
00170   report$=env$('temp')&"\LocRpt-"&session$&".txt" !:
        subprocfile$=env$('temp')&"\loc3-"&session$&".tmp" !:
        procfile$=env$('temp')&"\Loc0-"&session$&".prc" !:
        tempfile1$=env$('temp')&"\Loc1-"&session$&".tmp" !:
        tempfile2$=env$('temp')&"\Loc2-"&session$&".tmp"
00180 !
00190 MAIN: ! 
00200   fnTos("Locate") !:
        lngth=8 : ps=lngth+2 : rc=lc=0
00210   fnLbl(lc+=1,1,'Find:',lngth,1)
00220   fnTxt(lc,ps,16,40) !:
        resp$(rc+=1)=lc$
00230   fnLbl(lc,ps+18,'and',lngth)
00240   fnTxt(lc,ps+22,16,40) !:
        resp$(rc+=1)=lc2$
00250   fnLbl(lc+=1,1,'Path:',lngth,1)
00260   fnTxt(lc,ps,38,66,0,'72') !:
        resp$(rc+=1)=dur$
00270   lc+=1 ! blank line
00280   fnLbl(lc+=1,1,'Replace:',lngth,1)
00290   fnTxt(lc,ps,38) !:
        resp$(rc+=1)=rep$
00300   fnLbl(lc+=1,1,"Filter:",lngth,1)
00310   fnTxt(lc,ps,38) !:
        resp$(rc+=1)=filter$
00320   fnChk(lc+=1,ps,'Append Previous Report',0) !:
        resp$(rc+=1)="FALSE"
00330   fnChk(lc+=1,ps,'Renumber all Programs',0) !:
        resp$(rc+=1)="FALSE"
00340 !  fnTxt(LC,PS,18,40) !:
        !  rESP$(RC+=1)=LC2$
00350   lc+=1 ! blank line
00360   fnLbl(lc+=1,1,"Leave Replace blank to locate only" )
00370   fnLbl(lc+=1,1,"Do NOT try to use Secondary Find if using Replace")
00380   fnLbl(lc+=1,1,"In Windows XP I can use '*.br,*.br'")
00390   fnCmdSet(2)
00400   fnAcs("Locate",0,mat resp$,ck)
00410   if ck=cancel then goto XIT
00420   lc$=trim$(resp$(1)) !:
        lc2$=trim$(resp$(2)) !:
        dur$=trim$(resp$(3)) !:
        rep$=trim$(resp$(4)) !:
        filter$=trim$(resp$(5)) !:
        app_prev$=resp$(6)
00430   if lc2$<>"" and rep$<>"" then goto MAIN
00440 ! 
00450   fngetdir(dur$,mat brfn$," /s ",filter$)
00460   for j=1 to udim(brfn$)
00470     if trim$(brfn$(j))="" then mat brfn$(j-1) : goto L490
00480   next j
00490 L490: pr "Found "&str$(j-1)&" files."
00500   open #2: "Name="&procfile$&",Replace",display,output 
00510   if uprc$(app_prev$)="FALSE" then !:
          execute "free Locate-Report.txt -n" ioerr L520
00520 L520: pr #2: "print border: 'Locating...'"
00530 ! pr #2: "ProcErr Return" ! quietly continue on error ! XXX
00540   for j=1 to udim(brfn$)
00550     pr #2: "Load "&brfn$(j) !:
          pr #2: "Load "&brfn$(j) !:
          pr #2: "Load "&brfn$(j) !:
          pr #2: "Load "&brfn$(j)
00560     pr #2: 'List >'&tempfile1$
00570     pr #2: 'Load '&tempfile1$&',Source'
00580 !  record program name !:
          pr #2: 'list 1 >'&tempfile2$ !:
          pr #2: 'type '&tempfile2$&' >>'&report$
00590     if rep$<>"" then !:
            pr #2: "List '"&lc$&"' >>"&report$ !:
            pr #2: "List '"&lc$&"' Replace '"&rep$&"' >>"&report$ !:
            pr #2: "List '"&lc$&"' Replace '"&rep$&"' >"&subprocfile$ !:
            pr #2: "SubProc "&subprocfile$ !:
            pr #2: "Replace "&brfn$(j)
00600     if rep$="" and lc2$="" then !:
            pr #2: "list '"&lc$&"' >>"&report$
00610     if rep$="" and lc2$<>"" then !:
            pr #2: "list '"&lc$&"' '"&lc2$&"' >>"&report$
00620   next j
00630   pr #2: "Print Border: 'Location Complete'"
00640   pr #2: "sy -w -C Notepad "&report$
00650   pr #2: "Load "&prg$ !:
        pr #2: "Load "&prg$ !:
        pr #2: "Load "&prg$ !:
        pr #2: "Load "&prg$ !:
        pr #2: "Run"
00660   close #2: 
00670   execute "Proc "&procfile$
00680 !
00690 XIT: ! 
00700 ! 
00710   stop  ! fnXIT("")
00720 !
00730 ! <Updateable Region: ERTN>
00740 ERTN: fnerror(program$,err,line,act$,"xit")
00750   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00760   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00770   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00780 ERTN_EXEC_ACT: execute act$ : goto ERTN
00790 ! /region
00800 !
