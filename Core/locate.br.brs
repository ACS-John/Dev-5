00010 !  Replace S:\Core\Locate
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fnopenprn,fngetdir,fnerror,fntos,fnflexadd1,fnacs,fnflexinit1,fnAcsInstallationPath$,fntxt,fnlbl,fnchk,fnxit,fncmdset,fntop,fnpause
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim a$*132,prg$*40,lc$*80,dur$*40,rep$*40,resp$(20)*100,txt$*100
00070   dim brfn$(1000)*255,brsfn$(1000)*255,dur$*200,item$(1000)*255
00080   dim report$*256,subprocfile$*256,procfile$*256,tempfile1$*256,tempfile2$*256
00090   dim insline$*78,filter$*38
00100 ! ______________________________________________________________________
00120   dur$=fnAcsInstallationPath$
00140   fntop(program$,"Locate 1")
00150   let filter$="*.br"
00160   cancel=5
00170   let report$=env$('temp')&"\LocRpt-"&session$&".txt" !:
        subprocfile$=env$('temp')&"\loc3-"&session$&".tmp" !:
        let procfile$=env$('temp')&"\Loc0-"&session$&".prc" !:
        let tempfile1$=env$('temp')&"\Loc1-"&session$&".tmp" !:
        let tempfile2$=env$('temp')&"\Loc2-"&session$&".tmp"
00180 ! ______________________________________________________________________
00190 MAIN: ! 
00200   fntos("Locate") !:
        lngth=17 : let ps=lngth+2 : let rc=lc=0
00210   fnlbl(lc+=1,1,'Find:',lngth,1)
00220   fntxt(lc,ps,16,63) !:
        let resp$(rc+=1)=lc$
00230   fnlbl(lc,ps+18,'and',lngth)
00240   fntxt(lc,ps+22,16,40) !:
        let resp$(rc+=1)=lc2$
00250   fnlbl(lc+=1,1,'Path:',lngth,1)
00260   fntxt(lc,ps,38,66,0,'72') !:
        let resp$(rc+=1)=dur$
00270   lc+=1 ! blank line
00280   fnlbl(lc+=1,1,'Replace:',lngth,1)
00290   fntxt(lc,ps,38) !:
        let resp$(rc+=1)=rep$
00300   fnlbl(lc,ps+40,"Leave Replace blank to locate only" )
00310   fnlbl(lc+=1,1,"Filter:",lngth,1)
00320   fntxt(lc,ps,38) !:
        let resp$(rc+=1)=filter$
00330   fnchk(lc+=1,ps,'Append Previous Report',0) !:
        let resp$(rc+=1)="False"
00340   fnchk(lc+=1,ps,'Renumber all Programs',0) !:
        let resp$(rc+=1)="False"
00350   lc+=1 ! blank line
00360   fnlbl(lc+=1,1,"Insert this Line:",lngth,1)
00370   fntxt(lc,ps,40,78,0,"",0,"This will be executed after Renumber, if you choose to, a good example is '45 ! this is a dumb comment'" ) !:
        let resp$(rc+=1)=""
00380   lc+=1 ! blank line
00390   fnlbl(lc+=1,1,"Do NOT try to use Secondary Find if using Replace")
00400   fncmdset(2)
00410   fnacs("Locate",0,mat resp$,ck)
00420   if ck=cancel then goto XIT
00430   lc$=trim$(resp$(1)) !:
        lc2$=trim$(resp$(2)) !:
        let dur$=trim$(resp$(3)) !:
        let rep$=trim$(resp$(4)) !:
        let filter$=trim$(resp$(5)) !:
        app_prev$=resp$(6) !:
        let rnm$=resp$(7) !:
        let insline$=trim$(resp$(8))
00440   if lc2$<>"" and rep$<>"" then goto MAIN
00450 ! 
00460   fngetdir(dur$,mat brfn$," /s ",filter$)
00470   for j=1 to udim(brfn$)
00480     if trim$(brfn$(j))="" then mat brfn$(j-1) : goto L500
00490   next j
00500 L500: pr "Found "&str$(j-1)&" files."
00510   open #2: "Name="&procfile$&",Replace",display,output 
00520   if uprc$(app_prev$)="FALSE" then !:
          execute "Free "&report$ ioerr L530
00530 L530: pr #2: "print border: 'Locating...'"
00540 ! pr #2: "ProcErr Return" ! quietly continue on error ! XXX
00550   for j=1 to udim(brfn$)
00560     pr #2: "Load "&brfn$(j) !:
          pr #2: "Load "&brfn$(j) !:
          pr #2: "Load "&brfn$(j) !:
          pr #2: "Load "&brfn$(j)
00570     pr #2: 'List >'&tempfile1$
00580     pr #2: 'Load '&tempfile1$&',Source'
00590 !  record program name !:
          pr #2: 'list 1 >'&tempfile2$ !:
          pr #2: 'type '&tempfile2$&' >>'&report$
00600     if rep$<>"" then !:
            pr #2: "List '"&lc$&"' >>"&report$ !:
            pr #2: "List '"&lc$&"' Replace '"&rep$&"' >>"&report$ !:
            pr #2: "List '"&lc$&"' Replace '"&rep$&"' >"&subprocfile$ !:
            pr #2: "SubProc "&subprocfile$ !:
            pr #2: "Replace "&brfn$(j)
00610     if rnm$="True" then !:
            pr #2: "Renum"
00620     if insline$<>"" then !:
            pr #2: insline$
00630     if rep$<>"" or insline$<>"" then !:
            pr #2: "Replace "&brfn$(j)
00640     if rep$="" and lc2$="" then !:
            pr #2: "List '"&lc$&"' >>"&report$
00650     if rep$="" and lc2$<>"" then !:
            pr #2: "List '"&lc$&"' '"&lc2$&"' >>"&report$
00660   next j
00670   pr #2: "Print Border: 'Location Complete'"
00680   pr #2: "sy -w Notepad "&report$
00690   let prg$="S:\Core\Locate" !:
        pr #2: "Load "&prg$ !:
        pr #2: "Load "&prg$ !:
        pr #2: "Load "&prg$ !:
        pr #2: "Load "&prg$ !:
        pr #2: "Run"
00700   close #2: 
00710   execute "Proc NoEcho"
00720   execute "Proc "&procfile$
00730 ! ______________________________________________________________________
00740 XIT: ! 
00760   fnxit
00770 ! ______________________________________________________________________
00780 ! <Updateable Region: ERTN>
00790 ERTN: let fnerror(program$,err,line,act$,"xit")
00800   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00810   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00820   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00830 ERTN_EXEC_ACT: execute act$ : goto ERTN
00840 ! /region
00850 ! ______________________________________________________________________
