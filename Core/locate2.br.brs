00010 !  Replace S:\Core\Locate2
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fnopenprn,fngetdir,fnerror,fntos,fnflexadd1,fnacs,fnflexinit1,fngetcd,fntxt,fnlbl,fntop,fnchk,fnxit,fncmdset
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim a$*132,prg$*40,lc$*40,dur$*40,rep$*40,resp$(20)*100,txt$*100
00070   dim brfn$(1000)*255,brsfn$(1000)*255,dur$*200,item$(1000)*255
00080   dim report$*256,subprocfile$*256,procfile$*256,tempfile1$*256,tempfile2$*256
00100 ! ______________________________________________________________________
00120   let fngetcd(dur$)
00140   let fntop(program$,'Locate2')
00150   let filter$="*.br, *.br"
00160   let cancel=5
00170   let report$=env$('temp')&"\LocRpt-"&session$&".txt" !:
        let subprocfile$=env$('temp')&"\loc3-"&session$&".tmp" !:
        let procfile$=env$('temp')&"\Loc0-"&session$&".prc" !:
        let tempfile1$=env$('temp')&"\Loc1-"&session$&".tmp" !:
        let tempfile2$=env$('temp')&"\Loc2-"&session$&".tmp"
00180 ! ______________________________________________________________________
00190 MAIN: ! 
00200   let fntos("Locate") !:
        let lngth=8 : let ps=lngth+2 : let rc=lc=0
00210   let fnlbl(lc+=1,1,'Find:',lngth,1)
00220   let fntxt(lc,ps,16,40) !:
        let resp$(rc+=1)=lc$
00230   let fnlbl(lc,ps+18,'and',lngth)
00240   let fntxt(lc,ps+22,16,40) !:
        let resp$(rc+=1)=lc2$
00250   let fnlbl(lc+=1,1,'Path:',lngth,1)
00260   let fntxt(lc,ps,38,66,0,'72') !:
        let resp$(rc+=1)=dur$
00270   let lc+=1 ! blank line
00280   let fnlbl(lc+=1,1,'Replace:',lngth,1)
00290   let fntxt(lc,ps,38) !:
        let resp$(rc+=1)=rep$
00300   let fnlbl(lc+=1,1,"Filter:",lngth,1)
00310   let fntxt(lc,ps,38) !:
        let resp$(rc+=1)=filter$
00320   let fnchk(lc+=1,ps,'Append Previous Report',0) !:
        let resp$(rc+=1)="FALSE"
00330   let fnchk(lc+=1,ps,'Renumber all Programs',0) !:
        let resp$(rc+=1)="FALSE"
00340 !  Let FNTXT(LC,PS,18,40) !:
        !  Let RESP$(RC+=1)=LC2$
00350   let lc+=1 ! blank line
00360   let fnlbl(lc+=1,1,"Leave Replace blank to locate only" )
00370   let fnlbl(lc+=1,1,"Do NOT try to use Secondary Find if using Replace")
00380   let fnlbl(lc+=1,1,"In Windows XP I can use '*.br,*.br'")
00390   let fncmdset(2)
00400   let fnacs("Locate",0,mat resp$,ck)
00410   if ck=cancel then goto XIT
00420   let lc$=trim$(resp$(1)) !:
        let lc2$=trim$(resp$(2)) !:
        let dur$=trim$(resp$(3)) !:
        let rep$=trim$(resp$(4)) !:
        let filter$=trim$(resp$(5)) !:
        let app_prev$=resp$(6)
00430   if lc2$<>"" and rep$<>"" then goto MAIN
00440 ! 
00450   let fngetdir(dur$,mat brfn$," /s ",filter$)
00460   for j=1 to udim(brfn$)
00470     if trim$(brfn$(j))="" then mat brfn$(j-1) : goto L490
00480   next j
00490 L490: print "Found "&str$(j-1)&" files."
00500   open #2: "Name="&procfile$&",Replace",display,output 
00510   if uprc$(app_prev$)="FALSE" then !:
          execute "free Locate-Report.txt -n" ioerr L520
00520 L520: print #2: "print border: 'Locating...'"
00530 ! Print #2: "ProcErr Return" ! quietly continue on error ! XXX
00540   for j=1 to udim(brfn$)
00550     print #2: "Load "&brfn$(j) !:
          print #2: "Load "&brfn$(j) !:
          print #2: "Load "&brfn$(j) !:
          print #2: "Load "&brfn$(j)
00560     print #2: 'List >'&tempfile1$
00570     print #2: 'Load '&tempfile1$&',Source'
00580 !  record program name !:
          print #2: 'list 1 >'&tempfile2$ !:
          print #2: 'type '&tempfile2$&' >>'&report$
00590     if rep$<>"" then !:
            print #2: "List '"&lc$&"' >>"&report$ !:
            print #2: "List '"&lc$&"' Replace '"&rep$&"' >>"&report$ !:
            print #2: "List '"&lc$&"' Replace '"&rep$&"' >"&subprocfile$ !:
            print #2: "SubProc "&subprocfile$ !:
            print #2: "Replace "&brfn$(j)
00600     if rep$="" and lc2$="" then !:
            print #2: "list '"&lc$&"' >>"&report$
00610     if rep$="" and lc2$<>"" then !:
            print #2: "list '"&lc$&"' '"&lc2$&"' >>"&report$
00620   next j
00630   print #2: "Print Border: 'Location Complete'"
00640   print #2: "sy -w -C Notepad "&report$
00650   print #2: "Load "&prg$ !:
        print #2: "Load "&prg$ !:
        print #2: "Load "&prg$ !:
        print #2: "Load "&prg$ !:
        print #2: "Run"
00660   close #2: 
00670   execute "Proc "&procfile$
00680 ! ______________________________________________________________________
00690 XIT: ! 
00700 ! 
00710   stop  ! Let FNXIT("")
00720 ! ______________________________________________________________________
00730 ! <Updateable Region: ERTN>
00740 ERTN: let fnerror(program$,err,line,act$,"xit")
00750   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00760   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00770   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00780 ERTN_EXEC_ACT: execute act$ : goto ERTN
00790 ! /region
00800 ! ______________________________________________________________________
