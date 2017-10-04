00010 ! replace S:\acsPR\Employee_Srch.br
00020 ! search for an employee
00030 ! ______________________________________________________________________
00040   def library fnemployee_srch(&x$;fixgrid)
00050     library 'S:\Core\Library': fntos,fnflexinit1,fnflexadd1,fnacs,fncmdset,fnerror,fncno,fngethandle
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     dim item$(6)*30,resp$(30)*80
00090 ! ______________________________________________________________________
00100 ! x$=account #     !:
          ! to extract the flexgrid information (master file)
00104     fncno(cno)
00105     open #file_num:=fngethandle: "Name="&env$('Q')&"\PRmstr\Rpmstr.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\rpIndex.h"&str$(cno)&",Shr",internal,input,keyed ioerr ERTN
00110 ! ______________________________________________________________________
00120     restore #file_num: 
00130     fntos(sn$="CustomerSrch")
00140     ch$(1)="Employee" : ch$(2)="Name" : ch$(3)="Address" !:
          ch$(4)="City, ST Zip" !:
          ch$(5)="SS Number" : ch$(6)="Phone" !:
          mat ch$(6) : mat cm$(6)
00160     let usefile=0 ! if fixgrid=99 then let usefile=0 else let usefile=1 !:
          ! set to rebuild grid file only as you exit prfm and the !:
          ! fixgrid code has been changed to necessary
00170     let usefile=fnflexinit1('Employee',1,1,10,70,mat ch$,mat cm$,1,usefile)
00180     if usefile>0 then goto L280 ! file already exists, do not recreate
00190 READ_FILE: ! 
00200     read #file_num,using 'Form POS 1,c 8,3*c 30,pos 99,c 11,pos 179,c 12': mat item$ eof L280 ioerr ERR_READ
00210     fnflexadd1(mat item$)
00220     goto READ_FILE
00230 ! ______________________________________________________________________
00240 ERR_READ: ! 
00250     if err<>61 then goto ERTN
00260     pr 'Record locked during employee_search flexgrid creation'
00264     read #file_num,release: 
00266     goto READ_FILE
00270 ! ______________________________________________________________________
00280 L280: if fixgrid=99 then goto XIT ! FIXING NEW GRID FILE BEFORE LEAVING UBFM
00290     fncmdset(2)
00292     fnacs(sn$,0,mat resp$,ckey)
00300     let x$=lpad$(resp$(1)(1:8),8)
00310     if ckey=5 then let x$="        " ! no one selected
00320     goto XIT
00330 ! ______________________________________________________________________
00340 ! <Updateable Region: ERTN>
00350 ERTN: let fnerror(program$,err,line,act$,"xit")
00360     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00370     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00380     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00390 ERTN_EXEC_ACT: execute act$ : goto ERTN
00400 ! /region
00410 ! ______________________________________________________________________
00420 XIT: close #file_num: : fnend 
00430 ! ______________________________________________________________________
