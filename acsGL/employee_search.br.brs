00010 ! replace S:\acsGL\Employee_Search.br
00020 ! search for employees in after fact payroll
00030 ! ______________________________________________________________________
00040   def library fnemployee_search(&x$;fixgrid)
00050     library 'S:\Core\Library': fntos,fnflexinit1,fnflexadd1,fnacs,fncmdset,fnerror,fncno,fngethandle,fnlbl
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     dim item$(5)*30,resp$(30)*80
00090 ! ______________________________________________________________________
00100 ! x$=employee #     !:
          ! to extract the flexgrid information (master file)
00104     let fncno(cno)
00105     open #file_num:=fngethandle: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRINDEX.h"&str$(cno)&",Shr",internal,input,keyed ioerr ERTN
00110 ! ______________________________________________________________________
00120     restore #file_num: 
00130     let fntos(sn$="EmployeeSrch")
00140     let ch$(1)="Emp #" : let ch$(2)="Name" : let ch$(3)="Address" !:
          let ch$(4)="City, ST Zip" !:
          let ch$(5)="Social Security" !:
          mat ch$(5) : mat cm$(5) : mat cm$=("5")
00145     mat cm$=(""): let cm$(1)="" ! "n 4"
00160     if fixgrid=99 then let usefile=0 else let usefile=1 !:
            ! set to rebuild grid file only as you exit ubfm and the !:
            ! fixgrid code has been changed to necessary
00170     let usefile=fnflexinit1('Employee',1,1,10,70,mat ch$,mat cm$,1,usefile)
00180     if usefile>0 then goto L280 ! file already exists, do not recreate
00190 READ_FILE: ! 
00200     read #file_num,using 'Form POS 1,c 4,3*c 25,c 11': mat item$ eof L280 ioerr ERR_READ
00210     let fnflexadd1(mat item$)
00220     goto READ_FILE
00230 ! ______________________________________________________________________
00240 ERR_READ: ! 
00250     if err<>61 then goto ERTN
00260     print 'Record locked during Customer_Search flexgrid creation' !:
          print 'It was skipped' !:
          read #file_num,release: !:
          goto READ_FILE
00270 ! ______________________________________________________________________
00280 L280: if fixgrid=99 then goto XIT ! FIXING NEW GRID FILE BEFORE LEAVING UBFM
00285     let fnlbl(12,1,"")
00290     let fncmdset(2): let fnacs(sn$,0,mat resp$,ckey) !:
          ! CALL FLEXGRID
00300     let x$=lpad$(resp$(1),4)
00310     if ckey=5 then let x$="    " ! no one selected
00320     goto XIT
00330 ! ______________________________________________________________________
00340 ! <Updateable Region: ERTN>
00350 ERTN: let fnerror(program$,err,line,act$,"xit")
00360     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00370     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00380     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00390 ERTN_EXEC_ACT: execute act$ : goto ERTN
00400 ! /region
00410 ! ______________________________________________________________________
00420 XIT: close #file_num: : fnend 
00430 ! ______________________________________________________________________
