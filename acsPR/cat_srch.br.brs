00010 ! replace S:\acsPR\Cat_Srch.br
00020 ! search for an valid caterories for this job
00030 ! ______________________________________________________________________
00040   def library fncat_srch(&cn$;fixgrid)
00050     library 'S:\Core\Library': fnTos,fnflexinit1,fnflexadd1,fnAcs,fnCmdSet,fnerror,fncno,fngethandle
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     dim item$(4)*40,resp$(30)*80
00090 ! ______________________________________________________________________
00100     jn$=lpad$(rtrm$(cn$),6) ! pass job over in category #, but pass back the category
00110     fncno(cno)
00120     open #category:=fngethandle: "Name="&env$('Q')&"\PRmstr\Jccat.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\catindx.h"&env$('cno')&",Shr",internal,input,keyed ioerr ERTN
00130 ! ______________________________________________________________________
00140     restore #category: 
00150     fnTos(sn$="CatSrch")
00160     ch$(1)="Job & Category": ch$(2)="Category Name" : !:
          mat ch$(2) : mat cm$(2) : mat cm$=("2")
00170     if fixgrid=99 then usefile=0 else usefile=1 !:
            ! set to rebuild grid file only as you exit program and the !:
            ! fixgrid code has been changed to necessary
00180     usefile=fnflexinit1('Cat',1,1,10,70,mat ch$,mat cm$,1,usefile)
00190     if usefile>0 then goto L310 ! file already exists, do not recreate
00200     restore #category,key>=jn$&"     ": nokey L310
00210 READ_FILE: ! 
00220     read #category,using 'Form POS 1,c 11,c 25': mat item$ eof L310 ioerr ERR_READ
00230     if item$(1)(1:6)=jn$ then goto L240 else goto L310
00240 L240: fnflexadd1(mat item$)
00250     goto READ_FILE
00260 ! ______________________________________________________________________
00270 ERR_READ: ! 
00280     if err<>61 then goto ERTN
00290     pr 'Record locked during cat_search flexgrid creation' !:
          pr 'It was skipped' !:
          read #category,release: !:
          goto READ_FILE
00300 ! ______________________________________________________________________
00310 L310: ! If FIXGRID=99 Then Goto XIT ! FIXING NEW GRID FILE BEFORE LEAVING UBFM
00320     fnCmdSet(2): fnAcs(sn$,0,mat resp$,ckey) !:
          ! CALL FLEXGRID
00330     cn$=lpad$(resp$(1),11)
00340     if ckey=5 then cn$=cn$(1:6)&"     " ! no one selected
00350     goto XIT
00360 ! ______________________________________________________________________
00370 ! <Updateable Region: ERTN>
00380 ERTN: fnerror(program$,err,line,act$,"xit")
00390     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00400     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00410     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00420 ERTN_EXEC_ACT: execute act$ : goto ERTN
00430 ! /region
00440 ! ______________________________________________________________________
00450 XIT: close #category: : fnend 
00460 ! ______________________________________________________________________
