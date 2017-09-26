00010 ! replace S:\acsPR\CAT_SRCH2.br
00020 ! search for an valid caterories for this job
00030 ! ______________________________________________________________________
00040   def library fncat_srch2(&cn$,&ckey;fixgrid)
00050     library 'S:\Core\Library': fntos,fnflexinit1,fnflexadd1,fnacs,fncmdset,fnerror,fncno,fngethandle,fncmdkey
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     dim item$(2)*40,resp$(30)*80
00090 ! ______________________________________________________________________
00100     let jn$=lpad$(rtrm$(cn$),6) ! pass job over in category #, but pass back the category
00110     let fncno(cno)
00120     let category=16 ! category file in jcmaint
00130 ! ______________________________________________________________________
00140     let fntos(sn$="CatSrch")
00150     let ch$(1)="Job & Category": let ch$(2)="Category Name" : !:
          mat ch$(2) : mat cm$(2) : mat cm$=("2")
00160     let fnflexinit1('Cat',1,1,10,70,mat ch$,mat cm$,1,usefile)
00170     restore #category: 
00180 READ_FILE: ! 
00190     read #category,using 'Form POS 1,c 5,c 25': mat item$ eof L270 ioerr ERR_READ
00200     let fnflexadd1(mat item$)
00210     goto READ_FILE
00220 ! ______________________________________________________________________
00230 ERR_READ: ! 
00240     if err<>61 then goto ERTN
00250     print 'Record locked during cat_search flexgrid creation' !:
          print 'It was skipped' !:
          read #category,release: !:
          goto READ_FILE
00260 ! ______________________________________________________________________
00270 L270: ! If FIXGRID=99 Then Goto XIT ! FIXING NEW GRID FILE BEFORE LEAVING UBFM
00280     let fncmdkey("&Add",97,0,0,"Add a new category record." ) !:
          let fncmdkey("&Edit",98,0,0,"Review or change category breakdown record." ) !:
          let fncmdkey("Re&view Details",95,1,0,"Review detail transactions") !:
          let fncmdkey("&Delete",96,0,0,"Deletes the highlited record") !:
          let fncmdkey("&Refresh",7,0,0,"Updates search grids and combo boxes with new category information") !:
          let fncmdkey("D&uplicate",12,0,1,"Duplicates all Caterories from anouther existing job.") !:
          let fncmdkey("E&xit",6,0,1,"Returns to main screen.")
00290     let fnacs(sn$,0,mat resp$,ckey) !:
          ! CALL FLEXGRID
00300     let cn$=lpad$(resp$(1),11)
00310     if ckey=5 then let cn$=cn$(1:6)&"     " ! no one selected
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
00420 XIT: fnend 
00430 ! ______________________________________________________________________
