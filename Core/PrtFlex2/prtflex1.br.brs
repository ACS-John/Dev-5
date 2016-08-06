00010 ! Replace Core\PrtFlex\prtflex1
00020 ! ______________________________________________________________________
00030   library 'Core\Library': fnacs,fnlbl,fntxt,fntos,fnerror,fncomboa,fnflexadd1,fnflexinit1,fnxit,fncursys$,fngetdir,fncmdset,fncmdkey,fngetudf,fntop
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim programfolder$*60,datafolder$*60,udf$*256
00070   dim options$(300)*87,gridname$*40,gridindx$*40
00080   dim resp$(87)*80,txt$*40, cap$*128,message$*40,ln$*132
00090   dim fullgridname$*60,fullgridindx$*60,gridinfo$(5)*87,item$(80)*30
00100   dim name$*30,vname$*20,colmask$*3,tt$*200,colhdr$(80)*30,colmask$(80)*3
00110   dim open_read$*80,abbrev$*20,filename$*40
00120 ! ______________________________________________________________________
00130   let datafolder$=uprc$(trim$(fncursys$))&"mstr" !:
        let programfolder$=uprc$(trim$(fncursys$))&"mstr"
00140   let udf$=env$('temp')&'\'
00150   let fntop(program$(4:pos(program$,'.',-1)-1),cap$="Grids")
00160 SELECTDATABASE: ! !:
        !  allows you to search the grid folder for any subfolders !:
        ! (You must create a sub-folder for each data base you can !:
        !  access)
00170   mat resp$=("") : mat options$(300) : mat options$=("") : let j=0
00180   if exists(programfolder$&"\Grid")=0 then !:
          execute "MkDir "&programfolder$&"\Grid"
00190   execute "Dir "&programfolder$&"\grid\*.* >FlexWork.tmp" ! Ioerr 271
00200   open #13: "Name=FlexWork.tmp",display,input ioerr L270
00210 L210: linput #13: ln$ eof L260
00220   let x=pos(ln$,"<DIR>",1)
00230   if x>0 and ln$(1:1)<>"." then goto L240 else goto L210
00240 L240: let options$(j+=1)=ln$(46:len(trim$(ln$)))
00250   goto L210
00260 L260: close #13: 
00270 L270: if j<=0 then let j=1
00280   mat options$(j)
00290 ! currently you cannot have more than 8 character database names !:
        ! error 4323 avoided !:
        ! database folders must be created by the programmer before !:
        ! you can run this program !:
        ! database folders must be under the grid folder.
00300   let fntos(sn$="dataselect")
00310   let fnlbl(1,1,"Data Base Name:",16,1)
00320   let fnlbl(2,1,"Current System: "&fncursys$)
00330   let fncomboa("OTHER",1,18,mat options$,empty$,20) !:
        let resp$(1)=options$(1)
00340   let fncmdset(2) !:
        let fnacs(sn$,0,mat resp$,ckey)
00350   if ckey=5 then goto XIT
00360   let database$=resp$(1)
00370 ! ______________________________________________________________________
00380 GRIDSELECTION: ! !:
        ! Allows you to select any grid that previously been created !:
        ! or allows you to create a new one.  All grids end with !:
        !    .grd and will automatically be placed in database file !:
        ! that is selected at the time
00390   mat options$(100)
00400   mat options$=("") !:
        mat resp$=("")
00410 ! Let SN$ = "flexselect" !:
        ! Let FNtos(SN$)
00420   let j=0
00430 ! 
00440   let txt$=programfolder$&"\grid\"&database$ !:
        let filter$="*.grd" !:
        let fngetdir(txt$,mat options$,empty$,filter$)
00450   let txt$=programfolder$&"\grid\"&database$ !:
        let filter$="*.FIL" !:
        let fngetdir(txt$,optionfile$,empty$,filter$)
00460 ! Execute "Dir "&PROGRAMFOLDER$&"\grid\"&DATABASE$&"\*.* >flexwork."&WSID$ Ioerr 540
00470 ! Open #12: "Name="&UDF$&"flexwork2.tmp,RecL=30,Replace",Internal,Outin
00480 ! Open #13: "Name=FlexWork.tmp",Display,Input Ioerr 540
00490 ! Linput #13: LN$ Eof 540
00500 ! Let LN$=TRIM$(UPRC$(LN$)) !:
        ! If LN$(LEN(LN$)-3:LEN(LN$))=".FIL" Then !:
        ! Let OPTIONFILE$=LN$(46:LEN(LN$)) : Goto 450
00510 ! Let LN$=TRIM$(UPRC$(LN$)) !:
        ! If LN$(LEN(LN$)-3:LEN(LN$))<>".GRD" Then Goto 450
00520 ! Let J=J+1
00530 ! Let OPTIONS$(J)=LN$(46:LEN(TRIM$(LN$))-4)
00540 ! Let OPTIONS$(J)=TRIM$(OPTIONS$(J))
00550 ! Write #12,Using 520: OPTIONS$(J)(1:20)
00560 ! Form POS 1,C 20
00570 ! Goto 450
00580 ! If J<=0 Then Let J=1
00590   for j=1 to udim(options$)
00600     if options$(j)="" then goto L630
00610     let options$(j)=options$(j)(1:pos(options$(j),".")-1)
00620   next j
00630 L630: ! 
00640   mat options$(max(1,j-1))
00650 ! Close #12: Ioerr 570
00660 ! __
00670   let fntos(sn$="flexselect")
00680   let fnlbl(1,1,"Flexgrid name:",16,1)
00690   let tt$="Choose a grid or click add to add a new grid" !:
        let fncomboa("GridNames",1,18,mat options$,tt$,20) !:
        let resp$(1)=options$(1)
00700 ! Let FILENAME$="gridnames."&WSID$ !:
        ! let DATAFILE$="flexwork2."&WSID$ !:
        ! Let IndexFILE$="" !:
        ! Let TT$="Choose a grid or enter the name of a new grid" !:
        ! Let FNComboF(FILENAME$,1,18,20,DATAFILE$,0,0,1,20,IndexFILE$,1)!:
        ! if choose 2 item the answer is there twice  (sample           sample)
00710 ! For J=1 To UDIM(OPTIONS$) !:
        ! Let RESP$(J)=OPTIONS$(J)(1:20) !:
        ! Next J
00720   let fncmdkey("&Next",1,1) !:
        let fncmdkey("&Back",2) !:
        let fncmdkey("&Add Grid",3) !:
        let fncmdkey("&Delete Grid",4) !:
        let fncmdkey("&Cancel",5,0,1)
00730   let fnacs(sn$,0,mat resp$,ckey)
00740   let resp$(1)=trim$(resp$(1)(1:20))
00750   if ckey=5 then goto XIT
00760   if ckey=3 then goto ADDGRIDNAME
00770   if ckey=2 or trim$(resp$(1))="" then goto SELECTDATABASE ! back or no grid name
00780 L780: let fullgridname$=programfolder$&"\Grid\"&database$&"\"&resp$(1)&".grd"
00790   let fullgridindx$=programfolder$&"\grid\"&database$&"\"&resp$(1)&".idx"
00800   let gridname$=resp$(1) !:
        let open_read$=programfolder$&"\grid\"&database$&"\"&database$&"_info"
00810   if ckey=4 then close #15: ioerr L820
00820 L820: if ckey=4 then execute "free "&fullgridname$ ioerr SELECTDATABASE : execute "free "&fullgridindx$: goto SELECTDATABASE
00830   close #1: ioerr L840
00840 L840: if ckey=1 then goto GRIDCOLUMNS
00850 ! If CKEY=4 Then Goto DISPLAYGRID
00860   if ckey=5 then goto XIT
00870   goto SELECTDATABASE
00880 ! ______________________________________________________________________
00890 GRIDCOLUMNS: !  Displays all vaiables in the data base and allows you to                        choose the ones you want in your grid
00900   mat item$(2)
00910   close #15: ioerr L920
00920 L920: open #15: "Name="&fullgridname$&",KFName="&fullgridindx$&",RecL=80,KPs=1,KLn=3,use",internal,outin,keyed ioerr SELECTDATABASE
00930   let sn$="mstrflex" !:
        let fntos(sn$)
00940   let txt$=uprc$(gridname$) !:
        let fnlbl(1,1,txt$,20,2,3)
00950   mat colhdr$(2) !:
        let colhdr$(1)="Column #" !:
        let colhdr$(2)="Description"
00960   mat colmask$(2) !:
        let colmask$(1)="30" !:
        let colmask$(2)=""
00970   let filename$="flexreview"
00980   let fnflexinit1(filename$,2,1,10,72,mat colhdr$,mat colmask$,1)
00990   if lrec(15)=0 then goto DISPLAYOPTIONS
01000 L1000: read #15,using L1020: columnnum,name$,vname$,fieldlen,colmask$,abbrev$ eof L1050
01010   let item$(1)=str$(columnnum) !:
        let item$(2)=name$
01020 L1020: form pos 1,n 3,c 30,c 20,n 4,c 3,c 20
01030   let fnflexadd1(mat item$)
01040   goto L1000
01050 L1050: let fnlbl(12,1," ")
01052   let fncmdkey("&Add Column",1,1) !:
        let fncmdkey("&Delete Column",2) !:
        let fncmdkey("Display &Grid",3) !:
        let fncmdkey("&Back",4) !:
        let fncmdkey("&Cancel",5,0,1)
01060   let fnacs(sn$,0,mat resp$,ckey) ! CALL items selected
01070   if ckey=5 then goto XIT
01080   if ckey=1 then goto DISPLAYOPTIONS
01090   if ckey=3 then goto PRINTGRID
01100   if ckey=4 then goto SELECTDATABASE ! select a different grid
01110   if ckey=2 then let deletekey$=cnvrt$("n 3",val(resp$(1))): !:
          delete #15,key=deletekey$: ioerr GRIDCOLUMNS
01120   if ckey<>2 then goto L1190
01130   restore #15: : let newcolumn=0
01140 L1140: read #15,using L1020: columnnum eof L1180
01150   let newcolumn=newcolumn+1
01160   rewrite #15,using L1020: newcolumn
01170   goto L1140
01180 L1180: goto GRIDCOLUMNS
01190 L1190: goto SELECTDATABASE
01200 DISPLAYGRID: ! 
01210 DISPLAYOPTIONS: ! 
01220   restore #15: ! determine next available column number
01230 L1230: read #15,using L1020: lastcolumn eof L1250
01240   goto L1230
01250 L1250: let fntos(sn$="options") !:
        let respc=0
01260   let fnlbl(1,1,"Data Base File:",20,1)
01270   let fntxt(1,22,20,20,0,"",1) !:
        let gridinfo$(respc+=1)=database$
01280   let fnlbl(2,1,"Grid Name:",20,1)
01290   let fntxt(2,22,20,20,0,"",1) !:
        let gridinfo$(respc+=1)=gridname$
01300   let fnlbl(3,1,"Column Number:",20,1)
01310   let tt$="Change column # if default not acceptable" !:
        let fntxt(3,22,2,2,0,"30",0,tt$) !:
        let gridinfo$(respc+=1)=str$(lastcolumn+1)
01320   let fnlbl(5,1,"Grid Options:",14,1)
01330   let x=0
01340   mat options$(300)
01350   mat options$=("")
01360   close #16: ioerr L1370
01370 L1370: open #16: "Name="&programfolder$&"\grid\"&database$&"\"&optionfile$,display,input 
01380 L1380: linput #16: ln$ eof L1420
01390   let x=x+1
01400   let options$(x)=trim$(ln$)
01410   goto L1380
01420 L1420: mat options$(x)
01430   close #16: ioerr L1440
01440 L1440: let tt$="Highlite any column heading you wish to add to your grid" !:
        let fncomboa("Grrr",5,16,mat options$,tt$,80) !:
        let gridinfo$(respc+=1)=options$(1)
01450   let fncmdkey("&Add Column",1,1) !:
        let fncmdkey("&Finish",5,0,1)
01460   let fnacs(sn$,0,mat gridinfo$,ckey) ! data options available
01470   if ckey=1 then goto ADDTOGRID
01480   if ckey=5 then goto GRIDCOLUMNS
01490   goto GRIDCOLUMNS
01500 ADDTOGRID: ! add items to individual grids
01510   let columnnum=val(gridinfo$(3)(1:3)) !:
        let name$=gridinfo$(4)(1:30) !:
        let vname$=gridinfo$(4)(31:50) !:
        let fieldlen=val(gridinfo$(4)(51:54)) !:
        let maskinfo$=gridinfo$(4)(55:67) !:
        let abbrev$=trim$(gridinfo$(4)(68:87))
01520   let maskinfo$=uprc$(maskinfo$)
01530   let maskformat$=maskinfo$(3:4) ! determine if pd,c,n etc format
01540   let decimalposition=val(maskinfo$(1:2))
01550   let x=pos(uprc$(name$),"DATE",1) !:
        if x>0 then let itisadate$="Y" else let itisadate$="N"
01560   if trim$(maskformat$)="N" and fieldlen<8 and itisadate$="Y" then let colmask$="1" !:
          goto L1650 ! DATE IN MMDDYY FORMAT
01570   if trim$(maskformat$)="N" and fieldlen>7 and itisadate$="Y" then let colmask$="3"
01580   if trim$(maskformat$)="PD" and fieldlen>7 and itisadate$="Y" then let colmask$="3" !:
          goto L1650 ! DATE IN CCYYMMDD FORMAT
01590   if trim$(maskformat$)="PD" and fieldlen<8 and itisadate$="Y" then let colmask$="1" !:
          goto L1650 ! DATE IN MMDDYY FORMAT
01600   if trim$(maskformat$)="N" and decimalposition=2 and itisadate$="N" then let colmask$="10" !:
          goto L1650 ! AMERICAN CURRENCY
01610   if trim$(maskformat$)="N" and decimalposition>0 and itisadate$="N" then let colmask$=str$(30+decimalposition) !:
          goto L1650 ! NUMERIC WITH DECIMALS
01620   if trim$(maskformat$)="PD" and decimalposition>0 and itisadate$="N" then let colmask$="10" !:
          goto L1650 ! NUMERIC WITH DECIMALS
01630   if (trim$(maskformat$)="N" or trim$(maskformat$)="PD") and decimalposition=0 and itisadate$="N" then let colmask$=str$(30+decimalposition) !:
          goto L1650 ! NUMERIC WITH DECIMALS
01640   if (trim$(maskformat$)="C" or trim$(maskformat$)="G") then let colmask$="80" !:
          goto L1650 ! NORMAL CHARACTER
01650 L1650: read #15,using L1020,key=cnvrt$("pic(zzz)",val(gridinfo$(3))): oldcolumn nokey L1670 ! CHECK TO SEE IF ALREADY EXITS
01660   goto INSERTGRIDCOLUMN
01670 L1670: write #15,using L1020: columnnum,name$,vname$,fieldlen,colmask$,abbrev$
01680   goto DISPLAYGRID
01690 ! ______________________________________________________________________
01700 ERTN: let fnerror(cap$,err,line,act$,"xit")
01710   if uprc$(act$)<>"PAUSE" then goto L1740
01720   execute "list -"&str$(line) !:
        pause  !:
        goto L1740
01730   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause 
01740 L1740: execute act$
01750   goto ERTN
01760 ! ______________________________________________________________________
01770 INSERTGRIDCOLUMN: !  Renumbers the selected grid columns if one is deleted
01780   restore #15: 
01790 L1790: read #15,using L1020: lastcolumn eof L1810
01800   goto L1790
01810 L1810: read #15,using L1020,key=cnvrt$("pic(zzz)",lastcolumn): newcolumn nokey ADDTOGRID eof ADDTOGRID
01820   let newcolumn=newcolumn+1
01830   rewrite #15,using L1020: newcolumn
01840   let lastcolumn=lastcolumn-1
01850   if lastcolumn>0 and lastcolumn>=columnnum then goto L1810
01860   goto L1670 ! WRITE NEW RECORD
01870 ! ____________________________________________________________________
01880 PRINTGRID: ! Creates grid lines for prtflex2
01890   mat item$(80)
01900   mat item$=("")
01910   mat colhdr$(80)
01920   mat colhdr$=("")
01930   mat colmask$(80)
01940   mat colmask$=("")
01950   let columns=0
01960   let specline=10010 !:
        let dataline=10510
01970 ! ____________________________________________________________________
01980 ! This section generates the program lines needed to create the column            headings and column masks
01990   close #15: ioerr L2000
02000 L2000: open #15: "Name="&fullgridname$&",KFName="&fullgridindx$&",RecL=80,KPs=1,KLn=3,use",internal,outin,keyed ioerr SELECTDATABASE
02010   open #10: "Name="&udf$&"GridSpecs1.tmp,RecL=255,Replace",display,output  ! temporary file to hold generated lines for grid specifications
02020   print #10,using L2090: "procerr return" !:
        ! skip next line if no lines exist
02030   print #10,using L2090: "del 10010,10480" !:
        ! delete any lines from previous grid
02040   print #10,using L2090: "procerr return" !:
        ! skip next line if no lines exist
02050   print #10,using L2090: "del 10510,10980"
02060 L2060: read #15,using L1020: columnnum,name$,vname$,fieldlen,colmask$,abbrev$ eof L2160
02070   let columns=columns+1
02080   print #10,using L2090: str$(specline)& " Let ColHdr$("&str$(columns)&")="&'"'&trim$(abbrev$)&'"'&" !:COLMASK$("&str$(columns)&")="&'"'&trim$(colmask$)&'"'
02090 L2090: form pos 1,c 255
02100   let specline=specline+10
02110   let x=pos(vname$,"$",1): if x>0 then goto L2120 else goto L2130 !:
          ! determine if numeric or character
02120 L2120: print #10,using L2090: str$(dataline)& " LET ITEM$("&str$(columns)&")="&trim$(vname$) !:
        goto L2140
02130 L2130: print #10,using L2090: str$(dataline)& " LET ITEM$("&str$(columns)&")= str$("&trim$(vname$)&")"
02140 L2140: let dataline=dataline+10
02150   goto L2060
02160 L2160: close #10: ioerr L2180
02170   if columns=0 then goto DISPLAYOPTIONS
02180 L2180: open #10: "Name="&udf$&"GridSpecs2.tmp,RecL=255,Replace",display,output 
02190   print #10,using L2090: "PROC NOECHO"
02200   print #10,using L2090: "Load Core\PrtFlex\PrtFlex2"
02210   print #10,using L2090: "Load Core\PrtFlex\PrtFlex2"
02220   print #10,using L2090: "subproc "&udf$&"gridspecs1.tmp"
02230   print #10,using L2090: "SubProc "&open_read$
02240   print #10,using L2090: "00210 Let columns = "&str$(columns)
02250   print #10,using L2090: "Replace Core\PrtFlex\PrtFlex2"
02260   print #10,using L2090: 'chain "Core\PrtFlex\PrtFlex2"'
02270 ! Print #10,Using 1980: "PROC ECHO"
02280   close #10: 
02290   open #11: "Name="&udf$&"Gridname.tmp,RecL=80,Replace",internal,output,relative 
02300   write #11,using L2310: gridname$
02310 L2310: form pos 1,c 40
02320   close #11: 
02330 ! write fullgridname$,fullgridIndex,columns to file for prtflex2
02340   print newpage
02350   execute "proc "&udf$&"gridspecs2.tmp"
02360 ! ____________________________________________________________________
02370 ADDGRIDNAME: !  Allows you to add columns to your grid
02380   mat resp$=("")
02390   let sn$="addgrid" !:
        let fntos(sn$)
02400   let txt$="Grid Name:" !:
        let fnlbl(1,1,txt$,20,1)
02410   let gridinfo$(2)=gridname$ !:
        let tt$="Limited to 11 characters!" !:
        let fntxt(1,22,11,11,0,"",0,tt$)
02420   let fncmdset(2): let fnacs(sn$,0,mat resp$,ckey)
02430   if ckey=5 then goto GRIDSELECTION
02440   let resp$(1)=trim$(resp$(1))
02450   let resp$(1)=trim$(resp$(1))(1:20)
02460   close #15: ioerr L2470
02470 L2470: let ckey=1 : goto L780
02480 ! ____________________________________________________________________
02490 DONE: close #1: ioerr XIT
02500 XIT: let fnxit
