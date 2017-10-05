00010 ! Replace S:\Core\PrtFlex\prtflex1
00020 ! ______________________________________________________________________
10000   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fntos,fnerror,fncomboa,fnflexadd1,fnflexinit1,fnxit,fncursys$,fngetdir,fncmdset,fncmdkey,fntop,fngetdir2,fnCopy
10020   on error goto ERTN
10040   fntop(program$,cap$="Grids")
12000 ! r: dims
12020   dim programfolder$*256,datafolder$*256
12040   dim gridname$*40,gridindx$*40,filename$*60
12060   dim resp$(87)*80,txt$*40, cap$*128,message$*40,ln$*132
12080   dim fullgridname$*60,fullgridindx$*60,gridinfo$(5)*87,item$(80)*30
12100   dim name$*30,vname$*20,colmask$*3,tt$*200,colhdr$(80)*30,colmask$(80)*3
12120   dim open_read$*80,abbrev$*20,filename$(10)*40
12140 ! /r
14000   datafolder$=env$('Q')&'\'&uprc$(trim$(fncursys$))&"mstr" ! data folder grid folder is for use and updating by customer
14020   programfolder$=os_filename$('S:\acs'&uprc$(trim$(fncursys$))) ! program grid folder (and sub-folder and files) are for distribution.  files and folders only distribute if they are missing.  makes updating them difficult.
16000 ! r: make any missing folders in the data directory
16020   if ~exists(datafolder$&"\Grid") then 
16040     execute 'MkDir "'&datafolder$&'\Grid"'
16060   end if 
16080   dim tmp_directory_list$(1)*256
16100   fngetdir2(programfolder$&'\Grid\',mat tmp_directory_list$, '/ad')
16120   for tmp_directory_list_item=1 to udim(mat tmp_directory_list$)
16140     if ~exists(datafolder$&'\Grid\'&tmp_directory_list$(tmp_directory_list_item)) then 
16160       execute 'MkDir "'&datafolder$&"\Grid\"&tmp_directory_list$(tmp_directory_list_item)&'"'
16180     end if 
16200   next tmp_directory_list_item
16220 ! /r
16240 ! r: copy any missing files to the data folder
16260   dim tmp_file_list$(1)*256
16280   fngetdir2(programfolder$&'\Grid\',mat tmp_file_list$, '/s /b','*.*')
16320   for tmp_file_list_item=1 to udim(mat tmp_file_list$)
16322 !       pr '1'; tmp_file_list$(tmp_file_list_item)
16330     tmp_file_list$(tmp_file_list_item)(1:len(programfolder$))='' ! remove the program folder prefix from it - keep \Grid\...
16332 !      pr '2'; tmp_file_list$(tmp_file_list_item)
16340     if ~exists(datafolder$&tmp_file_list$(tmp_file_list_item)) then 
16342 !      pr '3'; 'copy "'&programfolder$&tmp_file_list$(tmp_file_list_item)&'" "'&datafolder$&tmp_file_list$(tmp_file_list_item)&'"' : pause
16360       execute 'copy "'&programfolder$&tmp_file_list$(tmp_file_list_item)&'" "'&datafolder$&tmp_file_list$(tmp_file_list_item)&'"'
16380     end if 
16400   next tmp_file_list_item
16420 ! /r
18000 SELECTDATABASE: ! 
18020 !  allows you to search the grid folder for any subfolders
18040 ! (You must create a sub-folder for each data base you can
18060 !  access)
18080   dim database_list$(1)*256
20470   fngetdir2(datafolder$&'\Grid\',mat database_list$)
20680 ! currently you cannot have more than 8 character database names
20700 ! error 4323 avoided
20720 ! database folders must be created by the programmer before
20740 ! you can run this program
20760 ! database folders must be under the grid folder.
20780   fntos(sn$="dataselect")
20790   mat resp$=("")
20800   fnlbl(1,1,"Data Base Name:",16,1)
20820   fnlbl(2,1,"Current System: "&fncursys$)
20840   fncomboa("OTHER",1,18,mat database_list$,empty$,20)
20860   resp$(1)=database_list$(1)
20880   fncmdset(2)
20900   fnacs(sn$,0,mat resp$,ckey)
20920   if ckey=5 then goto XIT
20940   database$=resp$(1)
20960 ! ______________________________________________________________________
20980 GRIDSELECTION: ! r:
21000 ! Allows you to select any grid that previously been created
21020 ! or allows you to create a new one.  All grids end with
21040 !    .grd and will automatically be placed in database file
21060 ! that is selected at the time
21120   mat resp$=("")
21130   dim gridname_list$(300)*87
21140   fngetdir2(datafolder$&"\Grid\"&database$ ,mat gridname_list$, '','*.grd') ! ,mat gd2_date$,mat gd2_time$,gd2_full_path)
21150   for gridname_list_item=1 to udim(mat gridname_list$) ! remove all the .grd extensions
21160     gridname_list$(gridname_list_item)=gridname_list$(gridname_list_item)(1:len(gridname_list$(gridname_list_item))-4)
21170   next gridname_list_item
21200   fntos(sn$="flexselect")
21220   fnlbl(1,1,"Flexgrid name:",16,1)
21880   fncomboa("GridNames",1,18,mat gridname_list$,"Choose a grid or click add to add a new grid",20)
21900   resp$(1)=gridname_list$(1)
22100   fncmdkey("&Select",1,1)
22120 ! fncmdkey("&Back",2)
22140   fncmdkey("&Add",3)
22160   fncmdkey("&Delete",4)
22180   fncmdkey("&Cancel",5,0,1)
22200   fnacs(sn$,0,mat resp$,ckey)
22220   resp$(1)=trim$(resp$(1)(1:20))
22240   if ckey=5 then goto SELECTDATABASE
22260   if ckey=3 then goto ADDGRIDNAME
22280 ! if ckey=2 or trim$(resp$(1))="" then goto SELECTDATABASE ! back or no grid name
22300 L780: fullgridname$=datafolder$&"\Grid\"&database$&"\"&resp$(1)&".grd"
22320   fullgridindx$=datafolder$&"\Grid\"&database$&"\"&resp$(1)&".idx"
22340   gridname$=resp$(1)
22360   open_read$=datafolder$&"\Grid\"&database$&"\"&database$&"_info"
22380   if ckey=4 then 
22390     close #hgridfile: ioerr ignore
22400     execute 'free "'&fullgridname$&'" -n' ioerr SELECTDATABASE
22402     execute 'free "'&fullgridindx$&'"'
22403     goto SELECTDATABASE
22404   end if 
22420   close #1: ioerr ignore
22440   if ckey=1 then goto GRIDCOLUMNS
22460 ! If CKEY=4 Then Goto DISPLAYGRID
22480   if ckey=5 then goto XIT
22500   goto SELECTDATABASE ! /r
22520 ! ______________________________________________________________________
22540 GRIDCOLUMNS: !  r: Displays all vaiables in the data base and allows you to                        choose the ones you want in your grid
22560   mat item$(2)
22580   close #hgridfile: ioerr ignore
22600   open #hgridfile:=15: "Name="&fullgridname$&",KFName="&fullgridindx$&",RecL=80,KPs=1,KLn=3,use",internal,outin,keyed ioerr SELECTDATABASE
22620   sn$="mstrflex"
22640   fntos(sn$)
22660   txt$=uprc$(gridname$)
22680   fnlbl(1,1,txt$,20,2,3)
22700   mat colhdr$(2)
22720   colhdr$(1)="Column #"
22740   colhdr$(2)="Description"
22760   mat colmask$(2)
22780   colmask$(1)="30"
22800   colmask$(2)=""
22820   filename$="flexreview"
22840   fnflexinit1(filename$,2,1,10,72,mat colhdr$,mat colmask$,1)
22860   if lrec(hgridfile)=0 then goto DISPLAYOPTIONS
22870   do 
22880     read #hgridfile,using fGridFile: columnnum,name$,vname$,fieldlen,colmask$,abbrev$ eof L1050
22900     item$(1)=str$(columnnum)
22920     item$(2)=name$
22940     fGridFile: form pos 1,n 3,c 30,c 20,n 4,c 3,c 20
22960     fnflexadd1(mat item$)
22980   loop 
23000 L1050: fncmdkey("&Add Column",1,1)
23020   fncmdkey("&Delete Column",2)
23040   fncmdkey("Display &Grid",3)
23060   fncmdkey("&Back",4)
23080   fncmdkey("&Cancel",5,0,1)
23100   fnacs(sn$,0,mat resp$,ckey) ! CALL items selected
23120   if ckey=5 then goto XIT
23140   if ckey=1 then goto DISPLAYOPTIONS
23160   if ckey=3 then goto PRINTGRID
23180   if ckey=4 then goto SELECTDATABASE ! select a different grid
23200   if ckey=2 then 
23210     deletekey$=cnvrt$("n 3",val(resp$(1)))
23220     delete #hgridfile,key=deletekey$: ioerr GRIDCOLUMNS
23240   end if 
23260   if ckey<>2 then goto SELECTDATABASE
23280   restore #hgridfile: : newcolumn=0
23300   do
23310     read #hgridfile,using fGridFile: columnnum eof GRIDCOLUMNS
23320     newcolumn=newcolumn+1
23340     rewrite #hgridfile,using fGridFile: newcolumn
23360   loop
23420 DISPLAYGRID: ! 
23440 DISPLAYOPTIONS: ! 
23460   restore #hgridfile: ! determine next available column number
23480 L1230: read #hgridfile,using fGridFile: lastcolumn eof L1250
23500   goto L1230
23520 L1250: fntos(sn$="options")
23540   respc=0
23560   fnlbl(1,1,"Data Base File:",20,1)
23580   fntxt(1,22,20,20,0,"",1)
23600   gridinfo$(respc+=1)=database$
23620   fnlbl(2,1,"Grid Name:",20,1)
23640   fntxt(2,22,20,20,0,"",1)
23660   gridinfo$(respc+=1)=gridname$
23680   fnlbl(3,1,"Column Number:",20,1)
23700   tt$="Change column # if default not acceptable"
23720   fntxt(3,22,2,2,0,"30",0,tt$)
23740   gridinfo$(respc+=1)=str$(lastcolumn+1)
23760   fnlbl(5,1,"Grid Options:",14,1)
23780   x=0
23800   mat gridname_list$(300)
23820   mat gridname_list$=("")
23840   close #16: ioerr ignore
23860   open #16: 'Name=S:\acs'&uprc$(trim$(fncursys$))&"\Grid\"&database$&"\"&database$&'.fil',display,input 
23880 L1380: linput #16: ln$ eof L1420
23900   x=x+1
23920   gridname_list$(x)=trim$(ln$)
23940   goto L1380
23960 L1420: ! 
23980   mat gridname_list$(x)
24000   close #16: ioerr ignore
24020   tt$="Highlite any column heading you wish to add to your grid"
24040   fncomboa("Grrr",5,16,mat gridname_list$,tt$,80)
24060   mat gridinfo$(respc+=1)
24062   if udim(mat gridname_list$)=>1 then gridinfo$(respc)=gridname_list$(1) else gridinfo$(respc)=''
24080   fncmdkey("&Add Column",1,1)
24100   fncmdkey("&Finish",5,0,1)
24120   fnacs(sn$,0,mat gridinfo$,ckey) ! data options available
24140   if ckey=1 then goto ADDTOGRID
24160   if ckey=5 then goto GRIDCOLUMNS
24180   goto GRIDCOLUMNS
24200 ADDTOGRID: ! add items to individual grids
24220   columnnum=val(gridinfo$(3)(1:3))
24240   name$=gridinfo$(4)(1:30)
24260   vname$=gridinfo$(4)(31:50)
24280   fieldlen=val(gridinfo$(4)(51:54))
24300   maskinfo$=gridinfo$(4)(55:67)
24320   abbrev$=trim$(gridinfo$(4)(68:87))
24340   maskinfo$=uprc$(maskinfo$)
24360   maskformat$=maskinfo$(3:4) ! determine if pd,c,n etc format
24380   decimalposition=val(maskinfo$(1:2))
24400   x=pos(uprc$(name$),"DATE",1)
24420   if x>0 then itisadate$="Y" else itisadate$="N"
24440   if trim$(maskformat$)="N" and fieldlen<8 and itisadate$="Y" then 
24460     colmask$="1"
24480     goto L1650 ! DATE IN MMDDYY FORMAT
24500   end if 
24520   if trim$(maskformat$)="N" and fieldlen>7 and itisadate$="Y" then 
24540     colmask$="3"
24560   end if 
24580   if trim$(maskformat$)="PD" and fieldlen>7 and itisadate$="Y" then 
24600     colmask$="3"
24620     goto L1650 ! DATE IN CCYYMMDD FORMAT
24640   end if 
24660   if trim$(maskformat$)="PD" and fieldlen<8 and itisadate$="Y" then 
24680     colmask$="1"
24700     goto L1650 ! DATE IN MMDDYY FORMAT
24720   end if 
24740   if trim$(maskformat$)="N" and decimalposition=2 and itisadate$="N" then 
24760     colmask$="10"
24780     goto L1650 ! AMERICAN CURRENCY
24800   end if 
24820   if trim$(maskformat$)="N" and decimalposition>0 and itisadate$="N" then 
24840     colmask$=str$(30+decimalposition)
24860     goto L1650 ! NUMERIC WITH DECIMALS
24880   end if 
24900   if trim$(maskformat$)="PD" and decimalposition>0 and itisadate$="N" then 
24920     colmask$="10"
24940     goto L1650 ! NUMERIC WITH DECIMALS
24960   end if 
24980   if (trim$(maskformat$)="N" or trim$(maskformat$)="PD") and decimalposition=0 and itisadate$="N" then 
25000     colmask$=str$(30+decimalposition)
25020     goto L1650 ! NUMERIC WITH DECIMALS
25040   end if 
25060   if (trim$(maskformat$)="C" or trim$(maskformat$)="G") then 
25080     colmask$="80"
25100     goto L1650 ! NORMAL CHARACTER
25120   end if 
25140 L1650: ! 
25160   read #hgridfile,using fGridFile,key=cnvrt$("pic(zzz)",val(gridinfo$(3))): oldcolumn nokey WRITE_NEW_RECORD ! CHECK TO SEE IF ALREADY EXITS
25180   goto INSERTGRIDCOLUMN
25200 WRITE_NEW_RECORD: !
25220   write #hgridfile,using fGridFile: columnnum,name$,vname$,fieldlen,colmask$,abbrev$
25240   goto DISPLAYGRID
25260 ! /r
25262 IGNORE: continue 
25280 ! <updateable region: ertn>
25300 ERTN: fnerror(program$,err,line,act$,"xit")
25320   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
25340   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT 
25360   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
25380 ERTN_EXEC_ACT: execute act$ : goto ERTN
25400 ! </updateable region: ertn>
25420 INSERTGRIDCOLUMN: ! r: Renumbers the selected grid columns if one is deleted
25430   read #hgridfile,using fGridFile,last: lastcolumn eof ignore
25440   ! restore #hgridfile: 
25460   ! do
25470   !   read #hgridfile,using fGridFile: lastcolumn eof L1810
25480   ! loop
25500   L1810: !
25510   read #hgridfile,using fGridFile,key=cnvrt$("pic(zzz)",lastcolumn): newcolumn nokey ADDTOGRID eof ADDTOGRID
25520   newcolumn=newcolumn+1
25540   rewrite #hgridfile,using fGridFile: newcolumn
25560   lastcolumn=lastcolumn-1
25580   if lastcolumn>0 and lastcolumn>=columnnum then goto L1810
25600   goto WRITE_NEW_RECORD ! WRITE NEW RECORD
25620 ! /r
25640 PRINTGRID: ! r: Creates grid lines for prtflex2
25660   mat item$(80)
25680   mat item$=("")
25700   mat colhdr$(80)
25720   mat colhdr$=("")
25740   mat colmask$(80)
25760   mat colmask$=("")
25780   columns=0
25800   specline=10010
25820   dataline=10510
25840 ! ____________________________________________________________________
25860 ! This section generates the program lines needed to create the column            headings and column masks
25880   close #hgridfile: ioerr ignore
25900   open #hgridfile:=15: "Name="&fullgridname$&",KFName="&fullgridindx$&",RecL=80,KPs=1,KLn=3,use",internal,outin,keyed ioerr SELECTDATABASE
25920   open #h_gridspecs1:=10: "Name="&env$('temp')&"\GridSpecs1.tmp,RecL=255,Replace",display,output  ! temporary file to hold generated lines for grid specifications
25940   pr #h_gridspecs1,using F_GRIDSPECS1: "procerr return" ! skip next line if no lines exist
25960   pr #h_gridspecs1,using F_GRIDSPECS1: "del 10010,10480" ! delete any lines from previous grid
25980   pr #h_gridspecs1,using F_GRIDSPECS1: "procerr return" ! skip next line if no lines exist
26000   pr #h_gridspecs1,using F_GRIDSPECS1: "del 10510,10980"
26020   do
26040     read #hgridfile,using fGridFile: columnnum,name$,vname$,fieldlen,colmask$,abbrev$ eof L2160
26050     if vname$(1:1)='(' then vname$(1:1)=''
26060     columns=columns+1
26080     pr #h_gridspecs1,using F_GRIDSPECS1: str$(specline)& " colHdr$("&str$(columns)&")="&'"'&trim$(abbrev$)&'"'&" : ColMask$("&str$(columns)&")="&'"'&trim$(colmask$)&'"'
26100     F_GRIDSPECS1: form pos 1,c 255
26120     specline=specline+10
26140     if pos(vname$,"$",1) then  ! determine if numeric or character
26180       pr #h_gridspecs1,using F_GRIDSPECS1: str$(dataline)& " item$("&str$(columns)&")="&trim$(vname$)
26200     else 
26240       pr #h_gridspecs1,using F_GRIDSPECS1: str$(dataline)& " item$("&str$(columns)&")= str$("&trim$(vname$)&")"
26260     end if
26280     dataline=dataline+10
26300   loop
26320   L2160: ! 
26340   close #h_gridspecs1: ioerr L2180
26360   if columns=0 then goto DISPLAYOPTIONS
26380 L2180: ! 
26382   dim new_prtflex2_name$*512
26384   new_prtflex2_name$=env$('temp')&'\PrtFlex2_'&session$&'.br'
26386   fnCopy("S:\Core\PrtFlex\PrtFlex2.br",new_prtflex2_name$)
26400   open #h_gridspecs2:=10: "Name="&env$('temp')&"\GridSpecs2.tmp,RecL=255,Replace",display,output 
26420   pr #h_gridspecs2,using F_GRIDSPECS1: "PROC NOECHO"
26440   pr #h_gridspecs2,using F_GRIDSPECS1: "Load "&new_prtflex2_name$ ! S:\Core\PrtFlex\PrtFlex2"
26460   pr #h_gridspecs2,using F_GRIDSPECS1: "Load "&new_prtflex2_name$ ! S:\Core\PrtFlex\PrtFlex2"
26480   pr #h_gridspecs2,using F_GRIDSPECS1: "subproc "&env$('temp')&"\gridspecs1.tmp"
26500   pr #h_gridspecs2,using F_GRIDSPECS1: "SubProc "&open_read$
26520   pr #h_gridspecs2,using F_GRIDSPECS1: "00210 columns = "&str$(columns)
26540   pr #h_gridspecs2,using F_GRIDSPECS1: "Replace "&new_prtflex2_name$ ! S:\Core\PrtFlex\PrtFlex2"
26560   pr #h_gridspecs2,using F_GRIDSPECS1: 'chain "'&new_prtflex2_name$&'"' ! S:\Core\PrtFlex\PrtFlex2"'
26580 ! pr #h_gridspecs2,Using 1980: "PROC ECHO"
26600   close #h_gridspecs2: 
26620   open #h_gridname:=11: "Name="&env$('temp')&"\Gridname.tmp,RecL=80,Replace",internal,output,relative 
26640   write #h_gridname,using 'form pos 1,c 40': gridname$
26680   close #h_gridname: 
26700 ! write fullgridname$,fullgridIndex,columns to file for prtflex2
26720 ! pr NEWPAGE
26740   execute "proc "&env$('temp')&"\gridspecs2.tmp"
26760 ! /r
26780 ADDGRIDNAME: ! r: Allows you to add columns to your grid
26800   mat resp$=("")
26820   sn$="addgrid"
26840   fntos(sn$)
26880   fnlbl(1,1,"Grid Name:" ,20,1)
26900   gridinfo$(2)=gridname$
26940   fntxt(1,22,11,11,0,"",0,"Limited to 11 characters!" )
26960   fncmdset(2)
26962   fnacs(sn$,0,mat resp$,ckey)
26980   if ckey=5 then goto GRIDSELECTION
27000   resp$(1)=trim$(resp$(1))
27020   resp$(1)=trim$(resp$(1))(1:20)
27040   close #hgridfile: ioerr ignore
27060   ckey=1 : goto L780
27080 ! /r
27100 DONE: close #1: ioerr ignore
27120 XIT: fnxit
