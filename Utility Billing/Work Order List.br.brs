00020   library program$: fnWorkOrderList
00025   library 'S:\Core\Library': fnxit,fntop
00030   fntop(program$,cap$="Work Order List")
00035 fnWorkOrderList('[All]')
00040 fnxit
00045 def fn_setup
00046   if ~setup then
00047     setup=1
00050     library 'S:\Core\Library': fnerror,fnopenprn,fncloseprn,fnAcs,fnTos
00052     library 'S:\Core\Library': fnLbl,fnTxt,fnmsgbox,fncomboa,fnButton,fnFra
00054     library 'S:\Core\Library': fncmbact
00056     library 'S:\Core\Library': fnCmdSet,fngethandle
00060     library 'S:\Core\Library': fncreg_read,fncreg_write
00065     library 'S:\Core\Library': fnureg_write,fnureg_read
00070     on error goto ERTN
00071     ! r: dims
00082     dim resp$(50)*320
00103     dim cap$*128,nam$*30,line$(5)*100
00155     ! /r
00250   end if
00260 fnend 
76000 ERTN: ! r:
76020   fnerror(program$,err,line,act$,"xit")
76040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
76100 ERTN_EXEC_ACT: execute act$ : goto ERTN
76120 ! /r
80000 def library fnWorkOrderList(; z$*10)
80010   if ~setup then let fn_setup
80020   fnTos(sn$="workorder")
80040   respc=0
80060   fnLbl(1,43," ",1,1)
80080   fnLbl(1,1,"Beginning Date to Review:",32,1)
80100   fnTxt(1,35,12,0,0,"3",0,"")
80120   resp$(respc+=1)=str$(beg_date)
80140   fnLbl(2,1,"Ending Date to Review:",32,1)
80160   fnTxt(2,35,12,0,0,"3",0,"")
80180   resp$(respc+=1)=str$(end_date)
80200   fnLbl(3,1,"Customer to Print:",32,1)
80220   fncmbact(3,35,1)
80240   resp$(respc+=1)=z$
80260   fnCmdSet(2)
80270   fnAcs(sn$,0,mat resp$,ck)
80280   if ck=5 then goto PWL_XIT
80300   beg_date=val(resp$(1)) ! beginning of year
80320   end_date=val(resp$(2)) ! ending day of year
80340   askz$=lpad$(trim$(resp$(3)(1:10)),10)
80360   fnopenprn
80370   open #h_workorder:=fngethandle: "Name=[Q]\UBmstr\WorkOrder.h[cno],KFName=[Q]\UBmstr\wkIndex.h[cno],Shr",internal,outIn,keyed
80380   gosub PWL_HDR
80400   if trim$(askz$)="[All]" or trim$(askz$)="" then 
80420     restore #h_workorder: 
80440   else
80460     restore #h_workorder,key>=askz$&"        ": nokey ignore
80480   end if
80500   do
80520     read #h_workorder,using "form pos 1,c 10,n 8,c 30,5*c 100": wkz$,wkdat,nam$,mat line$ eof PWL_FINIS
80530 ! pr wkz$,wkdat,nam$ : pause
80540     if trim$(askz$)="[All]" or trim$(askz$)="" or trim$(askz$)=trim$(wkz$) then 
80560       if beg_date=0 or wkdat=>beg_date then 
80580         if end_date=0 or wkdate<=end_date then
80600           pr #255,using "form pos 1,c 10,x 1,pic(zzzz/zz/zz),x 1,c 30": wkz$,wkdat,nam$
80620           for j=1 to 5
80640             if trim$(line$(j))<>"" then 
80660               pr #255,using 'form pos 5,c 100': line$(j) pageoflow PWL_PGOFLOW
80680             end if
80700           next j
80720         end if
80740       end if
80760     end if
80780   loop while trim$(askz$)=trim$(wkz$) or trim$(askz$)="[All]" or trim$(askz$)=""
80800 PWL_FINIS: ! 
80820   fncloseprn
80840 PWL_XIT: ! 
80860   pgno=0
80880   close #h_workorder: 
80900 fnend
82000 PWL_HDR: ! r:
82020   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
82040   pr #255: "\qc {\f181 {\fs24 {\b Work Order Listing}"
82060   pr #255: "{\fs20 "&env$('cnam')&"}}}"
82080   pr #255: "\qc {\fs18 From: "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&" To: "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
82100   pr #255: "\ql "
82120   return  ! /r
84000 PWL_PGOFLOW: ! r:
84020   pr #255: newpage
84040   gosub PWL_HDR
84060 continue  ! /r
