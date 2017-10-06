00010 ! formerly S:\acsGL\glSchFM
00020 ! Schedule File  (Maintenance routines) was Form POS 1,N 2,2*C 78,3*N  1,80*c 12  now Form POS 1,N 3,2*C 78,3*N  1  Breakdowns in seperate file
32000 ! r: setup, open files, library, set constants, on err, etc
32020   library 'S:\Core\Library': fntop,fnxit,fnerror,fntos,fnlbl,fncombof,fncmdkey,fnacs,fntxt,fnchk,fncomboa,fnflexinit1,fnflexadd1,fnhamster,fnmsgbox,fnFree
32040   on error goto ERTN
32060 ! ______________________________________________________________________
32080   dim gl$(80)*12
32100   dim sn$*78,ft$*78
32120   dim schnam$*78,ml$(3)*80
32140   dim option$(6)*60,item$(7)*80
32160   dim resp$(7)*80
32180 ! ______________________________________________________________________
32200   fntop(program$)
32220   gosub BUILD_LAYOUT
32240   if exists(env$('Q')&"\GLmstr\acglschs.h"&env$('cno'))=0 then 
32260     close #10: ioerr ignore
32280     open #10: "Name="&env$('Q')&"\GLmstr\ACGLSCHS.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\schindex.h"&env$('cno'),internal,outin,keyed ioerr ignore
32300     close #10,free: ioerr ignore
32320     CreateAcGlSchs: open #10: "Name="&env$('Q')&"\GLmstr\ACGLSCHS.h"&env$('cno')&",SIZE=0,RecL=162",internal,output 
32340     CloseAcGlSchs: close #10: ioerr ignore
32360     close #11: ioerr ignore
32380     gosub INDEX
32400   else if ~exists(env$('Q')&"\GLmstr\schindex.h"&env$('cno'))or ~exists(env$('Q')&"\GLmstr\schindx2.h"&env$('cno'))=0 then 
32420     gosub INDEX
32440   end if
32460   L210: !
32480   open #schedule:=10: "Name="&env$('Q')&"\GLmstr\ACGLSCHS.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\schindex.h"&env$('cno')&",Shr",internal,outin,keyed ioerr L1580
32500   open #11: "Name="&env$('Q')&"\GLmstr\ACGLSCHS.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\SchIndX2.h"&env$('cno')&",Shr",internal,outin,keyed ioerr CloseAcGlSchs
32520   goto SCHEDULEGRID
32540   close #10: ioerr ignore
32560   execute "Index "&env$('Q')&"\GLmstr\ACGLSCHS.h"&env$('cno')&' '&env$('Q')&"\GLmstr\SchIndX2.h"&env$('cno')&" 3 30 Replace DupKeys -n"
32580 goto L210 ! /r
44000 SCHEDULEGRID: ! r:
44020   fntos(sn$="Schedule") 
44040   respc=0
44060   mat chdr$(7) : mat cmask$(7) : mat flxitm$(7) 
44080   chdr$(1)="Rec" 
44100   chdr$(2)="Schedule #" : chdr$(3)="Schedule Name" 
44120   chdr$(4)="Footnote" : chdr$(5)="Dollar" 
44140   chdr$(6)="Reverse Sign" : chdr$(7)="Choice" 
44160   cmask$(2)='30' : cmask$(3)=cmask$(4)='' 
44180   cmask$(5)='30' : cmask$(6)='30' 
44200   cmask$(6)='30'
44220   fnflexinit1('schedulegl',lc=1,1,10,70,mat chdr$,mat cmask$,1)
44240   restore #10:
44260 READ_SCHEDULE: ! read schedule file
44280   read #schedule,using 'Form POS 1,N 3,2*C 78,3*N 1': sn,schnam$,ft$,dp,rs,cm eof EO_SCHEDULE_GRID norec L350
44300   item$(1)=str$(rec(schedule)) 
44320   item$(2)=str$(sn): item$(3)=schnam$: item$(4)=ft$ 
44340   item$(5)=str$(dp) : item$(6)=str$(rs) : item$(7)=str$(cm) 
44360   fnflexadd1(mat item$)
44380 L350: goto READ_SCHEDULE
44400 EO_SCHEDULE_GRID: ! 
44420   fncmdkey("&Add",1,0,0,"Allows you to add new schedules.")
44440 ! 
44460   fncmdkey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit to change any existing schedule.")
44480   fncmdkey("&Delete",8,0,0,"Highlight any record and click Delete to remove the schedule.")
44500 ! fnCMDKEY("&Print",3,0,0,"Takes you directly to the pr Schedules option")
44520   fncmdkey("E&xit",5,0,1,"Exits to main menu")
44540   fnacs(sn$,0,mat resp$,ckey)
44560   if ckey=5 then goto XIT
44580   add=edit=0
44600   editrec=val(resp$(1))
44620   if ckey=2 then edit=1
44640   if ckey=3 then chain "S:\acsGL\acglschp" ! prints prints a schedule
44660   if ckey=1 then 
44670     add=1 
44680     sn=dp=rs=cm=0 
44700     schnam$=ft$="" 
44720     goto ADD_EDIT_SCHEDULES ! add
44740   else if ckey=2 then 
44760     read #schedule,using 'Form POS 1,N 3,2*C 78,3*N 1',rec=editrec: sn,schnam$,ft$,dp,rs,cm norec SCHEDULEGRID 
44780     holdsn=sn 
44800     goto ADD_EDIT_SCHEDULES
44820   else if ckey=8 then 
44840     read #schedule,using 'Form POS 1,N 3,2*C 78,3*N 1',rec=editrec,release: sn,schnam$,ft$,dp,rs,cm norec SCHEDULEGRID 
44860     gosub DELETEIT 
44880     goto SCHEDULEGRID
44900   end if
44920   pause 
44940 ! /r
48000 ADD_EDIT_SCHEDULES: ! r:
48020   fntos(sn$="Schedule1") 
48040   mylen=20: mypos=mylen+3 : right=1
48060   fnlbl(1,1,"Schedule Number:",mylen,right)
48080   fncombof('glschedule',1,mypos,0,env$('Q')&"\GLmstr\acglschs.h"&env$('cno'),1,3,4,30,env$('Q')&"\GLmstr\schindex.h"&env$('cno'),add_all)
48100   if edit=1 then resp$(1)=str$(sn)
48120   if add=1 then resp$(1)=""
48140   fnlbl(2,1,"Schedule Nane::",mylen,right)
48160   fntxt(2,mypos,80,0,left,"",0,"",0 ) 
48180   resp$(2)=schnam$
48200   fnlbl(3,1,"Footnote:",mylen,right)
48220   fntxt(3,mypos,80,0,left,"",0,"",0 ) 
48240   resp$(3)=ft$
48260   fnchk(4,mypos,"Print Dollar Signs:",1) 
48280   if dp=1 then resp$(4)="True" else resp$(4)="False"
48300   fnchk(5,mypos,"Reverse Sign:",1) 
48320   if rs=1 then resp$(5)="True" else resp$(5)="False"
48340   fnlbl(6,1,"Type of Schedule:",mylen,right)
48360   option$(1)="Print Year to Date Only" 
48380   option$(2)="Print Current Month and Year to Date" 
48400   option$(3)="Print Comparison (Income and Expense Accounts" 
48420   option$(4)="Print Comparison (Balance Sheet Accounts"
48440   fncomboa("TypeOfPrint",6,mypos,mat option$,"You can choose any of the four types of schedules.",60)
48460   if cm=0 then cm=1
48480   resp$(6)=option$(cm)
48500   fncmdkey("&Display G/L #'s",1,1,0,"Allows you to review, add, or change the G/L accounts that are contained in this schedule.")
48520   fncmdkey("&Cancel",5,0,1,"Returns to list of schedules withouit saving any changes.")
48540   fnacs(sn$,0,mat resp$,ckey)
48560   if ckey=5 then goto SCHEDULEGRID
48580   sn=val(resp$(1)(1:3)) conv ADD_EDIT_SCHEDULES
48600   schnam$=resp$(2)
48620   ft$=resp$(3)
48640   if resp$(4)="True" then dp=1 else dp=0
48660   if resp$(5)="True" then rs=1 else rs=0
48680   for j=1 to j
48700     if resp$(6)=option$(j) then cm=j
48720   next j
48740   if edit=1 then goto REWRITE_EXISTING_SCHEDULE
48760   if add=1 then goto WRITE_NEW_SCHEDULE
48780   pause 
48800 ! /r
52000 REWRITE_EXISTING_SCHEDULE: ! r:
52020   if sn=0 then goto ADD_EDIT_SCHEDULES
52040   if holdsn<>sn and holdsn<>0 then 
52060     goto MSGBOX1 
52080   else 
52100     goto L950
52120   end if
52140   MSGBOX1: ! 
52160   mat ml$(3)
52180   ml$(1)="You are changing schedule # "&str$(holdsn)&" to " 
52200   ml$(2)="schedule # "&str$(sn)&".  Click OK to continue, " 
52220   ml$(3)="else Cancel to prevent changing the #." 
52240   fnmsgbox(mat ml$,resp$,'',49)
52260   if resp$="OK" then 
52280     execute "Copy "&env$('Q')&"\GLmstr\schedule"&str$(holdsn)&".h"&env$('cno')&' '&env$('Q')&"\GLmstr\schedule"&str$(sn)&".h"&env$('cno')&" -n" ioerr ignore ! move breakdowns to new schedule #
52300     L950: !
52320     rewrite #10,using L1010,rec=editrec: sn,schnam$,ft$,dp,rs,cm
52340     goto SCHEDULE_BREAKDOWN
52360   end if
52380 goto ADD_EDIT_SCHEDULES ! /r
54000 WRITE_NEW_SCHEDULE: ! r:
54020   write #10,using L1010: sn,schnam$,ft$,dp,rs,cm
54040   L1010: form pos 1,n 3,2*c 78,3*n 1
54060   new1=1
54080 goto SCHEDULE_BREAKDOWN ! /r
56000 INDEX: ! r: (main schedule files)
56020   execute "Index "&env$('Q')&"\GLmstr\ACGLSCHS.h"&env$('cno')&' '&env$('Q')&"\GLmstr\schindex.h"&env$('cno')&" 1 3 Replace DupKeys -n"
56040   execute "Index "&env$('Q')&"\GLmstr\ACGLSCHS.h"&env$('cno')&' '&env$('Q')&"\GLmstr\SchIndX2.h"&env$('cno')&" 4 30 Replace DupKeys -n"
56060 return ! /r
57000 INDEX2: ! r: index to gl breakdowns
57020   execute "Index "&env$('Q')&"\GLmstr\schedule"&str$(sn)&".H"&env$('cno')&' '&env$('Q')&"\GLmstr\schedule"&str$(sn)&"-idx.h"&env$('cno') &" 1 12 Replace,DupKeys" ioerr ignore
57040 return ! /r
58000 ! PROOF: ! r:
58020 !   restore #10,key>="  ": eof ignore ioerr ADD_EDIT_SCHEDULES
58040 !   ! win=102 
58060 !   ! message$="Printing: Please wait..." 
58080 !   ! fnwait(win,env$('program_caption'),message$,1)
58100 !   ! on fkey 5 goto L1530
58120 !   fnopenprn
58140 !   do
58160 !     read #10,using L1010: sn,schnam$,ft$,dp,rs,cm eof L1530
58180 !     pr #255,using L1250: date$('mm/dd/yy'),time$,"Print Schedules File Proof List"
58200 !     L1250: form skip 1,pos 1,c 8,skip 1,pos 1,c 8,pos 51,c 31,skip 1
58220 !     pr #255,using L1270: env$('cnam'),dat$
58240 !     L1270: form pos 1,cc 122,skip 1,pos 1,cc 122,skip 2
58260 !     pr #255,using L1290: "Schedule Number",sn
58280 !     L1290: form pos 1,c 15,pos 20,pic(zz),skip 1
58300 !     pr #255,using L1310: "Schedule Name  ",schnam$
58320 !     L1310: form pos 1,c 15,pos 20,c 80,skip 1
58340 !     pr #255,using L1310: "FootNote       ",ft$
58360 !     pr #255,using L1360: "Dollar Sign Print",dp
58380 !     pr #255,using L1360: "Reverse Sign",rs
58400 !     pr #255,using L1360: "Print Current Month Figures",cm
58420 !     L1360: form pos 1,c 27,pos 30,pic(#),skip 1
58440 !     pr #255: tab(29);"Dept  Account Sub"
58460 !     for j=1 to 80
58480 !       if gl$(j)="  0     0  0" then goto L1470
58500 !       if j1><48 then goto L1450
58520 !       pr #255: newpage
58540 !       pr #255,using L1430: "G/L Account Number",gl$(j)(1:3),gl$(j)(4:9),gl$(j)(10:12)
58560 !       L1430: form skip 6,pos 1,c 18,pos 30,c 3,x 2,c 6,x 2,c 3,skip 1
58580 !       goto L1470
58600 !       L1450: !
58620 !       pr #255,using L1460: "G/L Account Number",gl$(j)(1:3),gl$(j)(4:9),gl$(j)(10:12)
58640 !       L1460: form pos 1,c 18,pos 30,c 3,x 2,c 6,x 2,c 3,skip 1
58660 !       L1470: !
58680 !       j1=j1+1
58700 !     next j
58720 !     j1=0
58740 !     pr #255: newpage
58760 !   loop
58780 ! ! ______________________________________________________________________
58800 !   L1530: !
58820 !   fncloseprn
58840 !   on fkey 5 ignore 
58860 !   if fnprocess=1 then goto XIT
58880 ! goto ADD_EDIT_SCHEDULES ! /r
62000 XIT: fnxit
64000 L1580: if err=4152 then goto CreateAcGlSchs else goto ERTN
66000 ! <Updateable Region: ERTN>
66020 ERTN: fnerror(program$,err,line,act$,"xit")
66040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
66060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
66080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
66100 ERTN_EXEC_ACT: execute act$ : goto ERTN
66120 ! /region
68000 SCHEDULE_BREAKDOWN: ! r:
68020   ! general ledger breakdowns for each schedule
68040   ! ______________________________________________________________________
68060   dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),sp(1),c$(1,8)*40
68080   ! ______________________________________________________________________
68100   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE 
68120   fnhamster("schgl",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
68140   gosub FIXGLACCOUNTS
68160   gosub CLOSE_FILE
68180   gosub INDEX2
68200 goto SCHEDULEGRID ! /r
70000 OPEN_FILE: ! r:
70020   open_file_count=1 ! this value is used in the close_file sub routine
70040   close #open_file_count: ioerr ignore
70060   if exists(env$('Q')&"\GLmstr\Schedule"&str$(sn)&".h"&env$('cno'))=0 then 
70080     open #open_file_count: "Name="&env$('Q')&"\GLmstr\schedule"&str$(sn)&".h"&env$('cno')&",Version=1,Replace,RecL=12",internal,outin 
70100     gosub CLOSE_FILE
70120     gosub INDEX2
70140   else
70160     if exists(env$('Q')&"\GLmstr\schedule"&str$(sn)&"-idx.h"&env$('cno'))=0 then gosub INDEX2
70180     open #open_file_count: "Name="&env$('Q')&"\GLmstr\schedule"&str$(sn)&".H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\schedule"&str$(sn)&"-idx.H"&env$('cno')&",Shr",internal,outin,keyed 
70200   end if
70220 return ! /r
74000 FIXGLACCOUNTS: ! r: left pad general ledger number and reference number
74020   restore #open_file_count: 
74040   do
74060     read #open_file_count, using "form pos 1,c 12": gl$ eof L1990
74080     gl$=lpad$(rtrm$(gl$),12)
74100     rewrite #open_file_count, using "form pos 1,c 12": gl$
74120   loop
74140   L1990: !
74160 return ! /r
76000 CLOSE_FILE: ! r:
76020   for j=1 to open_file_count
76040     close #j: ioerr ignore
76060   next j
76080 return ! /r
78000 BUILD_LAYOUT: ! r:
78020   ! ** Field Labels    ** 
78040   ic=0 ! temporary Item Counter
78060   lbl$(ic+=1)="G/L Number"
78080 ! ** Text Box / Field Display   Lengths   ** 
78100   ic=0 ! temporary Item Counter 
78120   mmddyy=8 
78140   ccyymmdd=10
78160   tln(ic+=1)=12
78180 ! ** Field Types ** 
78200   ic=0
78220   fltyp$(ic+=1)='C'
78240 ! ** Field Storage Lengths ** 
78260   ic=0 
78280   mmddyy=6 : ccyymmdd=8
78300   sln(ic+=1)=12
78320 ! ** Field Masks ** 
78340   ic=0 
78360   pointtwo=32 : number=30 
78380   ccyymmdd=3 : mmddyy=1 : glnumber=53
78400   mask(ic+=1)=0
78420 ! ** Storage Positions ** 
78440   ! starting field position - default to the same as order displayed 
78460   ic=0
78480   sp(ic+=1)=1
78500 ! ** Combo Boxes **                                                   
78520   cl=1 : c$(cl,1)='ComboF' 
78540   c$(cl,2)=env$('Q')&"\GLmstr\GLmstr.h"&env$('cno') 
78560   c$(cl,3)="1" : c$(cl,4)="12" 
78580   c$(cl,5)="13": c$(cl,6)="40" 
78600   c$(cl,7)=env$('Q')&"\GLmstr\glindex.h"&env$('cno') 
78620   ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     
78640   limit_to_list$='1'
78660 ! ** Combo Boxes **                                                   
78680   ! cL=2 : c$(CL,1)='ComboF' 
78700   ! c$(CL,2)=env$('Q')&"\GLmstr\transcode.h"&env$('cno') 
78720   ! c$(CL,3)="1" : c$(CL,4)="2" 
78740   ! c$(CL,5)="3" : c$(CL,6)="30" 
78760   ! c$(CL,7)=env$('Q')&"\GLmstr\transcode-idx.h"&env$('cno') 
78780   ! c$(CL,8)="1" 
78800   ! lIMIT_TO_LIST$=('1'=yes' ; '0'=NO)
78820 return ! /r
82000 DELETEIT: !  r: delete a schedule
82020   mat ml$(3) 
82040   ml$(1)="You are attempting to delete schedule # "&str$(sn)&"." 
82060   ml$(2)="Click OK to continue, " 
82080   ml$(3)="else Cancel to prevent deleting the schedule." 
82100   fnmsgbox(mat ml$,resp$,'',49)
82120   if uprc$(resp$)="OK" then goto L2310 else goto ADD_EDIT_SCHEDULES
82140   L2310: delete #10,rec=editrec: 
82160   fnFree(env$('Q')&"\GLmstr\schedule"&str$(sn)&".h"&env$('cno'))
82180 return ! /r
