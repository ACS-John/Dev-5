00300   def library fndate_picker$ (;_date$,format$,row,column,___, window,days_in_week,gridspec$*255,usermonth,save_date$*8,baseyear)
00400     save_date$=_date$
00500     baseyear=val(env$('status.config.baseyear'))
00600     if baseyear <= 1900 then execute 'config baseyear 1930'
00700 ! 
00800     let format$=lwrc$(format$)
00900     date$("*mdcy")
01000     if val(_date$)=0 then 
01100       if format$='' then 
01200         let _date$=date$: let format$="mdcy"
01300       else 
01400         date$("*"&format$)
01500         let _date$=date$
01600         let _date$=date$(days(val(_date$),format$),'mdcy')
01700         date$("*mdcy")
01800       end if 
01900     else 
02000       if (lwrc$(_date$(1:1))='m' or lwrc$(_date$(1:1))='d') and len(format$)=8 then 
02100         let _date$=lpad$(_date$,8,"0")
02200       end if 
02300       if format$='' then let format$="mdcy"
02400       let _date$=date$(days(val(_date$),format$),'mdcy')
02500     end if 
02600 ! 
02700     if not row then row=1
02800     if not column then column=1
02900     dim forms$(1)*255
03000     days_in_week=7
03100     rows_on_grid=6
03200     open #window:=fngetfilenumber: "parent=none,srow="&str$(row)&",scol="&str$(column)&",rows=10,cols=25,caption=Please select a date,N=/#000066:#B0C4DE",display,outin 
03300     pr #window, fields "1,1,C 1,,B2500;1,6,C 1,,B2501;1,8,C 1,,B2502;1,15,C 1,,B2503": "<", ">", "<", ">"
03400     pr #window, fields "10,1,C 7,,B2504;10,18,C 7,,B2505": "OK", "Cancel"
03500     pr #window, fields "9,1,C 25": "Today: "&date$("d3 m3 dd, ccyy")
03600     month=val(_date$(1:2)) : let year=val(_date$(5:8))
03700     fngridform(mat headers$,mat widths,mat forms$,days_in_week)
03800     let gridspec$="2,1,grid "&str$(rows_on_grid+1)&"/27"
03900     pr #window, fields gridspec$&",headers,/W:#B0C4DE" : (mat headers$,mat widths,mat forms$)
04000     do 
04100       fnprintdays (_date$,window,gridspec$,days_in_week,rows_on_grid)
04200       let usermonth=month
04300       let useryear=year
04400       rinput #window, fields gridspec$&",cell,cur;1,3,N 02,AEX/#000066:#FFFFFF;1,10,N 4,AEX/#000066:#FFFFFF": day$, usermonth, useryear
04500       fnupdatemonthandyear (usermonth,useryear,month,year,fkey)
04600       if fkey=2504 then let fkey(201)
04700       if fkey=2505 then let fkey(99)
04800 ! 
04900       let _date$= lpad$(str$(month),2,'0')&day$&str$(year)
05000     loop until fkey=93 or fkey=99 or (fkey=201 and trim$(day$)<>'')
05100     if fkey=93 or fkey=99 then let _date$=save_date$
05200     close #window: 
05300     if not format$='mdcy' and not (fkey=93 or fkey=99) then 
05400       let _date$=date$(days(val(_date$),'mdcy'),format$)
05500     end if 
05600     let fkey(-1) ! reset fkey, so the calling program doesn't start reacting unexpectedly
05700     fndate_picker$=_date$
05800 DATE_PICKER_COMPLETE: ! 
05900     if baseyear <= 1900 then execute 'config baseyear '&str$(baseyear)
06000   fnend 
06200   def fnupdatemonthandyear (usermonth,useryear,&month,&year,_fkey;___)
06300     if usermonth < 1 or usermonth > 12 then 
06400       msgbox("Invalid month")
06500     else 
06600       month=usermonth
06700     end if 
06800     if useryear < 1900 or useryear > 2100 then 
06900       msgbox("Invalid year")
07000     else 
07100       let year=useryear
07200     end if 
07300     if _fkey=2500 then 
07400       month -= 1
07500       if month=0 then 
07600         month=12
07700         let year -=1
07800       end if 
07900     else if _fkey=2501 then 
08000       month += 1
08100       if month=13 then 
08200         month=1
08300         let year += 1
08400       end if 
08500     else if _fkey=2502 then 
08600       let year -= 1
08700     else if _fkey=2503 then 
08800       let year += 1
08900     end if 
09000   fnend 
09200   def fnprintdays (_date$,window,gridspec$,days_in_week,rows_on_grid;___,index_,offset,days_this_month,idx,year)
09300     mat days$(42)=("")
09400     month=val(_date$(1:2))
09500     let year=val(_date$(5:8))
09600     offset=fndayofweek(_date$(1:2)&'01'&_date$(5:8),days_in_week)
09700     days_this_month=fndaysinmonth(month,year)
09800     for rowindex_=1 + offset to days_this_month + offset
09900       days$(rowindex_)=lpad$(str$(idx:=idx+1),2,'0')
10000     next rowindex_
10100     pr #window, fields gridspec$&",=": mat days$
10200   fnend 
10400   def fngridform(mat headers$,mat widths,mat forms$,days_in_week;___,index_)
10500     mat headers$(days_in_week)=("")
10600     mat widths(days_in_week)=(0)
10700     mat forms$(days_in_week)=('')
10800     for index_=1 to days_in_week
10900       let widths(index_)=3
11000       let forms$(index_)="CC 2,/#000066:#FFFFFF"
11100     next index_
11200     headers$(1) ="Sun"
11300     headers$(2) ="Mon"
11400     headers$(3) ="Tue"
11500     headers$(4) ="Wed"
11600     headers$(5) ="Thu"
11700     headers$(6) ="Fri"
11800     headers$(7) ="Sat"
11900   fnend 
12100   def fndaysinmonth (month,year;___,daysinmonth)
12200     fndaysinmonth=date(days(date$(days(date$(str$(year)&lpad$(str$(month),2,"0")&"01"),"CCYYMMDD")+32,"CCYYMM01"),"CCYYMMDD")-1,"DD")
12300   fnend 
12500   def fndayofweek(_date$,days_in_week) ! 0=sunday,1=monday, etc
12600     fndayofweek=mod(days(_date$),days_in_week)
12700   fnend 
12900   def fngetfilenumber(;___, index_)
13000     index_=1
13100     do 
13200       if file(index_)=-1 then 
13300         let filenumber=index_
13400         exit do 
13500       else 
13600         index_ += 1
13700       end if 
13800     loop 
13900     fngetfilenumber=filenumber
14000   fnend 
14200 IGNORE: continue 
