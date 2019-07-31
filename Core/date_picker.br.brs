def library fndate_picker$ (;_date$,format$,row,column,___, window,days_in_week,gridspec$*255,usermonth,save_date$*8,baseyear)
	library 'S:\Core\Library': fnGetHandle
	save_date$=_date$
	baseyear=val(env$('status.config.baseyear'))
	if baseyear <= 1900 then execute 'config baseyear 1930'

	format$=lwrc$(format$)
	date$("*mdcy")
	if val(_date$)=0 then 
		if format$='' then 
			_date$=date$: format$="mdcy"
		else 
			date$("*"&format$)
			_date$=date$
			_date$=date$(days(val(_date$),format$),'mdcy')
			date$("*mdcy")
		end if 
	else 
		if (lwrc$(_date$(1:1))='m' or lwrc$(_date$(1:1))='d') and len(format$)=8 then 
			_date$=lpad$(_date$,8,"0")
		end if 
		if format$='' then format$="mdcy"
		_date$=date$(days(val(_date$),format$),'mdcy')
	end if 

	if not row then row=1
	if not column then column=1
	dim forms$(1)*255
	days_in_week=7
	rows_on_grid=6
	open #window:=fnGetHandle: "parent=none,srow="&str$(row)&",scol="&str$(column)&",rows=10,cols=25,caption=Date Selection,N=/#000066:#B0C4DE",display,outIn 
	pr #window, fields "1,1,C 1,,B2500;1,6,C 1,,B2501;1,8,C 1,,B2502;1,15,C 1,,B2503": "<", ">", "<", ">"
	pr #window, fields "10,1,C 7,,B2504;10,18,C 7,,B2505": "OK", "Cancel"
	pr #window, fields "9,1,C 25": "Today: "&date$("d3 m3 dd, ccyy")
	month=val(_date$(1:2)) : year=val(_date$(5:8))
	fn_gridForm(mat headers$,mat widths,mat forms$,days_in_week)
	gridspec$="2,1,grid "&str$(rows_on_grid+1)&"/27"
	pr #window, fields gridspec$&",headers,/W:#B0C4DE" : (mat headers$,mat widths,mat forms$)
	do 
		fnprintdays (_date$,window,gridspec$,days_in_week,rows_on_grid)
		usermonth=month
		useryear=year
		rinput #window, fields gridspec$&",cell,cur;1,3,N 02,AEX/#000066:#FFFFFF;1,10,N 4,AEX/#000066:#FFFFFF": day$, usermonth, useryear
		fnupdatemonthandyear (usermonth,useryear,month,year,fkey)
		if fkey=2504 then let fkey(201)
		if fkey=2505 then let fkey(99)

		_date$= lpad$(str$(month),2,'0')&day$&str$(year)
	loop until fkey=93 or fkey=99 or (fkey=201 and trim$(day$)<>'')
	if fkey=93 or fkey=99 then _date$=save_date$
	close #window: 
	if not format$='mdcy' and not (fkey=93 or fkey=99) then 
		_date$=date$(days(val(_date$),'mdcy'),format$)
	end if 
	fkey(-1) ! reset fkey, so the calling program doesn't start reacting unexpectedly
	fndate_picker$=_date$
	DATE_PICKER_COMPLETE: ! 
	if baseyear <= 1900 then execute 'config baseyear '&str$(baseyear)
fnend 
def fnupdatemonthandyear (usermonth,useryear,&month,&year,_fkey;___)
	if usermonth < 1 or usermonth > 12 then 
		msgbox("Invalid month")
	else 
		month=usermonth
	end if 
	if useryear < 1900 or useryear > 2100 then 
		msgbox("Invalid year")
	else 
		year=useryear
	end if 
	if _fkey=2500 then 
		month -= 1
		if month=0 then 
			month=12
			year -=1
		end if 
	else if _fkey=2501 then 
		month += 1
		if month=13 then 
			month=1
			year += 1
		end if 
	else if _fkey=2502 then 
		year -= 1
	else if _fkey=2503 then 
		year += 1
	end if 
fnend 
def fnprintdays(_date$,window,gridspec$,days_in_week,rows_on_grid;___,index_,offset,days_this_month,idx,year)
	mat days$(42)=("")
	month=val(_date$(1:2))
	year=val(_date$(5:8))
	offset=fn_dayOfWeek(_date$(1:2)&'01'&_date$(5:8),days_in_week)
	days_this_month=fn_daysInMonth(month,year)
	for rowindex_=1 + offset to days_this_month + offset
		days$(rowindex_)=lpad$(str$(idx:=idx+1),2,'0')
	next rowindex_
	pr #window, fields gridspec$&",=": mat days$
fnend 
def fn_gridForm(mat headers$,mat widths,mat forms$,days_in_week;___,index_)
	mat headers$(days_in_week)=("")
	mat widths(days_in_week)=(0)
	mat forms$(days_in_week)=('')
	for index_=1 to days_in_week
		widths(index_)=3
		forms$(index_)="CC 2,/#000066:#FFFFFF"
	next index_
	headers$(1) ="Sun"
	headers$(2) ="Mon"
	headers$(3) ="Tue"
	headers$(4) ="Wed"
	headers$(5) ="Thu"
	headers$(6) ="Fri"
	headers$(7) ="Sat"
fnend 
def fn_daysInMonth (month,year;___,daysinmonth)
	fn_daysInMonth=date(days(date$(days(date$(str$(year)&lpad$(str$(month),2,"0")&"01"),"CCYYMMDD")+32,"CCYYMM01"),"CCYYMMDD")-1,"DD")
fnend 
def fn_dayOfWeek(_date$,days_in_week) ! 0=sunday,1=monday, etc
	fn_dayOfWeek=mod(days(_date$),days_in_week)
fnend 

