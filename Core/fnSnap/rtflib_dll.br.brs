! REPLACE RTFLIB_dll.br
! r: unsure - test zone probably
	library "RTFLIB_dll.br": fnrtf,fnamt$,fntext$
	dim types$(4)*2,styles$(4)*1000,data$(20)*1200,sub$(1)*1000
	open #rtffile=126: "name=c:\temp.rtf,recl=1000,replace",display,output
	types$(1)="H"
	types$(2)="F"
	types$(3)="D"
	types$(4)="T"
	styles$(1)="li0|ri0|fs18|cfBlue|tc3.25|Header"
	styles$(2)="li0|ri0|fs8|cfRed|tc3.25|Footer"
	styles$(3)="li0.25|ri0|fs10||tl2.5|td3|tl3.2|td4.0|tl4.2|td4.6|tl4.8|td5.4|Data"
	mask$="pic(ZZZ,ZZ#)"
	styles$(4)="li0.5|ri0|fs10|tl2.5|td3|tl3.2|td4.0|tl4.2|td4.6|tl4.8|td5.4|Totals"
	data$(1)="H|\b\tab Title Of Report"
	data$(2)="F|\tab Page Footer"
	data$(3)="D|Description |"&fnamt$(1000,mask$,"$")&"|"&fnamt$(2000,mask$,"$")&"|"&fnamt$(3000,mask$,"$")&"|"&fnamt$(4000,mask$,"$")
	for a=4 to 17
		if mod(a,2) then data$(a)="D|Description |"&fnamt$(1000,mask$)&"|"&fnamt$(2000,mask$)&"|"&fnamt$(3000,mask$)&"|"&fnamt$(4000,mask$) else data$(a)="D|Description |"&fnamt$(-1000,mask$)&"|"&fnamt$(-2000,mask$)&"|"&fnamt$(-3000,mask$)&"|"&fnamt$(-4000,mask$)
	next a
 data$(18)="D|"&fntext$("Description that is longer than the allowed size of the data space and needs to be out on multiple lines",30)&"|"&fnamt$(1000,mask$," ","s")&"|"&fnamt$(2000,mask$," ","s")&"|"&fnamt$(3000,mask$," ","s")&"|"&fnamt$(4000,mask$," ","s")
 data$(19)="T|Total|"&fnamt$(18000,mask$,"$","d")&"|"&fnamt$(36000,mask$,"$","d")&"|\cfRed "&fnamt$(54000,mask$,"$","d")&"|"&fnamt$(72000,mask$,"$","d")
 fnrtf(mat types$,mat styles$,mat data$,rtffile)
	dim rtffile$*250
 rtffile$=file$(rtffile)
	close #rtffile:
	execute "sys "&env$("PD")&"spoolbat.bat "&rtffile$&" WORD"
stop ! /r
! INIT: ! r: unreferenced

	datfmt$="MM-DD-CCYY"
	maxsrows=22
	ssav=103
	windev=owindev=69
	mga$="24,2,c 78,"
	pfk=23
	! Common Variables Almost Always Required By Fnsnap
	pgup=90 : pgdn=91 : event=98
	esc=99 : up=102 : left=103
	dn=104 : home=112
	end=113
	click=201 : dblclick=202
	help=100 : rtclick=100
	rtdblclick=100
	upfld=105 : dnfld=106 : foflow=107
	right=109 : left=108 : home=112
	end=113 : fldplus=114 : fldminus=115
return ! /r
	def library fnextract(winno,spec$;header$*200,footer$*200,title$*200,mat select,nolines,nosort,nototals$*200,subtotal)

		! alls FNLISTPRINT.  The function name was changed after it was
		! reated.  This function maintains compatability

		library env$("PD")&"Core\fnsnap\rtflib_dll.br": fnlistprint
		fnlistprint(winno,spec$,header$,footer$,title$,mat select,nolines,nosort,nototals$,subtotal)
	fnend

	def library fnlistprint(winno,spec$*100;header$*200,footer$*200,title$*200,mat selected,nolines,nosort,nototals$*200,nosubtotal,print)

		! Extract information from a listbox and pr t information
		!   in an RTF report using Word
		! WINNO t window number in which t listbox appears
		! SPEC$ t identifier such as "1,1 LIST 5/25" for t list
		! HEADER$ t text to use as a page header on each page
		! FOOTER$ t text to use as a page footer in additon to page #
		!    and report date
		! MAT SELECTED t separate array if any used to flag selected
		!    elements
		! NOLINES if true suppresses lines around cells
		! NOSORT if true ignores resorted sequence
		! NOTOTALS$ string of 0/1/Xs 1 will surpress total pr
		!    X will suppress t printing of t column
		! NOSUBTOTAL if true prevents printing of subtotals

		library 'S:\Core\Library': fnget_wordprocessor_exe
		fnget_wordprocessor_exe(word_processor_exe$, 'word')
		dim mprint$*50,word_processor_exe$*256
		if pr then mprint$=" /q /n /mFilePrintDefault /mFileCloseOrExit" else mprint$=""
		library env$("PD")&"Core\fnsnap\RTFLIB_dll.br": fnrtf,fnamt$,fntext$
		library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnmsexe$,fnh,fndelrow,fndelrow$,fn_cs,fncopyc2s,fncopys2c,fnwaitwin
		dim theaders$(1)*50,headers$(1)*50,widths(1),forms$(1)*50,aa$(1)*50,ab$(1)*50,ba$(1)*50,bb$(1)*50,cc$(1)*3000,lastcc$(1)*3000,ccs$(1)*3000,ccst$(1)*3000,ccs$*2000,ccb$*2000,headers$*2000,cc$*20000,dorder(1),sorder(1),exclude(1),outfile$*100,dummy$*2000
		execute 'proc '&env$("PD")&'Core\fnsnap\tt'
		_cs=fn_cs
		dim textfile$*100,rtfout$*100
		input #winno, fields spec$&",rowcnt,sel,nowait ": selno
		if (selno>1 or sum(mat selected)>1) and msgbox("Do you ONLY want to include selected items in your report?","List Report",yn$,"QST")=2 then : _
			selected=1 : _
		else selected=0
		input #winno, fields spec$&",MASK, nowait": mat mask_array
		if sum(mat mask_array)>0 then masked=1 else masked=0

		! Read t Header arrays


		input #winno, fields spec$&",HEADERS,,nowait ": (mat headers$,mat widths,mat forms$)
		x=udim(mat forms$)
		for a=1 to x
			forms$(a)=trim$(forms$(a))
		next a
		nototals$=rpad$(uprc$(nototals$),x,"0")

		! Get t order in which records are displayed
		! Get t order in which t columns were sorted

		input #winno, fields spec$&",rowsub,all,displayed_order,nowait": mat dorder
		input #winno, fields spec$&",sort_order,all,nowait": mat sort_order

		! Get t order in which columns have been sorted and displayed
		

		for a=1 to udim(mat sort_order) : sort_order(a)=abs(sort_order(a)) : next a
		x=udim(dorder)
		mat sorder(x)
		for a=1 to x : sorder(a)=a : next a
		if nosort then mat dorder=sorder
		exclude=0

		! Determine if any columns are zero width or any headers are
		! blank or flagged to be excluded by an X in t nototals$
		! variable.  These will not be included in t report

		input #winno, fields spec$&",colcnt,all,nowait": lcols
		for a=lcols to 1 step -1
			if widths(a)<=0 or trim$(headers$(a))<="" or uprc$(nototals$(a:a))="X" then
				exclude+=1 : mat exclude(exclude)
				exclude(exclude)=a
			end if
		next a

		! Remove any excluded columns


		if exclude then for a=1 to exclude : fndelrow$(mat headers$,exclude(a)) : next a
		if exclude then for a=1 to exclude : fndelrow(mat widths,exclude(a)) : next a
		if exclude then for a=1 to exclude : fndelrow$(mat forms$,exclude(a)) : next a
		if exclude then for a=1 to exclude : fndelrow(mat sort_order,exclude(a)) : next a
		if sum(mat sort_order)>0 then : _
			sort_col=srch(mat sort_order,1) else : _
			sort_col=0 
				
			! et a flag to indicate what column was last chosen for sort   
			! equence.  This will be used for sub-total breaks             

		lcols=udim(mat headers$) 
		mat cc$(lcols)=("") 
		mat col_format(lcols)=(0) 
		mat col_total(lcols)=(0) 
		mat col_subtotal(lcols)=(0)
		gosub SET_FORMAT
 
		! nly selected rows are to pr                         


		if selected or masked then
 
			! etermine if external selected array or Windows selection     
			! as used - MAT SELECTED is external array                     

			x=udim(mat selected)
			if not x or not sum(mat selected) then
				mat asel(0)
				input #winno, fields spec$&",rowcnt,sel,nowait": asel : _
				if asel>1 then mat asel(asel) : _
					input #winno, fields spec$&",rowsub,sel,nowait": mat asel
				if (masked and not sum(asel)) then
					am=0
					mat asel(sum(mat mask_array))
					for a=1 to udim(mask_array)
						if mask_array(a)>0 then : _
							am+=1 : _
							asel(am)=a
					next a
				end if
			else
				asel=sum(selected) : _
				mat asel(asel) : _
				as=0
				for a=1 to x
					if selected(dorder(a))>0 then as+=1 : _
						asel(as)=dorder(a)
				next a
			end if
		end if
		aa=0
		waitwin=fnwaitwin("Creating RTF Report - Please wait")
 : _
		! tart writing RTF text file                                   : _


		open #(textfile:=fnh): "name=[Temp]\text[Session].txt,recl=5000,replace",display,output
		if sort_col>0 then header$(inf:inf)="[RTFLINE] Sorted by "&headers$(sort_col)
		print #textfile: "H|[SPEC(spec[Session].spc)]"&header$&"[BOTLINE]" : _
		print #textfile: "F|"&footer$&"|Page [PAGE] |[RTFDATE][TOPLINE]" : _
		print #textfile: "T| "&title$&"[RTFLINE]"
		mat theaders$(udim(headers$)) : _
		mat theaders$=headers$
		if sort_col>0 and nototals$(sort_col:sort_col)="2" then fndelrow$(mat theaders$,sort_col)
		mat2str(mat theaders$,headers$," | ")
		print #textfile: "1| "&headers$
_
			: _
		! rite detail lines to RTF text file                           : _


START_SELECTED: !
		aa+=1
		if selected+masked and aa>udim(mat dorder) then mat cc$(0) : goto L10580 else : _
			if not selected+masked and aa>udim(dorder) then mat cc$(0) : goto L10580
 : _
		! nly selected rows will pr                                 : _


		if selected+masked then
			if srch(mat asel,dorder(aa))>0 then
				mat start(1): mat end(1) : mat start=(dorder(aa)) : mat end=start : _
				input #winno, fields spec$&",row,range,nowait": mat start,mat end,(mat cc$)
			else
				goto START_SELECTED
			end if
		end if
!
 : _
		! emove excluded columns from detail                           : _


		if not selected+masked then : _
			mat start(1): mat end(1) : mat start=(dorder(aa)) : mat end=(dorder(aa)) : _
			input #winno, fields spec$&",row,range,nowait": mat start,mat end,(mat cc$) : _
			mat ccl$(udim(mat cc$))
 : _
		! ll rows will pr                                           : _


		if exclude then for a=exclude to 1 step -1 : fndelrow$(mat cc$,exclude(a)) : next a
 : _
		! ormat cells for each column                                  : _


		for aaa=1 to udim(mat cc$)
 : _
			! heck numeric columns and add to column total if no conversion
			! rror.  If a conversion error occurs mark t column as string
			! o that no total will appear and no further conversion testing
			! ill be done                                                  : _

			if aaa<=udim(col_format) then
				if not col_format(aaa) then : _
					numeric=val(srep$(trim$(cc$(aaa)),"$","")) conv SET_NUM
 : _
				! ormat t numeric column if conversion works                 : _


				if not col_format(aaa) and trim$(hforms$(aaa))>"" then
					cc$(aaa)=cnvrt$(hforms$(aaa),val(srep$(trim$(cc$(aaa)),"$",""))) error ignore
				end if
 : _
				! ormat dates                                                  : _


				if col_format(aaa) and trim$(hforms$(aaa))(1:4)="DATE" then execute "LET CC$(AAA)=date$(val(TRIM$(cc$(aaa))),"&srep$(srep$(hforms$(aaa),'DATE(','"'),')','")')
 : _
				! ouble up \ to \\ so that t RTF reader will not mistake it  : _
				! s an RTF command                                             : _

				cc$(aaa)=srep$(trim$(cc$(aaa)),"\","\\") ! pause
			end if
		next aaa
PRINT_DETAIL: !
			: _
		! rint t detail elements to t report                       : _


		mat2str(mat cc$,cc$," | ") : _
		mat lastcc$(udim(cc$)) : _
		mat dummy$(udim(c$))
		if sort_col>0 and sort_count>1 and not cc$(sort_col)=lastcc$(sort_col) then
			gosub PRINT_SUBTOTALS
		else
			if not nosort and sort_col>0 and not cc$(sort_col)=lastcc$(sort_col) then
				sort_count=0 : _
				mat col_subtotal=(0)
				if nototals$(sort_col:sort_col)="2" then : _
					print #textfile: "5| "&cc$(sort_col) else : _
					print #textfile: "5| |"
			end if
		end if
		for a=1 to lcols
			if not col_format(a) then : _
				col_total(a)+=val(srep$(cc$(a),"$","")) : _
				col_subtotal(a)+=val(srep$(cc$(a),"$",""))
		next a
		mat lastcc$(udim(cc$)) : mat lastcc$=cc$
		if nototals$(sort_col:sort_col)="2" then : _
			fndelrow$(mat cc$,sort_col) : _
			mat2str(mat cc$,cc$," | ") else : _
			mat2str(mat cc$,cc$," | ")
		if udim(mat cc$)>0 then print #textfile: "2| "&cc$&"|" : _
			cc$="" : _
			mat cc$(udim(lastcc$)) : _
			mat cc$=("") : _
			sort_count+=1
 : _
		! hen t end of a list is reached t row extraction          : _
		! DIM's to zero                                                : _

L10580: if udim(mat cc$)>0 then : _
			goto START_SELECTED
 : _
		! eset t CC$ array to t correct number of columns          : _


		mat cc$(lcols): mat lastcc$(lcols)
		if sort_col>0 and sort_count>1 and not cc$(sort_col)=lastcc$(sort_col) then : _
			gosub PRINT_SUBTOTALS else : _
			if sort_col>0 and not cc$(sort_col)=lastcc$(sort_col) then : _
				sort_count=0 : _
				col_subtotal=0
!
		for aa=1 to udim(col_total)
 : _
			! lear non-numeric column totals                               : _


			if col_format(aa)>0 or val(nototals$(aa:aa))>0 then
				cc$(aa)=""
			else
				cc$(aa)=str$(col_total(aa))

				! ormat numeric column totals to match data elements


				if trim$(hforms$(aa))>"" then
					cc$(aa)=cnvrt$(hforms$(aa),col_total(aa)) error ignore
				end if
			end if
		next aa
		mat cct$(udim(cc$)) : _
		mat cct$=cc$ : _
		if sort_col>0 and nototals$(sort_col:sort_col)="2" then fndelrow$(mat cct$,sort_col)
		mat2str(mat cct$,cc$," | ")
		print #textfile: "3| "&cc$&"|"
		mat col_total=(0) : _
		sort_count=0
		textfile$=file$(textfile)
		close #textfile:
		open #textfile: "name="&textfile$,display,input
		_seq=-1
L10690: _seq+=1: open #(rtfout:=fnh): "name=[Temp]\temp"&session$&str$(_seq)&".rtf,eol=none,replace",display,output ioerr L10690
		rtfout$=file$(rtfout)
! close #waitwin:! PAUSE
		gosub BUILD_SPEC
		fnrtf(textfile,env$("temp")&"\spec[Session].spc",rtfout,env$("temp")&"\")
		if file(textfile)>-1 then close #textfile:
		if file(rtfout)>-1 then close #rtfout:
		if _cs then fncopys2c(rtfout$,outfile$:="temp\temp[Session].rtf",1)
		close #waitwin: ! PAUSE
		if close=1 then mw$=" -M" else mw$=""
		dim word$*150
		if _cs then
			execute "config shell default client "
			word$=fnmsexe$("winword.exe")
			execute "sys -w"&mw$&" "&word$&" "&os_filename$("@:"&outfile$)&mprint$
			execute "config shell default server"
		else
			execute "sys -w"&mw$&" -c "&word_processor_exe$&" "&os_filename$(rtfout$)&mprint$ !        execute "sys -w"&mw$&" -c "&fnmsexe$("winword.exe")&" "&os_filename$(rtfout$)&mprint$
		end if
	fnend

SET_NUM: ! r:
		: _
	! dd 1 to t col_format array to indicate it is NOT a numeric : _
	! rray.  Numeric arrays have a zero col_format number          : _

 col_format(aaa)+=1
continue ! /r

BUILD_SPEC: ! r: build an RTF specification file

	! uild an RTF specification file in t %TEMP% directory based
	! n t number of columns and text of t lIST to print.

	open #(specfile=fnh): "name=[Temp]\spec[Session].spc,recl=2000,replace",display,output
	print #specfile: ""

	! et margins


 widths=sum(mat widths)
	print #specfile: "let LMARGIN=.50"
	print #specfile: "let RMARGIN=0.75 "

	! et page orientation


	if widths>100 then : _
		print #specfile: "let ORIENTATION$='LANDSCAPE'" : landscape=1 else : _
		print #specfile: "let ORIENTATION$='PORTRAIT'" : landscape=0

	! ight add-in to end of line if style code is E and checklist
	! s true

	print #specfile: "let LEFTTEXT$='Y   N   N/A '"
	print #specfile: "let CHECKLIST=1"
	print #specfile: "let PAPER$='LETTER'"

	! f NUME is true t sequential E styles will automatically
	! e numbered

	print #specfile: "let NUME=0"

	! reate RTF style codes


	print #specfile: "MAT TYPES$(11)"
	print #specfile: "LET TYPES$(1)='H'"
	print #specfile: "LET TYPES$(2)='F'"
	print #specfile: "LET TYPES$(3)='D'"
	print #specfile: "LET TYPES$(4)='T'"
	print #specfile: "LET TYPES$(5)='A'"
	print #specfile: "LET TYPES$(6)='B'"
	print #specfile: "LET TYPES$(7)='C'"
	print #specfile: "LET TYPES$(8)='E'"
	print #specfile: "LET TYPES$(9)='G'"
	print #specfile: "LET TYPES$(10)='Y'"
	print #specfile: "LET TYPES$(11)='I'"

	! reate details of RTF styles


	print #specfile: "MAT STYLES$(11)"
	if widths>100 then : _
		print #specfile: "LET STYLES$(1)='li0|ri0|fARIAL|fs14|cfBlue|tc5.00|Header'" else : _
		print #specfile: "LET STYLES$(1)='li0|ri0|fARIAL|fs14|cfBlue|tc3.25|Header'"
	if widths>100 then : _
		print #specfile: "LET STYLES$(2)='li0|ri0|fARIAL|fs8|cfBlack|tc5.00|tr10.0|Footer'" else : _
		print #specfile: "LET STYLES$(2)='li0|ri0|fARIAL|fs8|cfBlack|tc3.25|tr7.5|Footer'"
	print #specfile: "LET STYLES$(3)='li0.5|QJ|fPALATINO|ri0|fs10|tl0.5|tl1.0|tl1.5|td5.4||Data'"
	print #specfile: "LET STYLES$(4)='li0.5|QC|fARIAL|sa1|ri0|B|fs15|tl0.5||tc3.25|Heading 1'"
	print #specfile: "LET STYLES$(5)='li0.25|ri0|fARIAL|B|fs14|tl0.5||tr5.4|Heading 2'"
	print #specfile: "LET STYLES$(6)='li0.25|ri0|fARIAL|B|fs13|tl0.5||tr5.4|Heading 3'"
	print #specfile: "LET STYLES$(7)='li0.25|ri0|fARIAL|B|fs12|tl0.5||td5.4|Heading 4'"
	print #specfile: "LET STYLES$(8)='fi-0.5|td0.75|li1.0|ri0|fPALATINO|fs09|tl0.5|tl1.0|tc7.0|Yes no Lines'"
	print #specfile: "LET STYLES$(9)='fi-0.4|li1.0|ri0|ft61|fs10|fCOURIER|tl0.5|tc4.0|td5.4|Program lines'"
	print #specfile: "LET STYLES$(10)='fi-0.5|td0.75|li1.0|ri0|fPALATINO|fs12|tl0.5|tl1.0|td6.0|Detail steps'"
	print #specfile: "LET STYLES$(11)='fi-1.25|li2.0|ri0|fPALATINO|fs12|tl2.0|Options'"
 : _
	! repare justification codes                                   : _
	!  right justify numeric columns                               : _
	!  center justify date columns                                 : _
	!  left justify string columns                                 : _

	mat hj$(udim(mat headers$))
 : _
	! repare cell row types                                        : _
	!  header row will repeat on each page                         : _
	!  detail rows within t table                                : _
	!  total row at t bottom of t table                        : _
	!  sub-total row                                               : _

 : _
	! eader row                                                    : _


	print #specfile: "mat cells$(5)"
 : _
	! et smaller font sizes for wider reports                      : _


	if widths>180 then fs$="04" : fst$="06" else : _
		if widths>120 then fs$="06": fst$="08" else : _
			fs$="08": fst$='10'
 : _
	! emove grid lines if NOLINES is true                          : _


	if nolines then : _
		br$="br1" : brt$="brt1|brbd1": brst$="brt1|brb1" else : _
		br$="brltrb1" : brt$="brltr1|brbd2": brst$="brltr1|brb1"
	if sort_col>0 and nototals$(sort_col:sort_col)="2" then : _
		delcol=widths(sort_col) else : _
		delcol=0
	print #specfile: "LET CELLS$(1)='li0.1|tg0.100|fPALATINO|fs"&fs$&"|trh|'"
	if landscape then page_width=10 else page_width=7.5
	for a=1 to udim(mat headers$)
		if not (sort_col=a and nototals$(a:a)="2") then print #specfile: "let cells$(1)(inf:inf)='c"&str$((widths(a)/(widths-delcol))*page_width)&"|brtlrb1|vc|hc|sh15|'"
	next a
 : _
	! etail row                                                    : _


	print #specfile: "let cells$(2)='li0.1|tg0.100|fPALATINO|fs"&fs$&"|'"
	for a=1 to udim(mat headers$)
		if pos(uprc$(forms$(a)),"PIC(")>0 or pos(uprc$(forms$(a)),"FMT(")>0 or uprc$(forms$(a)(1:2))="N " or col_format(a)=0 then : _
			hj$(a)="hr" : _
		else if pos(uprc$(forms$(a)),"DATE")>0 then hj$(a)="hc" : _
		else hj$(a)="hl"
	if not (sort_col=a and nototals$(a:a)="2") then print #specfile: "let cells$(2)(inf:inf)='c"&str$((widths(a)/(widths-delcol))*page_width)&"|"&br$&"|vt|"&hj$(a)&"|'"
next a
! PAUSE
 : _
! otal row                                                     : _

 !
print #specfile: "LET CELLS$(3)='li0.1|tg0.100|fPALATINO|fs"&fs$&"|'"
let brdr$="br1" : _
let brdr$(1)="brtlr1" : _
let brdr$(2)="brbd1"
for a=1 to udim(mat headers$)
	if hj$(a)='hr' and not col_format(a) and not pos("12",nototals$(a:a)) then
		if not (sort_col=a and nototals$(a:a)="2") then print #specfile: "let cells$(3)(inf:inf)='c"&str$((widths(a)/(widths-delcol))*page_width)&"|"&brt$&"|vt|"&hj$(a)&"|'"
	else
		if not (sort_col=a and nototals$(a:a)="2") then print #specfile: "let cells$(3)(inf:inf)='c"&str$((widths(a)/(widths-delcol))*page_width)&"|"&brdr$&"|vt|"&hj$(a)&"|'"
	end if
next a
! CLOSE #SPECFILE:
 : _
! ub-total row                                                 : _

 !
print #specfile: "LET CELLS$(4)='li0.1|tg0.100|fPALATINO|fs"&fs$&"|'"
let brdr$="br1" : _
let brdr$(1)="brtlr1" : _
let brdr$(2)="brb1"
for a=1 to udim(mat headers$)
	if hj$(a)='hr' and not col_format(a) and not pos("12",nototals$(a:a)) then
		if not (sort_col=a and nototals$(a:a)="2") then print #specfile: "let cells$(4)(inf:inf)='c"&str$((widths(a)/(widths-delcol))*page_width)&"|"&brst$&"|vt|"&hj$(a)&"|'"
	else
		if not (sort_col=a and nototals$(a:a)="2") then print #specfile: "let cells$(4)(inf:inf)='c"&str$((widths(a)/(widths-delcol))*page_width)&"|"&brdr$&"|vt|"&hj$(a)&"|'"
	end if
next a
! CLOSE #SPECFILE:
 : _
! lank spacer row                                              : _

 !
print #specfile: "LET CELLS$(5)='li0.1|tg0.100|fPALATINO|fs"&fst$&"|'"
let brdr$="br1"
print #specfile: "let cells$(5)(inf:inf)='c"&str$(page_width)&"|"&brdr$&"|vt|hl|'"
close #specfile:
return ! /r

SET_FORMAT: ! r:
 : _
! etermine numeric v. string formats for initial processing    : _
! ased on FORMS$ array elements                                : _
 !
dim hforms$(1)*50,_b$(1)*50
_h=udim(mat headers$)
mat hforms$(_h)
for a=1 to _h
 hforms$(a)=forms$(a)
	if uprc$(forms$(a))(1:4)="PIC(" then hforms$(a)=forms$(a)(1:pos(forms$(a),")"))
	if uprc$(forms$(a))(1:4)="FMT(" then hforms$(a)=forms$(a)(1:pos(forms$(a),")"))
	if uprc$(forms$(a))(1:2)="N " and pos(forms$(a),",")>0 then : _
		hforms$(a)=forms$(a)(1:pos(forms$(a),",")) else : _
		if uprc$(forms$(a))(1:2)="N " then hforms$(a)=forms$(a)
	if uprc$(forms$(a))(1:3)="ZD " and pos(forms$(a),",")>0 then : _
		hforms$(a)=forms$(a)(1:pos(forms$(a),")")) else : _
		if uprc$(forms$(a))(1:3)="ZD " then hforms$(a)=forms$(a)
! IF NOT NOTOTALS$(A:A)="0" THEN cOL_FORMAT(A)+=1
	if uprc$(forms$(a))(1:1)="C" then col_format(a)+=1
	if uprc$(forms$(a))(1:1)="V" then col_format(a)+=1
	if uprc$(forms$(a))(1:1)="G" then col_format(a)+=1
	if uprc$(forms$(a))(1:1)="L" then col_format(a)+=1
	if uprc$(forms$(a))(1:4)="DATE" then col_format(a)+=1 : _
		hforms$(a)=uprc$(forms$(a)(1:pos(forms$(a),")")))
	_p=pos(hforms$(a),")")
	_c=pos(hforms$(a),",",-1)
	if _c>_p then
		_x=str2mat(hforms$(a),mat _b$,",")
		if _x>1 then mat _b$(_x-1)
		mat2str(mat _b$,hforms$(a),",")
	end if
next a
return ! /r

PRINT_SUBTOTALS: ! r:
 : _
! eset t ccs$ array to t correct number of columns          : _

 !
if not nosubtotal then
	mat ccs$(lcols) : _
	mat ccb$(lcols) : _
 mat2str(mat ccb$,ccb$," | ")
	for aas=1 to udim(col_total)

		! lear non-numeric column totals

		if col_format(aas)>0 or val(nototals$(aas:aas))>0 then
			ccs$(aas)=""
		else
			ccs$(aas)=str$(col_subtotal(aas))

			! ormat numeric column totals to match data elements

			if trim$(hforms$(aas))>"" then : _
				ccs$(aas)=cnvrt$(hforms$(aas),col_subtotal(aas)) : _
				col_subtotal(aas)=0
		end if
	next aas
	mat ccst$(udim(ccs$)) : _
	mat ccst$=ccs$ : _
	if sort_col>0 and nototals$(sort_col:sort_col)="2" then fndelrow$(mat ccst$,sort_col)
 mat2str(mat ccst$,ccs$," | ")
	print #textfile: "4| "&ccs$
 sort_count=0 : _
	mat col_subtotal=(0)
	mat bb$(udim(mat cc$))=("") : _
 mat2str(mat bb$,ccs$," | ")
	if sort_col>0 and nototals$(sort_col:sort_col)="2" then : _
		print #textfile: "5| "&cc$(sort_col) else : _
		print #textfile: "5| "&ccs$
end if
ZPRINT_SUBTOTALS: return ! /r

def library fnlistfilter(winno,spec$*100,filter$;nosort,delay)
		: _
	! Extract information from a listbox and pr t information : _
	! a new LIST saving t old list to a single cell              : _
	! The old list can be recalled by ESC                          : _
	! WINNO t window number in which t listbox appears         : _
	! SPEC$ t list secifier 1,1,LIST 12/15 for example           : _
	! FILTER$ t contiguous letters to look for                   : _
	! NOSORT if true forces t filtered list to original sequence : _
	! DELAY  if true waits for t enter key before filtering      : _

	library env$("PD")&"Core\fnsnap\rtflib_dll.br": fnlistcopy
	library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnmsexe$,fnh,fndelrow,fndelrow$,fnlistspec$,fnwaitwin,fnwaitbar
	dim headers$(1)*50,widths(1),forms$(1)*50,aa$(1)*50,ab$(1)*50,ba$(1)*50,bb$(1)*50,cc$(1)*3000,headers$*2000,cc$*20000,dorder(1),sorder(1),exclude(1)
	execute 'proc '&env$("PD")&'Core\fnsnap\tt'
	input #winno, fields spec$&",HEADERS,,nowait ": (mat headers$,mat widths,mat forms$) : _
		: _
	! Read t Header arrays                                       : _


 filterspec$=fnlistspec$(filterwin,srow:=10,scol:=10,frows:=10,fcols:=min(sum(widths),70),arows:=1,mat headers$,mat widths,mat forms$,"Filtered","",0,2) : _
		: _
	! Open t filtered array list and window                      : _


	input #winno, fields spec$&",rowsub,all,displayed_order,nowait": mat dorder : _
		: _
	! Get t order in which records are displayed                 : _


 x=udim(dorder) : _
	mat sorder(x) : _
	for a=1 to x : sorder(a)=a : next a : _
		: _
	! et SORDER to 1 through x for record read in same order as    : _
	! riginal unsorted list                                        : _

	if nosort then mat dorder=sorder
 : _
	! etermine numeric v. string columns based on FORMS$ array     : _


 lcols=udim(mat headers$) : _
	mat cc$(lcols) : _
	mat col_format(lcols)=(0) : _
	mat col_total(lcols)=(0)
	gosub SET_FORMAT
BUILD_FILTER: a$=kstat$(1) : _
		: _
	! Set a filter for processing incoming records                 : _


	if unhex$(a$)="08" then : _
		filter$=filter$(1:len(filter$)-1) : _
		print #filterwin,fields "12,2,c 30": "Current filter "&filter$ : _
		goto BUILD_FILTER : _
	else : _
		if len(unhex$(a$))<4 and not unhex$(a$)="0D" then filter$=filter$&a$
	if len(unhex$(a$))>2 then mat cc$(0) : goto END_FILTER
	print #filterwin,fields "12,2,c ": "Current filter "&filter$
	if delay and not unhex$(a$)="0D" then goto BUILD_FILTER
START_FILTER: !
 aa=udim(dorder)+1 : _
 x=0
 waitwin=fnwaitwin("      Processing filter           ") !
START_FILTER1: !
		: _
	! ead t incoming array and process for filtered items        : _


 fnwaitbar(udim(dorder),udim(dorder)-aa)
 aa-=1
	if aa<=0 then : _
		mat cc$(0) : close #waitwin: : waitwin=0 : goto BUILD_FILTER
	mat start(1): mat end(1) : mat start=(dorder(aa)) : mat end=(dorder(aa)) : _
	input #winno, fields spec$&",row,range,nowait": mat start,mat end,mat cc$ : _
	mat dd$(udim(mat cc$))=cc$ : _
		: _
	! ead each row in t input array                              : _


	print mat cc$
 : _
	! ormat cells for each column                                  : _


	for aaa=1 to udim(mat cc$)
		if not col_format(aaa) then : _
			col_total(aaa)+=val(srep$(cc$(aaa),"$","")) conv SET_NUM : _
				: _
			! heck numeric columns and add to column total if no conversion
			! rror.  If a conversion error occurs mark t colun as string : _
			! o that no total will appear and no further conversion testing
			! ill be done                                                  : _

		if not col_format(aaa) and trim$(hforms$(aaa))>"" then : _
			cc$(aaa)=cnvrt$(hforms$(aaa),val(srep$(cc$(aaa),"$",""))) : _
				: _
			! ormat t numeric column if conversion works                 : _


		if col_format(aaa) and trim$(hforms$(aaa))(1:4)="DATE" then : _
			execute "LET CC$(AAA)=date$(val(cc$(aaa)),"&srep$(srep$(hforms$(aaa),'DATE(','"'),')','")') : _
				: _
			! ormat dates                                                  : _


		if pos(uprc$(cc$(aaa)),uprc$(filter$))>0 then : _
			x+=1 : _
			mat asel(x) : _
			asel(x)=start : _
			goto SHOW_FILTER
	next aaa
	goto END_FILTER
! PAUSE
SHOW_FILTER: !
	if x=1 then : _
		print #filterwin, fields filterspec$&",HEADERS,[H]": (mat headers$,mat widths,mat forms$) : _
		print #filterwin, fields filterspec$&",RANGE": 1,udim(dorder),mat dd$ : _
		mat dd$=("") : _
		mat cc$=("") : _
		goto START_FILTER1 : _
			: _
		! dd t row to t top of t output array and process t    : _
		! ext row                                                      : _

	if x>1 then print #filterwin, fields filterspec$&",RANGE": 1,0,mat dd$ : _
		mat dd$=("") : _
		mat cc$=("") : _
		goto START_FILTER1 : _
			: _
		! dd t row to t top of t output array and process t    : _
		! ext row                                                      : _

END_FILTER: if filter$>"" and len(unhex$(a$))<4 then : _
		goto START_FILTER1
 : _
	! eset t CC$ array to t correct number of columns          : _


	mat cc$(lcols)
	input #filterwin,fields filterspec$&",rowsub,all,displayed_order,nowait": mat dorder
	if udim(dorder)>0 then fnlistcopy(filterwin,filterspec$,winno,spec$)
	close #filterwin:
fnend

def library fnlistcopy(readwin,readspec$,savewin,savespec$)

	! ransfers t contents of one listbox to another. Both lists
	! ust have t same header information in terms of t number
	! f columns
	! EADWIN t window number where t source list is located
	! EADSPEC$ t specification of row,col LIST rows/cols of sourc
	! AVEWIN t window number of t receiving list
	! AVESPEC$ t specification row,col LIST rows/cols of destina

	library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnh,fndelrow,fndelrow$,fnlistspec$
	dim sh$(1)*50,sw(1),sf$(1)*50,ss$(1)*100,ss$*20000,rdo(1),sdo(1),rso(1),sso(1)
 copy_count=0
	input #readwin, fields readspec$&",HEADERS,,nowait ": (mat sh$,mat sw,mat sf$)
 x=udim(mat sf$) : _
	for a=1 to x : _
		sf$(a)=trim$(sf$(a)) : _
	next a
	input #readwin, fields readspec$&",rowsub,all,displayed_order,nowait": mat rdo
 copy_count=udim(rdo)
	input #readwin, fields readspec$&",row,all,nowait": mat ss$
	print #savewin, fields savespec$&",=R": mat ss$ : _
		: _
	! Erase t contents of t existing SAVE LIST and replace     : _
	! with t new records                                         : _

 fnlistcopy=copy_count
fnend

def library fnlistsave(savewin,savespec$,&sspec$;_swin,&_srow,&_scol)
		: _
	! ave a copy of a list in t lower left corner of window 0    : _
	! s t default.  Optionally another window and location can   : _
	! e specified.                                                 : _
	! SWIN window to save LIST to default is 0                     : _
	! SROW row number within window default is bottom row          : _
	! SCOL column number of row to save LIST default is col 1      : _
	! he saved coy of t list occupies a 1 x 1 character space in : _
	! he designated window                                         : _
	! NLISTSAVE = value of : _SWIN                                   : _

	library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnmsexe$,fnh,fndelrow,fndelrow$,fnlistspec$,fnwinrowcol
	dim sh$(1)*50,sw(1),sf$(1)*50,ss$(1)*20,ss$*20000,so(1)
 fnlistsave=0
	input #savewin, fields savespec$&",HEADERS,,nowait ": (mat sh$,mat sw,mat sf$)
 x=udim(mat sf$) : _
	for a=1 to x : _
		sf$(a)=trim$(sf$(a)) : _
	next a
 fnwinrowcol(_swin,lrow,lcol) : _
	print lrow : _
		: _
	! et t screen size of t main window


	if not _srow then _srow=lrow+1 else _srow=min(_srow,lrow)
	if not _scol then _scol=1 else _scol=min(_scol,lcol)
	print #_swin, fields (sspec$:=str$(_srow)&","&str$(_scol)&",LIST 1/1")&",HEADERS,[H]": (mat sh$,mat sw,mat sf$)

	! reate a list header in a single cell in t lower left
	! orner of t main screen

START_SAVE: !
	input #savewin, fields savespec$&",row,all,nowait": mat ss$
	print #_swin,fields sspec$&",=R": mat ss$
END_SAVE: !
 fnlistsave=_swin
fnend
def library fnlistenter(winno,spec$*100,element)

	! Extract information from a listbox and pr t information
	! in an RTF report using Word
	! WINNO t window number in which t listbox appears
	! SPEC$ t list secifier 1,1,LIST 12/15 for example

! LIBRARY env$("PD")&"Core\fnsnap\rtflib_dll.br": FNLISTCOPY
	library env$("PD")&"Core\fnsnap\fnsnap_dll.br": fnmsexe$,fnh,fndelrow,fndelrow$,fnlistspec$,fnwaitwin,fnwaitbar
	dim headers$(1)*50,widths(1),forms$(1)*50,aa$(1)*50,ab$(1)*50,ba$(1)*50,bb$(1)*50,cc$(1)*3000,headers$*2000,cc$*20000,dorder(1),sorder(1),exclude(1)
	dim inwrk$(1)*60,indate$(1)*60
	execute 'proc '&env$("PD")&'Core\fnsnap\tt'
	input #winno, fields spec$&",HEADERS,,nowait ": (mat headers$,mat widths,mat forms$)

	! Read t Header arrays


	input #winno, fields spec$&",rowsub,all,displayed_order,nowait": mat dorder

	! Get t order in which records are displayed


	input #winno, fields spec$&",colcnt,all,nowait": lcols

	! Get t number of columns in t control


	for a=1 to lcols : hlen=max(hlen,len(headers$(a))) : next a
	for a=1 to lcols : wlen=max(wlen,widths(a)) : next a
	sort_cnt=0
	x=udim(dorder)
! IF NOSORT THEN MAT DORDER=SORDER

	! etermine numeric v. string columns based on FORMS$ array


	mat cc$(lcols)
	mat col_format(lcols)=(0)
	mat col_total(lcols)=(0)
	gosub SET_FORMAT
GET_ROW: !

	! Set a row of data to change or edit


	mat start(1): mat end(1) : mat start=(dorder(element)): mat end=start
	input #winno, fields spec$&",row,range,nowait": mat start,mat end,mat cc$
	mat dd$(udim(mat cc$))=cc$

	! ead t selected row from t list ELEMENT


	print mat cc$

	! ormat cells for each column


	for aaa=1 to udim(mat cc$)

		! heck numeric columns and add to column total if no conversion
		! rror.  If a conversion error occurs mark t colun as string
		! o that no total will appear and no further conversion testing
		! ill be done

		if not col_format(aaa) then col_total(aaa)+=val(srep$(cc$(aaa),"$","")) conv SET_NUM

			! heck to see if conversion works for otherwise numeric column


		if not col_format(aaa) and trim$(hforms$(aaa))>"" then
			cc$(aaa)=cnvrt$(hforms$(aaa),val(srep$(cc$(aaa),"$","")))

			! format t numeric column if conversion works
		end if


	next aaa

	! pen a window and allow data entry


! OPEN #(ENTERWIN:=fnH): "srow=5,scol=5,rows="&STR$(UDIM(MAT CC$)+1)&",cols="&STR$(50)&",PARENT=NONE",DISPLAY,outIn
	open #(enterwin:=fnh): "srow=5,scol=5,rows="&str$(udim(mat cc$)+1)&",cols="&str$(hlen+wlen+10)&",PARENT=NONE",display,outin
	for a=1 to udim(mat headers$)
		print #enterwin,fields str$(a)&",2,c "&str$(hlen)&",N/W:T": trim$(headers$(a))
		mat inwrk$(a): mat indate(a): mat indate$(a) : _
		if hforms$(a)>"" then : _
			inwrk$(a)=str$(a)&","&str$(hlen+3)&","&hforms$(a)&",[D]" else : _
			inwrk$(a)=str$(a)&","&str$(hlen+3)&","&forms$(a)&",[D]"
		_p=pos(forms$(a),")",-1) : _
		_c=pos(forms$(a),",",-1) : _
		if _c>_p and pos(uprc$(forms$(a)),"P",_c)>0 then inwrk$(a)=srep$(inwrk$(a),",[D]",",P[D]")
		if widths(a)=0 then inwrk$(a)=srep$(inwrk$(a),",[D]",",P[D]")
		if pos(uprc$(inwrk$(a)),"DATE(")>0 then : _
			inwrk$(a)(pos(inwrk$(a),"("):pos(inwrk$(a),")"))="(MMDDYY)" : _
			indate(a)=1 : _
			indate$(a)=inwrk$(a)(1:pos(inwrk$(a),","))&str$(hlen+2+10)&",C "&str$(50-(hlen+2+10))
	next a
ENTER_DATA: !
L15320: rinput #enterwin, fields mat inwrk$: mat cc$
 af=curfld
 ak=fkey
	if indate(af) then cc=val(cc$(af)) conv L15320
	if indate(af) and not cc then : _
		curfld(af) : _
		fnwaitwin("Invalid date use MMDDYY","Date Error","OK") : _
		goto L15320
! IF INDATE(AF) THEN : _
	print #enterwin, fields indate$(af): " "
	if curfld=udim(mat cc$) then goto L15400
	if not ak then curfld(af+1,ak) else curfld(af,ak)
	goto ENTER_DATA
END_ENTER_DATA: !
	mat cc$(lcols)
! INPUT #FILTERWIN,FIELDS FILTERSPEC$&",rowsub,all,displayed_order,nowait": MAT DORDER
!
L15400: close #enterwin:
! pr #WINNO,FIELDS SPEC$&",CELL_RANGE": (ELEMENT-1)*UDIM(CC$)+1,(ELEMENT)*(UDIM(CC$)),MAT CC$ : _
	print #winno,fields spec$&",RANGE": element,element,mat cc$
fnend


def library fnrtf(txtfile,specfile$*100,rtffile;picpath$*100,subpath$*100)
! mat TYPES$ holds single letters used to designate type
	! of line such as H=header F=Footer D=Detail data T=Total line
	! mat STYLES$ holds t formatting specifications followed
	! by t name of t style such as HEADER
	! segments of t style are separated by a pipe | character
	! elements of t style might be li0.5 for left indent 1/2 inch
	! rt2 sets a tab at 2 inches
	! dt2.5 sets a decimal tab at 2 1/2 inches
	! mat DATA$ holds t body of t report
	! t first character designates what style is to be applied to t
	! line t remaining elements separated by pipes are t data to be
	! printed using t style
	! a long sentence will be wrapped by t RTF reader and so long text
	! lines do not need to be broken
	! RFTFILE is t file number already opened to hold t report
	! this file should be a display file with an ".rtf" suffix
	library env$("PD")&"Core\fnsnap\RTFLIB_dll.br": fnamt$,fntext$,fntype
 orientation$="PORTRAIT"
 paper$="LETTER"
 lmargin=rmargin=tmargin=bmargin=0.5
 nume=0
	mat styles$(1)=("")
	mat cells$(1)=("")
	mat types$(1)=("")
 lefttext$=""
 setspec=1
	linput #txtfile: data$
	if pos(data$,"[SPEC(") then gosub SET_SPECFILE : restore #txtfile:

	execute "proc *"&specfile$
 setspec=0
	if uprc$(trim$(orientation$))="LANDSCAPE" then landscape=1 else landscape=0
 t=udim(mat types$) : _
 crlf$=chr$(13)&chr$(10)
	dim s$(1)*2000,se$(1)*1000,colors$(19),data$*32000,bold$(1),fs$*100,fc$*100,sa$*100
	dim types$(1)*2,styles$(1)*2000,cells$(1)*2000,lefttext$*100,papersize$*100
	dim boxmargins$*1000,oldmargins$*1000,boxmarginsn$*1000
	dim subx$(1)*500
	colors$(1)="[BLACK]"
	colors$(2)="[BLUE]"
	colors$(3)="[LTBLUE]"
	colors$(4)="[LTGREEN]"
	colors$(5)="[PINK]"
	colors$(6)="[RED]"
	colors$(7)="[YELLOW]"
	colors$(8)="[WHITE]"
	colors$(9)="[DKBLUE]"
	colors$(10)="[BLUEGREEN]"
	colors$(11)="[GREEN]"
	colors$(12)="[PURPLE]"
	colors$(13)="[BURGUNDY]"
	colors$(14)="[LTOLIVE]"
	colors$(15)="[GRAY]"
	colors$(16)="[LTGRAY]"
	colors$(17)="[DKGREEN]"
	colors$(18)="[OLIVE]"
	colors$(19)="[SEAGREEN]"
	mat s$(t)
	mat se$(t)
	mat lin$(t)
	mat bold$(t)
	mat lin$=("\lin0")
	mat bold$=("")
	qa$="\ql "
	gosub SET_STYLES
	if exists(subpath$&"subtext.txt")=2 then execute "proc *"&subpath$&"subtext.txt"
BUILD_RTF: linput #txtfile: data$ eof ZBUILD_RTF
	if pos(data$,"[SPEC(")>0 then gosub SET_SPECFILE ! Change pr specifications
	if pos(data$,"[NEWCELL(")>0 then gosub SET_NEWCELL ! Change cell specifications
	if pos(data$,"[FONT(")>0 then gosub SET_FONT ! Change font
	if pos(data$,"[SUB(")>0 then gosub SET_SUB
	if pos(data$,"[RTFCOL(") then
		xs=pos(data$,"[RTFCOL(")+8
		xe=pos(data$,")",xs)-1
		x=val(data$(xs:xe))
		print #rtffile: "\sect \sectd \sbknone\linex0\cols"&str$(x)&"\sectdefaultcl "
		data$=srep$(data$,"[RTFCOL("&str$(x)&")]","")
		! Start printing in columns with no separator
	end if
	if pos(data$,"[RTFCOLL(") then
		xs=pos(data$,"[RTFCOLL(")+9
		xe=pos(data$,")",xs)-1
		x=val(data$(xs:xe))
		print #rtffile: "\sect \sectd \sbknone\linex0\cols"&str$(x)&"\linebetcol\sectdefaultcl "
		data$=srep$(data$,"[RTFCOLL("&str$(x)&")]","")
		! Start printing in columns with a line separating t columns
	end if
	if pos(data$,"[WD(") then
		xs=pos(data$,"[WD(")+4
		xe=pos(data$,")]",xs)-1
		x=val(data$(xs:xe))
		data$=srep$(data$,"[WD("&str$(x)&")]",'{{\field{\*\fldinst SYMBOL '&str$(x)&' \\f "Wingdings" \\s 17}{\fldrslt\f14}}}')
		! Insert WingDing symbol
	end if
	if trim$(data$)<="" then
		print #rtffile: "\par "
		goto BUILD_RTF
		! Insert a blank line
	end if
 data$=srep$(data$,chr$(9),"|")
	if data$(1:1)="N" and pos(data$,"|")=3 then
		newdata=1
		data$=data$(2:len(data$))
	else
		newdata=0
	end if
	if laststyle=1 and pos("0123456789",data$(1:1)) then
		print #prntfil: "\pard \ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0"&crlf$
		! Start printing cells
	end if
 data$=fn_rtfcolor$(data$)
	if data$(1:2)="D|" then
		x=len(data$)
		if x<30 and data$(x:x)=":" then
			data$=data$(1:2)&"{\b "&data$(3:len(data$))&"}"
		end if
	end if

	! Format Detail Step Section Code E


	if uprc$(data$(1:1))="E" then
		if checklist>0 then
			linecnt+=1
			if pos(data$(1:7),".")>1 or pos(data$(1:7),")")>1 then
				numplc=pos(data$," ")
			else
				numplc=0
			end if
			if nume then
				data$=data$(1:2)&"\tab "&str$(linecnt)&".)"&"\tab "&fntext$(data$(3:len(data$)),50)&"\tab "&lefttext$&"[RTFLINE]"
			else if numplc>0 then
				data$=data$(1:numplc-1)&"\tab    "&fntext$(ltrm$(data$(numplc:len(data$))),50)&"\tab "&lefttext$&"[RTFLINE]"
			else
				data$=fntext$(ltrm$(data$),50)&"\tab "&lefttext$&"[RTFLINE]"
			end if
		end if
	else
		x=0
		x=val(data$(3:7)) conv L50240
		if x>0 then
			data$=data$(1:2)&"{\par\b "&data$(3:6)&"}"&data$(7:len(data$))
		end if
		L50240: !
		if data$(3:len(data$))="Cause" then
			data$(3:len(data$))="  "&data$(3:len(data$))
		else
			data$=srep$(data$,"Cause ","  Cause [RTFLINE]")
		end if
		if data$(3:len(data$))="Remedy" then
			data$(3:len(data$))="  "&data$(3:len(data$))
		else
			data$=srep$(data$,"Remedy ","  Remedy [RTFLINE]")
		end if
	! not sure how these line fit       else
	! not sure how these line fit       	linecnt=0 ! END IF
	end if

! Create special processing for PROGRAM G lines


if uprc$(data$(1:1))="G" then
 data$=srep$(data$,"!:","!:[RTFLINE]")
end if

! Perform bracket replacement parameters


let data$=srep$(data$,"[RTFPAGE]","\page ")
let data$=srep$(data$,"[RTFLINE]","\line ")
let data$=srep$(data$,"[RTFDATE]",date$("Month DD, CCYY"))


! Draw box around text following this point


if pos(data$,"[RTFBOX]")>0 then
 rtfbox=1
 data$=srep$(data$,"[RTFBOX]","")
 d$=""
end if
! End t box started by prior line


if pos(data$,"[\RTFBOX]")>0 then : _
 rtfbox=0 : _
 data$=srep$(data$,"[\RTFBOX]","") : _
 d$=""

! Create a line above text


if pos(data$,"[TOPLINE]")>0 then : _
 data$=srep$(data$,"[TOPLINE]","") : _
 topline=1 : _
 d$=""
if pos(data$,"[\TOPLINE]")>0 then : _
 data$=srep$(data$,"[\TOPLINE]","") : _
 topline=0 : _
 d$=""

! Create a line below text


if pos(data$,"[BOTLINE]")>0 then : _
 data$=srep$(data$,"[BOTLINE]","") : _
 botline=1 : _
 d$=""
if pos(data$,"[\BOTLINE]")>0 then : _
 data$=srep$(data$,"[\BOTLINE]","") : _
 botline=0 : _
 d$=""

! Create a line in t middle of a box. Verticle lines are
! created by using Bar Tabs in a style line

if pos(data$,"[MIDLINE]")>0 then
	midline=1
	data$=srep$(data$,"[MIDLINE]","") : d$=""
end if
if pos(data$,"[\MIDLINE]")>0 then
	midline=0
	data$=srep$(data$,"[\MIDLINE]","") : d$=""
end if
!  Create a header to appear at t top of every page


if uprc$(data$(1:1))="H" then
	gosub HEADER
	goto L50640
end if

! Create a footer to appear at t bottom of every page


if uprc$(data$(1:1))="F" then
	gosub FOOTER
	goto L50640
end if
if not uprc$(data$(1:1))=d$ then gosub NEW_STYLE
gosub PICTURE ! pr #RTFFILE: "{\par "&SREP$(DATA$(3:LEN(DATA$)),"|","}{\tab}{ ")&"}"
L50640: d$=uprc$(data$(1:1))
goto BUILD_RTF
ZBUILD_RTF: print #rtffile: "}"
header=0
fnend

HEADER: ! r:
if header then
	print #rtffile: "\par\sect \sectd \linex0\endnhere\sectlinegrid360\sectdefaultcl"&crlf$
	if lmargin>0 then print #rtffile: "\margl"&fn_twips$(lmargin)&"\marglsxn"&fn_twips$(lmargin)&crlf$
	if rmargin>0 then print #rtffile: "\margr"&fn_twips$(rmargin)&"\margrsxn"&fn_twips$(rmargin)&crlf$
	if tmargin>0 then print #rtffile: "\margt"&fn_twips$(tmargin)&"\margtsxn"&fn_twips$(tmargin)&crlf$
	if bmargin>0 then print #rtffile: "\margb"&fn_twips$(bmargin)&"\margbsxn"&fn_twips$(bmargin)&crlf$
! \marglsxn1800\margrsxn1440\margtsxn720\margbsxn720
!  IF THEADER >0 THEN pr #RTFFILE: "\linex0\headery"&fn_twips$(THEADER)&"\footery"&fn_twips$(TFOOTER)&"\endnhere\sectlinegrid"&fn_twips$(THEADER)&"\sectdefaultcl"&CRLF$

end if
if botline then

	print #rtffile: "{\header \pard \plain "&srep$(s$(srch(mat types$,"H")),"\widctlpar","\widctlpar\brdrb\brdrs\brdrw10\brsp20 ")&"{"&srep$(srep$(data$(3:len(data$)),"|","}{\tab}{ "),"[RTFBAR]","|")&"\par }}"&crlf$ ! " {"&DATA$(3:LEN(DATA$))&" \par }}"&CRLF$
 botline=0
else
	print #rtffile: "{\header \pard \plain "&s$(srch(mat types$,"H"))&" {"&srep$(srep$(data$(3:len(data$)),"|","}{\tab}{"),"[RTFBAR]","|")&" \par }}"&crlf$
end if
header+=1
return ! /r
FOOTER: ! r:
if topline then
	print #rtffile: "{\footer \pard \plain "&srep$(srep$(srep$(s$(srch(mat types$,"F")),"widctlpar","widctlpar\brdrt\brdrs\brdrw10\brsp20 ")&" {"&srep$(data$(3:len(data$)),"[PAGE]","  {{\field{\*\fldinst{\f4  PAGE }}{\fldrslt{\f4 2}}}}"),"|","}{\tab}{ "),"[RTFBAR]","|")&" \par }}"&crlf$
 topline=0
else
	print #rtffile: "{\footer \pard \plain "&s$(srch(mat types$,"F"))&" {"&srep$(srep$(srep$(data$(3:len(data$)),"[PAGE]","  {{\field{\*\fldinst{\f4  PAGE }}{\fldrslt{\f4 2}}}}"),"|","}{\tab}{"),"[RTFBAR]","|")&" \par }}"&crlf$
end if
footer+=1
return ! /r
NEW_STYLE: ! r:
! IF SRCH(MAT TYPES$,UPRC$(DATA$(1:1)))>0 THEN pr #RTFFILE: "\pard \plain "&S$(SRCH(MAT TYPES$,UPRC$(DATA$(1:1))))
if rtfbox then
 xlis=pos(s$(srch(mat types$,uprc$(data$(1:1)))),"\li")
 xlie=pos(s$(srch(mat types$,uprc$(data$(1:1)))),"\",xlis+1)-1
 boxmargins$="\li"&str$(lmargin*1440)&"\ri"&str$((lmargin+rmargin)*2*1440)
 oldmargins$=s$(srch(mat types$,uprc$(data$(1:1))))(xlis:xlie)
 boxmarginsn$="\aspalpha\aspnum\faauto\adjustright"&srep$(srep$(boxmargins$,"\ri","\rin"),"\li","lin")&"\itap0"
	if midline then
		if srch(mat types$,uprc$(data$(1:1)))>0 then print #rtffile: "\pard \plain "&srep$(srep$(s$(srch(mat types$,uprc$(data$(1:1)))),"widctlpar","wictlpar\brdrt\brdrs\brdrw10\brsp20 \brdrl\brdrs\brdrw10\brsp80 \brdrb\brdrs\brdrw10\brsp20 \brdrr\brdrs\brdrw10\brsp80 \brdrbtw\brdrs\brdrw10\brsp20 "),oldmargins$,boxmargins$)&crlf$&boxmarginsn$&crlf$
	else
		if srch(mat types$,uprc$(data$(1:1)))>0 then print #rtffile: "\pard \plain "&srep$(srep$(s$(srch(mat types$,uprc$(data$(1:1)))),"widctlpar","wictlpar\brdrt\brdrs\brdrw10\brsp20 \brdrl\brdrs\brdrw10\brsp80 \brdrb\brdrs\brdrw10\brsp20 \brdrr\brdrs\brdrw10\brsp80 "),oldmargins$,boxmargins$)&crlf$&boxmarginsn$&crlf$
	end if
else
	if midline then
		if srch(mat types$,uprc$(data$(1:1)))>0 then print #rtffile: "\pard \plain "&s$(srch(mat types$,uprc$(data$(1:1))))&crlf$
	else
		if srch(mat types$,uprc$(data$(1:1)))>0 then print #rtffile: "\pard \plain "&s$(srch(mat types$,uprc$(data$(1:1))))&crlf$
	end if
end if
d$=uprc$(data$(1:1))
return
SET_STYLES: !
perin=1440
for a=1 to t
 text$=trim$(text$)
 ap=0
! s$(A)="\s"&STR$(A+10)&"\q1 "
 s$(a)="\s"&str$(a+10)&"\ql "

SET_STYLES_1: ap+=1
 tx=pos(styles$(a),"|")
	if tx<0 then tx=len(styles$(a))+1
 ap+=1

	! First line indent


	if uprc$(styles$(a)(1:2))="FI" then
		s$(a)=s$(a)&"\fi"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
		goto ZSET_STYLES_1
	end if
 ap+=1

	! Left indent


	if uprc$(styles$(a)(1:2))="LI" then
		s$(a)=s$(a)&"\li"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
		lin$(a)="\lin"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
		goto ZSET_STYLES_1
	end if

	! Right indent


	if uprc$(styles$(a)(1:2))="RI" then
		s$(a)=s$(a)&"\ri"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
		goto ZSET_STYLES_1
	end if
	if ap=1 then s$(a)=s$(a)&"\li0\ri0\widctlpar" : _
	else if pos(s$(a),"widctlpar")<1 then s$(a)=s$(a)&"\widctlpar "

! Tab left align

 !
if uprc$(styles$(a)(1:2))="TL" then
 s$(a)=s$(a)&"\tx"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
	goto ZSET_STYLES_1
end if

! Tab center align


if uprc$(styles$(a)(1:2))="TC" then
 s$(a)=s$(a)&"\tqc\tx"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
	goto ZSET_STYLES_1
end if

! Tab right align


if uprc$(styles$(a)(1:2))="TR" then
 s$(a)=s$(a)&"\tqr\tx"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
	goto ZSET_STYLES_1
end if

! Tab decimal point align


if uprc$(styles$(a)(1:2))="TD" then
 s$(a)=s$(a)&"\tqdec\tx"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
	goto ZSET_STYLES_1
end if

! Tab vertical Bar align


if uprc$(styles$(a)(1:2))="TB" then
 s$(a)=s$(a)&"\tb"&str$(round(val(styles$(a)(3:tx-1))*perin,0))
	goto ZSET_STYLES_1
end if

! Font if other than Times new roman (Panto....)
! Only Courier and Arial are currently configured

if uprc$(styles$(a)(1:9))="FPALATINO" then ff$="\f29"
	goto ZSET_STYLES_1
if uprc$(styles$(a)(1:6))="FFRITZ" then ff$="\f28" : _
	goto ZSET_STYLES_1
if uprc$(styles$(a)(1:6))="FTIMES" then ff$="\f0" : _
	goto ZSET_STYLES_1
if uprc$(styles$(a)(1:6))="FARIAL" then ff$="\f1" : _
	goto ZSET_STYLES_1
if uprc$(styles$(a)(1:8))="FCOURIER" then ff$="\f2" : _
	goto ZSET_STYLES_1

! Font size


if uprc$(styles$(a)(1:2))="FS" then
 fs$=str$(round(val(styles$(a)(3:tx-1))*2,0))
	goto ZSET_STYLES_1
end if

! Font color


if uprc$(styles$(a)(1:2))="CF" then
! INPUT FIELDS "23,64,c 1": PAUSE$
 fc$="\cf"&str$(max(0,srch(mat colors$,uprc$(styles$(a)(3:tx-1)))))&" "
	goto ZSET_STYLES_1
end if

! Font BOLD


if uprc$(styles$(a)(1:1))="B" then
 bold$(a)="\b"
	goto ZSET_STYLES_1
end if

! Space After Paragraph


if uprc$(styles$(a)(1:2))="SA" then
 sa$="\sa"&cnvrt$("pic(####)",val(style$(a)(3:tx-1))*320)&" "
	goto ZSET_STYLES_1
end if

! Paragraph Alignment


if uprc$(styles$(a)(1:1))="Q" and pos("LRCJ",uprc$(styles$(a)(2:2)))>0 then
 qa$="\q"&lwrc$(styles$(a)(2:2))&" "
 s$(a)=srep$(s$(a),"\ql ",qa$)
	goto ZSET_STYLES_1
end if
ZSET_STYLES_1: !
styles$(a)=styles$(a)(tx+1:len(styles$(a)))
if len(styles$(a))>1 and pos(styles$(a),"|")>0 then goto SET_STYLES_1
s$(a)=s$(a)&"\aspalpha\aspnum\faauto\adjustright\rin0"&lin$(a)&"\itap0 "&bold$(a)&ff$&"\fs"&fs$&fc$&sa$&"\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 "
se$(a)="\sbasedon"&str$((a-1)+10)&" \snext"&str$(a+10)
fc$=fs$=sa$=ff$=""
next a
print #rtffile: "{\rtf1\ansi\ansicpg1252\uc1 \deff0\deflang1033\deflangfe1033"&crlf$
print #rtffile: "{\fonttbl"&crlf$
print #rtffile: "{\f0\froman\fcharset0\fprq2{\*\panose 02020603050405020304}Times New Roman;}"&crlf$
print #rtffile: "{\f1\fswiss\fcharset0\fprq2{\*\panose 020b0604020202020204}Arial;}"&crlf$
print #rtffile: "{\f2\fmodern\fcharset0\fprq1{\*\panose 02070309020205020404}Courier New;}"&crlf$
print #rtffile: "{\f14\fnil\fcharset2\fprq2{\*\panose 05000000000000000000}Wingdings;}"&crlf$
print #rtffile: "{\f28\froman\fcharset0\fprq2{\*\panose 02020500000000000000}Fritz-Quad;}"&crlf$
print #rtffile: "{\f29\froman\fcharset0\fprq2{\*\panose 02040502050505030304}Palatino Linotype;}"&crlf$
print #rtffile: "{\f42\fbidi \fmodern\fcharset0\fprq2{\*\panose 050b0009000000000000}OCR-A;}"&crlf$
print #rtffile: "{\f69\fmodern\fcharset0\fprq1{\*\panose 020b0609040504020204}Lucida Console;}"&crlf$
print #rtffile: "}"&crlf$
print #rtffile: "{\colortbl;\red0\green0\blue0;\red0\green0\blue255;\red0\green255\blue255;\red0\green255\blue0;\red255\green0\blue255;\red255\green0\blue0;\red255\green255\blue0;\red255\green255\blue255;"&crlf$
print #rtffile: "\red0\green0\blue128;\red0\green128\blue128;\red0\green128\blue0;\red128\green0\blue128;\red128\green0\blue0;\red128\green128\blue0;\red128\green128\blue128;\red192\green192\blue192;\red255\green255\blue255;\red217\green217\blue217;}"&crlf$
print #rtffile: "{\stylesheet"&crlf$
print #rtffile: "{\ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0\itap0 \fs20\lang1033\langfe1033\cgrid\langnp1033\langfenp1033 \snext0 Normal;}"&crlf$
print #rtffile: "{\*\cs10 \additive Default Paragraph Font;}"&crlf$
for a=1 to t
	print #rtffile: "{"&s$(a)&se$(a)&" "&styles$(a)&";}"&crlf$
next a
print #rtffile: "}"&crlf$
print #rtffile: "{\info"&crlf$
print #rtffile: "{\title This is the first line}"&crlf$
print #rtffile: "{\title This is the first line}"&crlf$
print #rtffile: "{\operator George L. Tisdale}"&crlf$
print #rtffile: "{\creatim\yr"&date$("ccyy")&"\mo"&date$("mm")&"\dy"&date$("dd")&"\hr"&time$(1:2)&"\min"&time$(4:5)&"}"&crlf$
print #rtffile: "{\revtim\yr"&date$("ccyy")&"\mo"&date$("mm")&"\dy"&date$("dd")&"\hr"&time$(1:2)&"\min"&time$(4:5)&"}"&crlf$
print #rtffile: "{\version2}"&crlf$
print #rtffile: "{\edmins0}"&crlf$
print #rtffile: "{\nofpages1}"&crlf$
print #rtffile: "{\nofwords0}"&crlf$
print #rtffile: "{\nofchars0}"&crlf$
print #rtffile: "{\*\company Advanced Informonics Corporation}"&crlf$
print #rtffile: "{\nofcharsws0}"&crlf$
print #rtffile: "{\vern8247}"&crlf$
if lmargin>0 then print #rtffile: "\margl"&str$(ip(lmargin*1440))&crlf$
if rmargin>0 then print #rtffile: "\margr"&str$(ip(rmargin*1440))&crlf$
if tmargin>0 then print #rtffile: "\margt"&str$(ip(tmargin*1440))&crlf$
if bmargin>0 then print #rtffile: "\margb"&str$(ip(bmargin*1440))&crlf$
print #rtffile: "}"&crlf$
papersize$="\paperw12240\paperh15840"
if uprc$(trim$(paper$))="LETTER" or paper$<="" then
	if uprc$(trim$(orientation$))="LANDSCAPE" then : _
		papersize$="\paperw15840\paperh12240" else : _
		papersize$="\paperw12240\paperh15840"
end if
!
if uprc$(trim$(paper$))="LEGAL" then
	if uprc$(trim$(orientation$))="LANDSCAPE" then : _
		papersize$="\paperw20160\paperh12240" else : _
		papersize$="\paperw12240\paperh20160"
end if
print #rtffile: papersize$&crlf$
! IF UPRC$(TRIM$(ORIENTATION$))="LANDSCAPE" THEN pr #RTFFILE: PAPERSIZE$&CRLF$
print #rtffile: "\widowctrl\ftnbj\aenddoc\noxlattoyen\expshrtn\noultrlspc\dntblnsbdb\nospaceforul\hyphcaps0\formshade\horzdoc\dgmargin\dghspace180\dgvspace180\dghorigin720\dgvorigin720\dghshow1\dgvshow1"&crlf$
dim theader$*100
if theader>0 and tfooter>0 then : _
 theader$="\headery"&fn_twips$(theader)&"\footery"&fn_twips$(tfooter) else if theader>0 then : _
 theader$="\headery"&fn_twips$(theader) else if tfooter>0 then theader$="\footery"&fn_twips$(tfooter) else : _
 theader$=""
print #rtffile: "\jexpand\viewkind4\viewscale100\pgbrdrhead\pgbrdrfoot\splytwnine\ftnlytwnine\htmautsp\nolnhtadjtbl\useltbaln\alntblind\lytcalctblwd\lyttblrtgr\lnbrkrule \fet0\sectd \linex0"&theader$&"\endnhere\sectlinegrid360\sectdefaultcl "&crlf$
! IF LANDSCAPE=1 THEN pr #RTFFILE: "\lndscpsxn\psz1"
if landscape=1 then print #rtffile: "\landscape"
return ! /r
def library fnamt$*50(value,mask$*20;sign$,underline$,reverse)
	dim av$*30
	if reverse>0 then reverse=-1 else reverse=1
 underline$=uprc$(underline$)
	on pos("SD",underline$) goto SINGLE,DOUBLE none NO_UNDERLINE
NO_UNDERLINE: !
	if value*reverse<0 then av$=sign$&"(\tab "&trim$(cnvrt$(mask$,abs(value)))&"}{)" else av$=sign$&" \tab "&trim$(cnvrt$(mask$,abs(value)))&"}{ "
	goto ZAMT
SINGLE: !
	if value*reverse<0 then av$=sign$&"(\ul\tab "&trim$(cnvrt$(mask$,abs(value)))&"}{)" else av$=sign$&" \ul\tab "&trim$(cnvrt$(mask$,abs(value)))&"}{ "
	goto ZAMT
DOUBLE: !
	if value*reverse<0 then av$=sign$&"(\uldb\tab "&trim$(cnvrt$(mask$,abs(value)))&"}{)" else av$=sign$&"\uldb\tab "&trim$(cnvrt$(mask$,abs(value)))&"}{ "
	goto ZAMT
ZAMT: fnamt$=av$
fnend
def library fntext$*4000(text$*4000,alen)
	dim t$*4000
 t$=""
L52460: text$=trim$(text$)
	print text$
	if text$(1:9)="[RTFLINE]" then text$(1:9)=""
 xl=min(pos(text$,"[RTFLINE]",9)-1,alen)
	if xl<1 then xl=alen else : _
		if xl>alen then xl=alen
	print "XL="&str$(xl),"ALEN="&str$(alen)
! PAUSE
 x=pos(text$&" "," ",xl)
	if x<2 then goto L52520
	if len(t$) then t$=t$&"\line "&text$(1:x) else t$=text$(1:x)
 text$=trim$(text$(x:len(text$)))
	if len(trim$(text$))>xl then goto L52460
L52520: ! IF LEN(TEXT$) THEN t$=T$&"\line "&TEXT$
	if len(t$)>0 and len(text$) then t$=t$&"\line "&text$ else if len(text$)>0 then t$=t$&text$
fntext$=t$
print t$
print "*********************************************"
! PAUSE
fnend
def fn_rtfcolor$*6000(cd$*6000)
 acolor=0
	do while acolor<19
		acolor+=1
		if pos(cd$,colors$(acolor))>0 then cd$=srep$(cd$,colors$(acolor),"\cf"&str$(acolor)&" ")
	loop
 fn_rtfcolor$=cd$
fnend

SET_NEWCELL: ! r:
ncs=pos(data$,"[NEWCELL(")+9
nce=pos(data$,")]",ncs)-1
! PAUSE
if exists(picpath$&trim$(data$(ncs:nce))) then execute "proc *"&picpath$&trim$(data$(ncs:nce)) else input fields "10,10,c 10": pause$
data$=srep$(data$,"[NEWCELL("&data$(ncs:nce)&")]","")
! PAUSE
return ! /r
SET_FONT: ! r:
nfs=pos(data$,"[FONT(")+6
nfe=pos(data$,")]",nfs)-1
if nfs>0 then
 data$=srep$(data$,"[FONT(ARIAL)]","\f1 ")
 data$=srep$(data$,"[FONT(COURIER)]","\f2 ")
 data$=srep$(data$,"[FONT(FRITZ)]","\f28 ")
 data$=srep$(data$,"[FONT(PALATINO)]","\f29 ")
 data$=srep$(data$,"[FONT(OCR-A)]","\f42 ")
 data$=srep$(data$,"[FONT(LUCIDA)]","\f69 ")
end if
return ! /r
SET_SUB: ! r: substitute values
nss=pos(data$,"[SUB(")+5
nse=pos(data$,")]",nss)-1
if nse>0 then
	execute "let subx="&data$(nss:nse)
	if subx<1 then : _
		msgbox("The replaceable parameter "&data$(nss-1:nse+1)&" was not found in the data-set. The parameter will be ommitted") : _
		data$(nss-5:nse+2)="" : _
	else subx$=data$(nss-5:nse+2)
	if subx>0 then data$=srep$(data$,subx$,sub$(subx))
	goto SET_SUB
end if
return ! /r
PICTURE: ! r:

! Merge in RTF picture file and pr final DATA line

 !
dim pict_name$*100
if newdata then print #rtffile: "{\cf9 "

	! Sets text of newlines to BLUE


if pos(data$,"[PICT(") >0 then

	! PICT indicares a figure to insert centered on a page with a
	! cation underneath specifying t figure number

 pict_start=pos(data$,"[PICT(")
 pict_end=pos(data$,")]",pict_start)
	print #rtffile: "{\par \qc "&srep$(data$(3:pict_start-1),"|","}{\tab}{")
	if picpath$>"" then : _
		pict_name$=picpath$&"\"&trim$(data$(pict_start+6:pict_end-1)) else : _
		pict_name$=trim$(data$(pict_start+6:pict_end-1))
 fntype(pict_name$,rtffile)
 x=pos(pict_name$,"\",-1)+1 : _
	print #rtffile: "{\par \fs16 Figure: "&pict_name$(x:len(pict_name$))&" \par }"
	print #rtffile: srep$(data$(pict_end+2:len(data$)),"|","}{\tab}{")&"}"&crlf$
else if pos(data$,"[SPICT(") >0 then

	! SPICT represents a signiture or similar RTF graphic to be
	! positioned to t left side of t page

 pict_start=pos(data$,"[SPICT(")
 pict_end=pos(data$,")]",pict_start)
	print #rtffile: "{\par \ql "&srep$(data$(3:pict_start-1),"|","}{\tab}{")
	if picpath$>"" then : _
		pict_name$=srep$(picpath$&"\"&trim$(data$(pict_start+7:pict_end-1)),"\\","\") else : _
		pict_name$=trim$(data$(pict_start+7:pict_end-1))
 fntype(pict_name$,rtffile)
! x=pos(PICT_NAME$,"\",-1)+1
	print #rtffile: "{\par \fs16 Figure: "&pict_name$(x:len(pict_name$))&" \par }"
	print #rtffile: srep$(data$(pict_end+2:len(data$)),"|","}{\tab}{")&"}"&crlf$
else if pos("1234567890",data$(1:1))>0 then
	mat ccells$(udim(cells$)) : fn_cells(rtffile,data$,mat cells$)
else
	print #rtffile: "{"&srep$(srep$(data$(3:len(data$)),"|","}{\tab}{"),"[RTFBAR]","|")&"\par }"&crlf$
end if
if newdata then print #rtffile: "}"

	!  Turns off BLUE newline text


laststyle=0
return ! /r

! Create table lines and pr to existing pr file


def fn_cells(prntfil,cdata$*2000,mat ccells$)
! prntfil is t already open rtfoutput file
	! cdata$ is t copy of DATA$ being used by t function
	! mat ccells$ is t matrix of cell definitions received as MAT cells$
! INPUT FIELDS "24,64,c 1": PAUSE$
	dim brdr$*500,celldata$(1)*1000,call$*500,csize$(1)*500,celldef$*3000
 css=cse=0 ! starting position

	! Parse t incoming data line to determine what table
	! format to use

SETCSS: css=cse+1
 cse=min(len(cdata$)+1,pos(cdata$&"|","|",css))
	if css=1 then
		cell=val(cdata$(css:cse-1))
		cnum=0
		goto SETCSS
	end if

	!  Set matrix CELLDATA$ to contents of mat cells$


	if css<len(cdata$) then
		cnum+=1
		mat celldata$(cnum)
		celldata$(cnum)=cdata$(css:cse-1)
		goto SETCSS
	end if

	! Begin building table definition parameters from CELLS$(cell)


 cnum=css=cse=0
 cface$=cfont$=""
 call$=crlf$&"\trowd \trgaph108\trleft-108"

	!  Build t RTF properties for t table line needed


SETCELL: css=cse+1
 cse=pos(ccells$(cell)&"|","|",css)
	if cse<=len(ccells$(cell)) then

		! Left side indent if any


		if pos(ccells$(cell)(css:cse-1),"li")=1 then : _
			cindent$=fn_twips$(val(ccells$(cell)(css+2:cse-1))) : _
			cindent=val(cindent$) : _
			call$=call$&"\trleft"&cindent$ : _
			goto SETCELL

		! Set t distance between cells.  The number is 1/2 t
		! distance measured in inches converted to twips

! IF pos(CCELLS$(CELL)(CSS:CSE-1),"tg")=1 THEN : _
		x$=fn_twips$(.5*val(ccells$(cell)(css+2:cse-1))) : _
		call$=srep$(call$,"\trgaph108\trleft-108","\trgaph"&x$&"\trleft-"&x$&" ") : _
		goto SETCELL
		if pos(ccells$(cell)(css:cse-1),"tg")=1 then : _
			x$=fn_twips$(.5*val(ccells$(cell)(css+2:cse-1))) : _
			call$=srep$(call$,"\trgaph108\trleft-108","\trgaph"&x$) : _
			goto SETCELL

		! Designate header rows for repeat on subsequent pages
		! Rows must be t first row(s) in t table and contiguous

		if pos(ccells$(cell)(css:cse-1),"trh")=1 then : _
			call$=call$&"\trhdr" : _
			goto SETCELL

		! Designate row height in postive inches for AT LEAST height
		! use negative inches for absolute height

		if pos(ccells$(cell)(css:cse-1),"trrh")=1 then : _
			call$=call$&"\trrh"&fn_twips$(val(ccells$(cell)(css+4:cse-1))) : _
			goto SETCELL

		! Set font size in points


		if pos(ccells$(cell)(css:cse-1),"fs")=1 and cnum=0 then : _
			cfont$="\fs"&str$(val(ccells$(cell)(css+2:cse-1))*2)&"" : _
			goto SETCELL

		! Set font type face


		if uprc$(ccells$(cell)(css:cse-1))="FTIMES" and cnum=0 then : _
			cface$="\f0" : _
			goto SETCELL
		if uprc$(ccells$(cell)(css:cse-1))="FARIAL" and cnum=0 then : _
			cface$="\f1" : _
			goto SETCELL
		if uprc$(ccells$(cell)(css:cse-1))="FCOURIER" and cnum=0 then : _
			cface$="\f2" : _
			goto SETCELL
		if uprc$(ccells$(cell)(css:cse-1))="FFRITZ" and cnum=0 then : _
			cface$="\f28" : _
			goto SETCELL
		if uprc$(ccells$(cell)(css:cse-1))="FOCR-A" and cnum=0 then : _
			cface$="\f42" : _
			goto SETCELL
		if uprc$(ccells$(cell)(css:cse-1))="FPALATINO" and cnum=0 then : _
			cface$="\f29" : _
			goto SETCELL
		if uprc$(ccells$(cell)(css:cse-1))="FLUCIDA" and cnum=0 then : _
			cface$="\f69" : _
			goto SETCELL

		! Build formats for each Cell left to right


		if pos(ccells$(cell)(css:cse-1),"c")=1 then
			cnum+=1
			mat cfmt$(cnum): mat csize$(cnum) : mat csize(cnum)
			cfmt$(cnum)="" : mat cface$(cnum) : mat cfont$(cnum)
			cface$(cnum)=cface$
			cfont$(cnum)=cfont$
			csize(cnum)=val(fn_twips$(val(ccells$(cell)(css+1:cse-1))))
! cSIZE$(CNUM)="\cellx"&STR$(SUM(CSIZE)+CINDENT)
			goto SETCELL
			csize$(cnum)="\cellx"&str$(sum(csize)+cindent)
			goto SETCELL
		end if

		! Determine border width from 0 to 5 (0 to 75 TWIPS)


		if pos(ccells$(cell)(css:cse-1),"br")=1 then
			brdr$=""
			if pos(cells$(cell)(css+2:cse),"s")>0 then brdrtyp$="s" else : _
				if pos(cells$(cell)(css+2:cse),"o")>0 then brdrtyp$="o" else : _
					if pos(cells$(cell)(css+2:cse),"d")>0 then brdrtyp$="d" else : _
						if pos(cells$(cell)(css+2:cse),"a")>0 then brdrtyp$="a" else : _
							brdrtyp$="s"
			brdrw=min(5,val(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(srep$(ccells$(cell)(css:cse-1),"b",""),"t",""),"r",""),"l",""),"b",""),"a",""),"s",""),"d",""),"o","")))*15
			if (x:=pos(ccells$(cell)(css+2:cse-1),"t"))>0 then : _
				brdr$="\clbrdrt"&fn_brdr$(brdrtyp$)&"\brdrw"&str$(brdrw)&" "
			if (x:=pos(ccells$(cell)(css+2:cse-1),"r"))>0 then : _
				brdr$=brdr$&"\clbrdrr"&fn_brdr$(brdrtyp$)&"\brdrw"&str$(brdrw)&" "
			if (x:=pos(ccells$(cell)(css+1:cse-1),"l"))>0 then : _
				brdr$=brdr$&"\clbrdrl"&fn_brdr$(brdrtyp$)&"\brdrw"&str$(brdrw)&" "
			if (x:=pos(ccells$(cell)(css+1:cse-1),"b"))>0 then : _
				brdr$=brdr$&"\clbrdrb"&fn_brdr$(brdrtyp$)&"\brdrw"&str$(brdrw)&" "
			csize$(cnum)=brdr$&csize$(cnum)
			goto SETCELL
		end if
 : _
		! Set vertical alignment for each cell                         : _


		if pos(ccells$(cell)(css:cse-1),"v")=1 then
			if pos(ccells$(cell)(css+1:cse-1),"t")=1 then csize$(cnum)=srep$(csize$(cnum),"\cellx","\clvertalt\cellx")
			if pos(ccells$(cell)(css+1:cse-1),"c")=1 then csize$(cnum)=srep$(csize$(cnum),"\cellx","\clvertalc\cellx")
			if pos(ccells$(cell)(css+1:cse-1),"b")=1 then csize$(cnum)=srep$(csize$(cnum),"\cellx","\clvertalb\cellx")
			goto SETCELL
		end if
 : _
		! Set horizontal alignment of each cell                        : _


		if pos(ccells$(cell)(css:cse-1),"h")=1 then
			if pos(ccells$(cell)(css+1:cse-1),"r")=1 then cfmt$(cnum)=cfmt$(cnum)&"\qr"
			if pos(ccells$(cell)(css+1:cse-1),"c")=1 then cfmt$(cnum)=cfmt$(cnum)&"\qc"
			if pos(ccells$(cell)(css+1:cse-1),"l")=1 then cfmt$(cnum)=cfmt$(cnum)&"\ql"
			goto SETCELL
		end if
 : _
		! Set t cell font if different than t default              : _


		if uprc$(ccells$(cell)(css:cse-1))="FTIMES" then : _
			cface$(cnum)="\f0 " : _
			goto SETCELL
		if uprc$(ccells$(cell)(css:cse-1))="FARIAL" then : _
			cface$(cnum)="\f1 " : _
			goto SETCELL
		if uprc$(ccells$(cell)(css:cse-1))="FCOURIER" then : _
			cface$(cnum)="\f2 " : _
			goto SETCELL
		if uprc$(ccells$(cell)(css:cse-1))="FFRITZ" then : _
			cface$(cnum)="\f28 " : _
			goto SETCELL
		if uprc$(ccells$(cell)(css:cse-1))="FPALATINO" then : _
			cface$(cnum)="\f29 " : _
			goto SETCELL
		if uprc$(ccells$(cell)(css:cse-1))="FOCR-A" then : _
			cface$(cnum)="\f42 " : _
			goto SETCELL
		if uprc$(ccells$(cell)(css:cse-1))="FLUCIDA" then : _
			cface$(cnum)="\f69 " : _
			goto SETCELL
 : _
		! Set t cell font if different than t default              : _


		if pos(ccells$(cell)(css:cse-1),"fs")=1 and cnum>0 then : _
			cfont$(cnum)="\fs"&str$(val(ccells$(cell)(css+2:cse-1))*2)&" " : _
			goto SETCELL
 : _
		! Set t backround shading of each cell                       : _


		if pos(ccells$(cell)(css:cse-1),"sh")=1 then
			csize$(cnum)=srep$(csize$(cnum),"\cellx","\clshdng"&str$(val(ccells$(cell)(css+2:cse-1))*100)&" "&"\cellx")
			goto SETCELL
		end if
		goto SETCELL
	end if
PRINT_CELLS: !

	! Start t printing of t CELL row


	mat celldata$(cnum) : _
		: _
	! Make t data matrix t same size as t number of cells


! INPUT FIELDS "24,64,c 1": PAUSE$
 celldef$=call$
 : _
	! Define t cell row and number and size of cells


	for a=1 to cnum
		if a=cnum then celldef$=celldef$&csize$(a)&crlf$ else celldef$=celldef$&csize$(a)
	next a
 : _
	! Print t cells


	print #prntfil: cface$&cfont$&celldef$
	for a=1 to cnum
		if a=1 then print #prntfil: "\pard \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0"&crlf$
		print #prntfil: cfmt$(a)&"{"&cface$(a)&cfont$(a)&srep$(srep$(celldata$(a),"|","}{ \tab }{ "),"[RTFBAR]","|")&"\cell }"&crlf$
		if a=cnum then print #prntfil: "\pard \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0"&crlf$
	next a
	print #prntfil: "{"&celldef$&crlf$&"\row }"&crlf$
	print #prntfil: "\pard \ql \li0\ri0\widctlpar\aspalpha\aspnum\faauto\adjustright\rin0\lin0"&crlf$
 laststyle=1
fnend
def fn_brdr$(btype$)
 fn_brdr$=""
	if btype$="s" then fn_brdr$="\brdrs"
	if btype$="o" then fn_brdr$="\brdrdot"
	if btype$="a" then fn_brdr$="\brdrdash"
	if btype$="d" then fn_brdr$="\brdrdb"
fnend
def fn_twips$(inch)
	if inch>0 then twips$=str$(round(inch*1440,0))
	if inch<0 then twips$="-"&str$(round(abs(inch)*1440,0))
	if inch=0 then twips$=""
 fn_twips$=twips$
fnend

SET_SPECFILE: ! r:
sps=pos(data$,"[SPEC(")+6
spe=pos(data$,")]",sps)-1
if not setspec then goto L55260
if exists(picpath$&trim$(data$(sps:spe))) then specfile$=picpath$&trim$(data$(sps:spe)) else pause : msgbox("Designated SPEC file does not exist.") : input fields "10,10,c 10": pause$
L55260: data$=srep$(data$,"[SPEC("&data$(sps:spe)&")]","")
! PAUSE
return ! /r
def library fntype(infile$*100,outfile)
	!
	! Moves t contents of one file into another without using
	! the BR TYPE command. OUTFILE must be an existing open file.
	!
! INPUT FIELDS "23,60, C1": PAUSE$
 infile=30
L60030: !
	if file(infile)<0 then goto L60050
 infile+=1
	goto L60030
L60050: open #infile: "name="&infile$&",recl=1",external,input
 infile_lrec=lrec(infile)
	close #infile:
 infile_recl=min(32000,infile_lrec)
	open #infile: "name="&infile$&",RECL="&str$(infile_recl),external,input,relative
 infile_rec=0
 infile_frm$="form C "&str$(infile_recl)
	if infile_recl=32000 then
L60130: !
		if infile_rec*infile_recl+infile_recl<=infile_lrec then
			read #infile,using infile_frm$: inrec$
			print #outfile: inrec$
			infile_rec+=1
			goto L60130
		else
			infile_frm$="form C "&str$(infile_lrec-infile_rec*32000)
			close #infile:
			open #infile: "name="&infile$&",RECL="&str$(infile_lrec-infile_rec*32000),external,input,relative
		end if
	end if
	read #infile,using infile_frm$,pos=infile_rec*infile_recl+1: inrec$
	print #outfile: inrec$
	dim inrec$*32000
ZTYPE: close #infile: !
 infile=0
fnend
