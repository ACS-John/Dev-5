pr 'this program ('&program$&') is not intended to be run directly.'
end
def fn_setup
	if ~setup_library then
		setup_library=1
		autoLibrary
	end if
	on error goto Ertn
	dim _program$(1)*255
	dim _menu$(1)*255
	dim combooptionsetlist$(0)*256
	tab$=hex$('09')
	setup=1
fnend

def library fnTos(;sn$*100)
	! screen ace top of screen function
	if ~setup then fn_setup
	if sn$='' then
		if env$('cursys')<>'' then
			sn$=env$('cursys')&'\'
		end if
		sn$=sn$&env$('program_caption')
	end if
	sn$=trim$(sn$&session$)
	tmp_combo_count_for_set=0
	combooptionsetlistcount=combooption_which_prior=combooption_which=0
	combokeycurrent$=combokeyprior$=''
	if len(sn$)>100 then : pr "INVALID FILE NAME: Too Long" : input fields "1,1,C 1,N": pause$ : goto Xit
	! close #119: ioerr ignore
	! open #119: "Name="&env$('temp')&'\acs\'&sn$&",RecL=1024,Replace",internal,outIn,relative	! recl was 500
	fn_clear_env(tmp_combo_count_for_read,tmp_combo_count_for_set)
	if env$('GUIMode')='OFF' then execute 'config GUI On'
fnend
def library fnLbl(myline,mypos,txt$*200; mylen,myalign,font_mod,container,tabcon,lbl_tooltip$*256)
	! add a label to a screen ace form	 ! fnLbl(l,p,t$*200; mylen,myalign,fm,c,tc)
	! myline	 vertical (that's up and down) Position of the Label.
	! mypos		 horizontal (left and right) position of the label.
	! txt$		 visible text/caption of the label.
	! mylen		 maximum length of the label (measured in characters)
	!					 if unspecified, it defaults to len(txt$)
	! myalign	 alignment of the text within the label
	!					 0=left,	1=right,	2=center
	if ~setup then fn_setup
	if mylen=0 then mylen=len(txt$)
	setenv('control'&str$(fn_control_count), "LABEL|"&str$(myline)&"|"&str$(mypos)&"|"&str$(mylen)&"|"&str$(myalign)&"|"&txt$&"|"&str$(container)&"|"&str$(tabcon)&"|"&str$(font_mod)&"|"&lbl_tooltip$)
fnend
def library fnPic(lyne,ps,height,width,picture$*300;con,tabcon)
	! add a picture/image to a screen ace form
	if ~setup then fn_setup
	setenv('control'&str$(fn_control_count), "PICTURE|"&str$(lyne)&"|"&str$(ps)&"|"&str$(width)&"|"&str$(height)&"|"&picture$&"|")
fnend
def library fnTxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
	if ~setup then fn_setup
	! screen ace text box
	if lwrc$(mask$)='mmddyy' then
		mask$='1'
	else if lwrc$(mask$)='ccyymmdd' then
		mask$='3'
	else if lwrc$(mask$)='pointtwo' then
		mask$='32'
	else if lwrc$(mask$)='number' then
		mask$='30'
	else if lwrc$(mask$)='currency' then
		mask$='10'
	end if
	if mask$='1' then
		width=8
	else if mask$='2' then
		width=10
	else if mask$='3' then
		width=10
	else if mask$='4' then
		width=10
	else if mask$='5' then
		width=8
	end if
	if val(mask$)>=10 and val(mask$)<=49 then ali=1
	if maxlen=0 then maxlen=width
	setenv('control'&str$(fn_control_count), "TEXT|"&str$(lyne)&"|"&str$(ps)&"|"&str$(width)&"|"&str$(maxlen)&"|"&str$(ali)&"|"&str$(disable)&"|"&mask$&"|"&tooltip$&"|"&str$(contain)&"|"&str$(tabcon)&"|"&addtomask$&"|")
fnend
def library fnMultiLine(lyne,ps,height,width;contain,tabcon,tooltip$*200)
	! add a multiline text box to a screen ace form
	if ~setup then fn_setup
	setenv('control'&str$(fn_control_count), "MULTILINE|"&str$(lyne)&"|"&str$(ps)&"|"&str$(height)&"|"&str$(width)&"|"&tooltip$&"|"&str$(contain)&"|"&str$(tabcon)&"|")
	lyne=ps=height=width=contain=tabcon=0
	tooltip$=""
fnend
def library fnOpt(lyne,ps, txt$*196; align,contain,tabcon)
	! lyne			vertical (that's up and down) Position of the Option.
	! ps				horizontal (left and right) position of the option.
	! txt$			visible text/caption
	! align			0=left,	 1=right
	! contain		container number (for containers like frames and tab strips)
	if ~setup then fn_setup
	if align=1 then ps=ps-len(rtrm$(txt$))
	setenv('control'&str$(fn_control_count), "OPTION|"&str$(lyne)&"|"&str$(ps)&"|"&str$(align)&"|"&txt$&"|"&str$(contain)&"|"&str$(tabcon)&"|")
fnend
def library fnChk(lyne,ps,txt$*196; align,contain,tabcon,chk_disable)
	! add a screen ace check box
	if ~setup then fn_setup
	if align=1 then ps=ps-len(rtrm$(txt$))-2
	setenv('control'&str$(fn_control_count), "CHECK|"&str$(lyne)&"|"&str$(ps)&"|"&str$(align)&"|"&txt$&"|"&str$(contain)&"|"&str$(tabcon)&"|"&str$(chk_disable)&"|")
fnend
def library fnComboA(sfn$*256,lyne,ps,mat opt$;ttt$*200,width,contain,tabcon)
	if ~setup then fn_setup
	fnComboA=fn_comboA(sfn$,lyne,ps,mat opt$, ttt$,width,contain,tabcon)
fnend
def fn_comboA(sfn$*256,lyne,ps,mat opt$;ttt$*200,width,contain,tabcon,comboa_combooptionset$*256)
	if env$('exitnow')='yes' then goto COMBOA_COMPLETE ! special processing to increase speed for exitnow
	! add a combo box (populated from an array) to a screen ace form
	! sfn$			simple file name
	!						(used to store options in to be passed to win6)
	! mat opt$	choices in the combobox and one of them is your answer
	! width			(optional) sets the field/max length of the combobox.
	!						may not be larger than 81
	! __________________________________________________
	! ** get/set constants ********
	! fncno(cno)
	sfn$=trim$(sfn$)&env$('cno')
	if width=0 then
		for j=1 to udim(mat opt$)
			width=max(width,len(opt$(j)))
		next j
	end if
	fn_add_combo_option_list('','',1)
	for rec_count=1 to udim(mat opt$)
		fn_add_combo_option_list(opt$(rec_count)(1:81),opt$(rec_count)(1:81))
	next rec_count
	setenv('control'&str$(fn_control_count),"COMBOA|"&str$(lyne)&"|"&str$(ps)&"|"&str$(width)&"|0|"&sfn$&"[SESSION].tmp|1|"&ttt$&"|"&str$(contain)&"|"&str$(tabcon)&"|"&comboa_combooptionset$&"|")
	fn_comboOptionSetList_add(comboa_combooptionset$)
	width=contain=0
	COMBOA_COMPLETE: !
fnend
def library fnComboF(sfn$*100,lyne,ps,width,df$*200,psk,lnk,psd,lnd; if$*200,limlis,urep,ttt$*200,contain,tabcon,keyFormat$)
	if env$('exitnow')='yes' then goto COMBOF_COMPLETE ! special processing to increase speed for exitnow
	! add a combo box (populated from a file) to a screen ace form
	if ~setup then fn_setup
	if keyFormat$='' then keyFormat$='C'
	dim key$*30,desc$*120,form$*200,becky$*199
	dim ml$(10)*256
	! df$ (data file) must be internal br format
	! psk (key position)=your key's/answer's starting pos
	! lnk (key length)
	! psd (description position)
	! lnd (description length)
	! if$ (index file)
	! limlis (limit to list)=0=no,	1=yes,	2=yes, but add a [all] option
	! urep	(use or replace)=0=make new/replace,	1=use if possible

	! **** get/set constants **********
	lnk=min(lnk,30) : lnd=min(lnd,60)
	width=min(width,81) : sfn$=trim$(sfn$) : df$=trim$(df$)
	if$=trim$(if$)
	form$="Form Pos "&str$(psk)&","&keyFormat$&" "&str$(lnk) : nodesc=1
	if psd<>0 and lnd<>0 then
		form$=form$&",Pos "&str$(psd)&",C "&str$(lnd) : nodesc=0
	end if
	becky$=sfn$&env$('cno')&"[SESSION].tmp" ! combof_whr$=env$('temp')&'\acs\'&becky$
	! __________________________________________________
	if width=0 then width=lnk+lnd+1
	dim combokeycurrent$*512,combokeyprior$*512
	combokeycurrent$='df='&df$&',if='&if$&',psk='&str$(psk)&',lnk='&str$(lnk)&',psd='&str$(psd)&'lnd='&str$(lnd)&',limlis='&str$(limlis)
	!			if combokeycurrent$=combokeyprior$ and combokeyprior$<>'' then
	!				acol_env_variable$='tmp_combo'&str$(tmp_combo_count_for_set+=1)
	!				setenv(acol_env_variable$&'_key','dupe') ! XXX env$(acol_env_variable_prior$&'_key')) ! pr 'adding first key: '&key$ : pause
	!				setenv(acol_env_variable$,'dupe') ! XXX env$(acol_env_variable_prior$))
	! !			pr 'saved time here ';savedtimecount+=1
	!				goto EODF
	!			else
	if if$="" then
		open #df:=fngethandle: "Name="&df$&",Shr",internal,input
	else
		open #df:=fngethandle: "Name="&df$&",KFName="&if$&",Shr",internal,input,keyed ioerr COMBOF_OPEN_IOERR
	end if
	fn_add_combo_option_list('','',1)
	if limlis=2 then
		fn_add_combo_option_list("[All]","[All]")
	end if
	do	! READ_DF: !
		if nodesc=0 then
			! pr form$ : pause
			if keyFormat$(1:1)='B' or keyFormat$(1:1)='N' or keyFormat$(1:2)='PD' then
				read #df,using form$: keyN,desc$ eof EODF ioerr ERR_READ
				key$=str$(keyN)
				! key$=cnvrt$(keyFormat$&' '&str$(lnk),keyN)
			else
				read #df,using form$: key$,desc$ eof EODF ioerr ERR_READ
			end if
			! pr key$,desc$
			fn_add_combo_option_list(rpad$(trim$(key$),lnk),rpad$(trim$(key$),lnk)&" "&desc$)
		else
			desc$=''
			read #df,using form$: key$ eof EODF ioerr ERR_READ
			fn_add_combo_option_list(rpad$(trim$(key$),lnk),rpad$(trim$(key$),lnk))
		end if
	loop

	ERR_READ: ! r:
	! this whole routine is untested because I've yet to see a need for it - but if there is a need for it - it should work...
	! just comment in the ioerrs on the reads in read_df above
	if err=61 then
		mat ml$(2)
		ml$(1)='Combo Box creation has encountered a locked record'
		ml$(2)='The locked record ('&str$(rec(df))&'will be skipped.'
		fnmsgbox(mat ml$, resp$, 'ComboBox Record Lock Error',0)
	else
		goto Ertn
	end if
	read #df,release:
	continue	! /r
	EODF: !
	acol_env_variable_prior$=acol_env_variable$
	combokeyprior$=combokeycurrent$
	close #df: ioerr ignore
	goto WRITE_IT
	COMBOF_OPEN_IOERR: ! r:
	pr '!! COMBOF_OPEN_ERROR !!'
	pr '	Name= '
	pr '		'&df$
	pr '	KFName= '
	pr '		'&if$
	pr '	err='&str$(err)&' on line '&str$(line)
	!
	fnpause
	goto WRITE_IT ! /r
	WRITE_IT: !
	setenv('control'&str$(fn_control_count),"COMBOF|"&str$(lyne)&"|"&str$(ps)&"|"&str$(width)&"|"&str$(lnk)&"|"&becky$&"|"&str$(limlis)&"|"&ttt$&"|"&str$(contain)&"|"&str$(tabcon)&"|"&combokeycurrent$&"|")
	fn_comboOptionSetList_add(combokeycurrent$)
	COMBOF_COMPLETE: !
fnend
def fn_comboOptionSetList_add(cosladd$*256)
	mat combooptionsetlist$(combooptionsetlistcount+=1)
	!		mat comboOptionItemList(comboOptionSetListCount)
	combooptionsetlist$(combooptionsetlistcount)=cosladd$
	!		comboOptionItemList(comboOptionSetListCount)=tmp_combo_count_for_set ! control_count
fnend
def fn_add_combo_option_list(key$*81,txt$*81; reset_only)
	key$=rtrm$(key$)
	txt$=rtrm$(txt$)
	if reset_only then
		acol_env_variable$='tmp_combo'&str$(tmp_combo_count_for_set+=1) ! control_count is not right - it's a response count
		setenv(acol_env_variable$,'') ! PR 'setting up '&acol_env_variable$
		acol_is_first=1
	else if acol_is_first then
		acol_is_first=0
		setenv(acol_env_variable$&'_key',key$) ! pr 'adding first key: '&key$ : pause
		setenv(acol_env_variable$,txt$)
	else
		setenv(acol_env_variable$&'_key',env$(acol_env_variable$&'_key')&'|'&key$)
		setenv(acol_env_variable$,env$(acol_env_variable$)&'|'&txt$)
	end if
fnend
def library fnFlexInit1(sfn$*100,lyne,ps,height,width,mat ch$;mat colMask$,seltype,usr,con,tabcon)
	! mat ch$		(column headers)=no more than 80 headers with 100 chrs each
	! mat colMask$		(column mask)=(see mask chart in screen ace manual)
	! seltype		0=editable cells,	 1=row selection,	 2=column selection
	! usr				(use or repl)=0=replace/build new,
	!												 >0=use previous (sorta disables fnFlexAdd1
	!												 =-1=append previous
	! sfn$			(simple file name) specific file you want flexgrid stored
	!						do not use an extension on the file name
	if ~setup then fn_setup
	if env$('exitnow')='yes' then goto FLEXINIT1_COMPLETE ! special processing to increase speed for exitnow
	dim hdrfile$*192,all_hdr$*6491,all_mask$*6491
	dim optfile$*199
	if usr=0 then grid_populated=0
	! __________________________________________________
	! if usr=0 then pr "USR=0-Replace"
	! if usr>0 then pr "USR>0-Use Previous USR="&str$(usr)
	! if usr<0 then pr "USR<0-append=-1		 USR="&str$(usr)
	sfn$=rtrm$(sfn$)
	if sfn$='' then
		pr 'SFN$ is required for Flex grids'
		pr 'Press Enter to continue without the flex grid'
		pause
		goto FLEXINIT1_COMPLETE
	end if
	if ~setup then fn_setup
	! fncno(cno)
	all_hdr$=all_mask$=""
	! fn_get_flexhandle
	filenumber=fn_get_flexhandle(1)
	sfn$=trim$(sfn$)&env$('cno')
	optfile$=sfn$&"[SESSION].tmp"
	hdr_count=udim(ch$) : hdrfile$=sfn$&".hdr"
	if usr<>0 then goto USEPREVIOUS
	XRETRY: ! !print "Retrying delete here! (If you see this twice)"
	if exists(env$('temp')&'\acs\'&optfile$) then
		fnFree(env$('temp')&'\acs\'&optfile$)
	end if

	close #filenumber: ioerr ignore
	if exists(env$('temp')&'\acs\'&hdrfile$)<>0 then
		fnFree(env$('temp')&'\acs\'&hdrfile$)
	end if
	USEPREVIOUS: !
	if usr>0 and exists(env$('temp')&'\acs\'&optfile$) then
		fnFlexInit1=1 : goto WRITE_TO_ACE
	end if
	! _______________________
	! ***	 test validity of some stuff **********
	fnFlexInit1=555
	fnmakesurepathexists(env$('temp')&'\acs\'&hdrfile$)
	open #filenumber: "Name="&env$('temp')&'\acs\'&hdrfile$&",Size=0,Replace,EoL=CRLF,RecL=8000",display,output
	for j=1 to udim(mat colMask$)
		if trim$(colMask$(j))="" then colMask$(j)="80"
	next j
	for j=1 to udim(ch$) : all_hdr$=all_hdr$&ch$(j)&chr$(9) : next j
	for j=1 to udim(mat colMask$) : all_mask$=all_mask$&colMask$(j)&chr$(9) : next j
	pr #filenumber,using "Form pos 1,C "&str$(len(all_hdr$)): all_hdr$
	pr #filenumber,using "Form pos 1,C "&str$(len(all_mask$)): all_mask$
	close #filenumber:
	! hdrfile name is expected by screen ace to be the same name as
	! .	 ! optfile$ only with the added .hdr extenstion
	if usr>0 and exists(env$('temp')&'\acs\'&optfile$)<>0 then
		fnFlexInit1=1 : goto WRITE_TO_ACE
	end if
	! __________________________________________________
	fnFlexInit1=0
	! if exists(env$('temp')&'\acs\'&optfile$) then
	! 	fnFree(env$('temp')&'\acs\'&optfile$)
	! end if
	open #filenumber: "Name="&env$('temp')&'\acs\'&optfile$&",Size=0,Replace,EoL=CRLF,RecL=6491",display,output
	WRITE_TO_ACE: !
	sorttype=0
	setenv('control'&str$(fn_control_count),"FLEX|"&str$(lyne)&"|"&str$(ps)&"|"&str$(height)&"|"&str$(width)&"|2|"&str$(seltype)&"|"&str$(sorttype)&"|"&sfn$&"|"&str$(hdr_count)&"|"&str$(con)&"|"&str$(tabcon)&"|")
	usr=0
	FLEXINIT1_COMPLETE: !
fnend
def library fnFlexAdd1(mat item$) ! this function may need to be updated to save data in a work file for re-adding later; this is due to error 980 when closing a list with all records filtered; Gordon should fix -- 5/12/14
	! add a line to a flexgrid on a screen ace form
	if env$('exitnow')='yes' then goto FLEXADD1_COMPLETE ! special processing to increase speed for exitnow
	if ~setup then fn_setup
	dim all_item$*6491
	mat2str(mat item$,all_item$,hex$('09'))
	flexhandle=fn_get_flexhandle
	grid_populated+=1
	pr #flexhandle,using "Form pos 1,C "&str$(len(all_item$)): all_item$ ioerr ignore
	FLEXADD1_COMPLETE: !
fnend
def library fnFra(lyne,ps,hi,wd; cap$*128,tooltip$*300,contain,tabcon)
	! add a frame to a screen ace form
	if ~setup then fn_setup
	setenv('control'&str$(fn_control_count),"FRAME|"&str$(lyne)&"|"&str$(ps)&"|"&str$(hi)&"|"&str$(wd)&"|"&cap$&"|"&tooltip$&"|"&str$(contain)&"|"&str$(tabcon)&"|")
fnend
def library fnTab(myline,mypos,height,width,mat cap$)
	! myline sets the vertical (up and down) position
	! mypos sets the horizontal (left and right) position
	! height/width	-	 duh
	! tabsperline=(tpl)=how many tabs will fit on each row of tabstrip
	! mat cap$=the captions on the tabs you want.
	! udim(mat cap$) will set the number of tabs
	! each tab caption should not be longer than 80 characters
	! no more than 99 tabs
	if ~setup then fn_setup
	open #tf1:=fngethandle: "Name="&env$('temp')&"\tab.txt,size=0,RecL=80,replace",internal,output
	for j=1 to udim(mat cap$)
		write #tf1,using "Form Pos 1,C 80": cap$(j)(1:80)
	next j
	close #tf1:
	setenv('control'&str$(fn_control_count),"TAB|"&str$(myline)&"|"&str$(mypos)&"|"&str$(height)&"|"&str$(width)&"|"&env$('temp')&"\|tab.txt|")
fnend
def library fnCmdKey(caption$*200,returnkey; default,cancel,tt$*200)
	! add a button to a screen ace form
	if ~setup then fn_setup
	setenv('control'&str$(fn_control_count),"CMDKEY|"&caption$&"|"&str$(returnkey)&"|"&str$(default)&"|"&str$(cancel)&"|"&tt$&"|")
fnend
def library fnCmdSet(bon)
	if ~setup then fn_setup
	if bon=1 then
		fnCmdKey("&Cancel",5,1,1)
	else if bon=2 then
		fnCmdKey("&Next",1,1)
		fnCmdKey("&Cancel",5,0,1)
	else if bon=3 then
		fnCmdKey("&Print",1,1)
		fnCmdKey("&Cancel",5,0,1)
	else if bon=4 then
		fnCmdKey("&Save",1,1)
		fnCmdKey("&Cancel",5,0,1)
	else if bon=5 then
		fnCmdKey("&Next",1,1)
		fnCmdKey("&Cancel",5,0,1)
		fnCmdKey("&Search",6)
	else if bon=6 then
		fnCmdKey("&Next",1,1)
		fnCmdKey("&Back",2)
		fnCmdKey("&Cancel",5,0,1)
	else if bon=7 then
		fnCmdKey("&Save",1,1)
		fnCmdKey("&Delete",4)
		fnCmdKey("&Cancel",5,0,1)
	else if bon=8 then
		fnCmdKey("&Print",1,1)
		fnCmdKey("&Back", 2)
		fnCmdKey("&Cancel",5,0,1)
	else if bon=11 then
		fnCmdKey("&Next",1,1)
		fnCmdKey("&Finish",5,0,1)
	else if bon=13 then
		fnCmdKey("&Next",1,1)
		fnCmdKey("&Add",2)
		fnCmdKey("&Cancel",5,0,1)
	else if bon=14 then
		fnCmdKey("&Add",1)
		fnCmdKey("E&dit",2,1)
		fnCmdKey("&Print",4)
		fnCmdKey("&Cancel",5,0,1)
	else if bon=15 then
		fnCmdKey("&Add",1,1)
		fnCmdKey("&Cancel",5,0,1)
	else if bon=17 then
		fnCmdKey("&Next",1,1)
		fnCmdKey("&Skip",2,0,1)
		fnCmdKey("&Finish",3)
	else if bon=19 then
		fnCmdKey("&Next",1,1)
		fnCmdKey("&Finish",2)
		fnCmdKey("&Cancel",5,0,1)
	else if bon=21 then
		fnCmdKey("&Print",1,1)
		fnCmdKey("&Search", 2)
		fnCmdKey("&Cancel",5,0,1)
	else if bon=22 then
		fnCmdKey("&Next",1,1)
		fnCmdKey("&Back",2)
		fnCmdKey("&Cancel",5,0,1)
	else if bon=23 then
		fnCmdKey("&Add",1,1)
		fnCmdKey("&Search",2)
		fnCmdKey("&Finish",4)
		fnCmdKey("&Cancel",5,0,1)
	else if bon=41 then
		fnCmdKey("&Ok",1,1,1)
	else if bon=52 then
		fnCmdKey("&Finish",5,1,1)
	else if bon=102 then
		fnCmdKey("&Print",1,1)
		fnCmdKey("E&dit",3)
		fnCmdKey("&Add",4)
		fnCmdKey("&Delete",7)
		fnCmdKey("&Refresh",6)
		fnCmdKey("&Cancel",5,0,1)
	end if
fnend
def library fnButton(lyne,ps,txt$*200,comkey; tt$*200,height,width,container,tabcon,default,cancel)
	! adds a screen ace button
	! mylen		 button.width
	! txt$		 button.caption
	! tt$			 button.tooltiptext
	if ~setup then fn_setup
	height=max(height,1) ! button height is at least 1
	if width=0 then width=len(txt$)
	setenv('control'&str$(fn_control_count),"BUTTON|"&str$(lyne)&"|"&str$(ps)&"|"&str$(height)&"|"&str$(width)&"|"&str$(comkey)&"|"&txt$&"|"&tt$&"|"&str$(default)&"|"&str$(cancel)&"|"&str$(container)&"|"&str$(tabcon)&"|")
fnend
def library fnPicBut(lyne,ps,txt$*40,comkey,pic1$*100,btnh,btnw; pic2$*100,tt$*150,container,tabcon,default,cancel,___,tmpControlX$*2048)
	if ~setup then fn_setup
	! mylen		 button.width
	! txt$		 button.caption
	! tt$			 button.tooltiptext
	btnh=max(btnh,1) ! button height is at least 1
	if btnw=0 then btnw=len(txt$)

	tmpControlX$&='PicBut|'
	tmpControlX$&=str$(lyne)&'|'
	tmpControlX$&=str$(ps)&'|'
	tmpControlX$&=txt$&'|'
	tmpControlX$&=str$(comkey)&'|'
	tmpControlX$&=pic1$&'|'
	tmpControlX$&=str$(btnh)&'|'
	tmpControlX$&=str$(btnw)&'|'
	tmpControlX$&=pic2$&'|'
	tmpControlX$&=tt$&'|'
	tmpControlX$&=str$(container)&'|'
	tmpControlX$&=str$(tabcon)&'|'
	tmpControlX$&=str$(default)&'|'
	tmpControlX$&=str$(cancel)&'|'
	setenv('control'&str$(fn_control_count),tmpControlX$)
fnend

def library fnDisplayMenu(mat dmText$,mat _program$,mat _status$; ___,envControlValue$*10000,item)
	if ~setup then fn_setup
	for item=1 to udim(mat dmText$)
		envControlValue$&=dmText$(item)&'~~~'&_program$(item)&'~~~'&_status$(item)&'###'
	next item
	setenv('control'&str$(fn_control_count),"menu|"&envControlValue$(1:len(envControlValue$)-3))
fnend
def library fnClearMenu
	fnClearMenu=fn_clearMenu
fnend
def fn_clearMenu
	mat _m$(0) : mat _p$(0) : mat _s$(0)
	display menu: mat _m$,mat _p$,mat _s$
fnend
def library fnAcs(mat resp$,&ckey; startfield,close_on_exit,parent_none,disabled_background)
	if ~setup then fn_setup
	dim txt$*201,path1$*300,tt$*400,tabline$*8019
	dim cap$*128 ! caption / title bar text
	dim addtomask$*40
	! on=1

	acs=1
	tabcon=0
	! if debug=1 then pr newpage

	for j=1 to udim(mat resp$) : resp$(j)=rtrm$(resp$(j)) : next j
	cap$=env$('Program_Caption')
	fn_get_flexhandle(1)
	! do we even need this line - it screws up other things.  Does removing it screw anything up?
	! yeah it screws things up to take it out - repetative flex grids
	fn_ace(sn$,unused,mat resp$,ckey,startfield,close_on_exit,parent_none,disabled_background)
	goto Xit
	Ertn: !
	fnerror(program$,err,line,act$,"Xit")
	if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
	execute "List -"&str$(line)
	pause
	ERTN_EXEC_ACT: !
	execute act$ : goto ERTN
	! please note for program pause to work the way we want Xit: and fnend must be on the same line.!
Xit: fnend
def fn_ace_init
	ace_io_count=ace_lyne_max=ace_column_max=grid_present=tmp_combo_count_for_read=0
	grid_index=date_boxes=respc=dropdown_menu_present=0 ! response counter
	date_fkey_base=2600 : file_select_fkey_base=2700 : txtbox_fkey=300
	mat ace_io$(0) : mat ace_typ$(0) : mat ace_resp$(0) : mat tabs(0,3)
	mat text_masks(0) : mat control$(0) : mat return_keys(0) : mat frames(0,4)
	mat date_fielddata(0,6) : mat file_select_data(0,2)
	grid_filter$=''
	scr_freeze
	resp_size=udim(mat resp$)
	button_win_width=0
	control_count=val(env$('control_count'))
	if udim(mat resp$)<control_count then mat resp$(control_count)
	mat ace_typ$(control_count)
	Session_Rows=val(env$('session_rows'))
	Session_Cols=val(env$('Session_Cols'))
	if Session_Rows<=0 then
		Session_Rows=35
		setenv('Session_Rows',str$(Session_Rows))
	end if
	if Session_Cols<=0 then
		Session_Cols=115
		setenv('Session_Cols',str$(Session_Cols))
	end if
fnend
def fn_draw_windows
	Session_Rows=max(Session_Rows,ace_lyne_max+4)   : setenv('Session_Rows',str$(Session_Rows)) ! in case 35 rows is not enough

	Session_Cols=max(Session_Cols,ace_column_max+4) : setenv('Session_Cols',str$(Session_Cols)) ! in case 115 columns is not enough
	if Session_Rows>35 or Session_Cols>115 or env$('cursys')='CM' then
		if env$('force80x24')='Yes' then
			Session_Rows=24
			setenv('Session_Rows',str$(Session_Rows))
			Session_Cols=80
			setenv('Session_Cols',str$(Session_Cols))
		end if
		open #0: 'SRow=1,SCol=2,Rows='&str$(Session_Rows)&",Cols="&str$(Session_Cols)&',Picture='&env$('background_picture')&',border=S:[screen],N=[screen],buttonRows=0',display,outIn
		! if login_name$='jbowman' then pr 'i opened zero, jbowman.' : pause
	end if
	dim borderText$*256
	if env$('acsProduct')='' then borderText$='ACS 5 ' else borderText$=env$('acsProduct')&' '
	if env$('enableClientSelection')='Yes' then borderText$&='- '&env$('client')&' '

	if session$(3:3)<>'1' then
		borderText$&='(Session '&session$(len(session$):len(session$))&') '
	end if
	dim systemName$*128
	systemName$=fnSystemNameFromAbbr$(env$('cursys'))
	if systemName$=cap$ then ! pr border:
		borderText$&='- '&systemName$
	else
		borderText$&='- '&systemName$&' - '&cap$
	end if
	if env$('cursys')<>'CM' then
		pr #0, border: borderText$
	end if

	fn_company_name(0,Session_Cols) ! fn_company_name(0,Session_Cols,trim$(cap$(1:pos(cap$,'(')-1))) ! fnSystemNameFromAbbr$(cursys$))

	if not grid_present then
		row=ceil((Session_Rows-ace_lyne_max)/2 )
		col=ceil((Session_Cols-ace_column_max)/2 )
		rows=ace_lyne_max
		cols=max(ace_column_max+2,button_win_width+2)
	else
		row= 3
		col= 2
		rows=Session_Rows-6
		cols=Session_Cols-2
	end if
	if parent_none then
		!			if disabled_background then open #disable_win=fngethandle: "srow=2,scol=2,rows="&str$(Session_Rows-2)&",cols="&str$(Session_Cols-2)&",picture=S:\Core\disable.png:TILE",display,output
		!			if disabled_background then open #disable_win=fngethandle: "srow=1,scol=1,rows="&str$(Session_Rows+2)&",cols="&str$(Session_Cols+1)&",border=none,picture=S:\Core\disable.png:TILE",display,output
		if disabled_background then let fn_backgrounddisable(1)
		open #acs_win:=fngethandle: "SRow="&str$(row)&",SCol="&str$(col)&",Rows="&str$(rows+3)&",Cols="&str$(cols+2)&",parent=none,Caption="&cap$&",border=S:[screen],N=[screen]",display,outIn
		open #button_win:=fngethandle: "SRow="&str$(rows+2)&",SCol=2,Rows=1,Cols="&str$(cols)&",parent="&str$(acs_win)&",border=S:[screen],N=[screen]",display,outIn
	else
		open #acs_win:=fngethandle: "SRow="&str$(row)&",SCol="&str$(col)&",Rows="&str$(rows)&",Cols="&str$(cols)&",parent=0,Caption="&cap$&",border=S:[screen],N=[screen]",display,outIn
		open #button_win:=fngethandle: "SRow="&str$(row+rows+2)&",SCol="&str$(col)&",Rows=1,Cols="&str$(cols)&",parent=0,border=S:[screen],N=[screen]",display,outIn
	end if
	ace_cmdkey_ps=max(1,cols-button_win_width-3) ! -1  ! changed from -1 to -3 on 8/23/2018 to prevent error (-2 also worked) on UB transaction button drawing on display grid screen when delete button was added
	                                             ! -3 caused issues on PR Enter time sheets (actual time entry screen editmode=0,semp=1)
	                                             ! so max(1,...) was added on 8/31/18 to resolve that issue
fnend
def fn_default_cmb_options
	dim tmp_combo_key$(1)*81,tmp_combo_item$(1)*81
	for tmp_combo_key_item=1 to tmp_combo_count_for_set
		str2mat( env$('tmp_combo'&str$(tmp_combo_key_item)&'_key'),mat tmp_combo_key$,'|')
		str2mat( env$('tmp_combo'&str$(tmp_combo_key_item)),mat tmp_combo_item$,'|')

		tck_response_item=val(env$('tmp_combo'&str$(tmp_combo_key_item)&'_response_item'))
		tck_which=srch(mat tmp_combo_key$,trim$(resp$(tck_response_item))) ! do not turn this trim$ into a rtrm$ - it messes up in and out with same person selected during UB Customer 9/20/2018
		if tck_which>0 and tck_which<=udim(mat tmp_combo_item$) then
			resp$(tck_response_item)=tmp_combo_item$(tck_which)
		end if
	next tmp_combo_key_item
fnend
def fn_set_controls
	dim control$(1)*5095
	for index_=1 to control_count
		str2mat(env$('control'&str$(index_)),mat control$,"|")
		ace_typ$(index_)=typ$=uprc$(control$(1))
		if typ$="LABEL" then
			fn_ace_rd_label
		else if typ$="TEXT" then
			fn_ace_rd_text
		else if typ$="COMBOA" then
			fn_ace_rd_combo('A')
		else if typ$="COMBOF" then
			fn_ace_rd_combo('F')
		else if typ$="CHECK" then
			fn_ace_rd_check
		else if typ$="OPTION" then
			fn_ace_rd_option
		else if typ$="FLEX" then
			fn_ace_rd_flex
		else if typ$="BUTTON" then
			fn_ace_rd_button
		else if typ$="PICTURE" then
			fn_ace_rd_pic
		else if typ$="CMDKEY" then
			fn_ace_rd_cmdkey
		else if typ$="PICBUT" then
			fn_ace_rd_picbut
		else if typ$="MULTILINE" then
			fn_ace_rd_multiline
		else if typ$="FRAME" then
			fn_ace_rd_frame
		else if typ$="TAB" then
			fn_ace_rd_tab
		else if typ$='MENU' then
			fn_ace_rd_menu
		end if
	next index_
fnend
def fn_equalize_resp_arrays
	respc=0
	dim ace_resp$(1)*1024, grid_filter$*1024
	if ace_io_count>udim(mat resp$) then mat resp$(ace_io_count)
	if ace_io_count then
		if grid_present then
			resp_size+=1
			grid_index=fn_gridIndex
			mat ace_resp$(ace_io_count)
			if grid_index=1 then
				mat ace_resp$(2:ace_io_count)=resp$(1:ace_io_count-1)
			else if grid_index=ace_io_count then
				mat ace_resp$(1:ace_io_count-1)=resp$(1:ace_io_count-1)
			else
				mat ace_resp$(1:grid_index-1)=resp$(1:grid_index-1) : mat ace_resp$(grid_index+1:ace_io_count)=resp$(grid_index:ace_io_count-1)
			end if
		else
			mat ace_resp$(ace_io_count)=resp$(1:ace_io_count)
		end if
	end if
fnend
def fn_validateGridSelection
	grid_selection_valid=1
	if fkey=0 or (fkey=201 and pos(ace_io$(curfld),"list ")) or fkey=default_button_fkey then
		if grid_present and row_count then
			if udim(ace_io$)=2 then
				if fnArrayEmpty(mat ace_resp$) then
					msgbox("Please, click on a row to select it and repeat your attempt.")
					grid_selection_valid=0
				end if
			else
				if fnArrayEmpty(mat grid_row$) then
					msgbox("Please, click on a row to select it and repeat your attempt.")
					grid_selection_valid=0
				end if
			end if
		end if
	end if
	fn_validateGridSelection=grid_selection_valid
fnend
def fn_processUserInput(; ___,returnN)
	if fkey=0 or (fkey=201 and pos(ace_io$(curfld),"list ")) then
		fkey(default_button_fkey) ! activate the default button when enter is pressed
		ckey=fkey
	else if fkey=2501 then
		fn_export_grid
		fkey(-1) : returnN=1
	else if fkey=2502 then
		fn_print_grid
		fkey(-1) : returnN=1
		!		else if fkey=2503 then
		!			pr f gridspec$&",sort": 1
		!			fkey(-1) : returnN=1
	else if fkey > date_fkey_base and fkey < date_fkey_base+100 then
		_date$=ace_resp$( date_fielddata(fkey-date_fkey_base,5))
		row=date_fielddata(fkey-date_fkey_base,2)
		column=date_fielddata(fkey-date_fkey_base,3)+date_fielddata(fkey-date_fkey_base,4)+1
		ace_resp$(date_fielddata(fkey-date_fkey_base,5))=fnDateSelect$ (_date$,'mdy',row,column)
		fkey(-1)
		returnN=1
	else if fkey > file_select_fkey_base and fkey < file_select_fkey_base+100 then
		fn_selectFile( ace_resp$(file_select_data(fkey-file_select_fkey_base,1) ), file_select_data(fkey-file_select_fkey_base,2))
		fkey(-1)
		returnN=1
	else if fkey=93 then
		if env$('ACSDeveloper')<>'' then let setenv('ExitNow','yes')
		ckey=fkey_cancel
	else if fkey=98 then
		ckey=fkey
	else if fkey=99 then
		ckey=fkey_cancel
	else if fkey=1504 then ! 2504 then
		if cap$='Select Company' then
			help_cursys$='co'
		else
			help_cursys$=lwrc$(env$('CurSys'))
		end if
		execute 'system -M start http://planetacs.net/help/'&help_cursys$&'/'&srep$(trim$(cap$),' ','%20')&'.html'
		returnN=1
	else if fkey=1505 then
		fn_programProperties
		returnN=1
	else if (fkey=105 or fkey=124 or fkey=106 or fkey=125) and (pos(ace_io$(curfld),"list ") or pos(ace_io$(curfld),"list ")) then
		returnN=1
		if fkey=105 or fkey=124 then let setenv('current_grid_row','1')
	else if not srch(mat return_keys,fkey)>0 or fkey=92 or fkey=208 or fkey=txtbox_fkey then ! this means user switched tabs (fkey 92) or picked a value from a combo box (fkey 208)
		returnN=1
	else
		ckey=fkey
	end if
	fn_processUserInput=returnN
fnend
def fn_reformatUserInput
	!		if udim(resp$)<ace_io_count then mat resp$(ace_io_count)
	!		if udim(ace_resp$)<ace_io_count then mat ace_resp$(ace_io_count)
	for ace_resp_item=1 to ace_io_count
		if ace_resp_item<=udim(mat ace_resp$) and ace_resp_item<=udim(mat resp$) then
			if pos(ace_io$(ace_resp_item),'check ')>0 or pos(ace_io$(ace_resp_item),'radio ')>0 then
				if ace_resp$(ace_resp_item)(1:1)='^' then
					resp$(ace_resp_item)='True'
				else
					resp$(ace_resp_item)='False'
				end if
			else if date_index := fn_isDate(mat date_fielddata,ace_resp_item) > 0 then
				_mask=date_fielddata(date_index,6)
				if _mask=1 then
					date_format$="mdy"
				else if _mask=2 then
					date_format$="mdcy"
				else if _mask=3 then
					date_format$="cymd"
				else if _mask=4 then
					date_format$="dmcy"
				else if _mask=5 then
					date_format$="dmy"
				end if
				if date_format$<>'mdy' then
					_days=days(lpad$(trim$(ace_resp$(ace_resp_item)),6,'0'),'mdy')
					resp$(ace_resp_item)=date$(_days,date_format$)
				else
					resp$(ace_resp_item)=ace_resp$(ace_resp_item)
				end if
			else
				resp$(ace_resp_item)=rtrm$(ace_resp$(ace_resp_item))
			end if
		end if
	next ace_resp_item
fnend
def fn_errorRedirection
	if env$("ACSDeveloper")<>"" then
		on soflow goto ERTN
	else
		on soflow ignore
	end if
fnend
def fn_gridIndex(;___,index_,grid_idx)
	for index_=1 to udim(mat ace_io$)
		if pos(ace_io$(index_),'list ') then
			grid_idx=index_
		end if
	next index_
	fn_gridIndex=grid_idx
fnend
def fn_mainInput
	if grid_present then
		grid_index=fn_gridIndex
		PopulateGrid: !
		current_grid_row=val( env$('current_grid_row')) ! if current_grid_row then pr 'current_grid_row=';current_grid_row : pause
		if current_grid_row then
			curfld(grid_index,current_grid_row)
			setenv('current_grid_row','')
			input fields gridspec$&",rowcnt,all,nowait": grid_rows
		else
			curfld(grid_index,row_count)
		end if

		grid_filter$=''
		dim grid_row$(1)*10000
		if not grid_populated then ! file_nonempty then ! if no rows have been populated, we have to create one
			pr f gridspec$&",headers,[gridheaders]" : (mat _headings$,mat _widths,mat _forms$)
			mat grid_row$(udim(ace_resp$))=("")
			pr f gridspec$&",=": mat grid_row$
		end if


		! mat ace_io$(ace_io_count)
		! mat ace_resp$(ace_io_count)
		if udim(ace_io$)=2 then ! this is if the grid is the only control
			curfld(2) !	 if ~current_grid_row then let curfld(2) ! if grid is first control than set the focus to the filter box
			rinput fields mat ace_io$: mat ace_resp$, grid_filter$ error ignore ! error MainInput886Avoidance
			current_grid_row=0
			if udim(ace_resp$)>1 then
				mat ace_resp$(1:udim(ace_resp$)-1)=ace_resp$(2:udim(ace_resp$))
				mat ace_resp$(udim(ace_resp$)-1)
			else
				mat ace_resp$(ace_io_count)=resp$(1:ace_io_count)
			end if
		else ! this is if the grid is not the only control
			mat grid_row$(udim(_headings$))=("") ! this will contain the selected row data
			if not udim(ace_io$)-1=udim(ace_resp$) then
				mat ace_resp$(udim(mat ace_resp$)-1) ! this will contain the rest of the non-grid data
				ace_io_count-=1
			end if
			if grid_index=1 then ! the grid and filter are the FIRST two controls in ace_io$
				curfld(2) ! if grid is first control than set the focus to the filter box
				rinput fields mat ace_io$: mat grid_row$, grid_filter$, mat ace_resp$(2:udim(ace_resp$)) error IGNORE ! conv CONV_HANDLER
			else if grid_index=udim(ace_resp$) then ! the grid and filter are the LAST two controls in ace_io$
				curfld(1)
				rinput fields mat ace_io$: mat ace_resp$(1:udim(ace_resp$)-1), mat grid_row$, grid_filter$ conv CONV_HANDLER error ignore ! error IGNORE_886
			else ! the grid and filter are in the middle of other controls in ace_io$
				curfld(1)
				rinput fields mat ace_io$: mat ace_resp$(1:grid_index-1), mat grid_row$, grid_filter$, mat ace_resp$(grid_index+1:udim(ace_resp$)) conv CONV_HANDLER error IGNORE
			end if
			if udim(grid_row$)>1 then ace_resp$(grid_index)=grid_row$(2) else ace_resp$(grid_index)=""
		end if
	else if ace_io_count <> 0 then
		if fn_allFieldsProtected(mat ace_io$) then
			pr f mat ace_io$: mat ace_resp$
			input #0, fields str$(Session_Rows)&","&str$(Session_Cols)&',C 1' : dummy$ ! this is when a screen has no inputs, only labels and/or buttons
		else
			rinput fields mat ace_io$: mat ace_resp$ conv CONV_HANDLER
		end if
	else
		input #acs_win, fields str$(rows)&','&str$(cols)&',C 1' : dummy$ ! this is when a screen has no inputs, only labels and/or buttons
		! input #0, fields str$(Session_Rows)&","&str$(Session_Cols)&',C 1' : dummy$ ! this is when a screen has no inputs, only labels and/or buttons
	end if
fnend
def fn_ace(sn$*100, unused,mat resp$, &ckey;startfield, close_on_exit, parent_none,background)
	if env$('ExitNow')='yes' then
		ckey=fkey_cancel
		goto AceFinis
	end if
	dim ace_io$(1)*255
	fn_ace_init
	fn_windowSize
	fn_draw_windows
	fn_errorRedirection
	fn_set_controls
	fn_default_cmb_options
	fn_clear_env(tmp_combo_count_for_read,tmp_combo_count_for_set)
	fn_equalize_resp_arrays
	if not dropdown_menu_present then fn_clearMenu
	MAIN_INPUT: !
	fn_mainInput
	if fn_processUserInput or not fn_validateGridSelection then
		goto MAIN_INPUT
	end if
	if env$('ExitNow')='yes' then
		ckey=fkey_cancel
		goto AceFinis
	end if
	if ckey<>fkey_cancel then
		if not fn_validateFields then
			goto MAIN_INPUT
		end if
	end if
	fn_reformatUserInput
	on soflow system
	if udim(mat resp$)<resp_size then
		mat resp$(resp_size)
	end if
	AceFinis: !
	fkey(-1)
	fn_close_windows ! :display menu:
	if disabled_background then let fn_backgrounddisable(0)
fnend
def fn_close_windows
	if file(acs_win)<>-1 then close #acs_win:
	if file(button_win)<>-1 then close #button_win:
	if file(disable_win)<>-1 then close #disable_win:
	if Session_Cols then
		pr #0, fields "1,"&str$(Session_Cols-5)&",C 2;1,"&str$(Session_Cols-2)&",C 2": rpt$(' ',2), rpt$(' ',2)
	end if
fnend
CONV_HANDLER: ! r:
	dim message$(1)*255, response$*255,temp_io$(1)*255
	bad_field=cnt+1
	message$(1)='You have entered an incorrect value at field number '&str$(bad_field)
	mat temp_io$(udim(ace_io$))=ace_io$
	fnmsgbox(mat message$, response$, "Error!",0)
	fnpause
	mat ace_resp$(udim(resp$))=resp$
	mat ace_io$(udim(temp_io$))=temp_io$
	curfld(bad_field)
retry	 ! /r
def fn_isDate (mat date_data,ace_item;___,index_,returnN)
	for index_=1 to udim(mat date_data)
		if date_data(index_,5)=ace_item then
			returnN=index_
			goto IsDateFinis
		end if
	next index_
	IsDateFinis: !
	fn_isDate=returnN
fnend
def fn_programProperties
	autoLibrary
	dim temp_resp$(1)*512,temp_ace_resp$(1)*512
	mat temp_io$(udim(ace_io$))=ace_io$
	mat temp_resp$(udim(resp$))=resp$
	mat temp_ace_resp$(udim(ace_resp$))=ace_resp$
	temp_io_count=ace_io_count
	fnprogram_properties
	mat ace_io$ (udim(temp_io$))=temp_io$
	mat resp$(udim(temp_resp$))=temp_resp$
	mat ace_resp$(udim(temp_ace_resp$))=temp_ace_resp$
	ace_io_count=temp_io_count
fnend
def fn_allFieldsProtected(mat ace_io$; ___,returnN,index_)
	returnN=1
	for index_=1 to udim(mat ace_io$)
		if not pos(ace_io$(index_),',P') then returnN=0
	next index_
	fn_allFieldsProtected=returnN
fnend
def fn_clear_env(&tmp_combo_count_for_read,&tmp_combo_count_for_set; ___,index_,tmp_combo_item)
	for index_=1 to control_count
		setenv('control'&str$(index_),"")
		!			setenv('combo'&str$(index_),"")
	next index_
	for tmp_combo_item=1 to tmp_combo_count_for_set
		setenv('tmp_combo'&str$(tmp_combo_item),'')
		setenv('tmp_combo'&str$(tmp_combo_count_for_read)&'_response_item','')
		setenv('tmp_combo'&str$(tmp_combo_count_for_read)&'_key','')
	next tmp_combo_item
	tmp_combo_count_for_set=tmp_combo_count_for_read=0
	setenv('control_count',0)
fnend
def fn_windowSize(; ___,index_,tmpLine,tmpPos,tmpHeight,tmpWidth)
	!		frame_or_tab_present=0
	for index_=1 to control_count
		str2mat(env$('control'&str$(index_)),mat control$,"|")
		ace_typ$(index_)=typ$=uprc$(control$(1))
		if typ$="LABEL" then
			ace_lyne_max=max(ace_lyne_max,val(control$(2)))
			ace_column_max=max(ace_column_max, val(control$(3))+max(val(control$(4)),len(control$(6))))
		else if typ$="TEXT" or typ$="COMBOA" or typ$="COMBOF" then
			ace_lyne_max=max(ace_lyne_max,val(control$(2)))
			ace_column_max=max( ace_column_max, val(control$(3))+val(control$(4)) )
		else if typ$="CHECK" or typ$="OPTION" then
			ace_lyne_max=max(ace_lyne_max,val(control$(2))+2)
		 ! if index_=5 then pr 'it is 5' : pause
			ace_column_max=max( ace_column_max, val(control$(3))+len(control$(5))+4) ! max( ace_column_max, val(control$(3))+len(trim$(control$(5)))+4)
		else if typ$="FLEX" or typ$="MULTILINE" or typ$="FRAME" or typ$="TAB" then
			if typ$="FLEX" then grid_present=1
			ace_lyne_max=max(ace_lyne_max,val(control$(2))+val(control$(4)) )
			if typ$="FRAME" or typ$="TAB" and (not save_bottom or val(control$(2))+val(control$(4))>save_bottom) then ace_lyne_max += 1
			ace_column_max=max( ace_column_max, val(control$(3))+val(control$(5))    )
			save_bottom=val(control$(2))+val(control$(4))
		else if typ$="CMDKEY" then
			button_win_width +=(len(control$(2))+1)
		else if typ$="BUTTON" then
			ace_lyne_max=max(ace_lyne_max,val(control$(2)))
			ace_column_max=max( ace_column_max, val(control$(3))+val(control$(5)) )
		else if typ$="PICTURE" then
			tmpLine  	=val(control$(2))
			tmpPos   	=val(control$(3))
			tmpHeight	=val(control$(4))
			tmpWidth 	=val(control$(5))
			ace_lyne_max  =max(ace_lyne_max  ,tmpLine+tmpHeight)
			ace_column_max=max(ace_column_max,tmpPos+tmpWidth  )
		else if typ$="PICBUT" then
			tmpLine  	=val(control$(2))
			tmpPos   	=val(control$(3))
			tmpHeight	=val(control$(7))
			tmpWidth 	=val(control$(8))
			ace_lyne_max=max(ace_lyne_max,tmpLine+tmpHeight)
			ace_column_max=max( ace_column_max,tmpPos+tmpWidth)
		else if typ$="MENU" then
			dropdown_menu_present=1
		end if
	next index_
fnend
def fn_validateFields(;___,found_invalid,_idx)
	! requires local: mat text_masks,mat ace_resp$,mat resp$
	for _idx=1 to udim(text_masks)
		if text_masks(_idx)>=1 and text_masks(_idx)<=5 then
			if ace_resp$(_idx)<>'0' then
				if not fn_validate_mdy(ace_resp$(_idx)) then
					if val(resp$(_idx)) then
						msgbox('Date '&ace_resp$(_idx)&' at field #'&str$(curfld)&' is invalid. Previous value of '&resp$(_idx)&' will be restored')
						ace_resp$(_idx)=resp$(_idx)
					else
						msgbox('Date '&ace_resp$(_idx)&' at field #'&str$(curfld)&' is invalid.')
					end if
					found_invalid=1
					curfld(_idx) : fkey(-1)
				end if
			end if
		else if text_masks(_idx)>1000 and (trim$(ace_resp$(_idx))='' or trim$(ace_resp$(_idx))='0') then
			msgbox('Field '&str$(_idx)&' is required.')
			found_invalid=1
			curfld(_idx) : fkey(-1)
		end if
	next _idx
	fn_validateFields=~ found_invalid
fnend
def fn_validate_mdy(_date$;separator$,___,month,day,year)
	fn_validate_mdy=1
	_date$=lpad$(_date$,6,'0')
	month=val(_date$(1:2))
	day=val(_date$(3:4))
	year=val(_date$(5:6))
	if not (fn_validateMonth(month) and fn_validate_day(day,month,year) and fn_validate_year(year)) then let fn_validate_mdy=0
 ! FINISHEDVALIDATE_MDY: !
fnend
def fn_validateMonth(month; ___,returnN)
	returnN=1
	if month < 1 or month > 12 then let returnN=0
	fn_validateMonth=returnN
fnend
def fn_validate_day(day,month,year; ___,returnN)
	returnN=1
	if day<1 or day>fn_days_in_month(month,year) then returnN=0
	fn_validate_day=returnN
fnend
def fn_validate_year(year; ___,returnN)
	returnN=1
	if year < 0 or year > 99 then returnN=0
	fn_validate_year=returnN
fnend
def fn_days_in_month (month,year;___,returnN)
	returnN=date(days(date$(days(date$(str$(year)&lpad$(str$(month),2,"0")&"01"),"CCYYMMDD")+32,"CCYYMM01"),"CCYYMMDD")-1,"DD")
	fn_days_in_month=returnN
fnend
def fn_ace_rd_menu (;___,index_,item_count)
	dim menu_items$(1)*1023,menu_sub_items$(1)*255,_menu$(1)*255,_program$(1)*255,_status$(1)*255
	str2mat(control$(2),mat menu_items$,'###')
	item_count=udim(mat menu_items$)
	mat _menu$(item_count)=(''): mat _program$(item_count)=(''): mat _status$(item_count)=('')
	for index_=1 to udim(mat menu_items$)
		str2mat(menu_items$(index_),mat menu_sub_items$,'~~~')
		_menu$(index_)=menu_sub_items$(1)
		_program$(index_)=menu_sub_items$(2)
		_status$(index_)=menu_sub_items$(3)
	next index_
	display menu: mat _menu$,mat _program$,mat _status$
fnend
def fn_ace_rd_multiline
	respc+=1
	lyne=val(control$(2))
	ps=val(control$(3))
	height=val(control$(4))
	width=val(control$(5))
	tt$=control$(6)
	container=val(control$(7))
	tabcon=val(control$(8))
	fn_remove_crlf(resp$(respc))
	 !	 resp$(respc)=srep$(resp$(respc),'"','""') ! fn2quote(resp$(respc))
fnend
def fn_ace_rd_picbut(; ___,lyne$,pos$,comkey$,height$,width$,container,tabcon,default,cancel,txt$*256,path1$*300,tt$*400,tmpWin)
	lyne$    =    control$(2)
	pos$     =    control$(3)
	txt$     =    control$(4) !  not used
	comkey$  =    control$(5)
	path1$   =    control$(6)
	height$  =    control$(7)
	width$   =    control$(8)
	! pic2$  =    control$(9)  alternate depressed picture... not implemented
	tt$      =    control$(10)
	container=val(control$(11))
	tabcon   =val(control$(12))
	default  =val(control$(13))
	cancel   =val(control$(14))
	! path2$=control$(13)
	! pr #0, fields "1,2,P 1/2,[buttons],"&str$(returnkey): env$('tmp_acs_back_arrow') ioerr ignore

		if container then
			tmpWin=frames(container,1)
		else if tabcon then
			tmpWin=tabs(tabcon,1)
		else
			tmpWin=acs_win
		end if

		if tt$<>'' then
	    ! pr comkey$ : pause
			pr #tmpWin, fields lyne$&','&pos$&',P '&height$&'/'&width$&',[buttons],'&comkey$, help '4;'&tt$&';': path1$ ! ioerr ignore
		else
	    pr #tmpWin, fields lyne$&','&pos$&',P '&height$&'/'&width$&',[buttons],'&comkey$: path1$ ! ioerr ignore
		end if

		mat return_keys(udim(return_keys)+1)
		return_keys(udim(return_keys))=val(comkey$)


fnend
def fn_ace_rd_cmdkey
	dim spec$*255
	spec$=''
	txt$=control$(2)
	mat return_keys(udim(return_keys)+1)
	returnkey=return_keys(udim(return_keys))=val(control$(3))
	default=val(control$(4))
	cancel=val(control$(5))
	if udim(control$)>=6 then
		tt$=control$(6)
	end if
	if cancel then fkey_cancel=returnkey
	txt$=srep$(txt$,'&','') ! remove underlined letters...	 would be nice to use them. xxx
	width=len(txt$)
	spec$='1,'&str$(ace_cmdkey_ps)&',CC '&str$(width)&',,B'&str$(returnkey)
	if default then
		default_button_fkey=returnkey
		spec$='1,'&str$(ace_cmdkey_ps)&',CC '&str$(width)&',[buttons],B'&str$(returnkey)
		pr #0, fields "1,5,P 1/2,[buttons],"&str$(returnkey): "S:\Core\Icon\forward-icon.png" ioerr ignore
	else if cancel then
		spec$='1,'&str$(ace_cmdkey_ps)&',CC '&str$(width)&',[buttoncancel],B'&str$(returnkey)
		if env$('tmp_acs_back_arrow')<>'' then
			pr #0, fields "1,2,P 1/2,[buttons],"&str$(returnkey): env$('tmp_acs_back_arrow') ioerr ignore
		else
			pr #0, fields "1,2,P 1/2,[buttons],"&str$(returnkey): "S:\Core\Icon\back-icon.png" ioerr ignore
		end if
	else
		spec$='1,'&str$(ace_cmdkey_ps)&',CC '&str$(width)&',,B'&str$(returnkey)
	end if

	dim _help$*255
	if tt$='' then
		_help$="1;Press ["
		if default then
			_help$&="Enter"
		else if cancel then
			_help$&="Esc"
		else if returnkey >=2 and returnkey<=12 then ! because F1 will be used for Help
			_help$&="F"&str$(returnkey)
		else
			_help$=''
		end if
		if _help$<>'' then _help$&="] to "&txt$&";"
	else
		_help$='1;'&tt$&';'
	end if
	pr #button_win, fields spec$, help _help$: txt$
	ace_cmdkey_ps+=(width+2)
fnend
def fn_ace_rd_tab
	lyne=val(control$(2))
	ps=val(control$(3))
	height=val(control$(4))
	width=val(control$(5))
	path1$=control$(6) & control$(7)
	open #tab_file:=fngethandle: "Name="&path1$,internal,outIn
	tabline$=""
	for j=1 to min(lrec(tab_file),99)
		read #tab_file,using "Form Pos 1,C 80": txt$
		tabline$=tabline$&trim$(txt$)&tab$
		open #tab:=fngethandle: 'srow='&str$(lyne+1)&',scol='&str$(ps+1)&',rows='&str$(height+1)&',cols='&str$(width+1)&',tab='&trim$(txt$)&',parent='&str$(acs_win)&',border=S:[screen],N=[screen]',display,outIn
		mat tabs(udim(tabs)+1,3)
		tabs(udim(tabs),1)=tab
		tabs(udim(tabs),2)=lyne+1
		tabs(udim(tabs),3)=ps+1
	next j
	close #tab_file:
fnend
def fn_ace_rd_frame
	lyne=val(control$(2))
	ps=val(control$(3))
	height=val(control$(4))
	width=val(control$(5))
	txt$=control$(6)
	tt$=control$(7)
	container=val(control$(8))
	tabcon=val(control$(9))
	height+=1
	txt$=trim$(txt$)
	! txt$=srep$(txt$,'"','""') ! fn2quote(txt$)
	mat frames(udim(frames)+1,4)
	if tabcon then
		open #frame:=fngethandle: 'srow='&str$(lyne+1)&',scol='&str$(ps+1)&',rows='&str$(height-1)&',cols='&str$(width)&',parent='&str$(tabs(tabcon,1))&',border=S:[screen],N=[screen],caption='&srep$(txt$,',','.'),display,outIn
		frames(udim(frames),4)=tabs(tabcon,1)
	else
		open #frame:=fngethandle: 'srow='&str$(lyne+1)&',scol='&str$(ps+1)&',rows='&str$(height-1)&',cols='&str$(width)&',parent='&str$(acs_win)&',border=S:[screen],N=[screen],caption='&srep$(txt$,',','.'),display,outIn
	end if
	frames(udim(frames),1)=frame
	frames(udim(frames),2)=lyne+1
	frames(udim(frames),3)=ps+1
fnend
def fn_ace_rd_flex(;___,index_)
	lyne        = val(control$(2))
	ps          = val(control$(3))
	height     	= rows - lyne       ! val(control$(4))
	width	      = cols - ps-2       ! val(control$(5))
	seltype    	= val(control$(7))
	sorttype   	= val(control$(8))
	path1$     	= control$(9)
	hdr_count  	= val(control$(10))
	container  	= val(control$(11))
	tabcon     	= val(control$(12))
	dim _headings$(1)*1024,_line$*10000,_chunks$(1)*2100,_forms$(1)*1024
	dim filterspec$*255	 !
	dim gridspec$*255		 !
	dim loading_spec$*50 ! where to print Loading: please wait...
	! pr env$('temp')&'\acs\'&trim$(path1$)&'.hdr' : pause
	open #grid_headers:=fngethandle: 'Name='&env$('temp')&'\acs\'&trim$(path1$)&'.hdr',display,input
	linput #grid_headers: _line$
	str2mat(_line$,mat _headings$,tab$)
	linput #grid_headers: _line$
	str2mat(_line$,mat _mask$,tab$)
	mat _mask$(udim(_headings$))
	close #grid_headers:
	file_nonempty=fn_gridform(mat _widths,mat _forms$,mat _mask$,mat _headings$)
	if not file_nonempty then
		for col_index_=2 to udim(mat _headings$)
			_widths (col_index_)=len(_headings$(col_index_))
			_forms$(col_index_)="C "&str$(_widths (col_index_))
		next col_index_
	end if
	! this is done when there is a grid to the right of a frame, so they don't overlap
	if udim(frames) and not container and ps > 2 then
		ps+=1
	end if
	widthEnhanced=width+2 ! tried to increase to +28 on 1/6/2018 to make work for UB Meter Info via fnHamsterFio	but it did not fix the error 4
	filterspec$=str$(lyne)&","&str$(ps)&","&str$(widthEnhanced)&"/filter "&str$(width)&",[textboxes],"&str$(lyne+1)&","&str$(ps)&",1,word"

	if not container and not tabcon then
		gridspec$=str$(lyne+1)&","&str$(ps)&",list "&str$(height-1)&"/"&str$(widthEnhanced)
		loading_spec$=str$(lyne + height)&","&str$(ps)&",C "
	else
		gridspec$=str$(lyne+1)&","&str$(ps)&",list "&str$(height-2)&"/"&str$(widthEnhanced)
		loading_spec$=str$(lyne + height-1)&","&str$(ps)&",C "
	end if

	if container then
		window_prefix$='#'&str$(frames(container,1))&','
	else if tabcon then
		window_prefix$='#'&str$(tabs(tabcon,1))&','
	else
		window_prefix$='#'&str$(acs_win)&','
	end if
	filterspec$(0:0)=window_prefix$
	gridspec$(0:0)=window_prefix$
	loading_spec$(0:0)=window_prefix$
	! if env$('acsDeveloper')<>'' then pr 'just before grid headers' : pause !
	pr f gridspec$&",headers,[gridheaders]" : (mat _headings$,mat _widths,mat _forms$)
	open #grid_data:=fngethandle: 'Name='&env$('temp')&'\acs\'&trim$(path1$)&'[SESSION].tmp',display,input
	clearflag$="="

	dim long_row$(1)*2100		 ! dim long_row$(1)*1024
	rows=1000								 ! rows=2000
	mat long_row$(rows * udim(_headings$))
	row_count=1 : record_count=1
	printed=0

	scr_thaw

	fn_alpha_mask_indices(mat _mask$,mat alpha_mask_indices)
	do while file_nonempty
		linput #grid_data: _line$ eof ignore
		! remove this line after you figure out how to addtomask$ negative numbers to the #pic spec
		!		_line$=srep$(_line$,'-','')

		if file(grid_data)<>0 then exit do
		record_count += 1
		if record_count=30000 then
			value= msgbox( "30,000 records have been loaded. Do you wish to continue loading?", "Message", "YN", "INF")
			if value=3 then goto GRID_DATA_LOAD_COMPLETE
		end if

		str2mat(_line$,mat _chunks$,tab$)
		if udim(_chunks$)<>udim(_headings$)-1 then ! truncate extra columns, which are there by mistake
			mat _chunks$(udim(_headings$)-1 )
		end if

		mat2str(mat _chunks$,_line$," ")

		for index_=1 to udim(mat alpha_mask_indices)
			cell_value=val(_chunks$(alpha_mask_indices(index_))) conv BAD_NUMERIC_CELL
		next index_
		goto CREATE_FILTER_COLUMN
		BAD_NUMERIC_CELL: !
		_chunks$(alpha_mask_indices(index_))='0' : retry

		CREATE_FILTER_COLUMN: !
		mat _chunks$(udim(_chunks$)+1)
		mat _chunks$(2:udim(_chunks$))=_chunks$(1:udim(_chunks$)-1)
		_chunks$(1)=_line$

		! CHECK_JULIAN_DATES: ! Convert dates to julain format for BR internal date specs
		dim datemask$,masknumber
		for index_=1 to udim(mat _mask$)
			masknumber=val(_mask$(index_)) conv NOT_JULIAN_DATE
			if masknumber>=1000 then masknumber-=1000
			if masknumber>=01 and masknumber<=05 then
		!					if masknumber=01 then !		 date format : mm/dd/yy
		!						datemask$="ccyymmdd" ! "mmddyy" ! Expected programs sending dates like mddyy
		!					else if masknumber=02 then !		date format : mm/dd/ccyy
		!						datemask$="ccyymmdd" ! "mmddccyy" ! Expected programs sending dates like mmddccyy
		!					else if masknumber=03 then !		date format : ccyy/mm/dd
		!						datemask$="ccyymmdd"
		!					else if masknumber=04 then !		date format : dd/mm/ccyy
		!						datemask$="ccyymmdd" ! "ddmmccyy"
		!					else if masknumber=05 then !		date format : dd/mm/yy
				datemask$="ccyymmdd" ! "ddmmyy"
		!					end if
		!					_chunks$(index_+1)=srep$(lpad$(_chunks$(index_+1),len(datemask$))," ","0") ! pause ! lpad with zeros to the right size
				quick_len=len(trim$(srep$(_chunks$(index_+1),'/',''))) ! pr quick_len
				if quick_len=5 then quick_len=6 : _chunks$(index_+1)='0'&trim$(_chunks$(index_+1))
				if quick_len=6 then _chunks$(index_+1)=date$(days(_chunks$(index_+1),'mmddyy'),'ccyymmdd') ! datemask$='mmddyy' else datemask$="ccyymmdd"! =str$(days(_chunks$(index_+1),datemask$)) ! Convert to julain date according to mask for data in expected format
		!					pr _chunks$(index_+1),quick_len : pause ! =date$(days(_chunks$(index_+1),'mmddyy'),'ccyymmdd') ! datemask$='mmddyy' else datemask$="ccyymmdd"! =str$(days(_chunks$(index_+1),datemask$)) ! Convert to julain date according to mask for data in expected format
				_chunks$(index_+1)=str$(days(_chunks$(index_+1),datemask$)) ! Convert to julain date according to mask for data in expected format
			end if
			NOT_JULIAN_DATE: ! Not a julian date field, leave it alone
		next index_
		if row_count <= rows then
			mat long_row$(1+(row_count-1)*udim(_chunks$):row_count*udim(_chunks$))= _chunks$
			if row_count=rows then
				pr f gridspec$&","&clearflag$&"L": mat long_row$
				pr f loading_spec$ : "Loading... Please wait"
				scr_freeze
				clearflag$="+"
				mat long_row$=('')
				row_count=1
				printed=1
			else
				row_count += 1
			end if
		end if
	loop
	row_count -= 1
	if row_count<>0 then
		if not printed then
			pr f gridspec$&",=L": mat long_row$(1:(row_count)*udim(_chunks$))
		else if row_count <> rows then
			! get an error 58 below - check (mat _headings$,mat _widths,mat _forms$)
			pr f gridspec$&",+L": mat long_row$(1:(row_count)*udim(_chunks$)) ! soflow ignore
		end if
	end if
	GRID_DATA_LOAD_COMPLETE: !
	! clear the "Loading..." message
	pr f loading_spec$: rpt$(" ",30)

	close #grid_data:
	fn_ace_io_add(gridspec$&",row,selone")
	fn_ace_io_add(filterspec$)
	!		filter_index=ace_io_count

	if not container and not tabcon then
		srow$=str$(lyne+height)
	else
		srow$=str$(lyne+height-1)
	end if

	pr f window_prefix$&srow$&","&str$(ps+00)&",CC 7,,B2501": "Export"
	! pr f window_prefix$&srow$&","&str$(ps+08)&",CC 7,,B2502": "Print" ! if env$('ACSDeveloper')<>'' then
	!		pr f window_prefix$&srow$&","&str$(ps+16)&",CC 7,,B2503": "Reset"
fnend
def fn_alpha_mask_indices(mat _mask$,mat alpha_mask_indices;___,index_,mask)
	mat alpha_mask_indices(0)
	for index_=1 to udim(mat _mask$)
		mask=val(_mask$(index_)) conv ignore
		if mask >=1000 then mask-=1000
		if mask<>0 and mask<>80 and mask<>81 then ! is numeric
			mat alpha_mask_indices(udim(mat alpha_mask_indices)+1)
			alpha_mask_indices(udim(mat alpha_mask_indices))=index_
		end if
	next index_
fnend
def fn_gridform(mat _widths,mat _forms$,mat _mask$,mat _headings$;___,index_)
	data_file_nonempty=0
	mat _headings$(udim(_headings$)+1)
	mat _headings$(2:udim(_headings$))=_headings$(1:udim(_headings$)-1)
	_headings$(1)="Combined"
	mat _widths(udim(_headings$))=(0): mat _forms$(udim(_headings$))=('')

	open #grid_data:=fngethandle: 'Name='&env$('temp')&'\acs\'&trim$(path1$)&'[SESSION].tmp',display,input
	for count=1 to 1500
		linput #grid_data: _line$ eof ignore
		if file(grid_data)<>0 then goto GRIDFORM_COMPLETE
		str2mat(_line$,mat _chunks$,chr$(9))
		if udim(mat _chunks$)<>udim(mat _headings$)-1 then ! truncate extra columns, which are there by mistake or are missing
			mat _chunks$( udim(mat _headings$)-1 )
		end if
		mat2str(mat _chunks$,_line$," ")
		_widths(1)=max(_widths(1),len(_line$)+udim(_chunks$)-1)
		for _index=1 to udim(mat _chunks$)
			_widths(_index+1)=max(_widths(_index+1),len(_chunks$(_index))+1)
			_widths(_index+1)=max(_widths(_index+1),len(_headings$(_index+1))+4)
			if _widths(_index+1) then data_file_nonempty=1
			_forms$(_index+1)="C "&str$(_widths(_index+1))&',L'
		next _index
	next count
	GRIDFORM_COMPLETE: !
	for index_=2 to udim(mat _mask$)+1
		fn_column_mask(_forms$(index_),_widths(index_),_mask$(index_-1))
	next index_
	! _forms$(1)="0/C 500" works... old, small
	_forms$(1)="0/C 999"
	! _forms$(1)="0/C 800"
	! _forms$(1)="0/C 1024" fails (unable to select things from grids)
	_widths(1)=0
	close #grid_data:
	fn_gridform=data_file_nonempty
fnend
def fn_column_mask(&form$,&width,mask$;___,invisible)
	maxlen=width + 10 ! to deal with bad data
	mask=val(mask$) conv ignore
	if mask=>1000 then
		mask-=1000
		invisible=1
	end if
	if mask=1 then ! date format : mm/dd/yy
		form$=str$(width)&'/date(m/d/yy)' !
	else if mask=2 then !		 date format : mm/dd/ccyy
		form$=str$(width)&'/date(m/d/ccyy)' ! $$$$$ GSB This code used to say FMT(99/99/99) which is only 6 digits long and appears wrong
	else if mask=03 then !		date format : ccyy/mm/dd
		form$=str$(width)&'/date(ccyy/m/d)' ! '/fmt(9999/99/99)'
	else if mask=04 then !		date format : dd/mm/ccyy
		form$=str$(width)&'/date(d/m/ccyy)'
	else if mask=05 then !		date format : dd/mm/yy
		form$=str$(width)&'/date(d/m/yy)'
	else if mask=10 then ! dollars, 2 decimals, commas
		form$=''
		for tm_char_index=1 to maxlen-4
			if mod(tm_char_index,4)=0 then
				form$(0:0)=','
			else
				form$(0:0)='-'
			end if
		next tm_char_index
		if form$(1:1)=',' then form$(0:0)='-'
		form$(0:0)=str$(width)&"/#PIC("
		form$&=".--)"
	else if mask=20 then ! 0 decimals, commas
		form$=''
		for tm_char_index=1 to maxlen
			if mod(tm_char_index,4)=0 then
				form$(0:0)=','
			else
				form$(0:0)='-'
			end if
		next tm_char_index
		if form$(1:1)=',' then form$(0:0)='-'
		form$(0:0)=str$(width)&"/#PIC("
		form$&=")"
	else if mask=30 then ! defaults 1 to 1
		if maxlen>15 then
			form$=str$(width)&"/PIC("&rpt$('-',maxlen)&")"
		else
			form$=str$(width)&"/#PIC("&rpt$('-',maxlen)&")"
		end if
	else if mask=31 then ! defaults 1 to 1.0
		form$=str$(width)&"/#PIC("&rpt$('-',maxlen-2)&".-)"
	else if mask=32 then ! defaults 1 to 1.00
		form$=str$(width)&"/#PIC("&rpt$('-',maxlen-3)&".--)"
	else if mask=33 then ! defaults 1 to 1.000
		form$=str$(width)&"/#PIC("&rpt$('-',maxlen-4)&".---)"
	else if mask=34 then ! defaults 1 to 1.0000
		form$=str$(width)&"/#PIC("&rpt$('-',maxlen-5)&".----)"
	else if mask=35 then ! defaults 1 to 1.00000
		form$=str$(width)&"/#PIC("&rpt$('-',maxlen-6)&".-----)"
	else if mask=36 then ! defaults 1 to 1.000000
		form$=str$(width)&"/#PIC("&rpt$('-',maxlen-7)&".------)"
	else if mask=40 then ! defaults 1 to 0.1
		form$=str$(width)&"/#PIC("&rpt$('-',maxlen)&")"
	else if mask=41 then ! defaults 1 to 0.10
		form$=str$(width)&"/#PIC("&rpt$('-',maxlen-2)&".-)"
	else if mask=42 then ! defaults 1 to 0.100
		form$=str$(width)&"/#PIC("&rpt$('-',maxlen-3)&".--)"
	else if mask=43 then ! defaults 1 to 0.1000
		form$=str$(width)&"/#PIC("&rpt$('-',maxlen-4)&".---)"
	else if mask=44 then ! defaults 1 to 0.10000
		form$=str$(width)&"/#PIC("&rpt$('-',maxlen-5)&".----)"
	else if mask=45 then ! defaults 1 to 0.100000
		form$=str$(width)&"/#PIC("&rpt$('-',maxlen-6)&".-----)"
	else if mask=46 then ! defaults 1 to 0.1000000
		form$=str$(width)&"/#PIC("&rpt$('-',maxlen-7)&".------)"
	else if mask=50 then
		form$=str$(width)&"/#PIC(------)" ! ######
	else if mask=51 then
		form$=str$(width)&"/#PIC(---D------)" ! ###-######
	else if mask=52 then
		form$=str$(width)&"/#PIC(------D---)" ! ######-###
	else if mask=53 then
		form$=str$(width)&"/#PIC(---D------D---)" ! ###-######-###
	else if mask=51 then !				general ledger : ###-######
		form$='fmt(999-999999)'
	else if mask=52 then !				general ledger : ######-###
		form$='fmt(999999-999)'
	else if mask=53 then !				general ledger : ###-######-###
		form$='fmt(999-999999-999)'
	else if mask=61 then !				general ledger : ############
		form$='fmt(999999999999999)'
	else if mask=65 then !				12 decimals no commas
		form$='#PIC(------.------------)'
	else if mask=80 then !				force column left aligned
		form$="C "&str$(width)
	else if mask=81 then !				force column right aligned
		form$="CR "&str$(width)
	end if
	if invisible then width=0
fnend
def fn_print_grid
	! library 'S:\Core\fnsnap\rtflib_dll.br': fnlistprint
	! fnLISTPRINT(LISTWIN,LISTSPEC$,"Selected Clients","","Selected clients	 for "&CLNR$,MAT DUMMY,0,0,"11111",0) : GOTO 5290
	fnlistprint(0,gridspec$,"","","Grid Print",mat dummy,0,0,"000000",0)
	!		goto PRINT_GRID_XIT
	!		fnopenprn
	!		mat2str(mat _headings$(2:udim(_headings$)),_line$,tab$)
	!		pr #255: _line$
	!		input fields gridspec$&",rowcnt,all,nowait": grid_rows
	!		input fields gridspec$&",colcnt,all,nowait": grid_columns
	!		mat _chunks$(grid_columns)
	!		for rowindex_=1 to grid_rows
	!			input fields gridspec$&",row,range,nowait": rowindex_, rowindex_, mat _chunks$
	!			mat2str(mat _chunks$,_line$," ")
	!			pr #255: _line$
	!		next rowindex_
	!		fncloseprn
	! PRINT_GRID_XIT: !
fnend
def fn_ace_rd_pic
	lyne=val(control$(2))
	ps=val(control$(3))
	width=val(control$(4))
	height=val(control$(5))
	path1$=control$(6)
fnend
def fn_ace_rd_button
	lyne=val(control$(2))
	ps=val(control$(3))
	height=val(control$(4))
	width=val(control$(5))
	comkey=val(control$(6))

	mat return_keys(udim(return_keys)+1)
	return_keys(udim(return_keys))=comkey

	txt$=srep$(trim$(control$(7)),chr$(38),"")
	tt$=control$(8) error ignore
	default=val(control$(9)) error ignore
	cancel=val(control$(10)) error ignore
	container=val(control$(11)) error ignore
	tabcon=val(control$(12)) error ignore
	dim spec$*255
	spec$=str$(lyne)&','&str$(ps)&','&str$(width)&'/CC '&str$(len(txt$))&',,B'&str$(comkey)
	! r: new logic 10/19/2015
	!		spec$=str$(lyne)&','&str$(ps)&',CC '&str$(width)&',,B'&str$(comkey)
	if default then
		default_button_fkey=comkey
		spec$=str$(lyne)&','&str$(ps)&','&str$(width)&'/CC '&str$(len(txt$))&',[buttons],B'&str$(comkey)
		pr #0, fields "1,5,P 1/2,[buttons],"&str$(comkey): "S:\Core\Icon\forward-icon.png" ioerr ignore
	else if cancel then
		spec$=str$(lyne)&','&str$(ps)&','&str$(width)&'/CC '&str$(len(txt$))&',[buttoncancel],B'&str$(comkey)
		if env$('tmp_acs_back_arrow')<>'' then
			pr #0, fields "1,2,P 1/2,[buttons],"&str$(comkey): env$('tmp_acs_back_arrow') ioerr ignore
		else
			pr #0, fields "1,2,P 1/2,[buttons],"&str$(comkey): "S:\Core\Icon\back-icon.png" ioerr ignore
		end if
	else
		spec$=str$(lyne)&','&str$(ps)&','&str$(width)&'/CC '&str$(len(txt$))&',,B'&str$(comkey)
	end if
	! /r
	if container then
		tmp_win=frames(container,1) ! pr #frames(container,1), fields spec$: txt$
	else if tabcon then
		tmp_win=tabs(tabcon,1) ! pr #tabs(tabcon,1), fields spec$: txt$
	else
		tmp_win=acs_win ! pr #acs_win, fields spec$: txt$
	end if
	! r: new help logic 10/19/2015
	dim _help$*255
	if tt$='' then
		_help$="1;Press ["
		if default then
			_help$&="Enter"
		else if cancel then
			_help$&="Esc"
		else if returnkey >=2 and returnkey<=12 then
			_help$&="F"&str$(returnkey)
		else
			_help$=''
		end if
		if not _help$='' then _help$&="] to "&txt$&";"
	else
		_help$='1;'&tt$&';'
	end if
	! /r
	pr #tmp_win, fields spec$, help _help$: txt$
fnend
def fn_ace_rd_option
	respc+=1
	lyne=val(control$(2))
	ps=val(control$(3))
	align=val(control$(4))
	txt$=control$(5)
	container=val(control$(6))
	tabcon=val(control$(7))
	! txt$=srep$(txt$,'"','""') ! fn2quote(txt$)
	if trim$(uprc$(resp$(respc)))<>"TRUE" and trim$(uprc$(resp$(respc)))<>"FALSE" then resp$(respc)="FALSE"
	txt$=ltrm$(txt$,'^')
	if trim$(uprc$(resp$(respc)))="TRUE" then resp$(respc)='^'&trim$(txt$) else resp$(respc)=trim$(txt$)
	if container then
		fn_ace_io_add('#'&str$(frames(container,1))&','&str$(lyne)&','&str$(ps)&',radio '&str$(len(trim$(txt$))+4)&',T') ! tab order
	else if tabcon then
		fn_ace_io_add('#'&str$(tabs(tabcon,1))&','&str$(lyne)&','&str$(ps)&',radio '&str$(len(trim$(txt$))+4)&',T') ! tab order
	else
		fn_ace_io_add('#'&str$(acs_win)&','&str$(lyne)&','&str$(ps)&',radio '&str$(len(trim$(txt$))+4)&',T') ! tab order
	end if
fnend
def fn_ace_rd_check
	respc+=1
	lyne=val(control$(2))
	ps=val(control$(3))
	if ps<=0 then ps=1
	align=val(control$(4))
	txt$=control$(5)
	container=val(control$(6))
	tabcon=val(control$(7))
	chk_disable=val(control$(8))
	! txt$=srep$(txt$,'"','""') ! fn2quote(txt$)
	txt$=ltrm$(txt$,'^')
	if trim$(uprc$(resp$(respc)))="TRUE" then resp$(respc)='^' else resp$(respc)=''
	spec$=','&str$(lyne)&','&str$(ps+len(txt$)+1)&',check 2'
	if align then
		align$='CR '
	else
		align$='C '
	end if
	if chk_disable then chk_protected$=',P' else chk_protected$=',T' ! either Protect the field or force it to be in the tab order
	if container then
		fn_ace_io_add('#'&str$(frames(container,1))&spec$&chk_protected$) ! Tab Order
		pr #frames(container,1), fields str$(lyne)&','&str$(ps)&','&align$&str$(len(txt$)): trim$(txt$)
	else if tabcon then
		fn_ace_io_add('#'&str$(tabs(tabcon,1))&spec$&chk_protected$) ! Tab Order
		pr #tabs(tabcon,1), fields str$(lyne)&','&str$(ps)&','&align$&str$(len(txt$)): trim$(txt$)
	else
		fn_ace_io_add('#'&str$(acs_win)&spec$&chk_protected$) ! Tab Order
		pr #acs_win, fields str$(lyne)&','&str$(ps)&','&align$&str$(len(txt$)): trim$(txt$)
	end if
fnend
def fn_ace_rd_label(; ___,lbl_win)
	lyne=val(control$(2))
	ps=val(control$(3))
	mylen=val(control$(4))
	align=val(control$(5))
	txt$=control$(6)
	container=val(control$(7))
	tabcon=val(control$(8))
	font_mod=val(control$(9))
	txt$=srep$(trim$(txt$),'&','')
	dim lbl_tooltip$*256
	if udim(control$)>=10 then lbl_tooltip$=control$(10) else lbl_tooltip$=''
	if txt$<>'' then
	! txt$=srep$(txt$,'"','""') ! fn2quote(txt$)
		mylen=max(mylen,len(txt$))
		if align=1 then ace_rd_label_align$='r' else if align=2 then ace_rd_label_align$='c' else ace_rd_label_align$='l'
		if container then
			lbl_win=frames(container,1)
		! pr #frames(container,1), fields str$(lyne)&','&str$(ps)&',C'&ace_rd_label_align$&' '&str$(mylen): trim$(txt$)
		else if tabcon then
			lbl_win=tabs(tabcon,1)
		! pr #tabs(tabcon,1), fields str$(lyne)&','&str$(ps)&',C'&ace_rd_label_align$&' '&str$(mylen): trim$(txt$)
		else
			lbl_win=acs_win
		! pr #acs_win, fields str$(lyne)&','&str$(ps)&',C'&ace_rd_label_align$&' '&str$(mylen): trim$(txt$)
		end if
		if lbl_tooltip$<>'' then
			pr #lbl_win, fields str$(lyne)&','&str$(ps)&',C'&ace_rd_label_align$&' '&str$(mylen), help '4;'&lbl_tooltip$&';': trim$(txt$)
		else
			pr #lbl_win, fields str$(lyne)&','&str$(ps)&',C'&ace_rd_label_align$&' '&str$(mylen): trim$(txt$)
		end if
	end if
fnend
def fn_ace_rd_text
	respc+=1
	lyne=val(control$(2))
	ps=val(control$(3)) ! -.6
	width=val(control$(4))
	maxlen=max(val(control$(5)),width)
	if maxlen=1 then maxlen=2
	align=val(control$(6))
	disable=val(control$(7))
	mask$=control$(8)
	tt$=control$(9)
	container=val(control$(10))
	tabcon=val(control$(11))
	addtomask$=control$(12)
! resp$(respc)=srep$(resp$(respc),'"','""') soflow ignore ! fn2quote(resp$(respc))
! tt$=srep$(tt$,'"','""') soflow ignore ! fn2quote(tt$)
	dim spec$*255
	spec$=fn_textMask$(mask$,lyne,ps,width,container,maxlen)
	mat text_masks(respc)
	text_masks(respc)=val(mask$)
	if container then
		fn_ace_io_add('#'&str$(frames(container,1))&','&str$(lyne)&','&str$(ps)&','&spec$)
	else if tabcon then
		fn_ace_io_add('#'&str$(tabs(tabcon,1))&','&str$(lyne)&','&str$(ps)&','&spec$)
	else
		fn_ace_io_add('#'&str$(acs_win)&','&str$(lyne)&','&str$(ps)&','&spec$)
	end if
fnend
def fn_textMask$*255(mask$*255,lyne,ps,width,container,maxlen; ___,return$*255)
	mask=0
	mask=val(mask$) conv ignore

	if mask>1000 then mask-=1000
	if mask>=1 and mask<=5 then
		fn_dateTextBox(mask,lyne,ps,width,container,disable)

		return$="9/#PIC(--/--/--)" ! return$="9/DATE(m/d/y)"
		if mask=1 then
			resp$(respc)=lpad$(trim$(resp$(respc)),6,'0')
			date_format$='mdy'
		else if mask=2 then
			date_format$='mdcy'
			resp$(respc)=lpad$(trim$(resp$(respc)),8,'0')
		else if mask=3 then
			date_format$='cymd'
			if len(trim$(resp$(respc)))<=6 then
				resp$(respc)=date$(days(val(lpad$(trim$(resp$(respc)),6,'0')),'mdy'),'cymd')
				if val(resp$(respc)(1:2))=19 and val(resp$(respc)(3:4)) < 20 then resp$(respc)(1:2)="20"
			end if
		else if mask=4 then
			date_format$='dmcy'
			resp$(respc)=lpad$(trim$(resp$(respc)),8,'0')
		else if mask=5 then
			date_format$='dmy'
			resp$(respc)=lpad$(trim$(resp$(respc)),6,'0')
		end if

		resp$(respc)=date$(days(trim$(resp$(respc)),date_format$),'mdy')

	else if mask=9 then ! defaults 100 to 1.00
		return$=str$(width)&"/#PIC("&rpt$('-',maxlen-3)&".--)"
		resp$(respc)=str$(val(resp$(respc))/100)
	else if mask=10 then ! dollars, 2 decimals, commas
		form$=''
		for tm_char_index=1 to maxlen-4
!			if mod(tm_char_index,4)=0 then
!				return$(0:0)=','
!			else
			return$(0:0)='-'
!			end if
		next tm_char_index
		if maxlen < 4 then return$(0:0)='-'
		return$(0:0)=str$(width)&"/#PIC("
		return$&=".--)"
		return$=srep$(return$,'/#PIC(,---.--','/#PIC(----.--')
	else if mask=12 then ! defaults 100 to 100.00, currency : American (2 decimals, commas)
		form$=''
		for tm_char_index=1 to maxlen-4
			if mod(tm_char_index,4)=0 then
				return$(0:0)=','
			else
				return$(0:0)='-'
			end if
		next tm_char_index
! pr return$ : pause
		return$(0:0)=str$(width)&"/#PIC("
		return$&=".--)"
	else if mask=20 then ! 0 decimals, commas
		form$=''
		for tm_char_index=1 to maxlen
			if mod(tm_char_index,4)=0 then
				return$(0:0)=','
			else
				return$(0:0)='-'
			end if
		next tm_char_index
		return$(0:0)=str$(width)&"/#PIC("
		return$&=")"
	else if mask=30 then ! defaults 1 to 1
		if maxlen>15 then
			return$=str$(width)&"/PIC("&rpt$('-',maxlen)&")"
		else
			return$=str$(width)&"/#PIC("&rpt$('-',maxlen)&")"
		end if
	else if mask=31 then ! defaults 1 to 1.0
		return$=str$(width)&"/#PIC("&rpt$('-',maxlen-2)&".-)"
	else if mask=32 then ! defaults 1 to 1.00
		return$=str$(width)&"/#PIC("&rpt$('-',maxlen-3)&".--)"
	else if mask=33 then ! defaults 1 to 1.000
		return$=str$(width)&"/#PIC("&rpt$('-',maxlen-4)&".---)"
	else if mask=34 then ! defaults 1 to 1.0000
		return$=str$(width)&"/#PIC("&rpt$('-',maxlen-5)&".----)"
	else if mask=35 then ! defaults 1 to 1.00000
		return$=str$(width)&"/#PIC("&rpt$('-',maxlen-6)&".-----)"
	else if mask=36 then ! defaults 1 to 1.000000
		return$=str$(width)&"/#PIC("&rpt$('-',maxlen-7)&".------)"
	else if mask=40 then ! defaults 1 to 0.1
		return$=str$(width)&"/#PIC("&rpt$('-',maxlen)&")"
!		resp$(respc)=str$(val(resp$(respc))/10)
	else if mask=41 then ! defaults 1 to 0.10
		return$=str$(width)&"/#PIC("&rpt$('-',maxlen-2)&".-)"
!		resp$(respc)=str$(val(resp$(respc))/10)
	else if mask=42 then ! defaults 1 to 0.100
		return$=str$(width)&"/#PIC("&rpt$('-',maxlen-3)&".--)"
!		resp$(respc)=str$(val(resp$(respc))/10)
	else if mask=43 then ! defaults 1 to 0.1000
		return$=str$(width)&"/#PIC("&rpt$('-',maxlen-4)&".---)"
!		resp$(respc)=str$(val(resp$(respc))/10)
	else if mask=44 then ! defaults 1 to 0.10000
		return$=str$(width)&"/#PIC("&rpt$('-',maxlen-5)&".----)"
!		resp$(respc)=str$(val(resp$(respc))/10)
	else if mask=45 then ! defaults 1 to 0.100000
		return$=str$(width)&"/#PIC("&rpt$('-',maxlen-6)&".-----)"
!		resp$(respc)=str$(val(resp$(respc))/10)
	else if mask=46 then ! defaults 1 to 0.1000000
		return$=str$(width)&"/#PIC("&rpt$('-',maxlen-7)&".------)"
!		resp$(respc)=str$(val(resp$(respc))/10)
	else if mask=50 then
		return$=str$(width)&"/#PIC(------)" ! ######
	else if mask=51 then
		return$=str$(width)&"/#PIC(---D------)" ! ###-######
	else if mask=52 then
		return$=str$(width)&"/#PIC(------D---)" ! ######-###
	else if mask=53 then
		return$=str$(width)&"/#PIC(---D------D---)" ! ###-######-###
	else if mask=60 then
		return$=str$(width)&"/#PIC(---D---D----)" ! ###-###-#### or ####-###-####
	else if mask=70 or mask=71 or mask=72 then
		fn_drawFileSelection(mask,lyne,ps,width,container)
	end if
	if return$='' then
		if align=1 then
			return$=str$(width)&'/CR '&str$(maxlen) ! right
		else if align=2 then
			return$=str$(width)&'/CC '&str$(maxlen) ! centered
		else
			return$=str$(width)&'/C '&str$(maxlen) ! left=default
		end if
	end if

	if disable then protected$='P' else protected$='T' ! either Protect the field or force it to be in the tab order
	return$&= ','&protected$&'[textboxes]' &','&str$(txtbox_fkey)
	return$=srep$(return$,'PIC(,','PIC(')
	fn_textMask$=return$
fnend
def fn_drawFileSelection(mask,lyne,ps,width,container; ___,fs_buttonIo$*255) ! requires local file_select_fkey_base,file_select_boxes (this may need to be reset somewhere... like fnTos)
	file_select_boxes+=1
	fs_buttonIo$=str$(lyne)&','&str$(ps+width+1)&',C 1,,B'&str$(file_select_fkey_base+file_select_boxes)
	if container then
		fs_buttonIo$(0:0)='#'&str$(frames(container,1))&','
	else if tabcon then
		fs_buttonIo$(0:0)='#'&str$(tabs(tabcon,1))&','
	else
		fs_buttonIo$(0:0)='#'&str$(acs_win)&','
	end if
	mat file_select_data(file_select_boxes,2)
	file_select_data(file_select_boxes,1)=respc
	file_select_data(file_select_boxes,2)=mask
	pr f fs_buttonIo$: "."
	! pr 'drawing the . with io of '&fs_buttonIo$
fnend
def fn_selectFile(&filename$,mask; ___,openOrSave$,newOrShare$,wasFilenamesUpperCase)
	if mask=70 or mask=71 or mask=72 then
		if mask=70 or mask=71 then
			openOrSave$='OPEN'
			newOrShare$='Shr'
		else if mask=72 then
			openOrSave$='SAVE'
			newOrShare$='New'
		end if
		filename$=rtrm$(filename$)
		h_selectfile=fngethandle
include: filenamesPushMixedCase
		if openOrSave$='SAVE' then
			dim tmpFilename$*1048
			dim tmpPath$*1048
			dim tmpExt$*128
			fnGetPp(filename$,tmpPath$,tmpFilename$,tmpExt$)
			! open #h_selectfile: 'Name='&openOrSave$&':'&env$('at')(1:2)&filename$&' All documents (*.*),RecL=1,'&newOrShare$,external,input ioerr ignore
			open #h_selectfile: 'Name='&openOrSave$&':'&env$('at')(1:2)&tmpPath$&'*.*,RecL=1,'&newOrShare$,external,output ioerr ignore
		else
			open #h_selectfile: 'Name='&openOrSave$&':'&env$('at')(1:2)&filename$&'All documents (*.*) |*.*,RecL=1,'&newOrShare$,external,input ioerr ignore
		end if
		if file(h_selectfile)=0 then
			filename$=os_filename$(file$(h_selectfile))
			! filename$=trim$(file$(h_selectfile)) (2:inf)
			if filename$(1:2)='@:' then filename$(1:2)=''
			if filename$(1:1)=':' then filename$(1:1)=''
			close #h_selectfile:
		end if
include: filenamesPopUpperCase

	end if
fnend
def fn_dateTextBox(mask,lyne,ps,&width,container; disable,___,disable$*1,date_button_spec$*255)
	date_boxes+=1
	if disable then disable$='P'
	date_button_spec$=str$(lyne)&','&str$(ps+width+1)&',P 1/2,'&disable$&','&str$(date_fkey_base+date_boxes)
	mat date_fielddata(date_boxes,6)
	if container then
		date_fielddata(date_boxes,1)=container
		date_fielddata(date_boxes,2)=lyne+frames(container,2)
		date_fielddata(date_boxes,3)=ps+frames(container,3)
		date_button_spec$(0:0)='#'&str$(frames(container,1))&','
	else if tabcon then
		date_fielddata(date_boxes,1)=tabcon
		date_fielddata(date_boxes,2)=lyne+tabs(tabcon,2)
		date_fielddata(date_boxes,3)=ps+tabs(tabcon,3)
		date_button_spec$(0:0)='#'&str$(tabs(tabcon,1))&','
	else
		date_fielddata(date_boxes,2)=lyne
		date_fielddata(date_boxes,3)=ps
		date_button_spec$(0:0)='#'&str$(acs_win)&','
	end if
	pr f date_button_spec$: "S:\Core\Icon\calendar_icon.png" ioerr ignore
	date_fielddata(date_boxes,4)=width
	date_fielddata(date_boxes,5)=respc ! parent textbox number
	date_fielddata(date_boxes,6)=mask
fnend
def fn_ace_rd_combo(combo$*1)
	dim ace_combo_io$*255
	ace_combo_io$=''
	! combo_count=0
	respc+=1
	lyne=val(control$(2))
	ps=val(control$(3))
	width=val(control$(4))
	! keylen=val(control$(5))
	path1$=control$(6)
	limittolist=val(control$(7))
	tt$=control$(8)
	container=val(control$(9))
	tabcon=val(control$(10))
	dim combooptionset$*256
	combooptionset$=control$(11)

	tmp_combo_count_for_read+=1

	if width<=2 then
		width=4
	end if
	if combo$='F' then
		poskeylen=pos(combooptionset$,'lnk=')
		keylen=val(combooptionset$(poskeylen+4:pos(combooptionset$,',',poskeylen)-1))
		posdesclen=pos(combooptionset$,'lnd=')
		desclen=val(combooptionset$(posdesclen+4:pos(combooptionset$,',',posdesclen)-1))
		if desclen>0 then spacerlen=1 else spacerlen=0
		if (keylen+desclen+spacerlen)>width then
			width=keylen+desclen+spacerlen
		end if
	end if
	if limittolist=1 or limittolist=2 then
		ace_combo_io$=str$(lyne)&','&str$(ps)&','&str$(width)&'/combo 128,+,Select'
	else
!		ace_combo_io$=str$(lyne)&','&str$(ps)&','&str$(width)&'/combo 128,+'
		ace_combo_io$=str$(lyne)&','&str$(ps)&','&str$(width)&'/combo '&str$(width)&',+'
	end if
	! tt$=srep$(tt$,'"','""') ! fn2quote(tt$)
	! r: test for invalid containter and/or tabcon
	if container>udim(mat frames,1) then
		pr 'invalid container ('&str$(container)&') specified for combobox (type '&combo$&')'
		fnpause
		container=0
	end if
	if tabcon>udim(mat tabs,1) then
		pr 'invalid tab container (tabcon='&str$(container)&') specified for combobox (type '&combo$&')'
		fnpause
		tabcon=0
	end if
	! /r
	if container then
		spec$='#'&str$(frames(container,1))&','&ace_combo_io$
	else if tabcon then
		spec$='#'&str$(tabs(tabcon,1))&','&ace_combo_io$
	else
		spec$='#'&str$(acs_win)&','&ace_combo_io$
	end if
	!
	! setenv('combo'&str$(respc),env$('tmp_combo'&str$(tmp_combo_count_for_read)))
	dim tmp_combo_option$(1)*81
	combooption_which=srch(mat combooptionsetlist$,combooptionset$)
	! if comboOption_which=4 then pause
	! if comboOption_which>comboOptionItemList(comboOption_which) then
	!		pr 'comboOptionSetList$(comboOption_which)=';comboOptionSetList$(comboOption_which)
	!		pr 'comboOptionItemList(comboOption_which)=';comboOptionItemList(comboOption_which)
	!		pr 'comboOption_which=';comboOption_which
	!		pr 'it is a repeat.'
	!		fnpause
	! else
	!		pr 'comboOption_which=';comboOption_which
	! end if
	! if comboOption_which>0 and comboOption_which<>comboOption_which_prior then
	str2mat(env$('tmp_combo'&str$(tmp_combo_count_for_read)),mat tmp_combo_option$,'|')
	! else
	!		pr 'saved time on tmp_combo'&str$(tmp_combo_count_for_read)
	! end if
	combooption_which_prior=combooption_which
	pr f spec$: mat tmp_combo_option$

	setenv('tmp_combo'&str$(tmp_combo_count_for_read)&'_response_item',str$(respc))
	! COMBO_COMPLETE: !
	!
	if pos(spec$,"+,Select")>0 then
		spec$=srep$(spec$,"+,Select","Select")
	else
		spec$=srep$(spec$,",+",",")
	end if
	spec$=spec$&'T[textboxes]'
	if pos (spec$,',Select')>0 then ! move the ,select to the end
		spec$=srep$(spec$,',Select',',')
		spec$=spec$&',Select'
	end if
	fn_ace_io_add(spec$)
fnend
def fn_ace_io_add(aia_in$*255)
	ace_io_count=udim(mat ace_io$)+1
	mat ace_io$(ace_io_count)
	ace_io$(ace_io_count)=aia_in$
fnend
def fn_control_count
	control_count=val(env$('control_count'))+1
	setenv('control_count',str$(control_count))
	fn_control_count=control_count
fnend
def library fnqgl(myline,mypos; qglcontainer,add_all_or_blank,use_or_replace,qgllength,qgltabcon)
	if ~setup then fn_setup
	if qgllength=0 then qgllength=35
!_
! the response$ for this - should be gotten with fnAGL
! fnComboA("XXX",MYLINE,MYPOS,MAT OPT$,"Select from your Chart of Accounts ("&qgl_cursys$&").",WIDTH=35)
! this function has an integrated fnComboA - similar to the one above
!_
	dim qglopt$*60
	dim glmstr_form$*80
	dim qgloption$(1)*255
	dim qgloptfile$*199
	dim qglsetupkeycurrent$*128
	dim qglsetupkeyprior$*128
! r: set qgl_cursys$ (for fnqgl purposes only)
	if env$('CurSys')="UB" and exists("[Q]\GLmstr\Company.h[cno]") then
		qgl_cursys$="GL"
	else if env$('CurSys')="PR" then
		if exists("[Q]\GLmstr\Company.h[cno]") then
			qgl_cursys$="GL"
		else if exists("[Q]\CLmstr\Company.h[cno]") then
			qgl_cursys$="CL"
		else
			qgl_cursys$="PR"
		end if
	else if env$('CurSys')='CR' and exists("[Q]\GLmstr\Company.h[cno]") then
		qgl_cursys$='GL'
	else if env$('CurSys')='CR' and exists("[Q]\GLmstr\Company.h[cno]")=0 then
		qgl_cursys$='CR'
	else if env$('CurSys')='CL' then
		qgl_cursys$='CL'
	else
		qgl_cursys$='GL'
	end if
! /r
	if setupqgl$<>qgl_cursys$ then ! r:
		setupqgl$=qgl_cursys$
		open #company:=fngethandle: "Name=[Q]\"&qgl_cursys$&"mstr\Company.h[cno],Shr",internal,input ioerr CLOSECOMPANY
		if qgl_cursys$='CL' then
			read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub ! read it from checkbook
		else if qgl_cursys$="GL" then
			read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub ! read it from General Ledger
		else if qgl_cursys$="PR" or qgl_cursys$="UB" or qgl_cursys$="CR" then
			use_dept=1 : use_sub=1 ! default both to yes if from pr chart of accounts
		end if
CLOSECOMPANY: !
		close #company: ioerr ignore
		if use_dept<>0 and use_sub<>0 then glmstr_form$="Form Pos 1,C 12"
		if use_dept =0 and use_sub<>0 then glmstr_form$="Form Pos 4,C 09"
		if use_dept =0 and use_sub =0 then glmstr_form$="Form Pos 4,C 06"
		if use_dept<>0 and use_sub =0 then glmstr_form$="Form Pos 1,C 09"
! add description to the form
		glmstr_form$=glmstr_form$&",Pos 13,C 50"
	end if	! /r
	qglsetupkeycurrent$='qglCursys='&qgl_cursys$&',add_all_or_blank='&str$(add_all_or_blank)
	if qglsetupkeycurrent$=qglsetupkeyprior$ then
!		pr 'saving time ';timesavecount+=1 : pause
		goto QGLFINIS
	else
		qglsetupkeyprior$=qglsetupkeycurrent$
		mat qgloption$(0) : option_count=0
		if add_all_or_blank=1 then
			mat qgloption$(option_count+=1) : qgloption$(option_count)='[All]'
		else if add_all_or_blank=2 then
			mat qgloption$(option_count+=1) : qgloption$(option_count)=''
		end if
! read the chart of accounts from the appropriate system into an array
		if qgl_cursys$='GL' or qgl_cursys$='CL' or qgl_cursys$='PR' or qgl_cursys$='UB' or qgl_cursys$='CR' then
			open #glmstr:=fngethandle: "Name=[Q]\"&qgl_cursys$&"mstr\GLmstr.h[cno],KFName=[Q]\"&qgl_cursys$&"mstr\glIndex.h[cno],Shr",internal,input,keyed ioerr QGL_ERROR
		end if
		do
			read #glmstr,using glmstr_form$: qglopt$,desc$ noRec QGL_LOOP_COMPLETE eof EO_QGL_GLMSTR ioerr QGL_ERROR
! reformat the options for typing
			if use_dept<>0 and use_sub<>0 then
				qglopt$=trim$(qglopt$(1:3))&"-"&trim$(qglopt$(4:9))&"-"&trim$(qglopt$(10:12))
			else if use_dept=0 and use_sub<>0 then
				qglopt$=trim$(qglopt$(1:6))&"-"&trim$(qglopt$(7:9))
			else if use_dept=0 and use_sub=0 then
				qglopt$=trim$(qglopt$(1:6))
			else if use_dept<>0 and use_sub=0 then
				qglopt$=trim$(qglopt$(1:3))&"-"&trim$(qglopt$(4:9))
			end if
!	 add spaces to the end of it
!	 - for spacing of the description,
!	 and the description
			qglopt$=(rpad$(qglopt$,14)&desc$)(1:qgllength)
!		write it into the comobobox option file
!		pr #whr,using "Form Pos 1,C 81": qglOpt$
			mat qgloption$(option_count+=1) : qgloption$(option_count)=qglopt$
QGL_LOOP_COMPLETE: !
		loop
EO_QGL_GLMSTR: !
		close #glmstr: ioerr ignore
	end if
	goto QGLFINIS
QGL_ERROR: !
	pr 'err ';err;' on line ';line
	pause
	goto QGLFINIS
QGLFINIS: ! WRITE_QGL_ACE: ! add it to the screen ace script file
	qgloptfile$=qgl_cursys$&"GLNumber"
	fn_comboA(qgloptfile$,myline,mypos,mat qgloption$, "Select from the Chart of Accounts ("&qgl_cursys$&").",qgllength,qglcontainer,qgltabcon,qglsetupkeycurrent$)
	myline=mypos=con=0
fnend
def fn_remove_crlf(&txt$)
	lastx=x=0
	do
		x=pos(txt$,hex$("0D0A"),lastx)
		if x>0 then
			txt$=txt$(1:x-1)&"HEX$(0D0A)"&txt$(x+2:len(trim$(txt$)))
			lastx=x
		end if
	loop while x>0
fnend
def fn_export_grid(;___,index_)
	dim filename$*1024
	filename$=''
	grid_rows=grid_columns=index_=0
	open #export_file:=fngethandle: "Name=save:"&env$('at')&"Text documents (*.txt) |*.txt,RecL=1,Replace",external,output error GRID_EXPORT_XIT
	filename$=file$(export_file)
	close #export_file:
	open #export_file: 'Name='&filename$&',RecL=2048,Replace',display,output
	input fields gridspec$&",RowCnt,all,nowait": grid_rows
	input fields gridspec$&",ColCnt,all,nowait": grid_columns
	mat _chunks$(grid_columns)
	mat2str(mat _headings$(2:udim(_headings$)),_line$,tab$)
	pr #export_file: _line$
	for index_=1 to grid_rows
		input fields gridspec$&",row,range,nowait": index_, index_, mat _chunks$
		for eg_grid_line_item=2 to udim(mat _chunks$)
			eg_tmp_mask=val(_mask$(eg_grid_line_item-1))
			if eg_tmp_mask=>1 and eg_tmp_mask<=5 then
! r: get date_format$
				if eg_tmp_mask=1 then
					date_format$='mdy'
				else if eg_tmp_mask=2 then
					date_format$='mdcy'
				else if eg_tmp_mask=3 then
					date_format$='cymd'
				else if eg_tmp_mask=4 then
					date_format$='dmcy'
				else if eg_tmp_mask=5 then
					date_format$='dmy'
				end if
! /r
!					pr _chunks$(eg_grid_line_item);'	date format is '&date_format$ : pause
				_chunks$(eg_grid_line_item)=date$(val(_chunks$(eg_grid_line_item)),date_format$)
			else if eg_tmp_mask<2 or eg_tmp_mask>65 then
				_chunks$(eg_grid_line_item)='"'&rtrm$(_chunks$(eg_grid_line_item))&'"'
			end if
		next eg_grid_line_item

		mat2str(mat _chunks$(2:udim(_chunks$)),_line$,tab$)
		pr #export_file: _line$ ! pr _line$ : pause
	next index_
	close #export_file:
! GRID_EXPORT_COMPLETE: !
	mat ml$(2)
	ml$(1)='Grid successfully exported to:'
	ml$(2)=os_filename$(filename$)
	fnmsgbox(mat ml$,resp$,"ACS",0)
GRID_EXPORT_XIT: !
fnend
def fn_get_flexhandle(;forceclose)
	if forceclose then
		forceclose=0
		close #118: ioerr ignore
	end if
	fn_get_flexhandle=118
fnend
def library fnbackgrounddisable(; activate)
	if ~setup_library then let fn_setup
	fnbackgrounddisable=fn_backgrounddisable( activate)
fnend
def fn_backgrounddisable(; activate)
	if activate then
		Session_Rows=val(env$('Session_Rows')) : if Session_Rows<=0 then Session_Rows=35
		Session_Cols=val(env$('Session_Cols')) : if Session_Cols<=0 then Session_Cols=115
		open #disable_win=fngethandle: "srow=1,scol=1,rows="&str$(Session_Rows)&",cols="&str$(Session_Cols)&",border=none,picture=S:\Core\disable.png:TILE,parent=0",display,output
	else
		close #disable_win: ioerr ignore
	end if
fnend
def library fncompany_name(window,win_cols)
	if ~setup_library then let fn_setup
	fncompany_name=fn_company_name(window,win_cols)
fnend
def fn_company_name(window,win_cols)
	pr #window, fields "1,08,CC 18,[screenheader]": date$("Month dd, ccyy")(1:18)
	pr #window, fields "1,27,CC 05,[screenheader]": env$("cno")
	pr #window, fields "1,33,CC 51,[screenheader]": env$('Program_Caption')(1:51)
	pr #window, fields "1,86,22/CC 24,[screenheader]": env$('cnam')(1:24)
	if env$('tmp_acs_back_arrow')='' then ! it is not the main menu.
		! pr #window, fields "1,"&str$(win_cols-05)&",P 1/2,[buttons],1505": "S:\Core\Icon\Properties.png" ioerr ignore
	end if
	pr #window, fields "1,"&str$(win_cols-02)&",P 1/2,[buttons],1504": "S:\Core\Icon\help_icon.png" ioerr ignore
fnend

