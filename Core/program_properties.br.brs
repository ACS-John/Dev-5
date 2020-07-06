def library fnprogram_properties(; forceProgramCaption$*256)
	autoLibrary
	on error goto Ertn
	if forceProgramCaption$<>'' then
		dim oldCap$*256
		oldCap$=env$('Program_Caption')
		setenv('Program_Caption',forceProgramCaption$)
	end if
	fnBackgroundDisable(1)
	fn_localPropertiesEdit
	if forceProgramCaption$<>'' then
		setenv('Program_Caption',oldCap$)
	end if
	fnBackgroundDisable(0)
fnend
def fn_localPropertiesEdit
	dim orient$(0)*9
	mat orient$(2)
	orient$(1)='Portrait'
	orient$(2)='Landscape'
	respc=0
	fn_readProgramPrintProperty('Orientation' ,resp$(sio_cmbOrientation :=respc+=1))
	fn_readProgramPrintProperty('Height'      ,resp$(sio_txtHeight      :=respc+=1))
	fn_readProgramPrintProperty('Width'       ,resp$(sio_txtWidth       :=respc+=1))
	fn_readProgramPrintProperty('Lines'       ,resp$(sio_txtLpp         :=respc+=1))
	fn_readProgramPrintProperty('FontSize'    ,resp$(sio_txtFontSize    :=respc+=1))
	fn_readProgramPrintProperty('TopMargin'   ,resp$(sio_txtMarginTop   :=respc+=1))
	fn_readProgramPrintProperty('BottomMargin',resp$(sio_txtMarginBottom:=respc+=1))
	fn_readProgramPrintProperty('LeftMargin'  ,resp$(sio_txtMarginLeft  :=respc+=1))
	fn_readProgramPrintProperty('RightMargin' ,resp$(sio_txtMarginRight :=respc+=1))
	fntos(sn$:='properties')
	lc=0
	fnlbl(lc+=1,1,'Orientation:'      ,18,1) : fncomboa('orientation',lc,33,mat orient$)
	lc+=1
	fnlbl(lc+=1,1,'Height:'           ,18,1) : fntxt(lc,33,10,0,0,'')
	fnlbl(lc+=1,1,'Width:'            ,18,1) : fntxt(lc,33,10,0,0,'')
	lc+=1
	fnlbl(lc+=1,1,'Lines Per Page:'   ,18,1) : fntxt(lc,33,10,0,0,'',0,'How many lines print before a page break is sent.')
	fnlbl(lc+=1,1,'Font Size:'        ,18,1) : fntxt(lc,33,10,0,0,'',0,'Base font size for printed report')
	lc+=1
	fnlbl(lc+=1,1,'Top Margin:'       ,18,1) : fntxt(lc,33,10,0,0,'')
	fnlbl(lc+=1,1,'Bottom Margin:'    ,18,1) : fntxt(lc,33,10,0,0,'')
	fnlbl(lc+=1,1,'Left Margin:'      ,18,1) : fntxt(lc,33,10,0,0,'')
	fnlbl(lc+=1,1,'Right Margin:'     ,18,1) : fntxt(lc,33,10,0,0,'')
	fncmdset(2)
	fnAcs(mat resp$,ckey, 0,0,0,1)
	if ckey<>5 then
		! r: if Landscape/Portrait, than switch height and width if necessary
		tmp_height=val(resp$(sio_txtHeight))
		tmp_width=val(resp$(sio_txtWidth))
		if resp$(sio_cmbOrientation)='Landscape' then
			if tmp_height>tmp_width then ! and it's taller than it is wide
				tmp_hold=tmp_height
				tmp_height=tmp_width : resp$(sio_txtHeight)=str$(tmp_height)
				tmp_width=tmp_hold : resp$(sio_txtWidth)=str$(tmp_width)
			end if
		else if resp$(sio_cmbOrientation)='Portrait' then
			if tmp_width>tmp_height then ! and it's wider than it is tall
				tmp_hold=tmp_height
				tmp_height=tmp_width : resp$(sio_txtHeight)=str$(tmp_height)
				tmp_width=tmp_hold : resp$(sio_txtWidth)=str$(tmp_width)
			end if
		end if
		! /r
		fn_writeProgramPrintProperty('Orientation',     resp$(sio_cmbOrientation)    )
		fn_writeProgramPrintProperty('Height',          resp$(sio_txtHeight)         )
		fn_writeProgramPrintProperty('Width',           resp$(sio_txtWidth)          )
		fn_writeProgramPrintProperty('Lines',           resp$(sio_txtLpp)            )
		fn_writeProgramPrintProperty('FontSize',        resp$(sio_txtFontSize)       )
		fn_writeProgramPrintProperty('TopMargin',       resp$(sio_txtMarginTop)      )
		fn_writeProgramPrintProperty('BottomMargin',    resp$(sio_txtMarginBottom)   )
		fn_writeProgramPrintProperty('LeftMargin',      resp$(sio_txtMarginLeft)     )
		fn_writeProgramPrintProperty('RightMargin',     resp$(sio_txtMarginRight)    )
	end if

fnend
def library fnpglen(&pglen)
	autoLibrary
	on error goto Ertn
	fn_readProgramPrintProperty('Lines',lpp$) : pglen=val(lpp$)
	fnpglen=pglen
fnend
def library fnread_program_print_property(key$*80,&value$; prgCapForSettingsOverride$*256)
	autoLibrary
	on error goto Ertn
	fnread_program_print_property=fn_readProgramPrintProperty(key$,value$, prgCapForSettingsOverride$)
fnend
def fn_readProgramPrintProperty(key$*80,&value$; prgCapForSettingsOverride$*256)
	dim prg$*256
	if prgCapForSettingsOverride$='' then
		prg$=env$('Program_Caption')
	else
		prg$=prgCapForSettingsOverride$
	end if
	len_prg=len(prg$)
	! if lwrc$(prg$(len_prg-2:len_prg))='.br' then prg$(len_prg-2:len_prg)='' ! remove the .br ending, if it is there
	! if lwrc$(trim$(prg$))=lwrc$("acspr\checkfile") then prg$="Payroll\Payroll Check History"
	if env$('ACSDeveloper')<>'' then !  if it is a developer
		fnsreg_read(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
	else
		fnreg_read(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
		if value$='' then
			fnsreg_read(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
		end if
	end if
	if value$='' then
		if lwrc$(key$)=lwrc$('Orientation' ) then value$= 'Portrait'
		if lwrc$(key$)=lwrc$('Height'      ) then value$= '11.000'
		if lwrc$(key$)=lwrc$('Width'       ) then value$= '8.500'
		if lwrc$(key$)=lwrc$('Lines'       ) then value$= '54'
		if lwrc$(key$)=lwrc$('FontSize'    ) then value$= '10'
		if lwrc$(key$)=lwrc$('TopMargin'   ) then value$= '0.500'
		if lwrc$(key$)=lwrc$('BottomMargin') then value$= '0.500'
		if lwrc$(key$)=lwrc$('LeftMargin'  ) then value$= '0.500'
		if lwrc$(key$)=lwrc$('RightMargin' ) then value$= '0.500'
		if env$('ACSDeveloper')='' then ! if not a developer
			 fnreg_write(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
		end if
	end if
	! if env$('acsDeveloper')<>'' and key$='Orientation' then
	!   pr 'read   "'&value$&'" from Reg:'&env$('cursys')&'.'&prg$&'.Print.'&key$
	!   pause
	! end if
fnend
def library fnwriteProgramPrintProperty(key$*80,value$*256; prgCapForSettingsOverride$*256)
	autoLibrary
	on error goto Ertn
	fnwriteProgramPrintProperty=fn_writeProgramPrintProperty(key$,value$, prgCapForSettingsOverride$)
fnend
def fn_writeProgramPrintProperty(key$*80,value$*256; prgCapForSettingsOverride$*256)
	dim prg$*256
	if prgCapForSettingsOverride$='' then
		prg$=env$('Program_Caption')
	else
		prg$=prgCapForSettingsOverride$
	end if
	if env$('ACSDeveloper')<>'' then
		fnsreg_write(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
	else
		fnreg_write(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
		! pr 'writting "'&value$&'" to Reg:'&env$('cursys')&'.'&prg$&'.Print.'&key$
		! pause
	end if
fnend
include: Ertn
