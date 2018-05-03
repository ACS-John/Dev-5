def fn_setup
  setup=1
  ! library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fncomboa,fnFra,fntab,fnCmdKey,fnTos
  library 'S:\Core\Library': fnfm
  library 'S:\Core\Library': fnerror,fnBackgroundDisable
  library 'S:\Core\Library': fnsreg_read,fnsreg_write
  library 'S:\Core\Library': fnreg_write,fnreg_read
  library 'S:\Core\Library': fntos,fnlbl,fncomboa,fntxt,fncmdset,fnacs,fnwrite_program_print_property
fnend
def library fnprogram_properties(; forceProgramCaption$*256)
  on error goto ERTN
  if ~setup then let fn_setup
  if forceProgramCaption$<>'' then
    dim oldCap$*256
    oldCap$=env$('Program_Caption')
    setenv('Program_Caption',forceProgramCaption$)
  end if
  fnBackgroundDisable(1)
  fnfm('Properties')
	! fn_localPropertiesEdit
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
	fnread_program_print_property('Orientation' ,s$(sio_cmbOrientation :=respc+=1))
	fnread_program_print_property('Height'      ,s$(sio_txtHeight      :=respc+=1))
	fnread_program_print_property('Width'       ,s$(sio_txtWidth       :=respc+=1))
	fnread_program_print_property('Lines'       ,s$(sio_txtLpp         :=respc+=1))
	fnread_program_print_property('FontSize'    ,s$(sio_txtFontSize    :=respc+=1))
	fnread_program_print_property('TopMargin'   ,s$(sio_txtMarginTop   :=respc+=1))
	fnread_program_print_property('BottomMargin',s$(sio_txtMarginBottom:=respc+=1))
	fnread_program_print_property('LeftMargin'  ,s$(sio_txtMarginLeft  :=respc+=1))
	fnread_program_print_property('RightMargin' ,s$(sio_txtMarginRight :=respc+=1))
	fntos(sn$:='properties')

	fnlbl(1,1,'Orientation:'      ,20,1) : fncomboa('orientation',lc,33,mat orient$)
	fnlbl(1,1,'Height:'           ,20,1) : fntxt(lc,33,10,0,0,'')
	fnlbl(1,1,'Width:'            ,20,1) : fntxt(lc,33,10,0,0,'')
	fnlbl(1,1,'Lines Per Page:'   ,20,1) : fntxt(lc,33,10,0,0,'',0,'How many lines print before a page break is sent.')
	fnlbl(1,1,'Font Size:'        ,20,1) : fntxt(lc,33,10,0,0,'',0,'Base font size for printed report')
	fnlbl(1,1,'Top Margin:'       ,20,1) : fntxt(lc,33,10,0,0,'')
	fnlbl(1,1,'Bottom Margin:'    ,20,1) : fntxt(lc,33,10,0,0,'')
	fnlbl(1,1,'Left Margin:'      ,20,1) : fntxt(lc,33,10,0,0,'')
	fnlbl(1,1,'Right Margin:'     ,20,1) : fntxt(lc,33,10,0,0,'')
	fncmdset(1)
	fnacs(sn$,0,mat resp$,ckey, 0,0,0,1)
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
		fnwrite_program_print_property('Orientation',     resp$(sio_cmbOrientation)    )
		fnwrite_program_print_property('Height',          resp$(sio_txtHeight)         )
		fnwrite_program_print_property('Width',           resp$(sio_txtWidth)          )
		fnwrite_program_print_property('Lines',           resp$(sio_txtLpp)            )
		fnwrite_program_print_property('FontSize',        resp$(sio_txtFontSize)       )
		fnwrite_program_print_property('TopMargin',       resp$(sio_txtMarginTop)      )
		fnwrite_program_print_property('BottomMargin',    resp$(sio_txtMarginBottom)   )
		fnwrite_program_print_property('LeftMargin',      resp$(sio_txtMarginLeft)     )
		fnwrite_program_print_property('RightMargin',     resp$(sio_txtMarginRight)    )
	end if

fnend
include: ertn
def library fnpglen(&pglen)
  library 'S:\Core\Library': fnread_program_print_property
  fnread_program_print_property('Lines',lpp$) : pglen=val(lpp$)
  fnpglen=pglen
fnend
def library fnread_program_print_property(key$*80,&value$; prgCapForSettingsOverride$*256)
  if ~setup then let fn_setup
  on error goto ERTN
  dim prg$*256
  if prgCapForSettingsOverride$='' then
    prg$=env$('Program_Caption')
  else
    prg$=prgCapForSettingsOverride$
  end if
  len_prg=len(prg$)
  ! if lwrc$(prg$(len_prg-2:len_prg))='.br' then prg$(len_prg-2:len_prg)='' ! remove the .br ending, if it is there
  ! if lwrc$(trim$(prg$))=lwrc$("acspr\checkfile") then prg$="acspr\checkhistory"
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
def library fnwrite_program_print_property(key$*80,value$*256; prgCapForSettingsOverride$*256)
  if ~setup then let fn_setup
  on error goto ERTN
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