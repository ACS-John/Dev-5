08000 def fn_setup
08020   setup=1
08030   ! library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fncomboa,fnFra,fntab,fnCmdKey,fnTos
08032   library 'S:\Core\Library': fnfm
08040   library 'S:\Core\Library': fnerror,fnBackgroundDisable
08050   library 'S:\Core\Library': fnsreg_read,fnsreg_write
08060   library 'S:\Core\Library': fnreg_write,fnreg_read
08100 fnend 
22000 def library fnprogram_properties(; forceProgramCaption$*256)
22020   on error goto ERTN
22040   if ~setup then let fn_setup
22060   if forceProgramCaption$<>'' then 
22080     dim oldCap$*256
22100     oldCap$=env$('Program_Caption')
22120     setenv('Program_Caption',forceProgramCaption$)
22140   end if
22160   fnBackgroundDisable(1)
22180   fnfm('Properties')
22200   if forceProgramCaption$<>'' then 
22220     setenv('Program_Caption',oldCap$)
22240   end if
22260   fnBackgroundDisable(0)
48380 fnend 
50000 ERTN: ! r:
50020   fnerror(program$,err,line,act$,"xit")
50040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
50060   if trim$(env$("ACSDeveloper"))<>"" then 
50080     execute "list -"&str$(line) : pause : goto ERTN_EXEC_ACT
50100   end if 
50120   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue."
50140   pr "" : pause : goto ERTN_EXEC_ACT
50160 ERTN_EXEC_ACT: execute act$ : goto ERTN ! /r
60030 def library fnpglen(&pglen)
60050   library 'S:\Core\Library': fnread_program_print_property
60080   fnread_program_print_property('Lines',lpp$) : pglen=val(lpp$)
60090   fnpglen=pglen
60100 fnend 
88000 def library fnread_program_print_property(key$*80,&value$; prgCapForSettingsOverride$*256)
88020   if ~setup then let fn_setup
88060   on error goto ERTN
88080   dim prg$*256
88100   if prgCapForSettingsOverride$='' then
88120     prg$=env$('Program_Caption')
88140   else
88160     prg$=prgCapForSettingsOverride$
88180   end if
88200   len_prg=len(prg$)
88220   ! if lwrc$(prg$(len_prg-2:len_prg))='.br' then prg$(len_prg-2:len_prg)='' ! remove the .br ending, if it is there
88240   ! if lwrc$(trim$(prg$))=lwrc$("acspr\checkfile") then prg$="acspr\checkhistory"
88260   if env$('ACSDeveloper')<>'' then !  if it is a developer
88280     fnsreg_read(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
88300   else
88320     fnreg_read(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
88340     if value$='' then
88360       fnsreg_read(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
88380     end if
88400   end if
88420   if value$='' then
88440     if lwrc$(key$)=lwrc$('Orientation' ) then value$= 'Portrait'
88460     if lwrc$(key$)=lwrc$('Height'      ) then value$= '11.000'
88480     if lwrc$(key$)=lwrc$('Width'       ) then value$= '8.500'
88500     if lwrc$(key$)=lwrc$('Lines'       ) then value$= '54'
88520     if lwrc$(key$)=lwrc$('FontSize'    ) then value$= '10'
88540     if lwrc$(key$)=lwrc$('TopMargin'   ) then value$= '0.500'
88560     if lwrc$(key$)=lwrc$('BottomMargin') then value$= '0.500'
88580     if lwrc$(key$)=lwrc$('LeftMargin'  ) then value$= '0.500'
88600     if lwrc$(key$)=lwrc$('RightMargin' ) then value$= '0.500'
88640     if env$('ACSDeveloper')='' then ! if not a developer
88660        fnreg_write(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
88680     end if
88690   end if
88692   ! if env$('acsDeveloper')<>'' and key$='Orientation' then 
88700   !   pr 'read   "'&value$&'" from Reg:'&env$('cursys')&'.'&prg$&'.Print.'&key$
88720   !   pause
88730   ! end if
88760 fnend
90000 def library fnwrite_program_print_property(key$*80,value$*256; prgCapForSettingsOverride$*256)
90020   if ~setup then let fn_setup
90060   on error goto ERTN
90080   dim prg$*256
90100   if prgCapForSettingsOverride$='' then
90120     prg$=env$('Program_Caption')
90140   else
90160     prg$=prgCapForSettingsOverride$
90180   end if
90200   if env$('ACSDeveloper')<>'' then
90220     fnsreg_write(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
90240   else
90260     fnreg_write(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
90270     ! pr 'writting "'&value$&'" to Reg:'&env$('cursys')&'.'&prg$&'.Print.'&key$
90272     ! pause
90280   end if
90300 fnend